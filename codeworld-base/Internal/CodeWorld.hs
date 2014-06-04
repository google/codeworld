{-# LANGUAGE CPP                      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module Internal.CodeWorld (
    Program,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf
    ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.Char (chr)
import           Data.Text (Text, singleton)
import           Data.Time.Clock
import           Internal.Num (Number, fromDouble, toDouble)
import           Internal.Text
import           Internal.Color
import           Internal.Picture
import           Internal.Event
import "base"    Prelude hiding (show)
import           System.IO.Unsafe
import           System.Random

#ifdef ghcjs_HOST_OS

import           GHCJS.DOM
import           GHCJS.DOM.DOMWindow
import           GHCJS.DOM.Document
import           GHCJS.DOM.EventM
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Types (Element, unElement)
import           GHCJS.Types
import qualified JavaScript.Canvas as Canvas

foreign import javascript safe "$1['getBoundingClientRect']()['left']"
    js_getBoundingClientLeft :: JSRef Element -> IO Int

foreign import javascript safe "$1['getBoundingClientRect']()['top']"
    js_getBoundingClientTop :: JSRef Element -> IO Int

--------------------------------------------------------------------------------
-- Draw state.  An affine transformation matrix, plus a Bool indicating whether
-- a color has been chosen yet.

type DrawState = (Double, Double, Double, Double, Double, Double, Bool)

initialDS :: DrawState
initialDS = (1, 0, 0, 1, 0, 0, False)

translateDS :: Double -> Double -> DrawState -> DrawState
translateDS x y (a,b,c,d,e,f,hc) =
    (a, b, c, d, a*x + c*y + e, b*x + d*y + f, hc)

scaleDS :: Double -> Double -> DrawState -> DrawState
scaleDS x y (a,b,c,d,e,f,hc) =
    (x*a, x*b, y*c, y*d, e, f, hc)

rotateDS :: Double -> DrawState -> DrawState
rotateDS r (a,b,c,d,e,f,hc) = let th = r * pi / 180 in
    (a * cos th + c * sin th,
     b * cos th + d * sin th,
     c * cos th - a * sin th,
     d * cos th - b * sin th,
     e, f, hc)

hasColorDS :: DrawState -> Bool
hasColorDS (_,_,_,_,_,_,hc) = hc

setColorDS :: DrawState -> DrawState
setColorDS (a,b,c,d,e,f,_) = (a,b,c,d,e,f,True)

withDS :: Canvas.Context -> DrawState -> IO () -> IO ()
withDS ctx (a,b,c,d,e,f,_) action = do
    Canvas.save ctx
    Canvas.transform a b c d e f ctx
    Canvas.beginPath ctx
    action
    Canvas.restore ctx

--------------------------------------------------------------------------------
-- Base drawing code for pictures.

followPath :: Canvas.Context -> [Point] -> IO ()
followPath _   [] = return ()
followPath ctx ((sx,sy):ps) = do
    Canvas.moveTo (toDouble sx) (toDouble sy) ctx
    forM_ ps $ \(x,y) -> Canvas.lineTo (toDouble x) (toDouble y) ctx

drawFigure :: Canvas.Context -> DrawState -> Number -> IO () -> IO ()
drawFigure ctx ds w figure = do
    Canvas.save ctx
    withDS ctx ds $ do
        figure
        when (w /= 0) $ do
            Canvas.lineWidth (toDouble w) ctx
            Canvas.stroke ctx
    when (w == 0) $ Canvas.stroke ctx
    Canvas.restore ctx

drawPicture :: Canvas.Context -> DrawState -> Picture -> IO ()
drawPicture ctx ds (Polygon ps) = do
    withDS ctx ds $ followPath ctx ps
    Canvas.fill ctx
drawPicture ctx ds (Line ps w) = do
    drawFigure ctx ds w $ followPath ctx ps
drawPicture ctx ds (Arc b e r w) = do
    when (r > 0) $ drawFigure ctx ds w $ do
        Canvas.arc 0 0 (toDouble r) (toDouble b * pi / 180)
                   (toDouble e * pi / 180) False ctx
drawPicture ctx ds (Text txt) = withDS ctx ds $ do
    Canvas.scale 1 (-1) ctx
    Canvas.fillText txt 0 0 ctx
drawPicture ctx ds (Color (RGBA r g b a) p)
  | hasColorDS ds = drawPicture ctx ds p
  | otherwise     = do
      Canvas.save ctx
      Canvas.strokeStyle (round $ toDouble r * 255)
                         (round $ toDouble g * 255)
                         (round $ toDouble b * 255)
                         (toDouble a) ctx
      Canvas.fillStyle (round $ toDouble r * 255)
                       (round $ toDouble g * 255)
                       (round $ toDouble b * 255)
                       (toDouble a) ctx
      drawPicture ctx (setColorDS ds) p
      Canvas.restore ctx
drawPicture ctx ds (Translate x y p) = drawPicture ctx (translateDS (toDouble x) (toDouble y) ds) p
drawPicture ctx ds (Scale x y p)     = drawPicture ctx (scaleDS (toDouble x) (toDouble y) ds) p
drawPicture ctx ds (Rotate r p)      = drawPicture ctx (rotateDS (toDouble r) ds) p
drawPicture ctx ds (Pictures ps)     = mapM_ (drawPicture ctx ds) (reverse ps)

drawFrame :: Canvas.Context -> Picture -> IO ()
drawFrame ctx pic = do
    Canvas.clearRect (-250) (-250) 500 500 ctx
    drawPicture ctx initialDS pic

setupScreenContext :: IO (Element, Canvas.Context)
setupScreenContext = do
    Just doc <- currentDocument
    Just canvas <- documentGetElementById doc ("screen" :: JSString)
    ctx <- Canvas.getContext (castRef (unElement canvas))
    Canvas.save ctx
    Canvas.translate 250 250 ctx
    Canvas.scale 1 (-1) ctx
    Canvas.textAlign Canvas.Left ctx
    Canvas.textBaseline Canvas.Alphabetic ctx
    Canvas.lineWidth 0 ctx
    Canvas.font "16px Times Roman" ctx
    return (canvas, ctx)

--------------------------------------------------------------------------------
-- Adapters from JavaScript values to logical values used by CodeWorld.

keyCodeToText :: Int -> Text
keyCodeToText n = case n of
    _ | n >= 47  && n <= 90  -> fromAscii n
    _ | n >= 96  && n <= 105 -> fromNum (n - 96)
    _ | n >= 112 && n <= 135 -> "F" <> fromNum (n - 111)
    3                        -> "Cancel"
    6                        -> "Help"
    8                        -> "Backspace"
    9                        -> "Tab"
    12                       -> "5"
    13                       -> "Enter"
    16                       -> "Shift"
    17                       -> "Ctrl"
    18                       -> "Alt"
    19                       -> "Break"
    20                       -> "CapsLock"
    27                       -> "Esc"
    32                       -> " "
    33                       -> "PageUp"
    34                       -> "PageDown"
    35                       -> "End"
    36                       -> "Home"
    37                       -> "Left"
    38                       -> "Up"
    39                       -> "Right"
    40                       -> "Down"
    42                       -> "*"
    43                       -> "+"
    44                       -> "PrintScreen"
    45                       -> "Insert"
    46                       -> "Delete"
    106                      -> "*"
    107                      -> "+"
    108                      -> "Separator"
    109                      -> "-"
    110                      -> "."
    111                      -> "/"
    144                      -> "NumLock"
    145                      -> "ScrollLock"
    186                      -> ";"
    187                      -> "="
    188                      -> ","
    189                      -> "-"
    190                      -> "."
    191                      -> "/"
    192                      -> "`"
    219                      -> "["
    220                      -> "\\"
    221                      -> "]"
    222                      -> "'"
    _                        -> "Unknown:" <> fromNum n
  where fromAscii n = singleton (chr n)
        fromNum   n = show (fromIntegral n)

getMousePos :: IsMouseEvent e => Element -> EventM e t Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ do
        cx <- js_getBoundingClientLeft (unElement canvas)
        cy <- js_getBoundingClientTop (unElement canvas)
        return (fromIntegral (ix - cx - 250), fromIntegral (cy - iy + 250))

--------------------------------------------------------------------------------
-- Runners for different kinds of activities.

data Activity = Activity {
    activityStep    :: Number -> Activity,
    activityEvent   :: Event -> Activity,
    activityDraw    :: Picture
    }

display :: Picture -> IO ()
display pic = do
    (_, ctx) <- setupScreenContext
    drawFrame ctx pic
    Canvas.restore ctx

setupEvents :: MVar Activity -> Element -> IO ()
setupEvents currentActivity canvas = do
    Just window <- currentWindow
    domWindowOnkeydown window $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                return (activityEvent activity (KeyPress keyName))
            preventDefault
            stopPropagation
    domWindowOnkeyup window $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                return (activityEvent activity (KeyRelease keyName))
            preventDefault
            stopPropagation
    domWindowOnmousedown window $ do
        button <- mouseButton
        pos <- getMousePos canvas
        let event = MousePress (fromIntegral button) pos
        liftIO $ modifyMVar_ currentActivity $ \ activity -> do
            return (activityEvent activity event)
    domWindowOnmouseup window $ do
        button <- mouseButton
        pos <- getMousePos canvas
        let event = MouseRelease (fromIntegral button) pos
        liftIO $ modifyMVar_ currentActivity $ \ activity -> do
            return (activityEvent activity event)
    domWindowOnmousemove window $ do
        pos <- getMousePos canvas
        let event = MouseMovement pos
        liftIO $ modifyMVar_ currentActivity $ \ activity -> do
            return (activityEvent activity event)
    domWindowOndrag window $ do
        pos <- getMousePos canvas
        let event = MouseDrag pos
        liftIO $ modifyMVar_ currentActivity $ \ activity -> do
            return (activityEvent activity event)
    return ()

passTime :: NominalDiffTime -> MVar Activity -> IO Activity
passTime dt activity = modifyMVar activity $ \a -> do
    let a' = activityStep a (realToFrac dt)
    return (a', a')

run :: Activity -> IO ()
run startActivity = do
    (canvas, ctx) <- setupScreenContext
    currentActivity <- newMVar startActivity
    setupEvents currentActivity canvas

    let go t0 activity = do
            drawFrame ctx (activityDraw activity)
            let target = addUTCTime 0.02 t0
            t1 <- getCurrentTime
            let timeToWait = round (1e6 * diffUTCTime target t1)
            t2 <- if timeToWait > 0
                  then threadDelay timeToWait >> getCurrentTime
                  else return t1
            a2 <- passTime (diffUTCTime t2 t0) currentActivity
            go t2 a2

    t0 <- getCurrentTime
    go t0 startActivity

type Program = IO ()

pictureOf :: Picture -> Program
pictureOf pic = display pic

animationOf :: (Number -> Picture) -> Program
animationOf f = simulationOf (const 0) (+) f

simulationOf :: ([Number] -> a)
             -> (Number -> a -> a)
             -> (a -> Picture)
             -> Program
simulationOf initial step draw = interactionOf initial step (const id) draw

interactionOf :: ([Number] -> a)
              -> (Number -> a -> a)
              -> (Event -> a -> a)
              -> (a -> Picture)
              -> Program
interactionOf initial step event draw = run . activity . initial =<< randoms
  where activity x = Activity {
                        activityStep    = (\dt -> activity (step dt x)),
                        activityEvent   = (\ev -> activity (event ev x)),
                        activityDraw    = draw x
                    }
        randoms :: IO [Number]
        randoms = do
            n  <- randomRIO (0,1)
            ns <- unsafeInterleaveIO randoms
            return (fromDouble n : ns)
#else

type Program = IO ()

pictureOf :: Picture -> Program
pictureOf _ = putStrLn "<<picture>>"

animationOf :: (Number -> Picture) -> Program
animationOf _ = putStrLn "<<animation>>"

simulationOf :: ([Number] -> a)
             -> (Number -> a -> a)
             -> (a -> Picture)
             -> Program
simulationOf _ _ _ = putStrLn "<<simulation>>"

interactionOf :: ([Number] -> a)
              -> (Number -> a -> a)
              -> (Event -> a -> a)
              -> (a -> Picture)
              -> Program
interactionOf _ _ _ _ = putStrLn "<<interaction>>"

#endif
