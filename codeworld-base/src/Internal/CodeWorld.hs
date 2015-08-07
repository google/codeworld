{-# LANGUAGE CPP                      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

module Internal.CodeWorld (
    Program,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf,
    trace
    ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.Char (chr)
import           Data.IORef
import           Data.JSString.Text
import           Data.Text (Text, singleton, pack)
import           Data.Time.Clock
import           Data.Word
import           Internal.Num (Number, fromDouble, toDouble)
import           Internal.Text hiding (show)
import qualified Internal.Text
import           Internal.Color
import           Internal.Picture
import           Internal.Event
import "base"    Prelude
import           System.IO.Unsafe
import           System.Random

#ifdef ghcjs_HOST_OS

import           GHCJS.DOM
import           GHCJS.DOM.Window as Window
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Types (Element, unElement)
import           GHCJS.Foreign
import           GHCJS.Types
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas

foreign import javascript interruptible "window.requestAnimationFrame($c);"
    js_waitAnimationFrame :: IO Double

foreign import javascript unsafe "$1['getBoundingClientRect']()['left']"
    js_getBoundingClientLeft :: JSRef Element -> IO Int

foreign import javascript unsafe "$1['getBoundingClientRect']()['top']"
    js_getBoundingClientTop :: JSRef Element -> IO Int

foreign import javascript unsafe "$1.drawImage($2, $3, $4);"
    js_canvasDrawImage :: Canvas.Context -> JSRef Element -> Int -> Int -> IO ()

foreign import javascript unsafe "window.reportRuntimeError($1, $2);"
    js_reportRuntimeError :: Bool -> JSString -> IO ()

foreign import javascript unsafe "$1.drawImage(document.getElementById($2), $3, $4, $5, $6);"
    js_drawCodeWorldLogo :: Canvas.Context -> JSString -> Int -> Int -> Int -> Int -> IO ()

drawCodeWorldLogo :: Canvas.Context -> Int -> Int -> Int -> Int -> IO ()
drawCodeWorldLogo ctx x y w h = js_drawCodeWorldLogo ctx "cwlogo" x y w h

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

followPath :: Canvas.Context -> [Point] -> Bool -> IO ()
followPath _   [] closed = return ()
followPath ctx ((sx,sy):ps) closed = do
    Canvas.moveTo (toDouble sx) (toDouble sy) ctx
    forM_ ps $ \(x,y) -> Canvas.lineTo (toDouble x) (toDouble y) ctx
    when closed $ Canvas.closePath ctx

drawFigure :: Canvas.Context -> DrawState -> Number -> IO () -> IO ()
drawFigure ctx ds w figure = do
    Canvas.save ctx
    withDS ctx ds $ do
        figure
        when (w /= 0) $ do
            Canvas.lineWidth (toDouble w) ctx
            Canvas.stroke ctx
    when (w == 0) $ do
        Canvas.lineWidth (1/25) ctx
        Canvas.stroke ctx
    Canvas.restore ctx

drawPicture :: Canvas.Context -> DrawState -> Picture -> IO ()
drawPicture ctx ds (Polygon ps) = do
    withDS ctx ds $ followPath ctx ps True
    Canvas.fill ctx
drawPicture ctx ds (Line ps w closed) = do
    drawFigure ctx ds w $ followPath ctx ps closed
drawPicture ctx ds (Arc b e r w) = do
    when (r > 0) $ drawFigure ctx ds w $ do
        Canvas.arc 0 0 (toDouble r) (toDouble b * pi / 180)
                   (toDouble e * pi / 180) False ctx
drawPicture ctx ds (Text txt) = withDS ctx ds $ do
    Canvas.scale 0.05 (-0.05) ctx
    Canvas.fillText (textToJSString txt) 0 0 ctx
drawPicture ctx ds Logo = withDS ctx ds $ do
    Canvas.scale 1 (-1) ctx
    drawCodeWorldLogo ctx (-9) (-2) 18 4
drawPicture ctx ds (Color (RGBA (r, g, b, a)) p)
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

setupScreenContext :: Element -> IO Canvas.Context
setupScreenContext canvas = do
    ctx <- Canvas.getContext $ Canvas.Canvas $ castRef $ unElement canvas
    Canvas.save ctx
    Canvas.translate 250 250 ctx
    Canvas.scale 25 (-25) ctx
    Canvas.textAlign Canvas.Left ctx
    Canvas.textBaseline Canvas.Alphabetic ctx
    Canvas.lineWidth 0 ctx
    Canvas.font "20px Times Roman" ctx
    return ctx

canvasDrawImage :: Canvas.Context -> Element -> Int -> Int -> IO ()
canvasDrawImage ctx elem x y = js_canvasDrawImage ctx (unElement elem) x y

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
        fromNum   n = Internal.Text.show (fromIntegral n)

getMousePos :: IsMouseEvent e => Element -> EventM w e Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ do
        cx <- js_getBoundingClientLeft (unElement canvas)
        cy <- js_getBoundingClientTop (unElement canvas)
        return (fromIntegral (ix - cx - 250) / 25, fromIntegral (cy - iy + 250) / 25)

fromButtonNum :: Word -> Maybe MouseButton
fromButtonNum 0 = Just LeftButton
fromButtonNum 1 = Just MiddleButton
fromButtonNum 2 = Just RightButton
fromButtonNum _ = Nothing

trace :: (a, Text) -> a
trace (x, msg) = unsafePerformIO $ do
    js_reportRuntimeError False (textToJSString msg)
    return x

--------------------------------------------------------------------------------
-- Runners for different kinds of activities.

data Activity = Activity {
    activityStep    :: Number -> Activity,
    activityEvent   :: Event -> Activity,
    activityDraw    :: Picture
    }

display :: Picture -> IO ()
display pic = do
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    ctx <- setupScreenContext canvas
    drawFrame ctx pic
    Canvas.restore ctx

setupEvents :: MVar Activity -> Element -> IO ()
setupEvents currentActivity canvas = do
    Just window <- currentWindow
    on window Window.keyDown $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                return (activityEvent activity (KeyPress keyName))
            preventDefault
            stopPropagation
    on window Window.keyUp $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                return (activityEvent activity (KeyRelease keyName))
            preventDefault
            stopPropagation
    on window Window.mouseDown $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing  -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                let event = MousePress (btn, pos)
                liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                    return (activityEvent activity event)
    on window Window.mouseUp $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing  -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                let event = MouseRelease (btn, pos)
                liftIO $ modifyMVar_ currentActivity $ \ activity -> do
                    return (activityEvent activity event)
    on window Window.mouseMove $ do
        pos <- getMousePos canvas
        let event = MouseMovement pos
        liftIO $ modifyMVar_ currentActivity $ \ activity -> do
            return (activityEvent activity event)
    return ()

passTime :: NominalDiffTime -> MVar Activity -> IO Activity
passTime dt activity = modifyMVar activity $ \a0 -> do
    let a1 = activityStep a0 (realToFrac (min dt 0.25))
    return (a1, a1)

run :: Activity -> IO ()
run startActivity = do
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)

    Just offscreenCanvas <- createElement doc (Just "canvas" :: Maybe JSString)
    setAttribute offscreenCanvas ("width" :: JSString)  ("500" :: JSString)
    setAttribute offscreenCanvas ("height" :: JSString) ("500" :: JSString)

    screen <- Canvas.getContext (Canvas.Canvas (castRef (unElement canvas)))
    buffer <- setupScreenContext offscreenCanvas

    currentActivity <- newMVar startActivity
    setupEvents currentActivity canvas

    let go t0 a0 = do
            drawFrame buffer (activityDraw a0)
            Canvas.clearRect 0 0 500 500 screen
            canvasDrawImage screen offscreenCanvas 0 0
            js_waitAnimationFrame
            t1 <- getCurrentTime
            a1 <- passTime (diffUTCTime t1 t0) currentActivity
            go t1 a1

    t0 <- getCurrentTime
    go t0 startActivity `catch` reportError

type Program = IO ()

reportError :: SomeException -> IO ()
reportError e = js_reportRuntimeError True (textToJSString (pack (show e)))

pictureOf :: Picture -> Program
pictureOf pic = display pic `catch` reportError

animationOf :: (Number -> Picture) -> Program
animationOf f = simulationOf (const 0, uncurry (+), f)

simulationOf :: ([Number] -> a, (a, Number) -> a, a -> Picture)
             -> Program
simulationOf (initial, step, draw) = interactionOf (initial, step, fst, draw)

interactionOf :: ([Number] -> a, (a, Number) -> a, (a, Event) -> a, a -> Picture)
              -> Program
interactionOf (initial, step, event, draw) = go `catch` reportError
  where go = run . activity . initial =<< randoms
        activity x = Activity {
                        activityStep    = (\dt -> activity (step (x, dt))),
                        activityEvent   = (\ev -> activity (event (x, ev))),
                        activityDraw    = draw x
                    }
        randoms :: IO [Number]
        randoms = do
            n  <- randomRIO (0,1)
            ns <- unsafeInterleaveIO randoms
            return (fromDouble n : ns)
#else

trace :: (a, Text) -> a
trace = undefined

type Program = IO ()

pictureOf :: Picture -> Program
pictureOf _ = putStrLn "<<picture>>"

animationOf :: (Number -> Picture) -> Program
animationOf _ = putStrLn "<<animation>>"

simulationOf :: ([Number] -> a, (a, Number) -> a, a -> Picture)
             -> Program
simulationOf (_, _, _) = putStrLn "<<simulation>>"

interactionOf :: ([Number] -> a, (a, Number) -> a, (a, Event) -> a, a -> Picture)
              -> Program
interactionOf (_, _, _, _) = putStrLn "<<interaction>>"

#endif
