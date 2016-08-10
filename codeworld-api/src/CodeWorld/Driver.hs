{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE OverloadedStrings        #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.Driver (
    drawingOf,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf,
    trace
    ) where

import           Data.Text (Text, singleton, pack)
import qualified Data.Text as T
import           CodeWorld.Picture
import           CodeWorld.Event

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           CodeWorld.Color
import           Data.Monoid
import           Data.List (zip4)
import           Numeric
import           Data.Char (chr)

#ifdef ghcjs_HOST_OS

import           Data.IORef
import           Data.JSString.Text
import           Data.Time.Clock
import           Data.Word
import           GHCJS.DOM
import           GHCJS.DOM.Window as Window
import           GHCJS.DOM.Document
import qualified GHCJS.DOM.ClientRect as ClientRect
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Types (Element, unElement)
import           GHCJS.Foreign
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import           JavaScript.Web.AnimationFrame
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas
import           System.IO.Unsafe
import           System.Random

#else

import qualified Graphics.Blank as Canvas
import Graphics.Blank (Canvas)
import System.IO
import Data.Time.Clock
import Text.Printf


#endif

#ifdef ghcjs_HOST_OS


--------------------------------------------------------------------------------

foreign import javascript unsafe "$1.drawImage($2, $3, $4, $5, $6);"
    js_canvasDrawImage :: Canvas.Context -> Element -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.getContext('2d', { alpha: false })"
    js_getCodeWorldContext :: Canvas.Canvas -> IO Canvas.Context

canvasFromElement :: Element -> Canvas.Canvas
canvasFromElement = Canvas.Canvas . unElement

elementFromCanvas :: Canvas.Canvas -> Element
elementFromCanvas = pFromJSVal . jsval

#endif

--------------------------------------------------------------------------------
-- Draw state.  An affine transformation matrix, plus a Bool indicating whether
-- a color has been chosen yet.

type DrawState = (Double, Double, Double, Double, Double, Double, Maybe Color)

initialDS :: DrawState
initialDS = (1, 0, 0, 1, 0, 0, Nothing)

translateDS :: Double -> Double -> DrawState -> DrawState
translateDS x y (a,b,c,d,e,f,hc) =
    (a, b, c, d, a*25*x + c*25*y + e, b*25*x + d*25*y + f, hc)

scaleDS :: Double -> Double -> DrawState -> DrawState
scaleDS x y (a,b,c,d,e,f,hc) =
    (x*a, x*b, y*c, y*d, e, f, hc)

rotateDS :: Double -> DrawState -> DrawState
rotateDS r (a,b,c,d,e,f,hc) =
    (a * cos r + c * sin r,
     b * cos r + d * sin r,
     c * cos r - a * sin r,
     d * cos r - b * sin r,
     e, f, hc)

setColorDS :: Color -> DrawState -> DrawState
setColorDS col (a,b,c,d,e,f,Nothing) = (a,b,c,d,e,f,Just col)
setColorDS _ (a,b,c,d,e,f,Just col) = (a,b,c,d,e,f,Just col)

getColorDS :: DrawState -> Maybe Color
getColorDS (a,b,c,d,e,f,col) = col

#ifdef ghcjs_HOST_OS

withDS :: Canvas.Context -> DrawState -> IO () -> IO ()
withDS ctx (ta,tb,tc,td,te,tf,col) action = do
    Canvas.save ctx
    Canvas.transform ta tb tc td te tf ctx
    Canvas.beginPath ctx
    action
    Canvas.restore ctx

--------------------------------------------------------------------------------
-- Base drawing code for pictures.

applyColor :: Canvas.Context -> DrawState -> IO ()
applyColor ctx ds = case getColorDS ds of
    Nothing -> do
      Canvas.strokeStyle 0 0 0 1 ctx
      Canvas.fillStyle 0 0 0 1 ctx
    Just (RGBA r g b a) -> do
      Canvas.strokeStyle (round $ r * 255)
                         (round $ g * 255)
                         (round $ b * 255)
                         a ctx
      Canvas.fillStyle (round $ r * 255)
                       (round $ g * 255)
                       (round $ b * 255)
                       a ctx

foreign import javascript unsafe "$1.globalCompositeOperation = $2"
    js_setGlobalCompositeOperation :: Canvas.Context -> JSString -> IO ()

drawCodeWorldLogo :: Canvas.Context -> DrawState -> Int -> Int -> Int -> Int -> IO ()
drawCodeWorldLogo ctx ds x y w h = do
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("cwlogo" :: JSString)
    case getColorDS ds of
        Nothing -> js_canvasDrawImage ctx canvas x y w h
        Just (RGBA r g b a) -> do
            -- This is a tough case.  The best we can do is to allocate an
            -- offscreen buffer as a temporary.
            buf <- Canvas.create w h
            bufctx <- js_getCodeWorldContext buf
            applyColor bufctx ds
            Canvas.fillRect 0 0 (fromIntegral w) (fromIntegral h) bufctx
            js_setGlobalCompositeOperation bufctx "destination-in"
            js_canvasDrawImage bufctx canvas 0 0 w h
            js_canvasDrawImage ctx (elementFromCanvas buf) x y w h

followPath :: Canvas.Context -> [Point] -> Bool -> Bool -> IO ()
followPath ctx [] closed _ = return ()
followPath ctx [p1] closed _ = return ()
followPath ctx ((sx,sy):ps) closed False = do
    Canvas.moveTo (25 * sx) (25 * sy) ctx
    forM_ ps $ \(x,y) -> Canvas.lineTo (25 * x) (25 * y) ctx
    when closed $ Canvas.closePath ctx
followPath ctx [p1, p2] False True = followPath ctx [p1, p2] False False
followPath ctx ps False True = do
    let [(x1,y1), (x2,y2), (x3,y3)] = take 3 ps
        dprev = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        dnext = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
        p     = dprev / (dprev + dnext)
        cx    = x2 + p * (x1 - x3) / 2
        cy    = y2 + p * (y1 - y3) / 2
     in do Canvas.moveTo (25 * x1) (25 * y1) ctx
           Canvas.quadraticCurveTo (25 * cx) (25 * cy) (25 * x2) (25 * y2) ctx
    forM_ (zip4 ps (tail ps) (tail $ tail ps) (tail $ tail $ tail ps)) $
        \((x1,y1),(x2,y2),(x3,y3),(x4,y4)) ->
            let dp  = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                d1  = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
                d2  = sqrt ((x4 - x3)^2 + (y4 - y3)^2)
                p   = d1 / (d1 + d2)
                r   = d1 / (dp + d1)
                cx1 = x2 + r * (x3 - x1) / 2
                cy1 = y2 + r * (y3 - y1) / 2
                cx2 = x3 + p * (x2 - x4) / 2
                cy2 = y3 + p * (y2 - y4) / 2
            in  Canvas.bezierCurveTo (25 * cx1) (25 * cy1) (25 * cx2) (25 * cy2)
                                     (25 * x3) (25 * y3) ctx
    let [(x1,y1), (x2,y2), (x3,y3)] = reverse $ take 3 $ reverse ps
        dp = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        d1 = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
        r  = d1 / (dp + d1)
        cx = x2 + r * (x3 - x1) / 2
        cy = y2 + r * (y3 - y1) / 2
     in Canvas.quadraticCurveTo (25 * cx) (25 * cy) (25 * x3) (25 * y3) ctx
followPath ctx ps@(_:(sx,sy):_) True True = do
    Canvas.moveTo (25 * sx) (25 * sy) ctx
    let rep = cycle ps
    forM_ (zip4 ps (tail rep) (tail $ tail rep) (tail $ tail $ tail rep)) $
        \((x1,y1),(x2,y2),(x3,y3),(x4,y4)) ->
            let dp  = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                d1  = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
                d2  = sqrt ((x4 - x3)^2 + (y4 - y3)^2)
                p   = d1 / (d1 + d2)
                r   = d1 / (dp + d1)
                cx1 = x2 + r * (x3 - x1) / 2
                cy1 = y2 + r * (y3 - y1) / 2
                cx2 = x3 + p * (x2 - x4) / 2
                cy2 = y3 + p * (y2 - y4) / 2
            in  Canvas.bezierCurveTo (25 * cx1) (25 * cy1) (25 * cx2) (25 * cy2)
                                     (25 * x3) (25 * y3) ctx
    Canvas.closePath ctx

drawFigure :: Canvas.Context -> DrawState -> Double -> IO () -> IO ()
drawFigure ctx ds w figure = do
    withDS ctx ds $ do
        figure
        when (w /= 0) $ do
            Canvas.lineWidth (25 * w) ctx
            applyColor ctx ds
            Canvas.stroke ctx
    when (w == 0) $ do
        Canvas.lineWidth 1 ctx
        applyColor ctx ds
        Canvas.stroke ctx

fontString :: TextStyle -> Font -> JSString
fontString style font = stylePrefix style <> "25px " <> fontName font
  where stylePrefix Plain        = ""
        stylePrefix Bold         = "bold "
        stylePrefix Italic       = "italic "
        fontName SansSerif       = "sans-serif"
        fontName Serif           = "serif"
        fontName Monospace       = "monospace"
        fontName Handwriting     = "cursive"
        fontName Fancy           = "fantasy"
        fontName (NamedFont txt) = "\"" <> textToJSString (T.filter (/= '"') txt) <> "\""

drawPicture :: Canvas.Context -> DrawState -> Picture -> IO ()
drawPicture ctx ds (Polygon ps smooth) = do
    withDS ctx ds $ followPath ctx ps True smooth
    applyColor ctx ds
    Canvas.fill ctx
drawPicture ctx ds (Path ps w closed smooth) = do
    drawFigure ctx ds w $ followPath ctx ps closed smooth
drawPicture ctx ds (Sector b e r) = withDS ctx ds $ do
    Canvas.arc 0 0 (25 * abs r) b e (b > e) ctx
    Canvas.lineTo 0 0 ctx
    applyColor ctx ds
    Canvas.fill ctx
drawPicture ctx ds (Arc b e r w) = do
    drawFigure ctx ds w $ do
        Canvas.arc 0 0 (25 * abs r) b e (b > e) ctx
drawPicture ctx ds (Text sty fnt txt) = withDS ctx ds $ do
    Canvas.scale 1 (-1) ctx
    applyColor ctx ds
    Canvas.font (fontString sty fnt) ctx
    Canvas.fillText (textToJSString txt) 0 0 ctx
drawPicture ctx ds Logo = withDS ctx ds $ do
    Canvas.scale 1 (-1) ctx
    drawCodeWorldLogo ctx ds (-225) (-50) 450 100
drawPicture ctx ds (Color col p)     = drawPicture ctx (setColorDS col ds) p
drawPicture ctx ds (Translate x y p) = drawPicture ctx (translateDS x y ds) p
drawPicture ctx ds (Scale x y p)     = drawPicture ctx (scaleDS x y ds) p
drawPicture ctx ds (Rotate r p)      = drawPicture ctx (rotateDS r ds) p
drawPicture ctx ds (Pictures ps)     = mapM_ (drawPicture ctx ds) (reverse ps)

drawFrame :: Canvas.Context -> Picture -> IO ()
drawFrame ctx pic = do
    Canvas.fillStyle 255 255 255 1 ctx
    Canvas.fillRect (-250) (-250) 500 500 ctx
    drawPicture ctx initialDS pic

setupScreenContext :: Element -> ClientRect.ClientRect -> IO Canvas.Context
setupScreenContext canvas rect = do
    cw <- ClientRect.getWidth rect
    ch <- ClientRect.getHeight rect
    ctx <- js_getCodeWorldContext (canvasFromElement canvas)
    Canvas.save ctx
    Canvas.translate (realToFrac cw / 2) (realToFrac ch / 2) ctx
    Canvas.scale (realToFrac cw / 500) (- realToFrac ch / 500) ctx
    Canvas.lineWidth 0 ctx
    Canvas.textAlign Canvas.Center ctx
    Canvas.textBaseline Canvas.Middle ctx
    return ctx

setCanvasSize :: Element -> Element -> IO ()
setCanvasSize target canvas = do
    Just rect <- getBoundingClientRect canvas
    cx <- ClientRect.getWidth rect
    cy <- ClientRect.getHeight rect
    setAttribute target ("width" :: JSString) (show (round cx))
    setAttribute target ("height" :: JSString) (show (round cy))

--------------------------------------------------------------------------------

foreign import javascript unsafe "window.reportRuntimeError($1, $2);"
    js_reportRuntimeError :: Bool -> JSString -> IO ()

-- | Prints a debug message to the CodeWorld console when a value is forced.
-- This is equivalent to the similarly named function in `Debug.Trace`, except
-- that it uses the CodeWorld console instead of standard output.
trace :: Text -> a -> a
trace msg x = unsafePerformIO $ do
    js_reportRuntimeError False (textToJSString msg)
    return x

reportError :: SomeException -> IO ()
reportError e = js_reportRuntimeError True (textToJSString (pack (show e)))

--------------------------------------------------------------------------------

-- | Draws a `Picture`.  This is the simplest CodeWorld entry point.
drawingOf :: Picture -> IO ()
drawingOf pic = display pic `catch` reportError

-- | Draws a `Picture`.  This is the simplest CodeWorld entry point.
pictureOf :: Picture -> IO ()
pictureOf pic = display pic `catch` reportError
{-# WARNING pictureOf "Please use drawingOf instead of pictureOf" #-}

display :: Picture -> IO ()
display pic = do
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    on window Window.resize $ liftIO (draw canvas)
    draw canvas
  where
    draw canvas = do
        setCanvasSize canvas canvas
        Just rect <- getBoundingClientRect canvas
        ctx <- setupScreenContext canvas rect
        drawFrame ctx pic
        Canvas.restore ctx

#endif

--------------------------------------------------------------------------------
-- Implementation of interactionOf

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
        fromNum   n = pack (show (fromIntegral n))


#ifdef ghcjs_HOST_OS
getMousePos :: IsMouseEvent e => Element -> EventM w e Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ do
        Just rect <- getBoundingClientRect canvas
        cx <- ClientRect.getLeft rect
        cy <- ClientRect.getTop rect
        cw <- ClientRect.getWidth rect
        ch <- ClientRect.getHeight rect
        return (20 * fromIntegral (ix - round cx) / realToFrac cw - 10,
                20 * fromIntegral (round cy - iy) / realToFrac cw + 10)

fromButtonNum :: Word -> Maybe MouseButton
fromButtonNum 0 = Just LeftButton
fromButtonNum 1 = Just MiddleButton
fromButtonNum 2 = Just RightButton
fromButtonNum _ = Nothing

#endif

data Activity = Activity {
    activityStep    :: Double -> Activity,
    activityEvent   :: Event -> Activity,
    activityDraw    :: Picture
    }

handleEvent :: Event -> MVar Activity -> IO ()
handleEvent event activity =
    modifyMVar_ activity $ \a0 -> return (activityEvent a0 event)

passTime :: Double -> MVar Activity -> IO Activity
passTime dt activity = modifyMVar activity $ \a0 -> do
    let a1 = activityStep a0 (realToFrac (min dt 0.25))
    return (a1, a1)


#ifdef ghcjs_HOST_OS

setupEvents :: MVar Activity -> Element -> Element -> IO ()
setupEvents currentActivity canvas offscreen = do
    Just window <- currentWindow
    on window Window.keyDown $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handleEvent (KeyPress keyName) currentActivity
            preventDefault
            stopPropagation
    on window Window.keyUp $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handleEvent (KeyRelease keyName) currentActivity
            preventDefault
            stopPropagation
    on window Window.mouseDown $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing  -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                liftIO $ handleEvent (MousePress btn pos) currentActivity
    on window Window.mouseUp $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing  -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                liftIO $ handleEvent (MouseRelease btn pos) currentActivity
    on window Window.mouseMove $ do
        pos <- getMousePos canvas
        liftIO $ handleEvent (MouseMovement pos) currentActivity
    return ()

run :: Activity -> IO ()
run startActivity = do
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    offscreenCanvas <- Canvas.create 500 500

    setCanvasSize canvas canvas
    setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    on window Window.resize $ do
        liftIO $ setCanvasSize canvas canvas
        liftIO $ setCanvasSize (elementFromCanvas offscreenCanvas) canvas

    currentActivity <- newMVar startActivity
    setupEvents currentActivity canvas (elementFromCanvas offscreenCanvas)

    screen <- js_getCodeWorldContext (canvasFromElement canvas)

    let go t0 a0 = do
            Just rect <- getBoundingClientRect canvas
            buffer <- setupScreenContext (elementFromCanvas offscreenCanvas) rect
            drawFrame buffer (activityDraw a0)
            Canvas.restore buffer

            t1 <- waitForAnimationFrame

            Just rect <- getBoundingClientRect canvas
            cw <- ClientRect.getWidth rect
            ch <- ClientRect.getHeight rect

            Canvas.clearRect 0 0 (realToFrac cw) (realToFrac ch) screen
            js_canvasDrawImage screen (elementFromCanvas offscreenCanvas) 0 0 (round cw) (round ch)
            a1 <- passTime ((t1 - t0) / 1000) currentActivity
            go t1 a1

    t0 <- waitForAnimationFrame
    go t0 startActivity `catch` reportError

#else

run :: Activity -> IO ()
run startActivity = runBlankCanvas $ \context -> do
    let w = Canvas.width context
        h = Canvas.height context

    offscreenCanvas <- Canvas.send context $ Canvas.newCanvas (w,h)

    currentActivity <- newMVar startActivity

    let go t0 a0 = do
            Canvas.send context $
                Canvas.with offscreenCanvas $
                    Canvas.saveRestore $ do
                        setupScreenContext (w,h)
                        drawFrame (activityDraw a0)

            threadDelay 50000
            t1 <- getCurrentTime

            Canvas.send context $ Canvas.drawImageAt (offscreenCanvas, 0, 0)
            a1 <- passTime (realToFrac (t1 `diffUTCTime` t0)) currentActivity
            go t1 a1

    t0 <- getCurrentTime
    go t0 startActivity `catch` reportError

#endif

-- | Runs an interactive event-driven CodeWorld program.  This is the most
-- advanced CodeWorld entry point.
interactionOf :: world
              -> (Double -> world -> world)
              -> (Event -> world -> world)
              -> (world -> Picture)
              -> IO ()
interactionOf initial step event draw = go `catch` reportError
  where go = run (activity initial)
        activity x = Activity {
                        activityStep    = (\dt -> activity (step dt x)),
                        activityEvent   = (\ev -> activity (event ev x)),
                        activityDraw    = draw x
                    }


data Wrapped a = Wrapped {
    state          :: a,
    paused         :: Bool,
    mouseMovedTime :: Double
    }

data Control :: * -> * where
  PlayButton    :: Control a
  PauseButton   :: Control a
  StepButton    :: Control a
  RestartButton :: Control Double
  BackButton    :: Control Double
  TimeLabel     :: Control Double

wrappedStep :: (Double -> a -> a) -> Double -> Wrapped a -> Wrapped a
wrappedStep f dt w = w {
    state          = if paused w then state w else f dt (state w),
    mouseMovedTime = mouseMovedTime w + dt
    }

wrappedEvent :: (Wrapped a -> [Control a])
      -> (Double -> a -> a)
      -> Event -> Wrapped a -> Wrapped a
wrappedEvent _     _ (MouseMovement _)         w
    = w { mouseMovedTime = 0 }
wrappedEvent ctrls f (MousePress LeftButton p) w
    = (foldr (handleControl f p) w (ctrls w)) { mouseMovedTime = 0 }
wrappedEvent _     _ _                         w = w

handleControl :: (Double -> a -> a)
              -> Point
              -> Control a
              -> Wrapped a
              -> Wrapped a
handleControl _ (x,y) RestartButton w
  | -9.4 < x && x < -8.6 && -9.4 < y && y < -8.6
      = w { state = 0 }
handleControl _ (x,y) PlayButton    w
  | -8.4 < x && x < -7.6 && -9.4 < y && y < -8.6
      = w { paused = False }
handleControl _ (x,y) PauseButton   w
  | -8.4 < x && x < -7.6 && -9.4 < y && y < -8.6
      = w { paused = True }
handleControl f (x,y) StepButton    w
  | -7.4 < x && x < -6.6 && -9.4 < y && y < -8.6
      = w { state = f 0.1 (state w) }
handleControl _ (x,y) BackButton    w
  | -6.4 < x && x < -5.6 && -9.4 < y && y < -8.6
      = w { state = max 0 (state w - 0.1) }
handleControl _ _     _             w = w

wrappedDraw :: (Wrapped a -> [Control a])
     -> (a -> Picture)
     -> Wrapped a -> Picture
wrappedDraw ctrls f w = drawControlPanel ctrls w <> f (state w)

drawControlPanel :: (Wrapped a -> [Control a]) -> Wrapped a -> Picture
drawControlPanel ctrls w = pictures [ drawControl w alpha c | c <- ctrls w ]
  where alpha | mouseMovedTime w < 4.5  = 1
              | mouseMovedTime w < 5.0  = 10 - 2 * mouseMovedTime w
              | otherwise               = 0

drawControl :: Wrapped a -> Double -> Control a -> Picture
drawControl _ alpha RestartButton = translated (-9) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (thickArc 0.1 (pi / 6) (11 * pi / 6) 0.2 <>
                     translated 0.173 (-0.1) (solidRectangle 0.17 0.17))
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      0.8 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)

drawControl _ alpha PlayButton = translated (-8) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (solidPolygon [ (-0.2, 0.25), (-0.2, -0.25), (0.2, 0) ])
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      0.8 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)

drawControl _ alpha PauseButton = translated (-8) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (translated (-0.15) 0 (solidRectangle 0.2 0.6) <>
                     translated ( 0.15) 0 (solidRectangle 0.2 0.6))
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      0.8 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)

drawControl _ alpha StepButton = translated (-7) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (translated (-0.15) 0 (solidRectangle 0.2 0.5) <>
                     solidPolygon [ (0.05, 0.25), (0.05, -0.25), (0.3, 0) ])
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      0.8 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)

drawControl _ alpha BackButton = translated (-6) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (translated 0.15 0 (solidRectangle 0.2 0.5) <>
                     solidPolygon [ (-0.05, 0.25), (-0.05, -0.25), (-0.3, 0) ])
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      0.8 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)

drawControl w alpha TimeLabel = translated (8) (-9) p
  where p = colored (RGBA 0   0   0   alpha)
                    (scaled 0.5 0.5 $
                        text (pack (showFFloatAlt (Just 4) (state w) "s")))
         <> colored (RGBA 0.2 0.2 0.2 alpha) (rectangle      3.0 0.8)
         <> colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 3.0 0.8)

animationControls :: Wrapped Double -> [Control Double]
animationControls w
  | mouseMovedTime w > 5    = []
  | paused w && state w > 0 = [ RestartButton, PlayButton, StepButton,
                                BackButton, TimeLabel ]
  | paused w                = [ RestartButton, PlayButton, StepButton,
                                TimeLabel ]
  | otherwise               = [ RestartButton, PauseButton, TimeLabel ]

-- | Shows an animation, with a picture for each time given by the parameter.
animationOf :: (Double -> Picture) -> IO ()
animationOf f =
    interactionOf initial
                  (wrappedStep (+))
                  (wrappedEvent animationControls (+))
                  (wrappedDraw animationControls f)
  where initial = Wrapped {
            state          = 0,
            paused         = False,
            mouseMovedTime = 1000
        }

simulationControls :: Wrapped w -> [Control w]
simulationControls w
  | mouseMovedTime w > 5 = []
  | paused w             = [ PlayButton, StepButton ]
  | otherwise            = [ PauseButton ]

-- | Shows a simulation, which is essentially a continuous-time dynamical
-- system described by an initial value and step function.
simulationOf :: world -> (Double -> world -> world) -> (world -> Picture) -> IO ()
simulationOf simInitial simStep simDraw =
    interactionOf initial
                  (wrappedStep simStep)
                  (wrappedEvent simulationControls simStep)
                  (wrappedDraw simulationControls simDraw)
  where initial = Wrapped {
            state          = simInitial,
            paused         = False,
            mouseMovedTime = 1000
        }

#ifndef  ghcjs_HOST_OS

trace :: Text -> a -> a
trace = undefined

withDS :: DrawState -> Canvas () -> Canvas ()
withDS (ta,tb,tc,td,te,tf,col) action =
    Canvas.saveRestore $ do
        Canvas.transform (ta, tb, tc, td, te, tf)
        Canvas.beginPath ()
        action

applyColor :: DrawState -> Canvas ()
applyColor ds = case getColorDS ds of
    Nothing -> do
      Canvas.strokeStyle "black"
      Canvas.fillStyle   "black"
    Just (RGBA r g b a) -> do
      let style = pack $ printf "rgba(%.0f,%.0f,%.0f,%f)" (r*255) (g*255) (b*255) a
      Canvas.strokeStyle style
      Canvas.fillStyle   style


drawFigure :: DrawState -> Double -> Canvas () -> Canvas ()
drawFigure ds w figure = do
    withDS ds $ do
        figure
        when (w /= 0) $ do
            Canvas.lineWidth (25 * w)
            applyColor ds
            Canvas.stroke ()
    when (w == 0) $ do
        Canvas.lineWidth 1
        applyColor ds
        Canvas.stroke ()

followPath :: [Point] -> Bool -> Bool -> Canvas ()
followPath [] closed _ = return ()
followPath [p1] closed _ = return ()
followPath ((sx,sy):ps) closed False = do
    Canvas.moveTo (25 * sx, 25 * sy)
    forM_ ps $ \(x,y) -> Canvas.lineTo (25 * x, 25 * y)
    when closed $ Canvas.closePath ()
followPath [p1, p2] False True = followPath [p1, p2] False False
followPath ps False True = do
    let [(x1,y1), (x2,y2), (x3,y3)] = take 3 ps
        dprev = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        dnext = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
        p     = dprev / (dprev + dnext)
        cx    = x2 + p * (x1 - x3) / 2
        cy    = y2 + p * (y1 - y3) / 2
     in do Canvas.moveTo (25 * x1, 25 * y1)
           Canvas.quadraticCurveTo (25 * cx, 25 * cy, 25 * x2, 25 * y2)
    forM_ (zip4 ps (tail ps) (tail $ tail ps) (tail $ tail $ tail ps)) $
        \((x1,y1),(x2,y2),(x3,y3),(x4,y4)) ->
            let dp  = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                d1  = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
                d2  = sqrt ((x4 - x3)^2 + (y4 - y3)^2)
                p   = d1 / (d1 + d2)
                r   = d1 / (dp + d1)
                cx1 = x2 + r * (x3 - x1) / 2
                cy1 = y2 + r * (y3 - y1) / 2
                cx2 = x3 + p * (x2 - x4) / 2
                cy2 = y3 + p * (y2 - y4) / 2
            in  Canvas.bezierCurveTo ( 25 * cx1, 25 * cy1, 25 * cx2, 25 * cy2
                                     , 25 * x3,  25 * y3)
    let [(x1,y1), (x2,y2), (x3,y3)] = reverse $ take 3 $ reverse ps
        dp = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        d1 = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
        r  = d1 / (dp + d1)
        cx = x2 + r * (x3 - x1) / 2
        cy = y2 + r * (y3 - y1) / 2
     in Canvas.quadraticCurveTo (25 * cx, 25 * cy, 25 * x3, 25 * y3)
followPath ps@(_:(sx,sy):_) True True = do
    Canvas.moveTo (25 * sx, 25 * sy)
    let rep = cycle ps
    forM_ (zip4 ps (tail rep) (tail $ tail rep) (tail $ tail $ tail rep)) $
        \((x1,y1),(x2,y2),(x3,y3),(x4,y4)) ->
            let dp  = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                d1  = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
                d2  = sqrt ((x4 - x3)^2 + (y4 - y3)^2)
                p   = d1 / (d1 + d2)
                r   = d1 / (dp + d1)
                cx1 = x2 + r * (x3 - x1) / 2
                cy1 = y2 + r * (y3 - y1) / 2
                cx2 = x3 + p * (x2 - x4) / 2
                cy2 = y3 + p * (y2 - y4) / 2
            in  Canvas.bezierCurveTo ( 25 * cx1, 25 * cy1, 25 * cx2, 25 * cy2
                                     , 25 * x3, 25 * y3)
    Canvas.closePath ()


fontString :: TextStyle -> Font -> Text
fontString style font = stylePrefix style <> "25px " <> fontName font
  where stylePrefix Plain        = ""
        stylePrefix Bold         = "bold "
        stylePrefix Italic       = "italic "
        fontName SansSerif       = "sans-serif"
        fontName Serif           = "serif"
        fontName Monospace       = "monospace"
        fontName Handwriting     = "cursive"
        fontName Fancy           = "fantasy"
        fontName (NamedFont txt) = "\"" <> T.filter (/= '"') txt <> "\""

drawPicture :: DrawState -> Picture -> Canvas ()
drawPicture ds (Polygon ps smooth) = do
    withDS ds $ followPath ps True smooth
    applyColor ds
    Canvas.fill ()
drawPicture ds (Path ps w closed smooth) = do
    drawFigure ds w $ followPath ps closed smooth
drawPicture ds (Sector b e r) = withDS ds $ do
    Canvas.arc (0, 0, 25 * abs r, b, e,  b > e)
    Canvas.lineTo (0, 0)
    applyColor ds
    Canvas.fill ()
drawPicture ds (Arc b e r w) = do
    drawFigure ds w $ do
        Canvas.arc (0, 0, 25 * abs r, b, e, b > e)
drawPicture ds (Text sty fnt txt) = withDS ds $ do
    Canvas.scale (1, -1)
    applyColor ds
    Canvas.font (fontString sty fnt)
    Canvas.fillText (txt, 0, 0)
{-
drawPicture ds Logo = withDS ds $ do
    Canvas.scale 1 (-1)
    drawCodeWorldLogo ds (-225) (-50) 450 100
-}
drawPicture ds (Color col p)     = drawPicture (setColorDS col ds) p
drawPicture ds (Translate x y p) = drawPicture (translateDS x y ds) p
drawPicture ds (Scale x y p)     = drawPicture (scaleDS x y ds) p
drawPicture ds (Rotate r p)      = drawPicture (rotateDS r ds) p
drawPicture ds (Pictures ps)     = mapM_ (drawPicture ds) (reverse ps)


pictureOf :: Picture -> IO ()
pictureOf = drawingOf
{-# WARNING pictureOf "Please use drawingOf instead of pictureOf" #-}

drawingOf :: Picture -> IO ()
drawingOf pic = display pic `catch` reportError

drawFrame :: Picture -> Canvas ()
drawFrame pic = do
    Canvas.fillStyle "white"
    Canvas.fillRect (-250, -250, 500, 500)
    drawPicture initialDS pic

setupScreenContext :: (Int, Int) -> Canvas ()
setupScreenContext (cw, ch) = do
    Canvas.translate (realToFrac cw / 2, realToFrac ch / 2)
    let s = min (realToFrac cw / 500) (realToFrac ch / 500)
    Canvas.scale (s, -s)
    Canvas.lineWidth 0
    Canvas.textAlign Canvas.CenterAnchor
    Canvas.textBaseline Canvas.MiddleBaseline

display :: Picture -> IO ()
display pic = runBlankCanvas $ \context ->
    Canvas.send context $ Canvas.saveRestore $ do
        let rect = (Canvas.width context, Canvas.height context)
        setupScreenContext rect
        drawFrame pic

runBlankCanvas :: (Canvas.DeviceContext -> IO ()) -> IO ()
runBlankCanvas act = do
    putStrLn $ printf "Open me on http://127.0.0.1:%d/" (Canvas.port options)
    Canvas.blankCanvas options $ \context -> do
        putStrLn "Program is starting..."
        act context
  where
    options = 3000 { Canvas.events =
        [ "mousedown", "mouseenter", "mouseout", "mouseover", "mouseup", "resize"]
    }


reportError :: SomeException -> IO ()
reportError e = hPrint stderr e

#endif
