{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.
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

module CodeWorld.CanvasM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (MonadIO)
import Data.Text (Text, pack)

#ifdef ghcjs_HOST_OS

import Data.JSString.Text
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas

#else

import qualified Graphics.Blank as Canvas
import Graphics.Blank (Canvas)
import Text.Printf

#endif

class Monad m => MonadCanvas m where
    type Image m

    save :: m ()
    restore :: m ()
    transform ::
           Double -> Double -> Double -> Double -> Double -> Double -> m ()
    translate :: Double -> Double -> m ()
    scale :: Double -> Double -> m ()
    newImage :: Int -> Int -> m a -> m (Image m, a)
    drawImage :: Image m -> Int -> Int -> Int -> Int -> m ()
    globalCompositeOperation :: Text -> m ()
    lineWidth :: Double -> m ()
    strokeColor :: Int -> Int -> Int -> Double -> m ()
    fillColor :: Int -> Int -> Int -> Double -> m ()
    font :: Text -> m ()
    textCenter :: m ()
    textMiddle :: m ()
    beginPath :: m ()
    closePath :: m ()
    moveTo :: (Double, Double) -> m ()
    lineTo :: (Double, Double) -> m ()
    quadraticCurveTo :: (Double, Double) -> (Double, Double) -> m ()
    bezierCurveTo ::
           (Double, Double) -> (Double, Double) -> (Double, Double) -> m ()
    arc :: Double -> Double -> Double -> Double -> Double -> Bool -> m ()
    rect :: Double -> Double -> Double -> Double -> m ()
    fill :: m ()
    stroke :: m ()
    fillRect :: Double -> Double -> Double -> Double -> m ()
    fillText :: Text -> (Double, Double) -> m ()
    measureText :: Text -> m Double
    isPointInPath :: (Double, Double) -> m Bool
    isPointInStroke :: (Double, Double) -> m Bool

saveRestore :: MonadCanvas m => m a -> m a
saveRestore m = do
    save
    r <- m
    restore
    return r

#ifdef ghcjs_HOST_OS

data CanvasM a = CanvasM
    { unCanvasM :: Canvas.Context -> IO a
    } deriving (Functor)

runCanvasM :: Canvas.Context -> CanvasM a -> IO a
runCanvasM = flip unCanvasM

instance Applicative CanvasM where
    pure x = CanvasM (const (return x))
    f <*> x = CanvasM (\ctx -> unCanvasM f ctx <*> unCanvasM x ctx)

instance Monad CanvasM where
    return = pure
    m >>= f = CanvasM (\ctx -> unCanvasM m ctx >>= ($ ctx) . unCanvasM . f)

foreign import javascript "$2.globalCompositeOperation = $1;"
               js_globalCompositeOperation :: JSString -> Canvas.Context -> IO ()

foreign import javascript "$r = $3.isPointInPath($1, $2);"
               js_isPointInPath :: Double -> Double -> Canvas.Context -> IO Bool

foreign import javascript "$r = $3.isPointInStroke($1, $2);"
               js_isPointInStroke :: Double -> Double -> Canvas.Context -> IO Bool

instance MonadIO CanvasM where
    liftIO = CanvasM . const

instance MonadCanvas CanvasM where
    type Image CanvasM = Canvas.Canvas
    save = CanvasM Canvas.save
    restore = CanvasM Canvas.restore
    transform a b c d e f = CanvasM (Canvas.transform a b c d e f)
    translate x y = CanvasM (Canvas.translate x y)
    scale x y = CanvasM (Canvas.scale x y)
    newImage w h m =
        CanvasM $
        const $ do
            buf <- Canvas.create w h
            ctx <- Canvas.getContext buf
            a <- unCanvasM m ctx
            return (buf, a)
    drawImage (Canvas.Canvas c) x y w h =
        CanvasM (Canvas.drawImage (Canvas.Image c) x y w h)
    globalCompositeOperation op =
        CanvasM (js_globalCompositeOperation (textToJSString op))
    lineWidth w = CanvasM (Canvas.lineWidth w)
    strokeColor r g b a = CanvasM (Canvas.strokeStyle r g b a)
    fillColor r g b a = CanvasM (Canvas.fillStyle r g b a)
    font t = CanvasM (Canvas.font (textToJSString t))
    textCenter = CanvasM (Canvas.textAlign Canvas.Center)
    textMiddle = CanvasM (Canvas.textBaseline Canvas.Middle)
    beginPath = CanvasM Canvas.beginPath
    closePath = CanvasM Canvas.closePath
    moveTo (x, y) = CanvasM (Canvas.moveTo x y)
    lineTo (x, y) = CanvasM (Canvas.lineTo x y)
    quadraticCurveTo (x1, y1) (x2, y2) =
        CanvasM (Canvas.quadraticCurveTo x1 y1 x2 y2)
    bezierCurveTo (x1, y1) (x2, y2) (x3, y3) =
        CanvasM (Canvas.bezierCurveTo x1 y1 x2 y2 x3 y3)
    arc x y r a1 a2 dir = CanvasM (Canvas.arc x y r a1 a2 dir)
    rect x y w h = CanvasM (Canvas.rect x y w h)
    fill = CanvasM Canvas.fill
    stroke = CanvasM Canvas.stroke
    fillRect x y w h = CanvasM (Canvas.fillRect x y w h)
    fillText t (x, y) = CanvasM (Canvas.fillText (textToJSString t) x y)
    measureText t = CanvasM (Canvas.measureText (textToJSString t))
    isPointInPath (x, y) = CanvasM (js_isPointInPath x y)
    isPointInStroke (x, y) = CanvasM (js_isPointInStroke x y)

#else

type CanvasM = Canvas

runCanvasM :: Canvas.DeviceContext -> CanvasM a -> IO a
runCanvasM = Canvas.send

instance MonadCanvas Canvas where
    type Image Canvas = Canvas.CanvasContext
    save = Canvas.save ()
    restore = Canvas.restore ()
    transform a b c d e f = Canvas.transform (a, b, c, d, e, f)
    translate x y = Canvas.translate (x, y)
    scale x y = Canvas.scale (x, y)
    newImage w h m = do
        ctx <- Canvas.newCanvas (w, h)
        a <- Canvas.with ctx m
        return (ctx, a)
    drawImage img x y w h =
        Canvas.drawImageSize
            ( img
            , fromIntegral x
            , fromIntegral y
            , fromIntegral w
            , fromIntegral h)
    globalCompositeOperation op = Canvas.globalCompositeOperation op
    lineWidth w = Canvas.lineWidth w
    strokeColor r g b a =
        Canvas.strokeStyle
            (pack
                 (printf
                      "rgba(%.0f,%.0f,%.0f,%f)"
                      (r * 255)
                      (g * 255)
                      (b * 255)
                      a))
    fillColor r g b a =
        Canvas.fillStyle
            (pack
                 (printf
                      "rgba(%.0f,%.0f,%.0f,%f)"
                      (r * 255)
                      (g * 255)
                      (b * 255)
                      a))
    font t = Canvas.font t
    textCenter = Canvas.textAlign Canvas.CenterAnchor
    textMiddle = Canvas.textBaseline Canvas.MiddleBaseline
    beginPath = Canvas.beginPath ()
    closePath = Canvas.closePath ()
    moveTo (x, y) = Canvas.moveTo (x, y)
    lineTo (x, y) = Canvas.lineTo (x, y)
    quadraticCurveTo (x1, y1) (x2, y2) =
        Canvas.quadraticCurveTo (x1, y1, x2, y2)
    bezierCurveTo (x1, y1) (x2, y2) (x3, y3) =
        Canvas.bezierCurveTo (x1, y1, x2, y2, x3, y3)
    arc x y r a1 a2 dir = Canvas.arc (x, y, r, a1, a2, dir)
    rect x y w h = Canvas.rect (x, y, w, h)
    fill = Canvas.fill ()
    stroke = Canvas.stroke ()
    fillRect x y w h = Canvas.fillRect (x, y, w, h)
    fillText t (x, y) = Canvas.fillText (t, x, y)
    measureText t = do
        Canvas.TextMetrics w <- Canvas.measureText t
        return w
    isPointInPath (x, y) = Canvas.isPointInPath (x, y)
    isPointInStroke (x, y) = return False

#endif
