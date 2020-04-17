{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans -Wno-unticked-promoted-constructors #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
module CodeWorld.Driver where

import qualified CodeWorld.CanvasM as CM
import CodeWorld.Color
import CodeWorld.DrawState
import CodeWorld.Event
import CodeWorld.Picture
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Bool
import Data.Char (chr)
import Data.Dependent.Sum
import Data.Foldable
import Data.IORef
import Data.List (zip4, intercalate)
import Data.Maybe
import Data.Serialize
import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Witherable
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.Stack
import GHC.StaticPtr
import Numeric (showFFloatAlt)
import qualified Reflex as R
import qualified Reflex.Host.Class as R
import System.IO.Unsafe
import System.Mem.StableName
import System.Random
import Text.Printf
import Text.Read

#ifdef ghcjs_HOST_OS

import CodeWorld.CanvasM (MonadCanvas, CanvasM, runCanvasM)
import CodeWorld.CollaborationUI (SetupPhase(..), Step(..), UIState)
import qualified CodeWorld.CollaborationUI as CUI
import CodeWorld.Message
import CodeWorld.Prediction
import Control.DeepSeq
import Control.Monad.Identity
import qualified Control.Monad.Trans.State as State
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Hashable
import qualified Data.JSString
import qualified GHCJS.DOM.ClientRect as ClientRect
import GHCJS.Concurrent (withoutPreemption)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers hiding (error, keyPress)
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Types (Window, Element, unElement)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import JavaScript.Object
import JavaScript.Web.AnimationFrame
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas
import qualified JavaScript.Web.Location as Loc
import qualified JavaScript.Web.MessageEvent as WS
import qualified JavaScript.Web.Performance as Performance
import qualified JavaScript.Web.WebSocket as WS
import Unsafe.Coerce

#else

import CodeWorld.CanvasM (MonadCanvas, runCanvasM)
import Data.Time.Clock
import qualified Graphics.Blank as Canvas
import System.Environment

#endif

-- | Applies the affine transformation from the DrawState and prepares to draw
-- with it.  This does not set the color at the same time, because different
-- pictures need to apply the color, if any, in different ways, often outside of
-- the action that sets up the geometry.
withDS :: MonadCanvas m => DrawState -> m a -> m a
withDS (DrawState (AffineTransformation ta tb tc td te tf) _col) action =
    CM.saveRestore $ do
        CM.transform ta tb tc td te tf
        CM.beginPath
        action

setColor :: MonadCanvas m => Color -> m ()
setColor (RGBA r g b a) = do
    CM.strokeColor
        (round $ r * 255)
        (round $ g * 255)
        (round $ b * 255)
        a
    CM.fillColor
        (round $ r * 255)
        (round $ g * 255)
        (round $ b * 255)
        a

applyColor :: MonadCanvas m => DrawState -> m ()
applyColor ds = case getColorDS ds of
    Nothing -> setColor (RGBA 0 0 0 1)
    Just c -> setColor c

-- | A slower way to draw a picture, which has some useful properties.  It
-- can draw images in non-standard colors, and apply transparent colors
-- properly to overlapping compositions of basic shapes.  There must be a
-- color in the DrawState.
viaOffscreen :: MonadCanvas m => Color -> (Color -> m ()) -> m ()
viaOffscreen (RGBA r g b a) pic = do
    w <- CM.getScreenWidth
    h <- CM.getScreenHeight
    when (w > 0.5 && h > 0.5) $ do
        img <- CM.newImage (round w) (round h)
        CM.withImage img $ do
            setupScreenContext (round w) (round h)
            pic (RGBA r g b 1)
        CM.saveRestore $ do
            px <- pixelSize
            CM.scale px (-px)
            CM.globalAlpha a
            CM.drawImage img (round (-w/2)) (round (-h/2)) (round w) (round h)

followPath :: MonadCanvas m => [Point] -> Bool -> Bool -> m ()
followPath [] _ _ = return ()
followPath [_] _ _ = return ()
followPath ((sx, sy):ps) closed False = do
    CM.moveTo (sx, sy)
    forM_ ps $ \(x, y) -> CM.lineTo (x, y)
    when closed CM.closePath
followPath [p1, p2] False True = followPath [p1, p2] False False
followPath ps False True = do
    let [p1@(x1, y1), p2@(x2, y2), p3@(x3, y3)] = take 3 ps
        dprev = euclideanDistance p1 p2
        dnext = euclideanDistance p2 p3
        p = dprev / (dprev + dnext)
        cx = x2 + p * (x1 - x3) / 2
        cy = y2 + p * (y1 - y3) / 2
    CM.moveTo (x1, y1)
    CM.quadraticCurveTo (cx, cy) (x2, y2)
    forM_ (zip4 ps (tail ps) (tail $ tail ps) (tail $ tail $ tail ps)) $ \(p1@(x1, y1), p2@(x2, y2), p3@(x3, y3), p4@(x4, y4)) -> do
        let dp = euclideanDistance p1 p2
            d1 = euclideanDistance p2 p3
            d2 = euclideanDistance p3 p4
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        CM.bezierCurveTo
            (cx1, cy1)
            (cx2, cy2)
            (x3,  y3)
    let [p1@(x1, y1), p2@(x2, y2), p3@(x3, y3)] = reverse $ take 3 $ reverse ps
        dp = euclideanDistance p1 p2
        d1 = euclideanDistance p2 p3
        r = d1 / (dp + d1)
        cx = x2 + r * (x3 - x1) / 2
        cy = y2 + r * (y3 - y1) / 2
    CM.quadraticCurveTo (cx, cy) (x3, y3)
followPath ps@(_:(sx, sy):_) True True = do
    CM.moveTo (sx, sy)
    let rep = cycle ps
    forM_ (zip4 ps (tail rep) (tail $ tail rep) (tail $ tail $ tail rep)) $ \(p1@(x1, y1), p2@(x2, y2), p3@(x3, y3), p4@(x4, y4)) -> do
        let dp = euclideanDistance p1 p2
            d1 = euclideanDistance p2 p3
            d2 = euclideanDistance p3 p4
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        CM.bezierCurveTo
            (cx1, cy1)
            (cx2, cy2)
            (x3,  y3)
    CM.closePath

euclideanDistance :: Point -> Point -> Double
euclideanDistance (x1, y1) (x2, y2) = sqrt $ square (x2 - x1) + square (y2 - y1)
  where
    square x = x * x

drawFigure :: MonadCanvas m => DrawState -> Double -> m () -> m ()
drawFigure ds w figure = do
    withDS ds $ do
        figure
        when (w /= 0) $ do
            CM.lineWidth w
            applyColor ds
            CM.stroke
    when (w == 0) $ do
        CM.lineWidth =<< pixelSize
        applyColor ds
        CM.stroke

fillFigure :: MonadCanvas m => DrawState -> m () -> m ()
fillFigure ds figure = do
    withDS ds figure
    applyColor ds
    CM.fill

--------------------------------------------------------------------------------

drawPicture :: MonadCanvas m => Picture -> DrawState -> m ()
drawPicture (SolidClosedCurve _ pts) ds = drawPolygon pts True ds
drawPicture (SolidPolygon _ pts) ds = drawPolygon pts False ds
drawPicture (Polygon _ pts) ds = drawPath pts 0 True False ds
drawPicture (ThickPolygon _ pts w) ds = drawPath pts w True False ds
drawPicture (Rectangle _ w h) ds = drawPath (rectangleVertices w h) 0 True False ds
drawPicture (SolidRectangle _ w h) ds = drawPolygon (rectangleVertices w h) False ds
drawPicture (ThickRectangle _ lw w h) ds = drawPath (rectangleVertices w h) lw True False ds
drawPicture (ClosedCurve _ pts) ds = drawPath pts 0 True True ds
drawPicture (ThickClosedCurve _ pts w) ds = drawPath pts w True True ds
drawPicture (Circle _ r) ds = drawArc 0 (2 * pi) r 0 ds
drawPicture (SolidCircle _ r) ds = drawSector 0 (2 * pi) r ds
drawPicture (ThickCircle _ lw r) ds = drawArc 0 (2 * pi) r lw ds
drawPicture (Polyline _ pts) ds = drawPath pts 0 False False ds
drawPicture (ThickPolyline _ pts w) ds = drawPath pts w False False ds
drawPicture (Curve _ pts) ds = drawPath pts 0 False True ds
drawPicture (ThickCurve _ pts w) ds = drawPath pts w False True ds
drawPicture (Sector _ b e r) ds = drawSector b e r ds
drawPicture (Arc _ b e r) ds = drawArc b e r 0 ds
drawPicture (ThickArc _ b e r w) ds = drawArc b e r w ds
drawPicture (Lettering _ txt) ds = drawText Plain Serif txt ds
drawPicture (Blank _) _ = return ()
drawPicture (StyledLettering _ sty fnt txt) ds = drawText sty fnt txt ds
drawPicture (Sketch _ name url w h) ds = drawImage name url w h ds
drawPicture (CoordinatePlane _) ds = drawPicture coordinatePlanePic ds
drawPicture (Color _ col p) ds
  | isSimplePic p || isOpaque col = drawPicture p (setColorDS col ds)
  | otherwise = viaOffscreen col $ \c -> drawPicture p (setColorDS c ds)
drawPicture (Translate _ x y p) ds = drawPicture p (translateDS x y ds)
drawPicture (Scale _ x y p) ds = drawPicture p (scaleDS x y ds)
drawPicture (Dilate _ k p) ds = drawPicture p (scaleDS k k ds)
drawPicture (Rotate _ r p) ds = drawPicture p (rotateDS r ds)
drawPicture (Reflect _ r p) ds = drawPicture p (reflectDS r ds)
drawPicture (Clip _ x y p) ds = do
    withDS ds $ followPath (rectangleVertices x y) True False
    CM.saveRestore $ CM.clip >> drawPicture p ds
drawPicture (Pictures _ ps) ds = forM_ (reverse ps) $ \p -> drawPicture p ds
drawPicture (PictureAnd _ ps) ds = forM_ (reverse ps) $ \p -> drawPicture p ds

pictureContains :: MonadCanvas m => Picture -> DrawState -> Point -> m Bool
pictureContains (SolidClosedCurve _ pts) ds pt = polygonContains pts True ds pt
pictureContains (SolidPolygon _ pts) ds pt = polygonContains pts False ds pt
pictureContains (Polygon _ pts) ds pt = pathContains pts 0 True False ds pt
pictureContains (ThickPolygon _ pts w) ds pt = pathContains pts w True False ds pt
pictureContains (Rectangle _ w h) ds pt = pathContains (rectangleVertices w h) 0 True False ds pt
pictureContains (SolidRectangle _ w h) ds pt = polygonContains (rectangleVertices w h) False ds pt
pictureContains (ThickRectangle _ lw w h) ds pt = pathContains (rectangleVertices w h) lw True False ds pt
pictureContains (ClosedCurve _ pts) ds pt = pathContains pts 0 True True ds pt
pictureContains (ThickClosedCurve _ pts w) ds pt = pathContains pts w True True ds pt
pictureContains (Circle _ r) ds pt = arcContains 0 (2 * pi) r 0 ds pt
pictureContains (SolidCircle _ r) ds pt = sectorContains 0 (2 * pi) r ds pt
pictureContains (ThickCircle _ lw r) ds pt = arcContains 0 (2 * pi) r lw ds pt
pictureContains (Polyline _ pts) ds pt = pathContains pts 0 False False ds pt
pictureContains (ThickPolyline _ pts w) ds pt = pathContains pts w False False ds pt
pictureContains (Curve _ pts) ds pt = pathContains pts 0 False True ds pt
pictureContains (ThickCurve _ pts w) ds pt = pathContains pts w False True ds pt
pictureContains (Sector _ b e r) ds pt = sectorContains b e r ds pt
pictureContains (Arc _ b e r) ds pt = arcContains b e r 0 ds pt
pictureContains (ThickArc _ b e r w) ds pt = arcContains b e r w ds pt
pictureContains (Lettering _ txt) ds pt = textContains Plain Serif txt ds pt
pictureContains (Blank _) _ _ = return False
pictureContains (StyledLettering _ sty fnt txt) ds pt = textContains sty fnt txt ds pt
pictureContains (Sketch _ name url w h) ds pt = imageContains name url w h ds pt
pictureContains (CoordinatePlane _) ds pt = pictureContains coordinatePlanePic ds pt
pictureContains (Color _ _ p) ds pt = pictureContains p ds pt
pictureContains (Translate _ x y p) ds pt = pictureContains p (translateDS x y ds) pt
pictureContains (Scale _ x y p) ds pt = pictureContains p (scaleDS x y ds) pt
pictureContains (Dilate _ k p) ds pt = pictureContains p (scaleDS k k ds) pt
pictureContains (Rotate _ r p) ds pt = pictureContains p (rotateDS r ds) pt
pictureContains (Reflect _ r p) ds pt = pictureContains p (reflectDS r ds) pt
pictureContains (Clip _ x y p) ds pt =
    (&&) <$> polygonContains (rectangleVertices x y) False ds pt
         <*> pictureContains p ds pt
pictureContains (Pictures _ ps) ds pt = orM [pictureContains p ds pt | p <- ps]
pictureContains (PictureAnd _ ps) ds pt = orM [pictureContains p ds pt | p <- ps]

isSimplePic :: Picture -> Bool
isSimplePic (Pictures _ []) = True
isSimplePic (Pictures _ [p]) = isSimplePic p
isSimplePic (Pictures _ _) = False
isSimplePic (PictureAnd _ []) = True
isSimplePic (PictureAnd _ [p]) = isSimplePic p
isSimplePic (PictureAnd _ _) = False
isSimplePic (Translate _ _ _ p) = isSimplePic p
isSimplePic (Scale _ _ _ p) = isSimplePic p
isSimplePic (Dilate _ _ p) = isSimplePic p
isSimplePic (Rotate _ _ p) = isSimplePic p
isSimplePic (Reflect _ _ p) = isSimplePic p
isSimplePic (Clip _ _ _ p) = isSimplePic p
isSimplePic (Color _ c p) = not (isOpaque c) || isSimplePic p
isSimplePic _ = True

isOpaque :: Color -> Bool
isOpaque (RGBA _ _ _ 1) = True
isOpaque _ = False

drawPolygon :: MonadCanvas m => [Point] -> Bool -> DrawState -> m ()
drawPolygon ps smooth ds = fillFigure ds $ followPath ps True smooth

polygonContains :: MonadCanvas m => [Point] -> Bool -> DrawState -> Point -> m Bool
polygonContains ps smooth ds p = do
    withDS ds $ followPath ps True smooth
    CM.isPointInPath p

drawPath :: MonadCanvas m => [Point] -> Double -> Bool -> Bool -> DrawState -> m ()
drawPath ps w closed smooth ds = drawFigure ds w $ followPath ps closed smooth

pathContains :: MonadCanvas m => [Point] -> Double -> Bool -> Bool -> DrawState -> Point -> m Bool
pathContains ps w closed smooth ds p = do
    s <- pixelSize
    drawFigure ds (max s w) $ followPath ps closed smooth
    CM.isPointInStroke p

drawSector :: MonadCanvas m => Double -> Double -> Double -> DrawState -> m ()
drawSector b e r ds =
    fillFigure ds $ CM.arc 0 0 (abs r) b e (b > e) >> CM.lineTo (0, 0)

sectorContains :: MonadCanvas m => Double -> Double -> Double -> DrawState -> Point -> m Bool
sectorContains b e r ds p = do
    withDS ds $ CM.arc 0 0 (abs r) b e (b > e) >> CM.lineTo (0, 0)
    CM.isPointInPath p

drawArc :: MonadCanvas m => Double -> Double -> Double -> Double -> DrawState -> m ()
drawArc b e r w ds =
    drawFigure ds w $ CM.arc 0 0 (abs r) b e (b > e)

arcContains :: MonadCanvas m => Double -> Double -> Double -> Double -> DrawState -> Point -> m Bool
arcContains b e r w ds p = do
    s <- pixelSize
    let width = max s w
    CM.lineWidth width
    drawFigure ds width $
        CM.arc 0 0 (abs r) b e (b > e)
    CM.isPointInStroke p

drawText :: MonadCanvas m => TextStyle -> Font -> Text -> DrawState -> m ()
drawText sty fnt txt ds = withDS ds $ do
    CM.scale (1/25) (-1/25)
    applyColor ds
    CM.font (fontString sty fnt)
    CM.fillText txt (0, 0)

textContains :: MonadCanvas m => TextStyle -> Font -> Text -> DrawState -> Point -> m Bool
textContains sty fnt txt ds p = do
    CM.font (fontString sty fnt)
    width <- (/ 25) <$> CM.measureText txt
    let height = 1 -- constant, defined in fontString
    withDS ds $ CM.rect ((-0.5) * width) ((-0.5) * height) width height
    CM.isPointInPath p

fontString :: TextStyle -> Font -> Text
fontString style font = stylePrefix style <> "25px " <> fontName font
  where
    stylePrefix Plain = ""
    stylePrefix Bold = "bold "
    stylePrefix Italic = "italic "

    fontName SansSerif = "sans-serif"
    fontName Serif = "serif"
    fontName Monospace = "monospace"
    fontName Handwriting = "cursive"
    fontName Fancy = "fantasy"
    fontName (NamedFont txt) = "\"" <> T.filter (/= '"') txt <> "\""

drawImage :: MonadCanvas m => Text -> Text -> Double -> Double -> DrawState -> m ()
drawImage name url imgw imgh ds = case getColorDS ds of
    -- Fast path: draw in original color.
    Nothing -> withDS ds $ do
        CM.scale 1 (-1)
        CM.drawImgURL name url imgw imgh

    -- Slow path: draw in a different color via an offscreen canvas.
    Just oc -> viaOffscreen oc $ \c -> do
        setColor c
        w <- CM.getScreenWidth
        h <- CM.getScreenHeight
        CM.fillRect (-w/2) (-h/2) w h
        CM.globalCompositeOperation "destination-in"
        withDS ds $ do
            CM.scale 1 (-1)
            CM.drawImgURL name url imgw imgh

imageContains :: MonadCanvas m => Text -> Text -> Double -> Double -> DrawState -> Point -> m Bool
imageContains _ _ imgw imgh ds p = withDS ds $ do
    CM.rect (-imgw / 2) (-imgh / 2) imgw imgh
    CM.isPointInPath p

coordinatePlanePic :: Picture
coordinatePlanePic = axes <> numbers <> guidelines
  where
    xline y = colored (RGBA 0 0 0 0.25) $ polyline [(-10, y), (10, y)]
    xaxis = colored (RGBA 0 0 0 0.75) $ polyline [(-10, 0), (10, 0)]
    axes = xaxis <> rotated (pi / 2) xaxis
    xguidelines = pictures [xline k | k <- [-10,-9 .. 10]]
    guidelines = xguidelines <> rotated (pi / 2) xguidelines
    numbers = xnumbers <> ynumbers
    xnumbers =
        pictures
            [ translated
                (fromIntegral k)
                0.3
                (scaled 0.5 0.5 (lettering (T.pack (show k))))
            | k <- [-9,-8 .. 9]
            , k /= (0 :: Int)
            ]
    ynumbers =
        pictures
            [ translated
                0.3
                (fromIntegral k)
                (scaled 0.5 0.5 (lettering (T.pack (show k))))
            | k <- [-9,-8 .. 9]
            , k /= (0 :: Int)
            ]

--------------------------------------------------------------------------------

clearScreen :: MonadCanvas m => m ()
clearScreen = do
    w <- CM.getScreenWidth
    h <- CM.getScreenHeight
    px <- pixelSize
    CM.fillColor 255 255 255 1
    CM.fillRect (-w/2 * px) (-h/2 * px) (w * px) (h * px)

drawFrame :: MonadCanvas m => Picture -> m ()
drawFrame pic = clearScreen >> drawPicture pic initialDS

pixelSize :: MonadCanvas m => m Double
pixelSize = do
    cw <- CM.getScreenWidth
    ch <- CM.getScreenHeight
    return $ max (20 / realToFrac cw) (20 / realToFrac ch)

setupScreenContext :: MonadCanvas m => Int -> Int -> m ()
setupScreenContext cw ch = do
    CM.translate (realToFrac cw / 2) (realToFrac ch / 2)
    s <- pixelSize
    CM.scale (1/s) (-1/s)
    CM.lineWidth 0
    CM.textCenter
    CM.textMiddle

--------------------------------------------------------------------------------

-- A NodeId a unique id for each node in a Picture, chosen by the order the node
-- appears in DFS.  Always >=0.
newtype NodeId = NodeId { getNodeId :: Int}
    deriving (Eq, Ord, Enum, Show)

getChildNodes :: Picture -> [Picture]
getChildNodes (Color _ _ p) = [p]
getChildNodes (Translate _ _ _ p) = [p]
getChildNodes (Scale _ _ _ p) = [p]
getChildNodes (Dilate _ _ p) = [p]
getChildNodes (Rotate _ _ p) = [p]
getChildNodes (Reflect _ _ p) = [p]
getChildNodes (Clip _ _ _ p) = [p]
getChildNodes (Pictures _ ps) = ps
getChildNodes (PictureAnd _ ps) = ps
getChildNodes _ = []

findTopShape :: MonadCanvas m => DrawState -> Picture -> Double -> Double -> m (Maybe NodeId)
findTopShape ds pic x y = do
    (found, n) <- searchSingle ds pic x y
    return $ if found
        then Just (NodeId n)
        else Nothing
  where
    searchSingle ds (Color _ _ p) x y =
        fmap (+ 1) <$> searchSingle ds p x y
    searchSingle ds (Translate _ dx dy p) x y =
        fmap (+ 1) <$> searchSingle (translateDS dx dy ds) p x y
    searchSingle ds (Scale _ sx sy p) x y =
        fmap (+ 1) <$> searchSingle (scaleDS sx sy ds) p x y
    searchSingle ds (Dilate _ k p) x y =
        fmap (+ 1) <$> searchSingle (scaleDS k k ds) p x y
    searchSingle ds (Rotate _ a p) x y =
        fmap (+ 1) <$> searchSingle (rotateDS a ds) p x y
    searchSingle ds (Reflect _ a p) x y =
        fmap (+ 1) <$> searchSingle (reflectDS a ds) p x y
    searchSingle ds (Clip _ w h p) x y = do
        inClip <- polygonContains (rectangleVertices w h) False ds (x, y)
        fmap (+ 1) <$> if inClip
                       then searchSingle ds p x y
                       else return (False, countNodes p)
    searchSingle ds (Pictures _ ps) x y =
        fmap (+ 1) <$> searchMulti ds ps x y
    searchSingle ds (PictureAnd _ ps) x y =
        fmap (+ 1) <$> searchMulti ds ps x y
    searchSingle ds p x y = do
        contained <- pictureContains p ds (x, y)
        if contained
          then pure (True, 0)
          else pure (False, 1)

    searchMulti _ [] _ _ = return (False, 0)
    searchMulti ds (pic:pics) x y = do
        (found, count) <- searchSingle ds pic x y
        if found
          then pure (True, count)
          else fmap (+ count) <$> searchMulti ds pics x y

    countNodes p = 1 + sum (map countNodes (getChildNodes p))

-- If a picture is found, the result will include an array of the base picture
-- and all transformations.
findTopShapeFromPoint :: MonadCanvas m => Point -> Picture -> m (Maybe NodeId)
findTopShapeFromPoint (x, y) pic = do
    cw <- CM.getScreenWidth
    ch <- CM.getScreenHeight
    img <- CM.newImage (round cw) (round ch)
    CM.withImage img $ do
        setupScreenContext (round cw) (round ch)
        findTopShape initialDS pic x y

trim :: Int -> String -> String
trim x y
  | x >= length y = y
  | otherwise = take mid y ++ "..." ++ reverse (take mid $ reverse y)
  where mid = (x - 3) `div` 2

showFloat :: Bool -> Double -> String
showFloat needNegParens x
  | needNegParens && x < 0 = "(" ++ result ++ ")"
  | otherwise = result
  where result = stripZeros (showFFloatAlt (Just 4) x "")
        stripZeros = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

showPoints :: [Point] -> String
showPoints pts =
    "[" ++
    intercalate ", " [
        "(" ++ showFloat False x ++ ", " ++ showFloat False y ++ ")"
        | (x, y) <- pts
    ] ++
    "]"

showColor :: Color -> String
showColor c@(RGBA r g b a)
  | c == black = "black"
  | c == white = "white"
  | c == red = "red"
  | c == green = "green"
  | c == blue = "blue"
  | c == yellow = "yellow"
  | c == orange = "orange"
  | c == brown = "brown"
  | c == pink = "pink"
  | c == purple = "purple"
  | c == gray = "gray"
  | haskellMode, a == 1 =
      printf "(RGB %s %s %s)" (showFloat True r) (showFloat True g) (showFloat True b)
  | a == 1 =
      printf "RGB(%s, %s, %s)" (showFloat False r) (showFloat False g) (showFloat False b)
  | haskellMode =
      printf "(RGBA %s %s %s %s)" (showFloat True r) (showFloat True g) (showFloat True b) (showFloat True a)
  | otherwise =
      printf "RGBA(%s, %s, %s, %s)" (showFloat False r) (showFloat False g) (showFloat False b) (showFloat False a)

describePicture :: Picture -> String
describePicture (Rectangle _ w h)
  | haskellMode = printf "rectangle %s %s" (showFloat True w) (showFloat True h)
  | otherwise   = printf "rectangle(%s, %s)" (showFloat False w) (showFloat False h)
describePicture (SolidRectangle _ w h)
  | haskellMode = printf "solidRectangle %s %s" (showFloat True w) (showFloat True h)
  | otherwise   = printf "solidRectangle(%s, %s)" (showFloat False w) (showFloat False h)
describePicture (ThickRectangle _ lw w h)
  | haskellMode = printf "thickRectangle %s %s %s" (showFloat True lw) (showFloat True w) (showFloat True h)
  | otherwise   = printf "thickRectangle(%s, %s, %s)" (showFloat False w) (showFloat False h) (showFloat False lw)
describePicture (Circle _ r)
  | haskellMode = printf "circle %s" (showFloat True r)
  | otherwise   = printf "circle(%s)" (showFloat False r)
describePicture (SolidCircle _ r)
  | haskellMode = printf "solidCircle %s" (showFloat True r)
  | otherwise   = printf "solidCircle(%s)" (showFloat False r)
describePicture (ThickCircle _ lw r)
  | haskellMode = printf "thickCircle %s %s" (showFloat True lw) (showFloat True r)
  | otherwise   = printf "thickCircle(%s, %s)" (showFloat False r) (showFloat False lw)
describePicture (SolidPolygon _ pts)
  | haskellMode = printf "solidPolygon %s" (showPoints pts)
  | otherwise   = printf "solidPolygon(%s)" (showPoints pts)
describePicture (SolidClosedCurve _ pts)
  | haskellMode = printf "solidClosedCurve %s" (showPoints pts)
  | otherwise   = printf "solidClosedCurve(%s)" (showPoints pts)
describePicture (Polygon _ pts)
  | haskellMode = printf "polygon %s" (showPoints pts)
  | otherwise   = printf "polygon(%s)" (showPoints pts)
describePicture (ThickPolygon _ pts w)
  | haskellMode = printf "thickPolygon %s %s" (showFloat True w) (showPoints pts)
  | otherwise   = printf "thickPolygon(%s, %s)" (showPoints pts) (showFloat False w)
describePicture (ClosedCurve _ pts)
  | haskellMode = printf "closedCurve %s" (showPoints pts)
  | otherwise   = printf "closedCurve(%s)" (showPoints pts)
describePicture (ThickClosedCurve _ pts w)
  | haskellMode = printf "thickClosedCurve %s %s" (showFloat True w) (showPoints pts)
  | otherwise   = printf "thickClosedCurve(%s, %s)" (showPoints pts) (showFloat False w)
describePicture (Polyline _ pts)
  | haskellMode = printf "polyline %s" (showPoints pts)
  | otherwise   = printf "polyline(%s)" (showPoints pts)
describePicture (ThickPolyline _ pts w)
  | haskellMode = printf "thickPolyline %s %s" (showFloat True w) (showPoints pts)
  | otherwise   = printf "thickPolyline(%s, %s)" (showPoints pts) (showFloat False w)
describePicture (Curve _ pts)
  | haskellMode = printf "curve %s" (showPoints pts)
  | otherwise   = printf "curve(%s)" (showPoints pts)
describePicture (ThickCurve _ pts w)
  | haskellMode = printf "thickCurve %s %s" (showFloat True w) (showPoints pts)
  | otherwise   = printf "thickCurve(%s, %s)" (showPoints pts) (showFloat False w)
describePicture (Sector _ b e r)
  | haskellMode = printf "sector %s %s %s" (showFloat True b) (showFloat True e) (showFloat True r)
  | otherwise   = printf "sector(%s°, %s°, %s)" (showFloat False (180 * b / pi)) (showFloat False (180 * e / pi)) (showFloat False r)
describePicture (Arc _ b e r)
  | haskellMode = printf "arc %s %s %s" (showFloat True b) (showFloat True e) (showFloat True r)
  | otherwise   = printf "arc(%s°, %s°, %s)" (showFloat False (180 * b / pi)) (showFloat False (180 * e / pi)) (showFloat False r)
describePicture (ThickArc _ b e r w)
  | haskellMode = printf "thickArc %s %s %s %s" (showFloat True w) (showFloat True b) (showFloat True e) (showFloat True r)
  | otherwise   = printf "thickArc(%s°, %s°, %s, %s)" (showFloat False (180 * b / pi)) (showFloat False (180 * e / pi)) (showFloat False r) (showFloat False w)
describePicture (Lettering _ txt)
  | haskellMode = printf "lettering %s" (show txt)
  | otherwise   = printf "lettering(%s)" (show txt)
describePicture (Blank _) = "blank"
describePicture (StyledLettering _ style font txt)
  | haskellMode = printf "styledLettering %s %s %s" (showsPrec 10 style "") (showsPrec 10 font "") (show txt)
  | otherwise   = printf "styledLettering(%s, %s, %s)" (show txt) (show font) (show style)
describePicture (Color _ c _)
  | haskellMode = printf "colored %s" (showColor c)
  | otherwise   = printf "colored(..., %s)" (showColor c)
describePicture (Translate _ x y _)
  | haskellMode = printf "translated %s %s" (showFloat True x) (showFloat True y)
  | otherwise   = printf "translated(..., %s, %s)" (showFloat False x) (showFloat False y)
describePicture (Scale _ x y _)
  | haskellMode = printf "scaled %s %s" (showFloat True x) (showFloat True y)
  | otherwise   = printf "scaled(..., %s, %s)" (showFloat False x) (showFloat False y)
describePicture (Rotate _ angle _)
  | haskellMode = printf "rotated %s" (showFloat True angle)
  | otherwise   = printf "rotated(..., %s°)" (showFloat False (180 * angle / pi))
describePicture (Reflect _ angle _)
  | haskellMode = printf "reflected %s" (showFloat True angle)
  | otherwise   = printf "reflected(..., %s°)" (showFloat False (180 * angle / pi))
describePicture (Clip _ x y _)
  | haskellMode = printf "clipped %s %s" (showFloat True x) (showFloat True y)
  | otherwise   = printf "rotated(..., %s, %s)" (showFloat False x) (showFloat False y)
describePicture (Dilate _ k _)
  | haskellMode = printf "dilated %s" (showFloat True k)
  | otherwise   = printf "dilated(..., %s)" (showFloat False k)
describePicture (Sketch _ name _ _ _) = T.unpack name
describePicture (CoordinatePlane _) = "coordinatePlane"
describePicture (Pictures _ _)
  | haskellMode = "pictures"
  | otherwise   = "pictures(...)"
describePicture (PictureAnd _ _)
  | haskellMode = "(&)"
  | otherwise   = "... & ..."

getPictureSrcLoc :: Picture -> Maybe SrcLoc
getPictureSrcLoc (SolidPolygon loc _) = loc
getPictureSrcLoc (SolidClosedCurve loc _) = loc
getPictureSrcLoc (Polygon loc _) = loc
getPictureSrcLoc (ThickPolygon loc _ _) = loc
getPictureSrcLoc (Rectangle loc _ _) = loc
getPictureSrcLoc (SolidRectangle loc _ _) = loc
getPictureSrcLoc (ThickRectangle loc _ _ _) = loc
getPictureSrcLoc (ClosedCurve loc _) = loc
getPictureSrcLoc (ThickClosedCurve loc _ _) = loc
getPictureSrcLoc (Circle loc _) = loc
getPictureSrcLoc (SolidCircle loc _) = loc
getPictureSrcLoc (ThickCircle loc _ _) = loc
getPictureSrcLoc (Polyline loc _) = loc
getPictureSrcLoc (ThickPolyline loc _ _) = loc
getPictureSrcLoc (Curve loc _) = loc
getPictureSrcLoc (ThickCurve loc _ _) = loc
getPictureSrcLoc (Sector loc _ _ _) = loc
getPictureSrcLoc (Arc loc _ _ _) = loc
getPictureSrcLoc (ThickArc loc _ _ _ _) = loc
getPictureSrcLoc (Lettering loc _) = loc
getPictureSrcLoc (Blank loc) = loc
getPictureSrcLoc (StyledLettering loc _ _ _) = loc
getPictureSrcLoc (Color loc _ _) = loc
getPictureSrcLoc (Translate loc _ _ _) = loc
getPictureSrcLoc (Scale loc _ _ _) = loc
getPictureSrcLoc (Dilate loc _ _) = loc
getPictureSrcLoc (Rotate loc _ _) = loc
getPictureSrcLoc (Reflect loc _ _) = loc
getPictureSrcLoc (Clip loc _ _ _) = loc
getPictureSrcLoc (Sketch loc _ _ _ _) = loc
getPictureSrcLoc (CoordinatePlane loc) = loc
getPictureSrcLoc (Pictures loc _) = loc
getPictureSrcLoc (PictureAnd loc _) = loc

#ifdef ghcjs_HOST_OS

--------------------------------------------------------------------------------
-- GHCJS implementation of drawing

-- Debug Mode logic
foreign import javascript unsafe "$1.drawImage($2, $3, $4, $5, $6);"
    canvasDrawImage :: Canvas.Context -> Element -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.getContext('2d', { alpha: false })"
    getCodeWorldContext :: Canvas.Canvas -> IO Canvas.Context

foreign import javascript unsafe "showCanvas()"
    showCanvas :: IO ()

canvasFromElement :: Element -> Canvas.Canvas
canvasFromElement = Canvas.Canvas . unElement

elementFromCanvas :: Canvas.Canvas -> Element
elementFromCanvas = pFromJSVal . jsval

createFrameRenderer :: Element -> IO (Picture -> IO ())
createFrameRenderer canvas = do
    offscreenCanvas <- Canvas.create 500 500
    screen <- getCodeWorldContext (canvasFromElement canvas)
    return $ \pic -> withoutPreemption $ do
        setCanvasSize (elementFromCanvas offscreenCanvas) canvas
        rect <- getBoundingClientRect canvas
        withScreen (elementFromCanvas offscreenCanvas) rect (drawFrame pic)
        cw <- ClientRect.getWidth rect
        ch <- ClientRect.getHeight rect
        when (cw > 0.5 && ch > 0.5) $
            canvasDrawImage screen (elementFromCanvas offscreenCanvas)
                            0 0 (round cw) (round ch)

getTime :: IO Double
getTime = (/ 1000) <$> Performance.now

nextFrame :: IO Double
nextFrame = (/ 1000) <$> waitForAnimationFrame

data Node = Node
  { nodeId :: NodeId
  , nodeName :: String
  , nodeSrcLoc :: Maybe SrcLoc
  , nodeSubs :: SubNodes
  }

data SubNodes
    = NoSubNodes
    | SubNode Node
    | SubNodes [Node]

instance ToJSON Node where
    toJSON (Node id name srcLoc subs) =
        object $
            ["id" .= getNodeId id , "name" .= name]
            <> srcLoc'
            <> subs'
      where
        srcLoc' = case srcLoc of
            Nothing -> []
            Just loc -> [ "startLine" .= srcLocStartLine loc
                        , "startCol" .= srcLocStartCol loc
                        , "endLine" .= srcLocEndLine loc
                        , "endCol" .= srcLocEndCol loc
                        ]
        subs' = case subs of
            NoSubNodes -> []
            SubNode node -> ["picture" .= node]
            SubNodes nodes -> ["pictures" .= nodes]

pictureToNode :: Picture -> Node
pictureToNode = flip State.evalState (NodeId 0) . go
  where
    go pic = case pic of
        Pictures _ ps -> nodeWithChildren pic ps
        PictureAnd _ ps -> nodeWithChildren pic ps
        Color _ _ p -> nodeWithChild pic p
        Translate _ _ _ p -> nodeWithChild pic p
        Scale _ _ _ p -> nodeWithChild pic p
        Dilate _ _ p -> nodeWithChild pic p
        Rotate _ _ p -> nodeWithChild pic p
        Reflect _ _ p -> nodeWithChild pic p
        Clip _ _ _ p -> nodeWithChild pic p
        SolidPolygon _ _ -> leafNode pic
        SolidClosedCurve _ _ -> leafNode pic
        Polygon _ _ -> leafNode pic
        ThickPolygon _ _ _ -> leafNode pic
        Rectangle _ _ _ -> leafNode pic
        SolidRectangle _ _ _ -> leafNode pic
        ThickRectangle _ _ _ _ -> leafNode pic
        ClosedCurve _ _ -> leafNode pic
        ThickClosedCurve _ _ _ -> leafNode pic
        Polyline _ _ -> leafNode pic
        ThickPolyline _ _ _ -> leafNode pic
        Curve _ _ -> leafNode pic
        ThickCurve _ _ _ -> leafNode pic
        Circle _ _ -> leafNode pic
        SolidCircle _ _ -> leafNode pic
        ThickCircle _ _ _ -> leafNode pic
        Sector _ _ _ _ -> leafNode pic
        Arc _ _ _ _ -> leafNode pic
        ThickArc _ _ _ _ _ -> leafNode pic
        StyledLettering _ _ _ _ -> leafNode pic
        Lettering _ _ -> leafNode pic
        CoordinatePlane _ -> leafNode pic
        Sketch _ _ _ _ _ -> leafNode pic
        Blank _ -> leafNode pic

    nodeWithChildren pic subs = node pic (SubNodes <$> traverse go subs)
    nodeWithChild pic sub = node pic (SubNode <$> go sub)
    leafNode pic = node pic (pure NoSubNodes)

    node pic getSubNodes = do
        nodeId <- State.get <* State.modify' succ
        let nodeName = trim 80 . describePicture $ pic
        let nodeSrcLoc = getPictureSrcLoc pic
        nodeSubs <- getSubNodes
        pure Node{..}

foreign import javascript unsafe "/\\bmode=haskell\\b/.test(location.search)"
    haskellMode :: Bool

withScreen :: Element -> ClientRect.ClientRect -> CanvasM a -> IO a
withScreen canvas rect action = do
    cw <- realToFrac <$> ClientRect.getWidth rect
    ch <- realToFrac <$> ClientRect.getHeight rect
    ctx <- getCodeWorldContext (canvasFromElement canvas)
    runCanvasM (cw, ch) ctx $ CM.saveRestore $ do
        setupScreenContext (round cw) (round ch)
        action

setCanvasSize :: Element -> Element -> IO ()
setCanvasSize target canvas = do
    rect <- getBoundingClientRect canvas
    cx <- ClientRect.getWidth rect
    cy <- ClientRect.getHeight rect
    setAttribute target ("width" :: JSString) (show (round cx :: Int))
    setAttribute target ("height" :: JSString) (show (round cy :: Int))

#else

--------------------------------------------------------------------------------
-- Stand-alone implementation of drawing

haskellMode :: Bool
haskellMode = True

type Port = Int

readPortFromEnv :: String -> Port -> IO Port
readPortFromEnv envName defaultPort = do
    ms <- lookupEnv envName
    return (fromMaybe defaultPort (ms >>= readMaybe))

runBlankCanvas :: (Canvas.DeviceContext -> IO ()) -> IO ()
runBlankCanvas act = do
    port <- readPortFromEnv "CODEWORLD_API_PORT" 3000
    let options =
            (fromIntegral port)
            { Canvas.events =
                  ["mousedown", "mouseup", "mousemove", "keydown", "keyup"]
            }
    putStrLn $ printf "Open me on http://127.0.0.1:%d/" (Canvas.port options)
    Canvas.blankCanvas options $ \context -> do
        putStrLn "Program is starting..."
        act context

#endif

--------------------------------------------------------------------------------
-- Common event handling and core interaction code

keyCodeToText :: Word -> Text
keyCodeToText n =
    case n of
        _ | n >= 47 && n <= 90 -> fromAscii n
        _ | n >= 96 && n <= 105 -> fromNum (n - 96)
        _ | n >= 112 && n <= 135 -> "F" <> fromNum (n - 111)
        3 -> "Cancel"
        6 -> "Help"
        8 -> "Backspace"
        9 -> "Tab"
        12 -> "5"
        13 -> "Enter"
        16 -> "Shift"
        17 -> "Ctrl"
        18 -> "Alt"
        19 -> "Break"
        20 -> "CapsLock"
        27 -> "Esc"
        32 -> " "
        33 -> "PageUp"
        34 -> "PageDown"
        35 -> "End"
        36 -> "Home"
        37 -> "Left"
        38 -> "Up"
        39 -> "Right"
        40 -> "Down"
        42 -> "*"
        43 -> "+"
        44 -> "PrintScreen"
        45 -> "Insert"
        46 -> "Delete"
        47 -> "Help"
        91 -> "OS"
        92 -> "OS"
        93 -> "ContextMenu"
        106 -> "*"
        107 -> "+"
        108 -> ","
        109 -> "-"
        110 -> "."
        111 -> "/"
        144 -> "NumLock"
        145 -> "ScrollLock"
        173 -> "-"
        186 -> ";"
        187 -> "="
        188 -> ","
        189 -> "-"
        190 -> "."
        191 -> "/"
        192 -> "`"
        193 -> "IntlRo"
        194 -> ","
        219 -> "["
        220 -> "\\"
        221 -> "]"
        222 -> "'"
        225 -> "AltGraph"
        255 -> "IntlYen"
        _ -> "Unknown:" <> fromNum n
  where
    fromAscii n = T.singleton (chr (fromIntegral n))
    fromNum n = T.pack (show n)

isUniversallyConstant :: (a -> s -> s) -> s -> Bool
isUniversallyConstant f old =
    unsafePerformIO $ falseOr $ do
        oldName <- makeStableName $! old
        genName <- makeStableName $! f undefined old
        return (genName == oldName)
  where
    falseOr x = x `catch` \(_ :: SomeException) -> return False

ifDifferent :: (s -> s) -> s -> Maybe s
ifDifferent f s0 = unsafePerformIO $ do
    oldName <- makeStableName $! s0
    newName <- makeStableName $! s1
    if newName == oldName then return Nothing else return (Just s1)
  where s1 = f s0

modifyMVarIfDifferent :: MVar s -> (s -> s) -> IO Bool
modifyMVarIfDifferent var f =
    modifyMVar var $ \s0 ->
        case ifDifferent f s0 of
            Nothing -> return (s0, False)
            Just s1 -> return (s1, True)

data GameToken
    = FullToken { tokenDeployHash :: Text
                , tokenNumPlayers :: Int
                , tokenInitial :: StaticKey
                , tokenStep :: StaticKey
                , tokenEvent :: StaticKey
                , tokenDraw :: StaticKey }
    | SteplessToken { tokenDeployHash :: Text
                    , tokenNumPlayers :: Int
                    , tokenInitial :: StaticKey
                    , tokenEvent :: StaticKey
                    , tokenDraw :: StaticKey }
    | PartialToken { tokenDeployHash :: Text }
    deriving (Generic)

deriving instance Generic Fingerprint

instance Serialize Fingerprint

instance Serialize GameToken

#ifdef ghcjs_HOST_OS

--------------------------------------------------------------------------------
-- GHCJS event handling and core interaction code

screenCoordsToPoint :: Element -> Double -> Double -> IO Point
screenCoordsToPoint canvas sx sy = do
    rect <- getBoundingClientRect canvas
    cx <- realToFrac <$> ClientRect.getLeft rect
    cy <- realToFrac <$> ClientRect.getTop rect
    cw <- realToFrac <$> ClientRect.getWidth rect
    ch <- realToFrac <$> ClientRect.getHeight rect
    let unitLen = min cw ch / 20
    let midx = cx + cw / 2
    let midy = cy + ch / 2
    return ((sx - midx) / unitLen, (midy - sy) / unitLen)

getMousePos :: IsMouseEvent e => Element -> EventM w e Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ screenCoordsToPoint canvas (fromIntegral ix) (fromIntegral iy)

onEvents :: Element -> (Event -> IO ()) -> IO ()
onEvents canvas handler = do
    Just window <- currentWindow
    _ <- on window keyDown $ do
        code <- getKeyCode =<< event
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handler (KeyPress keyName)
            preventDefault
            stopPropagation
        key <- getKey =<< event
        when (T.length key == 1) $ do
            liftIO $ handler (TextEntry key)
            preventDefault
            stopPropagation
    _ <- on window keyUp $ do
        code <- getKeyCode =<< event
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handler (KeyRelease keyName)
            preventDefault
            stopPropagation
    _ <- on window mouseDown $ do
        pos <- getMousePos canvas
        liftIO $ handler (PointerPress pos)
    _ <- on window mouseUp $ do
        pos <- getMousePos canvas
        liftIO $ handler (PointerRelease pos)
    _ <- on window mouseMove $ do
        pos <- getMousePos canvas
        liftIO $ handler (PointerMovement pos)
    return ()

encodeEvent :: (Timestamp, Maybe Event) -> String
encodeEvent = show

decodeEvent :: String -> Maybe (Timestamp, Maybe Event)
decodeEvent = readMaybe

data GameState s
    = Main (UIState SMain)
    | Connecting WS.WebSocket
                 (UIState SConnect)
    | Waiting WS.WebSocket
              GameId
              PlayerId
              (UIState SWait)
    | Running WS.WebSocket
              GameId
              Timestamp
              PlayerId
              (Future s)

gameTime :: GameState s -> Timestamp -> Double
gameTime (Running _ _ tstart _ _) t = t - tstart
gameTime _ _ = 0

-- It's worth trying to keep the canonical animation rate exactly representable
-- as a float, to minimize the chance of divergence due to rounding error.
gameRate :: Double
gameRate = 1 / 16

gameStep :: (Double -> s -> s) -> Double -> GameState s -> GameState s
gameStep _ t (Main s) = Main (CUI.step t s)
gameStep _ t (Connecting ws s) = Connecting ws (CUI.step t s)
gameStep _ t (Waiting ws gid pid s) = Waiting ws gid pid (CUI.step t s)
gameStep step t (Running ws gid tstart pid s) =
    Running ws gid tstart pid (currentTimePasses step gameRate (t - tstart) s)

gameDraw ::
       (Double -> s -> s)
    -> (PlayerId -> s -> Picture)
    -> GameState s
    -> Timestamp
    -> Picture
gameDraw _ _ (Main s) _ = CUI.picture s
gameDraw _ _ (Connecting _ s) _ = CUI.picture s
gameDraw _ _ (Waiting _ _ _ s) _ = CUI.picture s
gameDraw step draw (Running _ _ tstart pid s) t =
    draw pid (currentState step gameRate (t - tstart) s)

handleServerMessage ::
       Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (PlayerId -> Event -> s -> s)
    -> MVar (GameState s)
    -> ServerMessage
    -> IO ()
handleServerMessage numPlayers initial stepHandler eventHandler gsm sm = do
    modifyMVar_ gsm $ \gs -> do
        t <- getTime
        case (sm, gs) of
            (GameAborted, _) -> return initialGameState
            (JoinedAs pid gid, Connecting ws s) ->
                return (Waiting ws gid pid (CUI.startWaiting gid s))
            (PlayersWaiting m n, Waiting ws gid pid s) ->
                return (Waiting ws gid pid (CUI.updatePlayers n m s))
            (Started, Waiting ws gid pid _) -> do
                let s = initFuture (initial (mkStdGen (hash gid))) numPlayers
                return (Running ws gid t pid s)
            (OutEvent pid eo, Running ws gid tstart mypid s) ->
                case decodeEvent eo of
                    Just (t', event) ->
                        let ours = pid == mypid
                            func = eventHandler pid <$> event -- might be a ping (Nothing)
                            result
                                | ours = s -- we already took care of our events
                                | otherwise =
                                    addEvent stepHandler gameRate mypid t' func s
                        in return (Running ws gid tstart mypid result)
                    Nothing -> return (Running ws gid tstart mypid s)
            _ -> return gs
    return ()

gameHandle ::
       Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (PlayerId -> Event -> s -> s)
    -> GameToken
    -> MVar (GameState s)
    -> Event
    -> IO ()
gameHandle numPlayers initial stepHandler eventHandler token gsm event = do
    gs <- takeMVar gsm
    case gs of
        Main s ->
            case CUI.event event s of
                ContinueMain s' -> do
                    putMVar gsm (Main s')
                Create s' -> do
                    ws <-
                        connectToGameServer
                            (handleServerMessage
                                 numPlayers
                                 initial
                                 stepHandler
                                 eventHandler
                                 gsm)
                    sendClientMessage ws (NewGame numPlayers (encode token))
                    putMVar gsm (Connecting ws s')
                Join gid s' -> do
                    ws <-
                        connectToGameServer
                            (handleServerMessage
                                 numPlayers
                                 initial
                                 stepHandler
                                 eventHandler
                                 gsm)
                    sendClientMessage ws (JoinGame gid (encode token))
                    putMVar gsm (Connecting ws s')
        Connecting ws s ->
            case CUI.event event s of
                ContinueConnect s' -> do
                    putMVar gsm (Connecting ws s')
                CancelConnect s' -> do
                    WS.close Nothing Nothing ws
                    putMVar gsm (Main s')
        Waiting ws gid pid s ->
            case CUI.event event s of
                ContinueWait s' -> do
                    putMVar gsm (Waiting ws gid pid s')
                CancelWait s' -> do
                    WS.close Nothing Nothing ws
                    putMVar gsm (Main s')
        Running ws gid tstart pid f -> do
            t <- getTime
            let gameState0 = currentState stepHandler gameRate (t - tstart) f
            let eventFun = eventHandler pid event
            case ifDifferent eventFun gameState0 of
                Nothing -> putMVar gsm gs
                Just _ -> do
                    sendClientMessage
                        ws
                        (InEvent (encodeEvent (gameTime gs t, Just event)))
                    let f1 =
                            addEvent
                                stepHandler
                                gameRate
                                pid
                                (t - tstart)
                                (Just eventFun)
                                f
                    putMVar gsm (Running ws gid tstart pid f1)

getWebSocketURL :: IO JSString
getWebSocketURL = do
    loc <- Loc.getWindowLocation
    proto <- Loc.getProtocol loc
    hostname <- Loc.getHostname loc
    let url =
            case proto of
                "http:" -> "ws://" <> hostname <> ":9160/gameserver"
                "https:" -> "wss://" <> hostname <> "/gameserver"
                _ -> error "Unrecognized protocol"
    return url

connectToGameServer :: (ServerMessage -> IO ()) -> IO WS.WebSocket
connectToGameServer handleServerMessage = do
    let handleWSRequest m = do
            maybeSM <- decodeServerMessage m
            case maybeSM of
                Nothing -> return ()
                Just sm -> handleServerMessage sm
    wsURL <- getWebSocketURL
    let req =
            WS.WebSocketRequest
            { url = wsURL
            , protocols = []
            , onClose = Just $ \_ -> handleServerMessage GameAborted
            , onMessage = Just handleWSRequest
            }
    WS.connect req
  where
    decodeServerMessage :: WS.MessageEvent -> IO (Maybe ServerMessage)
    decodeServerMessage m =
        case WS.getData m of
            WS.StringData str -> do
                return $ readMaybe (Data.JSString.unpack str)
            _ -> return Nothing

sendClientMessage :: WS.WebSocket -> ClientMessage -> IO ()
sendClientMessage ws msg = WS.send (encodeClientMessage msg) ws
  where
    encodeClientMessage :: ClientMessage -> JSString
    encodeClientMessage m = Data.JSString.pack (show m)

initialGameState :: GameState s
initialGameState = Main CUI.initial

foreign import javascript unsafe "cw$deterministic_math();"
    enableDeterministicMath :: IO ()

runGame ::
       GameToken
    -> Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (Int -> Event -> s -> s)
    -> (Int -> s -> Picture)
    -> IO ()
runGame token numPlayers initial stepHandler eventHandler drawHandler = do
    enableDeterministicMath
    let fullStepHandler dt = stepHandler dt . eventHandler (-1) (TimePassing dt)

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)

    setCanvasSize canvas canvas
    _ <- on window resize $ do
        liftIO $ setCanvasSize canvas canvas

    showCanvas

    frameRenderer <- createFrameRenderer canvas

    currentGameState <- newMVar initialGameState
    onEvents canvas $
        gameHandle
            numPlayers
            initial
            fullStepHandler
            eventHandler
            token
            currentGameState

    let go t0 lastFrame = do
            gs <- readMVar currentGameState
            let pic = gameDraw fullStepHandler drawHandler gs t0
            picFrame <- makeStableName $! pic
            when (picFrame /= lastFrame) $ frameRenderer pic
            t1 <- nextFrame
            modifyMVar_ currentGameState $ return . gameStep fullStepHandler t1
            go t1 picFrame
    t0 <- nextFrame
    nullFrame <- makeStableName undefined
    go t0 nullFrame

getDeployHash :: IO Text
getDeployHash = pFromJSVal <$> js_getDeployHash

foreign import javascript "/[&?]dhash=(.{22})/.exec(window.location.search)[1]"
    js_getDeployHash :: IO JSVal

propagateErrors :: ThreadId -> IO () -> IO ()
propagateErrors tid action = action `catch` \ (e :: SomeException) -> throwTo tid e

run :: s
    -> (Double -> s -> s)
    -> (e -> s -> s)
    -> (s -> Picture)
    -> (Double -> e)
    -> IO (e -> IO (), IO s)
run initial stepHandler eventHandler drawHandler injectTime = do
    let fullStepHandler dt = stepHandler dt . eventHandler (injectTime dt)

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)

    needsRedraw <- newMVar ()
    _ <- on window resize $ void $ liftIO $ do
        setCanvasSize canvas canvas
        tryPutMVar needsRedraw ()
    setCanvasSize canvas canvas

    showCanvas

    frameRenderer <- createFrameRenderer canvas
    currentState <- newMVar initial
    eventHappened <- newMVar ()
    let go t0 lastFrame lastStateName needsTime = do
            pic <- drawHandler <$> readMVar currentState
            picFrame <- makeStableName $! pic
            when (picFrame /= lastFrame) $ frameRenderer pic
            t1 <-
                case needsTime of
                    True -> do
                        t1 <- nextFrame
                        let dt = min (t1 - t0) 0.25
                        when (dt > 0) $ void $
                            modifyMVarIfDifferent currentState (fullStepHandler dt)
                        return t1
                    False -> do
                        takeMVar eventHappened
                        nextFrame
            nextState <- readMVar currentState
            nextStateName <- makeStableName $! nextState
            let nextNeedsTime =
                    nextStateName /= lastStateName ||
                    needsTime && not (isUniversallyConstant fullStepHandler nextState)
            redrawResult <- tryTakeMVar needsRedraw
            nextFrame <- case redrawResult of
                Nothing -> return picFrame
                Just () -> makeStableName undefined
            go t1 nextFrame nextStateName nextNeedsTime
    t0 <- nextFrame
    nullFrame <- makeStableName undefined
    initialStateName <- makeStableName $! initial
    mainThread <- myThreadId
    drawThread <- forkIO $ propagateErrors mainThread $
        go t0 nullFrame initialStateName True
    let sendEvent event = propagateErrors drawThread $ do
            changed <-
                modifyMVarIfDifferent currentState (eventHandler event)
            when changed $ void $ tryPutMVar eventHappened ()
        getState = readMVar currentState
    return (sendEvent, getState)

getNodeAtCoords :: Element -> Double -> Double -> Picture -> IO (Maybe NodeId)
getNodeAtCoords canvas x y pic = do
    rect <- getBoundingClientRect canvas
    cx <- realToFrac <$> ClientRect.getLeft rect
    cy <- realToFrac <$> ClientRect.getTop rect
    cw <- realToFrac <$> ClientRect.getWidth rect
    ch <- realToFrac <$> ClientRect.getHeight rect

    -- It's safe to pass undefined for the context because
    -- findTopShapeFromPoint only draws to an offscreen buffer.
    runCanvasM (cw, ch) undefined $
        findTopShapeFromPoint (x - cx, y - cy) pic

drawPartialPic :: Element -> NodeId -> Picture -> IO ()
drawPartialPic canvas nodeId pic = do
    setCanvasSize canvas canvas
    let node = fromMaybe blank (getNode nodeId pic)
    frameRenderer <- createFrameRenderer canvas
    frameRenderer (node <> coordinatePlane)

applySelectAndHighlights :: Maybe NodeId -> [NodeId] -> Picture -> Picture
applySelectAndHighlights sel hs p = applyHighlights hs' p'
    where (p', hs') = applySelect sel (p, hs)

applySelect :: Maybe NodeId -> (Picture, [NodeId]) -> (Picture, [NodeId])
applySelect Nothing (pic, highlights) = (pic, highlights)
applySelect (Just (NodeId n)) (pic, highlights) =
    case getNode (NodeId n) pic of
        Nothing -> (pic, highlights)
        Just pic' -> (pic', [ NodeId (h - n) | NodeId h <- highlights ])

applyHighlights :: [NodeId] -> Picture -> Picture
applyHighlights hs p = pictures [highlight h p | h <- hs] <> p

highlight :: NodeId -> Picture -> Picture
highlight n pic = case getTransformedNode n pic of
    Nothing -> blank
    Just shape -> colored (RGBA 0 0 0 0.25) shape

indexNode :: Bool -> Int -> NodeId -> Picture -> Either Int Picture
indexNode _ i (NodeId n) p
    | i == n = Right p
    | i > n = Left 0
indexNode True i n (Translate loc x y p)
    = Translate loc x y <$> indexNode True (i + 1) n p
indexNode True i n (Scale loc x y p)
    = Scale loc x y <$> indexNode True (i + 1) n p
indexNode True i n (Dilate loc k p)
    = Dilate loc k <$> indexNode True (i + 1) n p
indexNode True i n (Rotate loc r p)
    = Rotate loc r <$> indexNode True (i + 1) n p
indexNode True i n (Reflect loc r p)
    = Reflect loc r <$> indexNode True (i + 1) n p
indexNode True i n (Clip loc x y p)
    = Clip loc x y <$> indexNode True (i + 1) n p
indexNode keepTx i n p = go keepTx (i + 1) (getChildNodes p)
  where go _ i [] = Left i
        go keepTx i (pic:pics) =
            case indexNode keepTx i n pic of
                Left ii -> go keepTx ii pics
                Right p -> Right p

getTransformedNode :: NodeId -> Picture -> Maybe Picture
getTransformedNode n pic = either (const Nothing) Just (indexNode True 0 n pic)

getNode :: NodeId -> Picture -> Maybe Picture
getNode n pic = either (const Nothing) Just (indexNode False 0 n pic)

data DebugState = DebugState
    { debugStateActive :: Bool
    , shapeHighlighted :: Maybe NodeId
    , shapeSelected :: Maybe NodeId
    } deriving (Eq, Show)

debugStateInit :: DebugState
debugStateInit = DebugState False Nothing Nothing

startDebugState :: DebugState -> DebugState
startDebugState = const (DebugState True Nothing Nothing)

stopDebugState :: DebugState -> DebugState
stopDebugState = const (DebugState False Nothing Nothing)

highlightDebugState :: Maybe NodeId -> DebugState -> DebugState
highlightDebugState n prev =
    case debugStateActive prev of
        True -> prev {shapeHighlighted = n}
        False -> DebugState False Nothing Nothing

selectDebugState :: Maybe NodeId -> DebugState -> DebugState
selectDebugState n prev =
    case debugStateActive prev of
        True -> prev {shapeSelected = n}
        False -> DebugState False Nothing Nothing

drawDebugState :: DebugState -> Picture -> Picture -> Picture
drawDebugState state inspectPic displayPic =
    case debugStateActive state of
        True -> applySelectAndHighlights
            (shapeSelected state)
            (maybeToList (shapeHighlighted state))
            inspectPic
        False -> displayPic

connectInspect
    :: Element
    -> IO Picture
    -> ((DebugState -> DebugState) -> IO ())
    -> IO ()
connectInspect canvas samplePicture fireUpdate = do
    -- Sample the current user picture to search for a current node.
    getNodeCB <- syncCallback1' $ \pointJS -> do
        let obj = unsafeCoerce pointJS
        x <- pFromJSVal <$> getProp "x" obj
        y <- pFromJSVal <$> getProp "y" obj
        n <- getNodeAtCoords canvas x y =<< samplePicture
        return (pToJSVal (maybe (-1) getNodeId n))

    -- Sample the current user picture to return the scene tree.
    getPicCB <- syncCallback' $ samplePicture >>= toJSVal_aeson . pictureToNode

    -- Sample the current user picture to draw to a canvas.
    drawCB <- syncCallback2 ContinueAsync $ \c n -> do
        let canvas = unsafeCoerce c :: Element
        let nodeId = NodeId (pFromJSVal n)
        drawPartialPic canvas nodeId =<< samplePicture

    -- Fire an event to change debug active state.
    setActiveCB <- syncCallback1 ContinueAsync $ \ active -> case pFromJSVal active of
        True  -> fireUpdate startDebugState
        False -> fireUpdate stopDebugState

    -- Fire an event to change the highlight or selection.
    highlightCB <- syncCallback2 ContinueAsync $ \t n -> do
        let isHighlight = pFromJSVal t
        let nodeNum = pFromJSVal n
        let nodeId = if nodeNum < 0 then Nothing else Just (NodeId nodeNum)
        if isHighlight then fireUpdate (highlightDebugState nodeId)
                       else fireUpdate (selectDebugState nodeId)

    js_initDebugMode getNodeCB setActiveCB getPicCB highlightCB drawCB

foreign import javascript unsafe "initDebugMode($1,$2,$3,$4,$5)"
    js_initDebugMode :: Callback (JSVal -> IO JSVal)
                     -> Callback (JSVal -> IO ())
                     -> Callback (IO JSVal)
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> IO ()

-- Utility functions that apply a function in either the left or right half of a
-- tuple.  Crucially, if the function preserves sharing on its side, then the
-- wrapper also preserves sharing.
inLeft :: (a -> a) -> (a, b) -> (a, b)
inLeft f ab = unsafePerformIO $ do
  let (a, b) = ab
  aName <- makeStableName $! a
  let a' = f a
  aName' <- makeStableName $! a'
  return $ if aName == aName' then ab else (a', b)

inRight :: (b -> b) -> (a, b) -> (a, b)
inRight f ab = unsafePerformIO $ do
  let (a, b) = ab
  bName <- makeStableName $! b
  let b' = f b
  bName' <- makeStableName $! b'
  return $ if bName == bName' then ab else (a, b')

foreign import javascript interruptible "window.dummyVar = 0;"
  waitForever :: IO ()

-- Wraps the event and state from run so they can be paused by pressing the Inspect
-- button.
runInspect
    :: s
    -> (Double -> s -> s)
    -> (Event -> s -> s)
    -> (s -> Picture)
    -> (s -> Picture)
    -> IO ()
runInspect initial step event draw rawDraw = do
    -- Ensure that the first frame picture doesn't expose any type errors,
    -- before showing the canvas.  This avoids showing a blank screen when
    -- there are deferred type errors that are effectively compile errors.
    evaluate $ rnf $ rawDraw initial

    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    let debugInitial = (debugStateInit, initial)
        debugStep dt s@(debugState, _) =
            case debugStateActive debugState of
                True -> s
                False -> inRight (step dt) s
        debugEvent evt s@(debugState, _) =
            case (debugStateActive debugState, evt) of
                (_, Left f) -> inLeft f s
                (True, _) -> s
                (_, Right e) -> inRight (event e) s
        debugDraw (debugState, s) =
            drawDebugState debugState (rawDraw s) (draw s)
        debugRawDraw (_debugState, s) = rawDraw s
    (sendEvent, getState) <-
        run debugInitial debugStep debugEvent debugDraw (Right . TimePassing)
    onEvents canvas (sendEvent . Right)
    connectInspect canvas (debugRawDraw <$> getState) (sendEvent . Left)
    waitForever

#else

--------------------------------------------------------------------------------
-- Stand-Alone event handling and core interaction code

getMousePos :: (Int, Int) -> (Double, Double) -> (Double, Double)
getMousePos (w, h) (x, y) =
    ((x - mx) / unitLen, (my - y) / unitLen)
  where
    w' = fromIntegral w
    h' = fromIntegral h
    unitLen = min w' h' / 20
    mx = w' / 2
    my = h' / 2

toEvent :: (Int, Int) -> Canvas.Event -> Maybe Event
toEvent rect Canvas.Event {..}
    | eType == "keydown"
    , Just code <- eWhich = Just $ KeyPress (keyCodeToText (fromIntegral code))
    | eType == "keyup"
    , Just code <- eWhich =
        Just $ KeyRelease (keyCodeToText (fromIntegral code))
    | eType == "mousedown"
    , Just pos <- getMousePos rect <$> ePageXY = Just $ PointerPress pos
    | eType == "mouseup"
    , Just pos <- getMousePos rect <$> ePageXY = Just $ PointerRelease pos
    | eType == "mousemove"
    , Just pos <- getMousePos rect <$> ePageXY = Just $ PointerMovement pos
    | otherwise = Nothing

onEvents :: Canvas.DeviceContext -> (Int, Int) -> (Event -> IO ()) -> IO ()
onEvents context rect handler = void $ forkIO $ forever $ do
    maybeEvent <- toEvent rect <$> Canvas.wait context
    forM_ maybeEvent handler

run :: s -> (Double -> s -> s) -> (Event -> s -> s) -> (s -> Picture) -> IO ()
run initial stepHandler eventHandler drawHandler =
    runBlankCanvas $ \context -> do
        let fullStepHandler dt = stepHandler dt . eventHandler (TimePassing dt)
        let cw = Canvas.width context
        let ch = Canvas.height context
        offscreenCanvas <- runCanvasM context $ CM.newImage cw ch
        currentState <- newMVar initial
        eventHappened <- newMVar ()
        onEvents context (cw, ch) $ \event -> do
            modifyMVar_ currentState (return . eventHandler event)
            void $ tryPutMVar eventHappened ()
        let go t0 lastFrame lastStateName needsTime = do
                pic <- drawHandler <$> readMVar currentState
                picFrame <- makeStableName $! pic
                when (picFrame /= lastFrame) $
                    runCanvasM context $ do
                        CM.withImage offscreenCanvas $
                            CM.saveRestore $ do
                                setupScreenContext cw ch
                                drawFrame pic
                        CM.drawImage offscreenCanvas 0 0 cw ch
                t1 <- case needsTime of
                    True -> do
                        tn <- getCurrentTime
                        threadDelay $
                            max
                                0
                                (50000 -
                                 round ((tn `diffUTCTime` t0) * 1000000))
                        t1 <- getCurrentTime
                        let dt = realToFrac (t1 `diffUTCTime` t0)
                        when (dt > 0) $ modifyMVar_ currentState (return . fullStepHandler dt)
                        return t1
                    False -> do
                        takeMVar eventHappened
                        getCurrentTime
                nextState <- readMVar currentState
                nextStateName <- makeStableName $! nextState
                let nextNeedsTime =
                        nextStateName /= lastStateName ||
                        needsTime && not (isUniversallyConstant fullStepHandler nextState)
                go t1 picFrame nextStateName nextNeedsTime
        t0 <- getCurrentTime
        nullFrame <- makeStableName undefined
        initialStateName <- makeStableName $! initial
        go t0 nullFrame initialStateName True

runInspect
    :: s
    -> (Double -> s -> s)
    -> (Event -> s -> s)
    -> (s -> Picture)
    -> (s -> Picture)
    -> IO ()
runInspect initial step event draw _rawDraw = run initial step event draw

getDeployHash :: IO Text
getDeployHash = error "game API unimplemented in stand-alone interface mode"

runGame
    :: GameToken
    -> Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (Int -> Event -> s -> s)
    -> (Int -> s -> Picture)
    -> IO ()
runGame = error "game API unimplemented in stand-alone interface mode"

#endif

--------------------------------------------------------------------------------
-- FRP implementation

data ReactiveInput t = ReactiveInput {
    keyPress :: R.Event t Text,
    keyRelease :: R.Event t Text,
    textEntry :: R.Event t Text,
    pointerPress :: R.Event t Point,
    pointerRelease :: R.Event t Point,
    pointerPosition :: R.Dynamic t Point,
    pointerDown :: R.Dynamic t Bool,
    timePassing :: R.Event t Double
    }

data ReactiveOutput = ReactiveOutput {
    userPictures :: [Picture],
    userTransform :: Picture -> Picture,
    systemPicture :: Picture
    }

instance Semigroup ReactiveOutput where
    a <> b = ReactiveOutput {
        userPictures = userPictures a ++ userPictures b,
        userTransform = userTransform a . userTransform b,
        systemPicture = systemPicture a & systemPicture b
        }

instance Monoid ReactiveOutput where
    mempty = ReactiveOutput [] id blank

newtype ReactiveProgram t m a = ReactiveProgram {
    unReactiveProgram :: ReaderT (ReactiveInput t) (R.DynamicWriterT t ReactiveOutput m) a
    }

deriving instance Functor m => Functor (ReactiveProgram t m)
deriving instance Monad m => Applicative (ReactiveProgram t m)
deriving instance Monad m => Monad (ReactiveProgram t m)
deriving instance MonadFix m => MonadFix (ReactiveProgram t m)
deriving instance MonadIO m => MonadIO (ReactiveProgram t m)
deriving instance R.MonadSample t m => R.MonadSample t (ReactiveProgram t m)
deriving instance R.MonadHold t m => R.MonadHold t (ReactiveProgram t m)
deriving instance R.PerformEvent t m => R.PerformEvent t (ReactiveProgram t m)
deriving instance R.TriggerEvent t m => R.TriggerEvent t (ReactiveProgram t m)
deriving instance R.PostBuild t m => R.PostBuild t (ReactiveProgram t m)

instance (MonadFix m, R.MonadHold t m, R.Adjustable t m) => R.Adjustable t (ReactiveProgram t m) where
    runWithReplace a0 a' =
        ReactiveProgram $ R.runWithReplace (unReactiveProgram a0) $ fmap unReactiveProgram a'
    traverseIntMapWithKeyWithAdjust f dm0 dm' =
        ReactiveProgram $ R.traverseIntMapWithKeyWithAdjust (\k v -> unReactiveProgram (f k v)) dm0 dm'
    traverseDMapWithKeyWithAdjust f dm0 dm' =
        ReactiveProgram $ R.traverseDMapWithKeyWithAdjust (\k v -> unReactiveProgram (f k v)) dm0 dm'
    traverseDMapWithKeyWithAdjustWithMove f dm0 dm' =
        ReactiveProgram $ R.traverseDMapWithKeyWithAdjustWithMove (\k v -> unReactiveProgram (f k v)) dm0 dm'

runReactiveProgram
    :: (R.Reflex t, MonadFix m)
    => ReactiveProgram t m ()
    -> ReactiveInput t
    -> m (R.Dynamic t Picture, R.Dynamic t Picture)
runReactiveProgram (ReactiveProgram program) input = do
    (_, output) <- R.runDynamicWriterT (runReaderT program input)
    let pic = coalescePics . userPictures <$> output
    let sysPic = (&) <$> (systemPicture <$> output)
                     <*> (userTransform <$> output <*> pic)
    return (pic, sysPic)
  where coalescePics []  = blank
        coalescePics [p] = p
        coalescePics ps  = pictures ps

withReactiveInput
    :: ReactiveInput t
    -> (ReactiveProgram t m a -> ReactiveProgram t m a)
withReactiveInput input (ReactiveProgram program)
    = ReactiveProgram (withReaderT (const input) program)

getReactiveInput :: Monad m => ReactiveProgram t m (ReactiveInput t)
getReactiveInput = ReactiveProgram ask

systemDraw :: (R.Reflex t, Monad m) => R.Dynamic t Picture -> ReactiveProgram t m ()
systemDraw = ReactiveProgram . R.tellDyn . fmap (\a -> mempty { systemPicture = a })

transformUserPicture
    :: (R.Reflex t, Monad m) => R.Dynamic t (Picture -> Picture) -> ReactiveProgram t m ()
transformUserPicture =
    ReactiveProgram . R.tellDyn . fmap (\a -> mempty { userTransform = a })

-- | Type class for the builder monad of a CodeWorld/Reflex app.
class
  (
    R.Reflex t,
    R.Adjustable t m,
    R.MonadHold t m,
    R.PostBuild t m,
    R.PerformEvent t m,
    R.TriggerEvent t m,
    MonadFix m,
    MonadIO m,
    MonadIO (R.Performable m)
  ) => ReflexCodeWorld t m | m -> t where
    -- | Gets an Event of key presses.  The event value is a logical key name.
    getKeyPress :: m (R.Event t Text)

    -- | Gets an Event of key presses.  The event value is a logical key name.
    getKeyRelease :: m (R.Event t Text)

    -- | Gets an Event of text entered.  The event value is the typed text.
    getTextEntry :: m (R.Event t Text)

    -- | Gets an event of pointer clicks.  The event value is the location of
    -- the click.
    getPointerClick :: m (R.Event t Point)

    -- | Gets the Dynamic position of the pointer.
    getPointerPosition :: m (R.Dynamic t Point)

    -- | Gets a Dynamic indicator whether the pointer is held down.
    isPointerDown :: m (R.Dynamic t Bool)

    -- | Gets an Event indicating the passage of time.
    getTimePassing :: m (R.Event t Double)

    -- | Emits a given Dynamic picture to be drawn to the screen.
    draw :: R.Dynamic t Picture -> m ()

instance
  (
    R.Reflex t,
    R.Adjustable t m,
    R.MonadHold t m,
    R.PostBuild t m,
    R.PerformEvent t m,
    R.TriggerEvent t m,
    MonadFix m,
    MonadIO m,
    MonadIO (R.Performable m)
  ) => ReflexCodeWorld t (ReactiveProgram t m) where
    getKeyPress = ReactiveProgram $ asks keyPress

    getKeyRelease = ReactiveProgram $ asks keyRelease

    getTextEntry = ReactiveProgram $ asks textEntry

    getPointerClick = ReactiveProgram $ asks pointerPress

    getPointerPosition = ReactiveProgram $ asks pointerPosition

    isPointerDown = ReactiveProgram $ asks pointerDown

    getTimePassing = ReactiveProgram $ asks timePassing

    draw = ReactiveProgram . R.tellDyn . fmap (\a -> mempty { userPictures = [a] })

gateDyn :: forall t a. R.Reflex t => R.Dynamic t Bool -> R.Event t a -> R.Event t a
gateDyn dyn e = R.switchDyn (bool R.never e <$> dyn)

type EventChannel t = Chan [DSum (R.EventTriggerRef t) R.TriggerInvocation]

-- | Handle the event channel used with 'runTriggerEventT'.
processEventTriggers
    :: EventChannel t
    -> R.FireCommand t (R.SpiderHost R.Global)
    -> IO ()
processEventTriggers events fireCommand = do
    -- Collect event triggers, and fire callbacks after propagation
    eventsAndTriggers <- readChan events
    eventsToFire <- flip wither eventsAndTriggers $
        \(R.EventTriggerRef ref :=> R.TriggerInvocation a _) ->
            fmap (==> a) <$> readRef ref
    void . R.runSpiderHost $
        R.runFireCommand fireCommand eventsToFire (pure ())
    -- Run callbacks
    traverse_ (\(_ :=> R.TriggerInvocation _ cb) -> cb) eventsAndTriggers

sendEvent
    :: R.FireCommand t (R.SpiderHost R.Global)
    -> IORef (Maybe (R.EventTrigger t a))
    -> a
    -> IO ()
sendEvent (R.FireCommand fire) triggerRef a =
    R.runSpiderHost $ readRef triggerRef
        >>= traverse_ (\t -> fire [t ==> a] (pure ()))

#ifdef ghcjs_HOST_OS

createPhysicalReactiveInput
    :: forall t m. (R.MonadReflexCreateTrigger t m, R.Reflex t, R.MonadHold t m)
    => Window
    -> Element
    -> ([DSum (R.EventTrigger t) Identity] -> IO ())
    -> m (ReactiveInput t)
createPhysicalReactiveInput window canvas fire = do
    keyPress <- R.newEventWithTrigger $ \trigger ->
        on window keyDown $ do
            keyName <- keyCodeToText <$> (getKeyCode =<< event)
            when (keyName /= "") $ do
                liftIO $ fire [ trigger ==> keyName ]
                preventDefault
                stopPropagation
    textEntry <- R.newEventWithTrigger $ \trigger ->
        on window keyDown $ do
            key <- getKey =<< event
            when (T.length key == 1) $ do
                liftIO $ fire [trigger ==> key]
                preventDefault
                stopPropagation
    keyRelease <- R.newEventWithTrigger $ \trigger ->
        on window keyUp $ do
            keyName <- keyCodeToText <$> (getKeyCode =<< event)
            when (keyName /= "") $ do
                liftIO $ fire [trigger ==> keyName]
                preventDefault
                stopPropagation
    pointerPress <- R.newEventWithTrigger $ \trigger ->
        on window mouseDown $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger ==> pos]
    pointerRelease <- R.newEventWithTrigger $ \trigger ->
        on window mouseUp $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger ==> pos]
    pointerMovement <- R.newEventWithTrigger $ \trigger ->
        on window mouseMove $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger ==> pos]

    timePassing <- R.newEventWithTrigger $ \trigger -> do
        active <- newIORef True
        let timeStep t1 t2 = do
                stillActive <- readIORef active
                when stillActive $ do
                    when (t2 > t1) $ fire [
                        trigger ==> min 0.25 ((t2 - t1) / 1000)]
                    void $ inAnimationFrame ContinueAsync (timeStep t2)
        t0 <- nextFrame
        void $ inAnimationFrame ContinueAsync (timeStep t0)
        return (writeIORef active False)

    pointerPosition <- R.holdDyn (0, 0) pointerMovement
    pointerDown <- R.holdDyn False $
        R.leftmost [True <$ pointerPress, False <$ pointerRelease]

    return ReactiveInput{..}

inspectLogicalInput
    :: forall t m. (R.Reflex t, R.MonadHold t m)
    => R.Dynamic t DebugState
    -> ReactiveInput t
    -> m (ReactiveInput t)
inspectLogicalInput debugState physicalInput = do
    -- Physical inputs should either be frozen or dropped during debugging.
    let filterInDebugMode :: forall a. R.Event t a -> R.Event t a
        filterInDebugMode = gateDyn (not . debugStateActive <$> debugState)
    let freezeInDebugMode :: forall a. R.Dynamic t a -> a -> m (R.Dynamic t a)
        freezeInDebugMode dyn initial =
            R.holdDyn initial (filterInDebugMode (R.updated dyn))

    logicalPointerPosition <- freezeInDebugMode (pointerPosition physicalInput) (0, 0)
    logicalPointerDown     <- freezeInDebugMode (pointerDown physicalInput) False

    return $ ReactiveInput {
            keyPress        = filterInDebugMode (keyPress physicalInput),
            keyRelease      = filterInDebugMode (keyRelease physicalInput),
            textEntry       = filterInDebugMode (textEntry physicalInput),
            pointerPress    = filterInDebugMode (pointerPress physicalInput),
            pointerRelease  = filterInDebugMode (pointerRelease physicalInput),
            pointerPosition = logicalPointerPosition,
            pointerDown     = logicalPointerDown,
            timePassing     = filterInDebugMode (timePassing physicalInput)
            }

runReactive
    :: (forall t m.
        ( R.Reflex t,
          R.Adjustable t m,
          R.MonadHold t m,
          R.PostBuild t m,
          R.PerformEvent t m,
          R.TriggerEvent t m,
          MonadFix m,
          MonadIO m,
          MonadIO (R.Performable m)
        ) => (ReactiveInput t -> m (R.Dynamic t Picture, R.Dynamic t Picture)))
    -> IO ()
runReactive program = do
    showCanvas

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    setCanvasSize canvas canvas

    frameRenderer <- createFrameRenderer canvas
    pendingFrame <- liftIO $ newMVar Nothing
    let asyncRender pic = do
            old <- swapMVar pendingFrame (Just pic)
            when (isNothing old) $ void $ inAnimationFrame ContinueAsync $ \ _t -> do
                pic <- swapMVar pendingFrame Nothing
                maybe (return ()) frameRenderer pic

    (postBuild, postBuildTriggerRef) <- R.runSpiderHost R.newEventWithTriggerRef

    (debugUpdate, debugUpdateTriggerRef) <- R.runSpiderHost R.newEventWithTriggerRef
    debugState <- R.runSpiderHost $ R.holdUniqDyn =<< R.foldDyn ($) debugStateInit debugUpdate

    rec
        physicalInput <- R.runSpiderHost $
            createPhysicalReactiveInput window canvas fireAndRedraw
        resizeEvent <- R.runSpiderHost $ R.newEventWithTrigger $ \trigger -> do
            on window resize $ liftIO $ fireAndRedraw [trigger ==> ()]
        logicalInput <- R.runSpiderHost $ inspectLogicalInput debugState physicalInput
        eventTriggers <- newChan
        (inspectPicture, fireCommand) <- R.runSpiderHost $ R.hostPerformEventT $ do
            (inspectPicture, displayPicture) <-
                flip R.runTriggerEventT eventTriggers .
                flip R.runPostBuildT postBuild $
                program logicalInput
            let logicalPicture = drawDebugState <$> debugState
                                                <*> inspectPicture
                                                <*> displayPicture
            R.performEvent_ $ liftIO <$> R.leftmost [
                (setCanvasSize canvas canvas >>) . asyncRender <$>
                    R.tagPromptlyDyn logicalPicture resizeEvent,
                asyncRender <$> R.updated logicalPicture,
                asyncRender <$> R.tagPromptlyDyn logicalPicture postBuild
                ]
            return inspectPicture

        let fireAndRedraw events = R.runSpiderHost $ void $
                R.runFireCommand fireCommand events (return ())

    let
        fireDebugUpdateAndRedraw = sendEvent fireCommand debugUpdateTriggerRef
        samplePicture = R.runSpiderHost $ R.runHostFrame $ R.sample $ R.current inspectPicture
    connectInspect canvas samplePicture fireDebugUpdateAndRedraw

    sendEvent fireCommand postBuildTriggerRef ()

    void . forkIO . forever $ processEventTriggers eventTriggers fireCommand
    waitForever

#else

runReactive
    :: (forall t m.
        ( R.Reflex t,
          R.Adjustable t m,
          R.MonadHold t m,
          R.PostBuild t m,
          R.PerformEvent t m,
          R.TriggerEvent t m,
          MonadFix m,
          MonadIO m,
          MonadIO (R.Performable m)
        ) => (ReactiveInput t -> m (R.Dynamic t Picture, R.Dynamic t Picture)))
    -> IO ()
runReactive program = runBlankCanvas $ \context -> do
    let cw = Canvas.width context
    let ch = Canvas.height context
    offscreenCanvas <- runCanvasM context $ CM.newImage cw ch

    let frame pic = runCanvasM context $ do
            CM.withImage offscreenCanvas $
                CM.saveRestore $ do
                    setupScreenContext cw ch
                    drawFrame pic
            CM.drawImage offscreenCanvas 0 0 cw ch

    (postBuild, postBuildTriggerRef) <- R.runSpiderHost R.newEventWithTriggerRef

    (keyPress, keyPressTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (textEntry, textEntryTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (keyRelease, keyReleaseTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerPress, pointerPressTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerRelease, pointerReleaseTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerMovement, pointerMovementTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (timePassing, timePassingTrigger) <- R.runSpiderHost R.newEventWithTriggerRef

    pointerPosition <- R.runSpiderHost $ R.holdDyn (0, 0) pointerMovement
    pointerDown <- R.runSpiderHost $ R.holdDyn False $
        R.leftmost [True <$ pointerPress, False <$ pointerRelease]

    let input = ReactiveInput{..}

    triggeredEvents <- newChan
    (_, fireCommand) <- R.runSpiderHost $ R.hostPerformEventT $ do
        (_inspectPicture, displayPicture) <-
            flip R.runTriggerEventT triggeredEvents .
            flip R.runPostBuildT postBuild $
            program input
        R.performEvent_ $ liftIO <$> R.leftmost [
            frame <$> R.updated displayPicture,
            frame <$> R.tagPromptlyDyn displayPicture postBuild
            ]
        return ()

    let sendEvent'
            :: IORef (Maybe (R.EventTrigger (R.SpiderTimeline R.Global) a))
            -> a
            -> IO ()
        sendEvent' = sendEvent fireCommand

    sendEvent' postBuildTriggerRef ()

    t0 <- getCurrentTime
    let go t1 = do
            events <- Canvas.flush context
            forM_ events $ \event -> case Canvas.eType event of
                "keydown" | Just code <- Canvas.eWhich event -> do
                    let keyName = keyCodeToText (fromIntegral code)
                    sendEvent' keyPressTrigger keyName
                    when (T.length keyName == 1) $ sendEvent' textEntryTrigger keyName
                "keyup" | Just code <- Canvas.eWhich event -> do
                    let keyName = keyCodeToText (fromIntegral code)
                    sendEvent' keyReleaseTrigger keyName
                "mousedown" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent' pointerPressTrigger pos
                "mouseup" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent' pointerReleaseTrigger pos
                "mousemove" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent' pointerMovementTrigger pos
                _ -> return ()

            processEventTriggers triggeredEvents fireCommand

            tn <- getCurrentTime
            threadDelay $ max 0 (50000 - (round ((tn `diffUTCTime` t0) * 1000000)))
            t2 <- getCurrentTime
            let dt = realToFrac (t2 `diffUTCTime` t1)
            sendEvent' timePassingTrigger dt
            go t2
    go t0

#endif
