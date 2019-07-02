{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans -Wno-unticked-promoted-constructors #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
import Control.Monad.Identity
import Control.Monad.Ref
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (chr)
import Data.List (zip4, intercalate)
import Data.Maybe
import Data.Serialize
import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as T
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
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Dependent.Map (DSum(..))
import Data.Hashable
import Data.IORef
import qualified Data.JSString
import qualified GHCJS.DOM.ClientRect as ClientRect
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

applyColor :: MonadCanvas m => DrawState -> m ()
applyColor ds =
    case getColorDS ds of
        Nothing -> do
            CM.strokeColor 0 0 0 1
            CM.fillColor 0 0 0 1
        Just (RGBA r g b a) -> do
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

followPath :: MonadCanvas m => [Point] -> Bool -> Bool -> m ()
followPath [] _ _ = return ()
followPath [_] _ _ = return ()
followPath ((sx, sy):ps) closed False = do
    CM.moveTo (sx, sy)
    forM_ ps $ \(x, y) -> CM.lineTo (x, y)
    when closed $ CM.closePath
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

--------------------------------------------------------------------------------

-- | A Drawing is an intermediate and simpler representation of a Picture, suitable
-- for drawing. A drawing does not contain unnecessary metadata like CallStacks.
-- The drawer is specific to the platform.
data Drawing m
    = Shape (Drawer m)
    | Transformation (DrawState -> DrawState)
                     (Drawing m)
    | Drawings [Drawing m]

instance Semigroup (Drawing m) where
    a <> Drawings bs = Drawings (a : bs)
    a <> b           = Drawings [a, b]

instance Monoid (Drawing m) where
    mempty = Drawings []
    mappend a (Drawings bs) = Drawings (a : bs)
    mappend a b = Drawings [a, b]
    mconcat = Drawings

pictureToDrawing :: MonadCanvas m => Picture -> Drawing m
pictureToDrawing (SolidClosedCurve _ pts) = Shape $ polygonDrawer pts True
pictureToDrawing (SolidPolygon _ pts) = Shape $ polygonDrawer pts False
pictureToDrawing (Polygon _ pts) = Shape $ pathDrawer pts 0 True False
pictureToDrawing (ThickPolygon _ pts w) = Shape $ pathDrawer pts w True False
pictureToDrawing (Rectangle _ w h) = Shape $ pathDrawer (rectangleVertices w h) 0 True False
pictureToDrawing (SolidRectangle _ w h) = Shape $ polygonDrawer (rectangleVertices w h) False
pictureToDrawing (ThickRectangle _ lw w h) = Shape $ pathDrawer (rectangleVertices w h) lw True False
pictureToDrawing (ClosedCurve _ pts) = Shape $ pathDrawer pts 0 True True
pictureToDrawing (ThickClosedCurve _ pts w) = Shape $ pathDrawer pts w True True
pictureToDrawing (Circle _ r) = Shape $ arcDrawer 0 (2 * pi) r 0
pictureToDrawing (SolidCircle _ r) = Shape $ sectorDrawer 0 (2 * pi) r 
pictureToDrawing (ThickCircle _ lw r) = Shape $ arcDrawer 0 (2 * pi) r lw
pictureToDrawing (Polyline _ pts) = Shape $ pathDrawer pts 0 False False
pictureToDrawing (ThickPolyline _ pts w) = Shape $ pathDrawer pts w False False
pictureToDrawing (Curve _ pts) = Shape $ pathDrawer pts 0 False True
pictureToDrawing (ThickCurve _ pts w) = Shape $ pathDrawer pts w False True
pictureToDrawing (Sector _ b e r) = Shape $ sectorDrawer b e r
pictureToDrawing (Arc _ b e r) = Shape $ arcDrawer b e r 0
pictureToDrawing (ThickArc _ b e r w) = Shape $ arcDrawer b e r w
pictureToDrawing (Lettering _ txt) = Shape $ textDrawer Plain Serif txt
pictureToDrawing (Blank _) = Drawings $ []
pictureToDrawing (StyledLettering _ sty fnt txt) = Shape $ textDrawer sty fnt txt
pictureToDrawing (Sketch _ name url w h) = Shape $ imageDrawer name url w h
pictureToDrawing (CoordinatePlane _) = Shape $ coordinatePlaneDrawer
pictureToDrawing (Color _ col p) =
    Transformation (setColorDS col) $ pictureToDrawing p
pictureToDrawing (Translate _ x y p) =
    Transformation (translateDS x y) $ pictureToDrawing p
pictureToDrawing (Scale _ x y p) =
    Transformation (scaleDS x y) $ pictureToDrawing p
pictureToDrawing (Dilate _ k p) =
    Transformation (scaleDS k k) $ pictureToDrawing p
pictureToDrawing (Rotate _ r p) =
    Transformation (rotateDS r) $ pictureToDrawing p
pictureToDrawing (Pictures _ ps) = Drawings $ pictureToDrawing <$> ps
pictureToDrawing (PictureAnd _ ps) = Drawings $ pictureToDrawing <$> ps

type Drawer m = DrawState -> DrawMethods m

data DrawMethods m = DrawMethods
    { drawShape :: m ()
    , shapeContains :: Double -> Double -> m Bool
    }

polygonDrawer :: MonadCanvas m => [Point] -> Bool -> Drawer m
polygonDrawer ps smooth ds =
    DrawMethods
    { drawShape = do
          trace
          applyColor ds
          CM.fill
    , shapeContains = \x y -> do
          trace
          CM.isPointInPath (x, y)
    }
  where
    trace = withDS ds $ followPath ps True smooth

pathDrawer :: MonadCanvas m => [Point] -> Double -> Bool -> Bool -> Drawer m
pathDrawer ps w closed smooth ds =
    DrawMethods
    { drawShape = drawFigure ds w $ followPath ps closed smooth
    , shapeContains = \x y -> do
          s <- pixelSize
          drawFigure ds (max s w) $ followPath ps closed smooth
          CM.isPointInStroke (x, y)
    }

sectorDrawer :: MonadCanvas m => Double -> Double -> Double -> Drawer m
sectorDrawer b e r ds =
    DrawMethods
    { drawShape = do
          trace
          applyColor ds
          CM.fill
    , shapeContains = \x y -> do
          trace
          CM.isPointInPath (x, y)
    }
  where
    trace =
        withDS ds $ do
            CM.arc 0 0 (abs r) b e (b > e)
            CM.lineTo (0, 0)

arcDrawer :: MonadCanvas m => Double -> Double -> Double -> Double -> Drawer m
arcDrawer b e r w ds =
    DrawMethods
    { drawShape =
          drawFigure ds w $ CM.arc 0 0 (abs r) b e (b > e)
    , shapeContains = \x y -> do
          s <- pixelSize
          let width = max s w
          CM.lineWidth width
          drawFigure ds width $
              CM.arc 0 0 (abs r) b e (b > e)
          CM.isPointInStroke (x, y)
    }

textDrawer :: MonadCanvas m => TextStyle -> Font -> Text -> Drawer m
textDrawer sty fnt txt ds =
    DrawMethods
    { drawShape =
          withDS ds $ do
              CM.scale (1/25) (-1/25)
              applyColor ds
              CM.font (fontString sty fnt)
              CM.fillText txt (0, 0)
    , shapeContains = \x y ->
          do CM.font (fontString sty fnt)
             width <- (/ 25) <$> CM.measureText txt
             let height = 1 -- constant, defined in fontString
             withDS ds $
                 CM.rect ((-0.5) * width) ((-0.5) * height) width height
             CM.isPointInPath (x, y)
    }
  where
    fontString style font = stylePrefix style <> "25px " <> fontName font
    stylePrefix Plain = ""
    stylePrefix Bold = "bold "
    stylePrefix Italic = "italic "
    fontName SansSerif = "sans-serif"
    fontName Serif = "serif"
    fontName Monospace = "monospace"
    fontName Handwriting = "cursive"
    fontName Fancy = "fantasy"
    fontName (NamedFont txt) = "\"" <> T.filter (/= '"') txt <> "\""

imageDrawer :: MonadCanvas m => Text -> Text -> Double -> Double -> Drawer m
imageDrawer name url imgw imgh ds =
    DrawMethods
    { drawShape = 
          case getColorDS ds of
              Nothing -> withDS ds $ do
                  CM.scale 1 (-1)
                  CM.drawImgURL name url imgw imgh
              Just (RGBA _ _ _ _) -> do
                  w <- CM.getScreenWidth
                  h <- CM.getScreenHeight
                  img <- CM.newImage (round w) (round h)
                  CM.withImage img $ do
                      applyColor ds
                      CM.fillRect 0 0 w h
                      setupScreenContext (round w) (round h)
                      withDS ds $ do
                          CM.globalCompositeOperation "destination-in"
                          CM.scale 1 (-1)
                          CM.drawImgURL name url imgw imgh
                  CM.saveRestore $ do
                      CM.scale 1 (-1)
                      CM.drawImage img (round (-w/2)) (round (-h/2)) (round w) (round h)
    , shapeContains = \x y ->
          withDS ds $ do
              CM.rect (-imgw / 2) (-imgh / 2) imgw imgh
              CM.isPointInPath (x, y)
    }

coordinatePlaneDrawer :: MonadCanvas m => Drawer m
coordinatePlaneDrawer ds =
    DrawMethods
    { drawShape = drawDrawing ds coordinatePlaneDrawing
    , shapeContains = drawingContains ds coordinatePlaneDrawing
    }

coordinatePlaneDrawing :: MonadCanvas m => Drawing m
coordinatePlaneDrawing = pictureToDrawing $ axes <> numbers <> guidelines
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

drawDrawing :: MonadCanvas m => DrawState -> Drawing m -> m ()
drawDrawing ds (Shape drawer) = drawShape (drawer ds)
drawDrawing ds (Transformation f d) = drawDrawing (f ds) d
drawDrawing ds (Drawings drs) = mapM_ (drawDrawing ds) (reverse drs)

drawingContains :: MonadCanvas m => DrawState -> Drawing m -> Double -> Double -> m Bool
drawingContains ds (Shape drawer) x y = shapeContains (drawer ds) x y
drawingContains ds (Transformation f d) x y = drawingContains (f ds) d x y
drawingContains ds (Drawings drs) x y = or <$> mapM (\d -> drawingContains ds d x y) drs

--------------------------------------------------------------------------------

clearScreen :: MonadCanvas m => m ()
clearScreen = do
    w <- CM.getScreenWidth
    h <- CM.getScreenHeight
    px <- pixelSize
    CM.fillColor 255 255 255 1
    CM.fillRect (-w/2 * px) (-h/2 * px) (w * px) (h * px)

drawFrame :: MonadCanvas m => Drawing m -> m ()
drawFrame drawing = clearScreen >> drawDrawing initialDS drawing

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

-- A NodeId a unique id for each node in a Picture of Drawing, chosen by the order
-- the node appears in DFS. When a Picture is converted to a drawing the NodeId's of
-- corresponding nodes are shared. Always >=0.
newtype NodeId = NodeId { getNodeId :: Int}
    deriving (Eq, Ord, Enum)

findTopShape :: MonadCanvas m => DrawState -> Drawing m -> Double -> Double -> m (Maybe NodeId)
findTopShape ds d x y = do
    (found, n) <- go ds d x y
    return $ if found
        then Just (NodeId n)
        else Nothing
  where
    go ds (Shape drawer) x y = do
        contained <- shapeContains (drawer ds) x y
        case contained of
            True -> return (True, 0)
            False -> return (False, 1)
    go ds (Transformation f d) x y =
        fmap (+ 1) <$> go (f ds) d x y
    go _ (Drawings []) _ _ = return (False, 1)
    go ds (Drawings (dr:drs)) x y = do
        (found, count) <- go ds dr x y
        case found of
            True -> return (True, count + 1)
            False -> fmap (+ count) <$> go ds (Drawings drs) x y

-- If a picture is found, the result will include an array of the base picture
-- and all transformations.
findTopShapeFromPoint :: MonadCanvas m => Point -> Drawing m -> m (Maybe NodeId)
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
  | otherwise = take mid y ++ "..." ++ (reverse $ take mid $ reverse y)
  where mid = (x - 3) `div` 2

showFloat :: Double -> String
showFloat x
  | haskellMode && x < 0 = "(" ++ result ++ ")"
  | otherwise = result
  where result = stripZeros (showFFloatAlt (Just 4) x "")
        stripZeros = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

showPoints :: [Point] -> String
showPoints pts =
    "[" ++
    intercalate ", " [
        "(" ++ showFloat x ++ ", " ++ showFloat y ++ ")"
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
  | haskellMode, a == 1 = printf "(RGB %s %s %s)" (showFloat r) (showFloat g) (showFloat b)
  | a == 1 = printf "RGB(%s, %s, %s)" (showFloat r) (showFloat g) (showFloat b)
  | haskellMode = printf "(RGBA %s %s %s %s)" (showFloat r) (showFloat g) (showFloat b) (showFloat a)
  | otherwise = printf "RGBA(%s, %s, %s, %s)" (showFloat r) (showFloat g) (showFloat b) (showFloat a)

describePicture :: Picture -> String
describePicture (Rectangle _ w h)
  | haskellMode = printf "rectangle %s %s" (showFloat w) (showFloat h)
  | otherwise   = printf "rectangle(%s, %s)" (showFloat w) (showFloat h)
describePicture (SolidRectangle _ w h)
  | haskellMode = printf "solidRectangle %s %s" (showFloat w) (showFloat h)
  | otherwise   = printf "solidRectangle(%s, %s)" (showFloat w) (showFloat h)
describePicture (ThickRectangle _ lw w h)
  | haskellMode = printf "thickRectangle %s %s %s" (showFloat lw) (showFloat w) (showFloat h)
  | otherwise   = printf "thickRectangle(%s, %s, %s)" (showFloat w) (showFloat h) (showFloat lw)
describePicture (Circle _ r)
  | haskellMode = printf "circle %s" (showFloat r)
  | otherwise   = printf "circle(%s)" (showFloat r)
describePicture (SolidCircle _ r)
  | haskellMode = printf "solidCircle %s" (showFloat r)
  | otherwise   = printf "solidCircle(%s)" (showFloat r)
describePicture (ThickCircle _ lw r)
  | haskellMode = printf "thickCircle %s %s" (showFloat lw) (showFloat r)
  | otherwise   = printf "thickCircle(%s, %s)" (showFloat r) (showFloat lw)
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
  | haskellMode = printf "thickPolygon %s %s" (showFloat w) (showPoints pts)
  | otherwise   = printf "thickPolygon(%s, %s)" (showPoints pts) (showFloat w)
describePicture (ClosedCurve _ pts)
  | haskellMode = printf "closedCurve %s" (showPoints pts)
  | otherwise   = printf "closedCurve(%s)" (showPoints pts)
describePicture (ThickClosedCurve _ pts w)
  | haskellMode = printf "thickClosedCurve %s %s" (showFloat w) (showPoints pts)
  | otherwise   = printf "thickClosedCurve(%s, %s)" (showPoints pts) (showFloat w)
describePicture (Polyline _ pts)
  | haskellMode = printf "polyline %s" (showPoints pts)
  | otherwise   = printf "polyline(%s)" (showPoints pts)
describePicture (ThickPolyline _ pts w)
  | haskellMode = printf "thickPolyline %s %s" (showFloat w) (showPoints pts)
  | otherwise   = printf "thickPolyline(%s, %s)" (showPoints pts) (showFloat w)
describePicture (Curve _ pts)
  | haskellMode = printf "curve %s" (showPoints pts)
  | otherwise   = printf "curve(%s)" (showPoints pts)
describePicture (ThickCurve _ pts w)
  | haskellMode = printf "thickCurve %s %s" (showFloat w) (showPoints pts)
  | otherwise   = printf "thickCurve(%s, %s)" (showPoints pts) (showFloat w)
describePicture (Sector _ b e r)
  | haskellMode = printf "sector %s %s %s" (showFloat b) (showFloat e) (showFloat r)
  | otherwise   = printf "sector(%s°, %s°, %s)" (showFloat (180 * b / pi)) (showFloat (180 * e / pi)) (showFloat r)
describePicture (Arc _ b e r)
  | haskellMode = printf "arc %s %s %s" (showFloat b) (showFloat e) (showFloat r)
  | otherwise   = printf "arc(%s°, %s°, %s)" (showFloat (180 * b / pi)) (showFloat (180 * e / pi)) (showFloat r)
describePicture (ThickArc _ b e r w)
  | haskellMode = printf "thickArc %s %s %s %s" (showFloat w) (showFloat b) (showFloat e) (showFloat r)
  | otherwise   = printf "thickArc(%s°, %s°, %s, %s)" (showFloat (180 * b / pi)) (showFloat (180 * e / pi)) (showFloat r) (showFloat w)
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
  | haskellMode = printf "translated %s %s" (showFloat x) (showFloat y)
  | otherwise   = printf "translated(..., %s, %s)" (showFloat x) (showFloat y)
describePicture (Scale _ x y _)
  | haskellMode = printf "scaled %s %s" (showFloat x) (showFloat y)
  | otherwise   = printf "scaled(..., %s, %s)" (showFloat x) (showFloat y)
describePicture (Rotate _ angle _)
  | haskellMode = printf "rotated %s" (showFloat angle)
  | otherwise   = printf "rotated(..., %s°)" (showFloat (180 * angle / pi))
describePicture (Dilate _ k _)
  | haskellMode = printf "dilated %s" (showFloat k)
  | otherwise   = printf "dilated(..., %s)" (showFloat k)
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

createFrameRenderer :: Element -> IO (Drawing CanvasM -> IO ())
createFrameRenderer canvas = do
    offscreenCanvas <- Canvas.create 500 500
    screen <- getCodeWorldContext (canvasFromElement canvas)
    return $ \pic -> do
        setCanvasSize (elementFromCanvas offscreenCanvas) canvas
        rect <- getBoundingClientRect canvas
        withScreen (elementFromCanvas offscreenCanvas) rect (drawFrame pic)
        cw <- ClientRect.getWidth rect
        ch <- ClientRect.getHeight rect
        when (cw > 0 && ch > 0) $
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
    modifyMVar var $ \s0 -> do
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
            (Started, Waiting ws gid pid _) ->
                return
                    (Running
                         ws
                         gid
                         t
                         pid
                         (initFuture (initial (mkStdGen (hash gid))) numPlayers))
            (OutEvent pid eo, Running ws gid tstart mypid s) ->
                case decodeEvent eo of
                    Just (t', event) ->
                        let ours = pid == mypid
                            func = eventHandler pid <$> event -- might be a ping (Nothing)
                            result
                                | ours = s -- we already took care of our events
                                | otherwise =
                                    addEvent
                                        stepHandler
                                        gameRate
                                        mypid
                                        t'
                                        func
                                        s
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

    showCanvas

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)

    setCanvasSize canvas canvas
    _ <- on window resize $ do
        liftIO $ setCanvasSize canvas canvas

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
            when (picFrame /= lastFrame) $ frameRenderer (pictureToDrawing pic)
            t1 <- nextFrame
            modifyMVar_ currentGameState $ return . gameStep fullStepHandler t1
            go t1 picFrame
    t0 <- getTime
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
    -> (s -> Drawing CanvasM)
    -> (Double -> e)
    -> IO (e -> IO (), IO s)
run initial stepHandler eventHandler drawHandler injectTime = do
    let fullStepHandler dt = stepHandler dt . eventHandler (injectTime dt)

    showCanvas

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)

    needsRedraw <- newMVar ()
    _ <- on window resize $ void $ liftIO $ do
        setCanvasSize canvas canvas
        tryPutMVar needsRedraw ()
    setCanvasSize canvas canvas

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
                        _ <- modifyMVarIfDifferent currentState (fullStepHandler dt)
                        return t1
                    False -> do
                        takeMVar eventHappened
                        getTime
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
    t0 <- getTime
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
        findTopShapeFromPoint (x - cx, y - cy) (pictureToDrawing pic)

drawPartialPic :: Element -> NodeId -> Picture -> IO ()
drawPartialPic canvas nodeId pic = do
    setCanvasSize canvas canvas
    let node = fromMaybe (Drawings []) $
            fst <$> getDrawNode nodeId (pictureToDrawing pic)
    frameRenderer <- createFrameRenderer canvas
    frameRenderer (node <> coordinatePlaneDrawing)

initDebugMode :: Element
              -> (Bool -> IO ())
              -> IO Picture
              -> (Bool -> Maybe NodeId -> IO ())
              -> IO ()
initDebugMode canvas setActive getPic highlight = do
    getNodeCB <-
        syncCallback1' $ \pointJS -> do
            let obj = unsafeCoerce pointJS
            x <- pFromJSVal <$> getProp "x" obj
            y <- pFromJSVal <$> getProp "y" obj
            pic <- getPic
            n <- getNodeAtCoords canvas x y pic
            return (pToJSVal (maybe (-1) getNodeId n))
    setActiveCB <- syncCallback1 ContinueAsync $ setActive . pFromJSVal
    getPicCB <- syncCallback' $ getPic >>= toJSVal_aeson . pictureToNode
    highlightCB <-
        syncCallback2 ContinueAsync $ \t n ->
            let select = pFromJSVal t
                node =
                    case ((pFromJSVal n) :: Int) < 0 of
                        True -> Nothing
                        False -> Just $ NodeId $ pFromJSVal n
            in highlight select node
    drawCB <-
        syncCallback2 ContinueAsync $ \c n -> do
            let canvas = unsafeCoerce c :: Element
                nodeId = NodeId $ pFromJSVal n
            drawPartialPic canvas nodeId =<< getPic
    js_initDebugMode getNodeCB setActiveCB getPicCB highlightCB drawCB

foreign import javascript unsafe "initDebugMode($1,$2,$3,$4,$5)"
    js_initDebugMode :: Callback (JSVal -> IO JSVal)
                     -> Callback (JSVal -> IO ())
                     -> Callback (IO JSVal)
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> IO ()

data DebugState = DebugState
    { debugStateActive :: Bool
    , shapeHighlighted :: Maybe NodeId
    , shapeSelected :: Maybe NodeId
    }

data DebugEvent
    = DebugStart
    | DebugStop
    | HighlightEvent (Maybe NodeId)
    | SelectEvent (Maybe NodeId)

debugStateInit :: DebugState
debugStateInit = DebugState False Nothing Nothing

updateDebugState :: DebugEvent -> DebugState -> DebugState
updateDebugState DebugStart _prev = DebugState True Nothing Nothing
updateDebugState DebugStop _prev = DebugState False Nothing Nothing
updateDebugState (HighlightEvent n) prev =
    case debugStateActive prev of
        True -> prev {shapeHighlighted = n}
        False -> DebugState False Nothing Nothing
updateDebugState (SelectEvent n) prev =
    case debugStateActive prev of
        True -> prev {shapeSelected = n}
        False -> DebugState False Nothing Nothing

drawDebugState :: MonadCanvas m => DebugState -> Drawing m -> Drawing m
drawDebugState state drawing =
    case debugStateActive state of
        True ->
            highlightSelectShape
                (shapeHighlighted state)
                (shapeSelected state)
                drawing
        False -> drawing

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
                (_, Left e) -> inLeft (updateDebugState e) s
                (True, _) -> s
                (_, Right e) -> inRight (event e) s
        debugDraw (debugState, s) =
            case debugStateActive debugState of
                True -> drawDebugState debugState (pictureToDrawing (rawDraw s))
                False -> pictureToDrawing (draw s)
        debugRawDraw (_debugState, s) = rawDraw s
    (sendEvent, getState) <-
        run debugInitial debugStep debugEvent debugDraw (Right . TimePassing)
    let pauseEvent True = sendEvent $ Left DebugStart
        pauseEvent False = sendEvent $ Left DebugStop
        highlightSelectEvent True n = sendEvent $ Left (HighlightEvent n)
        highlightSelectEvent False n = sendEvent $ Left (SelectEvent n)
    onEvents canvas (sendEvent . Right)
    initDebugMode canvas pauseEvent (debugRawDraw <$> getState) highlightSelectEvent
    waitForever

-- Given a drawing, highlight the first node and select second node. Both recolor
-- the nodes, but highlight also brings the node to the top.
highlightSelectShape :: MonadCanvas m => Maybe NodeId -> Maybe NodeId -> Drawing m -> Drawing m
highlightSelectShape h s drawing
    | isNothing s =
        fromMaybe drawing $ do
            h' <- h
            hp <- piece h'
            return $ hp <> drawing
    | isNothing h =
        fromMaybe drawing $ do
            s' <- s
            sp <- piece s'
            replaceDrawNode s' sp drawing
    | otherwise =
        fromMaybe drawing $ do
            h' <- h
            s' <- s
            hp <- piece h'
            sp <- piece s'
            replaced <- replaceDrawNode s' sp drawing
            return $ hp <> replaced
  where
    piece n =
        (\(node, ds) -> highlightDrawing ds node) <$> getDrawNode n drawing

highlightDrawing :: MonadCanvas m => DrawState -> Drawing m -> Drawing m
highlightDrawing (DrawState at _) drawing =
    Transformation (\_ -> DrawState at (Just col')) drawing
  where
    col' = RGBA 0 0 0 0.25

getDrawNode :: NodeId -> Drawing m -> Maybe (Drawing m, DrawState)
getDrawNode n _
    | n < (NodeId 0) = Nothing
getDrawNode n drawing = either Just (const Nothing) $ go initialDS n drawing
  where
    go ds (NodeId 0) d = Left (d, ds)
    go _ n (Shape _) = Right (pred n)
    go ds n (Transformation f dr) = go (f ds) (pred n) dr
    go _ n (Drawings []) = Right (pred n)
    go ds n (Drawings (dr:drs)) =
        case go ds (pred n) dr of
            Left d -> Left d
            Right n -> go ds (succ n) $ Drawings drs

replaceDrawNode :: forall m. NodeId -> Drawing m -> Drawing m -> Maybe (Drawing m)
replaceDrawNode n _ _
    | n < (NodeId 0) = Nothing
replaceDrawNode n with drawing = either Just (const Nothing) $ go n drawing
  where
    go :: NodeId -> Drawing m -> Either (Drawing m) NodeId
    go (NodeId 0) _ = Left with
    go n (Shape _) = Right (pred n)
    go n (Transformation f d) = mapLeft (Transformation f) $ go (pred n) d
    go n (Drawings []) = Right (pred n)
    go n (Drawings (dr:drs)) =
        case go (pred n) dr of
            Left d -> Left $ Drawings (d : drs)
            Right _ ->
                mapLeft (\(Drawings qs) -> Drawings (dr : qs)) $
                go (succ n) $ Drawings drs
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft f = either (Left . f) Right

--------------------------------------------------------------------------------
-- FRP implementation

createPhysicalReactiveInput
    :: (R.Reflex t, R.MonadReflexCreateTrigger t m, R.MonadHold t m,
        MonadFix m, MonadIO m)
    => Window
    -> Element
    -> ([DSum (R.EventTrigger t) Identity] -> IO ())
    -> m (ReactiveInput t)
createPhysicalReactiveInput window canvas fire = do
    keyPress <- R.newEventWithTrigger $ \trigger ->
        on window keyDown $ do
            keyName <- keyCodeToText <$> (getKeyCode =<< event)
            when (keyName /= "") $ do
                liftIO $ fire [ trigger :=> Identity keyName ]
                preventDefault
                stopPropagation
    textEntry <- R.newEventWithTrigger $ \trigger ->
        on window keyDown $ do
            key <- getKey =<< event
            when (T.length key == 1) $ do
                liftIO $ fire [trigger :=> Identity key]
                preventDefault
                stopPropagation
    keyRelease <- R.newEventWithTrigger $ \trigger ->
        on window keyUp $ do
            keyName <- keyCodeToText <$> (getKeyCode =<< event)
            when (keyName /= "") $ do
                liftIO $ fire [trigger :=> Identity keyName]
                preventDefault
                stopPropagation
    pointerPress <- R.newEventWithTrigger $ \trigger ->
        on window mouseDown $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger :=> Identity pos]
    pointerRelease <- R.newEventWithTrigger $ \trigger ->
        on window mouseUp $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger :=> Identity pos]
    pointerMovement <- R.newEventWithTrigger $ \trigger ->
        on window mouseMove $ do
            pos <- getMousePos canvas
            liftIO $ fire [trigger :=> Identity pos]

    timePassing <- R.newEventWithTrigger $ \trigger -> do
        active <- newIORef True
        let timeStep t1 t2 = do
                stillActive <- readIORef active
                when stillActive $ do
                    fire [trigger :=> Identity ((t2 - t1) / 1000)]
                    void $ inAnimationFrame ContinueAsync (timeStep t2)
        t0 <- getTime
        void $ inAnimationFrame ContinueAsync (timeStep t0)
        return (writeIORef active False)

    pointerPosition <- R.holdDyn (0, 0) pointerMovement
    pointerDown <- R.holdDyn False $
        R.mergeWith (&&) [True <$ pointerPress, False <$ pointerRelease]

    return ReactiveInput{..}

connectInspect
    :: forall t m. (MonadIO m, MonadRef m, R.MonadReflexHost t m,
                    MonadFix m, R.MonadHold t m, Ref m ~ IORef)
    => Element
    -> R.Dynamic t Picture
    -> (forall a. m a -> IO a)
    -> (forall a. IORef (Maybe (R.EventTrigger t a)) -> a -> IO ())
    -> m (R.Dynamic t Bool, R.Dynamic t (Maybe NodeId), R.Dynamic t (Maybe NodeId))
connectInspect canvas userPicture runHost fireRef = do
    (activeSetEvent, activeTriggerRef) <- R.newEventWithTriggerRef
    (highlightSetEvent, highlightTriggerRef) <- R.newEventWithTriggerRef
    (selectionSetEvent, selectionTriggerRef) <- R.newEventWithTriggerRef

    let samplePic = runHost $
            R.runHostFrame $ R.sample $ R.current userPicture

    -- Sample the current user picture to search for a current node.
    getNodeCB <- liftIO $ syncCallback1' $ \pointJS -> do
        pic <- samplePic
        let obj = unsafeCoerce pointJS
        x <- pFromJSVal <$> getProp "x" obj
        y <- pFromJSVal <$> getProp "y" obj
        n <- getNodeAtCoords canvas x y pic
        return (pToJSVal (maybe (-1) getNodeId n))

    -- Sample the current user picture to return the scene tree.
    getPicCB <- liftIO $ syncCallback' $ do
        pic <- samplePic
        toJSVal_aeson (pictureToNode pic)

    -- Sample the current user picture to draw to a canvas.
    drawCB <- liftIO $ syncCallback2 ContinueAsync $ \c n -> do
        pic <- samplePic
        let canvas = unsafeCoerce c :: Element
        let nodeId = NodeId (pFromJSVal n)
        drawPartialPic canvas nodeId pic

    -- Fire an event to change debug active state.
    setActiveCB <- liftIO $ syncCallback1 ContinueAsync $ \ active -> do
        fireRef activeTriggerRef (pFromJSVal active)

    -- Fire an event to change the highlight or selection.
    highlightCB <- liftIO $ syncCallback2 ContinueAsync $ \t n -> do
        let isSelect = pFromJSVal t
        let nodeNum = pFromJSVal n
        let nodeId = if nodeNum < 0 then Nothing else Just (NodeId nodeNum)
        if isSelect then fireRef selectionTriggerRef nodeId
                    else fireRef highlightTriggerRef nodeId

    liftIO $ js_initDebugMode getNodeCB setActiveCB getPicCB highlightCB drawCB

    debugActive <- R.holdUniqDyn =<< R.holdDyn False activeSetEvent
    debugHighlight <- R.holdDyn Nothing highlightSetEvent
    debugSelection <- R.holdDyn Nothing selectionSetEvent

    return (debugActive, debugHighlight, debugSelection)

wrapInspect
    :: forall t m. (MonadIO m, MonadRef m, R.MonadReflexHost t m,
                    MonadFix m, Ref m ~ IORef, R.MonadHold t m)
    => Element
    -> ReactiveInput t
    -> R.Dynamic t Picture
    -> (forall a. m a -> IO a)
    -> (forall a. IORef (Maybe (R.EventTrigger t a)) -> a -> IO ())
    -> m (ReactiveInput t, R.Dynamic t (Drawing CanvasM))
wrapInspect canvas physicalInput userPicture runHost fireRef = do
    (debugActive, debugHighlight, debugSelection)
        <- connectInspect canvas userPicture runHost fireRef

    -- Many physical inputs should either be frozen or dropped during debugging.
    let filterInDebugMode :: forall a. R.Event t a -> R.Event t a
        filterInDebugMode = R.gate (not <$> R.current debugActive)
    let freezeInDebugMode :: forall a. R.Dynamic t a -> a -> m (R.Dynamic t a)
        freezeInDebugMode dyn initial =
            R.holdDyn initial (filterInDebugMode (R.updated dyn))

    logicalPointerPosition <- freezeInDebugMode (pointerPosition physicalInput) (0, 0)
    logicalPointerDown     <- freezeInDebugMode (pointerDown physicalInput) False
    logicalCurrentTime     <- freezeInDebugMode (currentTime physicalInput) 0

    let logicalInputs = ReactiveInput {
            keyPress        = filterInDebugMode (keyPress physicalInput),
            keyRelease      = filterInDebugMode (keyRelease physicalInput),
            textEntry       = filterInDebugMode (textEntry physicalInput),
            pointerPress    = filterInDebugMode (pointerPress physicalInput),
            pointerRelease  = filterInDebugMode (pointerRelease physicalInput),
            pointerPosition = logicalPointerPosition,
            pointerDown     = logicalPointerDown,
            timePassing     = filterInDebugMode (timePassing physicalInput),
            currentTime     = logicalCurrentTime
            }

    let userDrawing = pictureToDrawing <$> userPicture
    let debugDrawing = do
            active <- debugActive
            if active then highlightSelectShape <$> debugHighlight
                                                <*> debugSelection
                                                <*> userDrawing
                      else userDrawing

    return (logicalInputs, debugDrawing)

runReactive
    :: (forall t m. (R.Reflex t, R.MonadHold t m, MonadFix m)
        => (ReactiveInput t -> m (R.Dynamic t Picture)))
    -> IO ()
runReactive program = do
    showCanvas

    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    setCanvasSize canvas canvas

    frameRenderer <- createFrameRenderer canvas
    pendingFrame <- liftIO $ newMVar Nothing

    let handleFrame _ = do
            pic <- swapMVar pendingFrame Nothing
            maybe (return ()) (frameRenderer . pictureToDrawing) pic

    let asyncRender pic = do
            old <- swapMVar pendingFrame (Just pic)
            when (isNothing old) $ void $ inAnimationFrame ContinueAsync handleFrame

    rec
        physicalInput <- R.runSpiderHost $
            createPhysicalReactiveInput window canvas fireAndRedraw
        userPicture <- R.runSpiderHost $ R.runHostFrame $ program logicalInput
        (logicalInput, finalDrawing) <- R.runSpiderHost $
            wrapInspect canvas physicalInput userPicture R.runSpiderHost fireRefAndRedraw
        finalDrawingHandle <- R.runSpiderHost $ R.subscribeEvent (R.updated finalDrawing)

        let fireAndRedraw events = do
            fireAndRedraw events = do
                drawing <- R.runSpiderHost $ R.fireEventsAndRead events $
                    R.readEvent finalDrawingHandle >>= sequence
                maybe (return ()) asyncRender drawing

    let redraw = do
            drawing <- R.runSpiderHost $ R.runHostFrame $ R.sample $ R.current finalDrawing
            asyncRender drawing
    _ <- on window resize $ liftIO $ setCanvasSize canvas canvas >> redraw

    redraw
    waitForever

diffsWith :: (R.Reflex t, R.MonadHold t m, MonadFix m)
          => (a -> a -> b) -> a -> R.Dynamic t a -> m (R.Event t b)
diffsWith f start dyn = do
    pairs <- R.foldDyn (\new (old, _) -> (new, old)) (start, start) (R.updated dyn)
    return $ uncurry f <$> R.updated pairs

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
                                drawDrawing initialDS (pictureToDrawing pic)
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
                        modifyMVar_ currentState (return . fullStepHandler dt)
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

runReactive
    :: (forall t m. (R.Reflex t, R.MonadHold t m, MonadFix m)
        => ReactiveInput t -> m (R.Dynamic t Picture))
    -> IO ()
runReactive program = runBlankCanvas $ \context -> do
    let cw = Canvas.width context
    let ch = Canvas.height context
    offscreenCanvas <- runCanvasM context $ CM.newImage cw ch

    let frame (Just pic) = do
            runCanvasM context $ do
                CM.withImage offscreenCanvas $
                    CM.saveRestore $ do
                        setupScreenContext cw ch
                        drawDrawing initialDS (pictureToDrawing pic)
                CM.drawImage offscreenCanvas 0 0 cw ch
        frame Nothing = return ()

    (keyPress, keyPressTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (textEntry, textEntryTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (keyRelease, keyReleaseTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerPress, pointerPressTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerRelease, pointerReleaseTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (pointerMovement, pointerMovementTrigger) <- R.runSpiderHost R.newEventWithTriggerRef
    (timePassing, timePassingTrigger) <- R.runSpiderHost R.newEventWithTriggerRef

    pointerPosition <- R.runSpiderHost $ R.holdDyn (0, 0) pointerMovement
    pointerDown <- R.runSpiderHost $ R.holdDyn False $
        R.mergeWith (&&) [True <$ pointerPress, False <$ pointerRelease]

    let input = ReactiveInput{..}

    (dynPicture, pictureHandle) <- R.runSpiderHost $ do
        pic <- R.runHostFrame (program input)
        handle <- R.subscribeEvent (R.updated pic)
        return (pic, handle)

    let redraw = do
            pic <- R.runSpiderHost $
                R.runHostFrame $ R.sample $ R.current dynPicture
            frame (Just pic)

    let sendEvent trigger val = do
            pic <- R.runSpiderHost $
                R.fireEventRefAndRead trigger val pictureHandle
            frame pic

    redraw

    t0 <- getCurrentTime
    let go t1 = do
            events <- Canvas.flush context
            forM_ events $ \event -> case Canvas.eType event of
                "keydown" | Just code <- Canvas.eWhich event -> do
                    let keyName = keyCodeToText (fromIntegral code)
                    sendEvent keyPressTrigger keyName
                    when (T.length keyName == 1) $ sendEvent textEntryTrigger keyName
                "keyup" | Just code <- Canvas.eWhich event -> do
                    let keyName = keyCodeToText (fromIntegral code)
                    sendEvent keyReleaseTrigger keyName
                "mousedown" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent pointerPressTrigger pos
                "mouseup" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent pointerReleaseTrigger pos
                "mousemove" | Just pos <- getMousePos (cw, ch) <$> Canvas.ePageXY event -> do
                    sendEvent pointerMovementTrigger pos
                _ -> return ()

            tn <- getCurrentTime
            threadDelay $ max 0 (50000 - (round ((tn `diffUTCTime` t0) * 1000000)))
            t2 <- getCurrentTime
            let dt = realToFrac (t2 `diffUTCTime` t1)
            sendEvent timePassingTrigger dt
            go t2
    go t0

#endif
