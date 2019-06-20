{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StaticPointers #-}

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

import CodeWorld.CollaborationUI (SetupPhase(..), Step(..), UIState)
import qualified CodeWorld.CollaborationUI as CUI
import qualified CodeWorld.CanvasM as CM
import CodeWorld.CanvasM (MonadCanvas, CanvasM, runCanvasM)
import CodeWorld.Color
import CodeWorld.Event
import CodeWorld.Picture
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (chr)
import Data.List (find, zip4, intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid
import Data.Serialize
import Data.Serialize.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace
import GHC.Exts
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.Prim
import GHC.Stack
import GHC.StaticPtr
import Numeric (showFFloatAlt)
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Mem.StableName
import System.Random
import Text.Printf
import Text.Read

#ifdef ghcjs_HOST_OS

import CodeWorld.Message
import CodeWorld.Prediction
import qualified Control.Monad.Trans.State as State
import Data.Hashable
import Data.IORef
import qualified Data.JSString
import Data.JSString.Text
import Data.Word
import GHCJS.Concurrent (withoutPreemption)
import GHCJS.DOM
import qualified GHCJS.DOM.ClientRect as ClientRect
import GHCJS.DOM.Document hiding (evaluate)
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers hiding (error)
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Types (Element, unElement)
import qualified GHCJS.DOM.Window as Window
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified JavaScript.Array as Array
import JavaScript.Object
import JavaScript.Web.AnimationFrame
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas
import qualified JavaScript.Web.Location as Loc
import qualified JavaScript.Web.MessageEvent as WS
import qualified JavaScript.Web.WebSocket as WS
import Unsafe.Coerce

#else

import Data.Time.Clock
import qualified Graphics.Blank as Canvas
import Graphics.Blank (Canvas)
import Text.Printf

#endif

--------------------------------------------------------------------------------
-- The common interface, provided by both implementations below.
-- | Draws a 'Picture'.  This is the simplest CodeWorld entry point.
drawingOf :: Picture  -- ^ The picture to show on the screen.
          -> IO ()

-- | Shows an animation, with a picture for each time given by the parameter.
animationOf :: (Double -> Picture)  -- ^ A function that produces animation
                                    --   frames, given the time in seconds.
            -> IO ()

-- | Runs an interactive CodeWorld program that responds to events.  Activities
-- can interact with the user, change over time, and remember information about
-- the past.
activityOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given an event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

-- | Runs an interactive CodeWorld program in debugging mode.  In this mode,
-- the program gets controls to pause and manipulate time, and even go back in
-- time to look at past states.
debugActivityOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given an event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

-- | Runs an interactive multi-user CodeWorld program that is joined by several
-- participants over the internet.
groupActivityOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- ^ numbered starting at 0.
  -> StaticPtr (StdGen -> world)
          -- ^ The initial state of the activity.
  -> StaticPtr (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> StaticPtr (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()

-- | A version of 'groupActivityOf' that avoids static pointers, and does not
-- check for consistency.
unsafeGroupActivityOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- ^ numbered starting at 0.
  -> (StdGen -> world)
          -- ^ The initial state of the activity.
  -> (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()

-- | Shows a simulation, which is essentially a continuous-time dynamical
-- system described by an initial value and step function.
simulationOf
  :: world                       -- ^ The initial state of the simulation.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

debugSimulationOf
  :: world                       -- ^ The initial state of the simulation.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

-- | Runs an interactive event-driven CodeWorld program.  This is a
-- generalization of simulations that can respond to events like key presses
-- and mouse movement.
interactionOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given a user interface event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

debugInteractionOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given a user interface event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()

-- | Runs an interactive multi-user CodeWorld program, involving multiple
-- participants over the internet.
collaborationOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- ^ numbered starting at 0.
  -> StaticPtr (StdGen -> world)
          -- ^ The initial state of the collaboration.
  -> StaticPtr (Double -> world -> world)
          -- ^ The time step function, which advances the state given the time
          --   difference.
  -> StaticPtr (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> StaticPtr (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()

-- | A version of 'collaborationOf' that avoids static pointers, and does not
-- check for consistent parameters.
unsafeCollaborationOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- ^ numbered starting at 0.
  -> (StdGen -> world)
          -- ^ The initial state of the collaboration.
  -> (Double -> world -> world)
          -- ^ The time step function, which advances the state given the time
          --   difference.
  -> (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()

-- | Prints a debug message to the CodeWorld console when a value is forced.
-- This is equivalent to the similarly named function in `Debug.Trace`, except
-- that it uses the CodeWorld console instead of standard output.
trace :: Text -> a -> a

--------------------------------------------------------------------------------
-- A Drawing is an intermediate and simpler representation of a Picture, suitable
-- for drawing. A drawing does not contain unnecessary metadata like CallStacks.
-- The drawer is specific to the platform.
data Drawing m
    = Shape (Drawer m)
    | Transformation (DrawState -> DrawState)
                     (Drawing m)
    | Drawings [Drawing m]

#if MIN_VERSION_base(4,11,0)

instance Semigroup (Drawing m) where
    a <> Drawings bs = Drawings (a : bs)
    a <> b           = Drawings [a, b]

#endif

instance Monoid (Drawing m) where
    mempty = Drawings []
    mappend a (Drawings bs) = Drawings (a : bs)
    mappend a b = Drawings [a, b]
    mconcat = Drawings

data DrawState =
    DrawState
        !AffineTransformation
        !(Maybe Color) -- ^ A 'Color', if already chosen.

-- | @(AffineTransformation a b c d e f)@ represents an affine transformation matrix
--
-- > a c e
-- > b d f
-- > 0 0 1
--
-- References:
-- https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/transform
-- https://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations
data AffineTransformation =
    AffineTransformation !Double !Double !Double !Double !Double !Double

initialAffineTransformation :: AffineTransformation
initialAffineTransformation = AffineTransformation 1 0 0 1 0 0

mapDSAT :: (AffineTransformation -> AffineTransformation) -> DrawState -> DrawState
mapDSAT f (DrawState at mc) = DrawState (f at) mc

mapDSColor :: (Maybe Color -> Maybe Color) -> DrawState -> DrawState
mapDSColor f (DrawState at mc) = DrawState at (f mc)

-- A NodeId a unique id for each node in a Picture of Drawing, chosen by the order
-- the node appears in DFS. When a Picture is converted to a drawing the NodeId's of
-- corresponding nodes are shared. Always >=0.
newtype NodeId = NodeId { getNodeId :: Int}
    deriving (Eq, Ord, Enum)

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
pictureToDrawing (Logo _) = Shape $ logoDrawer
pictureToDrawing (Sketch _ _ url) = Shape $ imageDrawer url
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

initialDS :: DrawState
initialDS = DrawState initialAffineTransformation Nothing

translateDS :: Double -> Double -> DrawState -> DrawState
translateDS x y = mapDSAT $ \(AffineTransformation a b c d e f) ->
    AffineTransformation
        a b c d
        (a * 25 * x + c * 25 * y + e)
        (b * 25 * x + d * 25 * y + f)

scaleDS :: Double -> Double -> DrawState -> DrawState
scaleDS x y = mapDSAT $ \(AffineTransformation a b c d e f) ->
    AffineTransformation (x * a) (x * b) (y * c) (y * d) e f

rotateDS :: Double -> DrawState -> DrawState
rotateDS r = mapDSAT $ \(AffineTransformation a b c d e f) ->
    AffineTransformation
        (a * cos r + c * sin r)
        (b * cos r + d * sin r)
        (c * cos r - a * sin r)
        (d * cos r - b * sin r)
        e
        f

setColorDS :: Color -> DrawState -> DrawState
setColorDS col = mapDSColor $ \mcol ->
    case (col, mcol) of
        (_, Nothing) -> Just col
        (RGBA _ _ _ 0, Just _) -> Just col
        (RGBA _ _ _ alpha, Just (RGBA rr gg bb alpha0)) -> Just (RGBA rr gg bb (alpha0 * alpha))

getColorDS :: DrawState -> Maybe Color
getColorDS (DrawState at col) = col

polygonDrawer :: MonadCanvas m => [Point] -> Bool -> Drawer m
pathDrawer :: MonadCanvas m => [Point] -> Double -> Bool -> Bool -> Drawer m
sectorDrawer :: MonadCanvas m => Double -> Double -> Double -> Drawer m
arcDrawer :: MonadCanvas m => Double -> Double -> Double -> Double -> Drawer m
textDrawer :: MonadCanvas m => TextStyle -> Font -> Text -> Drawer m
logoDrawer :: MonadCanvas m => Drawer m
imageDrawer :: MonadCanvas m => Text -> Drawer m
coordinatePlaneDrawer :: MonadCanvas m => Drawer m
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
            , k /= 0
            ]
    ynumbers =
        pictures
            [ translated
                0.3
                (fromIntegral k)
                (scaled 0.5 0.5 (lettering (T.pack (show k))))
            | k <- [-9,-8 .. 9]
            , k /= 0
            ]

withDS :: MonadCanvas m => DrawState -> m a -> m a
withDS (DrawState (AffineTransformation ta tb tc td te tf) col) action = CM.saveRestore $ do
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

type Drawer m = DrawState -> DrawMethods m

data DrawMethods m = DrawMethods
    { drawShape :: m ()
    , shapeContains :: m Bool
    }

polygonDrawer ps smooth ds =
    DrawMethods
    { drawShape = do
          trace
          applyColor ds
          CM.fill
    , shapeContains = do
          trace
          CM.isPointInPath (0, 0)
    }
  where
    trace = withDS ds $ followPath ps True smooth

pathDrawer ps w closed smooth ds =
    DrawMethods
    { drawShape = drawFigure ds w $ followPath ps closed smooth
    , shapeContains =
          do let width =
                     if w == 0
                         then 0.3
                         else w
             drawFigure ds width $ followPath ps closed smooth
             CM.isPointInStroke (0, 0)
    }

sectorDrawer b e r ds =
    DrawMethods
    { drawShape = do
          trace
          applyColor ds
          CM.fill
    , shapeContains = do
          trace
          CM.isPointInPath (0, 0)
    }
  where
    trace =
        withDS ds $ do
            CM.arc 0 0 (25 * abs r) b e (b > e)
            CM.lineTo (0, 0)

arcDrawer b e r w ds =
    DrawMethods
    { drawShape =
          drawFigure ds w $ CM.arc 0 0 (25 * abs r) b e (b > e)
    , shapeContains =
          do let width =
                     if w == 0
                         then 0.3
                         else w
             CM.lineWidth (width * 25)
             drawFigure ds width $
                 CM.arc 0 0 (25 * abs r) b e (b > e)
             CM.isPointInStroke (0, 0)
    }

textDrawer sty fnt txt ds =
    DrawMethods
    { drawShape =
          withDS ds $ do
              CM.scale 1 (-1)
              applyColor ds
              CM.font (fontString sty fnt)
              CM.fillText txt (0, 0)
    , shapeContains =
          do CM.font (fontString sty fnt)
             width <- CM.measureText txt
             let height = 25 -- constant, defined in fontString
             withDS ds $
                 CM.rect ((-0.5) * width) ((-0.5) * height) width height
             CM.isPointInPath (0, 0)
    }

logoDrawer ds =
    DrawMethods
    { drawShape =
          withDS ds $ do
              CM.scale 1 (-1)
              drawCodeWorldLogo ds (-221) (-91) 442 182
    , shapeContains =
          withDS ds $ do
              CM.rect (-221) (-91) 442 182
              CM.isPointInPath (0, 0)
    }

imageDrawer url ds =
    DrawMethods { drawShape = return (), shapeContains = return False }

coordinatePlaneDrawer ds =
    DrawMethods
    { drawShape = drawDrawing ds coordinatePlaneDrawing
    , shapeContains = isJust <$> findTopShape ds coordinatePlaneDrawing
    }

followPath :: MonadCanvas m => [Point] -> Bool -> Bool -> m ()
followPath [] closed _ = return ()
followPath [p1] closed _ = return ()
followPath ((sx, sy):ps) closed False = do
    CM.moveTo (25 * sx, 25 * sy)
    forM_ ps $ \(x, y) -> CM.lineTo (25 * x, 25 * y)
    when closed $ CM.closePath
followPath [p1, p2] False True = followPath [p1, p2] False False
followPath ps False True = do
    let [(x1, y1), (x2, y2), (x3, y3)] = take 3 ps
        dprev = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
        dnext = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
        p = dprev / (dprev + dnext)
        cx = x2 + p * (x1 - x3) / 2
        cy = y2 + p * (y1 - y3) / 2
    CM.moveTo (25 * x1, 25 * y1)
    CM.quadraticCurveTo (25 * cx, 25 * cy) (25 * x2, 25 * y2)
    forM_ (zip4 ps (tail ps) (tail $ tail ps) (tail $ tail $ tail ps)) $ \((x1, y1), (x2, y2), (x3, y3), (x4, y4)) -> do
        let dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
            d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
            d2 = sqrt ((x4 - x3) ^ 2 + (y4 - y3) ^ 2)
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        CM.bezierCurveTo
            (25 * cx1, 25 * cy1)
            (25 * cx2, 25 * cy2)
            (25 * x3,  25 * y3)
    let [(x1, y1), (x2, y2), (x3, y3)] = reverse $ take 3 $ reverse ps
        dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
        d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
        r = d1 / (dp + d1)
        cx = x2 + r * (x3 - x1) / 2
        cy = y2 + r * (y3 - y1) / 2
    CM.quadraticCurveTo (25 * cx, 25 * cy) (25 * x3, 25 * y3)
followPath ps@(_:(sx, sy):_) True True = do
    CM.moveTo (25 * sx, 25 * sy)
    let rep = cycle ps
    forM_ (zip4 ps (tail rep) (tail $ tail rep) (tail $ tail $ tail rep)) $ \((x1, y1), (x2, y2), (x3, y3), (x4, y4)) -> do
        let dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
            d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
            d2 = sqrt ((x4 - x3) ^ 2 + (y4 - y3) ^ 2)
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        CM.bezierCurveTo
            (25 * cx1, 25 * cy1)
            (25 * cx2, 25 * cy2)
            (25 * x3,  25 * y3)
    CM.closePath

drawFigure :: MonadCanvas m => DrawState -> Double -> m () -> m ()
drawFigure ds w figure = do
    withDS ds $ do
        figure
        when (w /= 0) $ do
            CM.lineWidth (25 * w)
            applyColor ds
            CM.stroke
    when (w == 0) $ do
        CM.lineWidth 1
        applyColor ds
        CM.stroke

drawCodeWorldLogo ::
       MonadCanvas m => DrawState -> Int -> Int -> Int -> Int -> m ()
drawCodeWorldLogo ds x y w h = do
    mlogo <- CM.builtinImage "cwlogo"
    case (mlogo, getColorDS ds) of
        (Nothing, _) -> return ()
        (Just logo, Nothing) -> CM.drawImage logo x y w h
        (Just logo, Just (RGBA r g b a)) -> do
            -- This is a tough case.  The best we can do is to allocate an
            -- offscreen buffer as a temporary.
            img <- CM.newImage w h
            CM.withImage img $ do
                applyColor ds
                CM.fillRect 0 0 (fromIntegral w) (fromIntegral h)
                CM.globalCompositeOperation "destination-in"
                CM.drawImage logo 0 0 w h
            CM.drawImage img x y w h

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

drawDrawing :: MonadCanvas m => DrawState -> Drawing m -> m ()
drawDrawing ds (Shape shape) = drawShape $ shape ds
drawDrawing ds (Transformation f d) = drawDrawing (f ds) d
drawDrawing ds (Drawings drs) = mapM_ (drawDrawing ds) (reverse drs)

findTopShape :: MonadCanvas m => DrawState -> Drawing m -> m (Maybe NodeId)
findTopShape ds d = do
    (found, n) <- go ds d
    return $ if found
        then Just (NodeId n)
        else Nothing
  where
    go ds (Shape drawer) = do
        contained <- shapeContains $ drawer ds
        case contained of
            True -> return (True, 0)
            False -> return (False, 1)
    go ds (Transformation f d) =
        fmap (+ 1) <$> go (f ds) d
    go ds (Drawings []) = return (False, 1)
    go ds (Drawings (dr:drs)) = do
        (found, count) <- go ds dr
        case found of
            True -> return (True, count + 1)
            False -> fmap (+ count) <$> go ds (Drawings drs)

handlePointRequest :: MonadCanvas m => IO Picture -> Point -> m (Maybe NodeId)
handlePointRequest getPic pt = do
    drawing <- liftIO $ pictureToDrawing <$> getPic
    findTopShapeFromPoint pt drawing

inspect ::
       IO Picture -> (Bool -> IO ()) -> (Bool -> Maybe NodeId -> IO ()) -> IO ()
inspect getPic handleActive highlight =
    initDebugMode handleActive getPic highlight

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
describePicture (Logo _) = "codeWorldLogo"
describePicture (Sketch _ name _) = T.unpack name
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
getPictureSrcLoc (Logo loc) = loc
getPictureSrcLoc (Sketch loc _ _) = loc
getPictureSrcLoc (CoordinatePlane loc) = loc
getPictureSrcLoc (Pictures loc _) = loc
getPictureSrcLoc (PictureAnd loc _) = loc

-- If a picture is found, the result will include an array of the base picture
-- and all transformations.
findTopShapeFromPoint :: MonadCanvas m => Point -> Drawing m -> m (Maybe NodeId)
findTopShapeFromPoint (x, y) pic = do
    img <- CM.newImage 500 500
    CM.withImage img $
        findTopShape (translateDS (10 - x / 25) (y / 25 - 10) initialDS)
                     pic

drawFrame :: MonadCanvas m => Drawing m -> m ()
drawFrame drawing = do
    CM.fillColor 255 255 255 1
    CM.fillRect (-250) (-250) 500 500
    drawDrawing initialDS drawing

setupScreenContext :: MonadCanvas m => Int -> Int -> m ()
setupScreenContext cw ch = do
    -- blank before transformation (canvas might be non-square)
    CM.fillColor 255 255 255 1
    CM.fillRect 0 0 (fromIntegral cw) (fromIntegral ch)
    CM.translate (realToFrac cw / 2) (realToFrac ch / 2)
    let s = min (realToFrac cw / 500) (realToFrac ch / 500)
    CM.scale s (-s)
    CM.lineWidth 0
    CM.textCenter
    CM.textMiddle

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

getTime :: IO Double
getTime = (/ 1000) <$> js_getHighResTimestamp

foreign import javascript unsafe "performance.now()"
    js_getHighResTimestamp :: IO Double

nextFrame :: IO Double
nextFrame = waitForAnimationFrame >> getTime

initDebugMode :: (Bool -> IO ())
              -> IO Picture
              -> (Bool -> Maybe NodeId -> IO ())
              -> IO ()
initDebugMode setActive getPic highlight = do
    getNodeCB <-
        syncCallback1' $ \pointJS -> do
            let obj = unsafeCoerce pointJS
            x <- pFromJSVal <$> getProp "x" obj
            y <- pFromJSVal <$> getProp "y" obj
            -- It's safe to use undefined for the context and dimensions
            -- because handlePointRequest ignores them and works only with
            -- an off-screen image.
            n <- runCanvasM undefined undefined (handlePointRequest getPic (x, y))
            return (pToJSVal (maybe (-1) getNodeId n))
    setActiveCB <- syncCallback1 ContinueAsync $ setActive . pFromJSVal
    getPicCB <- syncCallback' $ getPic >>= picToObj
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
            drawing <- pictureToDrawing <$> getPic
            let node = fromMaybe (Drawings []) $ fst <$> getDrawNode nodeId drawing
            offscreenCanvas <- Canvas.create 500 500
            setCanvasSize canvas canvas
            setCanvasSize (elementFromCanvas offscreenCanvas) canvas
            screen <- getCodeWorldContext (canvasFromElement canvas)
            rect <- getBoundingClientRect canvas
            withScreen (elementFromCanvas offscreenCanvas) rect $
                drawFrame (node <> coordinatePlaneDrawing)
            rect <- getBoundingClientRect canvas
            cw <- ClientRect.getWidth rect
            ch <- ClientRect.getHeight rect
            canvasDrawImage
                screen
                (elementFromCanvas offscreenCanvas)
                0
                0
                (round cw)
                (round ch)
    js_initDebugMode getNodeCB setActiveCB getPicCB highlightCB drawCB

foreign import javascript unsafe "initDebugMode($1,$2,$3,$4,$5)"
    js_initDebugMode :: Callback (JSVal -> IO JSVal)
                     -> Callback (JSVal -> IO ())
                     -> Callback (IO JSVal)
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> Callback (JSVal -> JSVal -> IO ())
                     -> IO ()

picToObj :: Picture -> IO JSVal
picToObj = fmap fst . flip State.runStateT 0 . picToObj'

picToObj' :: Picture -> State.StateT Int IO JSVal
picToObj' pic = objToJSVal <$> case pic of
    Pictures _ ps -> mkNodeWithChildren ps
    PictureAnd _ ps -> mkNodeWithChildren ps
    Color _ _ p -> mkNodeWithChild p
    Translate _ _ _ p -> mkNodeWithChild p
    Scale _ _ _ p -> mkNodeWithChild p
    Dilate _ _ p -> mkNodeWithChild p
    Rotate _ _ p -> mkNodeWithChild p
    _ -> mkSimpleNode
  where
    mkSimpleNode :: State.StateT Int IO Object
    mkSimpleNode = do
        obj <- liftIO create
        id <- do
            currentId <- State.get
            State.put (currentId + 1)
            return currentId
        liftIO $ do
            setProp "id" (pToJSVal id) obj
            setProp "name" (pToJSVal $ (trim 80 . describePicture) pic) obj
            case getPictureSrcLoc pic of
                Just loc -> do
                    setProp "startLine" (pToJSVal $ srcLocStartLine loc) obj
                    setProp "startCol" (pToJSVal $ srcLocStartCol loc) obj
                    setProp "endLine" (pToJSVal $ srcLocEndLine loc) obj
                    setProp "endCol" (pToJSVal $ srcLocEndCol loc) obj
                Nothing -> return ()
        return obj

    mkNodeWithChild :: Picture -> State.StateT Int IO Object
    mkNodeWithChild p = do
        obj <- mkSimpleNode
        subPic <- picToObj' p
        liftIO $ setProp "picture" subPic obj
        return obj

    mkNodeWithChildren :: [Picture] -> State.StateT Int IO Object
    mkNodeWithChildren ps = do
        obj <- mkSimpleNode
        arr <- liftIO $ Array.create
        mapM_ (\p -> picToObj' p >>= liftIO . flip Array.push arr) ps
        liftIO $ setProp "pictures" (unsafeCoerce arr) obj
        return obj

    objToJSVal = unsafeCoerce :: Object -> JSVal

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
    setAttribute target ("width" :: JSString) (show (round cx))
    setAttribute target ("height" :: JSString) (show (round cy))

#else

--------------------------------------------------------------------------------
-- Stand-alone implementation of drawing

initDebugMode :: (Bool -> IO ())
              -> IO Picture
              -> (Bool -> Maybe NodeId -> IO ())
              -> IO ()
initDebugMode _ _ _ = return ()

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
    fromNum n = T.pack (show (fromIntegral n))

isUniversallyConstant :: (a -> s -> s) -> s -> Bool
isUniversallyConstant f old =
    unsafePerformIO $ falseOr $ do
        oldName <- makeStableName $! old
        genName <- makeStableName $! f undefined old
        return (genName == oldName)
  where
    falseOr x = x `catch` \(e :: SomeException) -> return False

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
    | NoToken
    deriving (Generic)

deriving instance Generic Fingerprint

instance Serialize Fingerprint

instance Serialize GameToken

#ifdef ghcjs_HOST_OS

--------------------------------------------------------------------------------
-- GHCJS event handling and core interaction code

getMousePos :: IsMouseEvent e => Element -> EventM w e Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ do
        rect <- getBoundingClientRect canvas
        cx <- ClientRect.getLeft rect
        cy <- ClientRect.getTop rect
        cw <- ClientRect.getWidth rect
        ch <- ClientRect.getHeight rect
        let unitLen = min cw ch / 20
        let mx = round (cx + cw / 2)
        let my = round (cy + ch / 2)
        return
            ( fromIntegral (ix - mx) / realToFrac unitLen
            , fromIntegral (my - iy) / realToFrac unitLen)

onEvents :: Element -> (Event -> IO ()) -> IO ()
onEvents canvas handler = do
    Just window <- currentWindow
    on window keyDown $ do
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
    on window keyUp $ do
        code <- getKeyCode =<< event
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handler (KeyRelease keyName)
            preventDefault
            stopPropagation
    on window mouseDown $ do
        pos <- getMousePos canvas
        liftIO $ handler (PointerPress pos)
    on window mouseUp $ do
        pos <- getMousePos canvas
        liftIO $ handler (PointerRelease pos)
    on window mouseMove $ do
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

isRunning :: GameState s -> Bool
isRunning Running {} = True
isRunning _ = False

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
                Just s1 -> do
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
    encodeClientMessage :: ClientMessage -> JSString
    encodeClientMessage m = Data.JSString.pack (show m)

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
    offscreenCanvas <- Canvas.create 500 500
    setCanvasSize canvas canvas
    setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    on window resize $ do
        liftIO $ setCanvasSize canvas canvas
        liftIO $ setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    currentGameState <- newMVar initialGameState
    onEvents canvas $
        gameHandle
            numPlayers
            initial
            fullStepHandler
            eventHandler
            token
            currentGameState
    screen <- getCodeWorldContext (canvasFromElement canvas)
    let go t0 lastFrame = do
            gs <- readMVar currentGameState
            let pic = gameDraw fullStepHandler drawHandler gs t0
            picFrame <- makeStableName $! pic
            when (picFrame /= lastFrame) $ do
                rect <- getBoundingClientRect canvas
                withScreen (elementFromCanvas offscreenCanvas) rect $
                    drawFrame (pictureToDrawing pic)
                rect <- getBoundingClientRect canvas
                cw <- ClientRect.getWidth rect
                ch <- ClientRect.getHeight rect
                when (cw > 0 && ch > 0) $ canvasDrawImage
                    screen
                    (elementFromCanvas offscreenCanvas)
                    0
                    0
                    (round cw)
                    (round ch)
            t1 <- nextFrame
            modifyMVar_ currentGameState $ return . gameStep fullStepHandler t1
            go t1 picFrame
    t0 <- getTime
    nullFrame <- makeStableName undefined
    initialStateName <- makeStableName $! initialGameState
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
    offscreenCanvas <- Canvas.create 500 500
    setCanvasSize canvas canvas
    setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    needsRedraw <- newMVar ()
    on window resize $ void $ liftIO $ do
        setCanvasSize canvas canvas
        setCanvasSize (elementFromCanvas offscreenCanvas) canvas
        tryPutMVar needsRedraw ()
    currentState <- newMVar initial
    eventHappened <- newMVar ()
    screen <- getCodeWorldContext (canvasFromElement canvas)
    let go t0 lastFrame lastStateName needsTime = do
            pic <- drawHandler <$> readMVar currentState
            picFrame <- makeStableName $! pic
            when (picFrame /= lastFrame) $ do
                rect <- getBoundingClientRect canvas
                withScreen (elementFromCanvas offscreenCanvas) rect $
                    drawFrame pic
                rect <- getBoundingClientRect canvas
                cw <- ClientRect.getWidth rect
                ch <- ClientRect.getHeight rect
                when (cw > 0 && ch > 0) $ canvasDrawImage
                    screen
                    (elementFromCanvas offscreenCanvas)
                    0
                    0
                    (round cw)
                    (round ch)
            t1 <-
                if | needsTime ->
                       do t1 <- nextFrame
                          let dt = min (t1 - t0) 0.25
                          modifyMVarIfDifferent currentState (fullStepHandler dt)
                          return t1
                   | otherwise ->
                       do takeMVar eventHappened
                          getTime
            nextState <- readMVar currentState
            nextStateName <- makeStableName $! nextState
            let nextNeedsTime =
                    nextStateName /= lastStateName ||
                    needsTime && not (isUniversallyConstant fullStepHandler nextState)
            nextFrame <- tryTakeMVar needsRedraw >>= \case
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
updateDebugState DebugStart prev = DebugState True Nothing Nothing
updateDebugState DebugStop prev = DebugState False Nothing Nothing
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
runInspect :: 
       (Wrapped s -> [Control s])
    -> s
    -> (Double -> s -> s)
    -> (Event -> s -> s)
    -> (s -> Picture) 
    -> IO ()
runInspect controls initial stepHandler eventHandler drawHandler = do
    -- Ensure that the first frame picture doesn't expose any type errors,
    -- before showing the canvas.  This avoids showing a blank screen when
    -- there are deferred type errors that are effectively compile errors.
    evaluate $ rnf $ drawHandler initial

    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    let initialWrapper = (debugStateInit, wrappedInitial initial)
        stepHandlerWrapper dt wrapper@(debugState, _) =
            case debugStateActive debugState of
                True -> wrapper
                False -> inRight (wrappedStep stepHandler dt) wrapper
        eventHandlerWrapper evt wrapper@(debugState, _) =
            case (debugStateActive debugState, evt) of
                (_, Left debugEvent) ->
                    inLeft (updateDebugState debugEvent) wrapper
                (True, _) -> wrapper
                (_, Right normalEvent) ->
                    inRight (wrappedEvent controls stepHandler eventHandler normalEvent) wrapper
        drawHandlerWrapper (debugState, wrappedState) =
            case debugStateActive debugState of
                True -> drawDebugState debugState $ pictureToDrawing $ drawHandler (state wrappedState)
                False -> pictureToDrawing (wrappedDraw controls drawHandler wrappedState)
        drawPicHandler (debugState, wrappedState) =
            drawHandler $ state wrappedState
    (sendEvent, getState) <-
        run
            initialWrapper
            stepHandlerWrapper
            (\e w -> (eventHandlerWrapper e) w)
            drawHandlerWrapper
            (Right . TimePassing)
    let pauseEvent True = sendEvent $ Left DebugStart
        pauseEvent False = sendEvent $ Left DebugStop
        highlightSelectEvent True n = sendEvent $ Left (HighlightEvent n)
        highlightSelectEvent False n = sendEvent $ Left (SelectEvent n)
    onEvents canvas (sendEvent . Right)
    inspect (drawPicHandler <$> getState) pauseEvent highlightSelectEvent
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
    go ds n (Shape _) = Right (pred n)
    go ds n (Transformation f dr) = go (f ds) (pred n) dr
    go ds n (Drawings []) = Right (pred n)
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
            Right m ->
                mapLeft (\(Drawings qs) -> Drawings (dr : qs)) $
                go (succ n) $ Drawings drs
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft f = either (Left . f) Right

#else

--------------------------------------------------------------------------------
-- Stand-Alone event handling and core interaction code

getMousePos :: (Int, Int) -> (Double, Double) -> (Double, Double)
getMousePos (w, h) (x, y) =
    ((x - mx) / realToFrac unitLen, (my - y) / realToFrac unitLen)
  where
    unitLen = min (fromIntegral w) (fromIntegral h) / 20
    mx = fromIntegral w / 2
    my = fromIntegral h / 2

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
onEvents context rect handler =
    void $
    forkIO $
    forever $ do
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
            tryPutMVar eventHappened ()
            return ()
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
                t1 <-
                    if | needsTime ->
                           do tn <- getCurrentTime
                              threadDelay $
                                  max
                                      0
                                      (50000 -
                                       round ((tn `diffUTCTime` t0) * 1000000))
                              t1 <- getCurrentTime
                              let dt = realToFrac (t1 `diffUTCTime` t0)
                              modifyMVar_ currentState (return . fullStepHandler dt)
                              return t1
                       | otherwise ->
                           do takeMVar eventHappened
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

runInspect :: 
       (Wrapped s -> [Control s])
    -> s
    -> (Double -> s -> s)
    -> (Event -> s -> s)
    -> (s -> Picture) 
    -> IO ()
runInspect controls initial stepHandler eventHandler drawHandler =
    run (wrappedInitial initial)
        (wrappedStep stepHandler)
        (wrappedEvent controls stepHandler eventHandler)
        (wrappedDraw controls drawHandler)

getDeployHash :: IO Text
getDeployHash = error "game API unimplemented in stand-alone interface mode"

runGame ::
       GameToken
    -> Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (Int -> Event -> s -> s)
    -> (Int -> s -> Picture)
    -> IO ()
runGame = error "game API unimplemented in stand-alone interface mode"

#endif

--------------------------------------------------------------------------------
-- Common code for game interface

groupActivityOf numPlayers initial event draw = do
    dhash <- getDeployHash
    let token =
            SteplessToken
            { tokenDeployHash = dhash
            , tokenNumPlayers = numPlayers
            , tokenInitial = staticKey initial
            , tokenEvent = staticKey event
            , tokenDraw = staticKey draw
            }
    runGame
        token
        numPlayers
        (deRefStaticPtr initial)
        (const id)
        (deRefStaticPtr event)
        (deRefStaticPtr draw)

unsafeGroupActivityOf numPlayers initial event draw =
    unsafeCollaborationOf numPlayers initial (const id) event draw

unsafeCollaborationOf numPlayers initial step event draw = do
    dhash <- getDeployHash
    let token = PartialToken dhash
    runGame token numPlayers initial step event draw
  where
    token = NoToken

collaborationOf numPlayers initial step event draw = do
    dhash <- getDeployHash
    let token =
            FullToken
            { tokenDeployHash = dhash
            , tokenNumPlayers = numPlayers
            , tokenInitial = staticKey initial
            , tokenStep = staticKey step
            , tokenEvent = staticKey event
            , tokenDraw = staticKey draw
            }
    runGame
        token
        numPlayers
        (deRefStaticPtr initial)
        (deRefStaticPtr step)
        (deRefStaticPtr event)
        (deRefStaticPtr draw)

--------------------------------------------------------------------------------
-- Common code for activity, interaction, animation and simulation interfaces

activityOf initial change picture =
    interactionOf initial (const id) change picture

interactionOf = runInspect (const [])

data StrictPoint = SP !Double !Double deriving (Eq, Show)

toStrictPoint :: Point -> StrictPoint
toStrictPoint (x, y) = SP x y

data StrictMaybe a = SNothing | SJust !a deriving (Functor, Show)

data Wrapped a = Wrapped
    { state :: a
    , playbackSpeed :: !Double
    , lastInteractionTime :: !Double
    , zoomFactor :: !Double
    , panCenter :: !StrictPoint
    , panDraggingAnchor :: !(StrictMaybe StrictPoint)
    , isDraggingSpeed :: !Bool
    , isDraggingHistory :: !Bool
    , isDraggingZoom :: !Bool
    } deriving (Show, Functor)

data Control :: * -> * where
    PlayButton :: Point -> Control a
    PauseButton :: Point -> Control a
    StepButton :: Point -> Control a
    RestartButton :: Point -> Control Double
    ZoomInButton :: Point -> Control a
    ZoomOutButton :: Point -> Control a
    PanningLayer :: Control a
    ResetViewButton :: Point -> Control a
    FastForwardButton :: Point -> Control a
    StartOverButton :: Point -> Control ([a], [a])
    BackButton :: Point -> Control Double
    TimeLabel :: Point -> Control Double
    SpeedSlider :: Point -> Control a
    ZoomSlider :: Point -> Control a
    UndoButton :: Point -> Control ([a], [a])
    RedoButton :: Point -> Control ([a], [a])
    HistorySlider :: Point -> Control ([a], [a])

wrappedInitial :: a -> Wrapped a
wrappedInitial w = Wrapped { 
      state = w,
      playbackSpeed = 1,
      lastInteractionTime = 1000,
      zoomFactor = 1,
      panCenter = SP 0 0,
      panDraggingAnchor = SNothing,
      isDraggingSpeed = False,
      isDraggingHistory = False,
      isDraggingZoom = False
    }

identical :: a -> a -> Bool
identical !x !y = isTrue# (reallyUnsafePtrEquality# x y)

toState :: (a -> a) -> (Wrapped a -> Wrapped a)
toState f w | identical s s' = w
            | otherwise = w { state = s' }
  where s  = state w
        s' = f s

wrappedStep :: (Double -> a -> a) -> Double -> Wrapped a -> Wrapped a
wrappedStep f dt w
  | playbackSpeed w == 0 = w
  | otherwise = toState (f (dt * playbackSpeed w)) w

reportDiff :: String -> (a -> a) -> (a -> a)
reportDiff msg f x = unsafePerformIO $ do
    a <- makeStableName $! x
    b <- makeStableName $! r
    return r
  where r = f x

wrappedEvent :: forall a . 
       (Wrapped a -> [Control a])
    -> (Double -> a -> a)
    -> (Event -> a -> a)
    -> Event
    -> Wrapped a
    -> Wrapped a
wrappedEvent ctrls stepHandler eventHandler event = markInteraction . handleChange
  where
    markInteraction w
      | TimePassing _  <- event, lastInteractionTime w > 5 = w
      | TimePassing dt <- event = w { lastInteractionTime = lastInteractionTime w + dt }
      | otherwise = w { lastInteractionTime = 0 }

    handleChange w0
      | playbackSpeed w0 == 0 || handled = w1
      | otherwise = toState (eventHandler (adaptEvent event)) w1
      where
        (w1, handled) = foldr doCtrl (w0, False) (ctrls w0)

        doCtrl _    (w, True)  = (w, True)
        doCtrl ctrl (w, False) = handleControl fullStep event ctrl w

        fullStep dt = stepHandler dt . eventHandler (TimePassing dt)

        adaptEvent (PointerMovement p) = PointerMovement (adaptPoint p)
        adaptEvent (PointerPress p)    = PointerPress (adaptPoint p)
        adaptEvent (PointerRelease p)  = PointerRelease (adaptPoint p)
        adaptEvent (TimePassing dt)    = TimePassing (dt * playbackSpeed w0)
        adaptEvent other               = other

        adaptPoint (x, y) = (x / k - dx, y / k - dy)

        SP dx dy = panCenter w1
        k        = zoomFactor w1

scaleRange :: (Double, Double) -> (Double, Double) -> Double -> Double
scaleRange (a1, b1) (a2, b2) x = min b2 $ max a2 $ (x - a1) / (b1 - a1) * (b2 - a2) + a2

snapSlider :: Double -> [Double] -> Double -> Double
snapSlider eps targets val = foldr snap val targets
    where snap t v | abs (t - v) < eps = t
                   | otherwise         = v

xToPlaybackSpeed :: Double -> Double
xToPlaybackSpeed x = snapSlider 0.2 [1..4] $ scaleRange (-1.4, 1.4) (0, 5) x

playbackSpeedToX :: Double -> Double
playbackSpeedToX s = scaleRange (0, 5) (-1.4, 1.4) s

zoomIncrement = 8 ** (1/10)

yToZoomFactor :: Double -> Double
yToZoomFactor y = zoomIncrement ** (scaleRange (-1.4, 1.4) (-10, 10) y)

zoomFactorToY :: Double -> Double
zoomFactorToY z = scaleRange (-10, 10) (-1.4, 1.4) (logBase zoomIncrement z)

handleControl ::
       (Double -> a -> a) -> Event -> Control a -> Wrapped a -> (Wrapped a, Bool)
handleControl _ (PointerPress (x, y)) (RestartButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {state = 0}, True)
handleControl _ (PointerPress (x, y)) (StartOverButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (fmap f w, True)
  where
    f (past, future) = let x:xs = reverse past in ([x], xs ++ future)
handleControl _ (PointerPress (x, y)) (PlayButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {playbackSpeed = 1}, True)
handleControl _ (PointerPress (x, y)) (PauseButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4  = (w {playbackSpeed = 0}, True)
handleControl _ (PointerPress (x, y)) (FastForwardButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4  = (w {playbackSpeed = min 5 $ max 2 $ playbackSpeed w + 1}, True)
handleControl _ (PointerPress (x, y)) (ZoomInButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = min (zoomIncrement ** 10) (zoomFactor w * zoomIncrement)}, True)
handleControl _ (PointerPress (x, y)) (ZoomOutButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = max (zoomIncrement ** (-10)) (zoomFactor w / zoomIncrement)}, True)
handleControl _ (PointerPress (x, y)) (ResetViewButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = 1, panCenter = SP 0 0}, True)
handleControl _ (PointerPress (x,y)) (BackButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 =
        (w {state = max 0 (state w - 0.1)}, True)
handleControl _ (PointerPress (x,y)) (UndoButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 =
        (fmap (\(x:xs, ys) -> (xs, x:ys)) w, True)
handleControl _ (PointerPress (x,y)) (RedoButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 =
        (fmap (\(xs, y:ys) -> (y:xs, ys)) w, True)
handleControl f (PointerPress (x, y)) (StepButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {state = f 0.1 (state w)}, True)
handleControl _ (PointerPress (x, y)) (SpeedSlider (cx, cy)) w
    | abs (x - cx) < 1.5 && abs (y - cy) < 0.4 = 
      (w {playbackSpeed = xToPlaybackSpeed (x - cx), isDraggingSpeed = True}, True)
handleControl _ (PointerMovement (x, y)) (SpeedSlider (cx, cy)) w
    | isDraggingSpeed w = (w {playbackSpeed = xToPlaybackSpeed (x - cx)}, True)
handleControl _ (PointerRelease (x, y)) (SpeedSlider (cx, cy)) w
    | isDraggingSpeed w = (w {playbackSpeed = xToPlaybackSpeed (x - cx), isDraggingSpeed = False}, True)
handleControl _ (PointerPress (x, y)) (ZoomSlider (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 1.5 = 
      (w {zoomFactor = yToZoomFactor (y - cy), isDraggingZoom = True}, True)
handleControl _ (PointerMovement (x, y)) (ZoomSlider (cx, cy)) w
    | isDraggingZoom w = (w {zoomFactor = yToZoomFactor (y - cy)}, True)
handleControl _ (PointerRelease (x, y)) (ZoomSlider (cx, cy)) w
    | isDraggingZoom w = (w {zoomFactor = yToZoomFactor (y - cy), isDraggingZoom = False}, True)
handleControl _ (PointerPress (x, y)) (HistorySlider (cx, cy)) w
    | abs (x - cx) < 2.5 && abs (y - cy) < 0.4 = 
      (travelToTime (x - cx) <$> w {isDraggingHistory = True}, True)
handleControl _ (PointerMovement (x, y)) (HistorySlider (cx, cy)) w
    | isDraggingHistory w = (travelToTime (x - cx) <$> w, True)
handleControl _ (PointerRelease (x, y)) (HistorySlider (cx, cy)) w
    | isDraggingHistory w = (travelToTime (x - cx) <$> w {isDraggingHistory = False}, True)
handleControl _ (PointerPress (x, y)) PanningLayer w =
      (w {panDraggingAnchor = SJust (SP x y)}, True)
handleControl _ (PointerMovement (x, y)) PanningLayer w
    | SJust (SP ax ay) <- panDraggingAnchor w
    , SP px py <- panCenter w
    = (w { panCenter = SP (px + (x - ax) / zoomFactor w)
                          (py + (y - ay) / zoomFactor w),
           panDraggingAnchor = SJust (SP x y)
         }, True)
handleControl _ (PointerRelease (x, y)) PanningLayer w
    | SJust (SP ax ay) <- panDraggingAnchor w = (w {panDraggingAnchor = SNothing}, True)
handleControl _ _ _ w = (w, False)

travelToTime :: Double -> ([s],[s]) -> ([s],[s])  
travelToTime t (past, future)
    | n == 1    = (past, future)
    | otherwise = go past future (n1 - desiredN1)
  where n1 = length past
        n2 = length future
        n = n1 + n2
        desiredN1 = round (scaleRange (-2.4, 2.4) (1, fromIntegral n) t)
        go past future diff
          | diff > 0 = go (tail past) (head past : future) (diff - 1)
          | diff < 0 = go (head future : past) (tail future) (diff + 1)
          | otherwise = (past, future)

wrappedDraw ::
       (Wrapped a -> [Control a]) -> (a -> Picture) -> Wrapped a -> Picture
wrappedDraw ctrls f w = drawControlPanel ctrls w <> dilated k (translated dx dy (f (state w)))
  where SP dx dy = panCenter w
        k        = zoomFactor w

drawControlPanel :: (Wrapped a -> [Control a]) -> Wrapped a -> Picture
drawControlPanel ctrls w
    | alpha > 0 = pictures [drawControl w alpha c | c <- ctrls w]
    | otherwise = blank
  where
    alpha
        | lastInteractionTime w < 4.5 = 1
        | lastInteractionTime w < 5.0 = 10 - 2 * lastInteractionTime w
        | otherwise = 0

drawControl :: Wrapped a -> Double -> Control a -> Picture
drawControl _ alpha (RestartButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (thickArc 0.1 (pi / 6) (11 * pi / 6) 0.2 <>
             translated 0.173 (-0.1) (solidRectangle 0.17 0.17)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (StartOverButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (thickArc 0.1 (pi / 6) (11 * pi / 6) 0.2 <>
             translated 0.173 (-0.1) (solidRectangle 0.17 0.17)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (PlayButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (solidPolygon [(-0.2, 0.25), (-0.2, -0.25), (0.2, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (PauseButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.15) 0 (solidRectangle 0.2 0.6) <>
             translated 0.15 0 (solidRectangle 0.2 0.6)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (FastForwardButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (solidPolygon [(-0.3, 0.25), (-0.3, -0.25), (-0.05, 0)] <>
             solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (ZoomInButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.05) (0.05) (
                thickCircle 0.1 0.22 <>
                solidRectangle 0.06 0.25 <>
                solidRectangle 0.25 0.06 <>
                rotated (-pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (ZoomOutButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.05) (0.05) (
                thickCircle 0.1 0.22 <>
                solidRectangle 0.25 0.06 <>
                rotated (-pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ _ PanningLayer = blank
drawControl _ alpha (ResetViewButton (x,y)) = translated x y p
  where
    p =
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.7 0.2) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.2 0.7) <>
        colored (RGBA 0.0 0.0 0.0 alpha) (thickRectangle 0.1 0.5 0.5) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (BackButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated 0.15 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(-0.05, 0.25), (-0.05, -0.25), (-0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (UndoButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated 0.15 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(-0.05, 0.25), (-0.05, -0.25), (-0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (StepButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.15) 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha (RedoButton (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.15) 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl w alpha (TimeLabel (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (scaled 0.5 0.5 $ lettering (T.pack (showFFloatAlt (Just 4) (state w) "s"))) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 3.0 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 3.0 0.8)
drawControl w alpha (SpeedSlider (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated xoff 0.75 $ scaled 0.5 0.5 $
                 lettering (T.pack (showFFloatAlt (Just 2) (playbackSpeed w) "x"))) <>
        colored (RGBA 0 0 0 alpha) (translated xoff 0 (solidRectangle 0.2 0.8)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 2.8 0.25) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 2.8 0.25)
    xoff = playbackSpeedToX (playbackSpeed w)
drawControl w alpha (ZoomSlider (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-1.1) yoff $ scaled 0.5 0.5 $
                 lettering (T.pack (show (round (zoomFactor w * 100) :: Int) ++ "%"))) <>
        colored (RGBA 0 0 0 alpha) (translated 0 yoff (solidRectangle 0.8 0.2)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.25 2.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.25 2.8)
    yoff = zoomFactorToY (zoomFactor w)
drawControl w alpha (HistorySlider (x,y)) = translated x y p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated xoff 0.75 $ scaled 0.5 0.5 $
                 lettering (T.pack (show n1 ++ "/" ++ show (n1 + n2)))) <>
        colored (RGBA 0.0 0.0 0.0 alpha) (translated xoff 0 (solidRectangle 0.2 0.8)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 4.8 0.25) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 4.8 0.25)
    xoff | n < 2 = 2.4
         | otherwise   = scaleRange (1, fromIntegral n) (-2.4, 2.4) (fromIntegral n1)
    n1 = length (fst (state w))
    n2 = length (snd (state w))
    n  = n1 + n2

drawingControls :: Wrapped () -> [Control ()]
drawingControls w
    | lastInteractionTime w > 5 = []
    | otherwise = commonControls ++ resetViewButton
  where
    commonControls = [
        PanningLayer,
        ZoomInButton (9, -4),
        ZoomOutButton (9, -8),
        ZoomSlider (9, -6)
      ]
    resetViewButton
      | zoomFactor w /= 1 || panCenter w /= SP 0 0 = [ResetViewButton (9, -3)]
      | otherwise = []

drawingOf pic = runInspect drawingControls () (\_ _ -> ()) (\_ _ -> ()) (const pic)

animationControls :: Wrapped Double -> [Control Double]
animationControls w
    | lastInteractionTime w > 5 = []
    | otherwise = commonControls ++ pauseDependentControls ++
                  backButton ++ resetViewButton
  where
    commonControls = [
        PanningLayer,
        RestartButton (-9, -9),
        TimeLabel (8, -9),
        SpeedSlider (-3, -9),
        FastForwardButton (-1, -9),
        ZoomInButton (9, -4),
        ZoomOutButton (9, -8),
        ZoomSlider (9, -6)
      ]
    pauseDependentControls
      | playbackSpeed w == 0 = [PlayButton (-8, -9), StepButton (-6, -9)]
      | otherwise = [PauseButton (-8, -9)]
    backButton
      | playbackSpeed w == 0 && state w > 0 = [BackButton (-7, -9)]
      | otherwise = []
    resetViewButton
      | zoomFactor w /= 1 || panCenter w /= SP 0 0 = [ResetViewButton (9, -3)]
      | otherwise = []

animationOf f = runInspect animationControls 0 (+) (\_ r -> r) f

simulationControls :: Wrapped w -> [Control w]
simulationControls w
    | lastInteractionTime w > 5 = []
    | otherwise = commonControls ++ pauseDependentControls ++ resetViewButton
  where
    commonControls = [
        PanningLayer,
        FastForwardButton (-4, -9),
        SpeedSlider (-6, -9),
        ZoomInButton (9, -4),
        ZoomOutButton (9, -8),
        ZoomSlider (9, -6)
      ]
    pauseDependentControls
      | playbackSpeed w == 0 = [PlayButton (-8, -9), StepButton (-2, -9)]
      | otherwise = [PauseButton (-8, -9)]
    resetViewButton
      | zoomFactor w /= 1 || panCenter w /= SP 0 0 = [ResetViewButton (9, -3)]
      | otherwise = []

statefulDebugControls :: Wrapped ([w],[w]) -> [Control ([w],[w])]
statefulDebugControls w
    | lastInteractionTime w > 5 = []
    | otherwise = panningLayer ++ pauseDependentControls ++ commonControls ++
                  resetViewButton
  where   
    hasHistory = not (null (tail (fst (state w))))
    hasFuture  = not (null (snd (state w)))
    advance | hasFuture  = [RedoButton (6, -9)]
            | otherwise  = [StepButton (6, -9)]
    regress | hasHistory = [UndoButton (0, -9)]
            | otherwise  = []
    commonControls = [
        StartOverButton (-1, -9),
        FastForwardButton (-4, -9),
        SpeedSlider (-6, -9),
        ZoomInButton (9, -4),
        ZoomOutButton (9, -8),
        ZoomSlider (9, -6)
      ]
    pauseDependentControls
      | playbackSpeed w == 0 = 
            [PlayButton (-8, -9), HistorySlider (3, -9)] ++ advance ++ regress
      | otherwise = [PauseButton (-8, -9)]
    resetViewButton
      | zoomFactor w /= 1 || panCenter w /= SP 0 0 = [ResetViewButton (9, -3)]
      | otherwise = []
    panningLayer 
      | playbackSpeed w == 0 = [PanningLayer]
      | otherwise = []

simulationOf initial step draw =
    runInspect simulationControls initial step (\_ r -> r) draw

prependIfChanged :: (a -> a) -> ([a],[a]) -> ([a],[a])
prependIfChanged f (x:xs, ys)
    | identical x x' = (x:xs, ys)
    | otherwise = (x':x:xs, ys)
    where x' = f x

debugSimulationOf initial simStep simDraw =
    runInspect statefulDebugControls ([initial],[]) step (\_ r -> r) draw
  where
    step dt = prependIfChanged (simStep dt)
    draw (x:_, _) = simDraw x

debugInteractionOf initial baseStep baseEvent baseDraw = 
  runInspect statefulDebugControls ([initial], []) step event draw 
  where
    step dt = prependIfChanged (baseStep dt)
    event e = prependIfChanged (baseEvent e)
    draw (x:_, _) = baseDraw x

debugActivityOf initial change picture =
    debugInteractionOf initial (const id) change picture

trace msg x = unsafePerformIO $ do
    oldMode <- hGetBuffering stderr
    hSetBuffering stderr (BlockBuffering Nothing)
    hPutStrLn stderr (T.unpack msg)
    hFlush stderr
    hSetBuffering stderr oldMode
    return x
