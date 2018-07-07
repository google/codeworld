{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE DataKinds #-}

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
module CodeWorld.Driver
    ( drawingOf
    , animationOf
    , activityOf
    , groupActivityOf
    , unsafeGroupActivityOf
    , simulationOf
    , interactionOf
    , collaborationOf
    , unsafeCollaborationOf
    , trace
    ) where

import CodeWorld.CollaborationUI (SetupPhase(..), Step(..), UIState)
import qualified CodeWorld.CollaborationUI as CUI
import qualified CodeWorld.CanvasM as CM
import CodeWorld.Color
import CodeWorld.Event
import CodeWorld.Picture
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (chr)
import Data.List (find, zip4, intercalate)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid
import Data.Serialize
import Data.Serialize.Text
import qualified Data.Text as T
import Data.Text (Text, pack, singleton)
import qualified Debug.Trace
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.Stack
import GHC.StaticPtr
import Numeric
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Mem.StableName
import System.Random
import Text.Printf
import Text.Read
#ifdef ghcjs_HOST_OS
import CodeWorld.CanvasM (CanvasM, runCanvasM)
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
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers hiding (error)
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
data Drawing
    = Shape Drawer
    | Transformation (DrawState -> DrawState)
                     Drawing
    | Drawings [Drawing]

instance Monoid Drawing where
    mempty = Drawings []
    mappend a (Drawings bs) = Drawings (a : bs)
    mappend a b = Drawings [a, b]
    mconcat = Drawings

-- A DrawState is an affine transformation matrix, plus a Bool indicating whether
-- a color has been chosen yet.
type DrawState = (Double, Double, Double, Double, Double, Double, Maybe Color)

-- A NodeId a unique id for each node in a Picture of Drawing, chosen by the order
-- the node appears in DFS. When a Picture is converted to a drawing the NodeId's of
-- corresponding nodes are shared. Always >=0.
type NodeId = Int

pictureToDrawing :: Picture -> Drawing
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
pictureToDrawing (Pictures ps) = Drawings $ pictureToDrawing <$> ps

initialDS :: DrawState
initialDS = (1, 0, 0, 1, 0, 0, Nothing)

translateDS :: Double -> Double -> DrawState -> DrawState
translateDS x y (a, b, c, d, e, f, hc) =
    (a, b, c, d, a * 25 * x + c * 25 * y + e, b * 25 * x + d * 25 * y + f, hc)

scaleDS :: Double -> Double -> DrawState -> DrawState
scaleDS x y (a, b, c, d, e, f, hc) = (x * a, x * b, y * c, y * d, e, f, hc)

rotateDS :: Double -> DrawState -> DrawState
rotateDS r (a, b, c, d, e, f, hc) =
    ( a * cos r + c * sin r
    , b * cos r + d * sin r
    , c * cos r - a * sin r
    , d * cos r - b * sin r
    , e
    , f
    , hc)

setColorDS :: Color -> DrawState -> DrawState
setColorDS col (a, b, c, d, e, f, Nothing) = (a, b, c, d, e, f, Just col)
setColorDS col@(RGBA _ _ _ 0) (a, b, c, d, e, f, _) =
    (a, b, c, d, e, f, Just col)
setColorDS _ (a, b, c, d, e, f, Just col) = (a, b, c, d, e, f, Just col)

getColorDS :: DrawState -> Maybe Color
getColorDS (a, b, c, d, e, f, col) = col

polygonDrawer :: [Point] -> Bool -> Drawer
pathDrawer :: [Point] -> Double -> Bool -> Bool -> Drawer
sectorDrawer :: Double -> Double -> Double -> Drawer
arcDrawer :: Double -> Double -> Double -> Double -> Drawer
textDrawer :: TextStyle -> Font -> Text -> Drawer
logoDrawer :: Drawer
coordinatePlaneDrawer :: Drawer
coordinatePlaneDrawing :: Drawing
coordinatePlaneDrawing = pictureToDrawing $ axes <> numbers <> guidelines
  where
    xline y = thickPolyline 0.01 [(-10, y), (10, y)]
    xaxis = thickPolyline 0.03 [(-10, 0), (10, 0)]
    axes = xaxis <> rotated (pi / 2) xaxis
    xguidelines = pictures [xline k | k <- [-10,-9 .. 10]]
    guidelines = xguidelines <> rotated (pi / 2) xguidelines
    numbers = xnumbers <> ynumbers
    xnumbers =
        pictures
            [ translated
                (fromIntegral k)
                0.3
                (scaled 0.5 0.5 (text (pack (show k))))
            | k <- [-9,-8 .. 9]
            , k /= 0
            ]
    ynumbers =
        pictures
            [ translated
                0.3
                (fromIntegral k)
                (scaled 0.5 0.5 (text (pack (show k))))
            | k <- [-9,-8 .. 9]
            , k /= 0
            ]

--------------------------------------------------------------------------------
-- GHCJS implementation of drawing
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
               "$1.drawImage($2, $3, $4, $5, $6);" js_canvasDrawImage ::
               Canvas.Context -> Element -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe
               "$1.getContext('2d', { alpha: false })" js_getCodeWorldContext ::
               Canvas.Canvas -> IO Canvas.Context

foreign import javascript unsafe "performance.now()"
               js_getHighResTimestamp :: IO Double

canvasFromElement :: Element -> Canvas.Canvas
canvasFromElement = Canvas.Canvas . unElement

elementFromCanvas :: Canvas.Canvas -> Element
elementFromCanvas = pFromJSVal . jsval

getTime :: IO Double
getTime = (/ 1000) <$> js_getHighResTimestamp

nextFrame :: IO Double
nextFrame = waitForAnimationFrame >> getTime

withDS :: DrawState -> CanvasM a -> CanvasM a
withDS (ta, tb, tc, td, te, tf, col) action = CM.saveRestore $ do
    CM.transform ta tb tc td te tf
    CM.beginPath
    action

applyColor :: DrawState -> CanvasM ()
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

drawCodeWorldLogo ::
       DrawState -> Int -> Int -> Int -> Int -> CanvasM ()
drawCodeWorldLogo ds x y w h = do
    Just doc <- liftIO $ currentDocument
    Just canvas <- liftIO $ getElementById doc ("cwlogo" :: JSString)
    case getColorDS ds of
        Nothing -> CM.drawImage (canvasFromElement canvas) x y w h
        Just (RGBA r g b a)
            -- This is a tough case.  The best we can do is to allocate an
            -- offscreen buffer as a temporary.
         -> do
            (img, _) <- CM.newImage w h $ do
                applyColor ds
                CM.fillRect 0 0 (fromIntegral w) (fromIntegral h)
                CM.globalCompositeOperation "destination-in"
                CM.drawImage (canvasFromElement canvas) 0 0 w h
            CM.drawImage img x y w h

-- Debug Mode logic
inspectStatic :: Picture -> IO ()
inspectStatic pic = inspect (return pic) (\_ -> return ()) (\_ _ -> return ())

inspect ::
       IO Picture -> (Bool -> IO ()) -> (Bool -> Maybe NodeId -> IO ()) -> IO ()
inspect getPic handleActive highlight =
    initDebugMode (handlePointRequest getPic) handleActive getPic highlight

handlePointRequest :: IO Picture -> Point -> IO (Maybe NodeId)
handlePointRequest getPic pt = do
    drawing <- pictureToDrawing <$> getPic
    findTopShapeFromPoint pt drawing

initDebugMode ::
       (Point -> IO (Maybe NodeId))
    -> (Bool -> IO ())
    -> IO Picture
    -> (Bool -> Maybe NodeId -> IO ())
    -> IO ()
initDebugMode getnode setactive getpicture highlight = do
    getnodeCB <-
        syncCallback1' $ \pointJS -> do
            let obj = unsafeCoerce pointJS
            x <- pFromJSVal <$> getProp "x" obj
            y <- pFromJSVal <$> getProp "y" obj
            pToJSVal . fromMaybe (-1) <$> getnode (x, y)
    setactiveCB <- syncCallback1 ContinueAsync $ setactive . pFromJSVal
    getpictureCB <- syncCallback' $ getpicture >>= picToObj
    highlightCB <-
        syncCallback2 ContinueAsync $ \t n ->
            let select = pFromJSVal t
                node =
                    case ((pFromJSVal n) :: Int) < 0 of
                        True -> Nothing
                        False -> Just $ pFromJSVal n
            in highlight select node
    drawCB <-
        syncCallback2 ContinueAsync $ \c n -> do
            let canvas = unsafeCoerce c :: Element
                nodeId = pFromJSVal n
            drawing <- pictureToDrawing <$> getpicture
            let node = fromMaybe (Drawings []) $ fst <$> getDrawNode nodeId drawing
            offscreenCanvas <- Canvas.create 500 500
            setCanvasSize canvas canvas
            setCanvasSize (elementFromCanvas offscreenCanvas) canvas
            screen <- js_getCodeWorldContext (canvasFromElement canvas)
            rect <- getBoundingClientRect canvas
            withScreen (elementFromCanvas offscreenCanvas) rect $
                drawFrame (node <> coordinatePlaneDrawing)
            rect <- getBoundingClientRect canvas
            cw <- ClientRect.getWidth rect
            ch <- ClientRect.getHeight rect
            js_canvasDrawImage
                screen
                (elementFromCanvas offscreenCanvas)
                0
                0
                (round cw)
                (round ch)
    js_initDebugMode getnodeCB setactiveCB getpictureCB highlightCB drawCB

picToObj :: Picture -> IO JSVal
picToObj = fmap fst . flip State.runStateT 0 . picToObj'

picToObj' :: Picture -> State.StateT Int IO JSVal
picToObj' pic =
    case pic of
        SolidPolygon cs pts -> do
            obj <- init "solidPolygon"
            ptsJS <- pointsToArr pts
            setProps [("points", ptsJS), ("smooth", pToJSVal False)] obj
            retVal obj
        SolidClosedCurve cs pts -> do
            obj <- init "solidClosedCurve"
            ptsJS <- pointsToArr pts
            setProps [("points", ptsJS), ("smooth", pToJSVal True)] obj
            retVal obj
        Polygon cs pts -> do
            obj <- init "polygon"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal (0 :: Double))
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        ThickPolygon cs pts w -> do
            obj <- init "thickPolygon"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal w)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        Rectangle _ w h -> do
            obj <- init "rectangle"
            setProps
                [ ("width",  pToJSVal w)
                , ("height", pToJSVal h)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        SolidRectangle _ w h -> do
            obj <- init "solidRectangle"
            setProps 
                [ ("width",  pToJSVal w)
                , ("height", pToJSVal h)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        ThickRectangle _ lw w h-> do
            obj <- init "thickRectangle"
            setProps
                [
                  ("linewidth", pToJSVal lw)
                , ("width",  pToJSVal w)
                , ("height", pToJSVal h)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        ClosedCurve cs pts -> do
            obj <- init "closedCurve"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal (0 :: Double))
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal True)
                ]
                obj
            retVal obj
        ThickClosedCurve cs pts w -> do
            obj <- init "thickClosedCurve"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal w)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal True)
                ]
                obj
            retVal obj
        Circle cs r -> do
            obj <- init "circle"
            setProps
                [ ("radius", pToJSVal r)
                , ("closed", pToJSVal True)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        SolidCircle cs r -> do
            obj <- init "solidCircle"
            setProps
                [ ("radius", pToJSVal r)
                , ("closed", pToJSVal True)
                ]
                obj
            retVal obj
        ThickCircle cs lw r -> do
            obj <- init "thickCircle"
            setProps
                [ ("radius", pToJSVal r)
                , ("linewidth", pToJSVal lw)
                , ("endAngle", pToJSVal True)
                , ("radius", pToJSVal False)
                ]
                obj
            retVal obj
        Polyline cs pts -> do
            obj <- init "polyline"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal (0 :: Double))
                , ("closed", pToJSVal False)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        ThickPolyline cs pts w -> do
            obj <- init "thickPolyline"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal w)
                , ("closed", pToJSVal False)
                , ("smooth", pToJSVal False)
                ]
                obj
            retVal obj
        Curve cs pts -> do
            obj <- init "curve"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal (0 :: Double))
                , ("closed", pToJSVal False)
                , ("smooth", pToJSVal True)
                ]
                obj
            retVal obj
        ThickCurve cs pts w -> do
            obj <- init "thickCurve"
            ptsJS <- pointsToArr pts
            setProps
                [ ("points", ptsJS)
                , ("width", pToJSVal w)
                , ("closed", pToJSVal False)
                , ("smooth", pToJSVal True)
                ]
                obj
            retVal obj
        Sector cs b e r -> do
            obj <- init "sector"
            setProps
                [ ("startAngle", pToJSVal b)
                , ("endAngle", pToJSVal e)
                , ("radius", pToJSVal r)
                ]
                obj
            retVal obj
        Arc cs b e r -> do
            obj <- init "arc"
            setProps
                [ ("startAngle", pToJSVal b)
                , ("endAngle", pToJSVal e)
                , ("radius", pToJSVal r)
                , ("width", pToJSVal (0 :: Double))
                ]
                obj
            retVal obj
        ThickArc cs b e r w -> do
            obj <- init "thickArc"
            setProps
                [ ("startAngle", pToJSVal b)
                , ("endAngle", pToJSVal e)
                , ("radius", pToJSVal r)
                , ("width", pToJSVal w)
                ]
                obj
            retVal obj
        Lettering cs txt -> do
            obj <- init "lettering"
            setProps
                [ ("font", pToJSVal $ fontString Plain Serif)
                , ("text", pToJSVal txt)
                ]
                obj
            retVal obj
        StyledLettering cs style font txt -> do
            obj <- init "styledLettering"
            setProps
                [ ("font", pToJSVal $ fontString style font)
                , ("text", pToJSVal txt)
                ]
                obj
            retVal obj
        Color cs (RGBA r g b a) p -> do
            obj <- init "color"
            picJS <- picToObj' p
            setProps
                [ ("picture", picJS)
                , ("red", pToJSVal r)
                , ("green", pToJSVal g)
                , ("blue", pToJSVal b)
                , ("alpha", pToJSVal a)
                ]
                obj
            retVal obj
        Translate cs x y p -> do
            obj <- init "translate"
            picJS <- picToObj' p
            setProps
                [("picture", picJS), ("x", pToJSVal x), ("y", pToJSVal y)]
                obj
            retVal obj
        Scale cs x y p -> do
            obj <- init "scale"
            picJS <- picToObj' p
            setProps
                [("picture", picJS), ("x", pToJSVal x), ("y", pToJSVal y)]
                obj
            retVal obj
        Dilate cs k p -> do
            obj <- init "scale"
            picJS <- picToObj' p
            setProps
                [("picture", picJS), ("k", pToJSVal k)]
                obj
            retVal obj
        Rotate cs angle p -> do
            obj <- init "rotate"
            picJS <- picToObj' p
            setProps [("picture", picJS), ("angle", pToJSVal angle)] obj
            retVal obj
        Pictures ps -> do
            obj <- init "pictures"
            arr <- liftIO $ Array.create
            let push = liftIO . flip Array.push arr
            mapM (\p -> picToObj' p >>= push) ps
            setProps [("pictures", unsafeCoerce arr)] obj
            retVal obj
        Logo cs -> init "logo" >>= retVal
        Blank cs -> init "blank" >>= retVal
        CoordinatePlane cs -> init "coordinatePlane" >>= retVal
  where
    incId :: State.StateT Int IO Int
    incId = do
        currentId <- State.get
        State.put (currentId + 1)
        return currentId
    init :: JSString -> State.StateT Int IO Object
    init tp = do
        obj <- liftIO create
        liftIO $ setProp "type" (pToJSVal tp) obj
        id <- incId
        liftIO $ setProp "id" (pToJSVal id) obj
        liftIO $ setCallInfo pic obj
        return obj
    objToJSVal = unsafeCoerce :: Object -> JSVal
    retVal :: Object -> State.StateT Int IO JSVal
    retVal = return . objToJSVal
    pointsToArr :: [Point] -> State.StateT Int IO JSVal
    pointsToArr pts =
        liftIO $ do
            let go [] _ = return ()
                go ((x, y):pts) arr = do
                    Array.push (pToJSVal x) arr
                    Array.push (pToJSVal y) arr
                    go pts arr
            arr <- Array.create
            go pts arr
            return $ (unsafeCoerce arr :: JSVal)
    setProps xs obj = liftIO $ void $ mapM (\(s, v) -> setProp s v obj) xs

trim :: Int -> String -> String
trim x y = let mid = (x - 2) `div` 2
    in case x >= (length y) of
                True -> y :: String
                False -> take mid y ++ ".." ++ (reverse $ take mid $ reverse y)

showShortFloat :: Double -> String
showShortFloat x = stripZeros (showFFloatAlt (Just 4) x "")
  where stripZeros = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

describePicture :: Picture -> String
describePicture (Rectangle _ w h) = "rectangle { width = " ++ showShortFloat w ++ ", height = " ++ showShortFloat h ++ " }"
describePicture (SolidPolygon _ pts) = "solidPolygon { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (SolidClosedCurve _ pts) = "solidClosedCurve { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (Polygon _ pts) = "polygon { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (ThickPolygon _ pts w) = "thickPolygon { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ ", width = " ++ showShortFloat w ++ " }"
describePicture (SolidRectangle _ w h) = "solidRectangle { width = " ++ showShortFloat w ++ ", height = " ++ showShortFloat h ++ " }"
describePicture (ThickRectangle _ lw w h) = "thickRectangle { linewidth = " ++ showShortFloat lw ++ ", width = " ++ showShortFloat w ++ ", height = " ++ showShortFloat h ++ " }"
describePicture (ClosedCurve _ pts) = "closedCurve { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (ThickClosedCurve _ pts w) = "thickClosedCurve { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ ", width = " ++ showShortFloat w ++ " }"
describePicture (Circle _ r) = "circle { radius = " ++ showShortFloat r ++ " }"
describePicture (SolidCircle _ r) = "solidCircle { radius = " ++ showShortFloat r ++ " }"
describePicture (ThickCircle _ lw r) =  "thickCircle { linewidth = " ++ showShortFloat lw ++ ", radius = " ++ showShortFloat r ++ " }"
describePicture (Polyline _ pts) = "polyline { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (ThickPolyline _ pts w) = "thickPolyline { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ ", width = " ++ showShortFloat w ++ " }"
describePicture (Curve _ pts) = "curve { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ " }"
describePicture (ThickCurve _ pts w) = "thickCurve { points = " ++ intercalate ", " [ "(" ++ showShortFloat x ++ ", " ++ showShortFloat y ++ ")" | (x, y) <- pts ] ++ ", width = " ++ showShortFloat w ++ " }"
describePicture (Sector _ b e r) =  "sector { startAngle = " ++ showShortFloat (180 * b / pi) ++ "° (" ++ showShortFloat b ++ " radians)" ++ ", endAngle = " ++ showShortFloat (180 * e / pi) ++ "°" ++ " (" ++ showShortFloat e ++ " radias )," ++ " radius = " ++ showShortFloat r ++ " }"
describePicture (Arc _ b e r) = "arc { startAngle = " ++ showShortFloat (180 * b / pi) ++ "° (" ++ showShortFloat b ++ " radians)" ++ ", endAngle = " ++ showShortFloat (180 * e / pi) ++ "°" ++ " (" ++ showShortFloat e ++ " radias )," ++ " radius = " ++ showShortFloat r ++ " }"
describePicture (ThickArc _ b e r w) =  "thickArc { startAngle = " ++ showShortFloat (180 * b / pi) ++ "° (" ++ showShortFloat b ++ " radians)" ++ ", endAngle = " ++ showShortFloat (180 * e / pi) ++ "°" ++ " (" ++ showShortFloat e ++ " radias )," ++ " radius = " ++ showShortFloat r ++ ", width = " ++ showShortFloat w ++ " }"
describePicture (Lettering _ txt) = printf "lettering { text = '%s' }" txt
describePicture (Blank _) = "blank"
describePicture (StyledLettering _ style font txt) = printf " styledLettering { style = %s , font = %s, text = '%s' }" (show style) (show font) txt
describePicture (Color _ (RGBA r g b a) _) = "colored { color = RGBA(" ++ showShortFloat r ++ ", " ++ showShortFloat g ++ ", " ++ showShortFloat b ++ ", " ++ showShortFloat a ++ ") }"
describePicture (Translate _ x y _) = "translated { x = " ++ showShortFloat x ++ ", y = " ++ showShortFloat y ++ " }"
describePicture (Scale _ x y _) = "scaled { x = " ++ showShortFloat x ++ ", y = " ++ showShortFloat y ++ " }"
describePicture (Rotate _ angle _) = "rotated { angle = " ++ showShortFloat angle ++ "° }"
describePicture (Dilate _ k _) = "dilated { factor = " ++ showShortFloat k ++  " }"
describePicture (Logo _) = "codeWorldLogo"
describePicture (CoordinatePlane _) = "coordinatePlane"
describePicture (Pictures _) = "pictures"

setCallInfo :: Picture -> Object -> IO ()
setCallInfo pic obj = do
    case getPictureSrcLoc pic of
        Just loc -> do
            setProp "name" (pToJSVal $ (trim 80 . describePicture) pic) obj
            setProp "startLine" (pToJSVal $ srcLocStartLine loc) obj
            setProp "startCol" (pToJSVal $ srcLocStartCol loc) obj
            setProp "endLine" (pToJSVal $ srcLocEndLine loc) obj
            setProp "endCol" (pToJSVal $ srcLocEndCol loc) obj
        Nothing -> return ()

getPictureSrcLoc :: Picture -> Maybe SrcLoc
getPictureSrcLoc (SolidPolygon loc _) = Just loc
getPictureSrcLoc (SolidClosedCurve loc _) = Just loc
getPictureSrcLoc (Polygon loc _) = Just loc
getPictureSrcLoc (ThickPolygon loc _ _) = Just loc
getPictureSrcLoc (Rectangle loc _ _) = Just loc
getPictureSrcLoc (SolidRectangle loc _ _) = Just loc
getPictureSrcLoc (ThickRectangle loc _ _ _) = Just loc
getPictureSrcLoc (ClosedCurve loc _) = Just loc
getPictureSrcLoc (ThickClosedCurve loc _ _) = Just loc
getPictureSrcLoc (Circle loc _) = Just loc
getPictureSrcLoc (SolidCircle loc _) = Just loc
getPictureSrcLoc (ThickCircle loc _ _) = Just loc
getPictureSrcLoc (Polyline loc _) = Just loc
getPictureSrcLoc (ThickPolyline loc _ _) = Just loc
getPictureSrcLoc (Curve loc _) = Just loc
getPictureSrcLoc (ThickCurve loc _ _) = Just loc
getPictureSrcLoc (Sector loc _ _ _) = Just loc
getPictureSrcLoc (Arc loc _ _ _) = Just loc
getPictureSrcLoc (ThickArc loc _ _ _ _) = Just loc
getPictureSrcLoc (Lettering loc _) = Just loc
getPictureSrcLoc (Blank loc) = Just loc
getPictureSrcLoc (StyledLettering loc _ _ _) = Just loc
getPictureSrcLoc (Color loc _ _) = Just loc
getPictureSrcLoc (Translate loc _ _ _) = Just loc
getPictureSrcLoc (Scale loc _ _ _) = Just loc
getPictureSrcLoc (Dilate loc _ _) = Just loc
getPictureSrcLoc (Rotate loc _ _) = Just loc
getPictureSrcLoc (Logo loc) = Just loc
getPictureSrcLoc (CoordinatePlane loc) = Just loc
getPictureSrcLoc (Pictures _) = Nothing

-- If a picture is found, the result will include an array of the base picture
-- and all transformations.
findTopShapeFromPoint :: Point -> Drawing -> IO (Maybe NodeId)
findTopShapeFromPoint (x, y) pic = do
    buf <- Canvas.create 500 500
    ctx <- Canvas.getContext buf
    (found, node) <- runCanvasM ctx $
        findTopShape (translateDS (10 - x / 25) (y / 25 - 10) initialDS)
                     pic
    case found of
        True -> return $ Just node
        False -> return Nothing

findTopShape :: DrawState -> Drawing -> CanvasM (Bool, Int)
findTopShape ds (Shape drawer) = do
    contained <- shapeContains $ drawer ds
    case contained of
        True -> return (True, 0)
        False -> return (False, 1)
findTopShape ds (Transformation f d) =
    map2 (+ 1) $ findTopShape (f ds) d
findTopShape ds (Drawings []) = return (False, 1)
findTopShape ds (Drawings (dr:drs)) = do
    (found, count) <- findTopShape ds dr
    case found of
        True -> return (True, count + 1)
        False -> map2 (+ count) $ findTopShape ds (Drawings drs)

map2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
map2 = fmap . fmap

foreign import javascript unsafe "initDebugMode($1,$2,$3,$4,$5)"
               js_initDebugMode :: Callback (JSVal -> IO JSVal)
                                -> Callback (JSVal -> IO ())
                                -> Callback (IO JSVal)
                                -> Callback (JSVal -> JSVal -> IO ())
                                -> Callback (JSVal -> JSVal -> IO ())
                                -> IO ()

-----------------------------------------------------------------------------------
-- GHCJS Drawing
type Drawer = DrawState -> DrawMethods

data DrawMethods = DrawMethods
    { drawShape :: CanvasM ()
    , shapeContains :: CanvasM Bool
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

coordinatePlaneDrawer ds =
    DrawMethods
    { drawShape = drawDrawing ds coordinatePlaneDrawing
    , shapeContains = fst <$> findTopShape ds coordinatePlaneDrawing
    }

foreign import javascript unsafe "showCanvas()" js_showCanvas ::
               IO ()

followPath :: [Point] -> Bool -> Bool -> CanvasM ()
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

drawFigure :: DrawState -> Double -> CanvasM () -> CanvasM ()
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
    fontName (NamedFont txt) =
        "\"" <> T.filter (/= '"') txt <> "\""

drawDrawing :: DrawState -> Drawing -> CanvasM ()
drawDrawing ds (Shape shape) = drawShape $ shape ds
drawDrawing ds (Transformation f d) = drawDrawing (f ds) d
drawDrawing ds (Drawings drs) = mapM_ (drawDrawing ds) (reverse drs)

drawFrame :: Drawing -> CanvasM ()
drawFrame drawing = do
    CM.fillColor 255 255 255 1
    CM.fillRect (-250) (-250) 500 500
    drawDrawing initialDS drawing

withScreen :: Element -> ClientRect.ClientRect -> CanvasM a -> IO a
withScreen canvas rect action = do
    cw <- ClientRect.getWidth rect
    ch <- ClientRect.getHeight rect
    ctx <- js_getCodeWorldContext (canvasFromElement canvas)
    runCanvasM ctx $ CM.saveRestore $ do
        CM.translate (realToFrac cw / 2) (realToFrac ch / 2)
        CM.scale (realToFrac cw / 500) (-realToFrac ch / 500)
        CM.lineWidth 0
        CM.textCenter
        CM.textMiddle
        action

setCanvasSize :: Element -> Element -> IO ()
setCanvasSize target canvas = do
    rect <- getBoundingClientRect canvas
    cx <- ClientRect.getWidth rect
    cy <- ClientRect.getHeight rect
    setAttribute target ("width" :: JSString) (show (round cx))
    setAttribute target ("height" :: JSString) (show (round cy))

drawingOf pic = runStatic pic
--------------------------------------------------------------------------------
-- Stand-alone implementation of drawing
#else
withDS :: DrawState -> Canvas () -> Canvas ()
withDS (ta, tb, tc, td, te, tf, col) action =
    Canvas.saveRestore $ do
        Canvas.transform (ta, tb, tc, td, te, tf)
        Canvas.beginPath ()
        action

applyColor :: DrawState -> Canvas ()
applyColor ds =
    case getColorDS ds of
        Nothing -> do
            Canvas.strokeStyle "black"
            Canvas.fillStyle "black"
        Just (RGBA r g b a) -> do
            let style =
                    pack $
                    printf
                        "rgba(%.0f,%.0f,%.0f,%f)"
                        (r * 255)
                        (g * 255)
                        (b * 255)
                        a
            Canvas.strokeStyle style
            Canvas.fillStyle style

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
followPath ((sx, sy):ps) closed False = do
    Canvas.moveTo (25 * sx, 25 * sy)
    forM_ ps $ \(x, y) -> Canvas.lineTo (25 * x, 25 * y)
    when closed $ Canvas.closePath ()
followPath [p1, p2] False True = followPath [p1, p2] False False
followPath ps False True = do
    let [(x1, y1), (x2, y2), (x3, y3)] = take 3 ps
        dprev = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
        dnext = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
        p = dprev / (dprev + dnext)
        cx = x2 + p * (x1 - x3) / 2
        cy = y2 + p * (y1 - y3) / 2
    Canvas.moveTo (25 * x1, 25 * y1)
    Canvas.quadraticCurveTo (25 * cx, 25 * cy, 25 * x2, 25 * y2)
    forM_ (zip4 ps (tail ps) (tail $ tail ps) (tail $ tail $ tail ps)) $ \((x1, y1), (x2, y2), (x3, y3), (x4, y4)) ->
        let dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
            d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
            d2 = sqrt ((x4 - x3) ^ 2 + (y4 - y3) ^ 2)
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        in Canvas.bezierCurveTo
               (25 * cx1, 25 * cy1, 25 * cx2, 25 * cy2, 25 * x3, 25 * y3)
    let [(x1, y1), (x2, y2), (x3, y3)] = reverse $ take 3 $ reverse ps
        dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
        d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
        r = d1 / (dp + d1)
        cx = x2 + r * (x3 - x1) / 2
        cy = y2 + r * (y3 - y1) / 2
    Canvas.quadraticCurveTo (25 * cx, 25 * cy, 25 * x3, 25 * y3)
followPath ps@(_:(sx, sy):_) True True = do
    Canvas.moveTo (25 * sx, 25 * sy)
    let rep = cycle ps
    forM_ (zip4 ps (tail rep) (tail $ tail rep) (tail $ tail $ tail rep)) $ \((x1, y1), (x2, y2), (x3, y3), (x4, y4)) ->
        let dp = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
            d1 = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
            d2 = sqrt ((x4 - x3) ^ 2 + (y4 - y3) ^ 2)
            p = d1 / (d1 + d2)
            r = d1 / (dp + d1)
            cx1 = x2 + r * (x3 - x1) / 2
            cy1 = y2 + r * (y3 - y1) / 2
            cx2 = x3 + p * (x2 - x4) / 2
            cy2 = y3 + p * (y2 - y4) / 2
        in Canvas.bezierCurveTo
               (25 * cx1, 25 * cy1, 25 * cx2, 25 * cy2, 25 * x3, 25 * y3)
    Canvas.closePath ()

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

type Drawer = DrawState -> Canvas ()

polygonDrawer ps smooth ds = do
    withDS ds $ followPath ps True smooth
    applyColor ds
    Canvas.fill ()

pathDrawer ps w closed smooth ds = drawFigure ds w $ followPath ps closed smooth

sectorDrawer b e r ds =
    withDS ds $ do
        Canvas.arc (0, 0, 25 * abs r, b, e, b > e)
        Canvas.lineTo (0, 0)
        applyColor ds
        Canvas.fill ()

arcDrawer b e r w ds =
    drawFigure ds w $ Canvas.arc (0, 0, 25 * abs r, b, e, b > e)

textDrawer sty fnt txt ds =
    withDS ds $ do
        Canvas.scale (1, -1)
        applyColor ds
        Canvas.font (fontString sty fnt)
        Canvas.fillText (txt, 0, 0)

logoDrawer ds = return ()

coordinatePlaneDrawer ds = drawDrawing ds coordinatePlaneDrawing

drawDrawing :: DrawState -> Drawing -> Canvas ()
drawDrawing ds (Shape drawer) = drawer ds
drawDrawing ds (Transformation f d) = drawDrawing (f ds) d
drawDrawing ds (Drawings drs) = mapM_ (drawDrawing ds) (reverse drs)

setupScreenContext :: (Int, Int) -> Canvas ()
setupScreenContext (cw, ch)
    -- blank before transformation (canvas might be non-sqare)
 = do
    Canvas.fillStyle "white"
    Canvas.fillRect (0, 0, fromIntegral cw, fromIntegral ch)
    Canvas.translate (realToFrac cw / 2, realToFrac ch / 2)
    let s = min (realToFrac cw / 500) (realToFrac ch / 500)
    Canvas.scale (s, -s)
    Canvas.lineWidth 0
    Canvas.textAlign Canvas.CenterAnchor
    Canvas.textBaseline Canvas.MiddleBaseline

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

display :: Drawing -> IO ()
display drawing =
    runBlankCanvas $ \context ->
        Canvas.send context $
        Canvas.saveRestore $ do
            let rect = (Canvas.width context, Canvas.height context)
            setupScreenContext rect
            drawDrawing initialDS drawing

drawingOf pic = display (pictureToDrawing pic)
#endif
--------------------------------------------------------------------------------
-- Common event handling and core interaction code
keyCodeToText :: Word -> Text
keyCodeToText n =
    case n of
        _
            | n >= 47 && n <= 90 -> fromAscii n
        _
            | n >= 96 && n <= 105 -> fromNum (n - 96)
        _
            | n >= 112 && n <= 135 -> "F" <> fromNum (n - 111)
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
    fromAscii n = singleton (chr (fromIntegral n))
    fromNum n = pack (show (fromIntegral n))

isUniversallyConstant :: (a -> s -> s) -> s -> IO Bool
isUniversallyConstant f old =
    falseOr $ do
        oldName <- makeStableName old
        genName <- makeStableName $! f undefined old
        return (genName == oldName)
  where
    falseOr x = x `catch` \(e :: SomeException) -> return False

applyIfModifying :: (s -> IO s) -> s -> IO (Maybe s)
applyIfModifying f s0 = do
    oldName <- makeStableName $! s0
    s1 <- f s0
    newName <- makeStableName $! s1
    if newName /= oldName
        then return (Just s1)
        else return Nothing

modifyMVarIfNeeded :: MVar s -> (s -> IO s) -> IO Bool
modifyMVarIfNeeded var f =
    modifyMVar var $ \s0 -> do
        ms1 <- applyIfModifying f s0
        case ms1 of
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
--------------------------------------------------------------------------------
-- GHCJS event handling and core interaction code
#ifdef ghcjs_HOST_OS
getMousePos :: IsMouseEvent e => Element -> EventM w e Point
getMousePos canvas = do
    (ix, iy) <- mouseClientXY
    liftIO $ do
        rect <- getBoundingClientRect canvas
        cx <- ClientRect.getLeft rect
        cy <- ClientRect.getTop rect
        cw <- ClientRect.getWidth rect
        ch <- ClientRect.getHeight rect
        return
            ( 20 * fromIntegral (ix - round cx) / realToFrac cw - 10
            , 20 * fromIntegral (round cy - iy) / realToFrac cw + 10)

fromButtonNum :: Word -> Maybe MouseButton
fromButtonNum 0 = Just LeftButton
fromButtonNum 1 = Just MiddleButton
fromButtonNum 2 = Just RightButton
fromButtonNum _ = Nothing

onEvents :: Element -> (Event -> IO ()) -> IO ()
onEvents canvas handler = do
    Just window <- currentWindow
    on window keyDown $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handler (KeyPress keyName)
            preventDefault
            stopPropagation
    on window keyUp $ do
        code <- uiKeyCode
        let keyName = keyCodeToText code
        when (keyName /= "") $ do
            liftIO $ handler (KeyRelease keyName)
            preventDefault
            stopPropagation
    on window mouseDown $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                liftIO $ handler (MousePress btn pos)
    on window mouseUp $ do
        button <- mouseButton
        case fromButtonNum button of
            Nothing -> return ()
            Just btn -> do
                pos <- getMousePos canvas
                liftIO $ handler (MouseRelease btn pos)
    on window mouseMove $ do
        pos <- getMousePos canvas
        liftIO $ handler (MouseMovement pos)
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
            ms1 <- (return . eventFun) `applyIfModifying` gameState0
            case ms1 of
                Nothing -> do
                    putMVar gsm gs
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

foreign import javascript
               "/[&?]dhash=(.{22})/.exec(window.location.search)[1]" js_deployHash
               :: IO JSVal

getDeployHash :: IO Text
getDeployHash = pFromJSVal <$> js_deployHash

runGame ::
       GameToken
    -> Int
    -> (StdGen -> s)
    -> (Double -> s -> s)
    -> (Int -> Event -> s -> s)
    -> (Int -> s -> Picture)
    -> IO ()
runGame token numPlayers initial stepHandler eventHandler drawHandler = do
    let fullStepHandler dt = stepHandler dt . eventHandler (-1) (TimePassing dt)
    js_showCanvas
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
    screen <- js_getCodeWorldContext (canvasFromElement canvas)
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
                js_canvasDrawImage
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

run :: s
    -> (Double -> s -> s)
    -> (e -> s -> s)
    -> (s -> Drawing)
    -> (Double -> e)
    -> IO (e -> IO (), IO s)
run initial stepHandler eventHandler drawHandler injectTime = do
    let fullStepHandler dt = stepHandler dt . eventHandler (injectTime dt)
    js_showCanvas
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    offscreenCanvas <- Canvas.create 500 500
    setCanvasSize canvas canvas
    setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    on window resize $ do
        liftIO $ setCanvasSize canvas canvas
        liftIO $ setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    currentState <- newMVar initial
    eventHappened <- newMVar ()
    let sendEvent event = do
            changed <-
                modifyMVarIfNeeded currentState (return . eventHandler event)
            when changed $ void $ tryPutMVar eventHappened ()
        getState = readMVar currentState
    screen <- js_getCodeWorldContext (canvasFromElement canvas)
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
                js_canvasDrawImage
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
                          modifyMVar_ currentState (return . fullStepHandler dt)
                          return t1
                   | otherwise ->
                       do takeMVar eventHappened
                          getTime
            nextState <- readMVar currentState
            nextStateName <- makeStableName $! nextState
            nextNeedsTime <-
                if | nextStateName /= lastStateName -> return True
                   | not needsTime -> return False
                   | otherwise ->
                       not <$> isUniversallyConstant fullStepHandler nextState
            go t1 picFrame nextStateName nextNeedsTime
    t0 <- getTime
    nullFrame <- makeStableName undefined
    initialStateName <- makeStableName $! initial
    forkIO $ go t0 nullFrame initialStateName True
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

drawDebugState :: DebugState -> Drawing -> Drawing
drawDebugState state drawing =
    case debugStateActive state of
        True ->
            highlightSelectShape
                (shapeHighlighted state)
                (shapeSelected state)
                drawing
        False -> drawing

runStatic :: Picture -> IO ()
runStatic pic = do
    js_showCanvas
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    offscreenCanvas <- Canvas.create 500 500
    setCanvasSize canvas canvas
    setCanvasSize (elementFromCanvas offscreenCanvas) canvas
    screen <- js_getCodeWorldContext (canvasFromElement canvas)
    debugState <- newMVar debugStateInit
    let draw =
            flip drawDebugState (pictureToDrawing pic) <$> readMVar debugState
        drawToScreen = withoutPreemption $ do
            drawing <- draw
            rect <- getBoundingClientRect canvas
            withScreen (elementFromCanvas offscreenCanvas) rect $
                drawFrame drawing
            rect <- getBoundingClientRect canvas
            cw <- ClientRect.getWidth rect
            ch <- ClientRect.getHeight rect
            js_canvasDrawImage
                screen
                (elementFromCanvas offscreenCanvas)
                0
                0
                (round cw)
                (round ch)
        sendEvent evt = do
            takeMVar debugState >>= putMVar debugState . updateDebugState evt
            drawToScreen
        handlePause True = sendEvent DebugStart
        handlePause False = sendEvent DebugStop
        handleHighlight True node = sendEvent $ HighlightEvent node
        handleHighlight False node = sendEvent $ SelectEvent node
    on window resize $
        liftIO $ do
            setCanvasSize canvas canvas
            setCanvasSize (elementFromCanvas offscreenCanvas) canvas
            drawToScreen
    inspect (return pic) handlePause handleHighlight
    drawToScreen

-- Wraps the event and state from run so they can be paused by pressing the Inspect
-- button.
runInspect ::
       s -> (Double -> s -> s) -> (Event -> s -> s) -> (s -> Picture) -> IO ()
runInspect initial stepHandler eventHandler drawHandler = do
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    let initialWrapper = (debugStateInit, initial)
        stepHandlerWrapper dt (debugState, state) =
            case debugStateActive debugState of
                True -> (debugState, state)
                False -> (debugState, stepHandler dt state)
        eventHandlerWrapper evt (debugState, state) =
            case evt of
                Left debugEvent ->
                    (updateDebugState debugEvent debugState, state)
                Right normalEvent ->
                    (debugState, eventHandler normalEvent state)
        drawHandlerWrapper (debugState, state) =
            drawDebugState debugState (pictureToDrawing $ drawHandler state)
        drawPicHandler (debugState, state) = drawHandler state
    (sendEvent, getState) <-
        run
            initialWrapper
            stepHandlerWrapper
            eventHandlerWrapper
            drawHandlerWrapper
            (Right . TimePassing)
    let pauseEvent True = sendEvent $ Left DebugStart
        pauseEvent False = sendEvent $ Left DebugStop
        highlightSelectEvent True n = sendEvent $ Left (HighlightEvent n)
        highlightSelectEvent False n = sendEvent $ Left (SelectEvent n)
    onEvents canvas (sendEvent . Right)
    inspect (drawPicHandler <$> getState) pauseEvent highlightSelectEvent

runPauseable ::
       Wrapped s
    -> (Double -> s -> s)
    -> (Wrapped s -> [Control s])
    -> (s -> Picture)
    -> IO ()
runPauseable initial stepHandler controls drawHandler = do
    Just window <- currentWindow
    Just doc <- currentDocument
    Just canvas <- getElementById doc ("screen" :: JSString)
    let initialWrapper = (debugStateInit, initial)
        stepHandlerWrapper dt (debugState, wrappedState) =
            case debugStateActive debugState of
                True -> (debugState, wrappedState)
                False -> (debugState, wrappedStep stepHandler dt wrappedState)
        eventHandlerWrapper evt (debugState, wrappedState) =
            case evt of
                Left debugEvent ->
                    (updateDebugState debugEvent debugState, wrappedState)
                Right normalEvent ->
                    ( debugState
                    , wrappedEvent controls stepHandler normalEvent wrappedState)
        drawHandlerWrapper (debugState, wrappedState) =
            case debugStateActive debugState of
                True -> drawDebugState debugState plainDrawing
                False ->
                    pictureToDrawing $
                    wrappedDraw controls drawHandler wrappedState
          where
            plainDrawing = pictureToDrawing $ drawHandler (state wrappedState)
        drawPicHandler (debugState, wrappedState) =
            drawHandler $ state wrappedState
    (sendEvent, getState) <-
        run
            initialWrapper
            stepHandlerWrapper
            eventHandlerWrapper
            drawHandlerWrapper
            (Right . TimePassing)
    let pauseEvent True = sendEvent $ Left DebugStart
        pauseEvent False = sendEvent $ Left DebugStop
        highlightSelectEvent True n = sendEvent $ Left (HighlightEvent n)
        highlightSelectEvent False n = sendEvent $ Left (SelectEvent n)
    onEvents canvas (sendEvent . Right)
    inspect (drawPicHandler <$> getState) pauseEvent highlightSelectEvent

-- Given a drawing, highlight the first node and select second node. Both recolor
-- the nodes, but highlight also brings the node to the top.
highlightSelectShape :: Maybe NodeId -> Maybe NodeId -> Drawing -> Drawing
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

highlightDrawing :: DrawState -> Drawing -> Drawing
highlightDrawing (a, b, c, d, e, f, _) drawing =
    Transformation (\_ -> (a, b, c, d, e, f, Just col')) drawing
  where
    col' = RGBA 0 0 0 0.25

getDrawNode :: NodeId -> Drawing -> Maybe (Drawing, DrawState)
getDrawNode n _
    | n < 0 = Nothing
getDrawNode n drawing = either Just (const Nothing) $ go initialDS n drawing
  where
    go ds 0 d = Left (d, ds)
    go ds n (Shape _) = Right (n - 1)
    go ds n (Transformation f dr) = go (f ds) (n - 1) dr
    go ds n (Drawings []) = Right (n - 1)
    go ds n (Drawings (dr:drs)) =
        case go ds (n - 1) dr of
            Left d -> Left d
            Right n -> go ds (n + 1) $ Drawings drs

replaceDrawNode :: NodeId -> Drawing -> Drawing -> Maybe Drawing
replaceDrawNode n _ _
    | n < 0 = Nothing
replaceDrawNode n with drawing = either Just (const Nothing) $ go n drawing
  where
    go :: Int -> Drawing -> Either Drawing Int
    go 0 _ = Left with
    go n (Shape _) = Right (n - 1)
    go n (Transformation f d) = mapLeft (Transformation f) $ go (n - 1) d
    go n (Drawings []) = Right (n - 1)
    go n (Drawings (dr:drs)) =
        case go (n - 1) dr of
            Left d -> Left $ Drawings (d : drs)
            Right m ->
                mapLeft (\(Drawings qs) -> Drawings (dr : qs)) $
                go (m + 1) $ Drawings drs
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft f = either (Left . f) Right
--------------------------------------------------------------------------------
-- Stand-Alone event handling and core interaction code
#else
fromButtonNum :: Int -> Maybe MouseButton
fromButtonNum 1 = Just LeftButton
fromButtonNum 2 = Just MiddleButton
fromButtonNum 3 = Just RightButton
fromButtonNum _ = Nothing

getMousePos :: (Int, Int) -> (Double, Double) -> (Double, Double)
getMousePos (w, h) (x, y) =
    ((x - fromIntegral w / 2) / s, -(y - fromIntegral h / 2) / s)
  where
    s = min (realToFrac w / 20) (realToFrac h / 20)

toEvent :: (Int, Int) -> Canvas.Event -> Maybe Event
toEvent rect Canvas.Event {..}
    | eType == "keydown"
    , Just code <- eWhich = Just $ KeyPress (keyCodeToText (fromIntegral code))
    | eType == "keyup"
    , Just code <- eWhich =
        Just $ KeyRelease (keyCodeToText (fromIntegral code))
    | eType == "mousedown"
    , Just button <- eWhich >>= fromButtonNum
    , Just pos <- getMousePos rect <$> ePageXY = Just $ MousePress button pos
    | eType == "mouseup"
    , Just button <- eWhich >>= fromButtonNum
    , Just pos <- getMousePos rect <$> ePageXY = Just $ MouseRelease button pos
    | eType == "mousemove"
    , Just pos <- getMousePos rect <$> ePageXY = Just $ MouseMovement pos
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
        let rect = (Canvas.width context, Canvas.height context)
        offscreenCanvas <- Canvas.send context $ Canvas.newCanvas rect
        currentState <- newMVar initial
        eventHappened <- newMVar ()
        onEvents context rect $ \event -> do
            modifyMVar_ currentState (return . eventHandler event)
            tryPutMVar eventHappened ()
            return ()
        let go t0 lastFrame lastStateName needsTime = do
                pic <- drawHandler <$> readMVar currentState
                picFrame <- makeStableName $! pic
                when (picFrame /= lastFrame) $
                    Canvas.send context $ do
                        Canvas.with offscreenCanvas $
                            Canvas.saveRestore $ do
                                setupScreenContext rect
                                drawDrawing initialDS (pictureToDrawing pic)
                        Canvas.drawImageAt (offscreenCanvas, 0, 0)
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
                nextNeedsTime <-
                    if nextStateName == lastStateName
                        then not <$> isUniversallyConstant fullStepHandler nextState
                        else return True
                go t1 picFrame nextStateName nextNeedsTime
        t0 <- getCurrentTime
        nullFrame <- makeStableName undefined
        initialStateName <- makeStableName $! initial
        go t0 nullFrame initialStateName True

runInspect ::
       s -> (Double -> s -> s) -> (Event -> s -> s) -> (s -> Picture) -> IO ()
runInspect = run

runPauseable ::
       Wrapped s
    -> (Double -> s -> s)
    -> (Wrapped s -> [Control s])
    -> (s -> Picture)
    -> IO ()
runPauseable initial stepHandler controls drawHandler =
    run initial
        (wrappedStep stepHandler)
        (wrappedEvent controls stepHandler)
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
activityOf initial event draw = interactionOf initial (const id) event draw

interactionOf initial step event draw = runInspect initial step event draw

data Wrapped a = Wrapped
    { state :: a
    , playbackSpeed :: Double
    , lastInteractionTime :: Double
    , isDragging :: Bool
    } deriving (Show)

data Control :: * -> * where
    PlayButton :: Control a
    PauseButton :: Control a
    StepButton :: Control a
    RestartButton :: Control Double
    BackButton :: Control Double
    TimeLabel :: Control Double
    SpeedSlider :: Control a

wrappedStep :: (Double -> a -> a) -> Double -> Wrapped a -> Wrapped a
wrappedStep f dt w =
    w
    { state =
          if playbackSpeed w == 0
              then state w
              else f (dt * playbackSpeed w) (state w)
    , lastInteractionTime = lastInteractionTime w + dt
    }

wrappedEvent ::
       (Wrapped a -> [Control a])
    -> (Double -> a -> a)
    -> Event
    -> Wrapped a
    -> Wrapped a
wrappedEvent _ _ (TimePassing _) w = w
wrappedEvent ctrls f (event) w =
    (foldr (handleControl f event) w (ctrls w)) {lastInteractionTime = 0}

handleControl ::
       (Double -> a -> a) -> Event -> Control a -> Wrapped a -> Wrapped a
handleControl _ (PointerPress (x, y)) RestartButton w
    | -9.4 < x && x < -8.6 && -9.4 < y && y < -8.6 = w {state = 0}
handleControl _ (PointerPress (x, y)) PlayButton w
    | -8.4 < x && x < -7.6 && -9.4 < y && y < -8.6 = w {playbackSpeed = 1}
handleControl _ (PointerPress (x, y)) PauseButton w
    | -8.4 < x && x < -7.6 && -9.4 < y && y < -8.6 = w {playbackSpeed = 0}
handleControl _ (PointerPress (x,y)) BackButton w
    | -7.4 < x && x < -6.6 && -9.4 < y && y < -8.6 =
        w {state = max 0 (state w - 0.1)}
handleControl f (PointerPress (x, y)) StepButton w
    | -6.4 < x && x < -5.6 && -9.4 < y && y < -8.6 = w {state = f 0.1 (state w)}
handleControl _ (PointerPress (x, y)) SpeedSlider w
    | -5.4 < x && x < -2.6 && -9.4 < y && y < -8.6 = w {playbackSpeed = 5 * (x + 5.4) / 2.8, isDragging = True}
handleControl _ (PointerMovement (x, y)) SpeedSlider w
    | isDragging w = w {playbackSpeed = 5 * (x + 5.4) / 2.8}
handleControl _ (PointerRelease (x, y)) SpeedSlider w
    | isDragging w = w {playbackSpeed = 5 * (x + 5.4) / 2.8, isDragging = False}
handleControl _ _ _ w = w

wrappedDraw ::
       (Wrapped a -> [Control a]) -> (a -> Picture) -> Wrapped a -> Picture
wrappedDraw ctrls f w = drawControlPanel ctrls w <> f (state w)

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
drawControl _ alpha RestartButton = translated (-9) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (thickArc 0.1 (pi / 6) (11 * pi / 6) 0.2 <>
             translated 0.173 (-0.1) (solidRectangle 0.17 0.17)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha PlayButton = translated (-8) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (solidPolygon [(-0.2, 0.25), (-0.2, -0.25), (0.2, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha PauseButton = translated (-8) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.15) 0 (solidRectangle 0.2 0.6) <>
             translated 0.15 0 (solidRectangle 0.2 0.6)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha BackButton = translated (-7) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated 0.15 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(-0.05, 0.25), (-0.05, -0.25), (-0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl _ alpha StepButton = translated (-6) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated (-0.15) 0 (solidRectangle 0.2 0.5) <>
             solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 0.8 0.8)
drawControl w alpha TimeLabel = translated 8 (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (scaled 0.5 0.5 $ text (pack (showFFloatAlt (Just 4) (state w) "s"))) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 3.0 0.8) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 3.0 0.8)
drawControl w alpha SpeedSlider = translated (-4) (-9) p
  where
    p =
        colored
            (RGBA 0 0 0 alpha)
            (translated x 0.45 $ scaled 0.5 0.5 $ lettering (pack (showFFloatAlt (Just 2) (playbackSpeed w) ""))) <>
        translated x 0 (solidRectangle 0.2 0.8) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 2.8 0.6) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 2.8 0.6)
    x = playbackSpeed w / 5 * 2.8 - 1.4

animationControls :: Wrapped Double -> [Control Double]
animationControls w
    | lastInteractionTime w > 5 = []
    | playbackSpeed w == 0 && state w > 0 =
        [RestartButton, PlayButton, StepButton, BackButton, TimeLabel, SpeedSlider]
    | playbackSpeed w == 0 = [RestartButton, PlayButton, StepButton, TimeLabel, SpeedSlider]
    | otherwise = [RestartButton, PauseButton, TimeLabel, SpeedSlider]

animationOf f = runPauseable initial (+) animationControls f
  where
    initial = Wrapped {state = 0, playbackSpeed = 1, lastInteractionTime = 1000, isDragging = False}

simulationControls :: Wrapped w -> [Control w]
simulationControls w
    | lastInteractionTime w > 5 = []
    | playbackSpeed w == 0 = [PlayButton, StepButton, SpeedSlider]
    | otherwise = [PauseButton, SpeedSlider]

simulationOf simInitial simStep simDraw =
    runPauseable initial simStep simulationControls simDraw
  where
    initial =
        Wrapped {state = simInitial, playbackSpeed = 1, lastInteractionTime = 1000, isDragging = False}

trace msg x = unsafePerformIO $ do
    hPutStrLn stderr (T.unpack msg)
    return x
