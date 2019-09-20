{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

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
module CodeWorld.EntryPoints where

import CodeWorld.Color
import CodeWorld.Driver
import CodeWorld.Event
import CodeWorld.Picture
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Prim
import GHC.StaticPtr
import GHC.Types
import Numeric (showFFloatAlt)
import System.IO
import System.IO.Unsafe
import System.Random

--------------------------------------------------------------------------------
-- Common code for activity, interaction, animation and simulation interfaces

-- | Runs an interactive CodeWorld program that responds to 'Event's.
-- Activities can interact with the user, change over time, and remember
-- information about the past.
--
-- Example: a program which draws a circle and changes its radius when user
-- presses Up or Down keys on her keyboard
--
-- @
--  &#x7b;-\# LANGUAGE OverloadedStrings \#-&#x7d;
-- import CodeWorld
--
-- main = activityOf initialRadius updateRadius circle
--    where
--      initialRadius = 1
--
--      updateRadius event radius =
--        case event of
--          KeyPress "Up"   -> radius + 1
--          KeyPress "Down" -> radius - 1
--          _               -> radius
-- @
activityOf
  :: world                       -- ^ The initial state of the activity.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given an event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()
activityOf initial change picture =
    interactionOf initial (const id) change picture

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
interactionOf initial step event draw = do
    hFlush stdout
    runInspect initial step event draw draw

{-# WARNING interactionOf ["Please use activityOf instead of interactionOf.",
                           "interactionOf may be removed July 2020."] #-}

data Timeline a = Timeline {
    past :: [a],   -- reversed list of past states
    present :: !a, -- present state
    future :: [a]  -- list of future states
    }

newTimeline :: a -> Timeline a
newTimeline x = Timeline [] x []

applyToTimeline :: (a -> a) -> Timeline a -> Timeline a
applyToTimeline f timeline@Timeline{..}
    | identical present new = timeline
    | otherwise = Timeline (present : past) new []
  where new = f present

undoTimeline :: Timeline a -> Timeline a
undoTimeline timeline@Timeline{..} = case past of
    [] -> timeline
    (x:xs) -> Timeline xs x (present : future)

redoTimeline :: Timeline a -> Timeline a
redoTimeline timeline@Timeline{..} = case future of
    [] -> timeline
    (x:xs) -> Timeline (present : past) x xs

restartTimeline :: Timeline a -> Timeline a
restartTimeline timeline@Timeline{..}
  | null past = timeline
  | otherwise = Timeline [] x (xs ++ present : future)
  where x : xs = reverse past

timelineLength :: Timeline a -> Int
timelineLength Timeline{..} = length past + 1 + length future

travelToTime :: Double -> Timeline a -> Timeline a
travelToTime t timeline@Timeline{..}
    | diff >= 0 = iterate redoTimeline timeline !! diff
    | otherwise = iterate undoTimeline timeline !! (-diff)
  where desiredPast = round (t * (fromIntegral (timelineLength timeline - 1)))
        actualPast = length past
        diff = desiredPast - actualPast

timelinePos :: Timeline a -> Double
timelinePos Timeline{..}
    | null past && null future = 1
    | otherwise = fromIntegral (length past) / fromIntegral (length past + length future)

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
    StartOverButton :: Point -> Control (Timeline a)
    BackButton :: Point -> Control Double
    TimeLabel :: Point -> Control Double
    SpeedSlider :: Point -> Control a
    ZoomSlider :: Point -> Control a
    UndoButton :: Point -> Control (Timeline a)
    RedoButton :: Point -> Control (Timeline a)
    HistorySlider :: Point -> Control (Timeline a)

data StrictPoint = SP !Double !Double deriving (Eq, Show)
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

wrappedEvent
    :: (Wrapped a -> [Control a])
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

zoomIncrement :: Double
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
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (restartTimeline <$> w, True)
handleControl _ (PointerPress (x, y)) (PlayButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {playbackSpeed = 1}, True)
handleControl _ (PointerPress (x, y)) (PauseButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {playbackSpeed = 0}, True)
handleControl _ (PointerPress (x, y)) (FastForwardButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {playbackSpeed = max 2 (playbackSpeed w + 1)}, True)
handleControl _ (PointerPress (x, y)) (ZoomInButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = zoomFactor w * zoomIncrement}, True)
handleControl _ (PointerPress (x, y)) (ZoomOutButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = zoomFactor w / zoomIncrement}, True)
handleControl _ (PointerPress (x, y)) (ResetViewButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {zoomFactor = 1, panCenter = SP 0 0}, True)
handleControl _ (PointerPress (x,y)) (BackButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (max 0 . (subtract 0.1) <$> w, True)
handleControl _ (PointerPress (x,y)) (UndoButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (undoTimeline <$> w, True)
handleControl _ (PointerPress (x,y)) (RedoButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (redoTimeline <$> w, True)
handleControl f (PointerPress (x, y)) (StepButton (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 0.4 = (w {state = f 0.1 (state w)}, True)
handleControl _ (PointerPress (x, y)) (SpeedSlider (cx, cy)) w
    | abs (x - cx) < 1.5 && abs (y - cy) < 0.4 =
      (w {playbackSpeed = xToPlaybackSpeed (x - cx), isDraggingSpeed = True}, True)
handleControl _ (PointerMovement (x, _)) (SpeedSlider (cx, _)) w
    | isDraggingSpeed w = (w {playbackSpeed = xToPlaybackSpeed (x - cx)}, True)
handleControl _ (PointerRelease (x, _)) (SpeedSlider (cx, _)) w
    | isDraggingSpeed w = (w {playbackSpeed = xToPlaybackSpeed (x - cx), isDraggingSpeed = False}, True)
handleControl _ (PointerPress (x, y)) (ZoomSlider (cx, cy)) w
    | abs (x - cx) < 0.4 && abs (y - cy) < 1.5 =
      (w {zoomFactor = yToZoomFactor (y - cy), isDraggingZoom = True}, True)
handleControl _ (PointerMovement (_, y)) (ZoomSlider (_, cy)) w
    | isDraggingZoom w = (w {zoomFactor = yToZoomFactor (y - cy)}, True)
handleControl _ (PointerRelease (_, y)) (ZoomSlider (_, cy)) w
    | isDraggingZoom w = (w {zoomFactor = yToZoomFactor (y - cy), isDraggingZoom = False}, True)
handleControl _ (PointerPress (x, y)) (HistorySlider (cx, cy)) w
    | abs (x - cx) < 2.5 && abs (y - cy) < 0.4 =
      (travelToTime (1/2 + (x - cx) / 4.8) <$> w { isDraggingHistory = True }, True)
handleControl _ (PointerMovement (x, _)) (HistorySlider (cx, _)) w
    | isDraggingHistory w = (travelToTime (1/2 + (x - cx) / 4.8) <$> w, True)
handleControl _ (PointerRelease (x, _)) (HistorySlider (cx, _)) w
    | isDraggingHistory w = (travelToTime (1/2 + (x - cx) / 4.8) <$> w { isDraggingHistory = False }, True)
handleControl _ (PointerPress (x, y)) PanningLayer w =
      (w {panDraggingAnchor = SJust (SP x y)}, True)
handleControl _ (PointerMovement (x, y)) PanningLayer w
    | SJust (SP ax ay) <- panDraggingAnchor w
    , SP px py <- panCenter w
    = (w { panCenter = SP (px + (x - ax) / zoomFactor w)
                          (py + (y - ay) / zoomFactor w),
           panDraggingAnchor = SJust (SP x y)
         }, True)
handleControl _ (PointerRelease _) PanningLayer w
    | SJust _ <- panDraggingAnchor w = (w {panDraggingAnchor = SNothing}, True)
handleControl _ _ _ w = (w, False)

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
                lettering (T.pack (show i ++ "/" ++ show n))) <>
        colored (RGBA 0.0 0.0 0.0 alpha) (translated xoff 0 (solidRectangle 0.2 0.8)) <>
        colored (RGBA 0.2 0.2 0.2 alpha) (rectangle 4.8 0.25) <>
        colored (RGBA 0.8 0.8 0.8 alpha) (solidRectangle 4.8 0.25)
    xoff = timelinePos (state w) * 4.8 - 2.4
    i = 1 + length (past (state w))
    n = timelineLength (state w)

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

-- | Draws a 'Picture'. This is the simplest CodeWorld entry point.
--
-- Example: a program which draws a circle of radius 1 in the middle of canvas
--
-- @
-- main = drawingOf $ circle 1
-- @
drawingOf :: Picture  -- ^ The picture to show on the screen.
          -> IO ()
drawingOf pic = do
    hFlush stdout
    runInspect (wrappedInitial ())
               (wrappedStep step)
               (wrappedEvent drawingControls step event)
               (wrappedDraw drawingControls draw)
               (draw . state)
  where step _ _ = ()
        event _ _ = ()
        draw _ = pic

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

-- | Shows an animation, with a picture for each time given by the parameter.
--
-- Example: a program showing a square which rotates once every two seconds
--
-- @
-- main = animationOf rotatingSquare
--
-- rotatingSquare :: Double -> Picture
-- rotatingSquare seconds = rotated angle square
--   where
--     square = rectangle 2 2
--     angle = pi * seconds
-- @
animationOf :: (Double -> Picture)  -- ^ A function that produces animation
                                    --   frames, given the time in seconds.
            -> IO ()
animationOf f = do
    hFlush stdout
    runInspect (wrappedInitial 0)
               (wrappedStep (+))
               (wrappedEvent animationControls (+) (const id))
               (wrappedDraw animationControls f)
               (f . state)

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

statefulDebugControls :: Wrapped (Timeline w) -> [Control (Timeline w)]
statefulDebugControls w
    | lastInteractionTime w > 5 = []
    | otherwise = panningLayer ++ pauseDependentControls ++ commonControls ++
                  resetViewButton
  where
    hasHistory = not (null (past (state w)))
    hasFuture  = not (null (future (state w)))
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

-- | Shows a simulation, which is essentially a continuous-time dynamical
-- system described by an initial value and step function.
simulationOf
  :: world                       -- ^ The initial state of the simulation.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()
simulationOf initial step draw = do
    hFlush stdout
    runInspect (wrappedInitial initial)
               (wrappedStep step)
               (wrappedEvent simulationControls step (const id))
               (wrappedDraw simulationControls draw)
               (draw . state)

{-# WARNING simulationOf ["Please use activityOf instead of simulationOf.",
                          "simulationOf may be removed July 2020."] #-}

debugSimulationOf
  :: world                       -- ^ The initial state of the simulation.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()
debugSimulationOf simInitial simStep simDraw = do
    hFlush stdout
    runInspect (wrappedInitial initial)
               (wrappedStep step)
               (wrappedEvent statefulDebugControls step (const id))
               (wrappedDraw statefulDebugControls draw)
               (draw . state)
  where
    initial = newTimeline simInitial
    step = applyToTimeline . simStep
    draw = simDraw . present

{-# WARNING debugSimulationOf ["Please use debugActivityOf instead of debugSimulationOf.",
                               "debugSimulationOf may be removed July 2020."] #-}

debugInteractionOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Double -> world -> world)  -- ^ The time step function, which advances
                                 --   the state given the time difference.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given a user interface event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()
debugInteractionOf baseInitial baseStep baseEvent baseDraw = do
    hFlush stdout
    runInspect (wrappedInitial initial)
               (wrappedStep step)
               (wrappedEvent statefulDebugControls step event)
               (wrappedDraw statefulDebugControls draw)
               (draw . state)
  where
    initial = newTimeline baseInitial
    step = applyToTimeline . baseStep
    event = applyToTimeline . baseEvent
    draw = baseDraw . present

{-# WARNING debugInteractionOf ["Please use debugActivityOf instead of debugInteractionOf.",
                                "debugInteractionOf may be removed July 2020."] #-}

-- | A version of 'activityOf' which runs an interactive CodeWorld program
-- in debugging mode.  In this mode, the program gets controls to pause and
-- manipulate time, and even go back in time to look at past states.
debugActivityOf
  :: world                       -- ^ The initial state of the interaction.
  -> (Event -> world -> world)   -- ^ The event handling function, which updates
                                 --   the state given an event.
  -> (world -> Picture)          -- ^ The visualization function, which converts
                                 --   the state into a picture to display.
  -> IO ()
debugActivityOf initial change picture =
    debugInteractionOf initial (const id) change picture

-- | Runs an interactive multi-user CodeWorld program that is joined by several
-- participants over the internet.
--
-- Example: a skeleton of a game for two players
--
-- @
-- &#x7b;-\# LANGUAGE StaticPointers, OverloadedStrings \#-&#x7d;
-- import CodeWorld
--
-- main = groupActivityOf 2 init step view
--   where
--     init = static (\\gen -> {- initialize state of the game world, possibly using random number generator -})
--     step = static (\\playerNumber event world -> {- modify world based on event occuring for given player -})
--     view = static (\\playerNumber world -> {- generate a picture that will be shown to given player in the given state of the world-})
-- @
groupActivityOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- numbered starting at 0.
  -> StaticPtr (StdGen -> world)
          -- ^ The function to create initial state of the activity. 'System.Random.StdGen' parameter can be used to generate random numbers.
  -> StaticPtr (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> StaticPtr (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()
groupActivityOf numPlayers initial event draw = do
    hFlush stdout
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

-- | A version of 'groupActivityOf' that avoids static pointers, and does not
-- check for consistency.
unsafeGroupActivityOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- numbered starting at 0.
  -> (StdGen -> world)
          -- ^ The initial state of the activity.
  -> (Int -> Event -> world -> world)
          -- ^ The event handling function, which updates the state given a
          --   participant number and user interface event.
  -> (Int -> world -> Picture)
          -- ^ The visualization function, which converts a participant number
          --   and the state into a picture to display.
  -> IO ()
unsafeGroupActivityOf numPlayers initial event draw =
    unsafeCollaborationOf numPlayers initial (const id) event draw

-- | A version of 'collaborationOf' that avoids static pointers, and does not
-- check for consistent parameters.
unsafeCollaborationOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- numbered starting at 0.
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
unsafeCollaborationOf numPlayers initial step event draw = do
    hFlush stdout
    dhash <- getDeployHash
    let token = PartialToken dhash
    runGame token numPlayers initial step event draw

{-# WARNING unsafeCollaborationOf ["Please use unsafeGroupActivityOf instead of unsafeCollaborationOf.",
                                   "unsafeCollaborationOf may be removed July 2020."] #-}

-- | Runs an interactive multi-user CodeWorld program, involving multiple
-- participants over the internet.
collaborationOf
  :: Int  -- ^ The number of participants to expect.  The participants will be
          -- numbered starting at 0.
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
collaborationOf numPlayers initial step event draw = do
    hFlush stdout
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

{-# WARNING collaborationOf ["Please use groupActivityOf instead of collaborationOf.",
                             "collaborationOf may be removed July 2020."] #-}

-- | Prints a debug message to the CodeWorld console when a value is forced.
-- This is equivalent to the similarly named function in `Debug.Trace`, except
-- that it sets appropriate buffering to use the CodeWorld console.
trace :: Text -> a -> a
trace msg x = unsafePerformIO $ do
    oldMode <- hGetBuffering stderr
    hSetBuffering stderr (BlockBuffering Nothing)
    hPutStrLn stderr (T.unpack msg)
    hFlush stderr
    hSetBuffering stderr oldMode
    return x
