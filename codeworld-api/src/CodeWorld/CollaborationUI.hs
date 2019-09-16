{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
module CodeWorld.CollaborationUI
    ( UIState
    , SetupPhase(..)
    , Step(..)
    , initial
    , step
    , event
    , picture
    , startWaiting
    , updatePlayers
    ) where

import CodeWorld.Color
import CodeWorld.Event
import CodeWorld.Picture

import Data.Char
import qualified Data.Text as T
import Data.Text (Text)

-- | The enumeration type contains all the high-level states that the game UI
-- can be in. It is used as a type-index to 'UIState' to ensure that the UI
-- state matches the abstract state.
--
-- The possible UI-triggered transitions of this state are described by
-- 'Step'.
data SetupPhase
    = SMain
    | SConnect
    | SWait

-- | Possible steps taken from a given setup phase
data family Step :: (SetupPhase -> *) -> SetupPhase -> *

data instance  Step f SMain = ContinueMain (f SMain)
                            | Create (f SConnect)
                            | Join Text (f SConnect)

data instance  Step f SConnect = ContinueConnect (f SConnect)
                               | CancelConnect (f SMain)

data instance  Step f SWait = ContinueWait (f SWait)
                            | CancelWait (f SMain)

-- | The UI state, indexed by the 'SetupPhase'
data UIState (s :: SetupPhase) where
    MainMenu :: Double -> Point -> UIState SMain
    Joining :: Double -> Point -> Text -> UIState SMain
    Connecting :: Double -> Point -> UIState SConnect
    Waiting
        :: Double
        -> Point
        -> Text
        -> Int
        -> Int {- numPlayers :: -}
            {- present -}
        -> UIState SWait

continueUIState :: UIState s -> Step UIState s
continueUIState s@MainMenu {} = ContinueMain s
continueUIState s@Joining {} = ContinueMain s
continueUIState s@Connecting {} = ContinueConnect s
continueUIState s@Waiting {} = ContinueWait s

time :: UIState s -> Double
time (MainMenu t _) = t
time (Joining t _ _) = t
time (Connecting t _) = t
time (Waiting t _ _ _ _) = t

mousePos :: UIState s -> Point
mousePos (MainMenu _ p) = p
mousePos (Joining _ p _) = p
mousePos (Connecting _ p) = p
mousePos (Waiting _ p _ _ _) = p

-- Takes an absolute time, not a delta. A bit easier.
step :: Double -> UIState s -> UIState s
step t (MainMenu _ p) = MainMenu t p
step t (Joining _ p c) = Joining t p c
step t (Connecting _ p) = Connecting t p
step t (Waiting _ p c n m) = Waiting t p c n m

setMousePos :: Point -> UIState s -> UIState s
setMousePos p (MainMenu t _) = MainMenu t p
setMousePos p (Joining t _ c) = Joining t p c
setMousePos p (Connecting t _) = Connecting t p
setMousePos p (Waiting t _ c n m) = Waiting t p c n m

initial :: UIState SMain
initial = MainMenu 0 (0, 0)

startWaiting :: Text -> UIState a -> UIState SWait
startWaiting code s = Waiting (time s) (mousePos s) code 0 0

updatePlayers :: Int -> Int -> UIState SWait -> UIState SWait
updatePlayers n m (Waiting time mousePos code _ _) =
    Waiting time mousePos code n m

-- | Handling a UI event. May change the phase.
event :: Event -> UIState s -> Step UIState s
event (PointerMovement p) s = continueUIState (setMousePos p s)
event CreateClick (MainMenu t p) = Create (Connecting t p)
event JoinClick (MainMenu t p) = ContinueMain (Joining t p "")
event (LetterPress k) (Joining t p code)
    | T.length code < 4 = ContinueMain (Joining t p (code <> k))
event BackSpace (Joining t p code)
    | T.length code > 0 = ContinueMain (Joining t p (T.init code))
event ConnectClick (Joining t p code)
    | T.length code == 4 = Join code (Connecting t p)
event CancelClick (Joining t p _) = ContinueMain (MainMenu t p)
event CancelClick (Connecting t p) = CancelConnect (MainMenu t p)
event CancelClick (Waiting t p _ _ _) = CancelWait (MainMenu t p)
event _ s = continueUIState s

pattern CreateClick :: Event
pattern CreateClick <-
        PointerPress (inButton 0 1.5 8 2 -> True)

pattern JoinClick :: Event
pattern JoinClick <-
        PointerPress (inButton 0 (-1.5) 8 2 -> True)

pattern ConnectClick :: Event
pattern ConnectClick <-
        PointerPress (inButton 0 (-3.0) 8 2 -> True)

pattern LetterPress :: Text -> Event
pattern LetterPress c <- (isLetterPress -> Just c)

pattern BackSpace :: Event
pattern BackSpace <- KeyPress "Backspace"

pattern CancelClick :: Event
pattern CancelClick <- (isCancelClick -> True)

isLetterPress :: Event -> Maybe Text
isLetterPress (KeyPress k)
    | T.length k == 1
    , isLetter (T.head k) = Just (T.toUpper k)
isLetterPress _ = Nothing

isCancelClick :: Event -> Bool
isCancelClick (KeyPress "Esc") = True
isCancelClick (PointerPress point) = inButton 0 (-3) 8 2 point
isCancelClick _ = False

picture :: UIState s -> Picture
picture (MainMenu time mousePos) =
    button "New" (dull green) 0 1.5 8 2 mousePos &
    button "Join" (dull green) 0 (-1.5) 8 2 mousePos &
    connectScreen "Main Menu" time
picture (Joining time mousePos code) =
    translated 0 2 (lettering "Enter the game key:") & letterBoxes white code &
    (if T.length code < 4
         then button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos
         else button "Join" (dull green) 0 (-3) 8 2 mousePos) &
    connectScreen "Join Game" time
picture (Connecting time mousePos) =
    button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos &
    connectScreen "Connecting..." time
picture (Waiting time mousePos code numPlayers present) =
    translated 0 2 (lettering "Share this key with other players:") &
    translated 0 4 (playerDots numPlayers present) &
    letterBoxes (HSL 0 0 0.8) code &
    button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos &
    connectScreen "Waiting" time

letterBoxes :: Color -> Text -> Picture
letterBoxes color txt =
    pictures
        [ translated x 0 (letterBox color (T.singleton c))
        | c <- pad 4 ' ' (take 4 (T.unpack txt))
        | x <- [-3, -1, 1, 3]
        ]

letterBox :: Color -> Text -> Picture
letterBox c t =
    thickRectangle 0.1 1.5 1.5 & lettering t & colored c (solidRectangle 1.5 1.5)

pad :: Int -> a -> [a] -> [a]
pad 0 _ xs = xs
pad n v (x:xs) = x : pad (n - 1) v xs
pad n v [] = v : pad (n - 1) v []

inButton :: Double -> Double -> Double -> Double -> Point -> Bool
inButton x y w h (mx, my) =
    mx >= x - w / 2 && mx <= x + w / 2 && my >= y - h / 2 && my <= y + h / 2

button ::
       Text -> Color -> Double -> Double -> Double -> Double -> Point -> Picture
button txt btnColor x y w h (mx, my) =
    translated x y $
    colored white (styledLettering Plain SansSerif txt) &
    colored color (roundRect w h)
  where
    color
        | inButton x y w h (mx, my) = btnColor
        | otherwise = dark btnColor

roundRect :: Double -> Double -> Picture
roundRect w h =
    solidRectangle w (h - 0.5) & solidRectangle (w - 0.5) h &
    pictures
        [ translated x y (solidCircle 0.25)
        | x <- [-w / 2 + 0.25, w / 2 - 0.25]
        , y <- [-h / 2 + 0.25, h / 2 - 0.25]
        ]

playerDots :: Int -> Int -> Picture
playerDots n m
    | n > 8 = lettering $ T.pack $ show m ++ " / " ++ show n
playerDots n m =
    pictures
        [ translated
            (size * fromIntegral i - size * fromIntegral n / 2)
            0
            (dot (i <= m))
        | i <- [1 .. n]
        ]
  where
    dot True = solidCircle (0.4 * size)
    dot False = circle (0.4 * size)
    size = 1

connectScreen :: Text -> Double -> Picture
connectScreen hdr t = translated 0 (-7) connectBox
        & translated 0 2.5 (colored background (solidRectangle 20 4))
        & translated 0 5 codeWorldLogo
        & colored background (solidRectangle 20 20)
  where
    connectBox = scaled 2 2 (lettering hdr)
               & rectangle 14 3
               & colored connectColor (solidRectangle 14 3)
    connectColor = let k = (1 + sin (3 * t)) / 5
                   in  HSL (k + 0.5) 0.8 0.7
    background = RGBA 0.85 0.86 0.9 1
