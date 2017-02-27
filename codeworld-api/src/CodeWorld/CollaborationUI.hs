{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PatternGuards   #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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
  ( State
  , Action(..)
  , initial
  , step
  , event
  , picture
  , waiting
  ) where

import CodeWorld.Color
import CodeWorld.Picture
import CodeWorld.Event

import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid

data State
    = MainMenu {
        time :: Double,
        mousePos :: Point
      }
    | Joining {
        time :: Double,
        mousePos :: Point,
        code :: Text
      }
    | Connecting {
        time :: Double,
        mousePos :: Point
      }
    | Waiting {
        time :: Double,
        mousePos :: Point,
        code :: Text,
        numPlayers :: Int,
        present :: Int
      }
{-
    | Disconnected {
        time :: Double,
        mousePos :: Point
      }
-}

data Action = Create | Join Text

initial :: State
initial = MainMenu { time = 0, mousePos = (0, 0) }

-- Takes an absolute time, not a delta. A bit easier.
step :: Double -> State -> State
step t s = s { time = t }

waiting :: Int -> Int -> Text ->  State -> State
waiting n m code s
  = Waiting { time = time s
            , mousePos = mousePos s
            , code = code
            , numPlayers = n
            , present = m
            }


event :: Event -> State -> (State, Maybe Action)
event (MouseMovement p) s = (s { mousePos = p }, Nothing)
event (MousePress LeftButton p) MainMenu{..}
  | inButton 0 ( 1.5) 8 2 p = (Connecting { .. }, Just Create)
  | inButton 0 (-1.5) 8 2 p = (Joining { code = "", .. }, Nothing)
event (MousePress LeftButton p) Joining{..}
  | inButton 0 (-3) 8 2 p
  , T.length code == 4    = (Connecting {..}, Just (Join code))
  | inButton 0 (-3) 8 2 p = (MainMenu {..}, Nothing)
{- TODO: Cancel making a connection
event (MousePress LeftButton p) Connecting{..}
  | inButton 0 (-3) 8 2 p = (MainMenu {..}, Nothing)
-}
event (KeyPress k) s@Joining{..}
  | T.length k == 1, T.length code < 4, isLetter (T.head k)
  = (s { code = code <> T.toUpper k }, Nothing)
  | k == "Backspace" && T.length code > 0
  = (s { code = T.init code }, Nothing)
event (KeyPress "Esc") Joining{..}
  = (MainMenu { .. }, Nothing)
{- TODO: Cancel making a connection
event (KeyPress "Esc") Connecting{..}
  = (MainMenu { .. }, Nothing)
  -}
event _ s = (s, Nothing)

picture :: State -> Picture
picture MainMenu{..} = button "New" (dull green) 0 ( 1.5) 8 2 mousePos
                     & button "Join" (dull green) 0 (-1.5) 8 2 mousePos
                     & connectScreen "Main Menu" time

picture Joining{..} = translated 0 2 (text "Enter the game key:")
                 & letterBoxes white code
                 & (if T.length code < 4
                    then button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos
                    else button "Join" (dull green) 0 (-3) 8 2 mousePos)
                 & connectScreen "Join Game" time

picture Connecting{..} =
                    --  button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos
                    connectScreen "Connecting..." time

picture Waiting{..} = translated 0 2 (text "Share this key with other players:")
                    & translated 0 4 (playerDots numPlayers present)
                    & letterBoxes (gray 0.8) code
                    & button "Cancel" (dull yellow) 0 (-3) 8 2 mousePos
                    & connectScreen "Waiting" time

letterBoxes :: Color -> Text -> Picture
letterBoxes color txt = pictures
    [ translated x 0 (letterBox color (T.singleton c))
    | c <- pad 4 ' ' (take 4 (T.unpack txt))
    | x <- [-3, -1, 1, 3] ]

letterBox :: Color -> Text -> Picture
letterBox c t = thickRectangle 0.1 1.5 1.5
              & text t
              & colored c (solidRectangle 1.5 1.5)

pad :: Int -> a -> [a] -> [a]
pad 0 _ xs     = xs
pad n v (x:xs) = x : pad (n - 1) v xs
pad n v []     = v : pad (n - 1) v []

inButton :: Double -> Double -> Double -> Double -> Point -> Bool
inButton x y w h (mx, my) =
    mx >= x - w / 2 &&
    mx <= x + w / 2 &&
    my >= y - h / 2 &&
    my <= y + h / 2

button :: Text -> Color -> Double -> Double -> Double -> Double -> Point -> Picture
button txt btnColor x y w h (mx, my) = translated x y $
    colored white (styledText Plain SansSerif txt) &
    colored color (roundRect w h)
  where color | inButton x y w h (mx, my) = btnColor
              | otherwise                 = dark btnColor

roundRect :: Double -> Double -> Picture
roundRect w h = solidRectangle w (h - 0.5)
              & solidRectangle (w - 0.5) h
              & pictures [ translated x y (solidCircle 0.25)
                         | x <- [ -w / 2 + 0.25, w / 2 - 0.25 ]
                         , y <- [ -h / 2 + 0.25, h / 2 - 0.25 ] ]

playerDots n m | n > 8 = text $ T.pack $ show m ++" / "++ show n
playerDots n m = pictures
    [ translated (size*fromIntegral i - size*fromIntegral n/2) 0 (dot (i <= m))
    | i <- [1..n]
    ]
  where dot True  = solidCircle (0.4*size)
        dot False = circle (0.4*size)
        size = 1



connectScreen :: Text -> Double -> Picture
connectScreen hdr t = translated 0 7 connectBox
        & translated 0 (-7) codeWorldLogo
        & colored (RGBA 0.85 0.86 0.9 1) (solidRectangle 20 20)
  where
    connectBox = scaled 2 2 (text hdr)
               & rectangle 14 3
               & colored connectColor (solidRectangle 14 3)
    connectColor = let k = (1 + sin (3 * t)) / 5
                   in  fromHSL (k + 0.5) 0.8 0.7




