{-# LANGUAGE PackageImports #-}

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

module Sketches where

import qualified "codeworld-api" CodeWorld.Sketches as CW
import GHC.Stack
import Internal.Picture
import "base" Prelude (($))

-- | Rough sketch of an airplane
sketchedAirplane :: HasCallStack => Picture
sketchedAirplane = withFrozenCallStack $ CWPic CW.sketchedAirplane

-- | Rough sketch of a clock
sketchedClock :: HasCallStack => Picture
sketchedClock = withFrozenCallStack $ CWPic CW.sketchedClock

-- | Rough sketch of an alien
sketchedAlien :: HasCallStack => Picture
sketchedAlien = withFrozenCallStack $ CWPic CW.sketchedAlien

-- | Rough sketch of an apple
sketchedApple :: HasCallStack => Picture
sketchedApple = withFrozenCallStack $ CWPic CW.sketchedApple

-- | Rough sketch of an astronaut
sketchedAstronaut :: HasCallStack => Picture
sketchedAstronaut = withFrozenCallStack $ CWPic CW.sketchedAstronaut

-- | Rough sketch of a baby
sketchedBaby :: HasCallStack => Picture
sketchedBaby = withFrozenCallStack $ CWPic CW.sketchedBaby

-- | Rough sketch of a balloon
sketchedBalloon :: HasCallStack => Picture
sketchedBalloon = withFrozenCallStack $ CWPic CW.sketchedBalloon

-- | Rough sketch of a ball
sketchedBall :: HasCallStack => Picture
sketchedBall = withFrozenCallStack $ CWPic CW.sketchedBall

-- | Rough sketch of a bell
sketchedBell :: HasCallStack => Picture
sketchedBell = withFrozenCallStack $ CWPic CW.sketchedBell

-- | Rough sketch of a bird
sketchedBird :: HasCallStack => Picture
sketchedBird = withFrozenCallStack $ CWPic CW.sketchedBird

-- | Rough sketch of a boat
sketchedBoat :: HasCallStack => Picture
sketchedBoat = withFrozenCallStack $ CWPic CW.sketchedBoat

-- | Rough sketch of a book
sketchedBook :: HasCallStack => Picture
sketchedBook = withFrozenCallStack $ CWPic CW.sketchedBook

-- | Rough sketch of a boy
sketchedBoy :: HasCallStack => Picture
sketchedBoy = withFrozenCallStack $ CWPic CW.sketchedBoy

-- | Rough sketch of a bubble
sketchedBubble :: HasCallStack => Picture
sketchedBubble = withFrozenCallStack $ CWPic CW.sketchedBubble

-- | Rough sketch of a bug
sketchedBug :: HasCallStack => Picture
sketchedBug = withFrozenCallStack $ CWPic CW.sketchedBug

-- | Rough sketch of a car
sketchedCar :: HasCallStack => Picture
sketchedCar = withFrozenCallStack $ CWPic CW.sketchedCar

-- | Rough sketch of a comic-style chat bubble
sketchedChatBubble :: HasCallStack => Picture
sketchedChatBubble = withFrozenCallStack $ CWPic CW.sketchedChatBubble

-- | Rough sketch of a treasure chest
sketchedChest :: HasCallStack => Picture
sketchedChest = withFrozenCallStack $ CWPic CW.sketchedChest

-- | Rough sketch of a cloud
sketchedCloud :: HasCallStack => Picture
sketchedCloud = withFrozenCallStack $ CWPic CW.sketchedCloud

-- | Rough sketch of a coin
sketchedCoin :: HasCallStack => Picture
sketchedCoin = withFrozenCallStack $ CWPic CW.sketchedCoin

-- | Rough sketch of a dog
sketchedDog :: HasCallStack => Picture
sketchedDog = withFrozenCallStack $ CWPic CW.sketchedDog

-- | Rough sketch of a dragon
sketchedDragon :: HasCallStack => Picture
sketchedDragon = withFrozenCallStack $ CWPic CW.sketchedDragon

-- | Rough sketch of an explosion
sketchedExplosion :: HasCallStack => Picture
sketchedExplosion = withFrozenCallStack $ CWPic CW.sketchedExplosion

-- | Rough sketch of a fish
sketchedFish :: HasCallStack => Picture
sketchedFish = withFrozenCallStack $ CWPic CW.sketchedFish

-- | Rough sketch of a triangular flag
sketchedFlag :: HasCallStack => Picture
sketchedFlag = withFrozenCallStack $ CWPic CW.sketchedFlag

-- | Rough sketch of a flame
sketchedFlame :: HasCallStack => Picture
sketchedFlame = withFrozenCallStack $ CWPic CW.sketchedFlame

-- | Rough sketch of a flower
sketchedFlower :: HasCallStack => Picture
sketchedFlower = withFrozenCallStack $ CWPic CW.sketchedFlower

-- | Rough sketch of an interlocking gear
sketchedGear :: HasCallStack => Picture
sketchedGear = withFrozenCallStack $ CWPic CW.sketchedGear

-- | Rough sketch of a ghost
sketchedGhost :: HasCallStack => Picture
sketchedGhost = withFrozenCallStack $ CWPic CW.sketchedGhost

-- | Rough sketch of a girl
sketchedGirl :: HasCallStack => Picture
sketchedGirl = withFrozenCallStack $ CWPic CW.sketchedGirl

-- | Rough sketch of a tuft of grass
sketchedGrass :: HasCallStack => Picture
sketchedGrass = withFrozenCallStack $ CWPic CW.sketchedGrass

-- | Rough sketch of a heart
sketchedHeart :: HasCallStack => Picture
sketchedHeart = withFrozenCallStack $ CWPic CW.sketchedHeart

-- | Rough sketch of a hot-air balloon
sketchedHotAirBalloon :: HasCallStack => Picture
sketchedHotAirBalloon = withFrozenCallStack $ CWPic CW.sketchedHotAirBalloon

-- | Rough sketch of a house or shack
sketchedHouse :: HasCallStack => Picture
sketchedHouse = withFrozenCallStack $ CWPic CW.sketchedHouse

-- | Rough sketch of a jewel or gem
sketchedJewel :: HasCallStack => Picture
sketchedJewel = withFrozenCallStack $ CWPic CW.sketchedJewel

-- | Rough sketch of a key
sketchedKey :: HasCallStack => Picture
sketchedKey = withFrozenCallStack $ CWPic CW.sketchedKey

-- | Rough sketch of a cat
sketchedCat :: HasCallStack => Picture
sketchedCat = withFrozenCallStack $ CWPic CW.sketchedCat

-- | Rough sketch of a light bulb
sketchedLightBulb :: HasCallStack => Picture
sketchedLightBulb = withFrozenCallStack $ CWPic CW.sketchedLightBulb

-- | Rough sketch of a bolt of lightning
sketchedLightning :: HasCallStack => Picture
sketchedLightning = withFrozenCallStack $ CWPic CW.sketchedLightning

-- | Rough sketch of a monster
sketchedMonster :: HasCallStack => Picture
sketchedMonster = withFrozenCallStack $ CWPic CW.sketchedMonster

-- | Rough sketch of the moon
sketchedMoon :: HasCallStack => Picture
sketchedMoon = withFrozenCallStack $ CWPic CW.sketchedMoon

-- | Rough sketch of a musical note
sketchedNote :: HasCallStack => Picture
sketchedNote = withFrozenCallStack $ CWPic CW.sketchedNote

-- | Rough sketch of a board game piece (a pawn), colored black
sketchedPawnBlack :: HasCallStack => Picture
sketchedPawnBlack = withFrozenCallStack $ CWPic CW.sketchedPawnBlack

-- | Rough sketch of a board game piece (a pawn), colored blue
sketchedPawnBlue :: HasCallStack => Picture
sketchedPawnBlue = withFrozenCallStack $ CWPic CW.sketchedPawnBlue

-- | Rough sketch of a board game piece (a pawn), colored green
sketchedPawnGreen :: HasCallStack => Picture
sketchedPawnGreen = withFrozenCallStack $ CWPic CW.sketchedPawnGreen

-- | Rough sketch of a board game piece (a pawn), colored red
sketchedPawnRed :: HasCallStack => Picture
sketchedPawnRed = withFrozenCallStack $ CWPic CW.sketchedPawnRed

-- | Rough sketch of a board game piece (a pawn), colored white
sketchedPawnWhite :: HasCallStack => Picture
sketchedPawnWhite = withFrozenCallStack $ CWPic CW.sketchedPawnWhite

-- | Rough sketch of a raindrop
sketchedRaindrop :: HasCallStack => Picture
sketchedRaindrop = withFrozenCallStack $ CWPic CW.sketchedRaindrop

-- | Rough sketch of a robot
sketchedRobot :: HasCallStack => Picture
sketchedRobot = withFrozenCallStack $ CWPic CW.sketchedRobot

-- | Rough sketch of a rocket
sketchedRocket :: HasCallStack => Picture
sketchedRocket = withFrozenCallStack $ CWPic CW.sketchedRocket

-- | Rough sketch of a blank scroll
sketchedScroll :: HasCallStack => Picture
sketchedScroll = withFrozenCallStack $ CWPic CW.sketchedScroll

-- | Rough sketch of a blank wood sign
sketchedSign :: HasCallStack => Picture
sketchedSign = withFrozenCallStack $ CWPic CW.sketchedSign

-- | Rough sketch of a sparkle effect
sketchedSparkle :: HasCallStack => Picture
sketchedSparkle = withFrozenCallStack $ CWPic CW.sketchedSparkle

-- | Rough sketch of a five-pointed star
sketchedStar :: HasCallStack => Picture
sketchedStar = withFrozenCallStack $ CWPic CW.sketchedStar

-- | Rough sketch of the sun
sketchedSun :: HasCallStack => Picture
sketchedSun = withFrozenCallStack $ CWPic CW.sketchedSun

-- | Rough sketch of a comic-style chat bubble
sketchedThoughtBubble :: HasCallStack => Picture
sketchedThoughtBubble = withFrozenCallStack $ CWPic CW.sketchedThoughtBubble

-- | Rough sketch of a wood block
sketchedBlock :: HasCallStack => Picture
sketchedBlock = withFrozenCallStack $ CWPic CW.sketchedBlock

-- | Rough sketch of a tree
sketchedTree :: HasCallStack => Picture
sketchedTree = withFrozenCallStack $ CWPic CW.sketchedTree
