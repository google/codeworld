{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module CodeWorld.Sketches where

import CodeWorld.Picture
import GHC.Stack
import Util.EmbedAsUrl

sketchedAirplane :: HasCallStack => Picture
sketchedAirplane =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedAirplane"
    $(embedAsUrl "image/svg+xml" "data/AirplaneTransparency70.svg")
    10
    10

sketchedClock :: HasCallStack => Picture
sketchedClock =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedClock"
    $(embedAsUrl "image/svg+xml" "data/AlarmTransparency70.svg")
    10
    10

sketchedAlien :: HasCallStack => Picture
sketchedAlien =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedAlien"
    $(embedAsUrl "image/svg+xml" "data/AlienTransparency70.svg")
    10
    10

sketchedApple :: HasCallStack => Picture
sketchedApple =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedApple"
    $(embedAsUrl "image/svg+xml" "data/AppleTransparency70.svg")
    10
    10

sketchedAstronaut :: HasCallStack => Picture
sketchedAstronaut =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedAstronaut"
    $(embedAsUrl "image/svg+xml" "data/AustronautTransparency70.svg")
    10
    10

sketchedBaby :: HasCallStack => Picture
sketchedBaby =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBaby"
    $(embedAsUrl "image/svg+xml" "data/BabyTransparency70.svg")
    10
    10

sketchedBalloon :: HasCallStack => Picture
sketchedBalloon =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBalloon"
    $(embedAsUrl "image/svg+xml" "data/BalloonTransparency70.svg")
    10
    10

sketchedBall :: HasCallStack => Picture
sketchedBall =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBall"
    $(embedAsUrl "image/svg+xml" "data/BallTransparency70.svg")
    10
    10

sketchedBell :: HasCallStack => Picture
sketchedBell =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBell"
    $(embedAsUrl "image/svg+xml" "data/BellTransparency70.svg")
    10
    10

sketchedBird :: HasCallStack => Picture
sketchedBird =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBird"
    $(embedAsUrl "image/svg+xml" "data/BirdTransparency70.svg")
    10
    10

sketchedBoat :: HasCallStack => Picture
sketchedBoat =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBoat"
    $(embedAsUrl "image/svg+xml" "data/BoatTransparency70.svg")
    10
    10

sketchedBook :: HasCallStack => Picture
sketchedBook =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBook"
    $(embedAsUrl "image/svg+xml" "data/BookTransparency70.svg")
    10
    10

sketchedBoy :: HasCallStack => Picture
sketchedBoy =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBoy"
    $(embedAsUrl "image/svg+xml" "data/BoyTransparency70.svg")
    10
    10

sketchedBubble :: HasCallStack => Picture
sketchedBubble =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBubble"
    $(embedAsUrl "image/svg+xml" "data/BubbleTransparency70.svg")
    10
    10

sketchedBug :: HasCallStack => Picture
sketchedBug =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBug"
    $(embedAsUrl "image/svg+xml" "data/BugTransparency70.svg")
    10
    10

sketchedCar :: HasCallStack => Picture
sketchedCar =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedCar"
    $(embedAsUrl "image/svg+xml" "data/CarTransparency70.svg")
    10
    10

sketchedChatBubble :: HasCallStack => Picture
sketchedChatBubble =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedChatBubble"
    $(embedAsUrl "image/svg+xml" "data/ChatBubbleTransparency70.svg")
    10
    10

sketchedChest :: HasCallStack => Picture
sketchedChest =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedChest"
    $(embedAsUrl "image/svg+xml" "data/ChestTransparency70.svg")
    10
    10

sketchedCloud :: HasCallStack => Picture
sketchedCloud =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedCloud"
    $(embedAsUrl "image/svg+xml" "data/CloudTransparency70.svg")
    10
    10

sketchedCoin :: HasCallStack => Picture
sketchedCoin =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedCoin"
    $(embedAsUrl "image/svg+xml" "data/CoinTransparency70.svg")
    10
    10

sketchedDog :: HasCallStack => Picture
sketchedDog =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedDog"
    $(embedAsUrl "image/svg+xml" "data/DogTransparency70.svg")
    10
    10

sketchedDragon :: HasCallStack => Picture
sketchedDragon =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedDragon"
    $(embedAsUrl "image/svg+xml" "data/DragonTransparency70.svg")
    10
    10

sketchedExplosion :: HasCallStack => Picture
sketchedExplosion =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedExplosion"
    $(embedAsUrl "image/svg+xml" "data/ExplosionTransparency70.svg")
    10
    10

sketchedFish :: HasCallStack => Picture
sketchedFish =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedFish"
    $(embedAsUrl "image/svg+xml" "data/FishTransparency70.svg")
    10
    10

sketchedFlag :: HasCallStack => Picture
sketchedFlag =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedFlag"
    $(embedAsUrl "image/svg+xml" "data/FlagTransparency70.svg")
    10
    10

sketchedFlame :: HasCallStack => Picture
sketchedFlame =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedFlame"
    $(embedAsUrl "image/svg+xml" "data/FlameTransparency70.svg")
    10
    10

sketchedFlower :: HasCallStack => Picture
sketchedFlower =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedFlower"
    $(embedAsUrl "image/svg+xml" "data/FlowerTransparency70.svg")
    10
    10

sketchedGear :: HasCallStack => Picture
sketchedGear =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedGear"
    $(embedAsUrl "image/svg+xml" "data/GearTransparency70.svg")
    10
    10

sketchedGhost :: HasCallStack => Picture
sketchedGhost =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedGhost"
    $(embedAsUrl "image/svg+xml" "data/GhostTransparency70.svg")
    10
    10

sketchedGirl :: HasCallStack => Picture
sketchedGirl =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedGirl"
    $(embedAsUrl "image/svg+xml" "data/GirlTransparency70.svg")
    10
    10

sketchedGrass :: HasCallStack => Picture
sketchedGrass =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedGrass"
    $(embedAsUrl "image/svg+xml" "data/GrassTransparency70.svg")
    10
    10

sketchedHeart :: HasCallStack => Picture
sketchedHeart =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedHeart"
    $(embedAsUrl "image/svg+xml" "data/HeartTransparency70.svg")
    10
    10

sketchedHotAirBalloon :: HasCallStack => Picture
sketchedHotAirBalloon =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedHotAirBalloon"
    $(embedAsUrl "image/svg+xml" "data/HotAirBalloonTransparency70.svg")
    10
    10

sketchedHouse :: HasCallStack => Picture
sketchedHouse =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedHouse"
    $(embedAsUrl "image/svg+xml" "data/HouseTransparency70.svg")
    10
    10

sketchedJewel :: HasCallStack => Picture
sketchedJewel =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedJewel"
    $(embedAsUrl "image/svg+xml" "data/JewelTransparency70.svg")
    10
    10

sketchedKey :: HasCallStack => Picture
sketchedKey =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedKey"
    $(embedAsUrl "image/svg+xml" "data/KeyTransparency70.svg")
    10
    10

sketchedCat :: HasCallStack => Picture
sketchedCat =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedCat"
    $(embedAsUrl "image/svg+xml" "data/KittenTransparency70.svg")
    10
    10

sketchedLightBulb :: HasCallStack => Picture
sketchedLightBulb =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedLightBulb"
    $(embedAsUrl "image/svg+xml" "data/LightbulbTransparency70.svg")
    10
    10

sketchedLightning :: HasCallStack => Picture
sketchedLightning =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedLightning"
    $(embedAsUrl "image/svg+xml" "data/LightningTransparency70.svg")
    10
    10

sketchedMonster :: HasCallStack => Picture
sketchedMonster =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedMonster"
    $(embedAsUrl "image/svg+xml" "data/MonsterTransparency70.svg")
    10
    10

sketchedMoon :: HasCallStack => Picture
sketchedMoon =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedMoon"
    $(embedAsUrl "image/svg+xml" "data/MoonTransparency70.svg")
    10
    10

sketchedNote :: HasCallStack => Picture
sketchedNote =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedNote"
    $(embedAsUrl "image/svg+xml" "data/NoteTransparency70.svg")
    10
    10

sketchedPawnBlack :: HasCallStack => Picture
sketchedPawnBlack =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedPawnBlack"
    $(embedAsUrl "image/svg+xml" "data/PawnBlackTransparency70.svg")
    10
    10

sketchedPawnBlue :: HasCallStack => Picture
sketchedPawnBlue =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedPawnBlue"
    $(embedAsUrl "image/svg+xml" "data/PawnBlueTransparency70.svg")
    10
    10

sketchedPawnGreen :: HasCallStack => Picture
sketchedPawnGreen =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedPawnGreen"
    $(embedAsUrl "image/svg+xml" "data/PawnGreenTransparency70.svg")
    10
    10

sketchedPawnRed :: HasCallStack => Picture
sketchedPawnRed =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedPawnRed"
    $(embedAsUrl "image/svg+xml" "data/PawnRedTransparency70.svg")
    10
    10

sketchedPawnWhite :: HasCallStack => Picture
sketchedPawnWhite =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedPawnWhite"
    $(embedAsUrl "image/svg+xml" "data/PawnWhiteTransparency70.svg")
    10
    10

sketchedRaindrop :: HasCallStack => Picture
sketchedRaindrop =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedRaindrop"
    $(embedAsUrl "image/svg+xml" "data/RaindropTransparency70.svg")
    10
    10

sketchedRobot :: HasCallStack => Picture
sketchedRobot =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedRobot"
    $(embedAsUrl "image/svg+xml" "data/RobotTransparency70.svg")
    10
    10

sketchedRocket :: HasCallStack => Picture
sketchedRocket =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedRocket"
    $(embedAsUrl "image/svg+xml" "data/RocketShipTransparency70.svg")
    10
    10

sketchedScroll :: HasCallStack => Picture
sketchedScroll =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedScroll"
    $(embedAsUrl "image/svg+xml" "data/ScrollTransparency70.svg")
    10
    10

sketchedSign :: HasCallStack => Picture
sketchedSign =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedSign"
    $(embedAsUrl "image/svg+xml" "data/SignTransparency70.svg")
    10
    10

sketchedSparkle :: HasCallStack => Picture
sketchedSparkle =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedSparkle"
    $(embedAsUrl "image/svg+xml" "data/SparklesTransparency70.svg")
    10
    10

sketchedStar :: HasCallStack => Picture
sketchedStar =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedStar"
    $(embedAsUrl "image/svg+xml" "data/StarTransparency70.svg")
    10
    10

sketchedSun :: HasCallStack => Picture
sketchedSun =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedSun"
    $(embedAsUrl "image/svg+xml" "data/SunTransparency70.svg")
    10
    10

sketchedThoughtBubble :: HasCallStack => Picture
sketchedThoughtBubble =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedThoughtBubble"
    $(embedAsUrl "image/svg+xml" "data/ThoughtBubbleTransparency70.svg")
    10
    10

sketchedBlock :: HasCallStack => Picture
sketchedBlock =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedBlock"
    $(embedAsUrl "image/svg+xml" "data/ToyCubeTransparency70.svg")
    10
    10

sketchedTree :: HasCallStack => Picture
sketchedTree =
  Sketch
    (getDebugSrcLoc callStack)
    "sketchedTree"
    $(embedAsUrl "image/svg+xml" "data/TreeTransparency70.svg")
    10
    10
