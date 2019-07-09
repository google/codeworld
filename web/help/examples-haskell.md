This is a collection of examples that demonstrate the capabilities of CodeWorld
Haskell.

Example: Fourier Series
=======================

This example activity approximates a user-drawn path with a complex Fourier
series. It is inspired by the "[But what is a Fourier series? From heat flow to
circle drawings](https://youtu.be/r6sGWTCMz2k)" video by 3Blue1Brown.

~~~~~ . clickable
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CodeWorld
import qualified Data.Text as T

resolution = 10000
initialDuration = 10
initialNumVectors = 30

tickDuration = 1 / resolution

sumVectors :: [Vector] -> Vector
sumVectors = foldr vectorSum (0, 0)

freqs :: [Double]
freqs = 0 : nonZeroFreqs 1
 where
  nonZeroFreqs n = (-n) : n : nonZeroFreqs (n+1)

rotatedVectors :: [Vector] -> Double -> [Vector]
rotatedVectors vectors t = zipWith rotatedVector angles vectors
 where
  angles = map (\n -> 2 * pi * n * t) freqs

computeVectors :: [Point] -> [Vector]
computeVectors points = map integral freqs
 where
  dt = 1 / fromIntegral (length points)
  integral n = scaledVector dt
    (sumVectors [ rotatedVector (-2 * pi * n *  t) ft
                | (ft, t) <- zip points [0,dt..]])

floorToTick :: Double -> Double
floorToTick time = fromIntegral (floor (time * resolution) :: Int) / resolution

data Camera
  = Follow Double
  | Fixed

data State
  = InitialState
  | UserDrawing [Point]
  | Replay
    { originalPath :: [Point]
    , vectors :: [Vector]
    , numVectors :: Int
    , camera :: Camera
    , duration :: Double
    , time :: Double
    , points ::  [Point]
    }

update :: Event -> State -> State
update (PointerPress point) InitialState = UserDrawing [point]
update (PointerMovement point) (UserDrawing points) = UserDrawing (point:points)
update (PointerRelease _) (UserDrawing points) = Replay
  { originalPath = points ++ [head points]
  , vectors = computeVectors (reverse points)
  , numVectors = initialNumVectors
  , camera = Fixed
  , duration = initialDuration
  , time = 0
  , points = []
  }
update (KeyPress "K") state@Replay{..} = state { numVectors = numVectors + 1 }
update (KeyPress "J") state@Replay{..} = state { numVectors = max 1 (numVectors-1) }
update (KeyPress "F") state@Replay{camera=Fixed} = state { camera = Follow 1 }
update (KeyPress "F") state@Replay{camera=Follow _} = state { camera = Fixed }
update (KeyPress "Up") state@Replay{camera=Follow zoom} = state { camera = Follow (zoom * 1.5) }
update (KeyPress "Down") state@Replay{camera=Follow zoom} = state { camera = Follow (zoom / 1.5) }
update (KeyPress "Left") state@Replay{..} = state { duration = duration * 1.5 }
update (KeyPress "Right") state@Replay{..} = state { duration = duration / 1.5 }
update (KeyPress "R") Replay{} = InitialState
update (TimePassing dt) state@Replay{..}
  | newTime == 1 = state { time = 0, points = [] }
  | otherwise = state {time = newTime, points = newPoints ++ points }
 where
  newTime = min 1 (time + dt / duration)
  mostRecentTick = floorToTick newTime
  ticks = takeWhile (>time) [mostRecentTick, mostRecentTick - tickDuration..]
  point t = sumVectors (take numVectors (rotatedVectors vectors t))
  newPoints = map point ticks
update _ state = state

circlesAndArrows :: [Vector] -> Picture
circlesAndArrows vectors = greyed 0.1 circles <> greyed 0.3 arrows
 where
  centers = scanl vectorSum (0,0) vectors
  mkCircle (x,y) vec = translated x y (circle (vectorLength vec))
  circles = mconcat (zipWith mkCircle centers vectors)
  arrows = polyline centers
  greyed x = colored (lighter (1-x) black)

instructions :: Int -> Picture
instructions numVectors =
  lettering ("j/k: decrease / increase number of vectors (" <> T.pack (show numVectors) <> ")")
  <> translated 0 (-1) (lettering "f: follow / unfollow the cursor")
  <> translated 0 (-2) (lettering "up/down: zoom in / zoom out")
  <> translated 0 (-3) (lettering "left/right: slow down / speed up")
  <> translated 0 (-4) (lettering "r: restart")

render :: State -> Picture
render InitialState = lettering "draw a closed shape without lifting the pencil"
render (UserDrawing points) = polyline points
render Replay{..} =
  transform
    (colored blue (thickPolyline thickness points)
     <> colored (lighter 0.3 brown) (polyline originalPath)
     <> circlesAndArrows (take numVectors (rotatedVectors vectors (floorToTick time))))
  <> translated (-7) 9.5 (scaled 0.3 0.3 (instructions numVectors))
 where
  transform
    | Fixed <- camera = id
    | [] <- points = id
    | Follow zoom <- camera, (x,y):_ <- points = scaled zoom zoom . translated (-x) (-y)
  thickness
    | Fixed <- camera = 0.04
    | Follow zoom <- camera = 0.04 / zoom

main :: IO ()
main = activityOf InitialState update render
~~~~~

Example: Raycaster
==================

This example activity renders a 3d maze using [ray
casting](https://en.wikipedia.org/wiki/Ray_casting).

~~~~~ . clickable
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T

{- VECTORS -}

i2d :: Int -> Double
i2d = fromIntegral

normalized :: Vector -> Vector
normalized v = if len < 1e-6 then (0,0) else scaledVector (1 / len) v
 where
  len = vectorLength v

angleBetween :: Vector -> Vector -> Double
angleBetween u@(ux, uy) v@(vx, vy) = atan2 det dot
 where
  det = ux * vy - vx * uy
  dot = dotProduct u v

{- MAP -}

type WallType = Int
type Map = A.Array (Int, Int) WallType

parseMap :: [String] -> Map
parseMap rows =
  A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
 where
  w = length (head rows)
  h = length rows
  parseRow j row = zipWith (parseCell j) [0..] row
  parseCell j i cell = ((i,j), read [cell])

testMap ::[String]
testMap =
  [ "111111111111111111111111"
  , "100000100000000010000001"
  , "100000100022000010000001"
  , "100300100022000010000001"
  , "100000100000000010000001"
  , "100000100000000010000001"
  , "100111111000011110000001"
  , "100000000000000000000001"
  , "100000000000000000000001"
  , "100030000003000000030001"
  , "100000000000000000000001"
  , "100000000000000000000001"
  , "111111111111111111111111"
  ]

{- GAME STATE -}

data State = State 
  { worldMap :: !Map 
  , playerPos :: !Vector
  , playerDir :: !Vector
  , keysPressed :: !(S.Set T.Text)
  }
  deriving (Show)

{- EVENT HANDLING -}

handle :: Event -> State -> State
handle e w@(State {..}) = handle' e
 where
  handle' (TimePassing dt) =
    w { playerPos = playerPos `vectorSum` scaledVector (2*dt) speed }
   where
    speed = normalized $
      (keyToDir "W" playerDir)
      `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
      `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
      `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
    keyToDir k dir =
      if S.member k keysPressed then dir else (0,0)
  handle' (PointerMovement (x, _)) =
      w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
  handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
  handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
  handle' _ = w

{- RAY CASTING -}

data HitSide = Inside | N | S | E | W
  deriving Show

-- from http://www.cse.yorku.ca/~amana/research/grid.pdf
cellsVisitedByRay 
  :: Vector -- starting point 
  -> Vector -- direction
  -> [(HitSide, (Int, Int), Double)]
cellsVisitedByRay (posX, posY) (dirX, dirY) =
  (Inside, (initI, initJ), 0) : go initI initJ initTMaxX initTMaxY 
 where
  initI = floor posX
  initJ = floor posY
  stepI = if dirX > 0 then 1 else -1
  stepJ = if dirY > 0 then 1 else -1
  tDeltaX = abs (1 / dirX)
  tDeltaY = abs (1 / dirY)
  xSide = if dirX > 0 then W else E
  ySide = if dirY > 0 then S else N
  initTMaxX =
    if dirX > 0
      then (1 + i2d initI - posX) * tDeltaX
      else (posX - i2d initI) * tDeltaX
  initTMaxY =
    if dirY > 0
      then (1 + i2d initJ - posY) * tDeltaY
      else (posY - i2d initJ) * tDeltaY
  go i j tMaxX tMaxY
    | tMaxX < tMaxY =
        let i' = i + stepI
            tMaxX' = tMaxX + tDeltaX
        in (xSide, (i', j), tMaxX) : go i' j tMaxX' tMaxY
    | otherwise =
        let j' = j + stepJ
            tMaxY' = tMaxY + tDeltaY
        in (ySide, (i, j'), tMaxY) : go i j' tMaxX tMaxY'

collision
  :: Map
  -> Vector -- starting point
  -> Vector -- camera direction
  -> Vector -- ray direction
  -> (HitSide, WallType, Double {- distance -})
collision m pos cameraDir rayDir =
  head 
  $ filter isWall
  $ map convert
  $ cellsVisitedByRay pos rayDir
 where
  convert (side, coord, d) =
    (side, m A.! coord, d * cos (angleBetween cameraDir rayDir))
  isWall (_, wallType, _) = wallType > 0

{- RENDERING -}

screenWidth, screenHeight :: Int
screenWidth = 80
screenHeight = 60

fov :: Double
fov = pi / 4

halfScreenWidth, halfScreenHeight :: Int
halfScreenWidth = screenWidth `div` 2
halfScreenHeight = screenHeight `div` 2

render :: State -> Picture
render state = hud state & world state

world :: State -> Picture
world State{..} =
  scaled ratio ratio (walls worldMap playerPos playerDir)
 where
  ratio = 20 / i2d screenWidth

walls :: Map -> Point -> Vector -> Picture
walls m pos dir =
  pictures (map wallSlice [-halfScreenWidth .. halfScreenWidth])
 where
  wallSlice i =
    let (hitSide, wallType, distance) = collision m pos dir (rayDir i)
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = wallColor hitSide wallType
    in colored color $ thickPolyline 1 [(x, -y), (x, y)]
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

wallColor :: HitSide -> WallType -> Color
wallColor hitSide wallType = colorModifier hitSide (minimapColor wallType)
 where
  colorModifier E = dark
  colorModifier W = dark
  colorModifier _ = id

hud :: State -> Picture
hud state = translated (-9) 7 $ scaled 0.2 0.2 $ minimap state

minimap :: State -> Picture
minimap State{..} = 
  player & pictures [cell i j | i <- [0..w], j <- [0..h]]
 where
  (_, (w, h)) = A.bounds worldMap
  cell i j = translated (i2d i) (i2d j) 
             $ colored (minimapColor (worldMap A.! (i,j)))
             $ solidRectangle 1 1
  player = uncurry translated playerPos 
           $ colored red 
           $ (solidCircle 0.5 & polyline [(0,0), playerDir])

minimapColor :: WallType -> Color
minimapColor 0 = white
minimapColor 1 = grey
minimapColor 2 = blue
minimapColor 3 = green
minimapColor _ = black

{- MAIN -}

main :: IO ()
main = do
  activityOf
    (State
      { worldMap = parseMap testMap
      , playerPos = (1.5,1.5)
      , playerDir = (1,1)
      , keysPressed = S.empty
      })
    handle
    render
~~~~~

License
=======

All examples in this document are covered by the following license notice.

> Copyright 2019 The CodeWorld Authors. All rights reserved.

> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
> http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
