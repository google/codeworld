{----- BEGIN LICENSE TEXT -----
  Copyright 2014 Google Inc. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
----- END LICENSE TEXT -----}
{-
    Example: Asteroids

    An elaborate game, consisting of scores, player controls and keys, enemies,
    energy levels... there's quite a lot here.  This is an involved demo of
    the kind of thing that's possible -- and still not too hard! -- using
    CodeWorld.
-}

main = interactionOf initial step event draw

data World = World {
    stars     :: [(Point, Number)],
    asts      :: [(Point, Vector)],
    ship      :: (Point, Vector),
    direction :: Number,
    left      :: Number,
    right     :: Number,
    thrust    :: Number,
    energy    :: Number,
    score     :: Number,
    lastScore :: Number,
    maxScore  :: Number,
    savedGen  :: [Number]
    }

initial rs = initialWith 0 0 rs

initialWith m l rs =
  let (generatedStars, rs') = makeStars 40 rs
      (generatedAsts, rs'') = makeAsts 20 rs'
  in  World {
          stars     = generatedStars,
          asts      = generatedAsts,
          ship      = ((0,0), (0,0)),
          direction = 0,
          left      = 0,
          right     = 0,
          thrust    = 0,
          energy    = 1,
          score     = 0,
          maxScore  = m,
          lastScore = l,
          savedGen  = rs''
          }

makeStars 0 rs = ([], rs)
makeStars n (r1:r2:r3:rs) = (star : stars, rs')
  where x = 20 * r1 - 10
        y = 20 * r2 - 10
        r = 0.1 * r3 + 0.05
        star = ((x,y),r)
        (stars, rs') = makeStars (n-1) rs

makeAsts 0 rs = ([], rs)
makeAsts n (r1:r2:r3:r4:rs) = (ast : asts, rs')
  where x = 20 * r1 - 10
        y = 20 * r2 - 10
        vx = 2.5 * (r3 - 1/2)
        vy = 2.5 * (r4 - 1/2)
        ast = ((x,y), (vx,vy))
        (asts, rs') = makeAsts (n-1) rs

effective w x | energy w > 0 = x w
              | otherwise    = 0

lost w = any (collision (ship w)) (asts w)
    where collision ((x1,y1),_) ((x2,y2),_) = (x2-x1)^2 + (y2-y1)^2 < 1.68^2

step dt w = if lost w
    then initialWith (maxScore w) (if score w < 1 then lastScore w else score w) (savedGen w)
    else w {
        asts      = map (stepBody dt) (asts w),
        ship      = stepThrust dt (stepBody dt (ship w)) (effective w thrust) (direction w),
        direction = stepDir    dt (direction w) (left w) (right w),
        energy    = fence 0 1 (energy w + dt * (0.5 * (1 - thrust w) - 1.0 * thrust w)),
        score     = score w + dt,
        maxScore  = max (maxScore w) (score w)
        }

fence lo hi v = max 0 (min hi v)

stepThrust dt ((x,y), (vx,vy)) th dir = ((x,y), (vx', vy'))
    where vx' = vx - 2 * th * sin dir * dt
          vy' = vy + 2 * th * cos dir * dt

stepDir dt dir l r = dir + l * 90 * dt - r * 90 * dt

stepBody dt ((x,y),(sx,sy)) = ((wrap (x + sx * dt), wrap (y + sy * dt)), (sx, sy))
  where wrap k | k <= (-12) = k + 24
               | k >=   12  = k - 24
               | otherwise  = k

draw w = pictures [
    drawScoreBar (score w) (lastScore w) (maxScore w),
    drawEnergyBar (energy w),
    drawShip (ship w) (direction w) (effective w thrust),
    drawAsts (asts w),
    drawStars (stars w),
    solidRectangle 20 20
    ]

drawStars ss = pictures [
    color (gray 0.5) (translate x y (solidCircle r ))
        | ((x,y),r)   <- ss
    ]

drawAsts  as = pictures [
    color (light red) (translate x y (solidCircle 1.2))
        | ((x,y),_) <- as
    ]

drawShip ((x,y),_) dir th = translate x y (rotate dir (pictures [
    if th > 0 then color orange (polygon [( -0.32, -0.32), (-0.4, -0.44), (0.4, -0.44), (0.32, -0.32)])
              else blank,
    color cyan   (polygon [(-0.36, -0.32), (0.36, -0.32), (0, 0.48) ]),
    color (gray 0.2) (circle 0.48)
    ]))

drawEnergyBar e = color yellow $ translate 0 (-9.2) $ solidRectangle (16 * e) 0.6

drawScoreBar s l m = pictures [
  color white $ translate (-8) 9 $ scale 0.7 0.5 $ text $ "Score: " <> fmtScore s,
  color white $ translate (-1) 9 $ scale 0.7 0.5 $ text $ "Last: " <> fmtScore l,
  color white $ translate ( 6) 9 $ scale 0.7 0.5 $ text $ "Max: " <> fmtScore m,
  color blue $ translate 0 9.2 $ solidRectangle 20 0.6
  ]

fmtScore :: Number -> Text
fmtScore s = show (floor (10 * s))

event (KeyPress   "Up")    w = w { thrust = 1 }
event (KeyRelease "Up")    w = w { thrust = 0 }
event (KeyPress   "Left")  w = w { left   = 1 }
event (KeyRelease "Left")  w = w { left   = 0 }
event (KeyPress   "Right") w = w { right  = 1 }
event (KeyRelease "Right") w = w { right  = 0 }
event _                    w = w
