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
  where x = 500 * r1 - 250
        y = 500 * r2 - 250
        r = 2 * r3 + 1
        star = ((x,y),r)
        (stars, rs') = makeStars (n-1) rs

makeAsts 0 rs = ([], rs)
makeAsts n (r1:r2:r3:r4:rs) = (ast : asts, rs')
  where x = 500 * r1 - 250
        y = 500 * r2 - 250
        vx = 60 * r3 - 30
        vy = 60 * r4 - 30
        ast = ((x,y), (vx,vy))
        (asts, rs') = makeAsts (n-1) rs

effective w x | energy w > 0 = x w
              | otherwise    = 0

lost w = any (collision (ship w)) (asts w)
    where collision ((x1,y1),_) ((x2,y2),_) = (x2-x1)^2 + (y2-y1)^2 < 1764

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

stepThrust dt ((x,y), (sx,sy)) th dir = ((x,y), (sx', sy'))
    where sx' = sx + th * (-30) * sin dir * dt
          sy' = sy + th *   30  * cos dir * dt

stepDir dt dir l r = dir + l * 90 * dt - r * 90 * dt

stepBody dt ((x,y),(sx,sy)) = ((wrap (x + sx * dt), wrap (y + sy * dt)), (sx, sy))
  where wrap k | k <= (-300) = k + 600
               | k >=   300  = k - 600
               | otherwise   = k

draw w = pictures [
    drawScoreBar (score w) (lastScore w) (maxScore w),
    drawEnergyBar (energy w),
    drawShip (ship w) (direction w) (effective w thrust),
    drawAsts (asts w),
    drawStars (stars w),
    solidRectangle 500 500
    ]

drawStars ss = pictures [
    color (gray 0.5) (translate x y (solidCircle r ))
        | ((x,y),r)   <- ss
    ]

drawAsts  as = pictures [
    color (light red) (translate x y (solidCircle 30))
        | ((x,y),_) <- as
    ]

drawShip ((x,y),_) dir th = translate x y (rotate dir (pictures [
    if th > 0 then color orange (polygon [( -8, -8), (-10, -11), (10, -11), (8, -8)])
              else blank,
    color cyan   (polygon [(-9, -8), (9, -8), ( 0, 12) ]),
    color (gray 0.2) (circle 12)
    ]))

drawEnergyBar e = color yellow $ translate 0 (-230) $ solidRectangle (400 * e) 15

drawScoreBar s l m = pictures [
  color white $ translate (-200) 225 $ scale 1.0 0.8 $ text $ "Score: " <> fmtScore s,
  color white $ translate ( -25) 225 $ scale 1.0 0.8 $ text $ "Last: " <> fmtScore l,
  color white $ translate ( 150) 225 $ scale 1.0 0.8 $ text $ "Max: " <> fmtScore m,
  color blue $ translate 0 230 $ solidRectangle 500 15
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
