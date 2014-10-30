Asteroids
=========

About the example
-----------------

Asteroids is an example of an elaborate game built with CodeWorld.  Using the
interaction mode, Asteroids keeps track of a detailed world state, and
simulates simple physics (vector acceleration and velocity) over time, as
well as responding to the user.  It tracks scores between games, and it uses
random numbers to make each game unique.  There's quite a lot there!

The program
-----------

    main = interactionOf(initial, step, event, draw)

    data World = World {
        stars        :: [(Point, Number)],
        asts         :: [(Point, Vector)],
        ship         :: (Point, Vector),
        direction    :: Number,
        left         :: Number,
        right        :: Number,
        thrust       :: Number,
        energy       :: Number,
        score        :: Number,
        lastScore    :: Number,
        maxScore     :: Number,
        savedRandoms :: [Number]
        }

    initial(rs) = initialWith(0, 0, rs)

    initialWith(m, l, rs) =
      let (generatedStars, rs2) = makeStars(40, rs)
          (generatedAsts,  rs3) = makeAsts(20, rs2)
      in  World {
              stars        = generatedStars,
              asts         = generatedAsts,
              ship         = ((0,0), (0,0)),
              direction    = 0,
              left         = 0,
              right        = 0,
              thrust       = 0,
              energy       = 1,
              score        = 0,
              maxScore     = m,
              lastScore    = l,
              savedRandoms = rs3
              }

    makeStars(0, rs         ) = ([], rs)
    makeStars(n, r1:r2:r3:rs) = (star : stars, rs2)
      where x = 20  * r1 - 10
            y = 20  * r2 - 10
            r = 0.1 * r3 + 0.05
            star = ((x,y),r)
            (stars, rs2) = makeStars(n-1, rs)

    makeAsts(0, rs            ) = ([], rs)
    makeAsts(n, r1:r2:r3:r4:rs) = (ast : asts, rs2)
      where x = 20 * r1 - 10
            y = 20 * r2 - 10
            vx = 2.5 * (r3 - 1/2)
            vy = 2.5 * (r4 - 1/2)
            ast = ((x,y), (vx,vy))
            (asts, rs2) = makeAsts(n-1, rs)

    effective(w, f) | energy w > 0 = f(w)
                    | otherwise    = 0

    lost(w) = any(asts(w), collision)
        where ((shipx, shipy),_) = ship(w)
              collision((x,y),_) = (x-shipx)^2 + (y-shipy)^2 < 1.68^2

    step(w, dt) =
        if lost(w)
        then initialWith(maxScore(w),
                         if score(w) < 1 then lastScore(w) else score(w),
                         savedRandoms(w))
        else w {
            asts      = [ stepBody(ast, dt) | ast <- asts(w) ],
            ship      = stepThrust(stepBody(ship(w), dt), effective(w, thrust), direction(w), dt),
            direction = stepDir(direction(w), left(w), right(w), dt),
            energy    = fence(energy w + dt * (0.5 * (1 - thrust w) - 1.0 * thrust w), 0, 1),
            score     = score w + dt,
            maxScore  = max(maxScore w, score w)
            }

    fence(v, lo, hi) = max(lo, min(hi, v))

    stepThrust(((x,y), (vx,vy)), th, dir, dt) = ((x,y), (vx2, vy2))
        where vx2 = vx - 2 * th * sin dir * dt
              vy2 = vy + 2 * th * cos dir * dt

    stepDir(dir, l, r, dt) = dir + l * 90 * dt - r * 90 * dt

    stepBody(((x,y),(sx,sy)), dt) = ((wrap (x + sx * dt), wrap (y + sy * dt)), (sx, sy))
      where wrap k | k <= (-12) = k + 24
                   | k >=   12  = k - 24
                   | otherwise  = k

    draw(w) = pictures[
        drawScoreBar(score(w), lastScore(w), maxScore(w)),
        drawEnergyBar(energy(w)),
        drawShip(ship(w), direction(w), effective(w, thrust)),
        drawAsts(asts(w)),
        drawStars(stars(w)),
        solidRectangle(20, 20)
        ]

    drawStars(ss) = pictures[
        color(translate(solidCircle(r), x, y), gray(0.5)) | ((x,y),r) <- ss
        ]

    drawAsts(as) = pictures[
        color(translate(solidCircle(1.2), x, y), light(red)) | ((x,y),_) <- as
        ]

    drawShip(((x,y),_), dir, th) = translate(rotate(ship, dir), x, y)
      where ship = pictures[
                     if th > 0 then color(fire, orange) else blank,
                     color(body, cyan),
                     color(circle 0.48, gray 0.2)
                   ]
            fire = solidPolygon[
                (-0.32, -0.32),
                (-0.4,  -0.44),
                ( 0.4,  -0.44),
                ( 0.32, -0.32)
                ]
            body = solidPolygon[
                (-0.36, -0.32),
                ( 0.36, -0.32),
                ( 0,     0.48)
                ]

    drawEnergyBar(e) = color(translate(solidRectangle(16*e, 0.6), 0, -9.2), yellow)

    drawScoreBar(s, l, m) = pictures [
      color(translate(scale(text("Score: " <> fmtScore(s)), 0.7, 0.5), -8, 9), white),
      color(translate(scale(text("Last: "  <> fmtScore(l)), 0.7, 0.5), -1, 9), white),
      color(translate(scale(text("Max: "   <> fmtScore(m)), 0.7, 0.5),  6, 9), white),
      color(translate(solidRectangle(20, 0.6), 0, 9.2), blue)
      ]

    fmtScore :: Number -> Text
    fmtScore(s) = show(floor(10 * s))

    event(w, KeyPress   "Up") = w { thrust = 1 }
    event(w, KeyRelease "Up") = w { thrust = 0 }
    event(w, KeyPress   "Left") = w { left   = 1 }
    event(w, KeyRelease "Left") = w { left   = 0 }
    event(w, KeyPress   "Right") = w { right  = 1 }
    event(w, KeyRelease "Right") = w { right  = 0 }
    event(w, _) = w

License
-------

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
