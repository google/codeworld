This is a collection of more advanced examples, that demonstrate the capabilities of
CodeWorld.

Example: Letters
================

This fancy CodeWorld logo is built out of geometric patterns.  It shows off
creating patterns with list comprehensions, as well as some other fun
tricks and capabilities of picture mode.

~~~~~ . clickable
program = drawingOf(logo)

logo = pictures([
  translated(top,                      0,  6),
  translated(colored(c, dark(Green)), -6,  3),
  translated(colored(o, dark(Blue)),  -2,  3),
  translated(colored(d, dark(Yellow)), 2,  3),
  translated(colored(e, dark(Red)),    6,  3),
  translated(w,                       -8, -2),
  translated(o,                       -4, -2),
  translated(r,                        0, -2),
  translated(l,                        4, -2),
  translated(d,                        8, -2),
  translated(bottom,                   0, -6)
  ])

c = pictures([ rotated(polyline([(1.6, 0),(0,1.6)]), th) | th <- [45, 55 .. 235]])
  & arc(60, 300, 1.6)

o = pictures([ rotated(scaled(circle(1.6), 0.3, 1), th) | th <- [0, 15 .. 165] ])

d = translated(scaled(parts, 1.5, 1), -1.2, 0)
  where parts = pictures([ arc(-90, 90, rad) | rad <- [0.2, 0.4 .. 1.6] ]) &
                pictures([ rotated(polyline([ (0, 0), (1.6, 0) ]), th) | th <- [-90, -80 .. 90] ])

e = go(8)
  where go(1) = blank
        go(n) = translated(arc(180, 270, 2), 1, 2 - phi)
              & translated(rotated(scaled(go (n-1), 1/phi, 1/phi), 270), 0, 1)
              & colored(rectangle(2, 2 * phi), Gray)
        phi = (1 + sqrt(5)) / 2

w = scaled(design, 0.4, 1)
  where design = pictures([
                   translated(rotated(corner, 45), -2, -1.4),
                   translated(rotated(corner, 45),  2, -1.4)
                   ])
        corner = pictures([ polyline([ (x,0), (0,4-x) ]) | x <- [ 0, 0.4 .. 4 ] ])

r = scaled(design, 1.5, 1)
  where design = pictures([
                   translated(pictures([ leg(2,   k) | k <- [0,   5 ..  35] ]), -0.6, 0.32),
                   translated(pictures([ leg(1.2, k) | k <- [40, 45 .. 180] ]), -0.6, 0.32)
                   ])
        leg(rad, k) = rotated(polyline([ (0,0), (0,-rad) ]), k)

l = go(5)
  where go(1) = blank
        go(n) = pictures([
                  solidRectangle(0.8, 1.2),
                  translated(scaled(go(n-1), 0.95, 1/3), 0,  1.2),
                  translated(scaled(go(n-1), 0.95, 1/3), 0, -1.2)
                  ])

top = go(5)
  where go(1) = polyline([(-4, 0), (4, 0)])
        go(n) = let sub = scaled(go (n-1), 1/3, 1/3)
               in pictures([ translated(sub, -8/3, 0),
                             translated(sub,  8/3, 0),
                             translated(rotated(sub,  60), -2/3, 2/sqrt(3)),
                             translated(rotated(sub, 300),  2/3, 2/sqrt(3)) ])

bottom = scaled(top, 1, -1)
~~~~~

Example: Ferris Wheel
=====================

An example of an animation.  An animation is a function from time to a
Picture.  The parameter, normally called t, is the total time that's passed
in seconds.

~~~~~ . clickable
program = animationOf(scene)

scene(t) = ferrisWheel(t) & backdrop(t)

backdrop(t) = pictures([
    movingCloud(t),
    colored(translated(solidRectangle(20, 4), 0, -8), light(Green)),
    colored(solidRectangle(20, 20), light(Blue))
    ])

movingCloud(t) = translated(cloud, remainder(2 * t, 28) - 14, 8)

cloud = colored(cloudParts, White)
  where cloudParts = translated(solidCircle(1.6),  0,  -0.4)
                   & translated(solidCircle(1.2), -1.2, 0.4)
                   & translated(solidCircle(1.2),  1,   0.2)

ferrisWheel(t) = pictures([
    movingPart(t),
    colored(solidPolygon([ (-8, -8), (0, -4), (8, -8) ]), Gray),
    colored(solidRectangle(0.4, 10), dark(Gray))
    ])

movingPart(t) =
    rotated(wheel, 30 * t) &
    pictures([ circularPath(car, 60 * a + 30 * t) | a <- [0 .. 5] ])

wheel =
    thickCircle(6, 0.4) &
    pictures([ rotated(solidRectangle(12, 0.2), 30 * a) | a <- [0 .. 5] ])

-- Rotate, translate, then rotate.  The result ends up not rotating at all in
-- balance, but moves along a circular path anyway.
circularPath(pic, a) = rotated(translated(rotated(pic, -a), 6, 0), a)

car = translated(solidRectangle(0.2, 1.2), 0, -0.5) &
      translated(solidRectangle(1.6, 0.8), 0, -1.2)
~~~~~

Example: Mandelbrot Set
=======================

This example draws a fractal known as the Mandelbrot set.  It is an advanced
example of pictures.  The main language features used are custom operators
and data types (for the complex numbers), and a crude way of drawing
bitmapped pictures.  The drawing is slow, but it works!

The example starts by teaching CodeWorld how to do arithmetic with complex
numbers.  The new operators `<+>` and `<*>` are chosen to represent complex
addition and multiplication.

~~~~~ . clickable
-- Complex arithmetic
data Complex where
    C :: (Number, Number) -> Complex

C(x1, y1) <+> C(x2, y2) = C(x1    + x2   , y1    + y2   )
C(x1, y1) <*> C(x2, y2) = C(x1*x2 - y1*y2, x1*y2 + y1*x2)

-- A depth can be a finite number, or infinite.
data Depth where
  Finite   :: Number -> Depth
  Infinite :: Depth

{-
    A point belongs to the Mandelbrot set if iterating the polynomial
    z -> z^2 + c, starting with z = 0, diverges.  As a quick test for
    divergence, we check whether either coordinate exceeds 2.

    To get a better image, for points that do not belong to the set, we
    measure how many iterations were needed to detect divergence.  This
    number will be large for points close to the set.  Returns Infinite for
    points in the set, and Finite k for points that leave the target box in
    k iterations.
-}
depth :: (Number, Complex, Complex, Number) -> Depth
depth(_, _, _, 0) = Infinite
depth(m, z, c, k) | diverged(z) = Finite(m)
                  | otherwise   = depth(m + 1, z <*> z <+> c, c, k - 1)
  where diverged :: Complex -> Truth
        diverged(C(x, y)) = abs(x) > 2 || abs(y) > 2

{-
    Finally, render the set on a 500 by 500 pixel grid.  The source
    area is the intervals [-2, 2] on the real and imaginary axes.
    A bit of simple math maps that to the screen.
-}
mandelbrot :: (Number, Number) -> Picture
mandelbrot(n, k) = rotated(scaled(pic, 5, 5), 90)
  where pic = pictures([ spot(x, y, m)
                         | x <- [-2, -2 + width .. 2]
                         , y <- [-2, -2 + width .. 2]
                         , Finite(m) <- [depth(0, C(0, 0), C(x, y), k)] ])
        width         = 4 / n
        spot(x, y, m) = translated(shade(solidRectangle(width, width), m), x, y)
        shade(p, m)   = colored(p, HSL(0, 0, (1 - 1/m)^5))

program = drawingOf(mandelbrot(500, 25))
~~~~~

Example: Bounce
===============

In this example, a ball bounces around the screen.  The ball starts with
a random position and velocity, and bounces when it hits the edge of the
screen.

This is an example of a program with state, using the `activityOf`
function.  The position of the ball would be difficult to describe in an
animation, as a function of the current time.  So instead, the program
simulates the movement of the ball in small steps, remembering what has
happened so far.

~~~~~ . clickable
program = activityOf(initial, fine(change, 0.05), picture)

data World where
    Ball :: (Point, Vector) -> World

radius = 2
border = 10 - radius
gravity = 30

initial(x:y:vx:vy:_) = Ball((16*x  - 8, 16*y  - 8),
                            (16*vx - 8, 16*vy - 8))

fine :: ((a, Event) -> a, Number) -> ((a, Event) -> a)
fine(f, max_dt) = ff
  where ff(x, TimePassing(dt))
          | dt > max_dt = ff(f(x, TimePassing(max_dt)), TimePassing(dt - max_dt))
        ff(x, other) = f(x, other)

change(world, TimePassing(dt)) = bounce(move(world, dt))
change(world, other)           = world

move(Ball((x,y), (vx,vy)), dt) = Ball((new_x, new_y), (new_vx, new_vy))
  where new_x  = x + vx * dt
        new_y  = y + vy * dt - 0.5 * gravity * dt ^ 2
        new_vx = vx
        new_vy = vy - gravity * dt

bounce(Ball((x,y), (vx,vy))) = Ball((x,y), (nvx, nvy))
  where nvx = if (x < -border && vx < 0) || (x > border && vx > 0) then -vx else vx
        nvy = if (y < -border && vy < 0) || (y > border && vy > 0) then -vy else vy

picture(Ball((x,y),_)) = translated(solidCircle(radius), x, y)
~~~~~

Example: Maze
=============

This example generates random mazes, and allows the player to navigate
through the maze using their keyboard.  This demonstrates more of the
capabilities of the `activityOf` function to respond to different kinds
of events.

~~~~~ . clickable
program = activityOf(createWorld, event, drawWorld)

{- A World contains
    * the location of the player in the maze
    * the maze itself, which is a Maze
-}
data World where
  WorldOf :: (Point, Maze) -> World

data Maze where
  MazeOf :: (Number, Number, [Point], [Door]) -> Maze

type Door = (Point, Point)

{- Create the initial maze, using the random number supply -}
createWorld :: RandomNumbers -> World
createWorld(rs) = WorldOf((0, 0), buildMaze(20, 20, rs))

{- Event handling: Move the player in the maze. -}
event(w, KeyPress("Up"))    = move(up,    w)
event(w, KeyPress("Down"))  = move(down,  w)
event(w, KeyPress("Left"))  = move(left,  w)
event(w, KeyPress("Right")) = move(right, w)
event(w, _) = w

move :: (Vector, World) -> World
move(d, WorldOf(p, MazeOf(w, h, vis, ds)))
  | containsDoor(ds, (p, p')) = WorldOf(p', MazeOf(w, h, vis, ds))
  | otherwise                 = WorldOf(p,  MazeOf(w, h, vis, ds))
  where p' = addDirToPoint(p, d)

{- Draw the maze and the player in it. -}
drawWorld :: World -> Picture
drawWorld(WorldOf(loc, maze)) = scaled(translated(
    drawBall(loc) & drawMaze(maze), -10, -10), 0.98, 0.98)
  where
    drawBall(x, y) = translated(ball, x, y)
    ball = translated(colored(solidCircle(0.5), Blue), 0.5, 0.5)

directions = [up, down, right, left]
up = (0,1); down = (0,-1); right = (1,0); left = (-1, 0)

addDirToPoint :: (Point, Vector) -> Point
addDirToPoint(p, d) = vectorSum(p, d)

reverseDoor :: Door -> Door
reverseDoor((fx, fy), (tx, ty)) = ((tx, ty), (fx, fy))

addDoor :: (Maze, Door) -> Maze
addDoor(MazeOf(w, h, vis, ds), d) = MazeOf(w, h, vis, addIfMissing(ds, d))

containsDoor(ds, d) = contains(ds, d) || contains(ds, reverseDoor(d))

markVisitedAt :: (Maze, Point) -> Maze
markVisitedAt(MazeOf(w, h, vis, ds), p) =
    MazeOf(w, h, addIfMissing(vis, p), ds)

isVisitedAt :: (Maze, Point) -> Truth
isVisitedAt(MazeOf(_, _, vis, ds), p) = contains(vis, p)

{- Find all the neighbors of a particular point in a grid -}
neighbors :: (Maze, Point) -> [Point]
neighbors(MazeOf(w, h, _, _), p) =
    [np | d <- directions, let np = addDirToPoint(p,d), inbounds(np)]
  where
    inbounds(x, y) = x >= 0 && x < w && y >= 0 && y < h

{- Find all the unvisited neighbors of a point in a grid -}
unvisitedNeighbors :: (Maze, Point) -> [Point]
unvisitedNeighbors(g, p) =
  [ n | n <- neighbors(g, p), not(isVisitedAt(g,n)) ]

{- The function for building a random maze -}
buildMaze :: (Number, Number, RandomNumbers) -> Maze
buildMaze(w, h, randoms) = go((w-1, h-1), startMaze, randoms) where
  startMaze = MazeOf(w, h, [], [entranceDoor, exitDoor]) where
    entranceDoor = ((-1, 0), (0, 0))
    exitDoor     = ((w-1, h-1), (w, h-1))
  go :: (Point, Maze, RandomNumbers) -> Maze
  go(current, g, rs) = foldl f newMaze nbors where
    newMaze = markVisitedAt(g, current)
    nbors = shuffled(unvisitedNeighbors(newMaze, current), rs # 1)
    f gacc n = if isVisitedAt(gacc, n) then gacc else recur where
      newG  = addDoor(gacc, (current, n))
      recur = go(n, newG, rest(rs, 1))

{- Maze painting code -}
drawMaze(MazeOf(w, h, _, ds)) = doorsPic & allGridLines
  where
    doorsPic = pictures [drawDoor(d) | d <- ds]
    allGridLines = colored(horizontalLines & verticalLines, Black)
    horizontalLines = pictures([polyline([(w, y), (0, y)]) | y <- [0..h]])
    verticalLines   = pictures([polyline([(x, h), (x, 0)]) | x <- [0..w]])

drawDoor :: Door -> Picture
drawDoor(from, to) = colored(thickPolyline(g(from, to), 0.1), White)
  where
    g :: (Point, Point) -> [Point]
    g ((fx,fy), (tx,ty))
      | fy < ty = [(fx,  fy+1), (tx+1,ty)]   -- going up
      | fy > ty = [(fx,  fy),   (tx+1,ty+1)] -- going down
      | fx < tx = [(fx+1,fy),   (tx,  ty+1)] -- going right
      | fx > tx = [(fx,  fy),   (tx+1,ty+1)] -- going left

{- Helper Functions -}
type RandomNumbers = [Number]
addIfMissing :: ([a], a) -> [a]
addIfMissing(as, a) = if contains(as, a) then as else a : as

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0 where
  lgo z []     =  z
  lgo z (x:xs) = lgo (f z x) xs
~~~~~

Example: Asteroids
==================

Asteroids is an example of an elaborate game built with CodeWorld.  It
combines interaction with the user through mouse and keyboard events, and
physics simulation with velocity and position vectors that runs
continuously over time.  The detailed game state also tracks scores
between games, and uses random numbers to make each game unique.  There's
quite a lot there!

~~~~~ . clickable
program = activityOf(initial, change, picture)

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

effective(w, f) | energy(w) > 0 = f(w)
                | otherwise     = 0

lost(w) = any([collision(ast) | ast <- asts(w)])
    where ((shipx, shipy),_) = ship(w)
          collision((x,y),_) = (x-shipx)^2 + (y-shipy)^2 < 1.68^2

change(w, TimePassing(dt))     = step(w, dt)
change(w, KeyPress("Up"))      = w { thrust = 1 }
change(w, KeyRelease("Up"))    = w { thrust = 0 }
change(w, KeyPress("Left"))    = w { left   = 1 }
change(w, KeyRelease("Left"))  = w { left   = 0 }
change(w, KeyPress("Right"))   = w { right  = 1 }
change(w, KeyRelease("Right")) = w { right  = 0 }
change(w, other)               = w

step(w, dt)
  | lost(w) = initialWith(maxScore(w),
                          if score(w) < 1 then lastScore(w) else score(w),
                          savedRandoms(w))
  | otherwise = w {
        asts      = [ stepBody(ast, dt) | ast <- asts(w) ],
        ship      = stepThrust(stepBody(ship(w), dt), effective(w, thrust), direction(w), dt),
        direction = stepDir(direction(w), left(w), right(w), dt),
        energy    = fence(energy(w) + dt * (0.5 - 1.5 * thrust(w)), 0, 1),
        score     = score(w) + dt,
        maxScore  = max(maxScore(w), score(w) + dt)
        }

fence(v, lo, hi) = max(lo, min(hi, v))

stepThrust(((x,y), (vx,vy)), th, dir, dt) = ((x,y), (vx2, vy2))
    where vx2 = vx - 2 * th * sin(dir) * dt
          vy2 = vy + 2 * th * cos(dir) * dt

stepDir(dir, l, r, dt) = dir + l * 90 * dt - r * 90 * dt

stepBody(((x,y),(sx,sy)), dt) = ((wrap(x + sx * dt), wrap(y + sy * dt)), (sx, sy))
  where wrap(k) | k <= (-12) = k + 24
                | k >=   12  = k - 24
                | otherwise  = k

picture(w) = scoreBar(score(w), lastScore(w), maxScore(w))
           & energyBar(energy(w))
           & shipPic(ship(w), direction(w), effective(w, thrust))
           & astsPic(asts(w))
           & starsPic(stars(w))
           & solidRectangle(20, 20)

starsPic(ss) = pictures([
    colored(translated(solidCircle(r), x, y), Gray) | ((x,y),r) <- ss
    ])

astsPic(as) = pictures([
    colored(translated(solidCircle(1.2), x, y), light(Red)) | ((x,y),_) <- as
    ])

shipPic(((x,y),_), dir, th) = translated(rotated(ship, dir), x, y)
  where ship = fire & body & colored(circle(0.48), dark(Gray))
        fire | th > 0 = colored(solidPolygon([
                          (-0.32, -0.32),
                          (-0.4,  -0.44),
                          ( 0.4,  -0.44),
                          ( 0.32, -0.32)
                        ]), Orange)
             | otherwise = blank
        body = colored(solidPolygon([
                  (-0.36, -0.32),
                  ( 0.36, -0.32),
                  ( 0,     0.48)
               ]), light(Blue))

energyBar(e) = colored(translated(solidRectangle(16*e, 0.6), 0, -9.2), Yellow)

scoreBar(s, l, m)
    = translated(label("Score", s), -8, 9)
    & translated(label("Last",  l), -1, 9)
    & translated(label("Max",   m),  6, 9)
    & colored(translated(solidRectangle(20, 0.6), 0, 9), Blue)
  where
    label :: (Text, Number) -> Picture
    label(txt, n) = colored(scaled(lettering(txt <> ": " <> fmtScore(n)),
                                   0.7, 0.5),
                            White)

    fmtScore :: Number -> Text
    fmtScore(n) = printed(floor(10 * n))
~~~~~

License
=======

All examples in this document are covered by the following license notice.

> Copyright 2018 The CodeWorld Authors. All rights reserved.

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
