Maze
====

About the example
-----------------

This example generates random mazes, and allows the player to navigate
through the maze.  It is an example of a different style of game, using
interaction mode.  Unlike the Asteroids example, this game doesn't use
the step function at all!  Instead, all changes in the state are made
in response to actions from the user.

The program
-----------

    main = interactionOf(createWorld, firstOfPair, event, drawWorld)

    {- A World contains
        * the location of the player in the maze
        * the maze itself, which is a Maze
    -}
    data World = World { loc :: Point, maze :: Maze }

    {- Create the initial maze, using the random number supply -}
    createWorld :: RandomNumbers -> World
    createWorld rs = World { loc = (0, 0), maze = buildMaze(20, 20, rs) }

    {- Event handling: Move the player in the maze. -}
    event(w, KeyPress "Up")    = move(up,    w)
    event(w, KeyPress "Down")  = move(down,  w)
    event(w, KeyPress "Left")  = move(left,  w)
    event(w, KeyPress "Right") = move(right, w)
    event(w, _) = w

    move :: (Direction, World) -> World
    move (d, w@(World p@(x, y) (Maze _ _ _ ds))) =
      w { loc = if (containsDoor (ds, (p, p')))  then p' else p }
      where p' = addDirToPoint(p,d)

    {- Draw the maze and the player in it. -}
    drawWorld :: World -> Picture
    drawWorld w = scaled (translated (pictures
      [drawBall (loc w), drawMaze (maze w)], -10, -10), 0.98, 0.98) where
      drawBall (x,y) = translated (ball, x, y)
      ball = translated (colored (solidCircle 0.5, blue), 0.5, 0.5)

    {- Maze generation code -}
    type Direction = Vector
    directions = [up, down, right, left]
    up = (0,1); down = (0,-1); right = (1,0); left = (-1, 0)
    addDirToPoint :: (Point, Direction) -> Point
    addDirToPoint (p,d) = vectorSum(p,d)

    type Door = (Point, Point)
    reverseDoor :: Door -> Door
    reverseDoor ((fx,fy),(tx,ty)) = ((tx,ty),(fx,fy))

    data Maze = Maze {
      width   :: Number,    height :: Number,
      visited :: Set Point, doors  :: Set Door }

    addDoor :: (Maze, Door) -> Maze
    addDoor (g,d) = g { doors = addToSet(doors g, d) }

    containsDoor (ds, d) = contains(ds, d) || contains(ds, reverseDoor(d))

    markVisitedAt :: (Maze, Point) -> Maze
    markVisitedAt (g,p) = g { visited = addToSet(visited g, p) }

    isVisitedAt :: (Maze, Point) -> Bool
    isVisitedAt (g,p) = contains(visited(g), p)

    {- Find all the neighbors of a particular point in a grid -}
    neighbors :: (Maze, Point) -> Set Point
    neighbors (g,p) =
      [np | d <- directions, let np = addDirToPoint(p,d), inbounds(np)]
      where inbounds (x,y) = x >= 0 && x < width g && y >= 0 && y < height g

    {- Find all the unvisited neighbors of a point in a grid -}
    unvisitedNeighbors :: (Maze, Point) -> Set Point
    unvisitedNeighbors (g,p) =
      [ n | n <- neighbors(g, p), not(isVisitedAt(g,n)) ]

    {- The main function for building a random maze -}
    buildMaze :: (Number, Number, RandomNumbers) -> Maze
    buildMaze (w,h,randoms) = go((w-1,h-1), startMaze, randoms) where
      startMaze = (Maze w h [] (entranceDoor : exitDoor : [])) where
        entranceDoor = ((-1,0), (0,0))
        exitDoor     = ((w-1,h-1), (w,h-1))
      go :: (Point, Maze, RandomNumbers) -> Maze
      go (current,g,rs) = foldl f newMaze nbors where
        newMaze = markVisitedAt(g, current)
        nbors = shuffled(unvisitedNeighbors(newMaze, current), at(rs, 0))
        f gacc n = if isVisitedAt(gacc, n) then gacc else recur where
          newG  = addDoor (gacc, (current, n))
          recur = go(n, newG, rest(rs, 1))

    {- Maze painting code -}
    drawMaze (Maze w h _ ds) = pictures [doorsPic, allGridLines] where
      doorsPic = pictures [drawDoor d | d <- ds]
      allGridLines = colored (pictures [horizontalLines, verticalLines], black)
      horizontalLines = pictures [line [(w, y), (0, y)] | y <- [0..h]]
      verticalLines   = pictures [line [(x, h), (x, 0)] | x <- [0..w]]

    drawDoor :: Door -> Picture
    drawDoor (from, to) = colored (thickLine (g(from, to), 0.1), white) where
     g :: (Point, Point) -> [Point]
     g ((fx,fy), (tx,ty))
       | fy < ty = [(fx,  fy+1), (tx+1,ty)]   -- going up
       | fy > ty = [(fx,  fy),   (tx+1,ty+1)] -- going down
       | fx < tx = [(fx+1,fy),   (tx,  ty+1)] -- going right
       | fx > tx = [(fx,  fy),   (tx+1,ty+1)] -- going left

    {- Helper Functions -}
    type RandomNumbers = [Number]
    type Set a = [a]
    addToSet :: (Set a, a) -> Set a
    addToSet (as, a) = if contains(as, a) then as else a : as

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl f z0 xs0 = lgo z0 xs0 where
      lgo z []     =  z
      lgo z (x:xs) = lgo (f z x) xs

License
-------

Copyright 2016 The CodeWorld Authors. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
