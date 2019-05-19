{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
-- | Additional entry points and graphical primitives for CodeWorld.
--
-- To use a function defined in this module, you must begin your code with this
-- line:
--
-- > import Extras.Cw(function)
--
-- where instead of @function@ you write the actual name of the function you
-- want to use.
--
-- You can specifiy more than one function. For example, if you want to use 3
-- functions, then instead of writing 3 separate @import@ lines, you can write
-- this:
--
-- > import Extras.Cw(function1,function2,function3)
--
-- All the functions defined in this module could be implemented directly
-- in CodeWorld, as no internal features of the CodeWorld system are
-- used in their implementation. These functions are provided to save
-- you the tedium of having to add them to your code. To keep this module
-- as small as possible, functions are added to this module only when
-- it has been observed that several people keep adding variations of them
-- to their respective codes.
-- 

module Extras.Cw(
    -- * Animation convenience functions
    between, beyond, saw
    -- * Functions for accessing the points in a curve
    , openCurvePoints, closedCurvePoints
    -- * Color convenience functions
    , rgb, withAlpha
    -- * Layout
    , pageFromTexts, grid, sprite, overlays, underlays
    -- * Graphing
    , graphed
    -- * New entry points
    , slideshow, autoSlideshow
    -- * Entry points with randomization
    -- $randomIntro
    , randomDrawingOf, randomAnimationOf, randomSlideshow, randomAutoSlideshow
    ) where


import Prelude
import Extras.Util

-------------------------------------------------------------------------------
--- Top Level
-------------------------------------------------------------------------------

-- | A slide show of the given list of pictures with keyboard and mouse
-- navigation. The following keys can be used to control the slide show:
--
--     * Restart the slide show: R
--     * Go to the next slide: N, space bar, Enter, Page Down
--     * Go to the previous slide: P, Backspace, Page Up
--
-- The given list of pictures must be finite. When the last slide is
-- reached, the slide show will wrap, so that the first slide is shown
-- next. This function cannot be used for showing undelimited step-motion
-- animations.
slideshow :: [Picture] -> Program
slideshow(slides) = randomSlideshow(makeslides)
    where
    makeslides _ = slides

-- | A slide show that automatically cycles through the list of given
-- pictures. Each picture will be shown for the given number of seconds
-- before advancing to the next picture.
autoSlideshow :: ([Picture], Number) -> Program
autoSlideshow(slides,period) = animationOf(sshow)
  where
  len = length(slides)
  sshow(t)
    | len < 1 = pictures([])
    | otherwise = slides#num
    where
    num = 1 + remainder(truncation(t/period), len)

-- $randomIntro
-- All the randomized versions of entry points provided in this module use
-- the same mechanism to handle random numbers. Before your code runs,
-- the CodeWorld system generates a very large list of random numbers,
-- which is passed to your code. For efficiency, the list is generated
-- on demand, so that only those random numbers actually used in your code
-- are generated. All the random numbers that the system generates are
-- between 0 (included) and 1 (excluded).
-- 
-- Using the randomized versions is very simple.
-- When you click on the output window, the drawing or animation that is
-- currently being shown is replaced with a new one that uses different
-- random numbers. You can keep clicking on your output to see different
-- randomized outputs of your program.
--
-- When you write a program that uses the randomized versions,
-- instead of providing a definition of a drawing, you
-- must provide a definition of a function that takes in the list of
-- random numbers as
-- input and generates your drawing as output. Inside your function, you
-- can use as many random numbers as you want.
--


-- | A randomized version of 'drawingOf'
--
-- Example:
--
-- > program = randomDrawingOf(draw)
-- > draw(random) = solidRectangle(width,height)
-- >     where
-- >     width  = 1 + 9 * random#1
-- >     height = 1 + 9 * random#2
--
-- The example above will show a different rectangle every time you click
-- on the output. The width and the height will vary randomly between 1
-- and 10 units.
--
randomDrawingOf :: ([Number] -> Picture) -> Program
randomDrawingOf(makeDrawing) = interactionOf(initial,update,handle,draw)
    where
    initial rs = rs
    update(model,_) = model
    handle(r:rs,PointerPress(_)) = rs
    handle(model,_) = model
    draw(r:_) = makeDrawing(randomNumbers(r))

-- | A randomized version of 'animationOf'
--
-- Example:
-- 
-- > program = randomAnimationOf(movie)
-- > movie(random,t) = rotated(solidRectangle(width,height),45*t)
-- >     where
-- >     width  = 1 + 9 * random#1
-- >     height = 1 + 9 * random#2
--
-- The example above will show a random rectangle that rotates around
-- the origin. When you click on the output, a different random rectangle
-- will be shown, which will also be rotating around the origin.
--
randomAnimationOf :: (([Number],Number) -> Picture) -> Program
randomAnimationOf(movie) = interactionOf(initial,update,handle,draw)
    where
    initial (seed:rs) = (rs,0,seed)

    update((rs,t,seed),dt) = (rs,t+dt,seed)

    handle((rs,t,seed),PointerPress(_)) = (newrs,t,newseed)
        where
        newseed:newrs = randomNumbers(seed)
    handle(model,_) = model

    draw(rs,t,seed) = movie(rs,t)

-- | A randomized version of 'slideshow'
randomSlideshow :: ([Number] -> [Picture]) -> Program
randomSlideshow = randomSlideshow_

-- | A randomized version of 'autoSlideshow'
randomAutoSlideshow :: ([Number] -> [Picture], Number) -> Program
randomAutoSlideshow(mkslides,period) = simulationOf(initial,update,render)
  where
    initial (r:rs) = SS { time = 0, tlast = 0, current = 1, random = rs
                        , slides = mkslides(randomNumbers(r))
                        }

    update(ss,dt) = update_wrap(update_current(update_time(ss,dt)))
    
    update_time(ss@SS{..},dt) = ss { time = time + dt }

    update_current(ss@SS{..})
        | time - tlast > period = ss { tlast = tlast + period
                                     , current = current + 1
                                     }
        | otherwise = ss

    update_wrap(ss@SS{..})
        | current > length(slides) = ss { current = 1
                                        , random = rs
                                        , slides = mkslides(randomNumbers(r))
                                        }
        | otherwise = ss
            where
            r:rs = random
            
    render(SS{..}) = slides#current

-------------------------------------------------------------------------------
--- Top Level
-------------------------------------------------------------------------------

data SS = SS
  { time :: Number
  , tlast :: Number
  , current :: Number
  , random :: [Number]
  , slides :: [Picture]
  }
  
randomSlideshow_ :: ([Number] -> [Picture]) -> Program
randomSlideshow_(mkslides) = interactionOf(initial,update,handle,render)
    where
    initial (r:rs) = SS { time = 0, tlast = 0, current = 1, random = rs
                        , slides = mkslides(randomNumbers(r))
                        }
    update(ss@SS{..},dt) = ss { time = time + dt }
    
    render(SS{..})
      | empty(slides) = pictures([])
      | otherwise     = showSlide & slides#current
      where
      showSlide
          | time - tlast > 2 = blank
          | otherwise = translated(mark,-9.5,-9.5)
                
      mark = scaled(lettering(printed(current)),0.5,0.5)
           & colored(solidRectangle(1,1),RGB(0.9,0.9,0.9))
           
    handle(ss,event) = mayHandleEvent(ss)
      where
      handleNav(c,s) = case event of
          KeyPress "R" -> 1+length(s)
          KeyPress "N" -> c+1
          KeyPress " " ->  c+1
          KeyPress "Enter" -> c+1
          KeyPress "PageDown" -> c+1
          KeyPress "P" -> c-1
          KeyPress "PageUp" -> c-1
          KeyPress "Backspace" -> c-1
          PointerPress _ -> c+1
          other -> c
        
      handlePan(c,s) = case event of
          KeyPress "Left" -> over c moveleft s
          KeyPress "Right" -> over c moveright s
          KeyPress "Up" -> over c moveup s
          KeyPress "Down" -> over c movedown s
          KeyPress "A" -> over c moveleft s
          KeyPress "D" -> over c moveright s
          KeyPress "W" -> over c moveup s
          KeyPress "S" -> over c movedown s
          --KeyPress key -> (0,[messages [key]])
          other -> s
          where
          moveleft(p)  = translated(p,-1, 0)
          moveright(p) = translated(p, 1, 0)
          moveup(p)    = translated(p, 0, 1)
          movedown(p)  = translated(p, 0,-1)
          over n f slides = [ if i == n then f s else s
                            | s <- slides
                            | i <- [1..]
                            ]

      mayHandleEvent(ss) = wrap(handleEvent(ss))
          
      handleEvent(ss@SS{..}) = ss
          { current = nextCurrent
          , tlast = time
          , slides = handlePan(current,slides)
          }
          where
          nextCurrent = handleNav(current,slides)
          
      remake(ss@SS{..}) = ss
          { random = rs
          , slides = mkslides(randomNumbers(r))
          }
          where
          r:rs = random

      realign(ss,newCurrent) = ss {current = newCurrent}
      
      wrap(ss@SS{..})
          | current > nslides = wrap(realign(remake(ss),current-nslides))
          | current < 1 = wrap(realign(remake(ss),current+nslides))
          | otherwise = ss
          where
          nslides = length(slides)


-------------------------------------------------------------------------------
-- Animation Helpers
-------------------------------------------------------------------------------

-- | The expression @between(t,start,stop,drawing)@ will show
-- the given @drawing@ when the time @t@ is between @start@ and @stop@
between :: (Number,Number,Number,Picture) -> Picture
between(t,start,stop,drawing) =
  if t < start then blank
  else if t < stop then drawing
  else blank

-- | The expression @beyond(t,start,drawing)@ will
-- show the given @drawing@ when the time @t@ is beyond the @start@ time
beyond :: (Number,Number,Picture) -> Picture
beyond(t,start,drawing) =
  if t < start then blank
  else drawing

-- | The expression @saw(t,p)@ is @0@
-- when @t=0@, increases up to 1 when @t=p/2@, and then decreases back
-- to 0 when @t=p@.
-- This increasing and decreasing when @t@ goes from @0@ to @p@ is called
-- an oscillation of period @p@. The oscillations will keep repeating,
-- so that the function is @0@ when @t@ is @0,p,2p,3p,4p,5p,...@
-- and it is 1 when @t@ is @p/2@, @3p/2@, @5p/2@, @7p/2@, @...@
saw :: (Number,Number) -> Number
saw(t,p) = 1 - abs(2*abs(remainder(t,p))/p - 1)

------------------------------------------------------------------------------
-- Curve interpolation 
------------------------------------------------------------------------------

-- | @openCurvePoints(controls,distance)@ is a list of points that approximate
-- a curve passing through the given @controls@. A variable number of points
-- is generated in such a way that the distance between them is approximately
-- the given @distance@.
openCurvePoints :: ([Point],Number) -> [Point]

-- | This function is similar to 'openCurvePoints', but the points approximate
-- a closed curve passing through the given controls.
closedCurvePoints :: ([Point],Number) -> [Point]

(openCurvePoints,closedCurvePoints) = (ocp,ccp)
  where
  -- 1D Linear interpolation
  lerp1(a,b,t) = (1-t)*a + t*b

  -- 2D Linear interpolation
  lerp2((x0,y0),(x1,y1),t) = (lerp1(x0,x1,t),lerp1(y0,y1,t))

  -- 2D Quadratic interpolation
  qerp2(p0,p1,p2,t) = lerp2(lerp2(p0,p1,t),lerp2(p1,p2,t),t)

  -- 2D Cubic interpolation
  cerp2(p0,p1,p2,p3,t) = lerp2(qerp2(p0,p1,p2,t),qerp2(p1,p2,p3,t),t)

  -- initial interpolation (p1,p2)
  bezierFirst(p1,p2,p3,grain)
    | d12 <= grain = [p1]
    | otherwise    = [ qerp2(p1,c,p2,t) | t <- [0,grain..l] ]
    where
    c = vectorSum(p2,scaledVector(vectorDifference(p1,p3),r/2))
    r = d12 / (d12 + d23)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    l = 1 - grain / d12

  -- final interpolation (p2,p3)
  bezierLast(p1,p2,p3,grain)
    | d23 <= grain = [p2]
    | otherwise    = [ qerp2(p2,c,p3,t) | t <- [0,grain..l] ]
    where
    c = vectorSum(p2,scaledVector(vectorDifference(p3,p1),r/2))
    r = d23 / (d12 + d23)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    l = 1 - grain / d23

  -- middle interpolation (p2,p3)
  bezierMiddle(p1,p2,p3,p4,grain)
    | d23 <= grain = [p2]
    | otherwise    = [ cerp2(p2,c1,c2,p3,t) | t <- [0,grain..l] ]
    where
    c1 = vectorSum(p2,scaledVector(vectorDifference(p3,p1),r1/2))
    c2 = vectorSum(p3,scaledVector(vectorDifference(p2,p4),r2/2))
    r1 = d23 / (d12 + d23)
    r2 = d23 / (d23 + d34)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    d34 = dist(p3,p4)
    l = 1 - grain / d23


  ccp([],_) = []
  ccp([p1],_) = []
  ccp([p1,p2],_) = [p1,p2]
  ccp(ps,grain) = go(last(ps,1) ++ ps ++ first(ps,2))
    where
    go(p1:p2:p3:p4:more) = bezierMiddle(p1,p2,p3,p4,grain) ++ go(p2:p3:p4:more)
    go(_) = []

  ocp([],_) = []
  ocp([p1],_) = []
  ocp([p1,p2],_) = [p1,p2]
  ocp(p1:p2:p3:more,grain) = bezierFirst(p1,p2,p3,grain) ++ go(p1:p2:p3:more)
    where
    go(p1:p2:p3:p4:more) = bezierMiddle(p1,p2,p3,p4,grain) ++ go(p2:p3:p4:more)
    go([p1,p2,p3]) = bezierLast(p1,p2,p3,grain)
    go(_) = []

  dist(p,q) = vectorLength(vectorDifference(p,q))


-------------------------------------------------------------------------------
-- Colors
-------------------------------------------------------------------------------

-- | This function allows you to specify color components in the range 0 to 255
-- instead of 0 to 1.
rgb :: (Number,Number,Number) -> Color
rgb(r,g,b) = RGB(r/256,g/256,b/256) -- Not exact, but colors aren't anyway...

-- | This function allows you to specify the level of transparency of the
-- given color. Transparency must be given in the range 0 (fully transparent)
-- to 1 (fully opaque).
withAlpha :: (Color,Number) -> Color
withAlpha(RGBA(r,g,b,_),a) = RGBA(r,g,b,a)

-------------------------------------------------------------------------------
--- Layout
-------------------------------------------------------------------------------

-- | A picture that represents the given list of texts, so that each
-- text in the list is shown in a separate line. Lines start at the
-- top left corner of the output window and grow downward.
-- Each line of text can fit 66 characters, and 40 lines can fit
-- in a single page. The lettering is shown in monospaced font.
--
-- Example:
--
-- > program = slideshow(pages)
-- >   where
-- >   pages = foreach(gs,pageFromTexts)
-- >   gs = groups(ls,40)
-- >   ls = foreach(result,\g -> joinedWith(g,", "))
-- >   result = groups(forloop(1,(<= 2000000),(+ 1),printed),7)
-- >
--
-- The example above shows two million numbers in 7143 pages, so that each
-- page has 40 lines, each of which has 7 numbers. This example uses
-- 'forloop' and 'foreach' from "Extras.Util".
--
pageFromTexts :: [Text] -> Picture
pageFromTexts(lines) = pictures([showline(i) | i <- [1..n]])
    where
    n = length(lines)
    showline(i) = translated(scaled(fmt(lines#i),0.5,0.5),0,10.25-0.5*i)
    -- Output should be 40 rows and 66 columns
    fmt(txt) = styledLettering(lJustified(txt,66),Monospace,Italic)

-- | A @grid(cell,rows,columns)@ is a grid with the given number of @rows@ and
-- @columns@, where the rows are numbered top to bottom (top row is row 1)
-- and the columns are numbered left to right (leftmost column is column 1).
-- The user needs to specify what to draw at each cell in the grid. The given
-- function @cell@ should specify a 20 by 20 picture for each row and column,
-- where the first argument is the row number, and the second argument is the
-- column number:
-- 
-- > cell(row,col) = fullSizePicture
--
-- Each full size picture will be scaled to fit within the corresponding cell.
-- Look at the example in the documentation of 'sprite' to see how to use it.
--
grid :: ((Number,Number) -> Picture,Number,Number) -> Picture
grid(cell,rows,cols) = translated(pictures(rpic),-10,10)
  where
  w = 20/cols
  h = 20/rows
  transform(row,col) = translated(base,w*(col-1/2),-h*(row-1/2))
    where
    base = scaled(cell(row,col),1/cols,1/rows)
  rpic = [ transform(row,col) | row <- [1..rows], col <- [1..cols] ]

-- | A @sprite(picture,rows,columns)@ creates a function that can be used to
-- place the given @picture@ of a sprite into a grid with the given
-- number of @rows@ and @columns@. The created function needs two
-- arguments: the row and the column at which you want to show the sprite.
-- The picture of the sprite should be a full size picture (20x20), which
-- will be scaled to fit the corresponding cell in the grid.
--
-- 
-- Example:
--
-- > import Extras.Cw(saw,grid,sprite)
-- > import Extras.Util(printedPoint)
-- > 
-- > program = animationOf(movie)
-- > 
-- > movie(t) = sprite1(row,col) & background
-- >   where
-- >   -- Jump between rows 1 and 10 every 100 seconds
-- >   row = 1 + truncation(10*saw(t,100))
-- >   -- Move smoothly between cols 1 and 10 every 10 seconds
-- >   col = 1 + 9*saw(t,10)
-- > 
-- > sprite1 = sprite(pic,10,10)
-- >   where
-- >   pic = rotated(eks,45)
-- >   eks = solidRectangle(10,1) & solidRectangle(1,10)
-- > 
-- > background = grid(cell,10,10)
-- >     where
-- >     cell(row,col) = dilated(lettering(printedPoint(row,col)),5)
-- >                   & rectangle(19,19)
--
sprite :: (Picture,Number,Number) -> (Number,Number) -> Picture
sprite(pic,rows,cols) = transform
  where
  w = 20/cols
  h = 20/rows
  dw = w/2 + 10
  dh = h/2 + 10
  base = scaled(pic,1/cols,1/rows)
  transform(row,col) = translated(base,w*col-dw,-h*row+dh)

-- | @overlays(fig,n)@ is a shortcut for @fig(1) & fig(2) & ... & fig(n)@
overlays :: ((Number -> Picture),Number) -> Picture
overlays(f,n) = overlays'(f,max(0,truncation(n)))
    where
    overlays'(f,0) = blank
    overlays'(f,n) = overlays'(f,n-1) & f(n)

-- | @underlays(fig,n)@ is a shortcut for @fig(n) & fig(n-1) & ... & fig(1)@
underlays :: ((Number -> Picture),Number) -> Picture
underlays(f,n) = underlays'(f,max(0,truncation(n)))
    where
    underlays'(f,0) = blank
    underlays'(f,n) = f(n) & underlays'(f,n-1)

-------------------------------------------------------------------------------
--- Zoomable graph
-------------------------------------------------------------------------------

-- | The given picture dilated by the given scaling factor and shown in a graph
-- that zooms along with the picture. For example, a scaling factor of 2 means
-- that the picture will show twice as big as usual in each direction.
--
-- Example:
--
-- > program = guiDrawingOf(widgets,draw)
-- >   where
-- >   widgets = [withConversion(\v -> 2^(-8 + v*16), slider("zoom",-8,9))]
-- >   draw([zoom]) = graphed(pictures([circle(n) | n <- [1..100]]), zoom)
--
-- The example above shows a graph with 100 circles. It used the function
-- 'guiDrawingOf' from "Extras.Widget".
--
graphed :: (Picture,Number) -> Picture
graphed(pic,zoom) = graph(10/zoom) & dilated(pic,zoom)

graph(maxnum) = labels & axes & rotated(axes,90)
  where
  semiMajor = pictures(forloop(0,(<= maxnum),(+ major),majorAxis))
  semiMinor = pictures(forloop(0,(<= maxnum),(+ minor),minorAxis))
  (limit,major,_) = resolution(maxnum)
  (_,minor,numdec) = resolution(major)
  scaling = 10/maxnum
  axes = semiMajor & scaled(semiMajor,-1,1)
       & semiMinor & scaled(semiMinor,-1,1)
    
  labels = pictures(forloop(major,(<= maxnum),(+ major),pq))
         & pictures(forloop(-major,(>= -maxnum),(+ (-major)),pq))
  axis(x) = polyline([(x*scaling,-10),(x*scaling,10)])
  majorAxis(x) = colored(axis(x),g(0.2,0.5))
  minorAxis(x) = colored(axis(x),g(0.1,0.2))
  g(s,a) = RGBA(s,s,s,a)
  pq(x) = p(x) & q(x)
  p(x) = translated(dilated(print,1/2),x*scaling,-1/2)
    where
    print = styledLettering(printed(x),Monospace,Plain)
  q(y) = translated(dilated(print,1/2),-1,y*scaling)
    where
    print = styledLettering(rJustified(printed(y),5),Monospace,Plain)

-- Major interval, minor interval and number of decimals
resolution(x) = if x >= 1 then goUp(1) else goDn(1,0)
  where
  
  goDn(base,dec) | x > base5 = (base,base/5,dec)
                 | x > base2 = (base5,base5/5,dec+1)
                 | x > base1 = (base2,base2/4,dec+1)
                 | otherwise = goDn(base1,dec+1)
      where
      base5 = base/2
      base2 = base/5
      base1 = base/10
    
  goUp(base) | x <= base1 = (base1,base1/5,0)
             | x <= base2 = (base2,base2/4,0)
             | x <= base5 = (base5,base5/5,0)
             | otherwise = goUp(base*10)
      where
      base1 = base
      base2 = base*2
      base5 = base*5
