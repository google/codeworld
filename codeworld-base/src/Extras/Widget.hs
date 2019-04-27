{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | A very simple Graphical User Interface (GUI) for user interaction with
-- buttons, checkboxes, sliders and a few others.
module Extras.Widget( guiDrawingOf, guiActivityOf
                    -- * Widgets
                    , Widget
                    , toggle, button, slider, randomBox, timer, counter
                    -- * Convenience functions
                    , withConversion, setConversion
                    -- * Examples
                    , widgetExample1, widgetExample2, widgetExample3
                    )
where

import Prelude


--------------------------------------------------------------------------------
-- Widget API
--------------------------------------------------------------------------------

-- | The function @guiDrawingOf@ is an entry point for drawing that allows
-- access to a simple GUI. It needs two arguments: a list of
-- Widgets and a function to create your drawing. This user-supplied drawing
-- function will have access to the list of the current values of the widgets,
-- which is passed as an argument.
--
-- Example of use:
--
-- > program = guiDrawingOf(widgets,draw)
-- >   where
-- >   widgets = [ withConversion(\v -> 1 + 19 * v   , slider("width"        ,-7,-7))
-- >             , withConversion(\v -> 1 + 19 * v   , slider("height"       ,-7,-9))
-- >             , withConversion(flipflop           , toggle("show circle"  ,-7,-5))
-- >             , withConversion(flipflop           , button("show in green",-7,-3))
-- >             , withConversion(\v -> 0.2 + 0.8 * v, randomBox("radius"    ,-7,-1))
-- >             ]
-- > 
-- >   flipflop(v) = truncation(1 + 2 * v)
-- > 
-- > draw(values) = blank
-- >   & [blank, circle(r)]#s
-- >   & colored(solidRectangle(w,h),[red,green]#c)
-- >   where
-- >   w = values#1
-- >   h = values#2
-- >   s = values#3
-- >   c = values#4
-- >   r = values#5
--
-- Note that the order in which the widgets are defined is important,
-- because it determines how to access the correct value.
-- Each widget fits in a box 4 units wide and 1 unit high.
guiDrawingOf :: ([Widget],[Number] -> Picture) -> Program
guiDrawingOf(widgetsUser,drawUser) = activityOf(initAll,updateAll,drawAll)
  where
  initAll(rs) = initRandom(widgetsUser,rs)

  updateAll(ws,event) = ws.$updateWidget(event)

  drawAll(ws) = pictures(ws.$drawWidget) & drawUser(ws.$value)

-- | The function @guiActivityOf@ is similar to @activityOf@, but it also
-- takes in a list of widgets. The updating and drawing functions also
-- receive a list of the current values of the widgets.
--
-- Example of use:
--
-- > program = guiActivityOf(widgets,init,update,draw)
-- >   where
-- >   widgets = [ withConversion(\v -> 20 * v, slider("width",-7,-7))
-- >             , withConversion(\v -> 2 + 3 * v, slider("height",-7,-9))
-- >             , withConversion
-- >               (\v -> truncation(1 + 2*v), toggle("show circle",-7,-5))
-- >             , button("restart",-7,-3)
-- >             , randomBox("new color",-7,-1)
-- >             ]
-- > 
-- > draw(values,(color@(RGB(r1,r2,r3)),angle,_)) = colored(base,color)
-- >     & [blank, circle(5)]#s
-- >     & translated(lettering(msg),0,9)
-- >     where
-- >     msg = joined(["(",printed(r1),",",printed(r2),",",printed(r3),")"])
-- >     base = rotated(solidRectangle(w,h),angle)
-- >     w = values#1
-- >     h = values#2
-- >     s = values#3
-- > 
-- >   init(rs) = (RGB(rs#1,rs#2,rs#3),0,0)
-- > 
-- >   update(values,(color@(RGB(r1,r2,r3)),angle,wait),TimePassing(_))
-- >     | values#4 > 0 , wait == 0 = (RGB(r2,r3,r),0,values#4)
-- >     | otherwise = (color,angle+1,wait)
-- >     where
-- >     r = values#5
-- > 
-- >   update(values,(color,angle,wait),PointerRelease(_)) = (color,angle,0)
-- > 
-- >   update(values,state,event) = state
--
-- Note that pre-defined actions on the widgets take precedence over
-- anything that you define in your updating function, so you cannot
-- alter the default behavior of the widgets.
guiActivityOf :: ( [Widget]
                 , [Number] -> state
                 , ([Number],state,Event) -> state
                 , ([Number],state) -> Picture) -> Program
guiActivityOf(widgetsUser,initUser,updateUser,drawUser) =
  activityOf(initAll,updateAll,drawAll)
  where
  initAll(rs) = ( initRandom(widgetsUser,rest(rs,1)) 
                , initUser(randomNumbers(rs#1)))
  
  updateAll((widgets,state),event) =
    (newWidgets,updateUser(widgets.$value,state,event))
    where
    newWidgets = widgets.$updateWidget(event)
    
  drawAll(widgets,state) =
    pictures(widgets.$drawWidget) & drawUser(widgets.$value,state)

initRandom(ws,rs) = [ t(w,r) | w <- ws | r <- rs ]
  where
  t(w,r) | isRandom(w) = let rp = randomNumbers(r)
                         in w { value_ = rp#1, randomPool = rest(rp,1) }
         | otherwise   = w

isRandom(Widget{widget = Random}) = True
isRandom(_                      ) = False

-- | A button placed at the given location. While
-- the button is pressed, the value produced is 0.5,
-- but when the button is released, the value reverts
-- back to 0.
button :: (Text,Number,Number) -> Widget
button(p) = (newWidget(p)) { widget = Button }

-- | A toggle (checkbox) with the given label at the given location.
-- When the box is not set, the value produced is 0. When the
-- box is set, the value produced is 0.5
toggle :: (Text,Number,Number) -> Widget
toggle(p) = (newWidget(p)) { widget = Toggle }

-- | A slider with the given label at the given location.
-- The possible values will range from 0 to 1, and the initial
-- value will be 0.
slider :: (Text,Number,Number) -> Widget
slider(p) = (newWidget(p)) { widget = Slider }

-- | A box that produces a random number between 0 and 1.
-- Each time you click on it, the value will change. The
-- value 1 is never produced, so the actual range of
-- values is 0 to 0.99999...
randomBox :: (Text,Number,Number) -> Widget
randomBox(p) = (newWidget(p)) { widget = Random }

-- | A button that keeps incrementing the value each time you press it.
-- The initial value is 1.
counter :: (Text,Number,Number) -> Widget
counter(p) = (newWidget(p)) { widget = Counter, value_ = 1 }

-- | A toggle that counts time up when you set it. When you click on
-- the left side of the widget, the current value is reset to 0.
-- You can stop the timer and start it again, and the value will increase
-- from where it was when you stopped it.
--
-- Example:
--
-- > program = guiDrawingOf(widgets,draw)
-- >   where
-- >   widgets = [ withConversion(\v -> 1 + 9 * v , slider("length",-7,-7))
-- >             , withConversion(\v -> v * 30    , timer("angle"  ,-7,-9)) ]
-- > 
-- >   draw([l,a]) = rotated(translated(colored(solidRectangle(l,0.25),red),l/2,0),a)
--
-- The timer operates in seconds, including decimals. However, the precision
-- of the timer is not guaranteed beyond one or two decimals.
--
timer :: (Text,Number,Number) -> Widget
timer(p) = (newWidget(p)) { widget = Timer }

-- | Make the widget use the provided function to convert values from
-- the default range of a widget to a different range.
--
-- Example:
--
-- > newSlider = withConversion(\v -> 20 * v - 10, oldSlider)
--
-- Assuming that the old slider did not have any conversion function applied
-- to it, the example above will make the new slider produce values
-- between -10 and 10, while the old slider will still produce values
-- between 0 and 1
withConversion :: (Number -> Number, Widget) -> Widget
withConversion(conv,w) = w { conversion = conv }

-- | Same functionality as @withConversion@, but using a different convention
-- for the arguments.
setConversion :: (Number -> Number) -> Widget -> Widget
setConversion(conv)(w) = w { conversion = conv }

{-
-- | Set the minimum and the maximum values. The current value
-- will then be set to the minimum.
setValues :: (Number,Number) -> Widget -> Widget
setValues(minv',maxv')(w) =
  w { minv = minv', maxv = maxv', value_ = minv' }
-}

-- | This is the example shown in the documentation for @guiDrawingOf@
widgetExample1 :: Program
widgetExample1 = guiDrawingOf(widgets,draw)
  where
  widgets = [ withConversion(\v -> 1 + 19 * v   , slider("width"        ,-7,-7))
            , withConversion(\v -> 1 + 19 * v   , slider("height"       ,-7,-9))
            , withConversion(flipflop           , toggle("show circle"  ,-7,-5))
            , withConversion(flipflop           , button("show in green",-7,-3))
            , withConversion(\v -> 0.2 + 0.8 * v, randomBox("radius"    ,-7,-1))
            ]

  flipflop(v) = truncation(1 + 2 * v)

  draw(values) = blank
    & [blank, circle(r)]#s
    & colored(solidRectangle(w,h),[red,green]#c)
    where
    w = values#1
    h = values#2
    s = values#3
    c = values#4
    r = values#5

-- | This is the example shown in the documentation for @guiActivityOf@
widgetExample2 :: Program
widgetExample2 = guiActivityOf(widgets,init,update,draw)
  where
  widgets = [ withConversion(\v -> 20 * v, slider("width",-7,-7))
            , withConversion(\v -> 2 + 3 * v, slider("height",-7,-9))
            , withConversion
              (\v -> truncation(1 + 2*v), toggle("show circle",-7,-5))
            , button("restart",-7,-3)
            , randomBox("new color",-7,-1)
            ]

  draw(values,(color@(RGB(r1,r2,r3)),angle,_)) = colored(base,color)
    & [blank, circle(5)]#s
    & translated(lettering(msg),0,9)
    where
    msg = joined(["(",printed(r1),",",printed(r2),",",printed(r3),")"])
    base = rotated(solidRectangle(w,h),angle)
    w = values#1
    h = values#2
    s = values#3

  init(rs) = (RGB(rs#1,rs#2,rs#3),0,0)

  update(values,(color@(RGB(r1,r2,r3)),angle,wait),TimePassing(_))
    | values#4 > 0 , wait == 0 = (RGB(r2,r3,r),0,values#4)
    | otherwise = (color,angle+1,wait)
    where
    r = values#5

  update(values,(color,angle,wait),PointerRelease(_)) = (color,angle,0)

  update(values,state,event) = state

-- | This is the example shown in the documentation for @timer@
widgetExample3 = guiDrawingOf(widgets,draw)
  where
  widgets = [ withConversion(\v -> 1 + 9 * v , slider("length",-7,-7))
            , withConversion(\v -> v * 30    , timer("angle"  ,-7,-9)) ]

  draw([l,a]) = rotated(translated(colored(solidRectangle(l,0.25),red),l/2,0),a)

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

data WidgetType = Button | Toggle | Slider | Random | Counter | Timer

-- | The internal structure of a @Widget@ is not exposed in the user interface. You
-- have access only to the current value of each widget.
data Widget = Widget
  { selected :: Truth
  , highlight :: Truth
  , width :: Number
  , height :: Number
  , centerAt :: (Number,Number)
  , label :: Text
  , conversion :: Number -> Number
  , value_ :: Number
  , widget :: WidgetType
  , randomPool :: [Number]
  }

newWidget(l,x,y) = Widget
  { selected = False, highlight = False, width = 4, height = 1
  , centerAt = (x,y), label = l
  , value_ = 0, conversion = (\v -> v)
  , widget = Button, randomPool = []
  }
  

-- The value, adjusted according to the conversion function
value :: Widget -> Number
value(Widget{..}) = value_.#conversion

-- The current value of a widget is set as follows.
-- For sliders, the value is a Number
-- between 0 and 1 (both included). For buttons and checkboxes,
-- the value is 0 when they are not set and 0.5 when they are set.
-- These values allow user programs to work with either
-- @guiDrawingOf@ or @randomDrawingOf@ interchangeably, without having
-- to alter the calculations in the code.

hit(mx,my,Widget {..}) = abs(mx-x) < width/2 && abs(my-y) < height/2
  where
  (x,y) = centerAt

hitReset(mx,my,Widget {..}) = mx - xmin < 0.3 && abs(my - y) < height/2
  where
  (x,y) = centerAt
  xmin = x - width/2
  
drawWidget(w) = case w.#widget of
  Button -> drawButton(w)
  Toggle -> drawToggle(w)
  Slider -> drawSlider(w)
  Random -> drawRandom(w)
  Counter -> drawCounter(w)
  Timer  -> drawTimer(w)
  
drawButton(Widget{..}) = drawLabel & drawSelection & drawHighlight
  where
  solid = scaled(solidCircle(0.5),w,h)
  outline = scaled(circle(0.5),w,h)
  (x,y) = centerAt
  msg = dilated(lettering(label),0.5)
  w = 0.9 * width
  h = 0.9 * height
  drawLabel = translated(msg,x,y)
  drawSelection
    | selected = translated(colored(solid,grey),x,y)
    | otherwise = translated(outline,x,y)
  drawHighlight
    | highlight = translated(colored(rectangle(width,height),light(grey)),x,y)
    | otherwise = blank

drawCounter(Widget{..}) = drawLabel & drawSelection
  where
  solid = scaled(solidPolygon(points),w,h)
  outline = scaled(polygon(points),w,h)
  points = [(0.5,0.3),(0,0.5),(-0.5,0.3),(-0.5,-0.3),(0,-0.5),(0.5,-0.3)]
  (x,y) = centerAt
  msg(txt) = translated(dilated(lettering(txt),0.5),x,y)
  w = 0.9 * width
  h = 0.9 * height
  drawLabel 
    | highlight = msg(printed(value_.#conversion))
    | otherwise = msg(label)
  drawSelection
    | selected = translated(colored(solid,grey),x,y)
    | highlight = translated(colored(outline,black),x,y)
    | otherwise = translated(colored(outline,grey),x,y)

drawToggle(Widget{..}) = drawSelection & drawLabel & drawHighlight
  where
  w = 0.5
  h = 0.5
  x' = x + 2/5*width
  drawSelection
    | selected = translated(colored(solidRectangle(w,h),grey),x',y)
    | otherwise = translated(rectangle(0.9*w,0.9*h),x',y)
  drawLabel = translated(msg,x - width/10,y)
  drawHighlight
    | highlight = colored(outline,light(grey))
                & translated(rectangle(w,h),x',y)
    | otherwise = colored(outline,light(light(grey)))
  outline = translated(rectangle(width,height),x,y)
  (x,y) = centerAt
  msg = dilated(lettering(label),0.5)
  
drawTimer(Widget{..}) = drawLabel & drawSelection & drawReset & drawHighlight
  where
  x' = x + 2/5*width
  xmin = x - width/2
  drawLabel
    | highlight = msg(printed(value_.#conversion))
    | otherwise = msg(label)
  drawSelection
    | selected  = translated(box(0.5,0.5), x', y)
    | otherwise = translated(rectangle(0.45,0.45),x',y)
  drawReset = translated(box(0.3,height), xmin+0.15, y)
  drawHighlight
    | highlight = outline
                & translated(rectangle(0.5,0.5),x',y)
    | otherwise = colored(outline,light(grey))
  outline = translated(rectangle(width,height),x,y)
  (x,y) = centerAt
  msg(txt) = translated(dilated(lettering(txt),0.5), x-width/10, y)
  box(w,h) = colored(solidRectangle(w,h),grey)
  
drawSlider(Widget{..}) = info & foreground & background
  where
  info = translated(infoMsg,x,y-height/4)
  foreground = translated(solidCircle(height/4),x',y')
             & translated(colored(solidRectangle(width,height/4),grey),x,y')
  x' = x - width/2 + value_ * width
  y' = y + height/4
  background
    | highlight = translated(colored(rectangle(width,height),light(grey)),x,y)
    | otherwise = blank
  (x,y) = centerAt
  infoMsg = dilated(lettering(label<>": "<>printed(value_.#conversion)),0.5)
  
drawRandom(Widget{..}) = drawLabel & drawSelection & drawHighlight
  where
  solid = scaled(solidRectangle(1,1),width,height)
  outline = scaled(rectangle(1,1),width,height)
  (x,y) = centerAt
  msg = dilated(lettering(label <> ": " <> printed(value_.#conversion)),0.5)
  drawLabel = translated(msg,x,y)
  drawSelection
    | selected = translated(colored(solid,grey),x,y)
    | otherwise = blank
  drawHighlight
    | highlight = translated(outline,x,y)
    | otherwise = colored(translated(outline,x,y),grey)

updateWidget(PointerPress(mx,my))(w@Widget{..})
  | widget == Button, hit(mx,my,w) = w { selected = True, highlight = False 
                                       , value_ = 0.5
                                       }
  | widget == Button               = w { selected = False, highlight = False 
                                       , value_ = 0
                                       }
  | widget == Counter, hit(mx,my,w) = w { selected = True, highlight = True 
                                       , value_ = 1 + value_
                                       }
  | widget == Toggle, hit(mx,my,w) = w { selected = not(selected)
                                       , value_ = 0.5 - value_
                                       , highlight = True
                                       }
  | widget == Timer, hitReset(mx,my,w) = w { value_ = 0 }
  | widget == Timer, hit(mx,my,w)  = w { selected = not(selected)
                                       , highlight = True
                                       }
  | widget == Slider, hit(mx,my,w) = w { selected = True, highlight = True
                                       , value_ = updateSliderValue(mx,w)
                                       }
  | widget == Random, hit(mx,my,w) = w { selected = True, highlight = True
                                       , value_ = randomPool#1
                                       , randomPool = rest(randomPool,1)
                                       }
  | otherwise                      = w

updateWidget(PointerMovement(mx,my))(w) =
  w.#updateHighlight(mx,my).#updateSlider(mx)
  
updateWidget(PointerRelease(_))(w@Widget{..})
  | widget == Toggle = w
  | widget == Timer  = w
  | selected         = w { selected = False, highlight = False
                         , value_ = if widget == Button then 0 else value_
                         }
  | otherwise        = w

updateWidget(TimePassing(dt))(w@Widget{..})
  | widget == Timer, selected = w { value_ = dt + value_ }
  | otherwise = w
  
updateWidget(_)(widget) = widget

updateHighlight(mx,my)(w)
  | hit(mx,my,w) = w { highlight = True }
  | otherwise    = w { highlight = False }

updateSlider(mx)(w@Widget{..})
  | widget == Slider, selected = w { value_ = updateSliderValue(mx,w) }
  | otherwise                  = w

updateSliderValue(mx,s@Widget{..}) =
  (mx' - x + width/2) / width
  where
  mx' = max(x-width/2,min(x+width/2,mx))
  (x,_) = centerAt

x .# f = f(x)
xs .$ f = [f(x) | x <- xs]
