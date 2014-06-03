{-
  Example: Grandma

  This game was written by Sophia, a 6th grader from Colorado.  It
  demonstrates some interesting uses of interactions, including drag
  and drop.
-}
type RandomNumbers = [Number]

data World = Splash RandomNumbers
           | Playing Drag [Obstacle] Number Tool Number RandomNumbers Number Number

data Drag = NotDragging
          | Dragging Point Tool

data Obstacle = Obstacle Point Number ObstacleType

data ObstacleType = Rock | Tree | Bird | Car | Worm

data Tool = Balloons | Parachute | Pillow | None deriving Eq

distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

initial :: RandomNumbers -> World
initial g = Splash g

event :: Event -> World -> World
event (MousePress _ _) (Splash g) = Playing NotDragging [] 100 None (-100) g 0 0
event (MousePress _ (x,y)) (Playing drag obstacles health tool gy g angle age) =
    if distance (x,y) balloonPosition < 50 then Playing (Dragging (x,y) Balloons) obstacles health tool gy g angle age
    else if distance (x,y) parachutePosition < 50 then Playing (Dragging (x,y) Parachute) obstacles health tool gy g angle age
    else if distance (x,y) pillowPosition < 50 then Playing (Dragging (x,y) Pillow) obstacles health tool gy g angle age
    else Playing drag obstacles health tool gy g angle age

event (MouseRelease _ (x,y)) (Playing NotDragging obstacles health tool gy g angle age) =
    Playing NotDragging obstacles health tool gy g angle age
event (MouseRelease _ (x,y)) (Playing (Dragging p t) obstacles health tool gy g angle age) =
    if distance (x,y) (-150,y) < 50
    then Playing NotDragging obstacles health t gy g angle 0
    else Playing NotDragging obstacles health tool gy g angle age

event (MouseMovement (x,y)) (Playing NotDragging obstacles health tool gy g angle age) =
    Playing NotDragging obstacles health tool gy g angle age
event (MouseMovement (x,y)) (Playing (Dragging p t) obstacles health tool gy g angle age) =
    Playing (Dragging (x,y) t) obstacles health tool gy g angle age

event e w = w

balloonPosition = (-150, -200)
parachutePosition = (0, -200)
pillowPosition = (150, -200)

step :: Number -> World -> World
step dt (Splash g) = Splash g
step dt w = checkDead (addObstacles dt (ageTool dt (moveGranny dt (updateHealth (updateObstacles dt (turnWheel dt w ))))))

checkDead :: World -> World
checkDead (Playing drag obstacles health tool y g angle age) =
    if health == 0
    then Splash g
    else Playing drag obstacles health tool y g angle age

addObstacles :: Number -> World -> World
addObstacles dt (Playing drag obstacles health tool y (t:j:rs) angle age) = Playing drag newObstacles health tool y rs' angle age
    where newObstacles = if t < dt / 5
                         then obstacles ++ [newObstacle]
                         else obstacles
          choice            = floor (5*j)
          (newObstacle,rs') = if choice == 0 then (newRock, rs)
                              else if choice == 1 then (newTree, rs)
                              else if choice == 2 then newBird rs
                              else if choice == 3 then newWorm rs
                              else newCar rs

newRock :: Obstacle
newRock  = Obstacle (300,-50) (-30) Rock

newTree :: Obstacle
newTree  = Obstacle (300,50) (-30) Tree

newCar :: RandomNumbers -> (Obstacle,RandomNumbers)
newCar (r:rs) = (Obstacle (300,-50) (20*r - 60) Car, rs)

newBird :: RandomNumbers -> (Obstacle,RandomNumbers)
newBird (r1:r2:rs) = (Obstacle (300,150*r2 + 100) (20*r1 - 60) Bird, rs)

newWorm :: RandomNumbers -> (Obstacle,RandomNumbers)
newWorm (r:rs) = (Obstacle (300,-50) (10*r - 40) Worm, rs)

ageTool :: Number -> World -> World
ageTool dt (Playing drag obstacles health tool y g angle age) = Playing drag obstacles health newTool y g angle newAge
    where newTool = if age <= 10 then tool else None
          newAge  = age + dt

moveGranny :: Number -> World -> World
moveGranny dt (Playing drag obstacles health tool y g angle age) = Playing drag obstacles health tool newY g angle age
    where newY = if y + ySpeed * dt > (-100) then y + ySpeed * dt else -100
          ySpeed = if      tool == Parachute && y > (-100) then -25
                   else if tool == Parachute               then   0
                   else if tool == Balloons && y < 250     then  25
                   else if tool == Balloons                then   0
                   else if y > (-100)                      then -50
                   else                                           0

updateHealth :: World -> World
updateHealth (Playing drag obstacles health tool y g angle age) = Playing drag newObstacles newHealth tool y g angle age
    where newObstacles = [ob | ob <- obstacles, not (tooClose ob y)]
          newHealth    = if length newObstacles < length obstacles then health - 10 else health

tooClose :: Obstacle -> Number -> Bool
tooClose (Obstacle (x,y) xs typ) gy = distance (-150,gy) (x,y) < 100

updateObstacles :: Number -> World -> World
updateObstacles dt (Playing drag obstacles health tool y g angle age) = Playing drag newObstacles health tool y g angle age
    where newObstacles = [Obstacle (x + xs * dt,y) xs t | Obstacle (x,y) xs t <- obstacles, x >= (-300) ]

turnWheel :: Number -> World -> World
turnWheel dt (Playing drag obstacles health tool y g angle age) = Playing drag obstacles health tool y g (angle - 45 * dt) age

draw :: World -> Picture
draw (Playing drag obstacles health tool y g angle age) = pictures [
    drawDraggedTool drag,
    drawGranny y angle tool,
    drawObstacles obstacles,
    drawBackground
    ]

draw (Splash g) = titleScreen

drawBackground = pictures [
               fullPic,
               color (dark violet) (solidRectangle 500 500)
               ]
drawObstacles obs = pictures [ translate x y (drawObstacle t) | (Obstacle (x,y) xs t) <- obs ]

drawObstacle Car  = car
drawObstacle Worm = worm
drawObstacle Tree = tree
drawObstacle Bird = bird
drawObstacle Rock = rock

drawGranny y angle tool = pictures [
                        translate (-150) y (scale 0.7 0.7 (grandmaTool angle tool))
                        ]

grandmaTool angle None = grandma angle
grandmaTool angle Balloons = pictures [ grandma angle, translate 0 180 (scale 0.7 0.7 balloonPic) ]
grandmaTool angle Parachute = pictures [ grandma angle, translate 0 180 (scale 2.0 2.0 parachute) ]
grandmaTool angle Pillow = pictures [ grandma angle, translate 0 (-75) pillow ]

drawDraggedTool NotDragging = blank
drawDraggedTool (Dragging (x,y) Parachute) = translate x (y+50) (scale 2.0 2.0 parachute)
drawDraggedTool (Dragging (x,y) Balloons) = translate x (y+50) (scale 0.7 0.7 balloonPic)
drawDraggedTool (Dragging (x,y) Pillow) = translate x y pillow

fullPic = pictures [
        translate 0 50 (color (light (light cyan)) (solidRectangle 500 400)),
        translate   150  (-200)  (scale 0.7 0.7 pillow),
        translate (   0) (-200)  (parachute),
        translate (-150) (-200)  (scale 0.3 0.3 balloonPic)
        ]

balloonPic = pictures [
        translate 0 100 (pictures (zipWith color balloonColors balloons)),
        balloonStrings
        ]

balloons =
        [translate (r*(-25)+50*c) ((-40)*r) (balloon) | r <- [0,1,2,3],
                                                      c <- [0 .. r]
                                                      ]

balloonStrings = line [
        ( 12, -178),
        ( 49,  -37),
        ( 12, -178),
        (  0,  -37),
        ( 12, -178),
        ( -50, -38),
        ( 12, -178),
        (  0,   77),
        ( 12, -178),
        ( 41,   42),
        ( 12, -178),
        (-60,   10),
        ( 12, -178),
        (-28,  -17),
        ( 12, -178),
        ( 77,  -20),
        ( 12, -178),
        (-77,  -34),
        ( 12, -178)
        ]


balloon = scale 1 1.2 (solidCircle 30)

balloonColors = cycle [red, green, blue, orange, cyan, violet]


parachute = translate 0 (-50) (pictures [
            topOfParachute,
            strings
            ])

topOfParachute = thickArc 60 120 80 20

strings = pictures [
    line [(0,0), (30,70)],
    line [(0,0), (0,80)],
    line [(0,0), (-30,70)]
    ]

pillow = pictures [
       corners,
       torso
       ]

torso = scale 1.5 1 (color (light(light blue)) (solidCircle 40))

corners = pictures [
        translate (-50)   24 (rotate (-45) (scale 1.7 0.6 (bx (solidCircle 10)))),
        translate   45    24 (rotate   45  (scale 1.7 0.6 (bx (solidCircle 10)))),
        translate (-48) (-24)(rotate   45  (scale 1.7 0.6 (bx (solidCircle 10)))),
        translate   40  (-24)(rotate (-45) (scale 1.7 0.6 (bx (solidCircle 10))))
        ]


bx = color (light(light blue))

bird = pictures [
       mouth,
       eye,
       wing2,
       wing1,
       birdBody
       ]

birdBody = color (light blue) (polygon [
       (-20, -3),
       (-22,-12),
       (-11,-17),
       ( 30,  0),
       ( 70,  2),
       ( 32, 10),
       ( 37, 33),
       ( 17,  7)
       ])

wing1 = color (light (light blue)) (polygon [
        (60, -20),
        (60, -50),
        ( 0,   0)
        ])

wing2 = rotate (-45) (scale (-1) 1 (wing1))

eye = color blue (translate (-15) (-9) (solidCircle 2))

mouth = color (dark red) (line [
        (-11,-13),
        (-13,-15)
        ])

car = scale (-1) 1 (pictures [
      color  blue         (win3),
      color (light blue)  (win2),
      color  green        (win1),
      wheel2,
      wheel1,
      color red (body),
      carTop
      ])

carTop = polygon [
      (-30, 30),
      ( 20, 30),
      ( 50,  0),
      (-50,  0)
      ]

body = solidRectangle 100 25

wheel1 = translate (-38) (-20) (solidCircle 15)

wheel2 = translate   38  (-20) (solidCircle 15)

win1 = translate 10 14 (solidRectangle 20 30)

win2 = polygon [
       (20,30),
       (40,10),
       (20,10)
       ]

win3 = translate 10 (-2) (solidRectangle 20 20)

rock = scale 0.3 0.3 (polygon [
       ( -50, 100),
       (-100,  50),
       (-100, -50),
       ( -50,-100),
       (  50,-100),
       ( 100, -50),
       ( 100,  50),
       (  50, 100)
       ])

tree = scale 0.5 0.5 (pictures [
       bulk,
       trunk
       ])

trunk = color (dark(dark(dark orange))) (solidRectangle 50 150)

bulk = pictures [
       color green (translate (-30)  90 (solidCircle 75)),
       color green (translate (-60) 185 (solidCircle 75)),
       color green (translate    0  160 (solidCircle 75)),
       color green (translate    0  215 (solidCircle 75)),
       color green (translate   60  185 (solidCircle 75)),
       color green (translate   30   90 (solidCircle 75))
       ]

worm    = pictures [hat, wormF]

wormF   = eye1 & eye2 & color brown (pictures [part1,part2,part3,part4,part5])

part1   = solidCircle 20
part2   = translate 20 (-10) (solidCircle 20)
part3   = translate (-20) (-10) (solidCircle 20)
part4   = translate 25 5 (part2)
part5   = translate (-25) 5 (part3)

eye1    = color black (translate 40 0 (solidCircle 5))
eye2    = color black (translate 55 0 (solidCircle 5))

hat     = translate 47 35 (pictures [flower, bottom, top])
top     = solidRectangle 25 50
bottom  = translate 0 (-20) (scale 2 1 (solidCircle 10))
flower  = pictures [petals,center]

center  = color yellow (solidCircle 5)

petals  = pictures [petal1,petal2,petal3,petal4]
petal1  = color violet (translate 5 5 (solidCircle 5))
petal2  = color violet (translate 5 (-5) (solidCircle 5))
petal3  = color violet (translate (-5) 5 (solidCircle 5))
petal4  = color violet (translate (-5) (-5) (solidCircle 5))

titleScreen = pictures [
              title,
              needle2,
              needle1,
              knittingBall,
              backround
              ]

backround = color (light(light blue)) (solidRectangle 500 500)

knittingBall = color violet (solidCircle 30)

needle1 = translate (-10) 0 (rotate 45 (solidRectangle 5 100))

needle2 = scale (-1) 1 (needle1)

title = pictures [
        translate (-130)   150  (scale 0.5 0.5 (text "WELCOME")),
        translate  (-50)    70  (scale 0.5 0.5 (text "TO...")),
        translate (-170) (-100) (scale 0.5 0.5 (text "YO GRANDMA!"))
        ]

grandma angle = pictures [
        wheelChair angle,
        knitting,
        arms,
        legs,
        gmbody,
        skull
        ]

wheelChair angle = pictures [
        translate 0 0 (rotate angle (scale 0.3 0.3 (wheel))),
        footRest,
        seat,
        armRest
        ]

skull = pictures [
      hairStrand2,
      hairStrand1,
      hair,
      gmeye,
      translate (-5) 135 (color yellow(solidCircle 30))
      ]

gmbody = translate 0 70 (scale 1.15 1.17 (color violet (solidCircle 40)))

legs = pictures [
       translate   50    45 (color violet (scale 1.4 1 (solidCircle 15))),
       translate   75    40 (color violet (solidCircle 12)),
       translate   47   114 (color violet (solidCircle 11))
       ]

arms = pictures [
       translate  (-3)  100 (color (dark violet)              (solidCircle 18)),
       translate   15    86 (color (dark violet) (scale 1.5 1 (solidCircle 15))),
       translate   47   110 (color       violet               (solidCircle 11)),
       translate   38    98 (color (dark violet)              (solidCircle 11))
       ]

knitting = translate 65 85 (scale 0.7 0.7 (pictures [
           color (dark green) (solidCircle 30),
           translate (-10) 0 (rotate    45  (solidRectangle 5 100)),
           translate (-10) 0 (rotate (-170) (solidRectangle 5 100))
           ]))


{-parts for the wheel Chair are below-}

armRest = pictures [
        line [ (-70, 0), (-70,85)],
        line [ ( 50, 0), ( 50,85)],
        line [ (-70,85), ( 50,85)]
        ]

seat = pictures [
     translate (-50) 100 (solidRectangle  15 110),
     translate   10   30 (solidRectangle 140  10)
     ]

footRest = line [
         ( 80, 30),
         (100,-15),
         (125,-15),
         (130, -5)
         ]

wheel = pictures [
      spokes,
      tire,
      middle
      ]

spokes = pictures [
       line [(0,0), ( 200,   0)],
       line [(0,0), ( 150,-150)],
       line [(0,0), (-200,   0)],
       line [(0,0), (-150, 150)],
       line [(0,0), (   0, 200)],
       line [(0,0), ( 150, 150)],
       line [(0,0), (   0,-200)],
       line [(0,0), (-150,-150)]
       ]

tire = thickCircle 200 100

middle = solidCircle 20

{-parts for the skull are below-}
gmeye = pictures [
    translate  12  142 (color black (scale 2 1 (solidCircle 3))),
    translate  12  142 (color blue  (solidCircle 2.5)),
    translate  13  141 (color black (solidCircle 1.6))
    ]

hair = pictures [
     translate (-16) 150 (color (dark (dark cyan)) (scale 1.2 1(thickCircle 20 7))),
     translate (-16) 150 (color (dark (dark cyan)) (scale 1.2 1(thickCircle 13 7))),
     translate (-16) 150 (color (dark (dark cyan)) (scale 1.2 1(thickCircle  6 7))),
     translate (-16) 150 (color (dark (dark cyan)) (solidCircle 2.5))
     ]

hairStrand1 = color (dark (dark cyan))(line [
              (5,145),
              (8,136),
              (6,129),
              (9,119)
              ])

hairStrand2 = color (dark (dark cyan))(line [
              (-2,139),
              ( 1,131),
              ( 0,129),
              ( 0,125)
              ])

main = interactionOf initial step event draw
