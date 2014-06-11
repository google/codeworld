{-
    Example: Bounce

    In this example, a ball bounces around the screen.  The ball starts with
    a random position and velocity, and bounces when it hits the edge of the
    screen.
-}

main = simulationOf initial step draw

data World = Ball Point Vector

radius = 40
border = 250 - radius

initial (x:y:vx:vy:_) = Ball (400*x  - 200, 400*y  - 200)
                             (400*vx - 200, 400*vy - 200)

step t world = bounce (move t world)

move t (Ball (x,y) (vx,vy)) = Ball (x + vx*t, y + vy*t) (vx, vy)

bounce (Ball (x,y) (vx,vy)) = Ball (nx,ny) (nvx, nvy)
  where nx  = fence (-border) border x
        ny  = fence (-border) border y
        nvx = if nx /= x then -vx else vx
        nvy = if ny /= y then -vy else vy

fence lo hi x | x < lo    = lo + (lo - x)
              | x > hi    = hi - (x - hi)
              | otherwise = x

draw (Ball (x,y) _) = translate x y (solidCircle radius)
