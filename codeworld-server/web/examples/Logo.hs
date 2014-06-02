{-
  Example: Logo

  A simple animated logo built out of geometric patterns.
-}
main       = animationOf design
design   t = pictures [ rotate (20*t) center, border ]
center     = pictures [ rotate  0 (color (muted red)   centerPart),
                        rotate 10 (color (muted green) centerPart),
                        rotate 20 (color (muted blue)  centerPart) ]
centerPart = pictures [ rotate t (scale 0.3 1 (circle 100)) | t <- [0, 30 .. 360] ]
border     = pictures [ rotate r corner | r <- [0, 90, 180, 270] ]
corner     = pictures [ diag x | x <- [0, 20 .. 500] ]
diag     x = line [(-250, 250-x), (x - 250, -250)]
