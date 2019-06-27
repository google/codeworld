{-
REQUIRES

Description: usesAllParams success
Rules:
 - usesAllParams: f
-}

f(x,y) = drawingOf(rotated(x,y))
program = f(codeWorldLogo, 90)