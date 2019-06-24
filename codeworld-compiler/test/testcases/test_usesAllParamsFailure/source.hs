{-
REQUIRES

Description: usesAllParams failure
Rules:
 - usesAllParams: f
-}

f(x,y) = drawingOf(x)
program = f(codeWorldLogo, 90)