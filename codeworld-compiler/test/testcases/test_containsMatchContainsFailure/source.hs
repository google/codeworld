{-
REQUIRES

Description: containsMatch contains failure
Rules:
 - containsMatch:
    template: |
      f($var) = $(contains [| translated |])
   explanation: f should use translation in its definition.
-}

f(x) = rotated(x, 90)
program = drawingOf(f(codeWorldLogo))