{-
REQUIRES

Description: containsMatch contains success
Rules:
 - containsMatch:
    template: |
      f($var) = $(contains [| translated |])
   explanation: f should use translation in its definition.
-}

f(x) = translated(x, 1, 1)
program = drawingOf(f(codeWorldLogo))