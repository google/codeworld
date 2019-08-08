{-
REQUIRES

Description: containsMatch any failure
Rules:
 - containsMatch:
    template: |
      _func_ $any = $any
    cardinality:
      atLeast: 4
   explanation: The code defines fewer than 4 functions.
-}

program = drawingOf(codeWorldLogo)