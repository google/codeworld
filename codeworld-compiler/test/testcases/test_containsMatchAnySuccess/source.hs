{-
REQUIRES

Description: containsMatch any success
Rules:
 - containsMatch:
    template: |
      _func_ $any = $any
    cardinality:
      atLeast: 4
   explanation: The code defines fewer than 4 functions.
-}

f(x) = codeWorldLogo
g(x) = f(x)
h(x) = g(x)
i(x) = h(x)
program = drawingOf(i(0))