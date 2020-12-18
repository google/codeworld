{-# LANGUAGE RecursiveDo #-}

import CodeWorld.Reflex
import Reflex

main =
  reflexOf $
    do rec {}
      draw
      (constDyn (circle 1))
