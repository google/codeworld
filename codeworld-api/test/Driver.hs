{-# LANGUAGE OverloadedStrings #-}

module Driver (
    tests
    ) where

import Test.Framework (Test, testGroup)
import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)
import CodeWorld
import CodeWorld.Driver
import CodeWorld.Event
import CodeWorld.CanvasM
import System.Mem.StableName
import Control.Concurrent


tests :: Test
tests = testGroup "Driver" [
    testCase "time step elision" timeStepElision
    ]

timeStepElision :: Assertion
timeStepElision = do
    let initial = ()
    state <- newMVar initial
    initialName <- makeStableName $! initial
    _ <- run state (const id) (const id) (\_ -> pictureToDrawing $ solidCircle 10)
            (Right . TimePassing) (NSteps 0 5)
    current <- readMVar state
    currentName <- makeStableName $! current
    assertBool "state is not changed by time steps" (currentName == initialName)