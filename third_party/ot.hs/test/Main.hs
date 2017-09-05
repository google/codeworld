module Main (main) where

import Test.Framework

import qualified Control.OperationalTransformation.Text.Tests
import qualified Control.OperationalTransformation.Selection.Tests
import qualified Control.OperationalTransformation.ClientServerTests

main :: IO ()
main = defaultMain
  [ Control.OperationalTransformation.Text.Tests.tests
  , Control.OperationalTransformation.Selection.Tests.tests
  , Control.OperationalTransformation.ClientServerTests.tests
  ]