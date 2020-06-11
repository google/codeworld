{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

module CodeWorld.Compile.Requirements.Types where

data Requirement = Requirement
  { requiredDescription :: String,
    requiredRules :: [Rule]
  }
  deriving (Show)

data Rule
  = DefinedByFunction String String
  | MatchesExpected String Int
  | HasSimpleParams String
  | UsesAllParams String
  | NotDefined String
  | NotUsed String
  | ContainsMatch
      { matchTemplate :: String,
        matchTopLevel :: Bool,
        matchCardinality :: Cardinality
      }
  | MatchesRegex
      { regexPattern :: String,
        regexCardinality :: Cardinality
      }
  | OnFailure String Rule
  | IfThen Rule Rule
  | AllOf [Rule]
  | AnyOf [Rule]
  | NotThis Rule
  | MaxLineLength Int
  | NoWarningsExcept [String]
  | TypeSignatures Bool
  | Blacklist [String]
  | Whitelist [String]
  deriving (Show)

data Cardinality = Cardinality
  { atLeast :: Maybe Int,
    atMost :: Maybe Int
  }
  deriving (Show)

anyNumber, exactlyOne, atLeastOne :: Cardinality
anyNumber = Cardinality Nothing Nothing
exactlyOne = Cardinality (Just 1) (Just 1)
atLeastOne = Cardinality (Just 1) Nothing

hasCardinality :: Cardinality -> Int -> Bool
hasCardinality (Cardinality (Just k) _) n | n < k = False
hasCardinality (Cardinality _ (Just k)) n | n > k = False
hasCardinality _ _ = True
