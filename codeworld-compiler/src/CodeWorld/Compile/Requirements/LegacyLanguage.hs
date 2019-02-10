{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.Compile.Requirements.LegacyLanguage (
    isLegacyFormat,
    parseLegacyRequirement
    ) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.Types
import Data.Char
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

-----------------------------------------------------------------
--                             WARNING!
--
-- This module defines a legacy parser for an old requirements
-- format.  Usually, it should NOT be updated when new rules
-- types are added.  Instead, please add them to
-- CodeWorld.Compile.Requirements.Language so that they have
-- a YAML-based format.
-----------------------------------------------------------------

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

quote :: Parser Char
quote = lexeme (char '\"')

nonquote :: Parser Char
nonquote = anySingleBut '\"'

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

integer :: Parser Int
integer = lexeme L.decimal

legacyRequirementParser :: Parser Requirement
legacyRequirementParser = do
    optional ws
    optional (symbol "REQUIRES")
    doc <- between quote quote (many nonquote)
    rules <- many ruleParser
    eof
    return (Requirement doc rules)

ruleParser :: Parser Rule
ruleParser = definedByParser <|>
             matchesExpectedParser <|>
             simpleParamsParser <|>
             usesAllParamsParser <|>
             notDefinedParser <|>
             notUsedParser

definedByParser :: Parser Rule
definedByParser = do
    symbol "definedByFunction"
    symbol "("
    a <- identifier
    symbol ","
    b <- identifier
    symbol ")"
    return (DefinedByFunction a b)

matchesExpectedParser :: Parser Rule
matchesExpectedParser = do
    symbol "matchesExpected"
    symbol "("
    a <- identifier
    symbol ","
    expectedHash <- integer
    symbol ")"
    return (MatchesExpected a expectedHash)

simpleParamsParser :: Parser Rule
simpleParamsParser = do
    symbol "hasSimpleParams"
    symbol "("
    a <- identifier
    symbol ")"
    return (HasSimpleParams a)

usesAllParamsParser :: Parser Rule
usesAllParamsParser = do
    symbol "usesAllParams"
    symbol "("
    a <- identifier
    symbol ")"
    return (UsesAllParams a)

notDefinedParser :: Parser Rule
notDefinedParser = do
    symbol "notDefined"
    symbol "("
    a <- identifier
    symbol ")"
    return (NotDefined a)

notUsedParser :: Parser Rule
notUsedParser = do
    symbol "notUsed"
    symbol "("
    a <- identifier
    symbol ")"
    return (NotUsed a)

isLegacyFormat :: Text -> Bool
isLegacyFormat txt =
    txt =~ ("^[[:space:]]*(REQUIRES)?[[:space:]]*\"[^\n]*\".*" :: Text)

parseLegacyRequirement :: Int -> Int -> Text -> Either String Requirement
parseLegacyRequirement ln col txt =
    either (Left . errorBundlePretty) Right $
        snd $ runParser' legacyRequirementParser initialState
  where str = T.unpack txt
        initialState = State str 0 posState
        posState = PosState str 0 srcPos (mkPos 8) (replicate (col - 1) ' ')
        srcPos = SourcePos "program.hs" (mkPos ln) (mkPos col)
