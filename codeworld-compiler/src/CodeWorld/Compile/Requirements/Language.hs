{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.Compile.Requirements.Language (
    Requirement,
    parseRequirement
    ) where

import CodeWorld.Compile.Requirements.Types
import Data.Char
import Data.Either
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

requirementParser :: Parser Requirement
requirementParser = do
    lexeme (symbol "REQUIRES")
    doc <- between quote quote (many nonquote)
    rules <- many ruleParser
    eof
    return (Requirement doc rules)

ruleParser :: Parser Rule
ruleParser = definedByParser <|> matchesExpectedParser <|> simpleParamsParser

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

parseRequirement :: Text -> Either String Requirement
parseRequirement bs = either (Left . errorBundlePretty) Right $
    parse requirementParser "" (unpack bs)
