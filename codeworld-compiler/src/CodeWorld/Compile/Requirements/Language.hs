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

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.Types
import Data.Aeson
import Data.Aeson.Types (explicitParseFieldMaybe)
import qualified Data.Aeson.Types as Aeson
import Data.Char
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void
import qualified Data.Yaml as Yaml
import Language.Haskell.Exts.SrcLoc
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

instance FromJSON Requirement where
    parseJSON = withObject "Requirement" $ \v ->
        Requirement <$> v .: "description"
                    <*> v .: "rules"

instance FromJSON Rule where
    parseJSON = withObject "Rule" $ \o -> do
        choices <- sequence
            [ explicitParseFieldMaybe definedByFunction o "definedByFunction"
            , explicitParseFieldMaybe matchesExpected o "matchesExpected"
            , explicitParseFieldMaybe hasSimpleParams o "hasSimpleParams"
            , explicitParseFieldMaybe usesAllParams o "usesAllParams"
            , explicitParseFieldMaybe notDefined o "notDefined"
            , explicitParseFieldMaybe notUsed o "notUsed"
            ]
        case asum choices of
            Just r  -> return r
            Nothing -> fail "Unrecognized rule type."

definedByFunction :: Aeson.Value -> Aeson.Parser Rule
definedByFunction = withObject "definedByFunction" $ \o ->
    DefinedByFunction <$> o .: "variable"
                      <*> o .: "function"

matchesExpected :: Aeson.Value -> Aeson.Parser Rule
matchesExpected = withObject "matchesExpected" $ \o ->
    MatchesExpected <$> o .: "variable"
                    <*> o .: "hash"

hasSimpleParams :: Aeson.Value -> Aeson.Parser Rule
hasSimpleParams = withText "hasSimpleParams" $ \t ->
    return $ HasSimpleParams $ T.unpack t

usesAllParams :: Aeson.Value -> Aeson.Parser Rule
usesAllParams = withText "usesAllParams" $ \t ->
    return $ UsesAllParams $ T.unpack t

notDefined :: Aeson.Value -> Aeson.Parser Rule
notDefined = withText "notDefined" $ \t ->
    return $ NotDefined $ T.unpack t

notUsed :: Aeson.Value -> Aeson.Parser Rule
notUsed = withText "notUsed" $ \t ->
    return $ NotUsed $ T.unpack t

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

legacyFormatPattern :: Text
legacyFormatPattern = "^[[:space:]]*\"[^\\n]*\".*"

parseRequirement :: Int -> Int -> Text -> Either String Requirement
parseRequirement ln col txt
  | txt =~ legacyFormatPattern = either (Left . errorBundlePretty) Right $
        snd $ runParser' legacyRequirementParser initialState
  | otherwise = either (Left . prettyPrintYamlParseException ln col) Right $
        Yaml.decodeEither' (T.encodeUtf8 txt)
  where str = T.unpack txt
        initialState = State str 0 posState
        posState = PosState str 0 srcPos (mkPos 8) (replicate (col - 1) ' ')
        srcPos = SourcePos "program.hs" (mkPos ln) (mkPos col)

prettyPrintYamlParseException ln col e =
    formatLocation srcSpan ++ Yaml.prettyPrintParseException e
  where srcSpan = SrcSpanInfo loc []
        loc     = SrcSpan "program.hs" ln col ln col
