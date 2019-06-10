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

module CodeWorld.Compile.Requirements.Language (parseRequirement) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.LegacyLanguage
import CodeWorld.Compile.Requirements.Types
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (explicitParseFieldMaybe)
import qualified Data.Aeson.Types as Aeson
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import Language.Haskell.Exts.SrcLoc

instance FromJSON Requirement where
    parseJSON = withObject "Requirement" $ \v ->
        Requirement <$> v .: "Description"
                    <*> v .: "Rules"

instance FromJSON Rule where
    parseJSON = withObject "Rule" $ \o -> do
        choices <- sequence
            [ explicitParseFieldMaybe definedByFunction o "definedByFunction"
            , explicitParseFieldMaybe matchesExpected o "matchesExpected"
            , explicitParseFieldMaybe hasSimpleParams o "hasSimpleParams"
            , explicitParseFieldMaybe usesAllParams o "usesAllParams"
            , explicitParseFieldMaybe notDefined o "notDefined"
            , explicitParseFieldMaybe notUsed o "notUsed"
            , explicitParseFieldMaybe containsMatch o "containsMatch"
            , explicitParseFieldMaybe matchesRegex o "matchesRegex"
            , explicitParseFieldMaybe ifThen o "ifThen"
            , explicitParseFieldMaybe allOf o "all"
            , explicitParseFieldMaybe anyOf o "any"
            , explicitParseFieldMaybe notThis o "not"
            , explicitParseFieldMaybe maxLineLength o "maxLineLength"
            , explicitParseFieldMaybe noWarningsExcept o "noWarningsExcept"
            , explicitParseFieldMaybe typeDeclarations o "typeDeclarations"
            , explicitParseFieldMaybe blacklist o "blacklist"
            , explicitParseFieldMaybe whitelist o "whitelist"
            ]
        case catMaybes choices of
            [r] -> decorateWith o r
            []  -> fail "No recognized rule type was defined."
            _   -> fail "More than one type was found for a single rule."

decorateWith :: Aeson.Object -> Rule -> Aeson.Parser Rule
decorateWith obj = wrapCustomMessage
  where wrapCustomMessage rule = do
            msg <- obj .:? "explanation"
            case msg of Just str -> return (OnFailure str rule)
                        _        -> return rule

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

containsMatch :: Aeson.Value -> Aeson.Parser Rule
containsMatch = withObject "containsMatch" $ \o ->
    ContainsMatch <$> o .: "template"
                  <*> o .:? "topLevel" .!= True
                  <*> o .:? "cardinality" .!= atLeastOne

matchesRegex :: Aeson.Value -> Aeson.Parser Rule
matchesRegex = withObject "matchesRegex" $ \o ->
    MatchesRegex <$> o .: "pattern"
                 <*> o .:? "cardinality" .!= atLeastOne

ifThen :: Aeson.Value -> Aeson.Parser Rule
ifThen = withObject "ifThen" $ \o ->
    OnFailure <$> o .: "if"
              <*> o .: "then"

allOf :: Aeson.Value -> Aeson.Parser Rule
allOf v = AllOf <$> withArray "all" (mapM parseJSON . toList) v

anyOf :: Aeson.Value -> Aeson.Parser Rule
anyOf v = AnyOf <$> withArray "any" (mapM parseJSON . toList) v

notThis :: Aeson.Value -> Aeson.Parser Rule
notThis v = NotThis <$> parseJSON v

maxLineLength :: Aeson.Value -> Aeson.Parser Rule
maxLineLength v = MaxLineLength <$> parseJSON v

noWarningsExcept :: Aeson.Value -> Aeson.Parser Rule
noWarningsExcept v = NoWarningsExcept <$> withArray "exceptions" (mapM parseJSON . toList) v

typeDeclarations :: Aeson.Value -> Aeson.Parser Rule
typeDeclarations v = TypeDeclarations <$> parseJSON v

blacklist :: Aeson.Value -> Aeson.Parser Rule
blacklist v = Blacklist <$> withArray "blacklist" (mapM parseJSON . toList) v

whitelist :: Aeson.Value -> Aeson.Parser Rule
whitelist v = Whitelist <$> withArray "whitelist" (mapM parseJSON . toList) v

instance FromJSON Cardinality where
    parseJSON val = parseAsNum val <|> parseAsObj val
      where parseAsNum val = do
                n <- parseJSON val
                return (Cardinality (Just n) (Just n))
            parseAsObj = withObject "cardinality" $ \o -> do
                exactly <- o .:? "exactly"
                mini <- o .:? "atLeast"
                maxi <- o .:? "atMost"
                case (exactly, mini, maxi) of
                    (Just n, Nothing, Nothing) ->
                        return (Cardinality (Just n) (Just n))
                    (Nothing, Nothing, Nothing) ->
                        fail "Missing cardinality"
                    (Nothing, m, n) ->
                        return (Cardinality m n)

parseRequirement :: Int -> Int -> Text -> Either String Requirement
parseRequirement ln col txt
  | isLegacyFormat txt = parseLegacyRequirement ln col txt
  | otherwise = either (Left . prettyPrintYamlParseException ln col) Right $
        Yaml.decodeEither' (T.encodeUtf8 txt)

prettyPrintYamlParseException ln col e =
    formatLocation srcSpan ++ Yaml.prettyPrintParseException e
  where srcSpan = SrcSpanInfo loc []
        loc     = SrcSpan "program.hs" ln col ln col
