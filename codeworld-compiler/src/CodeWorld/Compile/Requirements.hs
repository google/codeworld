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

module CodeWorld.Compile.Requirements (
    Requirement,
    parseRequirement,
    evalRequirement
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Either
import Data.Generics hiding (empty)
import Data.Hashable
import Data.Void
import Language.Haskell.Exts hiding (Rule, parse)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Requirement = Requirement {
    requiredDescription :: String,
    requiredRules :: [Rule]
    }
    deriving (Show)

data Rule = DefinedByFunction String String
          | MatchesExpected String Int
          | HasSimpleParams String
    deriving (Show)

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

parseRequirement :: ByteString -> Either String Requirement
parseRequirement bs = either (Left . errorBundlePretty) Right $
    parse requirementParser "" (unpack bs)

evalRequirement :: Requirement -> Module SrcSpanInfo -> (String, Bool, [String])
evalRequirement Requirement{..} m =
    (requiredDescription, null failures, failures)
  where failures = concatMap (ruleFailures m) requiredRules

ruleFailures :: Module SrcSpanInfo -> Rule -> [String]
ruleFailures m (DefinedByFunction a b) = checkDefinedBy m a b
ruleFailures m (MatchesExpected a expectedHash) =
    checkMatchesExpected m a expectedHash
ruleFailures m (HasSimpleParams a) = checkHasSimpleParams m a

checkDefinedBy :: Module SrcSpanInfo -> String -> String -> [String]
checkDefinedBy m a b
  | null aDefs = ["`" ++ a ++ "` is not defined."]
  | null bDefs = ["`" ++ b ++ "` is not defined."]
  | all (isDefinedBy b) aDefs = []
  | otherwise = [ "`" ++ a ++ "` is not defined directly using `" ++ b ++ "`." ]
  where aDefs = allDefinitionsOf a m
        bDefs = allDefinitionsOf b m

        isDefinedBy :: String -> Rhs SrcSpanInfo -> Bool
        isDefinedBy b (UnGuardedRhs _ exp) = isExpOf b exp
        isDefinedBy b (GuardedRhss _ rhss) =
            all (\(GuardedRhs _ _ exp) -> isExpOf b exp) rhss

        isExpOf :: String -> Exp SrcSpanInfo -> Bool
        isExpOf b (Var _ (UnQual _ (Ident _ bb))) = b == bb
        isExpOf b (App _ exp _) = isExpOf b exp
        isExpOf b (Let _ _ exp) = isExpOf b exp
        isExpOf b (Paren _ exp) = isExpOf b exp
        isExpOf b _ = False

checkMatchesExpected :: Module SrcSpanInfo -> String -> Int -> [String]
checkMatchesExpected m a expectedHash
  | null defs = ["`" ++ a ++ "` is not defined."]
  | computedHash == expectedHash = []
  | otherwise = ["`" ++ a ++ "` does not have the expected definition. (" ++
                 show computedHash ++ ")"]
  where defs = allDefinitionsOf a m
        computedHash = hash (concatMap (show . eraseSrcLocs) defs) `mod` 1000000
        eraseSrcLocs rhs = everywhere (mkT (const noSrcSpan)) rhs

checkHasSimpleParams :: Module SrcSpanInfo -> String -> [String]
checkHasSimpleParams m a
  | null paramPatterns = ["`" ++ a ++ "` is not defined as a function."]
  | all isSimpleParam paramPatterns = []
  | otherwise = ["`" ++ a ++ "` has equations with pattern matching."]
  where paramPatterns = everything (++) (mkQ [] matchParams) m

        matchParams :: Match SrcSpanInfo -> [Pat SrcSpanInfo]
        matchParams (Match _ (Ident _ aa) pats _ _) | a == aa = pats
        matchParams _ = []

        isSimpleParam :: Pat SrcSpanInfo -> Bool
        isSimpleParam (PVar _ (Ident _ nm)) = isLower (head nm)
        isSimpleParam (PTuple _ _ pats) = all isSimpleParam pats
        isSimpleParam (PParen _ pat) = isSimpleParam pat
        isSimpleParam (PWildCard _) = True
        isSimpleParam _ = False

allDefinitionsOf :: String -> Module SrcSpanInfo -> [Rhs SrcSpanInfo]
allDefinitionsOf a m = everything (++) (mkQ [] funcDefs) m ++
                       everything (++) (mkQ [] patDefs) m
  where funcDefs :: Match SrcSpanInfo -> [Rhs SrcSpanInfo]
        funcDefs (Match _ (Ident _ aa) _ rhs _) | a == aa = [rhs]
        funcDefs _ = []

        patDefs :: Decl SrcSpanInfo -> [Rhs SrcSpanInfo]
        patDefs (PatBind _ pat rhs _) | patDefines pat a = [rhs]
        patDefs _ = []

hasDefinition :: Module SrcSpanInfo -> String -> Bool
hasDefinition m a = everything (||) (mkQ False isFuncDef) m ||
                    everything (||) (mkQ False isPatternDef) m
  where isFuncDef :: Match SrcSpanInfo -> Bool
        isFuncDef (Match _ (Ident _ aa) _ _ _) = a == aa

        isPatternDef :: Decl SrcSpanInfo -> Bool
        isPatternDef (PatBind _ pat _ _) = patDefines pat a

patDefines :: Pat SrcSpanInfo -> String -> Bool
patDefines (PVar _ (Ident _ aa)) a = a == aa
patDefines (PParen _ pat) a = patDefines pat a
