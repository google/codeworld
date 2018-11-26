{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module CodeWorld.Compile.Stages (runCustomDiagnostics, hasOldStyleMain) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Generics
import Data.Maybe (isJust)
import Data.Monoid
import Data.List (sort)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.Exts
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

hasOldStyleMain :: Text -> Bool
hasOldStyleMain src = isJust (findOldStyleMain src)

runCustomDiagnostics :: MonadCompile m => m ()
runCustomDiagnostics = do
    checkDangerousSource
    checkOldStyleMain
    checkFunctionParentheses
    checkVarlessPatterns
    checkRequirements

-- Look for uses of Template Haskell or related features in the compiler.  These
-- cannot currently be used, because the compiler isn't properly sandboxed, so
-- this would be a remote code execution vulnerability.
--
-- At the moment, we don't look in the parsed code, but instead match
-- regular expressions against the source.  That's because I don't quite
-- trust haskell-src-exts to correctly parse the source.
checkDangerousSource :: MonadCompile m => m ()
checkDangerousSource = do
    src <- decodeUtf8 <$> getSourceCode
    when (src =~ (".*TemplateHaskell.*" :: Text) ||
          src =~ (".*QuasiQuotes.*" :: Text) ||
          src =~ (".*glasgow-exts.*" :: Text)) $ do
        addDiagnostics
            [ (noSrcSpan, Error,
               "Sorry, but your program uses forbidden language features.")
            ]

-- Looks for uses of old-style main in CodeWorld-mode modules.  These
-- will be broken soon, so we issue a warning.
checkOldStyleMain :: MonadCompile m => m ()
checkOldStyleMain = do
    mode <- gets compileMode
    src <- decodeUtf8 <$> getSourceCode
    case (mode, findOldStyleMain src) of
        ("codeworld", Just (off, len)) -> do
            addDiagnostics
                [ (srcSpanFor src off len, Warning,
                   "warning:\n" ++
                   "\tPlease define 'program' instead of 'main'.\n" ++
                   "\tDefining 'main' may stop working July 2019.")
                ]
        _ -> return ()

findOldStyleMain :: Text -> Maybe (Int, Int)
findOldStyleMain src
  | rangeSize (bounds matchArray) > 2 = Just (matchArray ! 2)
  | otherwise = Nothing
  where matchArray :: MatchArray = src =~ ("(^|\\n)(main)[ \\t]*=" :: Text)

checkFunctionParentheses :: MonadCompile m => m ()
checkFunctionParentheses = do
    mode <- gets compileMode
    parsed <- getParsedCode
    let diags = case (mode, parsed) of
            ("codeworld", Parsed mod) ->
                everything (++) (mkQ [] badExpApps) mod ++
                everything (++) (mkQ [] badMatchApps) mod ++
                everything (++) (mkQ [] badPatternApps) mod
            _ -> []  -- Fall back on GHC for user-visible parse errors.
    addDiagnostics diags

badExpApps :: Exp SrcSpanInfo -> [Diagnostic]
badExpApps (App loc _ e)
    | not (isGoodExpAppRhs e) = [(loc, Warning, errorMsg)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badExpApps _ = []

badMatchApps :: Match SrcSpanInfo -> [Diagnostic]
badMatchApps (Match loc _ pats _ _) =
    [(loc, Warning, errorMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badMatchApps _ = []

badPatternApps :: Pat SrcSpanInfo -> [Diagnostic]
badPatternApps (PApp loc _ pats) =
    [(loc, Warning, errorMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    errorMsg = "warning: Missing parentheses in constructor application."
badPatternApps _ = []

isGoodExpAppRhs :: Exp l -> Bool
isGoodExpAppRhs (Paren _ _) = True
isGoodExpAppRhs (Tuple _ _ _) = True
isGoodExpAppRhs _ = False

isGoodPatAppRhs :: Pat l -> Bool
isGoodPatAppRhs (PParen _ _) = True
isGoodPatAppRhs (PTuple _ _ _) = True
isGoodPatAppRhs _ = False

checkVarlessPatterns :: MonadCompile m => m ()
checkVarlessPatterns = do
    mode <- gets compileMode
    parsed <- getParsedCode
    let diags = case (mode, parsed) of
            ("codeworld", Parsed mod) ->
                everything (++) (mkQ [] varlessPatBinds) mod
            _ -> []
    addDiagnostics diags

varlessPatBinds :: Decl SrcSpanInfo -> [Diagnostic]
varlessPatBinds (PatBind loc pat _ _)
  | not (everything (||) (mkQ False isPatVar) pat) = [(loc, Error, errorMsg)]
  where errorMsg = "This definition doesn't define any variables.\n\t" ++
                   "Variables must begin with a lowercase letter."
varlessPatBinds _ = []

isPatVar :: Pat SrcSpanInfo -> Bool
isPatVar (PVar _ _) = True
isPatVar (PNPlusK _ _ _) = True
isPatVar (PAsPat _ _ _) = True
isPatVar _ = False
