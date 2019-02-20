{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module CodeWorld.Compile.Stages
    ( hasOldStyleMain
    , checkDangerousSource
    , checkCodeConventions
    , checkRequirements
    ) where

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

-- Checks a full list of conventions that are enforced by the CodeWorld
-- compiler for "codeworld" mode.  In other modes, this has no effect.
checkCodeConventions :: MonadCompile m => m ()
checkCodeConventions = do
    mode <- gets compileMode
    checkOldStyleMixed mode
    when (mode == "codeworld") $ do
        checkOldStyleMain
        checkFunctionParentheses
        checkVarlessPatterns
        checkPatternGuards

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
            [ (noSrcSpan, CompileError,
               "error: Sorry, but your program uses forbidden language features.")
            ]

-- Looks for uses of old-style main in CodeWorld-mode modules.  These
-- will be broken soon, so we issue a warning.
checkOldStyleMain :: MonadCompile m => m ()
checkOldStyleMain = do
    src <- decodeUtf8 <$> getSourceCode
    case findOldStyleMain src of
        Just (off, len) -> do
            addDiagnostics
                [ (srcSpanFor src off len, CompileSuccess,
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

-- Looks for use of `mixed` with either a pair of colors (in CodeWorld mode) or
-- two colors (in Haskell mode).  This is likely to be old code from before the
-- type signature was changed, so there's a custom error message.
checkOldStyleMixed :: MonadCompile m => SourceMode -> m ()
checkOldStyleMixed mode =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] (oldStyleMixed mode)) mod
        _ -> return ()
  where oldStyleMixed :: SourceMode -> Exp SrcSpanInfo -> [Diagnostic]
        oldStyleMixed "codeworld"
            (App loc (Var _ (UnQual _ (Ident _ "mixed")))
                     (Tuple _ _ [_, _]))
            = [(loc, CompileError,
                "error: Outdated use of mixed function." ++
                "\n    The argument should be a list of colors." ++
                "\n    Example: mixed([red, orange, white])")]
        oldStyleMixed "haskell"
            (App loc (App _ (Var _ (UnQual _ (Ident _ "mixed"))) _) _)
            = [(loc, CompileError,
                "error: Outdated use of mixed function." ++
                "\n    The argument should be a list of colors." ++
                "\n    Example: mixed [red, orange, white]")]
        oldStyleMixed _ _ = []

-- Look for function applications without parentheses.  Since CodeWorld
-- functions are usually applied with parentheses, this often indicates a
-- missing piece of punctuation, such as an operator or comma, or misplaced
-- parentheses.
checkFunctionParentheses :: MonadCompile m => m ()
checkFunctionParentheses =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            dedupErrorSpans (everything (++) (mkQ [] badExpApps) mod) ++
            everything (++) (mkQ [] badMatchApps) mod ++
            everything (++) (mkQ [] badPatternApps) mod
        _ -> return ()  -- Fall back on GHC for parse errors.

dedupErrorSpans :: [Diagnostic] -> [Diagnostic]
dedupErrorSpans [] = []
dedupErrorSpans [err] = [err]
dedupErrorSpans ((loc1, sev1, msg1) : (loc2, sev2, msg2) : errs)
  | loc1 `contains` loc2 = dedupErrorSpans ((loc1, sev1, msg1) : errs)
  | otherwise = (loc1, sev1, msg1) : dedupErrorSpans ((loc2, sev2, msg2) : errs)
  where
    SrcSpanInfo {srcInfoSpan = span1} `contains` SrcSpanInfo {srcInfoSpan = span2} =
        srcSpanFilename span1 == srcSpanFilename span2 &&
        (srcSpanStartLine span1 < srcSpanStartLine span2 ||
         (srcSpanStartLine span1 == srcSpanStartLine span2 &&
          srcSpanStartColumn span1 <= srcSpanStartColumn span2)) &&
        (srcSpanEndLine span1 > srcSpanEndLine span2 ||
         (srcSpanEndLine span1 == srcSpanEndLine span2 &&
          srcSpanEndColumn span1 >= srcSpanEndColumn span2))

badExpApps :: Exp SrcSpanInfo -> [Diagnostic]
badExpApps (App loc lhs rhs)
    | not (isGoodExpAppLhs lhs) = [(ann rhs, CompileError, errorMsg)]
    | not (isGoodExpAppRhs rhs) = [(ann rhs, CompileSuccess, warningMsg)]
  where
    errorMsg = "error: " ++ missingParenError ++ missingParenMultiplySuggestion
    warningMsg = "warning: " ++ missingParenError ++ missingParenMultiplySuggestion ++ missingParenFunctionSuggestion
badExpApps _ = []

badMatchApps :: Match SrcSpanInfo -> [Diagnostic]
badMatchApps (Match loc _ pats _ _) =
    take 1 [(ann p, CompileSuccess, warningMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    warningMsg = "warning: " ++ missingParenError ++ missingParenFunctionSuggestion
badMatchApps _ = []

badPatternApps :: Pat SrcSpanInfo -> [Diagnostic]
badPatternApps (PApp loc _ pats) =
    take 1 [(ann p, CompileSuccess, warningMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    warningMsg = "warning: " ++ missingParenError ++ missingParenFunctionSuggestion
badPatternApps _ = []

missingParenError :: String
missingParenError =
    "Missing punctuation before this expression." ++
    "\n    Perhaps you forgot a comma, an operator, or a bracket."

missingParenMultiplySuggestion :: String
missingParenMultiplySuggestion =
    "\n    \x2022 To multiply expressions, please use the * operator."

missingParenFunctionSuggestion :: String
missingParenFunctionSuggestion =
    "\n    \x2022 To apply a function, add parentheses around the arguments." ++
    "\n    (Will assume that function arguments were intended.)"

isGoodExpAppLhs :: Exp l -> Bool
isGoodExpAppLhs (Lit _ _) = False
isGoodExpAppLhs (NegApp _ _) = False
isGoodExpAppLhs (Tuple _ _ _) = False
isGoodExpAppLhs (UnboxedSum _ _ _ _) = False
isGoodExpAppLhs (List _ _) = False
isGoodExpAppLhs (ParArray _ _) = False
isGoodExpAppLhs (RecConstr _ _ _) = False
isGoodExpAppLhs (RecUpdate _ _ _) = False
isGoodExpAppLhs (EnumFrom _ _) = False
isGoodExpAppLhs (EnumFromTo _ _ _) = False
isGoodExpAppLhs (EnumFromThen _ _ _) = False
isGoodExpAppLhs (EnumFromThenTo _ _ _ _) = False
isGoodExpAppLhs (ParArrayFromTo _ _ _) = False
isGoodExpAppLhs (ParArrayFromThenTo _ _ _ _) = False
isGoodExpAppLhs (ListComp _ _ _) = False
isGoodExpAppLhs (ParComp _ _ _) = False
isGoodExpAppLhs (ParArrayComp _ _ _) = False
isGoodExpAppLhs (VarQuote _ _) = False
isGoodExpAppLhs (TypQuote _ _) = False
isGoodExpAppLhs _ = True

isGoodExpAppRhs :: Exp l -> Bool
isGoodExpAppRhs (Paren _ _) = True
isGoodExpAppRhs (Tuple _ _ _) = True
isGoodExpAppRhs _ = False

isGoodPatAppRhs :: Pat l -> Bool
isGoodPatAppRhs (PParen _ _) = True
isGoodPatAppRhs (PTuple _ _ _) = True
isGoodPatAppRhs _ = False

checkVarlessPatterns :: MonadCompile m => m ()
checkVarlessPatterns =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] varlessPatBinds) mod
        _ -> return ()

varlessPatBinds :: Decl SrcSpanInfo -> [Diagnostic]
varlessPatBinds (PatBind loc pat _ _)
  | not (everything (||) (mkQ False isPatVar) pat)
    = [(loc, CompileError,
        "error: This definition doesn't define any variables.\n\t" ++
        "Variables must begin with a lowercase letter.")]
varlessPatBinds _ = []

isPatVar :: Pat SrcSpanInfo -> Bool
isPatVar (PVar _ _) = True
isPatVar (PNPlusK _ _ _) = True
isPatVar (PAsPat _ _ _) = True
isPatVar _ = False

checkPatternGuards :: MonadCompile m => m ()
checkPatternGuards =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] patternGuards) mod
        _ -> return ()

patternGuards :: GuardedRhs SrcSpanInfo -> [Diagnostic]
patternGuards (GuardedRhs _ stmts _) =
    [ (loc, CompileError,
       "error: This arrow can't be used here.\n\t" ++
       "To compare a negative number, add a space between < and -.")
      | Generator loc _ _ <- stmts ]
