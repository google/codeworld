{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
    ( checkDangerousSource
    , checkCodeConventions
    ) where

import CodeWorld.Compile.Framework
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Function (on)
import Data.Generics
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.Exts
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

-- Checks a full list of conventions that are enforced by the CodeWorld
-- compiler for "codeworld" mode.  In other modes, this has no effect.
checkCodeConventions :: MonadCompile m => m ()
checkCodeConventions = do
    mode <- gets compileMode
    checkOldStyleMixed mode
    checkOldStyleGray
    when (mode == "codeworld") $ do
        checkExcludedSyntax
        checkFunctionParentheses
        checkVarlessPatterns
        checkPatternGuards
        checkExtraCommas
        checkNoncontiguousEquations

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

-- Look for excluded syntax.  In CodeWorld mode, students are not intended to
-- use some Haskell-specific features, such as do-blocks and character literals.
checkExcludedSyntax :: MonadCompile m => m ()
checkExcludedSyntax =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $ dedupErrorSpans $
            everything (++) (mkQ [] checkExps) mod ++
            everything (++) (mkQ [] checkPats) mod
        _ -> return ()
  where checkExps :: Exp SrcSpanInfo -> [Diagnostic]
        checkExps (Do loc _)                    = [(loc, CompileError, doBlockMsg)]
        checkExps (Lit loc (Char _ _ _))        = [(loc, CompileError, charLiteralMsg)]
        checkExps (Lit loc (PrimChar _ _ _))    = [(loc, CompileError, charLiteralMsg)]
        checkExps (VarQuote loc _)              = [(loc, CompileError, charLiteralMsg)]
        checkExps (TypQuote loc _)              = [(loc, CompileError, charLiteralMsg)]
        checkExps _                             = []

        checkPats :: Pat SrcSpanInfo -> [Diagnostic]
        checkPats (PLit loc _ (Char _ _ _))     = [(loc, CompileError, charLiteralMsg)]
        checkPats (PLit loc _ (PrimChar _ _ _)) = [(loc, CompileError, charLiteralMsg)]
        checkPats _                             = []

        doBlockMsg = "error:\n" ++
            "    The word do should not be used in your code.\n" ++
            "    If this is a variable name, please choose another name."
        charLiteralMsg = "error:\n" ++
            "    Text should be written with double quotes, not single quotes."

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

-- Looks for use of `gray` or `grey` with an argument.  This is likely to be old
-- code from before the type signature was changed, so there's a custom error
-- message.
checkOldStyleGray :: MonadCompile m => m ()
checkOldStyleGray =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] oldStyleGray) mod
        _ -> return ()
  where oldStyleGray :: Exp SrcSpanInfo -> [Diagnostic]
        oldStyleGray
            (App loc (Var _ (UnQual _ (Ident _ "gray"))) _)
            = [(loc, CompileError,
                "error: Outdated use of gray as a function." ++
                "\n    Remove the function argument for a medium shade of gray." ++
                "\n    For a different shade of gray, use light, dark, or HSL.")]
        oldStyleGray
            (App loc (Var _ (UnQual _ (Ident _ "grey"))) _)
            = [(loc, CompileError,
                "error: Outdated use of grey as a function." ++
                "\n    Remove the function argument for a medium shade of grey." ++
                "\n    For a different shade of gray, use light, dark, or HSL.")]
        oldStyleGray _ = []

-- Look for functions whose equations are not contiguous.
checkNoncontiguousEquations :: MonadCompile m => m ()
checkNoncontiguousEquations =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] checkModule) mod ++
            everything (++) (mkQ [] checkBinds) mod
        _ -> return ()
  where checkModule :: Module SrcSpanInfo -> [Diagnostic]
        checkModule (Module _ _ _ _ decls) = checkDeclList decls
        checkModule _ = []

        checkBinds :: Binds SrcSpanInfo -> [Diagnostic]
        checkBinds (BDecls _ decls) = checkDeclList decls
        checkBinds _ = []

        checkDeclList :: [Decl SrcSpanInfo] -> [Diagnostic]
        checkDeclList decls = map toMessage $
            map (\elems -> (fst (head elems), map snd elems)) $
            filter ((> 1) . length) $
            groupBy ((==) `on` fst) $
            sortBy (compare `on` fst) $
            mapMaybe funDecl decls

        funDecl :: Decl SrcSpanInfo -> Maybe (String, Decl SrcSpanInfo)
        funDecl decl@(FunBind _ ((Match _ name _ _ _) : _))
            = Just (nameStr name, decl)
        funDecl decl@(FunBind _ ((InfixMatch _ _ name _ _ _) : _))
            = Just (nameStr name, decl)
        funDecl _ = Nothing

        nameStr :: Name SrcSpanInfo -> String
        nameStr (Ident _ s) = "function " ++ s
        nameStr (Symbol _ s) = "operator (" ++ s ++ ")"

        toMessage :: (String, [Decl SrcSpanInfo]) -> Diagnostic
        toMessage (name, decls) = (ann (decls !! 1), CompileError, "error:\n" ++
            "    Multiple equations for the same function must be contiguous.\n" ++
            "    The " ++ name ++ " has equations at:\n" ++
            (decls >>= \decl -> "    \8226 " ++ formatLocation (ann decl) ++ "\n"))

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

badExpApps :: Exp SrcSpanInfo -> [Diagnostic]
badExpApps (App loc lhs rhs)
    | not (isGoodExpAppLhs lhs) = [(ann rhs, CompileError, nonFuncErrorMsg)]
    | isParenthesizedOpExp rhs  = [(ann rhs, CompileError, opErrorMsg)]
    | not (isGoodExpAppRhs rhs) = [(ann rhs, CompileError, funcErrorMsg)]
  where
    opErrorMsg = "error:" ++ appliedToOperatorError
    nonFuncErrorMsg = "error:" ++ missingParenError ++ multiplicationPhrase
    funcErrorMsg = "error:" ++ missingParenError ++ multiplicationPhrase ++ functionPhrase
    functionPhrase
      | isLikelyFunctionExp lhs = missingParenFunctionSuggestion lhs rhs
      | otherwise = ""
    multiplicationPhrase
      | isLikelyNumberExp lhs && isLikelyNumberExp rhs = missingParenMultiplySuggestion lhs rhs
      | otherwise = ""
badExpApps _ = []

badMatchApps :: Match SrcSpanInfo -> [Diagnostic]
badMatchApps (Match loc lhs pats _ _) = take 1 $
    [(ann p, CompileError, opErrorMsg) | p <- pats, isParenthesizedOpPat p] ++
    [(ann p, CompileError, funcErrorMsg p) | p <- pats, not (isGoodPatAppRhs p)]
  where
    opErrorMsg = "error:" ++ appliedToOperatorError
    funcErrorMsg p = "error:" ++ missingParenError ++ missingParenFunctionSuggestion lhs p
badMatchApps _ = []

badPatternApps :: Pat SrcSpanInfo -> [Diagnostic]
badPatternApps (PApp loc lhs pats) = take 1 $
    [(ann p, CompileError, opErrorMsg) | p <- pats, isParenthesizedOpPat p] ++
    [(ann p, CompileError, funcErrorMsg p) | p <- pats, not (isGoodPatAppRhs p)]
  where
    opErrorMsg = "error:" ++ appliedToOperatorError
    funcErrorMsg p = "error:" ++ missingParenError ++ missingParenFunctionSuggestion lhs p
badPatternApps _ = []

appliedToOperatorError :: String
appliedToOperatorError =
    "\n    \x2022 The expression in this function argument is incomplete."

missingParenError :: String
missingParenError =
    "\n    \x2022 Missing punctuation before this expression." ++
    "\n      Perhaps you forgot a comma, an operator, or a bracket."

missingParenMultiplySuggestion :: (Pretty a, Pretty b) => a -> b -> String
missingParenMultiplySuggestion lhs rhs =
    "\n    \x2022 To multiply, please use the * operator." ++
    "\n      For example: " ++ lhsStr ++ " * " ++ rhsStr
  where lhsStr = fromMaybe "a" (prettyPrintInline lhs)
        rhsStr = fromMaybe "b" (prettyPrintInline rhs)

missingParenFunctionSuggestion :: (Pretty a, Pretty b) => a -> b -> String
missingParenFunctionSuggestion lhs rhs =
    "\n    \x2022 To apply a function, add parentheses around the argument." ++
    "\n      For example: " ++ lhsStr ++ "(" ++ rhsStr ++ ")"
  where lhsStr = fromMaybe "f" (prettyPrintInline lhs)
        rhsStr = fromMaybe "x" (prettyPrintInline rhs)

prettyPrintInline :: Pretty a => a -> Maybe String
prettyPrintInline a
  | length result < 25 && not ('\n' `elem` result) = Just result
  | otherwise = Nothing
  where result = prettyPrintStyleMode style{ mode = OneLineMode } defaultMode a

-- | Determines whether the left-hand side of a function application
-- might possibly be a function.  This eliminates cases where just by
-- syntax alone, we know this cannot possibly be a function, such as
-- when it's a number or a list literal.
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
isGoodExpAppLhs (Paren _ exp) = isGoodExpAppLhs exp
isGoodExpAppLhs _ = True

-- Determines whether the right-hand side of a function app expression is
-- acceptable because it's surrounded by parentheses.
isGoodExpAppRhs :: Exp l -> Bool
isGoodExpAppRhs (Paren _ _) = True
isGoodExpAppRhs (Tuple _ _ _) = True
isGoodExpAppRhs (List _ _) = True
isGoodExpAppRhs (Con _ (Special _ (UnitCon _))) = True
isGoodExpAppRhs (ParArray _ _) = True
isGoodExpAppRhs (EnumFrom _ _) = True
isGoodExpAppRhs (EnumFromThen _ _ _) = True
isGoodExpAppRhs (EnumFromTo _ _ _) = True
isGoodExpAppRhs (EnumFromThenTo _ _ _ _) = True
isGoodExpAppRhs (ParArrayFromTo _ _ _) = True
isGoodExpAppRhs (ParArrayFromThenTo _ _ _ _) = True
isGoodExpAppRhs (ListComp _ _ _) = True
isGoodExpAppRhs (ParComp _ _ _) = True
isGoodExpAppRhs (ParArrayComp _ _ _) = True
isGoodExpAppRhs (TupleSection _ _ _) = True
isGoodExpAppRhs _ = False

-- Determines whether the right-hand side of a function app pattern is
-- acceptable because it's surrounded by parentheses.
isGoodPatAppRhs :: Pat l -> Bool
isGoodPatAppRhs (PParen _ _) = True
isGoodPatAppRhs (PTuple _ _ _) = True
isGoodPatAppRhs (PList _ _) = True
isGoodPatAppRhs (PApp _ (Special _ (UnitCon _)) []) = True
isGoodPatAppRhs _ = False

-- Determines whether a function argument is a parenthesized symbol or
-- section.  These look like they contain parentheses, but the contents of
-- those parentheses aren't ordinary expressions.  To keep the grammar simple
-- for students, these are forbidden without an extra layer of parentheses.
-- This is a special case because the default error message is confusing.

isParenthesizedOpExp :: Exp l -> Bool
isParenthesizedOpExp (Var _ qname) = isOpQName qname
isParenthesizedOpExp (Con _ qname) = isOpQName qname
isParenthesizedOpExp (LeftSection _ _ _) = True
isParenthesizedOpExp (RightSection _ _ _) = True
isParenthesizedOpExp _ = False

isParenthesizedOpPat :: Pat l -> Bool
isParenthesizedOpPat (PVar _ n) = isOpName n
isParenthesizedOpPat _ = False

isOpQName :: QName l -> Bool
isOpQName (Qual _ _ n) = isOpName n
isOpQName (UnQual _ n) = isOpName n
isOpQName (Special _ (FunCon _)) = True
isOpQName (Special _ (TupleCon _ _ _)) = True
isOpQName (Special _ (Cons _)) = True
isOpQName (Special _ (UnboxedSingleCon _)) = True
isOpQName _ = False

isOpName :: Name l -> Bool
isOpName (Symbol _ _) = True
isOpName _ = False

-- | Determines whether an expression is likely to be usable as a function
-- by adding parenthesized arguments.  Note that when this would usually
-- require parentheses (such as with a lambda), this should return false.
isLikelyFunctionExp :: Exp l -> Bool
isLikelyFunctionExp (Var _ _) = True
isLikelyFunctionExp (Con _ _) = True
isLikelyFunctionExp (LeftSection _ _ _) = True
isLikelyFunctionExp (RightSection _ _ _) = True
isLikelyFunctionExp (Paren _ exp) = isGoodExpAppLhs exp
isLikelyFunctionExp _ = False

-- | Determines whether an expression is likely to be usable as a number
-- in a multiplication.  Note that when this would usually require
-- parentheses (such as with a let statement), this should return false.
isLikelyNumberExp :: Exp l -> Bool
isLikelyNumberExp (Var _ _) = True
isLikelyNumberExp (Lit _ _) = True
isLikelyNumberExp (NegApp _ _) = True
isLikelyNumberExp (App _ _ _) = True
isLikelyNumberExp (Paren _ _) = True
isLikelyNumberExp _ = False

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

checkExtraCommas :: MonadCompile m => m ()
checkExtraCommas =
    getParsedCode >>= \parsed -> case parsed of
        Parsed mod -> addDiagnostics $
            everything (++) (mkQ [] extraCommas) mod
        _ -> return ()

extraCommas :: Exp SrcSpanInfo -> [Diagnostic]
extraCommas (TupleSection topLoc _ parts) =
    [ (loc, CompileError,
       "error: This tuple is missing a value, or has an extra comma.")
      | loc <- badLocs ]
  where groups  = groupBy ((==) `on` isJust) parts
        badLocs = catMaybes $ zipWith3 toLoc ([] : groups) groups (tail groups ++ [[]])
        toLoc [] (Nothing : _) []
            = let SrcSpanInfo (SrcSpan _ l1 c1 l2 c2) _ = topLoc
              in  Just (SrcSpanInfo (SrcSpan file l1 (c1 + 1) l2 (max 1 (c2 - 1))) [])
        toLoc (reverse -> Just before : _) (Nothing : _) []
            = let SrcSpanInfo (SrcSpan _ _ _ l1 c1) _ = ann before
                  SrcSpanInfo (SrcSpan _ _ _ l2 c2) _ = topLoc
              in  Just (SrcSpanInfo (SrcSpan file l1 c1 l2 (max 1 (c2 - 1))) [])
        toLoc [] (Nothing : _) (Just after : _)
            = let SrcSpanInfo (SrcSpan _ l1 c1 _ _) _ = topLoc
                  SrcSpanInfo (SrcSpan _ l2 c2 _ _) _ = ann after
              in  Just (SrcSpanInfo (SrcSpan file l1 (c1 + 1) l2 c2) [])
        toLoc (reverse -> Just before : _) (Nothing : _) (Just after : _)
            = let SrcSpanInfo (SrcSpan _ _ _ l1 c1) _ = ann before
                  SrcSpanInfo (SrcSpan _ l2 c2 _ _) _ = ann after
              in  Just (SrcSpanInfo (SrcSpan file l1 c1 l2 c2) [])
        toLoc _ _ _ = Nothing
        file = srcSpanFilename (srcInfoSpan topLoc)
extraCommas _ = []

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
