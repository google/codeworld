{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
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

module CodeWorld.Compile.Requirements.Eval (
    Requirement,
    evalRequirement
    ) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.Matcher
import CodeWorld.Compile.Requirements.Types
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import Data.Generics hiding (empty)
import Data.Hashable
import qualified Data.Text as T
import Data.Void
import Language.Haskell.Exts hiding (Rule, parse)

evalRequirement :: MonadCompile m => Requirement -> m (Maybe Bool, [String])
evalRequirement Requirement{..} = do
    results <- fmap concat <$> (sequence <$> mapM checkRule requiredRules)
    return $ case results of
        Nothing -> (Nothing, ["Could not check this requirement."])
        Just errs -> (Just (null errs), errs)

type Result = Maybe [String]

success :: MonadCompile m => m Result
success = return (Just [])

failure :: MonadCompile m => String -> m Result
failure err = return (Just [err])

abort :: MonadCompile m => m Result
abort = return Nothing

withParsedCode :: MonadCompile m
               => (Module SrcSpanInfo -> m Result)
               -> m Result
withParsedCode check = do
    getParsedCode >>= \pc -> case pc of
        NoParse -> abort
        Parsed m -> check m

checkRule :: MonadCompile m => Rule -> m Result

checkRule (DefinedByFunction a b) = withParsedCode $ \m -> do
    let defs = allDefinitionsOf a m

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

    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | all (isDefinedBy b) defs -> success
       | otherwise -> failure ("`" ++ a ++ "` is not defined directly using `" ++ b ++ "`.")

checkRule (MatchesExpected a h) = withParsedCode $ \m -> do
    let defs = allDefinitionsOf a m
        computedHash = hash (concatMap (show . eraseSrcLocs) defs) `mod` 1000000
        eraseSrcLocs rhs = everywhere (mkT (const noSrcSpan)) rhs
    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | computedHash == h -> success
       | otherwise -> failure $
            "`" ++ a ++ "` does not have the expected definition. (" ++
            show computedHash ++ ")"

checkRule (HasSimpleParams a) = withParsedCode $ \m -> do
    let paramPatterns = everything (++) (mkQ [] matchParams) m

        matchParams :: Match SrcSpanInfo -> [Pat SrcSpanInfo]
        matchParams (Match _ (Ident _ aa) pats _ _) | a == aa = pats
        matchParams _ = []

        isSimpleParam :: Pat SrcSpanInfo -> Bool
        isSimpleParam (PVar _ (Ident _ nm)) = isLower (head nm)
        isSimpleParam (PTuple _ _ pats) = all isSimpleParam pats
        isSimpleParam (PParen _ pat) = isSimpleParam pat
        isSimpleParam (PWildCard _) = True
        isSimpleParam _ = False

    if | null paramPatterns -> failure $ "`" ++ a ++ "` is not defined as a function."
       | all isSimpleParam paramPatterns -> success
       | otherwise -> failure $ "`" ++ a ++ "` has equations with pattern matching."

checkRule (UsesAllParams a) = withParsedCode $ \m -> do
    let usesAllParams = everything (&&) (mkQ True targetVarUsesParams) m

        targetVarUsesParams :: Decl SrcSpanInfo -> Bool
        targetVarUsesParams (FunBind _ ms)
            | any isTargetMatch ms = all matchUsesAllArgs ms
        targetVarUsesParams _ = True

        isTargetMatch (Match _ (Ident _ aa) _ _ _) = a == aa
        isTargetMatch (InfixMatch _ _ (Ident _ aa) _ _ _) = a == aa

        matchUsesAllArgs (Match _ _ ps rhs binds) = uses ps rhs binds
        matchUsesAllArgs (InfixMatch _ p _ ps rhs binds) = uses (p:ps) rhs binds

        uses ps rhs binds =
            all (\v -> rhsUses v rhs || bindsUses v binds) (patVars ps)

        patVars ps = concatMap (everything (++) (mkQ [] patShallowVars)) ps

        patShallowVars :: Pat SrcSpanInfo -> [String]
        patShallowVars (PVar _ (Ident _ v)) = [v]
        patShallowVars (PNPlusK _ (Ident _ v) _) = [v]
        patShallowVars (PAsPat _ (Ident _ v) _) = [v]
        patShallowVars _ = []

        rhsUses v rhs = everything (||) (mkQ False (isVar v)) rhs
        bindsUses v binds = everything (||) (mkQ False (isVar v)) binds

        isVar :: String -> Exp SrcSpanInfo -> Bool
        isVar v (Var _ (UnQual _ (Ident _ vv))) = v == vv
        isVar _ _ = False

    if | usesAllParams -> success
       | otherwise -> failure $ "`" ++ a ++ "` has unused arguments."

checkRule (NotDefined a) = withParsedCode $ \m -> do
    if | null (allDefinitionsOf a m) -> success
       | otherwise -> failure $ "`" ++ a ++ "` should not be defined."

checkRule (NotUsed a) = withParsedCode $ \m -> do
    let exprUse :: Exp SrcSpanInfo -> Bool
        exprUse (Var _ (UnQual _ (Ident _ v))) | v == a = True
        exprUse _ = False

    if | everything (||) (mkQ False exprUse) m
             -> failure $ "`" ++ a ++ "` should not be used."
       | otherwise -> success

checkRule (ContainsMatch a) = withParsedCode $ \m -> do
    tmpl <- parseCode (T.pack a)
    case (tmpl, m) of
        (Parsed (Module _ _ _ _ tmplDecls), Module _ _ _ _ decls)
          | all (\t -> any (match t) decls) tmplDecls -> success
          | otherwise -> failure $ "There was no match."
        _ -> abort

checkRule _ = abort

allDefinitionsOf :: String -> Module SrcSpanInfo -> [Rhs SrcSpanInfo]
allDefinitionsOf a m = everything (++) (mkQ [] funcDefs) m ++
                       everything (++) (mkQ [] patDefs) m
  where funcDefs :: Match SrcSpanInfo -> [Rhs SrcSpanInfo]
        funcDefs (Match _ (Ident _ aa) _ rhs _) | a == aa = [rhs]
        funcDefs _ = []

        patDefs :: Decl SrcSpanInfo -> [Rhs SrcSpanInfo]
        patDefs (PatBind _ pat rhs _) | patDefines pat a = [rhs]
        patDefs _ = []

patDefines :: Pat SrcSpanInfo -> String -> Bool
patDefines (PVar _ (Ident _ aa)) a = a == aa
patDefines (PParen _ pat) a = patDefines pat a
