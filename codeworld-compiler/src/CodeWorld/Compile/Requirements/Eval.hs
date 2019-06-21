{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
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

module CodeWorld.Compile.Requirements.Eval (
    Requirement,
    evalRequirement
    ) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.Matcher
import CodeWorld.Compile.Requirements.Types
import Control.Monad.IO.Class
import Data.Array
import Data.Char
import Data.Either
import Data.Generics hiding (empty)
import Data.Hashable
import Data.List
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Void
import qualified Language.Haskell.Exts as Exts
import Text.Regex.TDFA hiding (match)

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" SrcLoc

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
                  => (HsModule GhcPs -> m Result)
                  -> m Result
withParsedCode check = do
    getGHCParsedCode >>= \pc -> case pc of
        GHCNoParse -> abort
        GHCParsed m -> check m       

checkRule :: MonadCompile m => Rule -> m Result

checkRule (DefinedByFunction a b) = withParsedCode $ \m -> do
    let defs = allDefinitionsOf a m

        isDefinedBy :: String -> (GRHSs GhcPs (LHsExpr GhcPs)) -> Bool
        isDefinedBy b (GRHSs{grhssGRHSs=rhss}) 
          = all (\(L _ (GRHS _ _ (L _ exp))) -> isExpOf b exp) rhss

        isExpOf :: String -> (HsExpr GhcPs) -> Bool
        isExpOf b (HsVar _ (L _ bb)) = b == idName bb
        isExpOf b (HsApp _ (L _ exp) _) = isExpOf b exp
        isExpOf b (HsLet _ _ (L _ exp)) = isExpOf b exp
        isExpOf b (HsPar _ (L _ exp)) = isExpOf b exp
        isExpOf b _ = False

    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | all (isDefinedBy b) defs -> success
       | otherwise -> failure ("`" ++ a ++ "` is not defined directly using `" ++ b ++ "`.")

checkRule (MatchesExpected a h) = withParsedCode $ \m -> do
    let defs = allBindingsOf a m
        computedHash = hash (concatMap showSDocUnsafe defs) `mod` 1000000
    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | computedHash == h -> success
       | otherwise -> failure $
            "`" ++ a ++ "` does not have the expected definition. (" ++
            show computedHash ++ ")"

checkRule (HasSimpleParams a) = withParsedCode $ \m -> do
    let paramPatterns = everything (++) (mkQ [] funParams) m

        funParams :: (HsBind GhcPs) -> [LPat GhcPs]
        funParams (FunBind{fun_id=(L _ aa), fun_matches=(MG{mg_alts=(L _ matches)})}) 
            | a == idName aa = concat $ matchParams <$> matches
        funParams _ = []

        matchParams :: (LMatch GhcPs (LHsExpr GhcPs)) -> [LPat GhcPs]
        matchParams (L _ (Match{m_pats=pats})) = pats
        matchParams _ = []

        isSimpleParam :: LPat GhcPs -> Bool
        isSimpleParam (VarPat _ (L _ nm)) = isLower (head (idName nm))
        isSimpleParam (TuplePat _ pats _) = all isSimpleParam pats
        isSimpleParam (ParPat _ pat) = isSimpleParam pat
        isSimpleParam (WildPat _) = True
        isSimpleParam _ = False

    if | null paramPatterns -> failure $ "`" ++ a ++ "` is not defined as a function."
       | all isSimpleParam paramPatterns -> success
       | otherwise -> failure $ "`" ++ a ++ "` has equations with pattern matching."

checkRule (UsesAllParams a) = withParsedCode $ \m -> do
    let usesAllParams = everything (&&) (mkQ True targetVarUsesParams) m

        targetVarUsesParams :: (HsBind GhcPs) -> Bool
        targetVarUsesParams (FunBind{fun_id=(L _ aa), fun_matches=(MG{mg_alts=(L _ ms)})}) 
            | idName aa == a = all matchUsesAllArgs ms
        targetVarUsesParams _ = True

        matchUsesAllArgs (L _ (Match{m_pats=ps, m_grhss=rhs})) = uses ps rhs

        uses ps rhs = 
            all (\v -> rhsUses v rhs) (patVars ps)

        patVars ps = concatMap (everything (++) (mkQ [] patShallowVars)) ps

        patShallowVars :: LPat GhcPs -> [String]
        patShallowVars (VarPat _ (L _ v)) = [idName v]
        patShallowVars (NPlusKPat _ (L _ v) _ _ _ _) = [idName v]
        patShallowVars (AsPat _ (L _ v) _) = [idName v]
        patShallowVars _ = []

        rhsUses v rhs = everything (||) (mkQ False (isVar v)) rhs

        isVar :: String -> HsExpr GhcPs -> Bool
        isVar v (HsVar _ (L _ vv)) = v == idName vv
        isVar _ _ = False

    if | usesAllParams -> success
       | otherwise -> failure $ "`" ++ a ++ "` has unused arguments."

checkRule (NotDefined a) = withParsedCode $ \m -> do
    if | null (allDefinitionsOf a m) -> success
       | otherwise -> failure $ "`" ++ a ++ "` should not be defined."

checkRule (NotUsed a) = withParsedCode $ \m -> do
    let exprUse :: HsExpr GhcPs -> Bool
        exprUse (HsVar _ (L _ v)) | idName v == a = True
        exprUse _ = False

    if | everything (||) (mkQ False exprUse) m
             -> failure $ "`" ++ a ++ "` should not be used."
       | otherwise -> success

checkRule (ContainsMatch tmpl topLevel card) = withParsedCode $ \m -> do
    tmpl <- ghcParseCode ["TemplateHaskell"] (T.pack tmpl)
    let n = case tmpl of
                GHCParsed (HsModule {hsmodDecls=[tmpl]}) ->
                    let decls | topLevel = concat $ gmapQ (mkQ [] id) m
                              | otherwise = everything (++) (mkQ [] (:[])) m
                    in  length (filter (match tmpl) decls)
                GHCParsed (HsModule {hsmodImports=[tmpl]}) ->
                    length $ filter (match tmpl) $ concat $ gmapQ (mkQ [] id) m       
    if | hasCardinality card n -> success
       | otherwise -> failure $ "Wrong number of matches."
    
checkRule (MatchesRegex pat card) = do
    src <- getSourceCode
    let n = rangeSize (bounds (src =~ pat :: MatchArray))
    if | hasCardinality card n -> success
       | otherwise -> failure $ "Wrong number of matches."

checkRule (OnFailure msg rule) = do
    result <- checkRule rule
    case result of
        Just (_:_) -> failure msg
        other -> return other

checkRule (IfThen a b) = do
    cond <- checkRule a
    case cond of
        Just [] -> checkRule b
        Just _ -> success
        Nothing -> abort

checkRule (AllOf rules) = do
    results <- sequence <$> mapM checkRule rules
    return (concat <$> results)

checkRule (AnyOf rules) = do
    results <- sequence <$> mapM checkRule rules
    return $ (<$> results) $ \errs ->
        if any null errs then [] else ["No alternatives succeeded."]

checkRule (NotThis rule) = do
    result <- checkRule rule
    case result of
        Just [] -> failure "A rule matched, but shouldn't have."
        Just _ -> success
        Nothing -> abort

checkRule (MaxLineLength len) = do
    src <- getSourceCode
    if | any (> len) (C.length <$> C.lines src) -> 
            failure $ "One or more lines longer than " ++ show len ++ " characters."
       | otherwise -> success

checkRule (NoWarningsExcept ex) = do
    diags <- getDiagnostics
    let warns = filter (\(Exts.SrcSpanInfo _ _,_,x) -> not (any (x =~) ex)) diags
    if | null warns -> success
       | otherwise -> do
             let (Exts.SrcSpanInfo (Exts.SrcSpan _ l c _ _) _,_,x) = head warns
             failure $ "Warning found at line " ++ show l ++ ", column " ++ show c

checkRule (TypeSignatures b) = withParsedCode $ \m -> do
    let defs = nub $ topLevelNames m
        noTypeSig = defs \\ typeSignatures m

    if | null noTypeSig || not b -> success
       | otherwise -> failure $ "The declaration of `" ++ head noTypeSig
           ++ "` has no type signature."

checkRule (Blacklist bl) = withParsedCode $ \m -> do
    let symbols = nub $ everything (++) (mkQ [] idNameList) m
        blacklisted = intersect bl symbols

        idNameList x = [idName x] 

    if | null blacklisted -> success
       | otherwise -> failure $ "The symbol `" ++ head blacklisted
           ++ "` is blacklisted."

checkRule (Whitelist wl) = withParsedCode $ \m -> do
    let symbols = nub $ everything (++) (mkQ [] idNameList) m
        notWhitelisted = symbols \\ wl

        idNameList x = [idName x]

    if | null notWhitelisted -> success
       | otherwise -> failure $ "The symbol `" ++ head notWhitelisted
           ++ "` is not whitelisted."

checkRule _ = abort

allDefinitionsOf :: String -> HsModule GhcPs -> [GRHSs GhcPs (LHsExpr GhcPs)]
allDefinitionsOf a m = everything (++) (mkQ [] defs) m
  where defs :: HsBind GhcPs -> [GRHSs GhcPs (LHsExpr GhcPs)]
        defs (FunBind{fun_id=(L _ funid), fun_matches=(MG{mg_alts=(L _ matches)})}) 
            | idName funid == a = concat $ funcDefs <$> matches
        defs (PatBind{pat_lhs=pat, pat_rhs=rhs}) | patDefines pat a = [rhs]
        defs _ = []

        funcDefs :: LMatch GhcPs (LHsExpr GhcPs) -> [GRHSs GhcPs (LHsExpr GhcPs)]
        funcDefs (L _ (Match{m_grhss=rhs})) = [rhs]
        funcDefs _ = []

allBindingsOf :: String -> HsModule GhcPs -> [SDoc]
allBindingsOf a m = everything (++) (mkQ [] binds) m
  where binds :: HsBind GhcPs -> [SDoc]
        binds (FunBind{fun_id=(L _ funid), fun_matches=matches}) | idName funid == a = [pprFunBind matches]
        binds (PatBind{pat_lhs=pat, pat_rhs=rhs}) | patDefines pat a = [pprPatBind pat rhs]
        binds _ = []

topLevelNames :: HsModule GhcPs -> [String]
topLevelNames (HsModule {hsmodDecls=decls}) = concat $ names <$> decls
  where names :: LHsDecl GhcPs -> [String]
        names (L _ (ValD _ FunBind{fun_id=(L _ funid)})) = [idName funid]
        names (L _ (ValD _ PatBind{pat_lhs=pat})) = [patName pat]
        names _ = []

        patName :: LPat GhcPs -> String
        patName (VarPat _ (L _ patid)) = idName patid
        patName (ParPat _ pat) = patName pat
        patName _ = []

typeSignatures :: HsModule GhcPs -> [String]
typeSignatures (HsModule {hsmodDecls=decls}) = concat $ typeSigNames <$> decls
  where typeSigNames :: LHsDecl GhcPs -> [String]
        typeSigNames (L _ (SigD _ (TypeSig _ sigids _))) = locatedIdName <$> sigids
        typeSigNames _ = []

        locatedIdName (L _ s) = idName s

patDefines :: LPat GhcPs -> String -> Bool
patDefines (VarPat _ (L _ patid)) a = idName patid == a
patDefines (ParPat _ pat) a = patDefines pat a
patDefines _ a = False

