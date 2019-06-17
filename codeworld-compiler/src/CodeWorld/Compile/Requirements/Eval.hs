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
import Language.Haskell.Exts hiding (Rule, parse)
import Text.Regex.TDFA hiding (match)

import qualified "ghc-lib-parser" DynFlags as GHCParse
import qualified "ghc-lib-parser" HsSyn as GHCParse
import qualified "ghc-lib-parser" OccName as GHCParse
import qualified "ghc-lib-parser" Outputable as GHCParse
import qualified "ghc-lib-parser" RdrName as GHCParse
import qualified "ghc-lib-parser" SrcLoc as GHCParse

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

withGHCParsedCode :: MonadCompile m
                  => (GHCParse.HsModule GHCParse.GhcPs -> m Result)
                  -> m Result
withGHCParsedCode check = do
    getGHCParsedCode >>= \pc -> case pc of
        GHCNoParse -> abort
        GHCParsed m -> check m       

checkRule :: MonadCompile m => Rule -> m Result

checkRule (DefinedByFunction a b) = withGHCParsedCode $ \m -> do
    let defs = allDefinitionsOf a m

        isDefinedBy :: String -> (GHCParse.GRHSs GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs)) -> Bool
        isDefinedBy b (GHCParse.GRHSs{grhssGRHSs=rhss}) 
          = all (\(GHCParse.L _ (GHCParse.GRHS _ _ (GHCParse.L _ exp))) -> isExpOf b exp) rhss

        isExpOf :: String -> (GHCParse.HsExpr GHCParse.GhcPs) -> Bool
        isExpOf b (GHCParse.HsVar _ (GHCParse.L _ bb)) = b == idName bb
        isExpOf b (GHCParse.HsApp _ (GHCParse.L _ exp) _) = isExpOf b exp
        isExpOf b (GHCParse.HsLet _ _ (GHCParse.L _ exp)) = isExpOf b exp
        isExpOf b (GHCParse.HsPar _ (GHCParse.L _ exp)) = isExpOf b exp
        isExpOf b _ = False

    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | all (isDefinedBy b) defs -> success
       | otherwise -> failure ("`" ++ a ++ "` is not defined directly using `" ++ b ++ "`.")

checkRule (MatchesExpected a h) = withGHCParsedCode $ \m -> do
    let defs = allBindingsOf a m
        computedHash = hash (concatMap GHCParse.showSDocUnsafe defs) `mod` 1000000
    if | null defs -> failure $ "`" ++ a ++ "` is not defined."
       | computedHash == h -> success
       | otherwise -> failure $
            "`" ++ a ++ "` does not have the expected definition. (" ++
            show computedHash ++ ")"

checkRule (HasSimpleParams a) = withGHCParsedCode $ \m -> do
    let paramPatterns = everything (++) (mkQ [] funParams) m

        funParams :: (GHCParse.HsBind GHCParse.GhcPs) -> [GHCParse.LPat GHCParse.GhcPs]
        funParams (GHCParse.FunBind{fun_id=(GHCParse.L _ aa), fun_matches=(GHCParse.MG{mg_alts=(GHCParse.L _ matches)})}) 
            | a == idName aa = concat $ matchParams <$> matches
        funParams _ = []

        matchParams :: (GHCParse.LMatch GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs)) -> [GHCParse.LPat GHCParse.GhcPs]
        matchParams (GHCParse.L _ (GHCParse.Match{m_pats=pats})) = pats
        matchParams _ = []

        isSimpleParam :: GHCParse.LPat GHCParse.GhcPs -> Bool
        isSimpleParam (GHCParse.VarPat _ (GHCParse.L _ nm)) = isLower (head (idName nm))
        isSimpleParam (GHCParse.TuplePat _ pats _) = all isSimpleParam pats
        isSimpleParam (GHCParse.ParPat _ pat) = isSimpleParam pat
        isSimpleParam (GHCParse.WildPat _) = True
        isSimpleParam _ = False

    if | null paramPatterns -> failure $ "`" ++ a ++ "` is not defined as a function."
       | all isSimpleParam paramPatterns -> success
       | otherwise -> failure $ "`" ++ a ++ "` has equations with pattern matching."

checkRule (UsesAllParams a) = withGHCParsedCode $ \m -> do
    let usesAllParams = everything (&&) (mkQ True targetVarUsesParams) m

        targetVarUsesParams :: (GHCParse.HsBind GHCParse.GhcPs) -> Bool
        targetVarUsesParams (GHCParse.FunBind{fun_id=(GHCParse.L _ aa), fun_matches=(GHCParse.MG{mg_alts=(GHCParse.L _ ms)})}) 
            | idName aa == a = all matchUsesAllArgs ms
        targetVarUsesParams _ = True

        matchUsesAllArgs (GHCParse.L _ (GHCParse.Match{m_pats=ps, m_grhss=rhs})) = uses ps rhs

        uses ps rhs = 
            all (\v -> rhsUses v rhs) (patVars ps)

        patVars ps = concatMap (everything (++) (mkQ [] patShallowVars)) ps

        patShallowVars :: GHCParse.LPat GHCParse.GhcPs -> [String]
        patShallowVars (GHCParse.VarPat _ (GHCParse.L _ v)) = [idName v]
        patShallowVars (GHCParse.NPlusKPat _ (GHCParse.L _ v) _ _ _ _) = [idName v]
        patShallowVars (GHCParse.AsPat _ (GHCParse.L _ v) _) = [idName v]
        patShallowVars _ = []

        rhsUses v rhs = everything (||) (mkQ False (isVar v)) rhs

        isVar :: String -> GHCParse.HsExpr GHCParse.GhcPs -> Bool
        isVar v (GHCParse.HsVar _ (GHCParse.L _ vv)) = v == idName vv
        isVar _ _ = False

    if | usesAllParams -> success
       | otherwise -> failure $ "`" ++ a ++ "` has unused arguments."

checkRule (NotDefined a) = withGHCParsedCode $ \m -> do
    if | null (allDefinitionsOf a m) -> success
       | otherwise -> failure $ "`" ++ a ++ "` should not be defined."

checkRule (NotUsed a) = withGHCParsedCode $ \m -> do
    let exprUse :: GHCParse.HsExpr GHCParse.GhcPs -> Bool
        exprUse (GHCParse.HsVar _ (GHCParse.L _ v)) | idName v == a = True
        exprUse _ = False

    if | everything (||) (mkQ False exprUse) m
             -> failure $ "`" ++ a ++ "` should not be used."
       | otherwise -> success

checkRule (ContainsMatch tmpl topLevel card) = withParsedCode $ \m -> do
    tmpl <- parseCode ["TemplateHaskell"] (T.pack tmpl)
    let n = case tmpl of
                Parsed (Module _ _ _ _ [tmpl]) ->
                    let decls | topLevel = concat $ gmapQ (mkQ [] id) m
                                | otherwise = everything (++) (mkQ [] (:[])) m
                    in  length (filter (match tmpl) decls)
                Parsed (Module _ _ _ [tmpl] _) ->
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
    let warns = filter (\(SrcSpanInfo _ _,_,x) -> not (any (x =~) ex)) diags
    if | null warns -> success
       | otherwise -> do
             let (SrcSpanInfo (SrcSpan _ l c _ _) _,_,x) = head warns
             failure $ "Warning found at line " ++ show l ++ ", column " ++ show c

checkRule (TypeSignatures b) = withGHCParsedCode $ \m -> do
    let defs = nub $ topLevelNames m
        noTypeSig = defs \\ typeSignatures m

    if | null noTypeSig || not b -> success
       | otherwise -> failure $ "The declaration of `" ++ head noTypeSig
           ++ "` has no type signature."

checkRule (Blacklist bl) = withGHCParsedCode $ \m -> do
    let symbols = nub $ everything (++) (mkQ [] idNameList) m
        blacklisted = intersect bl symbols

        idNameList x = [idName x] 

    if | null blacklisted -> success
       | otherwise -> failure $ "The symbol `" ++ head blacklisted
           ++ "` is blacklisted."

checkRule (Whitelist wl) = withGHCParsedCode $ \m -> do
    let symbols = nub $ everything (++) (mkQ [] idNameList) m
        notWhitelisted = symbols \\ wl

        idNameList x = [idName x]

    if | null notWhitelisted -> success
       | otherwise -> failure $ "The symbol `" ++ head notWhitelisted
           ++ "` is not whitelisted."

checkRule _ = abort

allDefinitionsOf :: String -> GHCParse.HsModule GHCParse.GhcPs -> [GHCParse.GRHSs GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs)]
allDefinitionsOf a m = everything (++) (mkQ [] defs) m
  where defs :: GHCParse.HsBind GHCParse.GhcPs -> [GHCParse.GRHSs GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs)]
        defs (GHCParse.FunBind{fun_id=(GHCParse.L _ funid), fun_matches=(GHCParse.MG{mg_alts=(GHCParse.L _ matches)})}) 
            | idName funid == a = concat $ funcDefs <$> matches
        defs (GHCParse.PatBind{pat_lhs=pat, pat_rhs=rhs}) | patDefines pat a = [rhs]
        defs _ = []

        funcDefs :: GHCParse.LMatch GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs) -> [GHCParse.GRHSs GHCParse.GhcPs (GHCParse.LHsExpr GHCParse.GhcPs)]
        funcDefs (GHCParse.L _ (GHCParse.Match{m_grhss=rhs})) = [rhs]
        funcDefs _ = []

allBindingsOf :: String -> GHCParse.HsModule GHCParse.GhcPs -> [GHCParse.SDoc]
allBindingsOf a m = everything (++) (mkQ [] binds) m
  where binds :: GHCParse.HsBind GHCParse.GhcPs -> [GHCParse.SDoc]
        binds (GHCParse.FunBind{fun_id=(GHCParse.L _ funid), fun_matches=matches}) | idName funid == a = [GHCParse.pprFunBind matches]
        binds (GHCParse.PatBind{pat_lhs=pat, pat_rhs=rhs}) | patDefines pat a = [GHCParse.pprPatBind pat rhs]
        binds _ = []

topLevelNames :: GHCParse.HsModule GHCParse.GhcPs -> [String]
topLevelNames (GHCParse.HsModule {hsmodDecls=decls}) = concat $ names <$> decls
  where names :: GHCParse.LHsDecl GHCParse.GhcPs -> [String]
        names (GHCParse.L _ (GHCParse.ValD _ GHCParse.FunBind{fun_id=(GHCParse.L _ funid)})) = [idName funid]
        names (GHCParse.L _ (GHCParse.ValD _ GHCParse.PatBind{pat_lhs=pat})) = [patName pat]
        names _ = []

        patName :: GHCParse.LPat GHCParse.GhcPs -> String
        patName (GHCParse.VarPat _ (GHCParse.L _ patid)) = idName patid
        patName (GHCParse.ParPat _ pat) = patName pat
        patName _ = []

typeSignatures :: GHCParse.HsModule GHCParse.GhcPs -> [String]
typeSignatures (GHCParse.HsModule {hsmodDecls=decls}) = concat $ typeSigNames <$> decls
  where typeSigNames :: GHCParse.LHsDecl GHCParse.GhcPs -> [String]
        typeSigNames (GHCParse.L _ (GHCParse.SigD _ (GHCParse.TypeSig _ sigids _))) = locatedIdName <$> sigids
        typeSigNames _ = []

        locatedIdName (GHCParse.L _ s) = idName s

patDefines :: GHCParse.LPat GHCParse.GhcPs -> String -> Bool
patDefines (GHCParse.VarPat _ (GHCParse.L _ patid)) a = idName patid == a
patDefines (GHCParse.ParPat _ pat) a = patDefines pat a
patDefines _ a = False

idName :: GHCParse.IdP GHCParse.GhcPs -> String
idName = GHCParse.occNameString . GHCParse.rdrNameOcc


