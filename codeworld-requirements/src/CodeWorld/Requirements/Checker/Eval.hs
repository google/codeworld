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

module CodeWorld.Requirements.Checker.Eval (
    Requirement,
    evalRequirement
    ) where

import CodeWorld.Requirements.Framework
import CodeWorld.Requirements.Checker.Matcher
import CodeWorld.Requirements.Checker.Types
import Control.Monad.IO.Class
import Data.Array
import Data.Char
import Data.Either
import Data.Generics hiding (empty)
import Data.Hashable
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Void
import qualified Language.Haskell.Exts as Exts
import Text.Regex.TDFA hiding (match)

import Bag
import DynFlags
import ErrUtils
import FastString
import HsSyn
import Name
import Outputable
import SrcLoc
import TcRnTypes
import Var

evalRequirement :: DynFlags -> Messages -> TcGblEnv -> HsModule GhcPs -> C.ByteString -> Requirement -> (Maybe Bool, [String])
evalRequirement f c e m s Requirement{..} = let
        results = map (checkRule f c e m s) requiredRules
        evals = if any isNothing results then Nothing else Just $ concat (catMaybes results)
    in case evals of
        Nothing -> (Nothing, ["Could not check this requirement."])
        Just errs -> (Just (null errs), errs)

type Result = Maybe [String]

success :: Result
success = Just []

failure :: String -> Result
failure err = Just [err]

abort :: Result
abort = Nothing

checkRule :: DynFlags -> Messages -> TcGblEnv -> HsModule GhcPs -> C.ByteString -> Rule -> Result
checkRule f c e m s r = case getStage r of
    Source -> checkRuleSource s r 
    Parse -> checkRuleParse f m r
    Typecheck -> checkRuleTypecheck c e r
    Multiple -> checkRuleMultiple f c e m s r

checkRuleSource :: C.ByteString -> Rule -> Result

checkRuleSource s (MatchesRegex pat card)
        | hasCardinality card n = success
        | otherwise = failure $ "Wrong number of matches."
    where
        n = rangeSize (bounds (s =~ pat :: MatchArray))

checkRuleSource s (MaxLineLength len)
    | any (> len) (C.length <$> C.lines s) = 
        failure $ "One or more lines longer than " ++ show len ++ " characters."
    | otherwise = success

checkRuleSource _ _ = abort

checkRuleParse :: DynFlags -> HsModule GhcPs -> Rule -> Result

checkRuleParse _ m (DefinedByFunction a b)
        | null defs = failure $ "`" ++ a ++ "` is not defined."
        | all (isDefinedBy b) defs = success
        | otherwise = failure ("`" ++ a ++ "` is not defined directly using `" ++ b ++ "`.")
    where
        defs = allDefinitionsOf a m

        isDefinedBy :: String -> (GRHSs GhcPs (LHsExpr GhcPs)) -> Bool
        isDefinedBy b (GRHSs{grhssGRHSs=rhss}) 
          = all (\(L _ (GRHS _ _ (L _ exp))) -> isExpOf b exp) rhss

        isExpOf :: String -> (HsExpr GhcPs) -> Bool
        isExpOf b (HsVar _ (L _ bb)) = b == idName bb
        isExpOf b (HsApp _ (L _ exp) _) = isExpOf b exp
        isExpOf b (HsLet _ _ (L _ exp)) = isExpOf b exp
        isExpOf b (HsPar _ (L _ exp)) = isExpOf b exp
        isExpOf b _ = False

checkRuleParse _ m (MatchesExpected a h)
        | null defs = failure $ "`" ++ a ++ "` is not defined."
        | computedHash == h = success
        | otherwise = failure $
            "`" ++ a ++ "` does not have the expected definition. (" ++
            show computedHash ++ ")"
    where
        defs = allBindingsOf a m
        computedHash = hash (concatMap showSDocUnsafe defs) `mod` 1000000

checkRuleParse _ m (HasSimpleParams a) 
        | null paramPatterns = failure $ "`" ++ a ++ "` is not defined as a function."
        | all isSimpleParam paramPatterns = success
        | otherwise = failure $ "`" ++ a ++ "` has equations with pattern matching."
    where
        paramPatterns = everything (++) (mkQ [] funParams) m

        funParams :: (HsBind GhcPs) -> [LPat GhcPs]
        funParams (FunBind{fun_id=(L _ aa), fun_matches=(MG{mg_alts=(L _ matches)})}) 
            | a == idName aa = concat $ matchParams <$> matches
        funParams _ = []

        matchParams :: (LMatch GhcPs (LHsExpr GhcPs)) -> [LPat GhcPs]
        matchParams (L _ (Match{m_pats=pats})) = pats
        matchParams _ = []

        isSimpleParam :: LPat GhcPs -> Bool
        isSimpleParam (L _ (VarPat _ (L _ nm))) = isLower (head (idName nm))
        isSimpleParam (L _ (TuplePat _ pats _)) = all isSimpleParam pats
        isSimpleParam (L _ (ParPat _ pat)) = isSimpleParam pat
        isSimpleParam (L _ (WildPat _)) = True
        isSimpleParam _ = False

checkRuleParse _ m (UsesAllParams a)
        | usesAllParams = success
        | otherwise = failure $ "`" ++ a ++ "` has unused arguments."
    where
        usesAllParams = everything (&&) (mkQ True targetVarUsesParams) m

        targetVarUsesParams :: (HsBind GhcPs) -> Bool
        targetVarUsesParams (FunBind{fun_id=(L _ aa), fun_matches=(MG{mg_alts=(L _ ms)})}) 
            | idName aa == a = all matchUsesAllArgs ms
        targetVarUsesParams _ = True

        matchUsesAllArgs (L _ (Match{m_pats=ps, m_grhss=rhs})) = uses ps rhs

        uses ps rhs = 
            all (\v -> rhsUses v rhs) (patVars ps)

        patVars ps = concatMap (everything (++) (mkQ [] patShallowVars)) ps

        patShallowVars :: LPat GhcPs -> [String]
        patShallowVars (L _ (VarPat _ (L _ v))) = [idName v]
        patShallowVars (L _ (NPlusKPat _ (L _ v) _ _ _ _)) = [idName v]
        patShallowVars (L _ (AsPat _ (L _ v) _)) = [idName v]
        patShallowVars _ = []

        rhsUses v rhs = everything (||) (mkQ False (isVar v)) rhs

        isVar :: String -> HsExpr GhcPs -> Bool
        isVar v (HsVar _ (L _ vv)) = v == idName vv
        isVar _ _ = False

checkRuleParse _ m (NotDefined a)
        | null (allDefinitionsOf a m) = success
        | otherwise = failure $ "`" ++ a ++ "` should not be defined."

checkRuleParse _ m (NotUsed a)
        | everything (||) (mkQ False exprUse) m
             = failure $ "`" ++ a ++ "` should not be used."
        | otherwise = success
    where
        exprUse :: HsExpr GhcPs -> Bool
        exprUse (HsVar _ (L _ v)) | idName v == a = True
        exprUse _ = False

checkRuleParse f m (ContainsMatch tmpl topLevel card)
        | hasCardinality card n = success
        | otherwise = failure $ "Wrong number of matches."
    where
        p = ghcParseCode f ["TemplateHaskell", "TemplateHaskellQuotes"] (T.pack tmpl)
        n = case p of
            GHCParsed (HsModule {hsmodDecls=[tmpl]}) ->
                let decls | topLevel = concat $ gmapQ (mkQ [] id) m
                          | otherwise = everything (++) (mkQ [] (:[])) m
                    in length (filter (match tmpl) decls)
            GHCParsed (HsModule {hsmodImports=[tmpl]}) ->
                length $ filter (match tmpl) $ concat $ gmapQ (mkQ [] id) m       

checkRuleParse _ m (TypeSignatures b)
        | null noTypeSig || not b = success
        | otherwise = failure $ "The declaration of `" ++ head noTypeSig
           ++ "` has no type signature."
    where
        defs = nub $ topLevelNames m
        noTypeSig = defs \\ typeSignatures m

checkRuleParse _ _ _ = abort

checkRuleTypecheck :: Messages -> TcGblEnv -> Rule -> Result

checkRuleTypecheck c e (NoWarningsExcept ex)
        | null warns = success
        | otherwise = failure $ "At least one forbidden warning found."
    where msgs = showSDocUnsafe <$> (pprErrMsgBagWithLoc $ fst c)
          warns = filter (\x -> not (any (x =~) ex)) msgs

checkRuleTypecheck _ e (Blacklist bl)
        | null forbidden = success
        | otherwise = failure $ "The symbol `" ++ head forbidden
            ++ "` is blacklisted."
    where
        names = nub $ everything (++) (mkQ [] nameStrings) (tcg_rn_decls e)
        forbidden = intersect bl names

        nameStrings x = [nameString x]

checkRuleTypecheck _ e (Whitelist wl)
        | null forbidden = success
        | otherwise = failure $ "The symbol `" ++ head forbidden
             ++ "` is not whitelisted."
    where
        symbols = nub $ everything (++) (mkQ [] nameStrings) (tcg_rn_decls e)
        forbidden = symbols \\ wl

        nameStrings x = [nameString x]

checkRuleTypecheck _ _ _ = abort

checkRuleMultiple :: DynFlags -> Messages -> TcGblEnv -> HsModule GhcPs -> C.ByteString -> Rule -> Result 

checkRuleMultiple f c e m s (OnFailure msg rule) = case checkRule f c e m s rule of
    Just (_:_) -> failure msg
    other -> other

checkRuleMultiple f c e m s (NotThis rule) = case checkRule f c e m s rule of
    Just [] -> failure "A rule matched, but shouldn't have."
    Just _ -> success
    Nothing -> abort

checkRuleMultiple f c e m s (IfThen a b) = case checkRule f c e m s a of 
    Just [] -> checkRule f c e m s b
    Just _ -> success
    Nothing -> abort

checkRuleMultiple f c e m s (AllOf rules) = do
    let results = map (checkRule f c e m s) rules
    if any isNothing results then abort else Just $ concat (catMaybes results)

checkRuleMultiple f c e m s (AnyOf rules) = do
    let results = map (checkRule f c e m s) rules
    if any isNothing results then abort else do
        let errs = catMaybes results 
        return (if any null errs then [] else ["No alternatives succeeded."])

checkRuleMultiple _ _ _ _ _ _ = abort

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
        patName (L _ (VarPat _ (L _ patid))) = idName patid
        patName (L _ (ParPat _ pat)) = patName pat
        patName _ = []

typeSignatures :: HsModule GhcPs -> [String]
typeSignatures (HsModule {hsmodDecls=decls}) = concat $ typeSigNames <$> decls
  where typeSigNames :: LHsDecl GhcPs -> [String]
        typeSigNames (L _ (SigD _ (TypeSig _ sigids _))) = locatedIdName <$> sigids
        typeSigNames _ = []

        locatedIdName (L _ s) = idName s

patDefines :: LPat GhcPs -> String -> Bool
patDefines (L _ (VarPat _ (L _ patid))) a = idName patid == a
patDefines (L _ (ParPat _ pat)) a = patDefines pat a
patDefines _ a = False

nameString :: IdP GhcRn -> String
nameString = showSDocUnsafe . pprOccName . nameOccName