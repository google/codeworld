{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
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

module CodeWorld.Compile.Requirements.Matcher where

import Control.Monad
import Data.Generics
import Data.Generics.Twins
import Data.List
import Data.Maybe
import Language.Haskell.Exts

import qualified "ghc-lib-parser" HsSyn as GHCParse
import qualified "ghc-lib-parser" OccName as GHCParse
import qualified "ghc-lib-parser" RdrName as GHCParse
import qualified "ghc-lib-parser" SrcLoc as GHCParse

class (Data a, Typeable a) => Template a where
  toSplice :: a -> Maybe (GHCParse.HsSplice GHCParse.GhcPs)
  fromBracket :: (GHCParse.HsBracket GHCParse.GhcPs) -> Maybe a

  toParens :: a -> Maybe a
  toTuple :: a -> Maybe [a]
  toVar :: a -> Maybe a
  toCon :: a -> Maybe a
  toLit :: a -> Maybe a
  toNum :: a -> Maybe a
  toChar :: a -> Maybe a
  toStr :: a -> Maybe a

instance Template (GHCParse.Pat GHCParse.GhcPs) where
  toSplice (GHCParse.SplicePat _ s) = Just s
  toSplice _ = Nothing

  fromBracket (GHCParse.PatBr _ p) = Just p
  fromBracket _ = Nothing

  toParens (GHCParse.ParPat _ x) = Just x
  toParens _ = Nothing

  toTuple (GHCParse.TuplePat _ ps _) = Just ps
  toTuple _ = Nothing

  toVar x@(GHCParse.VarPat _ _) = Just x
  toVar _ = Nothing

  toCon x@(GHCParse.ConPatIn _ _) = Just x
  toCon x@(GHCParse.ConPatOut {}) = Just x
  toCon _ = Nothing

  toLit x@(GHCParse.LitPat _ _) = Just x
  toLit _ = Nothing

  toNum x@(GHCParse.LitPat _ (GHCParse.HsInt _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsInteger _ _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsRat _ _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsIntPrim _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsWordPrim _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsInt64Prim _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsWord64Prim _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsFloatPrim _ _)) = Just x
  toNum x@(GHCParse.LitPat _ (GHCParse.HsDoublePrim _ _)) = Just x
  toNum _ = Nothing

  toChar x@(GHCParse.LitPat _ (GHCParse.HsChar _ _)) = Just x
  toChar x@(GHCParse.LitPat _ (GHCParse.HsCharPrim _ _)) = Just x
  toChar _ = Nothing

  toStr x@(GHCParse.LitPat _ (GHCParse.HsString _ _)) = Just x
  toStr x@(GHCParse.LitPat _ (GHCParse.HsStringPrim _ _)) = Just x
  toStr _ = Nothing

instance Template (GHCParse.HsExpr GHCParse.GhcPs) where
  toSplice (GHCParse.HsSpliceE _ s) = Just s
  toSplice _ = Nothing

  fromBracket (GHCParse.ExpBr _ (GHCParse.L _ e)) = Just e
  fromBracket _ = Nothing

  toParens (GHCParse.HsPar _ (GHCParse.L _ x)) = Just x
  toParens _ = Nothing

  toTuple (GHCParse.ExplicitTuple _ args _) = Just (concat $ tupArgExpr <$> args)
  toTuple _ = Nothing

  toVar x@(GHCParse.HsVar _ _) = Just x
  toVar _ = Nothing

  toCon x@(GHCParse.HsConLikeOut _ _) = Just x
  toCon _ = Nothing

  toLit x@(GHCParse.HsLit _ _) = Just x
  toLit x@(GHCParse.NegApp _ (GHCParse.L _ (GHCParse.HsLit _ _)) _) = Just x
  toLit _ = Nothing

  toNum x@(GHCParse.HsLit _ (GHCParse.HsInt _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsInteger _ _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsRat _ _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsIntPrim _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsWordPrim _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsInt64Prim _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsWord64Prim _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsFloatPrim _ _)) = Just x
  toNum x@(GHCParse.HsLit _ (GHCParse.HsDoublePrim _ _)) = Just x
  toNum x@(GHCParse.NegApp _ (GHCParse.L _ (toNum -> Just _)) _)= Just x
  toNum _ = Nothing

  toChar x@(GHCParse.HsLit _ (GHCParse.HsChar _ _)) = Just x
  toChar x@(GHCParse.HsLit _ (GHCParse.HsCharPrim _ _)) = Just x
  toChar _ = Nothing

  toStr x@(GHCParse.HsLit _ (GHCParse.HsString _ _)) = Just x
  toStr x@(GHCParse.HsLit _ (GHCParse.HsStringPrim _ _)) = Just x
  toStr _ = Nothing

tupArgExpr :: (GHCParse.LHsTupArg GHCParse.GhcPs) -> [GHCParse.HsExpr GHCParse.GhcPs]
tupArgExpr (GHCParse.L _ (GHCParse.Present _ (GHCParse.L _ x))) = [x]
tupArgExpr _ = []

match :: Data a => a -> a -> Bool
match tmpl val = matchQ tmpl val

matchQ :: GenericQ (GenericQ Bool)
matchQ = (matchesSpecials :: (GHCParse.Pat GHCParse.GhcPs) -> (GHCParse.Pat GHCParse.GhcPs) -> Maybe Bool)
      ||| (matchesSpecials :: (GHCParse.HsExpr GHCParse.GhcPs) -> (GHCParse.HsExpr GHCParse.GhcPs) -> Maybe Bool)
      ||| matchesWildcard
      ||| structuralEq

matchesSpecials :: Template a => a -> a -> Maybe Bool
matchesSpecials (toParens -> Just x) y = Just (matchQ x y)
matchesSpecials x (toParens -> Just y) = Just (matchQ x y)
matchesSpecials (toSplice -> Just (GHCParse.HsTypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsApp _ op (GHCParse.L _ (GHCParse.HsBracket _ (fromBracket -> Just tmpl))))))) x = 
  case op of
        (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))) ->
            case idName id of
              "tupleOf" -> case toTuple x of Just xs -> Just (all (match tmpl) xs); Nothing -> Just False
              "contains" -> Just (everything (||) (mkQ False (match tmpl)) x)
              _ -> Nothing
        _ -> Nothing
matchesSpecials (toSplice -> Just (GHCParse.HsUntypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsApp _ op (GHCParse.L _ (GHCParse.HsBracket _ (fromBracket -> Just tmpl))))))) x = 
  case op of
        (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))) ->
            case idName id of
              "tupleOf" -> case toTuple x of Just xs -> Just (all (match tmpl) xs); Nothing -> Just False
              "contains" -> Just (everything (||) (mkQ False (match tmpl)) x)
              _ -> Nothing
        _ -> Nothing
matchesSpecials (toSplice -> Just (GHCParse.HsTypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsApp _ op (GHCParse.L _ (GHCParse.ExplicitList _ _ (sequence . map (\(GHCParse.L _ (GHCParse.HsBracket _ b)) -> fromBracket b) -> Just xs))))))) x = 
  case op of
        (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))) ->
            case idName id of
              "allOf" -> Just (all (flip match x) xs)
              "anyOf" -> Just (any (flip match x) xs)
              "noneOf" -> Just (not (any (flip match x) xs))
        _ -> Nothing
matchesSpecials (toSplice -> Just (GHCParse.HsUntypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsApp _ op (GHCParse.L _ (GHCParse.ExplicitList _ _ (sequence . map (\(GHCParse.L _ (GHCParse.HsBracket _ b)) -> fromBracket b) -> Just xs))))))) x = 
  case op of
        (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))) ->
            case idName id of
              "allOf" -> Just (all (flip match x) xs)
              "anyOf" -> Just (any (flip match x) xs)
              "noneOf" -> Just (not (any (flip match x) xs))
        _ -> Nothing
matchesSpecials (toSplice -> Just (GHCParse.HsTypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))))) x =
  case idName id of
    "any" -> Just True
    "var" -> case toVar x of Just _ -> Just True; Nothing -> Just False
    "con" -> case toCon x of Just _ -> Just True; Nothing -> Just False
    "lit" -> case toLit x of Just _ -> Just True; Nothing -> Just False
    "num" -> case toNum x of Just _ -> Just True; Nothing -> Just False
    "char" -> case toChar x of Just _ -> Just True; Nothing -> Just False
    "str" -> case toStr x of Just _ -> Just True; Nothing -> Just False
    _ -> Nothing
matchesSpecials (toSplice -> Just (GHCParse.HsUntypedSplice _ _ _ (GHCParse.L _ (GHCParse.HsVar _ (GHCParse.L _ id))))) x =
  case idName id of
    "any" -> Just True
    "var" -> case toVar x of Just _ -> Just True; Nothing -> Just False
    "con" -> case toCon x of Just _ -> Just True; Nothing -> Just False
    "lit" -> case toLit x of Just _ -> Just True; Nothing -> Just False
    "num" -> case toNum x of Just _ -> Just True; Nothing -> Just False
    "char" -> case toChar x of Just _ -> Just True; Nothing -> Just False
    "str" -> case toStr x of Just _ -> Just True; Nothing -> Just False
    _ -> Nothing
matchesSpecials _ _ = Nothing

matchesWildcard :: GHCParse.IdP GHCParse.GhcPs -> GHCParse.IdP GHCParse.GhcPs -> Maybe Bool
matchesWildcard id _ | "_" `isPrefixOf` (idName id) && "_" `isSuffixOf` (idName id) = Just True
matchesWildcard _ _ = Nothing

structuralEq :: (Data a, Data b) => a -> b -> Bool
structuralEq x y = toConstr x == toConstr y && and (gzipWithQ matchQ x y)

(|||) :: (Typeable a, Typeable b, Typeable x)
    => (x -> x -> Maybe Bool)
    -> (a -> b -> Bool)
    -> (a -> b -> Bool)
f ||| g = \x y -> fromMaybe (g x y) (join (f <$> cast x <*> cast y))
infixr 0 |||

idName :: GHCParse.IdP GHCParse.GhcPs -> String
idName = GHCParse.occNameString . GHCParse.rdrNameOcc
