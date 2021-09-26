{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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
import "ghc" RdrHsSyn
import "ghc" GHC.Hs
import "ghc" OccName
import "ghc" RdrName
import "ghc" SrcLoc

class (Data a, Typeable a) => Template a where
  toSplice :: a -> Maybe (HsSplice GhcPs)
  fromBracket :: (HsBracket GhcPs) -> Maybe a

  toParens :: a -> Maybe a
  toTuple :: a -> Maybe [a]
  toVar :: a -> Maybe a
  toCon :: a -> Maybe a
  toLit :: a -> Maybe a
  toNum :: a -> Maybe a
  toChar :: a -> Maybe a
  toStr :: a -> Maybe a

instance Template (Pat GhcPs) where
  toSplice (SplicePat _ s) = Just s
  toSplice _ = Nothing

  fromBracket (PatBr _ (L _ p)) = Just p
  fromBracket _ = Nothing

  toParens (ParPat _ (L _ x)) = Just x
  toParens _ = Nothing

  toTuple (TuplePat _ ps _) = Just [p | L _ p <- ps]
  toTuple _ = Nothing

  toVar x@(VarPat _ _) = Just x
  toVar _ = Nothing

  toCon x@(ConPatIn _ _) = Just x
  toCon x@(ConPatOut {}) = Just x
  toCon _ = Nothing

  toLit x@(LitPat _ _) = Just x
  toLit _ = Nothing

  toNum x@(LitPat _ (HsInt _ _)) = Just x
  toNum x@(LitPat _ (HsInteger _ _ _)) = Just x
  toNum x@(LitPat _ (HsRat _ _ _)) = Just x
  toNum x@(LitPat _ (HsIntPrim _ _)) = Just x
  toNum x@(LitPat _ (HsWordPrim _ _)) = Just x
  toNum x@(LitPat _ (HsInt64Prim _ _)) = Just x
  toNum x@(LitPat _ (HsWord64Prim _ _)) = Just x
  toNum x@(LitPat _ (HsFloatPrim _ _)) = Just x
  toNum x@(LitPat _ (HsDoublePrim _ _)) = Just x
  toNum _ = Nothing

  toChar x@(LitPat _ (HsChar _ _)) = Just x
  toChar x@(LitPat _ (HsCharPrim _ _)) = Just x
  toChar _ = Nothing

  toStr x@(LitPat _ (HsString _ _)) = Just x
  toStr x@(LitPat _ (HsStringPrim _ _)) = Just x
  toStr _ = Nothing

instance Template (HsExpr GhcPs) where
  toSplice (HsSpliceE _ s) = Just s
  toSplice _ = Nothing

  fromBracket (ExpBr _ (L _ e)) = Just e
  fromBracket _ = Nothing

  toParens (HsPar _ (L _ x)) = Just x
  toParens _ = Nothing

  toTuple (ExplicitTuple _ args _) = Just (concat $ tupArgExpr <$> args)
  toTuple _ = Nothing

  toVar x@(HsVar _ _) = Just x
  toVar _ = Nothing

  toCon x@(HsConLikeOut _ _) = Just x
  toCon _ = Nothing

  toLit x@(HsLit _ _) = Just x
  toLit x@(NegApp _ (L _ (HsLit _ _)) _) = Just x
  toLit _ = Nothing

  toNum x@(HsLit _ (HsInt _ _)) = Just x
  toNum x@(HsLit _ (HsInteger _ _ _)) = Just x
  toNum x@(HsLit _ (HsRat _ _ _)) = Just x
  toNum x@(HsLit _ (HsIntPrim _ _)) = Just x
  toNum x@(HsLit _ (HsWordPrim _ _)) = Just x
  toNum x@(HsLit _ (HsInt64Prim _ _)) = Just x
  toNum x@(HsLit _ (HsWord64Prim _ _)) = Just x
  toNum x@(HsLit _ (HsFloatPrim _ _)) = Just x
  toNum x@(HsLit _ (HsDoublePrim _ _)) = Just x
  toNum x@(NegApp _ (L _ (toNum -> Just _)) _) = Just x
  toNum _ = Nothing

  toChar x@(HsLit _ (HsChar _ _)) = Just x
  toChar x@(HsLit _ (HsCharPrim _ _)) = Just x
  toChar _ = Nothing

  toStr x@(HsLit _ (HsString _ _)) = Just x
  toStr x@(HsLit _ (HsStringPrim _ _)) = Just x
  toStr _ = Nothing

tupArgExpr :: (LHsTupArg GhcPs) -> [HsExpr GhcPs]
tupArgExpr (L _ (Present _ (L _ x))) = [x]
tupArgExpr _ = []

match :: Data a => a -> a -> Bool
match tmpl val = matchQ tmpl val

matchQ :: GenericQ (GenericQ Bool)
matchQ =
  matchesGhcPs
    ||| (matchesSpecials :: (Pat GhcPs) -> (Pat GhcPs) -> Maybe Bool)
    ||| (matchesSpecials :: (HsExpr GhcPs) -> (HsExpr GhcPs) -> Maybe Bool)
    ||| matchesWildcard
    ||| mismatchedNames
    ||| structuralEq

matchesGhcPs :: GhcPs -> GhcPs -> Maybe Bool
matchesGhcPs _ _ = Just True

matchesSpecials :: Template a => a -> a -> Maybe Bool
matchesSpecials (toParens -> Just x) y = Just (matchQ x y)
matchesSpecials x (toParens -> Just y) = Just (matchQ x y)
matchesSpecials
  ( toSplice ->
      Just (HsTypedSplice _ _ _ (L _ (HsApp _ op (L _ (HsBracket _ (fromBracket -> Just tmpl))))))
    )
  x =
    matchBrackets op tmpl x
matchesSpecials
  ( toSplice ->
      Just (HsUntypedSplice _ _ _ (L _ (HsApp _ op (L _ (HsBracket _ (fromBracket -> Just tmpl))))))
    )
  x =
    matchBrackets op tmpl x
matchesSpecials
  ( toSplice ->
      Just (HsTypedSplice _ _ _ (L _ (HsApp _ op (L _ (ExplicitList _ _ (sequence . map (\(L _ (HsBracket _ b)) -> fromBracket b) -> Just xs))))))
    )
  x =
    matchLogical op xs x
matchesSpecials
  ( toSplice ->
      Just (HsUntypedSplice _ _ _ (L _ (HsApp _ op (L _ (ExplicitList _ _ (sequence . map (\(L _ (HsBracket _ b)) -> fromBracket b) -> Just xs))))))
    )
  x =
    matchLogical op xs x
matchesSpecials
  ( toSplice ->
      Just (HsTypedSplice _ _ _ (L _ (HsVar _ (L _ id))))
    )
  x =
    matchSimple id x
matchesSpecials
  ( toSplice ->
      Just (HsUntypedSplice _ _ _ (L _ (HsVar _ (L _ id))))
    )
  x =
    matchSimple id x
matchesSpecials _ _ = Nothing

matchBrackets :: Template a => LHsExpr GhcPs -> a -> a -> Maybe Bool
matchBrackets op tmpl x = case op of
  (L _ (HsVar _ (L _ id))) ->
    case idName id of
      "tupleOf" -> case toTuple x of Just xs -> Just (all (match tmpl) xs); Nothing -> Just False
      "contains" -> Just (everything (||) (mkQ False (match tmpl)) x)
      _ -> Nothing
  _ -> Nothing

matchLogical :: Template a => LHsExpr GhcPs -> [a] -> a -> Maybe Bool
matchLogical op xs x = case op of
  (L _ (HsVar _ (L _ id))) ->
    case idName id of
      "allOf" -> Just (all (flip match x) xs)
      "anyOf" -> Just (any (flip match x) xs)
      "noneOf" -> Just (not (any (flip match x) xs))
      _ -> Nothing
  _ -> Nothing

matchSimple :: Template a => IdP GhcPs -> a -> Maybe Bool
matchSimple id x = case idName id of
  "any" -> Just True
  "var" -> case toVar x of Just _ -> Just True; Nothing -> Just False
  "con" -> case toCon x of Just _ -> Just True; Nothing -> Just False
  "lit" -> case toLit x of Just _ -> Just True; Nothing -> Just False
  "num" -> case toNum x of Just _ -> Just True; Nothing -> Just False
  "char" -> case toChar x of Just _ -> Just True; Nothing -> Just False
  "str" -> case toStr x of Just _ -> Just True; Nothing -> Just False
  _ -> Nothing

matchesWildcard :: IdP GhcPs -> IdP GhcPs -> Maybe Bool
matchesWildcard id _ | "_" `isPrefixOf` (idName id) && "_" `isSuffixOf` (idName id) = Just True
matchesWildcard _ _ = Nothing

mismatchedNames :: IdP GhcPs -> IdP GhcPs -> Maybe Bool
mismatchedNames x y = if idName x /= idName y then Just False else Nothing

structuralEq :: (Data a, Data b) => a -> b -> Bool
structuralEq x y = toConstr x == toConstr y && and (gzipWithQ matchQ x y)

(|||) ::
  (Typeable a, Typeable b, Typeable x) =>
  (x -> x -> Maybe Bool) ->
  (a -> b -> Bool) ->
  (a -> b -> Bool)
f ||| g = \x y -> fromMaybe (g x y) (join (f <$> cast x <*> cast y))

infixr 0 |||

idName :: IdP GhcPs -> String
idName = occNameString . rdrNameOcc
