{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

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

module CodeWorld.Compile.Requirements.Matcher where

import Control.Monad
import Data.Generics
import Data.Generics.Twins
import Data.List
import Data.Maybe
import Language.Haskell.Exts

match :: Data a => a -> a -> Bool
match tmpl val = matchQ tmpl val

matchQ :: GenericQ (GenericQ Bool)
matchQ = matchesSrcSpanInfo ||| matchesPatParens ||| matchesExpParens
    ||| matchesPatWildcard ||| matchesExpWildcard ||| structuralEq

matchesSrcSpanInfo :: SrcSpanInfo -> SrcSpanInfo -> Maybe Bool
matchesSrcSpanInfo _ _ = Just True

matchesPatParens :: Pat SrcSpanInfo -> Pat SrcSpanInfo -> Maybe Bool
matchesPatParens (PParen _ a) b = Just (matchQ a b)
matchesPatParens a (PParen _ b) = Just (matchQ a b)
matchesPatParens a b = Nothing

matchesExpParens :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Maybe Bool
matchesExpParens (Paren _ a) b = Just (matchQ a b)
matchesExpParens a (Paren _ b) = Just (matchQ a b)
matchesExpParens a b = Nothing

matchesPatWildcard :: Pat SrcSpanInfo -> Pat SrcSpanInfo -> Maybe Bool
matchesPatWildcard (PVar _ (Ident _ "__any")) _ = Just True
matchesPatWildcard (PVar _ (Ident _ "__var")) p = Just (isVar p)
  where isVar (PVar _ _) = True
        isVar _ = False
matchesPatWildcard (PVar _ (Ident _ "__num")) p = Just (isNum p)
  where isNum (PLit _ _ (Int _ _ _)) = True
        isNum (PLit _ _ (Frac _ _ _)) = True
        isNum _ = False
matchesPatWildcard (PVar _ (Ident _ "__str")) p = Just (isStr p)
  where isStr (PLit _ _ (String _ _ _)) = True
        isStr _ = False
matchesPatWildcard (PVar _ (Ident _ "__char")) p = Just (isChr p)
  where isChr (PLit _ _ (Char _ _ _)) = True
        isChr _ = False
matchesPatWildcard (PApp _ (UnQual _ (Ident _ "TupleOf_")) pat) p = Just (allMatch pat p)
  where allMatch pat (PTuple _ _ pats) = all (matchQ pat) pats
        allMatch _ _ = False
matchesPatWildcard (PApp _ (UnQual _ (Ident _ "Contains_")) pat) p = Just (subMatch pat p)
  where subMatch pat = everything (||) (mkQ False (match pat))
matchesPatWildcard _ _ = Nothing

matchesExpWildcard :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Maybe Bool
matchesExpWildcard (Var _ (UnQual _ (Ident _ "__any"))) _ = Just True
matchesExpWildcard (Var _ (UnQual _ (Ident _ "__var"))) e = Just (isVar e)
  where isVar (Var _ _) = True
        isVar _ = False
matchesExpWildcard (Var _ (UnQual _ (Ident _ "__num"))) e = Just (isNum e)
  where isNum (Lit _ (Int _ _ _)) = True
        isNum (Lit _ (Frac _ _ _)) = True
        isNum _ = False
matchesExpWildcard (Var _ (UnQual _ (Ident _ "__str"))) e = Just (isStr e)
  where isStr (Lit _ (String _ _ _)) = True
        isStr _ = False
matchesExpWildcard (Var _ (UnQual _ (Ident _ "__char"))) e = Just (isChr e)
  where isChr (Lit _ (Char _ _ _)) = True
        isChr _ = False
matchesExpWildcard (App _ (Var _ (UnQual _ (Ident _ "tupleOf_"))) exp) e = Just (allMatch exp e)
  where allMatch exp (Tuple _ _ exps) = all (matchQ exp) exps
        allMatch _ _ = False
matchesExpWildcard (App _ (Var _ (UnQual _ (Ident _ "contains_"))) exp) e = Just (subMatch exp e)
  where subMatch exp = everything (||) (mkQ False (match exp))
matchesExpWildcard _ _ = Nothing

structuralEq :: (Data a, Data b) => a -> b -> Bool
structuralEq x y = toConstr x == toConstr y && and (gzipWithQ matchQ x y)

(|||) :: (Typeable a, Typeable b, Typeable x)
    => (x -> x -> Maybe Bool)
    -> (a -> b -> Bool)
    -> (a -> b -> Bool)
f ||| g = \x y -> fromMaybe (g x y) (join (f <$> cast x <*> cast y))
infixr 0 |||
