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

import Data.Generics
import Data.Generics.Twins
import Data.List
import Language.Haskell.Exts

match :: Data a => a -> a -> Bool
match tmpl val = matchQ tmpl val

matchQ :: GenericQ (GenericQ Bool)
matchQ tmpl val
  | Just _ <- cast tmpl :: Maybe SrcSpanInfo = True
  | Just t <- cast tmpl, Just v <- cast val, matchesAnyPat t v = True
  | Just t <- cast tmpl, Just v <- cast val, matchesAnyExp t v = True
  | otherwise = toConstr tmpl == toConstr val &&
                and (gzipWithQ matchQ tmpl val)

matchesAnyPat :: Pat SrcSpanInfo -> Pat SrcSpanInfo -> Bool
matchesAnyPat (PVar _ (Ident _ "__any")) _ = True
matchesAnyPat _ _ = False

matchesAnyExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Bool
matchesAnyExp (Var _ (UnQual _ (Ident _ "__any"))) _ = True
matchesAnyExp _ _ = False
