{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module HaskellPrelude (module M, ifThenElse) where
import "base" Prelude as M
import "base" Data.String as M
ifThenElse a b c = if a then b else c
