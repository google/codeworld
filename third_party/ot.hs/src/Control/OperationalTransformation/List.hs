{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}

module Control.OperationalTransformation.List
  ( N (..)
  , Vector (..)
  , Operation (..)
  , apply
  , compose
  , TransformedPair (..)
  , transform
  ) where

data N = Z | S N deriving (Eq, Show)

data Vector :: * -> N -> * where
  EmptyV :: Vector a Z
  ConsV  :: a -> Vector a n -> Vector a (S n)

data Operation :: * -> N -> N -> * where
  EmptyOp  :: Operation a Z Z
  RetainOp :: Operation a n m -> Operation a (S n) (S m)
  InsertOp :: a -> Operation a n m -> Operation a n (S m)
  DeleteOp :: Operation a n m -> Operation a (S n) m

apply :: Operation a n m -> Vector a n -> Vector a m
apply EmptyOp         EmptyV       = EmptyV
apply (RetainOp o')   (ConsV x xs) = ConsV x (apply o' xs)
apply (InsertOp x o') xs           = ConsV x (apply o' xs)
apply (DeleteOp o')   (ConsV _ xs) = apply o' xs
apply _               _            = error "not possible!"

addDeleteOp :: Operation a n m -> Operation a (S n) m
addDeleteOp (InsertOp x o') = InsertOp x (addDeleteOp o')
addDeleteOp o               = DeleteOp o

compose :: Operation a n m -> Operation a m k -> Operation a n k
compose EmptyOp         EmptyOp         = EmptyOp
compose (DeleteOp a')   b               = addDeleteOp (compose a' b)
compose a               (InsertOp x b') = InsertOp x (compose a b')
compose (RetainOp a')   (RetainOp b')   = RetainOp (compose a' b')
compose (RetainOp a')   (DeleteOp b')   = addDeleteOp (compose a' b')
compose (InsertOp x a') (RetainOp b')   = InsertOp x (compose a' b')
compose (InsertOp _ a') (DeleteOp b')   = compose a' b'
compose _               _               = error "not possible!"

data TransformedPair :: * -> N -> N -> * where
  TP :: Operation a n k -> Operation a m k -> TransformedPair a n m

transform :: Operation a n m -> Operation a n k -> TransformedPair a k m
transform EmptyOp         EmptyOp         = TP EmptyOp EmptyOp
transform (InsertOp x a') b               = case transform a' b  of TP at bt -> TP (InsertOp x at)  (RetainOp bt)
transform a               (InsertOp x b') = case transform a  b' of TP at bt -> TP (RetainOp at)    (InsertOp x bt)
transform (RetainOp a')   (RetainOp b')   = case transform a' b' of TP at bt -> TP (RetainOp at)    (RetainOp bt)
transform (DeleteOp a')   (DeleteOp b')   = transform a' b'
transform (RetainOp a')   (DeleteOp b')   = case transform a' b' of TP at bt -> TP at               (addDeleteOp bt)
transform (DeleteOp a')   (RetainOp b')   = case transform a' b' of TP at bt -> TP (addDeleteOp at) bt
transform _               _               = error "not possible!"