{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Control.OperationalTransformation.Text.Tests
  ( tests
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Text
import Control.OperationalTransformation.Properties

import Control.OperationalTransformation.Text.Gen

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Text as T
import Data.Binary (encode, decode)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson.Types hiding (Result)

deltaLength :: TextOperation -> Int
deltaLength (TextOperation ops) = sum (map len ops)
  where len (Retain _) = 0
        len (Insert i) = T.length i
        len (Delete d) = -d

prop_json_id :: TextOperation -> Bool
prop_json_id o = parseMaybe parseJSON (toJSON o) == Just o

prop_binary_id :: TextOperation -> Bool
prop_binary_id o = decode (encode o) == o

prop_apply_length :: T.Text -> Property
prop_apply_length doc = property $ do
  op <- genOperation doc
  return $ case apply op doc of
    Left _ -> False
    Right str' -> T.length str' == T.length doc + deltaLength op

prop_compose_length :: T.Text -> Property
prop_compose_length doc = property $ do
  a <- genOperation doc
  return $ case apply a doc of
    Left _ -> property rejected
    Right doc' -> property $ do
      b <- genOperation doc'
      return $ case compose a b of
        Left _ -> False
        Right ab -> deltaLength a + deltaLength b == deltaLength ab

prop_compose_well_formed :: T.Text -> Property
prop_compose_well_formed doc = property $ do
  a <- genOperation doc
  let Right doc' = apply a doc
  b <- genOperation doc'
  return $ case compose a b of
    Left _ -> False
    Right ab -> wellFormed ab

prop_transform_well_formed :: T.Text -> Property
prop_transform_well_formed doc = property $ do
  a <- genOperation doc
  b <- genOperation doc
  return $ case transform a b of
    Left _ -> False
    Right (a', b') -> wellFormed a' && wellFormed b'

wellFormed :: TextOperation -> Bool
wellFormed (TextOperation ops) = all (not . nullLength) ops
  where nullLength (Retain n) = n == 0
        nullLength (Insert i) = i == ""
        nullLength (Delete d) = d == 0

prop_invert :: T.Text -> Gen Bool
prop_invert doc = do
  op <- genOperation doc
  return $ case (,) <$> invertOperation op doc <*> apply op doc of
    Left _ -> False
    Right (invOp, doc') -> case apply invOp doc' of
      Left _ -> False
      Right doc2 -> doc2 == doc

tests :: Test
tests = testGroup "Control.OperationalTransformation.Text.Tests"
  [ testProperty "prop_json_id" prop_json_id
  , testProperty "prop_binary_id" prop_binary_id
  , testProperty "prop_compose_assoc" (prop_compose_assoc :: DocHistory T.Text TextOperation Three -> Result)
  , testProperty "prop_compose_apply" (prop_apply_functorial :: DocHistory T.Text TextOperation Two -> Result)
  , testProperty "prop_transform_apply_comm" (prop_transform_apply_comm :: ConcurrentDocHistories T.Text TextOperation One One -> Result)
  , testProperty "prop_transform_comm" (prop_transform_comm :: ConcurrentDocHistories T.Text TextOperation One One -> Result)
  -- prop_transform_compose_compat_l, prop_transform_compose_compat_r and prop_transform_functorial
  -- are /not/ supported.
  , testProperty "prop_apply_length" prop_apply_length
  , testProperty "prop_compose_length" prop_compose_length
  , testProperty "prop_compose_well_formed" prop_compose_well_formed
  , testProperty "prop_transform_well_formed" prop_transform_well_formed
  , testProperty "prop_invert" prop_invert
  ]
