{-# LANGUAGE DataKinds, ConstraintKinds, KindSignatures, GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.OperationalTransformation.Properties
  ( ArbitraryFor (..)
  -- , TestableOTSystem
  -- , ArbitraryOTSystem
  , Nat (..), One, Two, Three
  , DocHistory (..)
  , ConcurrentDocHistories (..)
  , prop_compose_assoc
  , prop_apply_functorial
  , prop_transform_apply_comm
  , prop_transform_comm
  , prop_transform_compose_compat_l
  , prop_transform_compose_compat_r
  , prop_transform_functorial
  ) where

import Control.OperationalTransformation
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Property
import Control.Applicative ((<$>), (<*>))

{-
type ArbitraryOTSystem doc op =
  ( OTSystem doc op, OTComposableOperation op
  , Arbitrary doc, ArbitraryFor doc op --Arbitrary (GenOp doc op)
  , Show doc, Eq doc, Show op, Eq op
  )
-}

type TestableOTSystem doc op =
  ( OTSystem doc op, OTComposableOperation op
  --, Arbitrary doc, ArbitraryFor doc op --Arbitrary (GenOp doc op)
  , Show doc, Eq doc, Show op, Eq op
  )

class ArbitraryFor a b where
  arbitraryFor :: a -> Gen b

genOp :: (OTSystem doc op, ArbitraryFor doc op) => doc -> Gen (op, doc)
genOp doc = do
  op <- arbitraryFor doc
  case apply op doc of
    Left err -> fail err
    Right doc' -> return (op, doc')


data Nat = Z | S !Nat deriving (Eq, Show)

type One = S Z
type Two = S One
type Three = S Two

data DocHistory doc op :: Nat -> * where
  -- | Last state
  LS :: doc -> DocHistory doc op Z
  -- | Snapshot
  SS :: doc -> op -> DocHistory doc op n -> DocHistory doc op (S n)

deriving instance (Show doc, Show op) => Show (DocHistory doc op n)
deriving instance (Eq doc, Eq op) => Eq (DocHistory doc op n)

data ConcurrentDocHistories doc op n k =
  CDH (DocHistory doc op n) (DocHistory doc op k)

deriving instance (Show doc, Show op) => Show (ConcurrentDocHistories doc op n k)
deriving instance (Eq doc, Eq op) => Eq (ConcurrentDocHistories doc op n k)

{-
getCurrentState :: DocHistory doc op n -> doc
getCurrentState (LS doc) = doc
getCurrentState (SS _ _ dh) = getCurrentState dh

snocDocHistory :: DocHistory doc op n -> op -> doc -> DocHistory doc op (S n)
snocDocHistory (LS doc) op doc' = SS doc op (LS doc')
snocDocHistory (SS doc op dh) op' doc' = SS doc op (snocDocHistory dh op' doc')
-}

instance ArbitraryFor doc (DocHistory doc op Z) where
  arbitraryFor = return . LS

instance (OTSystem doc op, ArbitraryFor doc op, ArbitraryFor doc (DocHistory doc op n)) => ArbitraryFor doc (DocHistory doc op (S n)) where
  arbitraryFor doc = do
    (op, doc') <- genOp doc
    SS doc op <$> arbitraryFor doc'

instance (Arbitrary doc, ArbitraryFor doc (DocHistory doc op n)) => Arbitrary (DocHistory doc op n) where
  arbitrary = (arbitrary :: Gen doc) >>= arbitraryFor

instance (ArbitraryFor doc (DocHistory doc op n), ArbitraryFor doc (DocHistory doc op k)) => ArbitraryFor doc (ConcurrentDocHistories doc op n k) where
  arbitraryFor doc = CDH <$> arbitraryFor doc <*> arbitraryFor doc

instance (Arbitrary doc, ArbitraryFor doc (ConcurrentDocHistories doc op n k)) => Arbitrary (ConcurrentDocHistories doc op n k) where
  arbitrary = (arbitrary :: Gen doc) >>= arbitraryFor

(==?) :: (Eq a, Show a) => a -> a -> Result
a ==? b | a == b    = succeeded
        | otherwise = failed { reason = "expected " ++ show a ++ " to be " ++ show b }

eitherResult :: Either String a -> (a -> Result) -> Result
eitherResult (Left err) _ = failed { reason = err }
eitherResult (Right a) f  = f a

eitherProperty :: Either String a -> (a -> Property) -> Property
eitherProperty (Left err) _ = property $ failed { reason = err }
eitherProperty (Right res) prop = prop res

prop_compose_assoc
  :: TestableOTSystem doc op
  => DocHistory doc op Three
  -> Result
prop_compose_assoc (SS _doc a (SS _ b (SS _ c _))) =
  eitherResult (compose a b) $ \ab ->
  eitherResult (compose ab c) $ \abc1 ->
  eitherResult (compose b c) $ \bc ->
  eitherResult (compose a bc) $ \abc2 ->
  abc1 ==? abc2

-- | @(b ∘ a)(d) = a(b(d))@ where /a/ and /b/ are two consecutive operations
-- and /d/ is the initial document.
prop_apply_functorial
  :: TestableOTSystem doc op
  => DocHistory doc op Two
  -> Result
prop_apply_functorial (SS doc a (SS _ b (LS _))) =
  eitherResult (apply a doc) $ \doc' ->
  eitherResult (apply b doc') $ \doc''1 ->
  eitherResult (compose a b) $ \ab ->
  eitherResult (apply ab doc) $ \doc''2 ->
  doc''1 ==? doc''2

-- | @b'(a(d)) = a'(b(d))@ where /a/ and /b/ are random operations, /d/ is the
-- initial document and @(a', b') = transform(a, b)@.
prop_transform_apply_comm
  :: TestableOTSystem doc op
  => ConcurrentDocHistories doc op One One
  -> Result
prop_transform_apply_comm (CDH (SS _ a (LS docA)) (SS _ b (LS docB))) =
  eitherResult (transform a b) $ \(a', b') ->
  eitherResult (apply a' docB) $ \doc''1 ->
  eitherResult (apply b' docA) $ \doc''2 ->
  doc''1 ==? doc''2

-- | @b' ∘ a = a' ∘ b@ where /a/ and /b/ are random operations and
-- @(a', b') = transform(a, b)@. Note that this is a stronger property than
-- 'prop_transform_apply_comm', because 'prop_transform_comm' and
-- 'prop_apply_functorial' imply 'prop_transform_apply_comm'.
prop_transform_comm
  :: TestableOTSystem doc op
  => ConcurrentDocHistories doc op One One
  -> Result
prop_transform_comm (CDH (SS _ a _) (SS _ b _)) =
  eitherResult (transform a b) $ \(a', b') ->
  eitherResult (compose a b') $ \ab' ->
  eitherResult (compose b a') $ \ba' ->
  ab' ==? ba'

-- | Transformation is compatible with composition on the left. That is, if we
-- have two consecutive operations /a/ and /b/ and a concurrent operation /c/,
-- then it doesn't make a difference whether we transform /c/ against /a/ and
-- then against /b/ or transform /c/ against the composition of /a/ and /b/.
-- In other terms, @c'_1 = c'_2@ where @(_, c'_1) = transform(b ∘ a, c)@,
-- @(_, c') = transform(a, c)@ and @(_, c'_2) = transform(b, c')@.
prop_transform_compose_compat_l
  :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show op, Eq op)
  => (doc -> Gen op)
  -> Property
prop_transform_compose_compat_l genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  c <- genOperation doc
  return $ eitherProperty (apply a doc) $ \doc' -> property $ do
    b <- genOperation doc'
    let res = (,) <$> (snd <$> (compose a b >>= flip transform c))
                  <*> (snd <$> (transform a c >>= transform b . snd))
    return $ eitherProperty res $ \(c'_1, c'_2) ->
      property $ c'_1 ==? c'_2

-- | Transformation is compatible with composition on the /right/.
prop_transform_compose_compat_r
  :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show op, Eq op)
  => (doc -> Gen op)
  -> Property
prop_transform_compose_compat_r genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  c <- genOperation doc
  return $ eitherProperty (apply a doc) $ \doc' -> property $ do
    b <- genOperation doc'
    let res = (,) <$> (fst <$> (compose a b >>= transform c))
                  <*> (fst <$> (transform c a >>= flip transform b . fst))
    return $ eitherProperty res $ \(c'_1, c'_2) -> property $ c'_1 ==? c'_2

-- second functor axiom (F(f . g) = Ff . Fg) for F = transform c
prop_transform_functorial
  :: TestableOTSystem doc op
  => ConcurrentDocHistories doc op One Two
  -> Result
prop_transform_functorial (CDH (SS _ c _) (SS _ a (SS _ b _))) =
  eitherResult (compose a b) $ \ab ->
  eitherResult (transform c ab) $ \(_c''1, abPrimed1) ->
  eitherResult (transform c a) $ \(c', a') ->
  eitherResult (transform c' b) $ \(_c''2, b') ->
  eitherResult (compose a' b') $ \abPrimed2 ->
  abPrimed1 ==? abPrimed2
