{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.OperationalTransformation.Text.Gen
  ( genOperation'
  , genOperation
  ) where

import Control.OperationalTransformation.Text
import Control.OperationalTransformation.Properties (ArbitraryFor (..))
import Test.QuickCheck hiding (Result)
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Data.Monoid ((<>))

genOperation' :: Int -> Gen TextOperation
genOperation' = fmap TextOperation . gen
  where
    maxLength = 32
    arbitraryText n = fmap (T.pack . take n) $ listOf1 (choose ('!', '~'))
    insert text [] = [Insert text]
    insert text (Insert text' : ops) = Insert (text <> text') : ops
    insert text ops@(Retain _ : _) = Insert text : ops
    insert text ops@(Delete _ : _) = Insert text : ops
    delete d [] = [Delete d]
    delete d (Insert text : ops) = Insert text : delete d ops
    delete d ops@(Retain _ : _) = Delete d : ops
    delete d (Delete d' : ops) = Delete (d+d') : ops
    retain r [] = [Retain r]
    retain r ops@(Insert _ : _) = Retain r : ops
    retain r (Retain r' : ops) = Retain (r+r') : ops
    retain r ops@(Delete _ : _) = Retain r : ops
    gen l =
      if l <= 0
      then oneof [return [], fmap ((:[]) . Insert) (arbitraryText maxLength)]
      else do
        len <- choose (1, min maxLength l)
        oneof [ retain len <$> gen (l - len)
              , do s2 <- arbitraryText len
                   insert s2 <$> gen l
              , delete len <$> gen (l - len)
              ]


genOperation :: T.Text -> Gen TextOperation
genOperation = genOperation' . T.length

instance ArbitraryFor T.Text TextOperation where
  arbitraryFor = genOperation

instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf (choose ('!', '~'))

instance Arbitrary TextOperation where
  arbitrary = arbitrary >>= genOperation
