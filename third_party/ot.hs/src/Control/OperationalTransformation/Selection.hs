{-# LANGUAGE CPP, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.OperationalTransformation.Selection
  ( Range (..)
  , Selection (..)
  , createCursor
  , size
  , somethingSelected
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Text
import Data.Aeson
import Control.Applicative
import Data.Monoid
import Data.List (sort)
import qualified Data.Text as T
#if MIN_VERSION_ghc(7,8,0)
import GHC.Exts (IsList (..))
#endif

-- | Range has `anchor` and `head` properties, which are zero-based indices into
-- the document. The `anchor` is the side of the selection that stays fixed,
-- `head` is the side of the selection where the cursor is. When both are
-- equal, the range represents a cursor.
data Range = Range { rangeAnchor :: !Int, rangeHead :: !Int }
  deriving (Show, Read, Eq, Ord)

instance ToJSON Range where
  toJSON (Range a h) = object [ "anchor" .= a, "head" .= h ]

instance FromJSON Range where
  parseJSON (Object o) = Range <$> o .: "anchor" <*> o .: "head"
  parseJSON _ = fail "expected an object"

instance OTCursor Range TextOperation where
  updateCursor (TextOperation actions) (Range a h) = Range a' h'
    where
      a' = updateComponent a
      h' = if a == h then a' else updateComponent h
      updateComponent c = loop c c actions
      loop :: Int -> Int -> [Action] -> Int
      loop oldIndex newIndex as
        | oldIndex < 0 = newIndex
        | otherwise =
          case as of
            (op:ops) -> case op of
              Retain r -> loop (oldIndex-r) newIndex ops
              Insert i -> loop oldIndex (newIndex + T.length i) ops
              Delete d -> loop (oldIndex-d) (newIndex - min oldIndex d) ops
            _ -> newIndex -- matching on `[]` gives a non-exhaustive pattern
                          -- match warning for some reason

-- | A selection consists of a list of ranges. Each range may represent a
-- selected part of the document or a cursor in the document.
newtype Selection = Selection { ranges :: [Range] }
  deriving (Monoid, Show, Read)

instance OTCursor Selection TextOperation where
  updateCursor op = Selection . updateCursor op . ranges

instance Eq Selection where
  Selection rs1 == Selection rs2 = sort rs1 == sort rs2

instance Ord Selection where
  Selection rs1 `compare` Selection rs2 = sort rs1 `compare` sort rs2

instance ToJSON Selection where
  toJSON (Selection rs) = object [ "ranges" .= rs ]

instance FromJSON Selection where
  parseJSON (Object o) = Selection <$> o .: "ranges"
  parseJSON _ = fail "expected an object"

#if MIN_VERSION_ghc(7,8,0)
instance IsList Selection where
  type Item Selection = Range
  fromList = Selection
  toList = ranges
#endif

-- | Create a selection that represents a cursor.
createCursor :: Int -> Selection
createCursor i = Selection [Range i i]

-- | Does the selection contain any characters?
somethingSelected :: Selection -> Bool
somethingSelected = any (\r -> rangeAnchor r /= rangeHead r) . ranges

-- | Number of selected characters
size :: Selection -> Int
size = sum . map (\r -> abs (rangeAnchor r - rangeHead r)) . ranges
