{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Internal.Text (
    Text,
    fromString,
    empty,
    T.append,
    (<>),
    appendAll,
    numberOfCharacters,
    numberOfWords,
    numberOfLines,
    characters,
    T.lines,
    T.words,
    T.unlines,
    T.unwords,
    show,
    T.intercalate,
    T.replace,
    T.toLower,
    T.toUpper,
    T.strip,
    T.stripPrefix,
    T.stripSuffix,
    search,
    substring
    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool, (.))

import Data.Text (Text)
import qualified Data.Text as T

import Internal.Num

fromString :: P.String -> Text
fromString = T.pack

numberOfCharacters :: Text -> Number
numberOfCharacters = fromInt . T.length

numberOfWords :: Text -> Number
numberOfWords = fromInt . P.length . T.words

numberOfLines :: Text -> Number
numberOfLines = fromInt . P.length . T.lines

characters :: Text -> [Text]
characters = P.map T.singleton . T.unpack

infixr 6 <>
(<>) :: Text -> Text -> Text
(<>) = T.append

appendAll :: [Text] -> Text
appendAll = T.concat

empty :: Text -> Bool
empty = T.null

show :: Number -> Text
show = T.pack . P.show

search :: Text -> Text -> [Number]
search needle haystack = P.map (fromInt . T.length . P.fst) (T.breakOnAll needle haystack)

substring :: Number -> Number -> Text -> Text
substring start length = T.take (toInt length) . T.drop (toInt start)
