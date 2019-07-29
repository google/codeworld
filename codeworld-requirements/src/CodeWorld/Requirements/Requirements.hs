{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.Requirements.Requirements (checkRequirements) where

import CodeWorld.Requirements.Framework
import CodeWorld.Requirements.Checker.Eval
import CodeWorld.Requirements.Checker.Language
import CodeWorld.Requirements.Checker.Types
import Codec.Compression.Zlib
import Control.Exception
import Control.Monad
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B (toStrict, fromStrict)
import qualified Data.ByteString.Base64 as B64
import Data.Char
import Data.Either
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Language.Haskell.Exts
import System.IO.Unsafe
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import DynFlags
import ErrUtils
import HsSyn
import TcRnTypes

checkRequirements :: DynFlags -> Messages -> TcGblEnv -> HsModule GhcPs -> ByteString -> Maybe String
checkRequirements e c f m s = do
    let (sources, sdiags) = extractRequirementsSource s
        (reqs, rdiags) = extractRequirements sources
    if (not (null reqs)) then
        let results = map (handleRequirement e c f m s) reqs
            obfuscated = T.unpack (obfuscate (map snd sources))
        in Just $  "\n                      :: REQUIREMENTS ::\n" ++
                   "Obfuscated:\n\n    XREQUIRES" ++ obfuscated ++ "\n\n" ++
                   concat sdiags ++ concat rdiags ++ concat results ++
                   "                  :: END REQUIREMENTS ::\n"
        else Nothing

plainPattern :: Text
plainPattern = "{-+[[:space:]]*REQUIRES\\b((\n|[^-]|-[^}])*)-}"

codedPattern :: Text
codedPattern = "{-+[[:space:]]*XREQUIRES\\b((\n|[^-]|-[^}])*)-}"

extractRequirementsSource :: ByteString -> ([(SrcSpanInfo, Text)], [String])
extractRequirementsSource s = let
        src = decodeUtf8 s
        plain = extractSubmatches plainPattern src
        blocks = map (fmap deobfuscate) (extractSubmatches codedPattern src)
        diags = [ "warning: Coded requirements were corrupted.\n"
                | (spn, Nothing) <- blocks ]
        coded = [ (spn, rule) | (spn, Just block) <- blocks, rule <- block ]
    in ((plain ++ coded), diags)

extractSubmatches :: Text -> Text -> [(SrcSpanInfo, Text)]
extractSubmatches pattern src =
    [ (srcSpanFor src off len, T.take len (T.drop off src))
      | matchArray :: MatchArray <- src =~ pattern
      , rangeSize (bounds matchArray) > 1
      , let (off, len) = matchArray ! 1 ]

extractRequirements :: [(SrcSpanInfo, Text)] -> ([Requirement], [String])
extractRequirements sources = (reqs, diags)
  where results = [ parseRequirement ln col source
                    | (SrcSpanInfo spn _, source) <- sources
                    , let ln = srcSpanStartLine spn
                    , let col = srcSpanStartColumn spn ]
        diags = [ format loc err | Left err <- results | (loc, _) <- sources ]
        reqs =  [ req | Right req <- results ]
        format loc err = ("error: The requirement could not be understood:\n" ++ err ++ "\n")

handleRequirement :: DynFlags -> Messages -> TcGblEnv -> HsModule GhcPs -> ByteString -> Requirement -> String
handleRequirement e c f m s req = let
    desc = requiredDescription req
    (success, msgs) = evalRequirement e c f m s req
    label | success == Nothing   = "[?] " ++ desc ++ "\n"
          | success == Just True = "[Y] " ++ desc ++ "\n"
          | otherwise            = "[N] " ++ desc ++ "\n"
    in label ++ concat [ "      " ++ msg ++ "\n" | msg <- msgs ]

obfuscate :: [Text] -> Text
obfuscate = wrapWithPrefix 60 "\n    " . decodeUtf8 . B64.encode .
            B.toStrict . compress . B.fromStrict . encodeUtf8 . T.pack .
            show . map T.unpack

deobfuscate :: Text -> Maybe [Text]
deobfuscate = fmap (map T.pack . read . T.unpack . decodeUtf8) .
              partialToMaybe . B.toStrict . decompress . B.fromStrict .
              B64.decodeLenient . encodeUtf8 . T.filter (not . isSpace)

wrapWithPrefix :: Int -> Text -> Text -> Text
wrapWithPrefix n pre txt = T.concat (parts txt)
  where parts t | T.length t < n = [pre <> t]
                | otherwise = let (a, b) = T.splitAt n t
                              in pre <> a : parts b

partialToMaybe :: a -> Maybe a
partialToMaybe = (eitherToMaybe :: Either SomeException a -> Maybe a) .
                 unsafePerformIO . try . evaluate

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
