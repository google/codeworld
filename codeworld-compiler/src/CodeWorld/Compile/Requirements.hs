{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.Compile.Requirements (checkRequirements) where

import CodeWorld.Compile.Framework
import CodeWorld.Compile.Requirements.Eval
import CodeWorld.Compile.Requirements.Language
import CodeWorld.Compile.Requirements.Types
import Codec.Compression.Zlib
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as B (fromStrict, toStrict)
import Data.Char
import Data.Either
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Language.Haskell.Exts
import System.IO.Unsafe
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

checkRequirements :: MonadCompile m => m ()
checkRequirements = do
  sources <- extractRequirementsSource
  reqs <- extractRequirements sources
  when (not (null reqs)) $ do
    results <- mapM handleRequirement reqs
    let obfuscated = T.unpack (obfuscate (map snd sources))
    addDiagnostics
      [ ( noSrcSpan,
          CompileSuccess,
          "                    :: REQUIREMENTS ::\n"
            ++ "Obfuscated:\n\n    XREQUIRES"
            ++ obfuscated
            ++ "\n\n"
            ++ concat results
            ++ "                  :: END REQUIREMENTS ::\n"
        )
      ]

plainPattern :: Text
plainPattern = "{-+[[:space:]]*REQUIRES\\b((\n|[^-]|-[^}])*)-}"

codedPattern :: Text
codedPattern = "{-+[[:space:]]*XREQUIRES\\b((\n|[^-]|-[^}])*)-}"

extractRequirementsSource :: MonadCompile m => m [(SrcSpanInfo, Text)]
extractRequirementsSource = do
  srcs <- gets compileSourcePaths
  fmap concat $ forM srcs $ \src -> do
    code <- decodeUtf8 <$> getSourceCode src
    let plain = extractSubmatches src plainPattern code
    let blocks = map (fmap deobfuscate) (extractSubmatches src codedPattern code)
    addDiagnostics
      [ (spn, CompileSuccess, "warning: Coded requirements were corrupted.")
        | (spn, Nothing) <- blocks
      ]
    let coded = [(spn, rule) | (spn, Just block) <- blocks, rule <- block]
    return (plain ++ coded)

extractSubmatches :: FilePath -> Text -> Text -> [(SrcSpanInfo, Text)]
extractSubmatches f pattern src =
  [ (srcSpanFor f src off len, T.take len (T.drop off src))
    | matchArray :: MatchArray <- src =~ pattern,
      rangeSize (bounds matchArray) > 1,
      let (off, len) = matchArray ! 1
  ]

extractRequirements :: MonadCompile m => [(SrcSpanInfo, Text)] -> m [Requirement]
extractRequirements sources = do
  addDiagnostics diags
  return reqs
  where
    results =
      [ parseRequirement ln col source
        | (SrcSpanInfo spn _, source) <- sources,
          let ln = srcSpanStartLine spn,
          let col = srcSpanStartColumn spn
      ]
    diags = [format loc err | Left err <- results | (loc, _) <- sources]
    reqs = [req | Right req <- results]
    format loc err =
      ( loc,
        CompileSuccess,
        "error: The requirement could not be understood:\n" ++ err
      )

handleRequirement :: MonadCompile m => Requirement -> m String
handleRequirement req = do
  let desc = requiredDescription req
  (success, msgs) <- evalRequirement req
  let label
        | success == Nothing = "[?] " ++ desc ++ "\n"
        | success == Just True = "[Y] " ++ desc ++ "\n"
        | otherwise = "[N] " ++ desc ++ "\n"
  return $ label ++ concat ["      " ++ msg ++ "\n" | msg <- msgs]

obfuscate :: [Text] -> Text
obfuscate =
  wrapWithPrefix 60 "\n    " . decodeUtf8 . B64.encode
    . B.toStrict
    . compress
    . B.fromStrict
    . encodeUtf8
    . T.pack
    . show
    . map T.unpack

deobfuscate :: Text -> Maybe [Text]
deobfuscate =
  fmap (map T.pack . read . T.unpack . decodeUtf8)
    . partialToMaybe
    . B.toStrict
    . decompress
    . B.fromStrict
    . B64.decodeLenient
    . encodeUtf8
    . T.filter (not . isSpace)

wrapWithPrefix :: Int -> Text -> Text -> Text
wrapWithPrefix n pre txt = T.concat (parts txt)
  where
    parts t
      | T.length t < n = [pre <> t]
      | otherwise =
        let (a, b) = T.splitAt n t
         in pre <> a : parts b

partialToMaybe :: a -> Maybe a
partialToMaybe =
  (eitherToMaybe :: Either SomeException a -> Maybe a)
    . unsafePerformIO
    . try
    . evaluate

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
