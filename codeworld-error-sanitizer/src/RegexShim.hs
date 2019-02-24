{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

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
module RegexShim (replace) where

import Data.Text (Text)

#ifdef ghcjs_HOST_OS

import Data.JSString (JSString)
import Data.JSString.Text

foreign import javascript unsafe
               "$3.replace(new RegExp($1, \"g\"), $2)" js_replace ::
               JSString -> JSString -> JSString -> JSString

replace :: Text -> Text -> Text -> Text
replace regex replacement str = textFromJSString $
    js_replace (textToJSString regex)
               (textToJSString replacement)
               (textToJSString str)

#else

import Data.Array (bounds, elems, (!))
import Data.Char (isDigit)
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

getInserts :: Text -> [(Int, (Int, Int))]
getInserts replacement = case replacement =~ ("[\\][0-9]" :: Text) :: [MatchArray] of
    [] -> []
    groups -> fmap processInsertion groups
    where processInsertion group =
              case decimal $ T.take len $ T.drop (start + 1) replacement of
                  Right (index, _) -> (index, (start, len))
                  Left _ -> error $ T.unpack $ "Can not parse replacement groups in "
                                  <> replacement -- should never happen beause handled
                                                 -- by regex matching above
              where (start, len) = group ! 0

applyInserts :: Text -> Text -> MatchArray -> [(Int, (Int, Int))] -> Text
applyInserts str replacement matches ((index, (rstart, rlen)):inserts) =
    let pre = T.take rstart replacement
        rest = T.drop (rstart + rlen) replacement
        (sstart, slen) = matches ! index
        source = T.take slen $ T.drop sstart str
    in pre <> source <> applyInserts str rest matches inserts
applyInserts _ replacement _ [] = replacement

replace :: Text -> Text -> Text -> Text
replace regex replacement str = case str =~ regex of
    (matches: _) ->
        let (start, len) = matches ! 0
            pre = T.take start str
            post = T.drop (start + len) str
            inserts = getInserts replacement
        in pre <> applyInserts str replacement matches inserts <> post
    _ -> str

#endif
