{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

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

import Data.Array (elems)
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

replace :: Text -> Text -> Text -> Text
replace regex replacement str =
    let parts = concatMap elems $ (str =~ regex :: [MatchArray])
    in foldl replaceOne str (reverse parts)
  where
    replaceOne :: Text -> (Int, Int) -> Text
    replaceOne str (start, len) = pre <> replacement <> post
      where
        pre = T.take start str
        post = T.drop (start + len) str

#endif
