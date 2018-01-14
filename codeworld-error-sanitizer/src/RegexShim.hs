{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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
module RegexShim
    ( replace
    ) where
#ifdef ghcjs_HOST_OS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.JSString (JSString)
import qualified Data.JSString as J

foreign import javascript unsafe
               "$3.replace(new RegExp($1, \"g\"), $2)" js_replace ::
               JSString -> JSString -> JSString -> JSString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace regex replacement str =
    j_to_b (js_replace (b_to_j regex) (b_to_j replacement) (b_to_j str))
  where
    j_to_b = C.pack . J.unpack
    b_to_j = J.pack . C.unpack
#else
import Data.Array (elems)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace regex replacement str =
    let parts = concatMap elems $ (str =~ regex :: [MatchArray])
    in foldl replaceOne str (reverse parts)
  where
    replaceOne :: ByteString -> (Int, Int) -> ByteString
    replaceOne str (start, len) = pre <> replacement <> post
      where
        pre = B.take start str
        post = B.drop (start + len) str
#endif
