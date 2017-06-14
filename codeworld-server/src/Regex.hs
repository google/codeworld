{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

module Regex where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process
import           Text.Regex.TDFA
import           Text.Regex

import Util

filterOutput :: ByteString -> IO (Maybe ByteString)
filterOutput output = do 
    let out  = subRegex (mkRegex "\226\8364\162")   (C.unpack output) ""
    let out1 = subRegex (mkRegex "\226\8364\732")   out               ""
    let out2 = subRegex (mkRegex "\226\8364\8482")  out1              ""
    let out3 = subRegex (mkRegex "'")               out2              ""
    return  (Just $ C.pack out3)    
