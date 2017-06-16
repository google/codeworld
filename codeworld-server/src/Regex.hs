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
import 		 Data.Char
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process
import           Text.Regex.TDFA
import           Text.Regex

import Util


filterOutput :: ByteString -> Maybe ByteString
filterOutput output =  
    let out   = subRegex (mkRegex "\226\8364\162")   (C.unpack output) ""
        out1  = subRegex (mkRegex "\226\8364\732")   out               ""
        out2  = subRegex (mkRegex "\226\8364\8482")  out1              ""
        out3  = subRegex (mkRegex "'")               out2              ""
        out4  = subRegex (mkRegex "IO action main")  out3              "variable main"
        out5  = subRegex (mkRegex "module Main")     out4              "your program"
        out6  = subRegex (mkRegex "main:Main")       out5              "your program"
        out7  = subRegex (mkRegex "Couldn't match expected type Text\\s*with actual type GHC.Types.Char")
                out6 "Text requires double quotes, rather than single."
        out8  = subRegex (mkRegex "base-[0-9.]*:GHC\\.Stack\\.Types\\.HasCallStack => ") 
                out7 ""
        out9  = subRegex (mkRegex "GHC\\.Types\\.Char") out8 ""
        out10 = subRegex (mkRegex "codeworld-base[-.:_A-Za-z0-9]*") out9 "the standard library"
        out11 = subRegex (mkRegex "Main\\.")         out10             ""
        out12 = subRegex (mkRegex "main :: t")       out11             "main program"
        out13 = subRegex (mkRegex "Prelude\\.")      out12             ""
        out14 = subRegex (mkRegex "\\bBool\\b")      out13             "Truth"
        out15 = subRegex (mkRegex "IO \\(\\)")       out14             "Program"
        out16 = subRegex (mkRegex "IO [a-z][a-zA-Z0-9_]*") out15       "Program"
        out17 = subRegex (mkRegex "\\bBool\\b")      out16             "Truth"
        out18 = subRegex (mkRegex "[ ]*Perhaps you intended to use TemplateHaskell\n") out17 ""
        out19 = subRegex (mkRegex "imported from [^)\n]*") out18       "defined in the standard library"
        out20 = subRegex (mkRegex "the first argument")    out19       "the parameter(s)"
        out21 = subRegex (mkRegex "[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n") 
                out20 ""
        out22 = subRegex (mkRegex "[ ]*but its type .* has only .*\n") out21 ""
        out23 = subRegex (mkRegex "A data constructor of that name is in scope; did you mean DataKinds\\?") 
                out22 "That name refers to a value, not a type."
        out24 = subRegex (mkRegex "type constructor or class") out23   "type"
        out25 = subRegex (mkRegex "Illegal tuple section: use TupleSections") 
                out24 "This tuple is missing a value, or has an extra comma."
        out26 = subRegex (mkRegex "Use -v to see a list of the files searched for\\.") out25 ""
        out27 = subRegex (mkRegex "\n\\s+\n")        out26             "\n"
        out28 = subRegex (mkRegex "When checking that:\\s*[^\n]*\\s*is more polymorphic than:\\s*[^\n]*(\n\\s*)?")
                out27 ""
        out29 = subRegex (mkRegex "\\[GHC\\.Types\\.Char\\] -> ") out28 "\n"
        out30 = subRegex (mkRegex "\\(and originally defined in [^)]*\\)") out29 "\n"
        out31 = subRegex (mkRegex "in string\\/character literal") out30 "in text literal"
        out32 = subRegex (mkRegex "CallStack \\(from HasCallStack\\):") out31  "When Evaluating:" 
        out33 = subRegex (mkRegex "at src\\/[A-Za-z0-9\\/.:]*") out32 ""
        out34 = subRegex (mkRegex "GHC\\.[A-Za-z.]*(\\s|\n)*->( |\n)*") out33 ""
        out35 = subRegex (mkRegex "integer-gmp(-[0-9\\.]*)?:(.|\n)*?->( |\n)*") out34 ""
        out36 = subRegex (mkRegex "base(-[0-9.]*)?\\:(.|\n)*?->( |\n)*")        out35             "\n"
    in  (Just  (C.pack out36))    
