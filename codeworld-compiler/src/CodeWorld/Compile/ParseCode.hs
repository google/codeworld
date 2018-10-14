{-# LANGUAGE OverloadedStrings #-}

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

module CodeWorld.Compile.ParseCode (runCustomDiagnostics, hasOldStyleMain) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.Generics
import Data.List (sort)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.Exts
import Text.Regex.TDFA

type Diagnostic = (SrcSpanInfo, Level, String)
data Level = Info | Warning | Error deriving (Eq, Ord)

runCustomDiagnostics :: String -> ByteString -> (Bool, [String])
runCustomDiagnostics mode contents = (abort, msgs)
  where diags = sort $ dangerousSourceDiagnostics contents ++
                       oldStyleDiagnostics mode contents ++
                       parsedCodeDiagnostics mode contents
        worst = maximum (Info : map (\(_, level, _) -> level) diags)
        abort = worst >= Error
        msgs  = [ formatLocation loc ++ msg | (loc, _, msg) <- diags ]

dangerousSourceDiagnostics :: ByteString -> [Diagnostic]
dangerousSourceDiagnostics contents
  | matches contents ".*TemplateHaskell.*" ||
    matches contents ".*QuasiQuotes.*" ||
    matches contents ".*glasgow-exts.*"
    = [(noSrcSpan, Error, "Sorry, but your program uses forbidden language features.")]
  | otherwise = []

oldStyleDiagnostics :: String -> ByteString -> [Diagnostic]
oldStyleDiagnostics "codeworld" contents
  | hasOldStyleMain contents
    = [(noSrcSpan, Warning,
        "warning:\n" ++
        "\tPlease define 'program' instead of 'main'.\n" ++
        "\tDefining 'main' may stop working July 2019.")]
oldStyleDiagnostics _ _ = []

hasOldStyleMain :: ByteString -> Bool
hasOldStyleMain contents = matches contents "(^|\\n)main[ \\t]*="

parsedCodeDiagnostics :: String -> ByteString -> [Diagnostic]
parsedCodeDiagnostics "codeworld" contents = case result of
    ParseOk mod -> getErrors mod
    ParseFailed _ _ -> []  -- Fall through and use GHC's parse errors.
  where result = parseFileContentsWithMode mode (unpack (decodeUtf8 contents))
        mode = defaultParseMode { parseFilename = "program.hs" }
parsedCodeDiagnostics _ _ = []

matches :: ByteString -> ByteString -> Bool
matches txt pat = txt =~ pat

getErrors :: Module SrcSpanInfo -> [Diagnostic]
getErrors m =
    everything (++) (mkQ [] badExpApps) m ++
    everything (++) (mkQ [] badMatchApps) m ++
    everything (++) (mkQ [] badPatternApps) m ++
    everything (++) (mkQ [] varlessPatBinds) m

badExpApps :: Exp SrcSpanInfo -> [Diagnostic]
badExpApps (App loc _ e)
    | not (isGoodExpAppRhs e) = [(loc, Warning, errorMsg)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badExpApps _ = []

badMatchApps :: Match SrcSpanInfo -> [Diagnostic]
badMatchApps (Match loc _ pats _ _) =
    [(loc, Warning, errorMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badMatchApps _ = []

badPatternApps :: Pat SrcSpanInfo -> [Diagnostic]
badPatternApps (PApp loc _ pats) =
    [(loc, Warning, errorMsg) | p <- pats, not (isGoodPatAppRhs p)]
  where
    errorMsg = "warning: Missing parentheses in constructor application."
badPatternApps _ = []

isGoodExpAppRhs :: Exp l -> Bool
isGoodExpAppRhs (Paren _ _) = True
isGoodExpAppRhs (Tuple _ _ _) = True
isGoodExpAppRhs _ = False

isGoodPatAppRhs :: Pat l -> Bool
isGoodPatAppRhs (PParen _ _) = True
isGoodPatAppRhs (PTuple _ _ _) = True
isGoodPatAppRhs _ = False

varlessPatBinds :: Decl SrcSpanInfo -> [Diagnostic]
varlessPatBinds (PatBind loc pat _ _)
  | not (everything (||) (mkQ False isPatVar) pat) = [(loc, Error, errorMsg)]
  where errorMsg = "This definition doesn't define any variables.\n\t" ++
                   "Variables must begin with a lowercase letter."
varlessPatBinds _ = []

isPatVar :: Pat SrcSpanInfo -> Bool
isPatVar (PVar _ _) = True
isPatVar (PNPlusK _ _ _) = True
isPatVar (PAsPat _ _ _) = True
isPatVar _ = False

formatLocation :: SrcSpanInfo -> String
formatLocation (SrcSpanInfo s _)
  | fn == ""  = "program.hs:1:1: "
  | otherwise = "program.hs:" ++ show line ++ ":" ++ show col ++ ": "
  where
    fn = srcSpanFilename s
    line = srcSpanStartLine s
    col = srcSpanStartColumn s
