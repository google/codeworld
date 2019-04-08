{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}

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

module CodeWorld.Compile.Base (generateBaseBundle, baseVersion) where

import Data.Char
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Process
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

generateBaseBundle ::
       [FilePath] -> [Text] -> String -> FilePath -> FilePath -> IO ()
generateBaseBundle hoogleDBs blacklist mode mainFile baseFile = do
    (imports, exprs) <- readHoogleDBs hoogleDBs blacklist
    let defs =
            [ "d" <> T.pack (show i) <> " = " <> e
            | i <- [0 :: Int ..]
            | e <- exprs
            ]
        src = T.unlines ("module LinkBase where" : imports ++ defs)
        mainDef =
            case mode of
                "codeworld" -> "program = drawingOf(blank)"
                _ -> "main = return ()"
    T.writeFile baseFile src
    T.writeFile mainFile $
        T.unlines ["module Main where", "import LinkBase", mainDef]

readHoogleDBs :: [FilePath] -> [Text] -> IO ([Text], [Text])
readHoogleDBs files blacklist = foldMap (flip readHoogleDB blacklist) files

readHoogleDB :: FilePath -> [Text] -> IO ([Text], [Text])
readHoogleDB file blacklist = do
    lns <- T.lines <$> T.readFile file
    return (parseHoogleDB blacklist Nothing lns)

parseHoogleDB :: [Text] -> Maybe Text -> [Text] -> ([Text], [Text])
parseHoogleDB blacklist _ (t:ts)
    | Just mod <- submatch t "^module ([A-Za-z0-9._']+)"
    , not (mod `elem` blacklist) =
        let (i, e) = parseHoogleDB blacklist (Just mod) ts
        in ("import qualified " <> mod : i, e)
parseHoogleDB blacklist (Just mod) (t:ts)
    | Just ident <- submatch t "^([A-Za-z0-9_']+) :: .*"
    , not (ident `elem` blacklist) =
        let (i, e) = parseHoogleDB blacklist (Just mod) ts
        in (i, mod <> "." <> ident : e)
    | Just sym <- submatch t "^\\(([!#$%&*+./<=>?@\\\\^|-~]+)\\) :: .*"
    , not (sym `elem` blacklist) =
        let (i, e) = parseHoogleDB blacklist (Just mod) ts
        in (i, "(" <> mod <> "." <> sym <> ")" : e)
parseHoogleDB blacklist mmod (_:ts) = parseHoogleDB blacklist mmod ts
parseHoogleDB _ _ [] = ([], [])

submatch :: Text -> Text -> Maybe Text
submatch t pat
    | [_, match] <- getAllTextSubmatches (t =~ pat) = Just match
    | otherwise = Nothing

baseVersion :: IO Text
baseVersion = do
    (_, Just outh, _, pid) <-
        createProcess
            (shell "ghcjs-pkg list -v 2>&1 | sha256sum")
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = NoStream
            , close_fds = True
            }
    hash <- T.decodeUtf8 <$> B.takeWhile (/= fromIntegral (ord ' ')) <$> B.hGetContents outh
    waitForProcess pid
    return hash
