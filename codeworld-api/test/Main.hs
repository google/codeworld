{-# LANGUAGE LambdaCase #-}

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

import System.Process
import System.Directory
import System.FilePath
import CodeWorld.Compile
import System.IO.Temp (withSystemTempDirectory)

-- testTimeStepElision :: IO Bool
-- testTimeStepElision = do
--     let initial = 0
--         next = fullStepHandler initial
--     initialName <- makeStableName $! initial
--     nextName <- makeStableName $! next
--     return (next == initial) == (initialName == nextName)

source =
    "{-# LANGUAGE OverloadedStrings #-}\n" ++
    "import CodeWorld\n" ++
    "main :: IO ()\n" ++
    "main = interactionOf () (const id) (const id) picture\n" ++
    "picture :: () -> Picture\n" ++
    "picture () = trace \"foo\" $ solidCircle 10\n"

main = withSystemTempDirectory "test" $ \tmpdir -> do
        let outputFile = tmpdir </> "test.js"
        let sourceFile = tmpdir </> "source.hs"
        let errFile = tmpdir </> "err.txt"
        let stage = FullBuild outputFile
        writeFile sourceFile source
        compileSource stage sourceFile errFile "haskell" False >>= \case
            CompileSuccess -> do
                 result <- readProcess "nodejs" [outputFile] []
                 print result
            _ -> error "Compilation is unsuccessful."
