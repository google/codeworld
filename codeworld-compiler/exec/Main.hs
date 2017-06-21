{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}  

import System.Environment
import System.Directory

import qualified "codeworld-compiler" Compile as C

main = do
    a <- getArgs
    if length a <= 3
        then print "Insufficient args, 4 arguments required(source, output, error, buildmode)"
        else do
            fileExists <- doesFileExist (head a)
            if fileExists then do
                let src = head a
                    out = a !! 1 
                    err = a !! 2
                    arg = drop 3 a
                compileOutput <- extractSource src out err arg
                case compileOutput of
                    True -> putStrLn "Done please check your output"
                    False -> putStrLn "Some error occoured while compiling"
                else print "Wrong file address please check it once again"

extractSource :: String -> String -> String -> [String] -> IO Bool
extractSource  source out err arg = doesFileExist source >>= \a ->
    if a then do
        res <- C.compileSource source out err arg
        return True
    else return False

