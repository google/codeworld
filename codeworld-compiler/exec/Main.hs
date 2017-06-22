{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}  

import System.Environment
import System.Directory

import qualified "codeworld-compiler" Compile as C

main = do
    a <- getArgs
    if length a <= 3
        then print "Insufficient args, atleast 4 required(source, output, error, buildargs)"
        else do
            fileExists <- doesFileExist (head a)
            if fileExists then do
                let src = head a
                    out = a !! 1 
                    err = a !! 2
                    arg = drop 3 a
                compileOutput <- extractSource src out err arg
                case compileOutput of
                    True -> putStrLn "Done please check your output file"
                    False -> putStrLn "Some error occoured while compiling please check the error file"
                else print "Wrong source file address please check it once again"

extractSource :: String -> String -> String -> [String] -> IO Bool
extractSource  source out err arg = doesFileExist source >>= \a ->
    if a then do
        res <- C.compileSource source out err arg
        return res
    else return False

