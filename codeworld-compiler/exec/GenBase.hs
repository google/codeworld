{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE PatternGuards     #-}

module GenBase where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Regex.TDFA.Text
import           Text.Regex.TDFA

generateBaseBundle :: FilePath -> [Text] -> String -> FilePath -> FilePath -> IO ()
generateBaseBundle hoogleDB blacklist mode mainFile baseFile = do
    (imports, exprs) <- readHoogleDB hoogleDB blacklist
    let defs    = [ "d" <> T.pack (show i) <> " = " <> e
                    | i <- [ 0 :: Int ..]
                    | e <- exprs ]
        src     = T.unlines ("module LinkBase where" : imports ++ defs)
        mainDef = case mode of
            "codeworld" -> "program = drawingOf(blank)"
            _           -> "main = return ()"
    T.writeFile baseFile src
    T.writeFile mainFile $
        T.unlines ["module Main where", "import LinkBase", mainDef]

readHoogleDB :: FilePath -> [Text] -> IO ([Text], [Text])
readHoogleDB file blacklist = do
    lns <- T.lines <$> T.readFile file
    return (parseHoogleDB blacklist Nothing lns)

parseHoogleDB :: [Text] -> Maybe Text -> [Text] -> ([Text], [Text])
parseHoogleDB blacklist _ (t:ts)
    | Just mod <- submatch t "^module ([A-Za-z0-9._']+)"
    , not (mod `elem` blacklist) =
        let (i, e) = parseHoogleDB blacklist (Just mod) ts
        in  ("import qualified " <> mod : i, e)
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
submatch t pat | [_, match] <- getAllTextSubmatches (t =~ pat) = Just match
               | otherwise                                     = Nothing
