{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings, ScopedTypeVariables #-}

{-
  Copyright 2016 The CodeWorld Authors. All Rights Reserved.

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
module Blocks.CodeGen (workspaceToCode)
  where


import Blocks.Parser
import Blockly.Workspace hiding (workspaceToCode)
import qualified Data.Map as M
import qualified Data.Text as T
import Prelude hiding ((++), show)
import qualified Prelude as P
import Blockly.Block
import Data.JSString.Text

(++) :: T.Text -> T.Text -> T.Text
a ++ b = a `T.append` b
pack = textToJSString
unpack = textFromJSString
show :: Show a => a -> T.Text
show = T.pack . P.show

errc :: T.Text -> Block -> SaveErr Expr
errc msg block = SE Comment $ Just $ Error msg block

type Code = T.Text

class Pretty a where
  pretty :: a -> T.Text

-- instance Pretty Product where 
--  pretty (Product s tps) = s ++ " " ++ T.concat (map pretty tps)

instance Pretty Type where
  pretty (Type s) = s
  pretty (Sum typeName ps) = let tps = map pretty ps 
                                 format = zipWith (++) (" = ":(repeat "      | ")) tps
                             in "data " ++ typeName ++ (T.intercalate "\n" format)
  pretty (Product s tps) = s ++ " " ++ T.unwords (map pretty tps)
  pretty (ListType t) = "[" ++ pretty t ++ "]"

instance Pretty Expr where
  pretty (LiteralS s) = escape s
  pretty (LiteralN d) = show d
  pretty (LocalVar name) = name
  pretty (CallFunc name vars_) = let vars = map pretty vars_
                                     varCode = if not $ null vars
                                               then "(" ++ T.intercalate "," vars ++ ")"
                                               else ""
                                in name ++ varCode
  pretty (CallFuncInfix name left right) = pretty left ++ " " ++ name ++ " " ++ pretty right
  pretty (FuncDef name vars expr) = let varCode = if not $ null vars 
                                                  then "(" ++ T.intercalate "," vars ++ ")"
                                                  else ""
                                    in name ++ varCode ++ " = " ++ (pretty expr) 
  pretty (If cond th el) = "If " ++ pretty cond ++ " then " ++ pretty th ++ " else " ++ pretty el 
  pretty (Case expr rows) = let entries = map (\(con, vars, expr) -> "let " ++ con ++ " " ++ T.unwords vars ++ pretty expr) rows
                            in  "case " ++ pretty expr ++ " of " ++ T.concat entries 
  pretty (UserType tp) = pretty tp
  pretty (Tuple exprs) = "(" ++ (T.intercalate "," $ map pretty exprs) ++ ")"
  pretty (ListCreate exprs) = "(" ++ (T.intercalate "," $ map pretty exprs) ++ ")"
  pretty (ListSpec left right) = "[" ++ (pretty left) ++ " .. " ++ (pretty right) ++ "]"
  pretty (ListComp act vars_ guards) = 
    let varCode = T.intercalate "," $ map (\(var,expr) -> var ++ " <- " ++ (pretty expr) ) vars_
        guardCode = T.intercalate "," $ map pretty guards
    in "[" ++ (pretty act) ++ " | " ++ varCode ++ (if T.null guardCode then "" else "," ++ guardCode)
                ++ "]" 

  pretty Comment = ""


workspaceToCode :: Workspace -> IO (Code,[Error])
workspaceToCode workspace = do
    topBlocks <- getTopBlocksTrue workspace >>= return . filter (not . isDisabled)
    let exprs = map blockToCode topBlocks
    let errors = map (\(SE code (Just e)) -> e) $
                 filter (\c -> case c of SE code Nothing -> False; _ -> True) exprs
    let code = T.intercalate "\n\n" $ map (\(SE expr _) -> pretty expr) exprs
    return (code,errors)
  where
    blockToCode :: Block -> SaveErr Expr
    blockToCode block = do 
      let blockType = getBlockType block 
      case M.lookup blockType blockCodeMap of
        Just func -> let (SE (code, oc) err) = func block
                     in SE code err
        Nothing -> errc "No such block in CodeGen" block


-- Helper functions

-- Escapes a string

escape :: T.Text -> T.Text
escape xs = T.pack $ escape' (T.unpack xs)
escape' :: String -> String
escape' xs = ("\""::String) P.++ (concatMap f xs :: String ) P.++ ("\""::String) where
    f :: Char -> String
    f ('\\'::Char) = "\\\\" :: String
    f ('\"'::Char) = "\\\"" :: String
    f x    = [x]



