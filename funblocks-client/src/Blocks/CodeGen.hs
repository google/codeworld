{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings, ScopedTypeVariables #-}

{-
  Copyright 2017 The CodeWorld Authors. All Rights Reserved.

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
import Prelude hiding ((<>), show)
import qualified Prelude as P
import Blockly.Block
import Data.JSString.Text
import Control.Monad.State.Lazy (get,put)
import qualified Control.Monad.State.Lazy as S
import qualified Blocks.Printer as PR
import Control.Monad
import Data.Monoid ((<>))

pack = textToJSString
unpack = textFromJSString
show :: Show a => a -> T.Text
show = T.pack . P.show

errc :: T.Text -> Block -> SaveErr Expr
errc msg block = SE Comment $ Just $ Error msg block

type Code = T.Text


class Pretty a where
  pretty :: a -> PR.Printer

-- instance Pretty Product where 
--  pretty (Product s tps) = s <> " " <> T.concat (map pretty tps)

instance Pretty Type where
  pretty (Type s) = PR.write_ s
  pretty (Sum typeName [] ) = PR.write_ $ "data " <> typeName -- Empty data declaration
  pretty (Sum typeName (tp:tps) ) = do 
                                      PR.write_ $ "data " <> typeName <> " ="
                                      c <- PR.getCol
                                      PR.write_ " "
                                      pretty tp
                                      mapM_ (\t -> PR.setCol (c-1) >> PR.writeLine "" >> PR.write "|" >> pretty t) tps

  pretty (Product s tps) = do PR.write_ s
                              when (length tps > 0) $ do
                                  PR.write_ "("
                                  PR.intercalation "," tps pretty 
                                  PR.write_ ")"
                              PR.write_ " "

  pretty (ListType t) = do 
                          PR.write_ "[" 
                          pretty t 
                          PR.write_ "]"

instance Pretty Expr where
  pretty (LiteralS s) = PR.write_ s
  pretty (LiteralN d) = PR.write_ $ if d == fromInteger (truncate d)
                                    then show $ truncate d
                                    else show d
              
  pretty (LocalVar name) = PR.write_ $ name
  pretty (CallFunc name vars_) = do 
                                    PR.write_ name
                                    case vars_ of
                                      [] -> return ()
                                      _ -> do 
                                             PR.write_ "("
                                             PR.intercalation "," vars_ pretty 
                                             PR.write_ ")"
  pretty (CallConstr name vars_) = do 
                                    PR.write name
                                    case vars_ of
                                      [] -> return ()
                                      _ -> do 
                                        PR.write_ "("
                                        PR.intercalation "," vars_ pretty 
                                        PR.write_ ")"

  pretty (CallFuncInfix name left right) = do shouldParenth left 
                                              PR.makeSpace
                                              PR.write name -- SPACES between?
                                              PR.makeSpace
                                              shouldParenth right
          where
            getPrec (CallFuncInfix name _ _) = infixP name
            getPrec _ = 9
            cur = infixP name
            shouldParenth expr = let prec = getPrec expr in
                                 if prec < cur then parenthesize expr else pretty expr
            parenthesize expr = do
                            PR.write_ "("
                            pretty expr
                            PR.write_ ")"

  pretty (FuncDef name vars expr) = do 
                                      let varCode = if not $ null vars 
                                                    then "(" <> T.intercalate "," vars <> ")"
                                                    else ""
                                      PR.write_ name
                                      if null vars then return () else PR.write_ varCode
                                      PR.write_ " = "
                                      pretty expr

  pretty (If cond th el) = do
                             PR.push
                             PR.write "if"
                             pretty cond
                             PR.reset 
                             PR.write "then"
                             pretty th
                             PR.reset
                             PR.write "else"
                             pretty el
                             PR.pop
  pretty (Case expr rows) = do 
                              col <- PR.getCol
                              PR.write "case"
                              PR.makeSpace
                              pretty expr
                              PR.makeSpace
                              PR.write "of"
                              mapM_ (\(con, vars, expr) -> do PR.setCol (col+4)
                                                              PR.writeLine ""
                                                              PR.write con
                                                              PR.makeSpace
                                                              when (length vars > 0) $ do
                                                                PR.write_ "("
                                                                PR.write_ $ T.intercalate "," vars 
                                                                PR.write_ ")"
                                                              PR.write "->"
                                                              pretty expr) rows
                              PR.pop
  pretty (UserType tp) = pretty tp
  pretty (Tuple exprs) = do 
                          PR.write_ "("
                          PR.intercalation "," exprs pretty
                          PR.write_ ")"
  pretty (ListCreate exprs) = do 
                          PR.write_ "["
                          PR.intercalation "," exprs pretty
                          PR.write_ "]"
  pretty (ListSpec left right) = do 
                          PR.write_ "["
                          pretty left
                          PR.write_ " .. "
                          pretty right
                          PR.write_ "]"
  pretty (ListSpecStep left next right) = do 
                          PR.write_ "["
                          pretty left
                          PR.write_ ", "
                          pretty next
                          PR.write_ " .. "
                          pretty right
                          PR.write_ "]"
  pretty (ListComp act vars_ guards) =  do
                          PR.write_ "["
                          pretty act
                          PR.write_ " | "
                          PR.intercalation ", " vars_ (\(var,expr) -> PR.write_ var >> PR.write_ " <- " >> pretty expr) 
                          when (length guards > 0) $ do PR.write_ ","
                                                        PR.makeSpace
                                                        PR.intercalation ", " guards pretty
                          PR.write_ "]"
  pretty Comment = PR.write_ ""

infixP "-" = 6
infixP "+" = 6
infixP "*" = 7
infixP "/" = 7
infixP "^" = 8
infixP "&" = 8
infixP "<>" = 8
infixP _ = 9

workspaceToCode :: Workspace -> IO (Code,[Error])
workspaceToCode workspace = do
    topBlocks <- getTopBlocksTrue workspace >>= return . filter (not . isDisabled)
    let exprs = map blockToCode topBlocks
    let errors = map (\(SE code (Just e)) -> e) $
                 filter (\c -> case c of SE code Nothing -> False; _ -> True) exprs
    let code = T.intercalate "\n\n" $ map (\(SE expr _) -> PR.run $ pretty expr) exprs
    return (code,errors)
  where
    blockToCode :: Block -> SaveErr Expr
    blockToCode block = do 
      let blockType = getBlockType block 
      case M.lookup blockType blockCodeMap of
        Just func -> let (SE code err) = func block
                     in SE code err
        Nothing -> errc "No such block in CodeGen" block



