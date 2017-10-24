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

-- Parse the Blockly structure into a Expr

module Blocks.Parser ( Error(..)
                      ,SaveErr(..)
                      ,Expr(..)
                      ,Type(..)
                      ,blockCodeMap
                      ,getGenerationBlocks)
  where

import Blockly.Block
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString.Text
import Data.Maybe (fromJust)
import GHCJS.Marshal
import qualified JavaScript.Array as JA
import Unsafe.Coerce
import Data.List (intercalate)
import qualified Data.Text as T
import Prelude hiding ((<>), show)
import qualified Prelude as P
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import Debug.Trace
import Data.Monoid ((<>))

-- Helpers for converting Text
pack = textToJSString
unpack = textFromJSString
show :: Show a => a -> T.Text
show = T.pack . P.show

data Error = Error T.Text Block -- errorMsg, block

-- always return a, only return first error, monadic interface
data SaveErr a = SE a (Maybe Error)

instance Functor SaveErr where
  fmap = liftM

instance Applicative SaveErr where
  pure = return
  (<*>) = ap

instance Monad SaveErr where
    return a = SE a Nothing 
    (SE code Nothing) >>= f = f code
    (SE code err@(Just e)) >>= f = do
        case f code of
          (SE code_ Nothing) -> SE code_ err
          (SE code_ a) -> SE code_ err

push a = SE a Nothing
errc :: T.Text -> Block -> SaveErr Expr
errc msg block = SE Comment $ Just $ Error msg block
errs :: T.Text -> Block -> SaveErr T.Text
errs msg block = SE "" $ Just $ Error msg block

errDisc = errc "There's a block missing" -- error when searching for a connected block
errInfix = errc "This block needs two blocks to be connected" -- error in missing inputs in infix block
errMsgsFunc = "This block requires a function block to be connected" -- error for simulationOf, 
errFunc = errc errMsgsFunc
errFunc_ = errs errMsgsFunc

type ParserFunction = Block -> SaveErr Expr 

-- A simple AST that will be used to store the structure of the code
-- We aren't too worried about types at this point (for now)

data Expr = LiteralS T.Text
          | LiteralN Float
          | LocalVar T.Text
          | CallFunc T.Text [Expr] -- name(arg1, arg2,...)
          | CallConstr T.Text [Expr] -- RGBA a b c
          | CallFuncInfix T.Text Expr Expr -- left <> right
          | FuncDef T.Text [T.Text] Expr -- let name x y z = expr
          | If Expr Expr Expr -- if expr then expr else expr
          | Case Expr [(T.Text,[T.Text],Expr)] -- case expr of consName vars expr
          | Tuple [Expr]
          | UserType Type          
          | TypeName T.Text
          | ListCreate [Expr] -- [1,2,3]
          | ListSpec Expr Expr -- [left..right]
          | ListSpecStep Expr Expr Expr -- [left,mid..right]
          | ListComp Expr [(T.Text,Expr)] [Expr] -- [action | var <- expr, var <- expr, guards]
          | Comment
  deriving Show

data Type = Type T.Text
     | ListType Type -- [type] -- unfortunately its separate for the special syntax
     | Product T.Text [Type] 
     | Sum T.Text [Type]
  deriving Show

-- data Product = Product T.Text [Type] -- Constructor tp1*tp2*tp3

-- standard function block, automatically handles machinery for functions
-- assumes inputs are in correct order
sFuncBlock :: ParserFunction
sFuncBlock block = do
  let argNames = getValueInputNames block
  let funcName = getFunctionName block
  args <- mapM (\arg -> valueToExpr block arg) argNames
  return $ CallFunc funcName args

sInfixBlock :: ParserFunction
sInfixBlock block = do
  let argNames = getValueInputNames block
  let funcName = getFunctionName block
  if length argNames /= 2 then
    errInfix block
  else do
    args@(left:right:xs) <- mapM (\arg -> valueToExpr block arg) argNames
    return (CallFuncInfix funcName left right)


-- PROGRAMS --------------------------------------
blockDrawingOf :: ParserFunction
blockDrawingOf block = do 
      expr <- valueToExpr block "VALUE" 
      return $ FuncDef "program" [] $ CallFunc "drawingOf" [expr]

-- NUMBERS -------------------------------------------------------

blockNumber :: ParserFunction
blockNumber block = do 
    let arg = getFieldValue block "NUMBER"
    return $ LiteralN (read $ T.unpack arg) 

blockNumberPerc :: ParserFunction
blockNumberPerc block = do 
    let arg = getFieldValue block "NUMBER"
    let numb = (read (T.unpack arg)) * 0.01
    return $ LiteralN numb


-- PICTURES ----------------------------------------------
blockCombine :: ParserFunction
blockCombine block = do
  let c = getItemCount block
  vals <- mapM (\t -> valueToExpr block t) ["PIC" <> show i | i <- [0..c-1]]
  return $ case vals of
            [] -> CallFuncInfix "&" (CallFunc "blank" []) (CallFunc "blank" [])
            [x] -> CallFuncInfix "&" x (CallFunc "blank" [])
            _ -> foldr1 (CallFuncInfix "&") vals

-- TEXT --------------------------------------------------

-- Escapes a string

escape :: T.Text -> T.Text
escape xs = T.pack $ escape' (T.unpack xs)
escape' :: String -> String
escape' = P.show 

blockString :: ParserFunction
blockString block = do 
    let txt = getFieldValue block "TEXT" 
    return $ LiteralS $ escape txt 

blockConcat :: ParserFunction
blockConcat block = do
  let c = getItemCount block
  vals <- mapM (\t -> valueToExpr block t) ["STR" <> show i | i <- [0..c-1]]
  return $ case vals of
            [] -> CallFuncInfix "<>" (LiteralS "") (LiteralS "")
            [x] -> CallFuncInfix "<>" x (LiteralS "")
            _ -> foldr1 (CallFuncInfix "<>") vals

-- LOGIC ------------------------------------------

blockIf :: ParserFunction
blockIf block = do 
    ifexpr <- valueToExpr block "IF" 
    thenexpr <- valueToExpr block "THEN" 
    elseexpr <- valueToExpr block "ELSE" 
    return $ If ifexpr thenexpr elseexpr


-- Let function block with parameters
foreign import javascript unsafe "$1.arguments_"
  js_funcargs :: Block -> JA.JSArray

blockLetFunc :: ParserFunction
blockLetFunc block = do 
    let name = getFieldValue block "NAME" 
    let vars = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    expr <- valueToExpr block "RETURN" 
    return $ FuncDef name vars expr 

blockLetCall :: ParserFunction
blockLetCall block = do 
    let name = getFieldValue block "NAME" 
    let args = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    vals <- mapM (\t -> valueToExpr block t) ["ARG" <> show i | i <- [0..length args - 1]]
    return $ CallFunc name vals

blockLocalVar :: ParserFunction
blockLocalVar block = do 
    let name = getFieldValue block "NAME" 
    return $ CallFunc name []

-- ANIMATION
blockAnim :: ParserFunction
blockAnim block = do
        draw <- aux "FUNC"
        return $ FuncDef "program" [] $ CallFunc "animationOf" 
                                            [CallFunc draw []]
  where
    aux name = case getInputBlock block name of
                      Just inpBlock -> return $ getFunctionName inpBlock 
                      Nothing -> errFunc_ block

blockSimulation :: ParserFunction
blockSimulation block = do
        initial <- aux "INITIAL"
        step <- aux "STEP"
        draw <- aux "DRAW"
        return $ FuncDef "program" [] $ CallFunc "simulationOf" 
                                            [CallFunc initial [],CallFunc step [],CallFunc draw []]
  where
    aux name = case getInputBlock block name of
                      Just inpBlock -> return $ getFunctionName inpBlock 
                      Nothing -> errFunc_ block

blockInteraction :: ParserFunction
blockInteraction block = do
        initial <- aux "INITIAL"
        step <- aux "STEP"
        event <- aux "EVENT"
        draw <- aux "DRAW"
        return $ FuncDef "program" [] $ CallFunc "interactionOf" 
                                            [CallFunc initial [],CallFunc step [],
                                             CallFunc event [] ,CallFunc draw []]
  where
    aux name = case getInputBlock block name of
                      Just inpBlock -> return $ getFunctionName inpBlock 
                      Nothing -> errFunc_ block




-- COMMENT
blockComment :: ParserFunction
blockComment block = return $ Comment

-- Tuples
blockCreatePair :: ParserFunction
blockCreatePair block = do 
    first <- valueToExpr block "FIRST" 
    second <- valueToExpr block "SECOND" 
    return $ Tuple [first,second]

-- LISTS

blockCreateList :: ParserFunction
blockCreateList block = do
  let c = getItemCount block
  vals <- mapM (\t -> valueToExpr block t) ["ADD" <> show i | i <- [0..c-1]]
  return $ ListCreate vals

blockNumGen :: ParserFunction
blockNumGen block = do 
    left <- valueToExpr block "LEFT" 
    right <- valueToExpr block "RIGHT" 
    return $ ListSpec left right

blockNumGenStep :: ParserFunction
blockNumGenStep block = do
    left <- valueToExpr block "LEFT" 
    next <- valueToExpr block "NEXT"
    right <- valueToExpr block "RIGHT" 
    return $ ListSpecStep left next right

-- LIST COMPREHENSION
foreign import javascript unsafe "$1.varCount_"
  js_blockVarCount :: Block -> Int

foreign import javascript unsafe "$1.guardCount_"
  js_blockGuardCount :: Block -> Int

foreign import javascript unsafe "$1.vars_"
  js_blockVars :: Block -> JA.JSArray

-- ListComp Expr [(T.Text,Expr)] [Expr] -- [action | var <- expr, var <- expr, guards]
blockListComp :: ParserFunction
blockListComp block = do 
    let varCount = js_blockVarCount block
    let guardCount = js_blockGuardCount block
    let vars = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_blockVars block

    varCodes <- mapM (\t -> valueToExpr block t) ["VAR" <> show i | i <- [0..varCount-1]]
    guards <- mapM (\t -> valueToExpr block t) ["GUARD" <> show i | i <- [0..guardCount-1]]
    doCode <- valueToExpr block "DO" 

    return $ ListComp doCode (zip vars varCodes) guards

-- TYPES

foreign import javascript unsafe "$1.itemCount_"
  js_itemCount :: Block -> Int

blockUserType :: ParserFunction
blockUserType block = do 
    let name = getFieldValue block "NAME"
    return $ UserType $ Type name

blockListType :: ParserFunction
blockListType block = do 
    tp <- valueToExpr block "TP" 
    return $ UserType $ ListType $ toType tp 
  where
    toType (UserType tp) = tp
    toType _ = Type ""

blockConstructor :: ParserFunction
blockConstructor block = do 
    let name :: T.Text = getFieldValue block "NAME"
    let itemCount = js_itemCount block
    tps :: [Expr] <- mapM (\n -> valueToExpr block n) ["TP" <> show i | i <- [0..itemCount-1]] 
    let out :: Expr = CallConstr name tps
    return $ out

blockProduct :: ParserFunction
blockProduct block = do 
    let constructor = getFieldValue block "CONSTRUCTOR"
    let itemCount = js_itemCount block
    tps :: [Expr] <- mapM (\n -> valueToExpr block n) ["TP" <> show i | i <- [0..itemCount-1]] 
    return $ UserType $ Product constructor $ map toType tps 
  where
    toType (UserType tp) = tp
    toType _ = Type ""

blockSum :: ParserFunction
blockSum block = do 
    let typeName = getFieldValue block "NAME"
    let itemCount = js_itemCount block
    tps <- mapM (\n -> valueToExpr block n) ["PROD" <> show i | i <- [0..itemCount-1]] 
    return $ UserType $ Sum typeName (map toType tps)
  where
    toType (UserType tp) = tp
    toType _ = Type ""

-- CASE

foreign import javascript unsafe "$1.getInputVars($2)"
  js_getCaseInputVars :: Block -> Int -> JA.JSArray

foreign import javascript unsafe "$1.getInputConstructor($2)"
  js_getCaseInputConstructor :: Block -> Int -> JSString

blockCase:: ParserFunction
blockCase block = do 
    let name = getFieldValue block "NAME"
    let itemCount = js_itemCount block
    inp :: Expr <- valueToExpr block "INPUT" 
    outs <- mapM (\n -> valueToExpr block n) ["CS" <> show i | i <- [0..itemCount-1]] 
    let vars_ :: [[T.Text]] = map vars [0..itemCount-1]
    let cons_ :: [T.Text] = map con [0..itemCount-1]
    return $ Case inp (zip3 cons_ vars_ outs :: [(T.Text,[T.Text],Expr)] ) 
  where
    vars i = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                  JA.toList $ js_getCaseInputVars block i
    con i = unpack $ js_getCaseInputConstructor block i

getGenerationBlocks :: [T.Text]
getGenerationBlocks = M.keys blockCodeMap

blockCodeMap = M.fromList $ concat [regularBlocks, specialBlocks, infixBlocks]

regularBlockNames = 
                  [  -- PROGRAMS 
                  "cwBlank"
                  ,"cwCoordinatePlane"
                  ,"cwCodeWorldLogo"
                  ,"cwText"
                  ,"cwCircle"
                  ,"cwThickCircle"
                  ,"cwSolidCircle"
                  ,"cwRectangle"
                  ,"cwThickRectangle"
                  ,"cwSolidRectangle"
                  ,"cwArc"
                  ,"cwSector"
                  ,"cwThickArc"
                  -- TRANSFORMATIONS
                  ,"lists_pictures"
                  ,"cwColored"
                  ,"cwTranslate"
                  ,"cwRotate"
                  ,"cwScale"
                  -- NUMBERS
                  ,"numMax"
                  ,"numMin"
                  ,"numOpposite"
                  ,"numAbs"
                  ,"numRound"
                  ,"numReciprocal"
                  ,"numQuot"
                  ,"numRem"
                  ,"numPi"
                  ,"numSqrt"
                  ,"numGCD"
                  ,"numLCM"
                  ,"numSin"
                  ,"numCos"
                  -- TEXT
                  ,"txtPrinted"
                  ,"txtLowercase"
                  ,"txtUppercase"
                  ,"txtCapitalized"
                  -- COLORS
                  ,"cwBlue"
                  ,"cwRed"
                  ,"cwGreen"
                  ,"cwBrown"
                  ,"cwOrange"
                  ,"cwBlack"
                  ,"cwWhite"
                  ,"cwCyan"
                  ,"cwMagenta"
                  ,"cwYellow"
                  ,"cwAquamarine"
                  ,"cwAzure"
                  ,"cwViolet"
                  ,"cwChartreuse"
                  ,"cwRose"
                  ,"cwPink"
                  ,"cwPurple"
                  ,"cwGray"
                  ,"cwMixed"
                  ,"cwLight"
                  ,"cwDark"
                  ,"cwBright"
                  ,"cwDull"
                  ,"cwTranslucent"
                  ,"cwRGBA"
                  -- LOGIC
                  ,"conNot"
                  ,"conTrue"
                  ,"conFalse"
                  ,"conEven"
                  ,"conOdd"
                  ,"conStartWith"
                  ,"conEndWith"
                  -- Tuples
                  ,"pair_first_typed"
                  ,"pair_second_typed"
                  -- Lists
                  ,"lists_repeating"
                  ,"lists_shuffled"
                  ,"lists_sorted"
                  ,"lists_reversed"
                  ,"lists_first"
                  ,"lists_rest"
                  ,"lists_length"
                  ,"lists_at"
                  -- FUNCTIONS
                  ,"lists_path"
                  ,"lists_thickPath"
                  ,"lists_polygon"
                  ,"lists_solidPolygon"
                  ,"lists_thickPolygon"
                  ,"lists_curve"
                  ,"lists_thickcurve"
                  ,"lists_loop"
                  ,"lists_solidLoop"
                  ,"lists_thickLoop"
                  -- TYPES
                    ]

regularBlocks :: [(T.Text,ParserFunction)]
regularBlocks = zip regularBlockNames (repeat sFuncBlock)


infixBlocks :: [(T.Text,ParserFunction)]
infixBlocks =   [ 
                   ("numAdd",sInfixBlock)
                  ,("numSub",sInfixBlock)
                  ,("numMult",sInfixBlock)
                  ,("numDiv",sInfixBlock)
                  ,("numExp",sInfixBlock)
                  ,("conAnd",sInfixBlock)
                  ,("conOr",sInfixBlock)
                  ,("conEq",sInfixBlock)
                  ,("conNeq",sInfixBlock)
                  ,("conGreater",sInfixBlock)
                  ,("conGeq",sInfixBlock)
                  ,("conLess",sInfixBlock)
                  ,("conLeq",sInfixBlock)
                  ,("lists_cons", sInfixBlock)
                    ]

specialBlocks :: [(T.Text,ParserFunction)]
specialBlocks = [  -- PROGRAMS 
                   ("cwDrawingOf",blockDrawingOf)
                  ,("cwAnimationOf",blockAnim)
                  ,("cwSimulationOf",blockSimulation)
                  ,("cwInteractionOf",blockInteraction)
                  -- PICTURES
                  ,("cwCombine", blockCombine)
                  -- NUMBERS
                  ,("numNumber",blockNumber)
                  ,("numNumberPerc",blockNumberPerc)
                  -- TEXT
                  ,("text_typed",blockString)
                  ,("txtConcat", blockConcat)
                  ,("conIf",blockIf)
                  ,("pair_create_typed", blockCreatePair)
                  ,("lists_create_with_typed", blockCreateList)
                  ,("lists_cons", sInfixBlock)
                  ,("lists_numgen", blockNumGen)
                  ,("lists_numgenstep", blockNumGenStep)
                  ,("lists_comprehension", blockListComp)
                  ,("procedures_letFunc",blockLetFunc)
                  ,("procedures_callreturn",blockLetCall)
                  ,("vars_local",blockLocalVar)
                  ,("comment",blockComment)
                  -- TYPES
                  ,("type_user", blockUserType)
                  ,("type_list", blockListType)
                  ,("expr_constructor", blockConstructor)
                  ,("expr_case", blockCase)
                  ,("type_product", blockProduct)
                  ,("type_sum", blockSum)
                    ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator

valueToExpr :: Block -> T.Text -> SaveErr Expr
valueToExpr block name = do
    case helper of 
      Just (func,inputBlock) -> do
        code <- func inputBlock
        push $ code 
      Nothing -> errDisc block
  where
    helper = do
      inputBlock <- getInputBlock block name
      let blockType = getBlockType inputBlock
      func <- M.lookup blockType blockCodeMap
      return (func, inputBlock)
