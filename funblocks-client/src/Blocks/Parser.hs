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
import Prelude hiding ((++), show)
import qualified Prelude as P
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

-- Helpers for converting Text
(++) :: T.Text -> T.Text -> T.Text
a ++ b = a `T.append` b
pack = textToJSString
unpack = textFromJSString
show :: Show a => a -> T.Text
show = T.pack . P.show

-- Helper functions
member :: Expr -> (Expr, OrderConstant)
member code = (code, CMember)
none :: Expr ->(Expr, OrderConstant)
none code = (code, CNone)
atomic :: Expr ->(Expr, OrderConstant)
atomic code = (code, CAtomic)


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
errg :: T.Text -> Block -> SaveErr (Expr, OrderConstant)
errg msg block = SE (Comment,CNone) $ Just $ Error msg block

type ParserFunction = Block -> SaveErr (Expr, OrderConstant)



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
          | ListComp Expr [(T.Text,Expr)] [Expr] -- [action | var <- expr, var <- expr, guards]
          | Comment

data Type = Type T.Text
     | ListType Type -- [type] -- unfortunately its separate for the special syntax
     | Product T.Text [Type] 
     | Sum T.Text [Type]

-- data Product = Product T.Text [Type] -- Constructor tp1*tp2*tp3

-- standard function block, automatically handles machinery for functions
-- assumes inputs are in correct order
sFuncBlock :: ParserFunction
sFuncBlock block = do
  let argNames = getValueInputNames block
  let funcName = getFunctionName block
  args <- mapM (\arg -> valueToExpr block arg CNone) argNames
  return $ none $ CallFunc funcName args

sInfixBlock :: OrderConstant -> ParserFunction
sInfixBlock ordr block = do
  let argNames = getValueInputNames block
  let funcName = getFunctionName block
  if length argNames /= 2 then
    errg "Infix functions must have 2 connected blocks" block
  else do
    args@(left:right:xs) <- mapM (\arg -> valueToExpr block arg ordr) argNames
    return (CallFuncInfix funcName left right, ordr)


-- PROGRAMS --------------------------------------
blockDrawingOf :: ParserFunction
blockDrawingOf block = do 
      expr <- valueToExpr block "VALUE" CNone
      return $ none $ FuncDef "main" [] $ CallFunc "drawingOf" [expr]

-- NUMBERS -------------------------------------------------------

blockNumber :: ParserFunction
blockNumber block = do 
    let arg = getFieldValue block "NUMBER"
    return $ none $ LiteralN (read $ T.unpack arg) 

blockNumberPerc :: ParserFunction
blockNumberPerc block = do 
    let arg = getFieldValue block "NUMBER"
    let numb = (read (T.unpack arg)) * 0.01
    return $ none $ LiteralN numb

-- TEXT --------------------------------------------------

blockString :: ParserFunction
blockString block = do 
    let txt = getFieldValue block "TEXT" 
    return $ none $ LiteralS txt 

-- LOGIC ------------------------------------------

blockIf :: ParserFunction
blockIf block = do 
    ifexpr <- valueToExpr block "IF" CNone
    thenexpr <- valueToExpr block "THEN" CNone
    elseexpr <- valueToExpr block "ELSE" CNone
    return $ none $ If ifexpr thenexpr elseexpr


-- Let function block with parameters
foreign import javascript unsafe "$1.arguments_"
  js_funcargs :: Block -> JA.JSArray

blockLetFunc :: ParserFunction
blockLetFunc block = do 
    let name = getFieldValue block "NAME" 
    let vars = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    expr <- valueToExpr block "RETURN" CNone
    return $ none $ FuncDef name vars expr 

blockLetCall :: ParserFunction
blockLetCall block = do 
    let name = getFieldValue block "NAME" 
    let args = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    vals <- mapM (\t -> valueToExpr block t CNone) ["ARG" ++ show i | i <- [0..length args - 1]]
    return $ none $ CallFunc name vals

blockLocalVar :: ParserFunction
blockLocalVar block = do 
    let name = getFieldValue block "NAME" 
    return $ none $ CallFunc name []

-- ANIMATION
blockAnim :: ParserFunction
blockAnim block =  
    case getInputBlock block "FUNC" of
      Just inpBlock -> do
                       let funcName = getFunctionName inpBlock 
                       return $ none $ FuncDef "main" [] $ CallFunc "animationOf" [CallFunc funcName []]
      Nothing -> errg "No function inserted" block

blockSimulation :: ParserFunction
blockSimulation block = do
        initial <- aux "INITIAL"
        step <- aux "STEP"
        draw <- aux "DRAW"
        return $ none $ FuncDef "main" [] $ CallFunc "simulationOf" 
                                            [CallFunc initial [],CallFunc step [],CallFunc draw []]
  where
    aux name = case getInputBlock block name of
                      Just inpBlock -> return $ getFunctionName inpBlock 
                      Nothing -> errs "No function inserted" block


-- COMMENT
blockComment :: ParserFunction
blockComment block = return $ none $ Comment

-- Tuples
blockCreatePair :: ParserFunction
blockCreatePair block = do 
    first <- valueToExpr block "FIRST" CNone
    second <- valueToExpr block "SECOND" CNone
    return $ none $ Tuple [first,second]

blockFst :: ParserFunction
blockFst = sFuncBlock

blockSnd :: ParserFunction
blockSnd = sFuncBlock 

-- LISTS

blockCreateList :: ParserFunction
blockCreateList block = do
  let c = getItemCount block
  vals <- mapM (\t -> valueToExpr block t CNone) ["ADD" ++ show i | i <- [0..c-1]]
  return $ none $ ListCreate vals

blockNumGen :: ParserFunction
blockNumGen block = do 
    left <- valueToExpr block "LEFT" CNone
    right <- valueToExpr block "RIGHT" CNone
    return $ none $ ListSpec left right

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

    varCodes <- mapM (\t -> valueToExpr block t CNone) ["VAR" ++ show i | i <- [0..varCount-1]]
    guards <- mapM (\t -> valueToExpr block t CNone) ["GUARD" ++ show i | i <- [0..guardCount-1]]
    doCode <- valueToExpr block "DO" CNone

    return $ none $ ListComp doCode (zip vars varCodes) guards

-- TYPES

foreign import javascript unsafe "$1.itemCount_"
  js_itemCount :: Block -> Int

blockUserType :: ParserFunction
blockUserType block = do 
    let name = getFieldValue block "NAME"
    return $ none $ UserType $ Type name

blockListType :: ParserFunction
blockListType block = do 
    tp <- valueToExpr block "TP" CNone
    return $ none $ UserType $ ListType $ toType tp 
  where
    toType (UserType tp) = tp
    toType _ = Type ""

blockConstructor :: ParserFunction
blockConstructor block = do 
    let name :: T.Text = getFieldValue block "NAME"
    let itemCount = js_itemCount block
    tps :: [Expr] <- mapM (\n -> valueToExpr block n CNone) ["TP" ++ show i | i <- [0..itemCount-1]] 
    let out :: Expr = CallConstr name tps
    return $ none out

blockProduct :: ParserFunction
blockProduct block = do 
    let constructor = getFieldValue block "CONSTRUCTOR"
    let itemCount = js_itemCount block
    tps :: [Expr] <- mapM (\n -> valueToExpr block n CNone) ["TP" ++ show i | i <- [0..itemCount-1]] 
    return $ none $ UserType $ Product constructor $ map toType tps 
  where
    toType (UserType tp) = tp
    toType _ = Type ""

blockSum :: ParserFunction
blockSum block = do 
    let typeName = getFieldValue block "NAME"
    let itemCount = js_itemCount block
    tps <- mapM (\n -> valueToExpr block n CNone) ["PROD" ++ show i | i <- [0..itemCount-1]] 
    return $ none $ UserType $ Sum typeName (map toType tps)
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
    inp :: Expr <- valueToExpr block "INPUT" CNone
    outs <- mapM (\n -> valueToExpr block n CNone) ["CS" ++ show i | i <- [0..itemCount-1]] 
    let vars_ :: [[T.Text]] = map vars [0..itemCount-1]
    let cons_ :: [T.Text] = map con [0..itemCount-1]
    return $ none $ Case inp (zip3 cons_ vars_ outs :: [(T.Text,[T.Text],Expr)] ) 
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
                  -- Lists
                  ,"lists_length"
                  ,"lists_at"
                  -- FUNCTIONS
                  ,"lists_path"
                  -- TYPES
                    ]

regularBlocks :: [(T.Text,ParserFunction)]
regularBlocks = zip regularBlockNames (repeat sFuncBlock)


infixBlocks :: [(T.Text,ParserFunction)]
infixBlocks =   [  -- PROGRAMS 
                  ("cwCombine",sInfixBlock CCombine)
                  ,("numAdd",sInfixBlock CAddition)
                  ,("numSub",sInfixBlock CSubtraction)
                  ,("numMult",sInfixBlock CMultiplication)
                  ,("numDiv",sInfixBlock CDivision)
                  ,("numExp",sInfixBlock CMultiplication)
                  ,("txtConcat",sInfixBlock CAtomic)
                  ,("conAnd",sInfixBlock CAtomic)
                  ,("conOr",sInfixBlock CAtomic)
                  ,("conEq",sInfixBlock CAtomic)
                  ,("conNeq",sInfixBlock CAtomic)
                  ,("conGreater",sInfixBlock CAtomic)
                  ,("conGeq",sInfixBlock CAtomic)
                  ,("conLess",sInfixBlock CAtomic)
                  ,("conLeq",sInfixBlock CAtomic)
                  ,("lists_cons", sInfixBlock CAtomic)
                    ]

specialBlocks :: [(T.Text,ParserFunction)]
specialBlocks = [  -- PROGRAMS 
                   ("cwDrawingOf",blockDrawingOf)
                  ,("cwAnimationOf",blockAnim)
                  ,("cwSimulationOf",blockSimulation)
                  ,("numNumber",blockNumber)
                  ,("numNumberPerc",blockNumberPerc)
                  ,("text_typed",blockString)
                  ,("conIf",blockIf)
                  ,("pair_create_typed", blockCreatePair)
                  ,("pair_first_typed", blockFst)
                  ,("pair_second_typed", blockSnd)
                  ,("lists_create_with_typed", blockCreateList)
                  ,("lists_cons", sInfixBlock CAtomic)
                  ,("lists_numgen", blockNumGen)
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

valueToExpr :: Block -> T.Text -> OrderConstant -> SaveErr Expr
valueToExpr block name ordr = do
    case helper of 
      Just (func,inputBlock) -> do
        (code,innerOrder) <- func inputBlock
        push $ code -- handleOrder (order innerOrder) (order ordr) code
      Nothing -> errc "Disconnected Input" block
  where
    helper = do
      inputBlock <- getInputBlock block name
      let blockType = getBlockType inputBlock
      func <- M.lookup blockType blockCodeMap
      return (func, inputBlock)

    -- TODO
    handleOrder innerOrdr odrd code
      | innerOrdr == 0 || innerOrdr == 99 = code
    handleOrder innerOrdr ordr code = if ordr <= innerOrdr
                          then if ordr == innerOrdr && (ordr == 0 || ordr == 99)
                               then code
                               else "(" ++ code ++ ")"
                          else code


--- FFI

foreign import javascript unsafe "Blockly.FunBlocks.valueToExpr($1, $2, $3)"
  js_valueToExpr :: Block -> JSString -> Int -> JSString


data OrderConstant =  CAtomic
                    | CMember
                    | CNew
                    | CFunctionCall
                    | CIncrement
                    | CDecrement
                    | CLogicalNot
                    | CBitwiseNot
                    | CUnaryPlus
                    | CUnaryNegation
                    | CTypeOf
                    | CCombine
                    | CExponentiation
                    | CMultiplication
                    | CDivision
                    | CModulus
                    | CAddition
                    | CSubtraction
                    | CBitwiseShift
                    | CRelational
                    | CIn
                    | CInstanceOf
                    | CEquality
                    | CBitwiseAnd
                    | CBitwiseXOR
                    | CBitwiseOR
                    | CLogicalAnd
                    | CLogicalOr
                    | CConditional
                    | CAssignment
                    | CComma
                    | CNone          


order :: OrderConstant -> Int
order CAtomic         = 0;  -- 0 "" ...
order CMember         = 1;  -- . []
order CNew            = 1;  -- new
order CFunctionCall   = 2;  -- ()
order CIncrement      = 3;  -- ++
order CDecrement      = 3;  -- --
order CLogicalNot     = 4;  -- !
order CBitwiseNot     = 4;  -- ~
order CUnaryPlus      = 4;  -- +
order CUnaryNegation  = 4;  -- -
order CTypeOf         = 4;  -- typeof
order CExponentiation = 4;  -- ^
order CCombine        = 5;  -- &
order CMultiplication = 5;  -- *
order CDivision       = 5;  -- /
order CModulus        = 5;  -- %
order CAddition       = 6;  -- +
order CSubtraction    = 6;  -- -
order CBitwiseShift   = 7;  -- << >> >>>
order CRelational     = 8;  -- < <= > >=
order CIn             = 8;  -- in
order CInstanceOf     = 8;  -- instanceof
order CEquality       = 9;  -- == != === !==
order CBitwiseAnd     = 10; -- &
order CBitwiseXOR     = 11; -- ^
order CBitwiseOR      = 12; -- |
order CLogicalAnd     = 13; -- &&
order CLogicalOr      = 14; -- ||
order CConditional    = 15; -- ?:
order CAssignment     = 16; -- = += -= *= /= %= <<= >>= ...
order CComma          = 17; -- ,
order CNone           = 99; -- (...)
