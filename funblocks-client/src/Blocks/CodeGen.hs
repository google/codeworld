{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

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

module Blocks.CodeGen (assignAll)
  where

import Blockly.Block
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString (pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Marshal
import qualified JavaScript.Array as JA
import Unsafe.Coerce

setCodeGen :: String -> (Block -> Maybe (String, OrderConstant) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 case func b of
                                    Just (code,ordr) -> do
                                            putStrLn "JUST"
                                            return $ js_makeArray (pack code) (order ordr)
                                    Nothing -> do
                                            putStrLn "NOTHING"
                                            return $ js_makeArray (pack "") 0
                                 )
  js_setGenFunction (pack blockName) cb


-- Helper functions
member :: Code -> (Code, OrderConstant)
member code = (code, CMember)
none :: Code ->(Code, OrderConstant)
none code = (code, CNone)

type Code = String
type GeneratorFunction = Block -> Maybe (Code, OrderConstant)

blockText :: GeneratorFunction
blockText block = do
      let arg = getFieldValue block "TEXT" 
      return $ none $ "text(\"" ++ arg ++ "\")"

blockDrawingOf :: GeneratorFunction
blockDrawingOf block = do 
      code <- valueToCode block "VALUE" CNone
      return $ none $ "main = drawingOf(" ++ code ++ ");"

-- TODO check if it is a number
blockNumber :: GeneratorFunction
blockNumber block = do 
    let arg = getFieldValue block "NUMBER"
    return $ none arg 

blockSolidRectangle :: GeneratorFunction
blockSolidRectangle block = do
    width <- valueToCode block "WIDTH" CNone
    height <- valueToCode block "HEIGHT" CNone
    return $ none $ "solidRectangle(" ++ width ++ "," ++ height ++ ")"

blockSolidCircle :: GeneratorFunction
blockSolidCircle block = do 
    radius <- valueToCode block "RADIUS" CAtomic
    return $ none $ "solidCircle(" ++ radius ++ ")"

blockCircle :: GeneratorFunction
blockCircle block = do 
    radius <- valueToCode block "RADIUS" CNone
    return $ none $ "circle(" ++ radius ++ ")"

blockCombine :: GeneratorFunction
blockCombine block = do
    pic1 <- valueToCode block "PIC1" CNone
    pic2 <- valueToCode block "PIC2" CNone
    return $ none $ "(" ++ pic1 ++ ") & (" ++ pic2 ++ ")"

blockColored :: GeneratorFunction
blockColored block = do 
    picture <- valueToCode block "PICTURE" CNone
    color <- valueToCode block "COLOR" CNone
    return $ none $ "colored (" ++ picture ++ ", " ++ color ++ ")"

blockTranslate :: GeneratorFunction
blockTranslate block = do 
    pic <- valueToCode block "PICTURE" CNone
    x <- valueToCode block "X" CNone
    y <- valueToCode block "Y" CNone
    return $ none $ "translated (" ++ pic ++ "," ++ x ++ "," ++ y ++ ")"
    
blockScale :: GeneratorFunction
blockScale block = do
    pic <- valueToCode block "PICTURE" CNone
    hor <- valueToCode block "HORZ" CNone
    vert <- valueToCode block "VERTZ" CNone
    return $ none $ "scaled (" ++ pic ++ "," ++ hor ++ "," ++ vert ++ ")"
    
blockRotate :: GeneratorFunction
blockRotate block = do 
    pic <- valueToCode block "PICTURE" CNone
    angle <- valueToCode block "ANGLE" CNone
    return $ none $ "rotated (" ++ pic ++ "," ++ angle ++ ")"

blockBlue :: GeneratorFunction
blockBlue block = return $ none "blue"

blockBrown :: GeneratorFunction
blockBrown block = return $ none "brown"

blockRed :: GeneratorFunction
blockRed block = return $ none "red"

blockGreen :: GeneratorFunction
blockGreen block = return $ none "green"

blockLetVar :: GeneratorFunction
blockLetVar block = do 
    let varName = getFieldValue block "VARNAME" 
    expr <- valueToCode block "VARVALUE" CNone
    return $ none $ varName ++ " = " ++ expr 

blockTrue :: GeneratorFunction
blockTrue block = return $ none "True"

blockFalse :: GeneratorFunction
blockFalse block = return $ none "False"

blockIf :: GeneratorFunction
blockIf block = do 
    ifexpr <- valueToCode block "IF" CNone
    thenexpr <- valueToCode block "THEN" CNone
    elseexpr <- valueToCode block "ELSE" CNone
    return $ none $ "if " ++ ifexpr ++ " then "
                  ++ thenexpr ++ " else " ++ elseexpr

blockEq :: GeneratorFunction
blockEq block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " == " ++ right

blockCodeMap = [ ("cw_text",blockText)
                ,("cw_translate", blockTranslate)
                ,("cw_combine", blockCombine)
                ,("cw_colored", blockColored)
                ,("cw_drawingof", blockDrawingOf)
                ,("number",blockNumber)
                ,("cw_solidrectangle", blockSolidRectangle)
                ,("cw_solidcircle", blockSolidCircle)
                ,("cw_circle", blockCircle)
                ,("cw_blue", blockBlue)
                ,("cw_red", blockRed)
                ,("cw_green", blockGreen)
                ,("cw_brown", blockBrown)
                ,("letVar", blockLetVar)
                ,("con_true", blockTrue)
                ,("con_false", blockFalse)
                ,("con_if", blockIf)
                ,("con_eq", blockEq)
                ,("cw_scale", blockScale)
                ,("cw_rotate", blockRotate)
                ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
assignAll :: IO ()
assignAll = mapM_ (uncurry setCodeGen) blockCodeMap

valueToCode :: Block -> String -> OrderConstant -> Maybe String
valueToCode block name ordr = do 
                inputBlock <- getInputBlock block name
                let val = unpack $ js_valueToCode block (pack name) (order ordr)
                if val=="" then Nothing
                else Just val 

--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()


foreign import javascript unsafe "Blockly.FunBlocks.valueToCode($1, $2, $3)"
  js_valueToCode :: Block -> JSString -> Int -> JSString

-- TODO, fix, Ugly hack incoming
foreign import javascript unsafe "[$1,$2]"
  js_makeArray :: JSString -> Int -> JSVal


-- TODO, remove, was used for testing
alert :: String -> IO ()
alert text = js_alert $ pack text
foreign import javascript unsafe "alert($1)" js_alert :: JSString -> IO ()



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
                    | CVoid
                    | CDelete
                    | CMultiplication
                    | CDivision
                    | CModulus
                    | CAddition
                    | CSubstraction
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


-- TODO, still JavaScript CodeGen stuff
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
order CVoid           = 4;  -- void
order CDelete         = 4;  -- delete
order CMultiplication = 5;  -- *
order CDivision       = 5;  -- /
order CModulus        = 5;  -- %
order CAddition       = 6;  -- +
order CSubstraction   = 6;  -- -
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
