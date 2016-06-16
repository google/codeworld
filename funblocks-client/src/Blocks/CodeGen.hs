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

setCodeGen :: String -> (Block -> (String, OrderConstant) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 let (code,ordr) = func b
                                 -- alert(code)
                                 return $ js_makeArray (pack code) (order ordr)
                                 -- toJSVal $ JA.fromList [v,order]
                                 )
  js_setGenFunction (pack blockName) cb


-- Helper functions
member :: Code -> (Code, OrderConstant)
member code = (code, CMember)
none :: Code -> (Code, OrderConstant)
none code = (code, CNone)

type Code = String
type GeneratorFunction = Block -> (Code, OrderConstant)

blockText :: GeneratorFunction
blockText block = none $ "text(\"" ++ arg ++ "\")"
  where
    arg = getFieldValue block "TEXT" 

blockDrawingOf :: GeneratorFunction
blockDrawingOf block = none $ "main = drawingOf(" ++ code ++ ");"
  where
    code = valueToCode block "VALUE" CNone

-- TODO check if it is a number
blockNumber :: GeneratorFunction
blockNumber block = none arg 
  where
    arg = getFieldValue block "NUMBER"

blockSolidRectangle :: GeneratorFunction
blockSolidRectangle block = none $ "solidRectangle(" ++ width ++ "," ++ height ++ ")"
  where
    width = valueToCode block "WIDTH" CNone
    height = valueToCode block "HEIGHT" CNone

blockSolidCircle :: GeneratorFunction
blockSolidCircle block = none $ "solidCircle(" ++ radius ++ ")"
  where
    radius = valueToCode block "RADIUS" CAtomic

blockCircle :: GeneratorFunction
blockCircle block = none $ "circle(" ++ radius ++ ")"
  where
    radius = valueToCode block "RADIUS" CNone

blockCombine :: GeneratorFunction
blockCombine block = none $ "(" ++ pic1 ++ ") & (" ++ pic2 ++ ")"
  where
    pic1 = valueToCode block "PIC1" CNone
    pic2 = valueToCode block "PIC2" CNone

blockColored :: GeneratorFunction
blockColored block = none $ "colored (" ++ picture ++ ", " ++ color ++ ")"
  where
    picture = valueToCode block "PICTURE" CNone
    color = valueToCode block "COLOR" CNone

blockTranslate :: GeneratorFunction
blockTranslate block = none $ "translated (" ++ pic ++ "," ++ x ++ "," ++ y ++ ")"
  where
    pic = valueToCode block "PICTURE" CNone
    x = valueToCode block "X" CNone
    y = valueToCode block "Y" CNone

blockScale :: GeneratorFunction
blockScale block = none $ "scaled (" ++ pic ++ "," ++ hor ++ "," ++ vert ++ ")"
  where
    pic = valueToCode block "PICTURE" CNone
    hor = valueToCode block "HORZ" CNone
    vert = valueToCode block "VERTZ" CNone

blockRotate :: GeneratorFunction
blockRotate block = none $ "rotated (" ++ pic ++ "," ++ angle ++ ")"
  where
    pic = valueToCode block "PICTURE" CNone
    angle = valueToCode block "ANGLE" CNone

blockBlue :: GeneratorFunction
blockBlue block = none "blue"

blockBrown :: GeneratorFunction
blockBrown block = none "brown"

blockRed :: GeneratorFunction
blockRed block = none "red"

blockGreen :: GeneratorFunction
blockGreen block = none "green"

blockLetVar :: GeneratorFunction
blockLetVar block = none $ varName ++ " = " ++ expr 
  where
    varName = getFieldValue block "VARNAME" 
    expr = valueToCode block "VARVALUE" CNone

blockTrue :: GeneratorFunction
blockTrue block = none "True"

blockFalse :: GeneratorFunction
blockFalse block = none "False"

blockIf :: GeneratorFunction
blockIf block = none $ "if " ++ ifexpr ++ " then "
                  ++ thenexpr ++ " else " ++ elseexpr
  where
   ifexpr = valueToCode block "IF" CNone
   thenexpr = valueToCode block "THEN" CNone
   elseexpr = valueToCode block "ELSE" CNone

blockEq :: GeneratorFunction
blockEq block = member $ left ++ " == " ++ right
  where
    left = valueToCode block "LEFT" CAtomic
    right = valueToCode block "RIGHT" CAtomic

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


valueToCode :: Block -> String -> OrderConstant -> String
valueToCode block name ordr = unpack $ 
                js_valueToCode block (pack name) (order ordr)

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
