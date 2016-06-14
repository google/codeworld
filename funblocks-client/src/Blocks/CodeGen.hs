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
blockText block = member $ "text(\"" ++ arg ++ "\")"
  where
    arg = getFieldValue block "TEXT" 

-- TODO check if it is a number
blockNumber :: GeneratorFunction
blockNumber block = member arg 
  where
    arg = getFieldValue block "NUMBER"

blockSolidRectangle :: GeneratorFunction
blockSolidRectangle block = member $ "solidRectangle(" ++ width ++ "," ++ height ++ ")"
  where
    width = valueToCode block "WIDTH" CAtomic
    height = valueToCode block "HEIGHT" CAtomic

blockSolidCircle :: GeneratorFunction
blockSolidCircle block = member $ "solidCircle(" ++ radius ++ ")"
  where
    radius = valueToCode block "RADIUS" CAtomic

blockCircle :: GeneratorFunction
blockCircle block = member $ "circle(" ++ radius ++ ")"
  where
    radius = valueToCode block "RADIUS" CAtomic

blockDrawingOf :: GeneratorFunction
blockDrawingOf block = member $ "main = drawingOf(" ++ code ++ ");"
  where
    code = valueToCode block "VALUE" CAtomic

blockCombine :: GeneratorFunction
blockCombine block = none $ "(" ++ pic1 ++ ") & (" ++ pic2 ++ ")"
  where
    pic1 = valueToCode block "PIC1" CAtomic
    pic2 = valueToCode block "PIC2" CAtomic

blockColored :: GeneratorFunction
blockColored block = none $ "colored (" ++ picture ++ ", " ++ color ++ ")"
  where
    picture = valueToCode block "PICTURE" CAtomic
    color = valueToCode block "COLOR" CAtomic

blockTranslate :: GeneratorFunction
blockTranslate block = none $ "translated (" ++ pic ++ "," ++ x ++ "," ++ y ++ ")"
  where
    pic = valueToCode block "PICTURE" CAtomic
    x = valueToCode block "X" CAtomic
    y = valueToCode block "Y" CAtomic

blockBlue :: GeneratorFunction
blockBlue block = member "blue"

blockBrown :: GeneratorFunction
blockBrown block = member "brown"

blockRed :: GeneratorFunction
blockRed block = member "red"

blockGreen :: GeneratorFunction
blockGreen block = member "green"

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
                ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
assignAll :: IO ()
assignAll = mapM_ (uncurry setCodeGen) blockCodeMap


valueToCode :: Block -> String -> OrderConstant -> String
valueToCode block name ordr = unpack $ js_valueToCode block (pack name) (order ordr)



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
