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

module Blocks.CodeGen (assignAll
                      ,getGenerationBlocks)
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

-- Helper functions
member :: Code -> (Code, OrderConstant)
member code = (code, CMember)
none :: Code ->(Code, OrderConstant)
none code = (code, CNone)

type Code = String
type GeneratorFunction = Block -> Either Block (Code, OrderConstant)

-- PROGRAMS --------------------------------------
blockDrawingOf :: GeneratorFunction
blockDrawingOf block = do 
      code <- valueToCode block "VALUE" CNone
      return $ none $ "main = drawingOf(" ++ code ++ ")"

-- PICTURES --------------------------------------
blockBlank :: GeneratorFunction
blockBlank block = return $ none "blank"

blockCoordinatePlane :: GeneratorFunction
blockCoordinatePlane block = return $ none "coordinatePlane"

blockCodeWorldLogo :: GeneratorFunction
blockCodeWorldLogo block = return $ none "codeWorldLogo"

blockText :: GeneratorFunction
blockText block = do
      arg <- valueToCode block "TEXT" CNone
      return $ none $ "text(" ++ arg ++ ")"

blockSolidCircle :: GeneratorFunction
blockSolidCircle block = do 
    radius <- valueToCode block "RADIUS" CAtomic
    return $ none $ "solidCircle(" ++ radius ++ ")"

blockCircle :: GeneratorFunction
blockCircle block = do 
    radius <- valueToCode block "RADIUS" CNone
    return $ none $ "circle(" ++ radius ++ ")"

blockThickCircle :: GeneratorFunction
blockThickCircle block = do 
    radius <- valueToCode block "RADIUS" CNone
    linewidth <- valueToCode block "LINEWIDTH" CNone
    return $ none $ "thickCircle(" ++ radius ++ "," ++ linewidth ++ ")"

blockRectangle :: GeneratorFunction
blockRectangle block = do
    width <- valueToCode block "WIDTH" CNone
    height <- valueToCode block "HEIGHT" CNone
    return $ none $ "rectangle(" ++ width ++ "," ++ height ++ ")"

blockThickRectangle :: GeneratorFunction
blockThickRectangle block = do
    width <- valueToCode block "WIDTH" CNone
    height <- valueToCode block "HEIGHT" CNone
    linewidth <- valueToCode block "LINEWIDTH" CNone
    return $ none $ "solidRectangle(" ++ width ++ "," ++ height ++ "," ++ linewidth ++ ")"

blockSolidRectangle :: GeneratorFunction
blockSolidRectangle block = do
    width <- valueToCode block "WIDTH" CNone
    height <- valueToCode block "HEIGHT" CNone
    return $ none $ "solidRectangle(" ++ width ++ "," ++ height ++ ")"

blockArc :: GeneratorFunction
blockArc block = do
    startangle <- valueToCode block "STARTANGLE" CNone
    endangle <- valueToCode block "ENDANGLE" CNone
    radius <- valueToCode block "RADIUS" CNone
    return $ none $ "arc(" ++ startangle ++ "," ++ endangle ++ "," ++ radius ++ ")"

blockSector :: GeneratorFunction
blockSector block = do
    startangle <- valueToCode block "STARTANGLE" CNone
    endangle <- valueToCode block "ENDANGLE" CNone
    radius <- valueToCode block "RADIUS" CNone
    return $ none $ "sector(" ++ startangle ++ "," ++ endangle ++ "," ++ radius ++ ")"

blockThickArc :: GeneratorFunction
blockThickArc block = do
    startangle <- valueToCode block "STARTANGLE" CNone
    endangle <- valueToCode block "ENDANGLE" CNone
    radius <- valueToCode block "RADIUS" CNone
    linewidth <- valueToCode block "LINEWIDTH" CNone
    return $ none $ "thickArc(" ++ startangle ++ "," ++ endangle ++ "," ++ radius ++ "," ++ linewidth ++ ")"


-- TRANSFORMATIONS ------------------------------------------------------

blockCombine :: GeneratorFunction
blockCombine block = do
    pic1 <- valueToCode block "PIC1" CNone
    pic2 <- valueToCode block "PIC2" CNone
    return $ none $ "(" ++ pic1 ++ ") & (" ++ pic2 ++ ")"

blockColored :: GeneratorFunction
blockColored block = do 
    picture <- valueToCode block "PICTURE" CNone
    color <- valueToCode block "COLOR" CNone
    return $ none $ "colored(" ++ picture ++ ", " ++ color ++ ")"

blockTranslate :: GeneratorFunction
blockTranslate block = do 
    pic <- valueToCode block "PICTURE" CNone
    x <- valueToCode block "X" CNone
    y <- valueToCode block "Y" CNone
    return $ none $ "translated(" ++ pic ++ "," ++ x ++ "," ++ y ++ ")"
    
blockScale :: GeneratorFunction
blockScale block = do
    pic <- valueToCode block "PICTURE" CNone
    hor <- valueToCode block "HORZ" CNone
    vert <- valueToCode block "VERTZ" CNone
    return $ none $ "scaled(" ++ pic ++ "," ++ hor ++ "," ++ vert ++ ")"
    
blockRotate :: GeneratorFunction
blockRotate block = do 
    pic <- valueToCode block "PICTURE" CNone
    angle <- valueToCode block "ANGLE" CNone
    return $ none $ "rotated(" ++ pic ++ "," ++ angle ++ ")"



-- NUMBERS -------------------------------------------------------

blockNumber :: GeneratorFunction
blockNumber block = do 
    let arg = getFieldValue block "NUMBER"
    return $ none arg 

blockAdd :: GeneratorFunction
blockAdd block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " + " ++ right

blockSub :: GeneratorFunction
blockSub block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " - " ++ right

blockMult :: GeneratorFunction
blockMult block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " * " ++ right

blockDiv :: GeneratorFunction
blockDiv block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " / " ++ right

blockExp :: GeneratorFunction
blockExp block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ "^" ++ right

blockMax :: GeneratorFunction
blockMax block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "max(" ++ left ++ "," ++ right ++ ")"

blockMin :: GeneratorFunction
blockMin block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "min(" ++ left ++ "," ++ right ++ ")"

blockOpposite :: GeneratorFunction
blockOpposite block = do 
    num <- valueToCode block "NUM" CNone
    return $ none $ "opposite(" ++ num ++ ")"

blockAbs :: GeneratorFunction
blockAbs block = do 
    num <- valueToCode block "NUM" CNone
    return $ none $ "absoluteValue(" ++ num ++ ")"

blockRound :: GeneratorFunction
blockRound block = do 
    num <- valueToCode block "NUM" CNone
    return $ none $ "round(" ++ num ++ ")"

blockReciprocal :: GeneratorFunction
blockReciprocal block = do 
    num <- valueToCode block "NUM" CNone
    return $ none $ "reciprocal(" ++ num ++ ")"

blockQuotient :: GeneratorFunction
blockQuotient block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "quotient(" ++ left ++ "," ++ right ++ ")"

blockRemainder :: GeneratorFunction
blockRemainder block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "remainder(" ++ left ++ "," ++ right ++ ")"

blockPi :: GeneratorFunction
blockPi block = return $ none "pi"

blockSqrt :: GeneratorFunction
blockSqrt block = do 
    num <- valueToCode block "NUM" CNone
    return $ none $ "squareRoot(" ++ num ++ ")"

blockGCD :: GeneratorFunction
blockGCD block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "gcd(" ++ left ++ "," ++ right ++ ")"

blockLCM :: GeneratorFunction
blockLCM block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "lcm(" ++ left ++ "," ++ right ++ ")"

-- TEXT --------------------------------------------------

blockString :: GeneratorFunction
blockString block = do 
    let txt = getFieldValue block "TEXT" 
    return $ none $ "\"" ++ txt ++ "\""

blockConcat :: GeneratorFunction
blockConcat block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ "<>" ++ right

blockPrinted :: GeneratorFunction
blockPrinted block = do 
    txt <- valueToCode block "TEXT" CNone
    return $ none $ "printed(" ++ txt ++ ")"

blockUppercase :: GeneratorFunction
blockUppercase block = do 
    txt <- valueToCode block "TEXT" CNone
    return $ none $ "uppercase(" ++ txt ++ ")"

blockLowercase :: GeneratorFunction
blockLowercase block = do 
    txt <- valueToCode block "TEXT" CNone
    return $ none $ "lowercase(" ++ txt ++ ")"

blockCapitalized :: GeneratorFunction
blockCapitalized block = do 
    txt <- valueToCode block "TEXT" CNone
    return $ none $ "capitalized(" ++ txt ++ ")"

-- LOGIC ------------------------------------------
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

blockNeq :: GeneratorFunction
blockNeq block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " /= " ++ right

blockAnd :: GeneratorFunction
blockAnd block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " && " ++ right

blockOr :: GeneratorFunction
blockOr block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " || " ++ right

blockNot :: GeneratorFunction
blockNot block = do 
    val <- valueToCode block "VALUE" CNone
    return $ none $ "not(" ++ val ++ ")"

blockGreater :: GeneratorFunction
blockGreater block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " > " ++ right

blockGeq :: GeneratorFunction
blockGeq block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " >= " ++ right

blockLess :: GeneratorFunction
blockLess block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " < " ++ right

blockLeq :: GeneratorFunction
blockLeq block = do 
    left <- valueToCode block "LEFT" CAtomic
    right <- valueToCode block "RIGHT" CAtomic
    return $ member $ left ++ " <= " ++ right

blockEven :: GeneratorFunction
blockEven block = do 
    val <- valueToCode block "VALUE" CNone
    return $ none $ "even(" ++ val ++ ")" 

blockOdd :: GeneratorFunction
blockOdd block = do 
    val <- valueToCode block "VALUE" CNone
    return $ none $ "odd(" ++ val ++ ")" 

blockStartWith :: GeneratorFunction
blockStartWith block = do 
    txtMain <- valueToCode block "TEXTMAIN" CNone
    txtTest <- valueToCode block "TEXTTEST" CNone
    return $ none $ "startsWith(" ++ txtMain ++ "," ++ txtTest ++ ")"


blockEndWith :: GeneratorFunction
blockEndWith block = do 
    txtMain <- valueToCode block "TEXTMAIN" CNone
    txtTest <- valueToCode block "TEXTTEST" CNone
    return $ none $ "endsWith(" ++ txtMain ++ "," ++ txtTest ++ ")"

blockOrange :: GeneratorFunction
blockOrange block = return $ none "orange"

blockBlue :: GeneratorFunction
blockBlue block = return $ none "blue"

blockBrown :: GeneratorFunction
blockBrown block = return $ none "brown"

blockRed :: GeneratorFunction
blockRed block = return $ none "red"

blockGreen :: GeneratorFunction
blockGreen block = return $ none "green"

blockBlack :: GeneratorFunction
blockBlack block = return $ none "black"

blockWhite :: GeneratorFunction
blockWhite block = return $ none "white"

blockCyan :: GeneratorFunction
blockCyan block = return $ none "cyan"

blockMagenta :: GeneratorFunction
blockMagenta block = return $ none "magenta"

blockYellow :: GeneratorFunction
blockYellow block = return $ none "yellow"

blockAquamarine :: GeneratorFunction
blockAquamarine block = return $ none "aquamarine"

blockAzure :: GeneratorFunction
blockAzure block = return $ none "azure"

blockViolet :: GeneratorFunction
blockViolet block = return $ none "violet"

blockChartreuse :: GeneratorFunction
blockChartreuse block = return $ none "chartreuse"

blockRose :: GeneratorFunction
blockRose block = return $ none "rose"

blockPink :: GeneratorFunction
blockPink block = return $ none "pink"

blockPurple :: GeneratorFunction
blockPurple block = return $ none "purple"

blockGray :: GeneratorFunction
blockGray block = do 
    val <- valueToCode block "VALUE" CNone
    return $ none $ "gray(" ++ val ++ ")" 

blockMixed :: GeneratorFunction
blockMixed block = do 
    col1 <- valueToCode block "COL1" CNone
    col2 <- valueToCode block "COL2" CNone
    return $ none $ "mixed(" ++ col1 ++ "," ++ col2 ++ ")" 

blockLight :: GeneratorFunction
blockLight block = do 
    col <- valueToCode block "COL" CNone
    return $ none $ "light(" ++ col ++ ")" 

blockDark :: GeneratorFunction
blockDark block = do 
    col <- valueToCode block "COL" CNone
    return $ none $ "dark(" ++ col ++ ")" 

blockBright :: GeneratorFunction
blockBright block = do 
    col <- valueToCode block "COL" CNone
    return $ none $ "bright(" ++ col ++ ")" 

blockDull :: GeneratorFunction
blockDull block = do 
    col <- valueToCode block "COL" CNone
    return $ none $ "dull(" ++ col ++ ")" 

blockTranslucent :: GeneratorFunction
blockTranslucent block = do 
    col <- valueToCode block "COL" CNone
    return $ none $ "translucent(" ++ col ++ ")" 

blockRGBA :: GeneratorFunction
blockRGBA block = do 
    red <- valueToCode block "RED" CNone
    blue <- valueToCode block "BLUE" CNone
    green <- valueToCode block "GREEN" CNone
    alpha <- valueToCode block "ALPHA" CNone
    return $ none $ "rgba(" ++ red ++ "," ++ blue ++ "," ++ green ++ "," ++ alpha ++ ")" 

blockLetVar :: GeneratorFunction
blockLetVar block = do 
    let varName = getFieldValue block "VARNAME" 
    expr <- valueToCode block "VARVALUE" CNone
    return $ none $ varName ++ " = " ++ expr 

blockComment :: GeneratorFunction
blockComment block = return $ none ""

getGenerationBlocks :: [String]
getGenerationBlocks = map fst blockCodeMap

blockCodeMap = [ ("cwBlank",blockBlank)
                  ,("cwCoordinatePlane",blockCoordinatePlane)
                  ,("cwCodeWorldLogo",blockCodeWorldLogo)
                  ,("cwText",blockText)
                  ,("cwDrawingOf",blockDrawingOf)
                  ,("cwCircle",blockCircle)
                  ,("cwThickCircle",blockThickCircle)
                  ,("cwSolidCircle",blockSolidCircle)
                  ,("cwRectangle",blockRectangle)
                  ,("cwThickRectangle",blockThickRectangle)
                  ,("cwSolidRectangle",blockSolidRectangle)
                  ,("cwArc",blockArc)
                  ,("cwSector",blockSector)
                  ,("cwThickArc",blockThickArc)
                  -- TRANSFORMATIONS
                  ,("cwColored",blockColored)
                  ,("cwTranslate",blockTranslate)
                  ,("cwCombine",blockCombine)
                  ,("cwRotate",blockRotate)
                  ,("cwScale",blockScale)
                  -- NUMBERS
                  ,("numNumber",blockNumber)
                  ,("numAdd",blockAdd)
                  ,("numSub",blockSub)
                  ,("numMult",blockMult)
                  ,("numDiv",blockDiv)
                  ,("numExp",blockExp)
                  ,("numMax",blockMax)
                  ,("numMin",blockMin)
                  ,("numOpposite",blockOpposite)
                  ,("numAbs",blockAbs)
                  ,("numRound",blockRound)
                  ,("numReciprocal",blockReciprocal)
                  ,("numQuot",blockQuotient)
                  ,("numRem",blockRemainder)
                  ,("numPi",blockPi)
                  ,("numSqrt",blockSqrt)
                  ,("numGCD",blockGCD)
                  ,("numLCM",blockLCM)
                  -- TEXT
                  ,("txtConcat",blockConcat)
                  ,("text",blockString)
                  ,("txtPrinted",blockPrinted)
                  ,("txtLowercase",blockLowercase)
                  ,("txtUppercase",blockUppercase)
                  ,("txtCapitalized",blockCapitalized)
                  -- COLORS
                  ,("cwBlue",blockBlue)
                  ,("cwRed",blockRed)
                  ,("cwGreen",blockGreen)
                  ,("cwBrown",blockBrown)
                  ,("cwOrange",blockOrange)
                  ,("cwBlack",blockBlack)
                  ,("cwWhite",blockWhite)
                  ,("cwCyan",blockCyan)
                  ,("cwMagenta",blockMagenta)
                  ,("cwYellow",blockYellow)
                  ,("cwAquamarine",blockAquamarine)
                  ,("cwAzure",blockAzure)
                  ,("cwViolet",blockViolet)
                  ,("cwChartreuse",blockChartreuse)
                  ,("cwRose",blockRose)
                  ,("cwPink",blockPink)
                  ,("cwPurple",blockPurple)
                  ,("cwGray",blockGray)
                  ,("cwMixed",blockMixed)
                  ,("cwLight",blockLight)
                  ,("cwDark",blockDark)
                  ,("cwBright",blockBright)
                  ,("cwDull",blockDull)
                  ,("cwTranslucent",blockTranslucent)
                  ,("cwRGBA",blockRGBA)
                  -- LOGIC
                  ,("conIf",blockIf)
                  ,("conAnd",blockAnd)
                  ,("conOr",blockOr)
                  ,("conNot",blockNot)
                  ,("conEq",blockEq)
                  ,("conNeq",blockNeq)
                  ,("conTrue",blockTrue)
                  ,("conFalse",blockFalse)
                  ,("conGreater",blockGreater)
                  ,("conGeq",blockGeq)
                  ,("conLess",blockLess)
                  ,("conLeq",blockLeq)
                  ,("conEven",blockEven)
                  ,("conOdd",blockOdd)
                  ,("conStartWith",blockStartWith)
                  ,("conEndWith",blockEndWith)
                  ,("letVar",blockLetVar)
                  ,("comment",blockComment)
                    ]
                                

setCodeGen :: String -> (Block -> Either Block (String, OrderConstant) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 case func b of
                                    Right (code,ordr) -> do
                                            return $ js_makeArray (pack code) (order ordr)
                                    Left eblock -> do
                                            addErrorSelect eblock
                                            js_removeErrorsDelay
                                            return $ js_makeArray (pack "") 0
                                 )
  js_setGenFunction (pack blockName) cb




-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
assignAll :: IO ()
assignAll = mapM_ (uncurry setCodeGen) blockCodeMap

valueToCode :: Block -> String -> OrderConstant -> Either Block String
valueToCode block name ordr =  
    case unpack $ js_valueToCode block (pack name) (order ordr) of
      "" ->  Left block
      val -> Right val
                -- case getInputBlock block name of
                --   Just inputBlock -> let val = unpack $ js_valueToCode block (pack name) (order ordr)
                --                      in if val=="" 
                --                         then Left block
                --                         else Right val 
                --   Nothing -> Left block

--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()


foreign import javascript unsafe "Blockly.FunBlocks.valueToCode($1, $2, $3)"
  js_valueToCode :: Block -> JSString -> Int -> JSString

-- TODO, fix, Ugly hack incoming
foreign import javascript unsafe "[$1,$2]"
  js_makeArray :: JSString -> Int -> JSVal

foreign import javascript unsafe "setTimeout(removeErrors,2000)"
  js_removeErrorsDelay :: IO ()

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
