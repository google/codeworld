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

module Blocks.CodeGen (assignAll
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

-- Helpers for converting Text
(++) :: T.Text -> T.Text -> T.Text
a ++ b = a `T.append` b
pack = textToJSString
unpack = textFromJSString
show :: Show a => a -> T.Text
show = T.pack . P.show

-- Helper functions
member :: Code -> (Code, OrderConstant)
member code = (code, CMember)
none :: Code ->(Code, OrderConstant)
none code = (code, CNone)
atomic :: Code ->(Code, OrderConstant)
atomic code = (code, CAtomic)


type Code = T.Text
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
    return $ none $ "thickRectangle(" ++ width ++ "," ++ height ++ "," ++ linewidth ++ ")"

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

blockPath :: GeneratorFunction
blockPath block = do
    list <- valueToCode block "LST" CNone
    return $ none $ "path (" ++ list ++ ")"

-- TRANSFORMATIONS ------------------------------------------------------

blockCombine :: GeneratorFunction
blockCombine block = do
    pic1 <- valueToCode block "PIC1" CCombine
    pic2 <- valueToCode block "PIC2" CCombine
    return ( pic1 ++ " & " ++ pic2, CCombine)

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

blockNumberPerc :: GeneratorFunction
blockNumberPerc block = do 
    let arg = getFieldValue block "NUMBER"
    let numb = (read (T.unpack arg) :: Float) * 0.01
    return $ none (show numb)

blockAdd :: GeneratorFunction
blockAdd block = do 
    left <- valueToCode block "LEFT" CAddition
    right <- valueToCode block "RIGHT" CAddition
    return (left ++ " + " ++ right, CAddition)

blockSub :: GeneratorFunction
blockSub block = do 
    left <- valueToCode block "LEFT" CSubtraction
    right <- valueToCode block "RIGHT" CSubtraction
    return (left ++ " - " ++ right, CSubtraction)

blockMult :: GeneratorFunction
blockMult block = do 
    left <- valueToCode block "LEFT" CMultiplication
    right <- valueToCode block "RIGHT" CMultiplication
    return (left ++ " * " ++ right, CMultiplication)

blockDiv :: GeneratorFunction
blockDiv block = do 
    left <- valueToCode block "LEFT" CDivision
    right <- valueToCode block "RIGHT" CDivision
    return (left ++ " / " ++ right, CDivision)

blockExp :: GeneratorFunction
blockExp block = do 
    left <- valueToCode block "LEFT" CExponentiation
    right <- valueToCode block "RIGHT" CExponentiation
    return (left ++ "^" ++ right, CExponentiation)

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
    return $ none $ "rounded(" ++ num ++ ")"

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
    return $ none $ escape txt 

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
    return $ none $ "RGBA(" ++ red ++ "," ++ green ++ "," ++ blue ++ "," ++ alpha ++ ")" 

-- Programs
blockLetVar :: GeneratorFunction
blockLetVar block = do 
    let varName = getFieldValue block "NAME" 
    expr <- valueToCode block "RETURN" CNone
    return $ none $ varName ++ " = " ++ expr 



-- Let function block with parameters
foreign import javascript unsafe "$1.arguments_"
  js_funcargs :: Block -> JA.JSArray

blockLetFunc :: GeneratorFunction
blockLetFunc block = do 
    let varName = getFieldValue block "NAME" 
    let vars = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    let varCode = if not $ null vars 
              then "(" ++ T.intercalate "," vars ++ ")"
              else ""
    expr <- valueToCode block "RETURN" CNone
    return $ none $ varName ++ varCode ++ " = " ++ expr 

blockLetCall :: GeneratorFunction
blockLetCall block = do 
    let varName = getFieldValue block "NAME" 
    let args = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_funcargs block
    vals <- mapM (\t -> valueToCode block t CNone) ["ARG" ++ show i | i <- [0..length args - 1]]
    let argCode = if null vals
              then ""
              else "(" ++ T.intercalate "," vals ++ ")"

    return $ none $ varName ++ argCode 

blockLocalVar :: GeneratorFunction
blockLocalVar block = do 
    let varName = getFieldValue block "NAME" 
    return $ none varName 

blockFuncVar :: GeneratorFunction
blockFuncVar block = do 
    let arg = getFieldValue block "VAR"
    if arg == "None"
      then Left block
      else return $ none arg

-- ANIMATION
blockAnim :: GeneratorFunction
blockAnim block =  
    case getInputBlock block "FUNC" of
      Just inpBlock -> do
                       let funcName = getFieldValue inpBlock "NAME"
                       return $ none $ "main = animationOf(" ++ funcName ++ ")"
      Nothing -> Left block

-- COMMENT
blockComment :: GeneratorFunction
blockComment block = return $ none ""

-- Tuples
blockCreatePair :: GeneratorFunction
blockCreatePair block = do 
    first <- valueToCode block "FIRST" CNone
    second <- valueToCode block "SECOND" CNone
    return $ none $ "(" ++ first ++ "," ++ second ++ ")" 

blockFst :: GeneratorFunction
blockFst block = do 
    pair <- valueToCode block "PAIR" CNone
    return ("fst (" ++ pair ++ ")" , CNone)

blockSnd :: GeneratorFunction
blockSnd block = do 
    pair <- valueToCode block "PAIR" CNone
    return ("snd (" ++ pair ++ ")" , CNone)

-- LISTS

blockCreateList :: GeneratorFunction
blockCreateList block = do
  let c = getItemCount block
  vals <- mapM (\t -> valueToCode block t CNone) ["ADD" ++ show i | i <- [0..c-1]]
  return $ none $ "[" ++ T.intercalate "," vals ++ "]"

blockCons :: GeneratorFunction
blockCons block = do 
    item <- valueToCode block "ITEM" CNone
    lst <- valueToCode block "LST" CNone
    return $ none $ item ++ ":" ++ lst


blockLength :: GeneratorFunction
blockLength block = do 
    lst <- valueToCode block "LST" CNone
    return $ none $ "length(" ++ lst ++ ")"

blockNumGen :: GeneratorFunction
blockNumGen block = do 
    left <- valueToCode block "LEFT" CNone
    right <- valueToCode block "RIGHT" CNone
    return $ none $ "[" ++ left ++ ".." ++ right ++ "]"

blockListVar :: GeneratorFunction
blockListVar block = do 
    let arg = getFieldValue block "VAR"
    if arg == "None"
      then Left block
      else return $ none arg

-- LIST COMPREHENSION
foreign import javascript unsafe "$1.varCount_"
  js_blockVarCount :: Block -> Int

foreign import javascript unsafe "$1.guardCount_"
  js_blockGuardCount :: Block -> Int

foreign import javascript unsafe "$1.vars_"
  js_blockVars :: Block -> JA.JSArray

blockListComp :: GeneratorFunction
blockListComp block = do 
    let varCount = js_blockVarCount block
    let guardCount = js_blockGuardCount block
    let vars = map unpack $ map (\n -> unsafeCoerce n :: JSString) $ 
                JA.toList $ js_blockVars block

    varCodes <- mapM (\t -> valueToCode block t CNone) ["VAR" ++ show i | i <- [0..varCount-1]]
    guards <- mapM (\t -> valueToCode block t CNone) ["GUARD" ++ show i | i <- [0..guardCount-1]]
    doCode <- valueToCode block "DO" CNone

    let varCode = T.intercalate "," $ zipWith (\var code -> var ++ " <- " ++ code) vars varCodes 
    let guardCode = T.intercalate "," guards
    let code = "[" ++ doCode ++ " | " ++ varCode ++ (if T.null guardCode then "" else "," ++ guardCode)
                ++ "]"
    return $ none code 

getGenerationBlocks :: [T.Text]
getGenerationBlocks = map fst blockCodeMap

blockCodeMap = [  -- PROGRAMS 
                   ("cwDrawingOf",blockDrawingOf)
                  ,("cwAnimationOf",blockAnim)
                  -- PICTURES
                  ,("cwBlank",blockBlank)
                  ,("cwCoordinatePlane",blockCoordinatePlane)
                  ,("cwCodeWorldLogo",blockCodeWorldLogo)
                  ,("cwText",blockText)
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
                  ,("numNumberPerc",blockNumberPerc)
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
                  ,("text_typed",blockString)
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
                  -- Tuples
                  ,("pair_create_typed", blockCreatePair)
                  ,("pair_first_typed", blockFst)
                  ,("pair_second_typed", blockSnd)
                  -- Lists
                  ,("lists_create_with_typed", blockCreateList)
                  ,("lists_length", blockLength)
                  ,("lists_cons", blockCons)
                  ,("lists_numgen", blockNumGen)
                  ,("lists_comprehension", blockListComp)
                  ,("variables_get_lists", blockListVar)
                  -- FUNCTIONS
                  ,("procedures_letVar",blockLetVar)
                  ,("procedures_letFunc",blockLetFunc)
                  ,("procedures_callreturn",blockLetCall)
                  ,("procedures_getVar",blockFuncVar)
                  ,("vars_local",blockLocalVar)
                  ,("comment",blockComment)
                  ,("lists_path",blockPath)
                    ]
                                

setCodeGen :: T.Text -> (Block -> Either Block (T.Text, OrderConstant) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 case func b of
                                    Right (code,ordr) -> do
                                            return $ js_makeArray (pack code) (order ordr)
                                    Left eblock -> do
                                            setWarningText eblock "Block contains an input field that is disconnected"
                                            addErrorSelect eblock
                                            js_removeErrorsDelay
                                            return $ js_makeArray (pack "") 0
                                 )
  js_setGenFunction (pack blockName) cb




-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
assignAll :: IO ()
assignAll = mapM_ (uncurry setCodeGen) blockCodeMap

valueToCode :: Block -> T.Text -> OrderConstant -> Either Block T.Text
valueToCode block name ordr = do
    inputBlock <- aux $ getInputBlock block name
    let blockType = getBlockType inputBlock
    func <- aux $ lookup blockType blockCodeMap
    (code,innerOrder) <- func inputBlock
    Right $ handleOrder (order innerOrder) (order ordr) code
  where
    aux m = case m of
      Just v -> Right v
      Nothing -> Left block

    handleOrder innerOrdr odrd code
      | innerOrdr == 0 || innerOrdr == 99 = code
    handleOrder innerOrdr ordr code = if ordr <= innerOrdr
                          then if ordr == innerOrdr && (ordr == 0 || ordr == 99)
                               then code
                               else "(" ++ code ++ ")"
                          else code

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

--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()


foreign import javascript unsafe "Blockly.FunBlocks.valueToCode($1, $2, $3)"
  js_valueToCode :: Block -> JSString -> Int -> JSString

-- TODO, fix, Ugly hack incoming
foreign import javascript unsafe "[$1,$2]"
  js_makeArray :: JSString -> Int -> JSVal

foreign import javascript unsafe "setTimeout(removeErrors,10000)"
  js_removeErrorsDelay :: IO ()

-- TODO, remove, was used for testing
alert :: T.Text -> IO ()
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
