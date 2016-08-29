{-# LANGUAGE OverloadedStrings #-}
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

module Blocks.Types(setBlockTypes, getTypeBlocks)
  where

import Blockly.DesignBlock 
import Blockly.General
import Blockly.Event
import Data.List(intersperse)
import qualified Data.Text as T

colorPicture = Color 160
colorNumber = Color 210
colorProgram = Color 0
colorColor = Color 290
colorPoly = Color 180
colorBool = Color 100
colorText = Color 45

typePicture = Picture
typeNumber = Number
typeProgram = Program
typeColor = Col
typeBool = Bool
typeText = Str
typeComment = Comment

inlineDef = Inline True

icon :: T.Text -> Field
icon name = FieldImage ("ims/" `T.append` name) 20 20


standardFunction cwName funcName ico types inputNames color tooltip = 
    DesignBlock cwName (Function funcName types)
      (header : (Dummy [Text "("])  : (argInputs ++ [ Dummy [Text ")"]] ) ) 
      inlineDef
      color
      (Tooltip tooltip)
  where
    header = case ico of
                Just i -> Dummy [TextE funcName, icon i] 
                Nothing -> Dummy [TextE funcName]
    argInputs = intersperse (Dummy [Text ","]) $ map (\name -> Value name []) inputNames

-- PICTURE ----------------------------------------------
cwBlank = DesignBlock "cwBlank" (Function "blank" [Picture])
          [Dummy 
            [TextE "blank"]
          ]
          inlineDef colorPicture 
          (Tooltip "Blank picture")

cwCoordinatePlane = DesignBlock "cwCoordinatePlane" (Function "coordinatePlane" [Picture] )
          [Dummy 
            [TextE "coordinatePlane"]
          ]
          inlineDef colorPicture 
          (Tooltip "Picture of coordinate plane")

cwCodeWorldLogo = DesignBlock "cwCodeWorldLogo" (Function "codeWorldLogo" [Picture])
          [Dummy 
            [TextE "codeWorldLogo"]
          ]
          inlineDef colorPicture 
          (Tooltip "Picture of CodeWorld logo")

cwText = DesignBlock "cwText" (Function "text" [typeText, Picture] )
          [Value "TEXT" [TextE "text" ] ]
          inlineDef colorPicture 
          (Tooltip "Enter some text")

cwDrawingOf = DesignBlock "cwDrawingOf" (Top "drawingOf" [typePicture, typeProgram])
          [Dummy [Text "(", TextE "drawingOf", icon "tooltip-image.svg"] 
            ,Value "VALUE" []
            ,Dummy [Text ")"]] 
          inlineDef colorProgram 
          (Tooltip "Displays a drawing of a picture")


cwCircle = standardFunction "cwCircle" "circle" Nothing [typeNumber, typePicture] ["RADIUS"] colorPicture "Picture of a circle"

cwThickCircle = standardFunction "cwThickCircle" "thickCircle" Nothing [typeNumber, typeNumber, typePicture] ["RADIUS", "LINEWIDTH"] 
                  colorPicture "Picture of a circle with a border"

cwSolidCircle = standardFunction "cwSolidCircle" "solidCircle" Nothing [typeNumber, typePicture] ["RADIUS"] 
                  colorPicture "Picture of a solid circle"

cwRectangle = standardFunction "cwRectangle" "rectangle" Nothing [typeNumber, typeNumber, typePicture] ["WIDTH", "HEIGHT"]
                colorPicture "Picture of a rectangle"

cwThickRectangle = standardFunction "cwThickRectangle" "thickRectangle" Nothing 
                    [typeNumber, typeNumber, typeNumber, typePicture] ["WIDTH", "HEIGHT", "LINEWIDTH"]
                    colorPicture "Picture of a rectangle with a border"

cwSolidRectangle = standardFunction "cwSolidRectangle" "solidRectangle" Nothing [typeNumber, typeNumber, typePicture] ["WIDTH", "HEIGHT"]
                      colorPicture "Picture of a solid rectangle"

cwArc = standardFunction "cwArc" "arc" Nothing [typeNumber, typeNumber, typeNumber, typePicture] ["STARTANGLE", "ENDANGLE", "RADIUS"]
          colorPicture "A thin arc"

cwSector = standardFunction "cwSector" "sector" Nothing [typeNumber, typeNumber, typeNumber, typePicture] ["STARTANGLE", "ENDANGLE", "RADIUS"]
              colorPicture "A solid sector of a circle"

cwThickArc = standardFunction "cwThickArc" "thickArc" Nothing [typeNumber, typeNumber, typeNumber, typeNumber, typePicture]
                ["STARTANGLE", "ENDANGLE", "RADIUS", "LINEWIDTH"]
                colorPicture "An arc with variable line width"

-- Transformations -----------------------------------------------
cwColored = standardFunction "cwColored" "colored" (Just "format-color-fill.svg") [typePicture, typeColor, typePicture]
              ["PICTURE", "COLOR"] colorPicture "A colored picture"

cwTranslate = standardFunction "cwTranslate" "translated" (Just "cursor-move.svg") [typePicture, typeNumber, typeNumber, typePicture]
                ["PICTURE", "X", "Y"] colorPicture "A translated picture"

cwScale = standardFunction "cwScale" "scaled" (Just "move-resize-variant.svg") [typePicture, typeNumber, typeNumber, typePicture]
            ["PICTURE", "HORZ", "VERTZ"] colorPicture "A scaled picture"

cwRotate = standardFunction "cwRotate" "rotated" (Just "rotate-3d.svg") [typePicture, typeNumber, typePicture ]
              ["PICTURE", "ANGLE"] colorPicture "A rotated picture"

-- NUMBERS ---------------------------------------------

numAdd = DesignBlock "numAdd" (Function "+" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "+"] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Add two numbers")

numSub = DesignBlock "numSub" (Function "-" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "-"] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Subtract two numbers")

numMult = DesignBlock "numMult" (Function "*" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\xD7"] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Multiply two numbers")

numDiv = DesignBlock "numDiv" (Function "/" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\xF7"] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Divide two numbers")

numExp = DesignBlock "numExp" (Function "^" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "^"] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Raise a number to a power")

numMax = standardFunction "numMax" "max" (Just "arrow-up.svg") [typeNumber, typeNumber, typeNumber]
            ["LEFT", "RIGHT"] colorNumber "The maximum of two numbers"

numMin = standardFunction "numMin" "min" (Just "arrow-down.svg") [typeNumber, typeNumber, typeNumber]
            ["LEFT", "RIGHT"] colorNumber "Take the minimum of two numbers"

numOpposite = standardFunction "numOpposite" "opposite" (Just "minus-box.svg") [typeNumber, typeNumber]
                ["NUM"] colorNumber "The opposite of a number"

numAbs = standardFunction "numAbs" "abs" Nothing [typeNumber, typeNumber]
            ["NUM"] colorNumber "The absolute value of a number"

numRound = standardFunction "numRound" "round" Nothing [typeNumber, typeNumber]
            ["NUM"] colorNumber "The rounded value of a number"

numReciprocal = standardFunction "numReciprocal" "reciprocal" Nothing [typeNumber, typeNumber]
                  ["NUM"] colorNumber "The reciprocal of a number"

numQuot = standardFunction "numQuot" "quotient" Nothing [typeNumber, typeNumber, typeNumber]
              ["LEFT", "RIGHT"] colorNumber "The integer part when dividing two numbers"

numRem = standardFunction "numRem" "remainder" Nothing [typeNumber, typeNumber, typeNumber]
            ["LEFT", "RIGHT"] colorNumber "The remainder when dividing two numbers"

numPi = DesignBlock "numPi" (Function "pi" [typeNumber])
          [Dummy 
            [TextE "\x3C0"]
          ]
          inlineDef colorNumber 
          (Tooltip "The number pi, 3.1415..")

numSqrt = DesignBlock "numSqrt" (Function "sqrt" [typeNumber, typeNumber])
        [Value "NUM" [TextE "\x221A"] ]
         (Inline True) colorNumber 
         (Tooltip "Gives the square root of a number")

numGCD = standardFunction "numGCD" "gcd" Nothing [typeNumber, typeNumber, typeNumber]
            ["LEFT", "RIGHT"] colorNumber "The greatest common demonitator between two numbers"

numLCM = standardFunction "numLCM" "lcm" Nothing [typeNumber, typeNumber, typeNumber]
          ["LEFT", "RIGHT"] colorNumber "The least common multiple between two numbers"


-- TEXT ------------------------------------------------

txtPrinted = standardFunction "txtPrinted" "printed" Nothing [typeNumber, typeText]
              ["TEXT"] colorText "The text value of a number"

txtLowercase = standardFunction "txtLowercase" "lowercase" Nothing [typeText, typeText]
                ["TEXT"] colorText "The text in lowercase"

txtUppercase = standardFunction "txtUppercase" "uppercase" Nothing [typeText, typeText]
                ["TEXT"] colorText "The text in uppercase"

-- A note to future readers, capitalized in GHJCS crashes, so this isn't used.
-- Issue #159
txtCapitalized = DesignBlock "txtCapitalized" (Function "capitalized" [typeText, typeText])
        [ Value "TEXT"  [TextE "capitalized"] ]
         (Inline True) colorText 
         (Tooltip "Gives the text with the first value capitalized")

-- COLORS ----------------------------------------------
cwBlue = DesignBlock "cwBlue" (Function "blue" [typeColor])
          [Dummy 
            [TextE "blue"]
          ]
          inlineDef colorColor 
          (Tooltip "The color blue")

cwRed = DesignBlock "cwRed" (Function "red" [typeColor])
          [Dummy 
            [TextE "red"]
          ]
          inlineDef colorColor 
          (Tooltip "The color red")

cwGreen = DesignBlock "cwGreen" (Function "green" [typeColor])
          [Dummy 
            [TextE "green"]
          ]
          inlineDef colorColor 
          (Tooltip "The color green")

cwOrange = DesignBlock "cwOrange" (Function "orange" [typeColor])
          [Dummy 
            [TextE "orange"]
          ]
          inlineDef colorColor 
          (Tooltip "The color orange")

cwBrown = DesignBlock "cwBrown" (Function "brown" [typeColor])
          [Dummy 
            [TextE "brown"]
          ]
          inlineDef colorColor 
          (Tooltip "The color brown")

cwBlack = DesignBlock "cwBlack" (Function "black" [typeColor])
          [Dummy 
            [TextE "black"]
          ]
          inlineDef colorColor 
          (Tooltip "The color black")

cwWhite = DesignBlock "cwWhite" (Function "white" [typeColor])
          [Dummy 
            [TextE "white"]
          ]
          inlineDef colorColor 
          (Tooltip "The color white")

cwCyan = DesignBlock "cwCyan" (Function "cyan" [typeColor])
          [Dummy 
            [TextE "cyan"]
          ]
          inlineDef colorColor 
          (Tooltip "The color cyan")

cwMagenta = DesignBlock "cwMagenta" (Function "magenta" [typeColor])
          [Dummy 
            [TextE "magenta"]
          ]
          inlineDef colorColor 
          (Tooltip "The color magenta")

cwYellow = DesignBlock "cwYellow" (Function "yellow" [typeColor])
          [Dummy 
            [TextE "yellow"]
          ]
          inlineDef colorColor 
          (Tooltip "The color yellow")

cwAquamarine = DesignBlock "cwAquamarine" (Function "aquamarine" [typeColor])
          [Dummy 
            [TextE "aquamarine"]
          ]
          inlineDef colorColor 
          (Tooltip "The color aquamarine")

cwAzure = DesignBlock "cwAzure" (Function "azure" [typeColor])
          [Dummy 
            [TextE "azure"]
          ]
          inlineDef colorColor 
          (Tooltip "The color azure")

cwViolet = DesignBlock "cwViolet" (Function "violet" [typeColor])
          [Dummy 
            [TextE "violet"]
          ]
          inlineDef colorColor 
          (Tooltip "The color violet")

cwChartreuse = DesignBlock "cwChartreuse" (Function "chartreuse" [typeColor])
          [Dummy 
            [TextE "chartreuse"]
          ]
          inlineDef colorColor 
          (Tooltip "The color chartreuse")

cwRose = DesignBlock "cwRose" (Function "rose" [typeColor])
          [Dummy 
            [TextE "rose"]
          ]
          inlineDef colorColor 
          (Tooltip "The color rose")

cwPink = DesignBlock "cwPink" (Function "pink" [typeColor])
          [Dummy 
            [TextE "pink"]
          ]
          inlineDef colorColor 
          (Tooltip "The color pink")

cwPurple = DesignBlock "cwPurple" (Function "purple" [typeColor])
          [Dummy 
            [TextE "purple"]
          ]
          inlineDef colorColor 
          (Tooltip "The color purple")

cwGray = standardFunction "cwGray" "gray" Nothing [typeNumber, typeColor]
          ["VALUE"] colorColor "The color gray, varying by an amount. Lower value is closer to black"

cwMixed = standardFunction "cwMixed" "mixed" (Just "pot-mix.svg")  [typeColor, typeColor, typeColor]
            ["COL1", "COL2"] colorColor "Two mix of two colors"

cwLight = standardFunction "cwLight" "light" Nothing [typeColor, typeColor]
            ["COL"] colorColor "A lighter color"

cwDark = standardFunction "cwDark" "dark" Nothing [typeColor, typeColor]
            ["COL"] colorColor "A darker color"

cwBright = standardFunction "cwBright" "bright" Nothing [typeColor, typeColor]
            ["COL"] colorColor "A brighter color"

cwDull = standardFunction "cwDull" "dull" Nothing [typeColor, typeColor]
            ["COL"] colorColor "A more dull color"

cwTranslucent = standardFunction "cwTranslucent" "translucent" Nothing [typeColor, typeColor]
                  ["COL"] colorColor "A more translucent color"

cwRGBA = DesignBlock "cwRGBA" (Function "RGBA" [typeNumber, typeNumber, typeNumber, typeNumber, typeColor])
          [Dummy [TextE "RGBA"] 
           ,Value "RED"  [Text "Red"] 
           ,Value "GREEN"  [Text "Green"] 
           ,Value "BLUE"  [Text "Blue"] 
           ,Value "ALPHA"  [Text "Alpha"] ] 
          (Inline False) colorColor 
          (Tooltip "Makes a color with the given red, blue, green and alpha values")

-- LOGIC -------------------------------------------
conIf = DesignBlock "conIf" (Function "if" [typeBool, Poly "a", Poly "a", Poly "a"])
        [ Value "IF"  [TextE "if"] 
         ,Value "THEN" [Text "then"]
         ,Value "ELSE" [Text "else"] 
         ]
         inlineDef colorPoly 
         (Tooltip "if condition is true then give a else b")

conAnd = DesignBlock "conAnd" (Function "&&" [typeBool, typeBool, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "and"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Logical AND operation")

conOr = DesignBlock "conOr" (Function "||" [typeBool, typeBool, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "or"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Logical OR operation")

conNot = DesignBlock "conNot" (Function "not" [typeBool, typeBool])
        [ Value "VALUE"  [TextE "not"] ]
         (Inline True) colorBool 
         (Tooltip "Negation of the logical value")

conEq = DesignBlock "conEq" (Function "==" [Poly "a", Poly "a", typeBool]) 
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "="]
         ]
         (Inline True) colorBool 
         (Tooltip "Are two items equal")

conNeq = DesignBlock "conNeq" (Function "/=" [Poly "a", Poly "a", typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\x2260"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Are two items not equal")

conTrue = DesignBlock "conTrue" (Function "True" [typeBool])
          [Dummy 
            [TextE "True"]
          ]
          inlineDef colorBool 
          (Tooltip "True logic value")

conFalse = DesignBlock "conFalse" (Function "False" [typeBool])
          [Dummy 
            [TextE "False"]
          ]
          inlineDef colorBool 
          (Tooltip "False logic value")

conGreater = DesignBlock "conGreater" (Function ">" [typeNumber, typeNumber, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE ">"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is greater than the other")

conGeq = DesignBlock "conGeq" (Function ">=" [typeNumber, typeNumber, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\x2265"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is greater than or equal to ther other")

conLess = DesignBlock "conLess" (Function "<" [typeNumber, typeNumber, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "<"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is less than the other")

conLeq = DesignBlock "conLeq" (Function "<=" [typeNumber, typeNumber, typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\x2264"] 
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is less than or equal to ther other")

conEven = DesignBlock "conEven" (Function "even" [typeNumber, typeBool])
        [ Value "VALUE"  [TextE "even"] ]
         (Inline True) colorBool 
         (Tooltip "Tells whether the number is even")

conOdd = DesignBlock "conOdd" (Function "odd" [typeNumber, typeBool])
        [ Value "VALUE"  [TextE "odd"] ]
         (Inline True) colorBool 
         (Tooltip "Tells whether the number is odd")

conStartWith = DesignBlock "conStartWith" (Function "startWith" [typeText, typeText, typeBool] )
          [Dummy [TextE "startsWith"] 
           ,Value "TEXTMAIN"  [] 
           ,Value "TEXTTEST"  [] ] 
          inlineDef colorBool 
          (Tooltip "Tells whether the given text starts with some other text")

conEndWith = DesignBlock "conEndWith" (Function "endWith" [typeText, typeText, typeBool])
          [Dummy [TextE "endsWith"] 
           ,Value "TEXTMAIN"  [] 
           ,Value "TEXTTEST"  [] ]
          inlineDef colorBool 
          (Tooltip "Tells whether the given text ends with some other text")

-- LISTS ----------------------------------------------
lstGenNum = DesignBlock "lstGenNum" (Function ".." [typeNumber, typeNumber, typeNumber])
        [ Dummy [Text "["]
         ,Value "LEFT"  [] 
         ,Value "RIGHT" [TextE ".."] 
         ,Dummy [Text "]"]
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is greater than the other")



comment = DesignBlock "comment" None
          [Dummy 
            [TextInput "" "TEXT",
            TextE "--"]
          ]
          inlineDef (Color 260) 
          (Tooltip "Enter a comment")

getTypeBlocks :: [T.Text]
getTypeBlocks = map (\(DesignBlock name _  _ _ _ _) -> name) blockTypes

blockTypes = [ 
              -- PICTURE
              cwBlank
              ,cwCoordinatePlane
              ,cwCodeWorldLogo
              ,cwText
              ,cwDrawingOf
              ,cwCircle
              ,cwThickCircle
              ,cwSolidCircle
              ,cwRectangle
              ,cwThickRectangle
              ,cwSolidRectangle
              ,cwArc
              ,cwSector
              ,cwThickArc
              -- TRANSFORMATIONS
              ,cwColored
              ,cwTranslate
              ,cwRotate
              ,cwScale
              -- NUMBERS
              --,numNumber
              ,numAdd
              ,numSub
              ,numMult
              ,numDiv
              ,numExp
              ,numMax
              ,numMin
              ,numOpposite
              ,numAbs
              ,numRound
              ,numReciprocal
              ,numQuot
              ,numRem
              ,numPi
              ,numSqrt
              ,numGCD
              ,numLCM
              -- TEXT
              ,txtPrinted
              ,txtLowercase
              ,txtUppercase
              ,txtCapitalized -- Added, but not in the toolbox. It crashed GHCJS, #159
              -- COLORS
              ,cwBlue
              ,cwRed
              ,cwGreen
              ,cwBrown
              ,cwOrange
              ,cwBlack
              ,cwWhite
              ,cwCyan
              ,cwMagenta
              ,cwYellow
              ,cwAquamarine
              ,cwAzure
              ,cwViolet
              ,cwChartreuse
              ,cwRose
              ,cwPink
              ,cwPurple
              ,cwGray
              ,cwMixed
              ,cwLight
              ,cwDark
              ,cwBright
              ,cwDull
              ,cwTranslucent
              ,cwRGBA
              -- LOGIC
              ,conIf
              ,conAnd
              ,conOr
              ,conNot
              ,conEq
              ,conNeq
              ,conTrue
              ,conFalse
              ,conGreater
              ,conGeq
              ,conLess
              ,conLeq
              ,conEven
              ,conOdd
              ,conStartWith
              ,conEndWith
              ,comment
                ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
setBlockTypes :: IO ()
setBlockTypes = mapM_ setBlockType blockTypes
