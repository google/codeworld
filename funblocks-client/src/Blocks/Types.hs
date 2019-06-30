{-# LANGUAGE OverloadedStrings #-}
{-
  Copyright 2019 The CodeWorld Authors. All Rights Reserved.

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
typeBool = Truth
typeText = Str
typeComment = Comment

inlineDef = Inline True

icon :: T.Text -> Field
icon name = FieldImage ("ims/" `T.append` name) 20 20

standardFunction cwName funcName ico types [] color tooltip =
    DesignBlock cwName (Function funcName types) [header] inlineDef color (Tooltip tooltip)
  where
    header = case ico of
                Just i -> Dummy [TextE funcName, icon i]
                Nothing -> Dummy [TextE funcName]

standardFunction cwName funcName ico types inputNames color tooltip =
    DesignBlock cwName (Function funcName types)
      (header : (argInputs ++ [Dummy [Text ")"]]))
      inlineDef
      color
      (Tooltip tooltip)
  where
    header = case ico of
                Just i -> Value (head inputNames) [Text "(", TextE funcName, icon i]
                Nothing -> Value (head inputNames) [Text "(", TextE funcName]
    argInputs = map (\name -> Value name [Text ","]) (tail inputNames)

-- PICTURE ----------------------------------------------
cwBlank = standardFunction "cwBlank" "blank" Nothing [Picture] [] colorPicture "Blank picture"

cwCoordinatePlane = standardFunction "cwCoordinatePlane" "coordinatePlane" Nothing [Picture] [] colorPicture "Picture of coordinate plane"

cwCodeWorldLogo = standardFunction "cwCodeWorldLogo" "codeWorldLogo" Nothing [Picture] [] colorPicture "Picture of CodeWorld logo"

cwLettering = standardFunction "cwLettering" "lettering" Nothing [typeText, Picture]
            ["TEXT"] colorPicture "Picture of text"

cwDrawingOf = DesignBlock "cwDrawingOf" (Top "drawingOf" [typePicture, typeProgram])
          [Value "VALUE" [Text "(", TextE "drawingOf", icon "shape-plus.svg"], Dummy [Text ")"]]
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

numRound = standardFunction "numRound" "rounded" Nothing [typeNumber, typeNumber]
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
              [Value "NUM" [TextE "\x221A"], Dummy []]
              (Inline True) colorNumber 
              (Tooltip "Gives the square root of a number")

numGCD = standardFunction "numGCD" "gcd" Nothing [typeNumber, typeNumber, typeNumber]
            ["LEFT", "RIGHT"] colorNumber "The greatest common demonitator between two numbers"

numLCM = standardFunction "numLCM" "lcm" Nothing [typeNumber, typeNumber, typeNumber]
          ["LEFT", "RIGHT"] colorNumber "The least common multiple between two numbers"

numSin = standardFunction "numSin" "sin" Nothing [typeNumber, typeNumber]
           ["VAL"] colorNumber "The sine of an angle"

numCos = standardFunction "numCos" "cos" Nothing [typeNumber, typeNumber]
           ["VAL"] colorNumber "The cosine of an angle"

-- TEXT ------------------------------------------------

txtPrinted = standardFunction "txtPrinted" "printed" Nothing [typeNumber, typeText]
              ["TEXT"] colorText "The text value of a number"

txtLowercase = standardFunction "txtLowercase" "lowercase" Nothing [typeText, typeText]
                ["TEXT"] colorText "The text in lowercase"

txtUppercase = standardFunction "txtUppercase" "uppercase" Nothing [typeText, typeText]
                ["TEXT"] colorText "The text in uppercase"

-- COLORS ----------------------------------------------
cwBlue = standardFunction "cwBlue" "blue" Nothing [typeColor] [] colorColor "The color blue"
cwRed = standardFunction "cwRed" "red" Nothing [typeColor] [] colorColor "The color red"
cwGreen = standardFunction "cwGreen" "green" Nothing [typeColor] [] colorColor "The color green"
cwOrange = standardFunction "cwOrange" "orange" Nothing [typeColor] [] colorColor "The color orange"
cwBrown = standardFunction "cwBrown" "brown" Nothing [typeColor] [] colorColor "The color brown"
cwBlack = standardFunction "cwBlack" "black" Nothing [typeColor] [] colorColor "The color black"
cwWhite = standardFunction "cwWhite" "white" Nothing [typeColor] [] colorColor "The color white"
cwCyan = standardFunction "cwCyan" "cyan" Nothing [typeColor] [] colorColor "The color cyan"
cwMagenta = standardFunction "cwMagenta" "magenta" Nothing [typeColor] [] colorColor "The color magenta"
cwYellow = standardFunction "cwYellow" "yellow" Nothing [typeColor] [] colorColor "The color yellow"
cwAquamarine = standardFunction "cwAquamarine" "aquamarine" Nothing [typeColor] [] colorColor "The color aquamarine"
cwAzure = standardFunction "cwAzure" "azure" Nothing [typeColor] [] colorColor "The color azure"
cwViolet = standardFunction "cwViolet" "violet" Nothing [typeColor] [] colorColor "The color violet"
cwChartreuse = standardFunction "cwChartreuse" "chartreuse" Nothing [typeColor] [] colorColor "The color chartreuse"
cwRose = standardFunction "cwRose" "rose" Nothing [typeColor] [] colorColor "The color rose"
cwPink = standardFunction "cwPink" "pink" Nothing [typeColor] [] colorColor "The color pink"
cwPurple = standardFunction "cwPurple" "purple" Nothing [typeColor] [] colorColor "The color purple"
cwGray = standardFunction "cwGray" "gray" Nothing [typeNumber] [] colorColor "The color gray"

cwMixed = standardFunction "cwMixed" "mixed" (Just "pot-mix.svg") [List typeColor, typeColor] 
            ["COL"] colorColor "Mix of a list of colors"

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

cwRGB = standardFunction "cwRGB" "RGB" Nothing [typeNumber, typeNumber, typeNumber, typeColor]
             ["RED", "GREEN", "BLUE"] colorColor
             "Makes a color with the given red, green, and blue portions"

cwRGBA = standardFunction "cwRGBA" "RGBA" Nothing [typeNumber, typeNumber, typeNumber, typeNumber, typeColor]
             ["RED", "GREEN", "BLUE", "ALPHA"] colorColor
             "Makes a color with the given red, green, blue and alpha portions"

cwHSL = standardFunction "cwHSL" "HSL" Nothing [typeNumber, typeNumber, typeNumber, typeColor]
             ["HUE", "SAT", "LUM"] colorColor
             "Makes a color with the given hue angle, saturation, and luminosity"

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

conNot = standardFunction "conNot" "not" Nothing [typeBool, typeBool]
            ["VALUE"] colorBool "Negation of logical value"

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

conTrue = standardFunction "conTrue" "True" Nothing [typeBool] [] colorBool "True logic value"
conFalse = standardFunction "conFalse" "False" Nothing [typeBool] [] colorBool "False logic value"

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

conEven = standardFunction "conEven" "even" Nothing [typeNumber, typeBool]
            ["VALUE"] colorBool "True if the number is even"

conOdd = standardFunction "conOdd" "odd" Nothing [typeNumber, typeBool]
            ["VALUE"] colorBool "True if the number is odd"

conStartWith = standardFunction "conStartWith" "startsWith" Nothing [typeText, typeText, typeBool]
                ["TEXTMAIN", "TEXTTEST"] colorBool "Test whether the text starts with the characters of the other text"

conEndWith = standardFunction "conEndWith" "endsWith" Nothing [typeText, typeText, typeBool]
              ["TEXTMAIN", "TEXTTEST"] colorBool "Test whether the text ends with the characters of the other text"

-- LISTS ----------------------------------------------
lstGenNum = DesignBlock "lstGenNum" (Function ".." [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT" [Text "["]
         ,Value "RIGHT" [TextE ".."] 
         ,Dummy [Text "]"]
         ]
         (Inline True) colorBool 
         (Tooltip "Tells whether one number is greater than the other")

comment = DesignBlock "comment" None
          [Dummy [TextInput "" "TEXT", TextE "--"]]
          inlineDef (Color 260) (Tooltip "Enter a comment")

getTypeBlocks :: [T.Text]
getTypeBlocks = map (\(DesignBlock name _  _ _ _ _) -> name) blockTypes

blockTypes = [ 
              -- PICTURE
              cwBlank
              ,cwCoordinatePlane
              ,cwCodeWorldLogo
              ,cwLettering
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
              ,numSin
              ,numCos
              -- TEXT
              ,txtPrinted
              ,txtLowercase
              ,txtUppercase
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
              ,cwRGB
              ,cwHSL
              -- LOGIC
              -- ,conIf
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
