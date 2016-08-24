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
import qualified Data.Text as T

colorPicture = Color 160
colorNumber = Color 210
colorProgram = Color 0
colorColor = Color 290
colorPoly = Color 70
colorBool = Color 100
colorText = Color 45

typePicture = Picture
typeNumber = Number
typeProgram = Program
typeColor = Col
typeBool = Bool
typeText = Str
typeComment = Comment

inlineDef = Inline False

icon :: T.Text -> Field
icon name = FieldImage ("ims/" `T.append` name) 20 20

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
          [Dummy [TextE "drawingOf", icon "tooltip-image.svg"] 
           ,Value "VALUE" [] ] 
          inlineDef colorProgram 
          (Tooltip "Displays a drawing of a picture")

cwCircle = DesignBlock "cwCircle" (Function "circle" [typeNumber, typePicture])
          [Dummy [TextE "circle"] 
           ,Value "RADIUS" [Text "Radius"]] 
          inlineDef colorPicture 
          (Tooltip "Picture of a circle")

cwThickCircle = DesignBlock "cwThickCircle" (Function "thickCircle"  [typeNumber, typeNumber, typePicture])
          [Dummy [TextE "thickCircle"] 
           ,Value "RADIUS" [Text "Radius"] 
           ,Value "LINEWIDTH" [Text "Line width"] ] 
          inlineDef colorPicture 
          (Tooltip "Picture of a circle")

cwSolidCircle = DesignBlock "cwSolidCircle" (Function "solidCircle" [typeNumber, typePicture] )
          [Dummy [TextE "solidCircle"] 
           ,Value "RADIUS"  [Text "Radius"] ] 
          inlineDef colorPicture 
          (Tooltip "Picture of a solid circle")

cwRectangle = DesignBlock "cwRectangle" (Function "rectangle" [typeNumber, typeNumber, typePicture] )
          [Dummy [TextE "rectangle"] 
           ,Value "WIDTH"  [Text "Width"] 
           ,Value "HEIGHT"  [Text "Height"] ] 
          inlineDef colorPicture 
          (Tooltip "Picture of a rectangle")

cwThickRectangle = DesignBlock "cwThickRectangle" (Function "thickRectangle" [typeNumber, typeNumber, typeNumber, typePicture] )
          [Dummy [TextE "thickRectangle"] 
           ,Value "WIDTH" [Text "Width"] 
           ,Value "HEIGHT" [Text "Height"] 
           ,Value "LINEWIDTH" [Text "Line Width"] ] 
          inlineDef colorPicture 
          (Tooltip "Picture of a rectangle")

cwSolidRectangle = DesignBlock "cwSolidRectangle" (Function "solidRectangle" [typeNumber, typeNumber, typePicture])
          [Dummy [TextE "solidRectangle"] 
           ,Value "WIDTH" [Text "Width"] 
           ,Value "HEIGHT" [Text "Height"] ] 
          inlineDef colorPicture 
          (Tooltip "Picture of a solid rectangle")

cwArc = DesignBlock "cwArc" (Function "arc" [typeNumber, typeNumber, typeNumber, typePicture] )
          [Dummy [TextE "arc"] 
            ,Value "STARTANGLE" [Text "Start Angle"] 
           ,Value "ENDANGLE" [Text "End Angle"] 
           ,Value "RADIUS" [Text "Radius"] ] 
          inlineDef colorPicture 
          (Tooltip "A thin arc")

cwSector = DesignBlock "cwSector" (Function "sector" [typeNumber, typeNumber, typeNumber, typePicture])
          [Dummy [TextE "sector"] 
            ,Value "STARTANGLE" [Text "Start Angle"] 
           ,Value "ENDANGLE" [Text "End Angle"] 
           ,Value "RADIUS" [Text "Radius"] ] 
          inlineDef colorPicture 
          (Tooltip "A solid sector of a circle")

cwThickArc = DesignBlock "cwThickArc" (Function "thickArc" [typeNumber, typeNumber, typeNumber, typeNumber, typePicture])
          [Dummy [TextE "thickArc"] 
            ,Value "STARTANGLE" [Text "Start Angle"] 
           ,Value "ENDANGLE" [Text "End Angle"] 
           ,Value "RADIUS" [Text "Radius"] 
           ,Value "LINEWIDTH" [Text "Line width"] ] 
          inlineDef colorPicture 
          (Tooltip "A arc with variable line width")

-- Transformations -----------------------------------------------
cwColored = DesignBlock "cwColored" (Function "colored" [typePicture, typeColor, typePicture])
          [Dummy [TextE "colored", icon "format-color-fill.svg"] 
           ,Value "PICTURE" []
           ,Value "COLOR" []
           ]
          (Inline True) colorPicture 
          (Tooltip "Change the color of a picture")

cwTranslate = DesignBlock "cwTranslate" (Function "translated" [typePicture, typeNumber, typeNumber, typePicture])
          [Dummy [TextE "translated", icon "cursor-move.svg"] 
           ,Value "PICTURE" [] 
           ,Value "X" []
           ,Value "Y" []
          ] 
          (Inline True) colorPicture 
          (Tooltip "Translate a picture")

cwScale = DesignBlock "cwScale" (Function "scaled" [typePicture, typeNumber, typeNumber, typePicture])
          [Dummy [TextE "scaled" , icon "move-resize-variant.svg"] 
           ,Value "PICTURE" [] 
           ,Value "HORZ" []
           ,Value "VERTZ" []
          ] 
          (Inline True) colorPicture 
          (Tooltip "Scale a picture")

cwRotate = DesignBlock "cwRotate" (Function "rotated" [typePicture, typeNumber, typePicture ])
          [Dummy [TextE "rotated", icon "rotate-3d.svg"] 
           ,Value "PICTURE" []
           ,Value "ANGLE" []
          ] 
          (Inline True) colorPicture 
          (Tooltip "Rotate")

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

numMax = DesignBlock "numMax" (Function "max" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "max", icon "arrow-up.svg"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Take the maximum of two numbers")

numMin = DesignBlock "numMin" (Function "min" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "min", icon "arrow-down.svg"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Take the minimum of two numbers")

numOpposite = DesignBlock "numOpposite" (Function "opposite" [typeNumber, typeNumber])
        [Value "NUM" [TextE "opposite", icon "minus-box.svg"] ]
         (Inline True) colorNumber 
         (Tooltip "Gives the negative of a number")

numAbs = DesignBlock "numAbs" (Function "abs" [typeNumber, typeNumber])
        [Value "NUM" [TextE "abs"] ]
         (Inline True) colorNumber 
         (Tooltip "Gives the absolute value of a number")

numRound = DesignBlock "numRound" (Function "round" [typeNumber, typeNumber])
        [Value "NUM" [TextE "round"] ]
         (Inline True) colorNumber 
         (Tooltip "Gives the number rounded to the nearest integer")

numReciprocal = DesignBlock "numReciprocal" (Function "reciprocal" [typeNumber, typeNumber])
        [Value "NUM" [TextE "reciprocal"] ]
         (Inline True) colorNumber 
         (Tooltip "Gives the reciprocal of a number")

numQuot = DesignBlock "numQuot" (Function "quotient" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "quotient"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Gives the integer part of the result when dividing two numbers")

numRem = DesignBlock "numRem" (Function "remainder" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "remainder"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Gives the remainder when dividing two numbers")

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

numGCD = DesignBlock "numGCD" (Function "gcd" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "gcd"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Gives the greatest common demoninator between two numbers")

numLCM = DesignBlock "numLCM" (Function "lcm" [typeNumber, typeNumber, typeNumber])
        [ Value "LEFT"  [TextE "lcm"] 
         ,Value "RIGHT" [] 
         ]
         (Inline True) colorNumber 
         (Tooltip "Gives the least common multiple between two numbers")

-- TEXT ------------------------------------------------

txtConcat = DesignBlock "txtConcat" (Function "<>" [typeText, typeText, typeText])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "<>"] 
         ]
         (Inline True) colorText 
         (Tooltip "Concatenates two pieces of text together")

txtPrinted = DesignBlock "txtPrinted" (Function "printed" [typeNumber, typeText])
        [ Value "TEXT"  [TextE "printed"] ]
         (Inline True) colorText 
         (Tooltip "Gives the text value of a number")

txtLowercase = DesignBlock "txtLowercase" (Function "lowercase" [typeText, typeText])
        [ Value "TEXT"  [TextE "lowercase"] ]
         (Inline True) colorText 
         (Tooltip "Gives the text all in lowercase")

txtUppercase = DesignBlock "txtUppercase" (Function "uppercase" [typeText, typeText])
        [ Value "TEXT"  [TextE "uppercase"] ]
         (Inline True) colorText 
         (Tooltip "Gives the text all in uppercase")

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

cwGray = DesignBlock "cwGray" (Function "gray" [typeNumber, typeColor])
          [Value "VALUE" [TextE "gray"] ] 
          (Inline True) colorColor 
          (Tooltip "The color gray, varying by an amount. Lower value is closer to black")

cwMixed = DesignBlock "cwMixed" (Function "mixed" [typeColor, typeColor, typeColor])
          [Dummy [TextE "mixed", icon "pot-mix.svg"] 
           ,Value "COL1"  [Text "Color"] 
           ,Value "COL2"  [Text "Color"] ] 
          inlineDef colorColor 
          (Tooltip "Gives the mix of two colors")

cwLight = DesignBlock "cwLight" (Function "light" [typeColor, typeColor])
          [Value "COL" [TextE "light"] ] 
          (Inline True) colorColor 
          (Tooltip "Makes a color lighter")

cwDark = DesignBlock "cwDark" (Function "dark" [typeColor, typeColor])
          [Value "COL" [TextE "dark"] ] 
          (Inline True) colorColor 
          (Tooltip "Makes a color darker")

cwBright = DesignBlock "cwBright" (Function "bright" [typeColor, typeColor])
          [Value "COL" [TextE "bright"] ] 
          (Inline True) colorColor 
          (Tooltip "Makes a color brighter")

cwDull = DesignBlock "cwDull" (Function "dull" [typeColor, typeColor])
          [Value "COL" [TextE "dull"] ] 
          (Inline True) colorColor 
          (Tooltip "Makes a color duller")

cwTranslucent = DesignBlock "cwTranslucent" (Function "translucent" [typeColor, typeColor])
          [Value "COL" [TextE "translucent"] ] 
          (Inline True) colorColor 
          (Tooltip "Makes a color more translucent")

cwRGBA = DesignBlock "cwRGBA" (Function "RGBA" [typeNumber, typeNumber, typeNumber, typeNumber, typeColor])
          [Dummy [TextE "RGBA"] 
           ,Value "RED"  [Text "Red"] 
           ,Value "GREEN"  [Text "Green"] 
           ,Value "BLUE"  [Text "Blue"] 
           ,Value "ALPHA"  [Text "Alpha"] ] 
          inlineDef colorColor 
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
         (Inline True) colorPoly 
         (Tooltip "Are two items equal")

conNeq = DesignBlock "conNeq" (Function "/=" [Poly "a", Poly "a", typeBool])
        [ Value "LEFT"  [] 
         ,Value "RIGHT" [TextE "\x2260"] 
         ]
         (Inline True) colorPoly 
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
           ,Value "TEXTMAIN"  [Text "Text"] 
           ,Value "TEXTTEST"  [Text "starts with"] ] 
          inlineDef colorBool 
          (Tooltip "Tells whether the given text starts with some other text")

conEndWith = DesignBlock "conEndWith" (Function "endWith" [typeText, typeText, typeBool])
          [Dummy [TextE "endsWith"] 
           ,Value "TEXTMAIN"  [Text "Text"] 
           ,Value "TEXTTEST"  [Text "ends with"] ]
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
              ,txtCapitalized
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
