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

module Blocks.Types(setBlockTypes)
  where

import Blockly.DesignBlock 
import Blockly.General
import Blockly.Event

colorPicture = Color 160
colorNumber = Color 210
colorProgram = Color 0
colorColor = Color 290
colorPoly = Color 70
colorBool = Color 100
colorText = Color 45

typePicture = Type "Picture"
typeNumber = Type "Number"
typeProgram = NoType
typeColor = Type "Color"
typeBool = Type "Bool"
typeText = Type "Text"
typeSet = NoType

inlineDef = Inline False

letVar = DesignBlock "letVar"
            [Dummy [TextInput "foo" "VARNAME"
            ,TextE "Let"]
           ,Value "VARVALUE" [Text ""] (Poly 0)]
          inlineDef colorPoly typeSet
          (Tooltip "Enter name of the variable")

-- PICTURE ----------------------------------------------
cwBlank = DesignBlock "cw_blank" 
          [Dummy 
            [TextE "Blank"]
          ]
          inlineDef colorPicture typePicture 
          (Tooltip "Blank picture")

cwCoordinatePlane = DesignBlock "cw_coordinatePlane" 
          [Dummy 
            [TextE "Coordinate Plane"]
          ]
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of coordinate plane")

cwCodeWorldLogo = DesignBlock "cw_codeWorldLogo" 
          [Dummy 
            [TextE "CodeWorld logo"]
          ]
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of CodeWorld logo")

cwText = DesignBlock "cw_text" 
          [Value "TEXT" [TextE "Text" ] typeText]
          inlineDef colorPicture typePicture 
          (Tooltip "Enter some text")

cwDrawingOf = DesignBlock "cw_drawingof"
          [Dummy [TextE "Drawing "] 
           ,Value "VALUE" [] typePicture] 
          inlineDef colorProgram typeProgram 
          (Tooltip "Displays a drawing of a picture")

cwCircle = DesignBlock "cw_circle"
          [Dummy [TextE "Circle"] 
           ,Value "RADIUS" [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture
          (Tooltip "Picture of a circle")

cwThickCircle = DesignBlock "cw_thickCircle"
          [Dummy [TextE "Thick Circle"] 
           ,Value "RADIUS" [Text "Radius"] typeNumber 
           ,Value "LINEWIDTH" [Text "Line width"] typeNumber] 
          inlineDef colorPicture typePicture
          (Tooltip "Picture of a circle")

cwSolidCircle = DesignBlock "cw_solidcircle"
          [Dummy [TextE "Solid Circle"] 
           ,Value "RADIUS"  [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture
          (Tooltip "Picture of a solid circle")

cwRectangle = DesignBlock "cw_rectangle"
          [Dummy [TextE "Rectangle"] 
           ,Value "WIDTH"  [Text "Width"] typeNumber 
           ,Value "HEIGHT"  [Text "Height"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of a rectangle")

cwThickRectangle = DesignBlock "cw_thickRectangle"
          [Dummy [TextE "Thick Rectangle"] 
           ,Value "WIDTH" [Text "Width"] typeNumber 
           ,Value "HEIGHT" [Text "Height"] typeNumber 
           ,Value "LINEWIDTH" [Text "Line Width"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of a rectangle")

cwSolidRectangle = DesignBlock "cw_solidrectangle"
          [Dummy [TextE "Solid Rectangle"] 
           ,Value "WIDTH" [Text "Width"] typeNumber 
           ,Value "HEIGHT" [Text "Height"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of a solid rectangle")

cwArc = DesignBlock "cw_arc"
          [Dummy [TextE "Arc"] 
            ,Value "STARTANGLE" [Text "Start Angle"] typeNumber 
           ,Value "ENDANGLE" [Text "End Angle"] typeNumber 
           ,Value "RADIUS" [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "A thin arc")


cwSector = DesignBlock "cw_sector"
          [Dummy [TextE "Sector"] 
            ,Value "STARTANGLE" [Text "Start Angle"] typeNumber 
           ,Value "ENDANGLE" [Text "End Angle"] typeNumber 
           ,Value "RADIUS" [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "A solid sector of a circle")

cwThickArc = DesignBlock "cw_thickArc"
          [Dummy [TextE "Thick Arc"] 
            ,Value "STARTANGLE" [Text "Start Angle"] typeNumber 
           ,Value "ENDANGLE" [Text "End Angle"] typeNumber 
           ,Value "RADIUS" [Text "Radius"] typeNumber
           ,Value "LINEWIDTH" [Text "Line width"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "A arc with variable line width")

-- Transformations -----------------------------------------------
cwColored = DesignBlock "cw_colored"
          [Dummy [TextE "Colored"] 
           ,Value "PICTURE" [Text "Picture"] typePicture 
           ,Value "COLOR" [Text "Color"] typeColor] 
          inlineDef colorPicture typePicture
          (Tooltip "Change the color of a picture")

cwTranslate = DesignBlock "cw_translate"
          [Dummy [TextE "Translated"] 
           ,Value "PICTURE" [Text "Picture"] typePicture 
           ,Value "X" [Text "x"] typeNumber
           ,Value "Y" [Text "y"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Translate a picture")

cwScale = DesignBlock "cw_scale"
          [Dummy [TextE "Scaled"] 
           ,Value "PICTURE" [Text "Picture"] typePicture 
           ,Value "HORZ" [Text "horizontal"] typeNumber
           ,Value "VERTZ" [Text "vertical"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Scale a picture")

cwRotate = DesignBlock "cw_rotate"
          [Dummy [TextE "Rotated"] 
           ,Value "PICTURE" [Text "Picture"] typePicture 
           ,Value "ANGLE" [Text "angle"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Rotate")

cwCombine = DesignBlock "cw_combine"
          [Dummy [TextE "&"] 
           ,Value "PIC1" [Text "Picture"] typePicture 
           ,Value "PIC2" [Text "Picture"] typePicture 
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Combines two pictures")

-- NUMBERS ---------------------------------------------

numNumber = DesignBlock "num_number" 
          [Dummy 
            [TextInput "3" "NUMBER"]
          ]
          inlineDef colorNumber typeNumber 
          (Tooltip "Enter a Number")

numAdd = DesignBlock "num_add"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "+"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Add two numbers")

numSub = DesignBlock "num_subtract"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "-"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Subtract two numbers")

numMult = DesignBlock "num_multiply"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "*"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Multiply two numbers")

numDiv = DesignBlock "num_div"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "/"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Divide two numbers")

numExp = DesignBlock "num_exp"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "^"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Raise a number to a power")

numMax = DesignBlock "num_max"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "Maximum"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Take the maximum of two numbers")

numMin = DesignBlock "num_min"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "Minimum"] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Take the minimum of two numbers")

numOpposite = DesignBlock "num_opposite"
        [Value "NUM" [TextE "Opposite"] typeNumber ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the negative of a number")

numAbs = DesignBlock "num_abs"
        [Value "NUM" [TextE "Absolute Value"] typeNumber ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the absolute value of a number")

numRound = DesignBlock "num_round"
        [Value "NUM" [TextE "Round"] typeNumber ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the number rounded to the nearest integer")

numReciprocal = DesignBlock "num_recip"
        [Value "NUM" [TextE "Reciprocol"] typeNumber ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the reciprocol of a number")

numQuot = DesignBlock "num_quot"
        [ Value "LEFT"  [TextE "Quotient"] typeNumber
         ,Value "RIGHT" [] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the integer part of the result when dividing two numbers")

numRem = DesignBlock "num_rem"
        [ Value "LEFT"  [TextE "Remainder"] typeNumber
         ,Value "RIGHT" [] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the remainder when dividing two numbers")

numPi = DesignBlock "num_pi" 
          [Dummy 
            [TextE "PI"]
          ]
          inlineDef colorColor typeNumber 
          (Tooltip "The number of Pi, 3.1415..")

numSqrt = DesignBlock "num_sqrt"
        [Value "NUM" [TextE "Square root of "] typeNumber ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the square root of a number")

numGCD = DesignBlock "num_gcd"
        [ Value "LEFT"  [TextE "GCD"] typeNumber
         ,Value "RIGHT" [] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the greatest common demoninator between two numbers")

numLCM = DesignBlock "num_lcm"
        [ Value "LEFT"  [TextE "LCM"] typeNumber
         ,Value "RIGHT" [] typeNumber
         ]
         (Inline True) colorNumber typeNumber
         (Tooltip "Gives the least common multiple between two numbers")

-- TEXT ------------------------------------------------

txtConcat = DesignBlock "txt_concat"
        [ Value "LEFT"  [] typeText
         ,Value "RIGHT" [TextE "<>"] typeText
         ]
         (Inline True) colorText typeText
         (Tooltip "Concatenates two pieces of text together")

txtPrinted = DesignBlock "txt_printed"
        [ Value "TEXT"  [TextE "Printed"] typeText ]
         (Inline True) colorText typeText
         (Tooltip "Gives the text value of a number")

txtLowercase = DesignBlock "txt_lowercase"
        [ Value "TEXT"  [TextE "Lowercase"] typeText ]
         (Inline True) colorText typeText
         (Tooltip "Gives the text all in lowercase")

txtUppercase = DesignBlock "txt_uppercase"
        [ Value "TEXT"  [TextE "Uppercase"] typeText ]
         (Inline True) colorText typeText
         (Tooltip "Gives the text all in uppercase")

txtCapitalized = DesignBlock "txt_capitalized"
        [ Value "TEXT"  [TextE "Capitalized"] typeText ]
         (Inline True) colorText typeText
         (Tooltip "Gives the text with the first value capitalized")

-- COLORS ----------------------------------------------
cwBlue = DesignBlock "cw_blue" 
          [Dummy 
            [TextE "Blue"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color blue")

cwRed = DesignBlock "cw_red" 
          [Dummy 
            [TextE "Red"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color red")

cwGreen = DesignBlock "cw_green" 
          [Dummy 
            [TextE "Green"]
          ]
          inlineDef colorColor typeColor
          (Tooltip "The color green")

cwOrange = DesignBlock "cw_orange" 
          [Dummy 
            [TextE "Orange"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color orange")

cwBrown = DesignBlock "cw_brown" 
          [Dummy 
            [TextE "Brown"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color brown")

cwBlack = DesignBlock "cw_black" 
          [Dummy 
            [TextE "Black"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color black")

cwWhite = DesignBlock "cw_white" 
          [Dummy 
            [TextE "White"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color white")

cwCyan = DesignBlock "cw_cyan" 
          [Dummy 
            [TextE "Cyan"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color cyan")

cwMagenta = DesignBlock "cw_magenta" 
          [Dummy 
            [TextE "Magenta"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color magenta")

cwYellow = DesignBlock "cw_yellow" 
          [Dummy 
            [TextE "Yellow"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color yellow")

cwAquamarine = DesignBlock "cw_aquamarine" 
          [Dummy 
            [TextE "Aquamarine"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color aquamarine")

cwAzure = DesignBlock "cw_azure" 
          [Dummy 
            [TextE "Azure"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color azure")

cwViolet = DesignBlock "cw_violet" 
          [Dummy 
            [TextE "Violet"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color violet")

cwChartreuse = DesignBlock "cw_chartreuse" 
          [Dummy 
            [TextE "Chartreuse"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color chartreuse")

cwRose = DesignBlock "cw_rose" 
          [Dummy 
            [TextE "Rose"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color rose")

cwPink = DesignBlock "cw_pink" 
          [Dummy 
            [TextE "Pink"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color pink")

cwPurple = DesignBlock "cw_purple" 
          [Dummy 
            [TextE "Purple"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color purple")

cwGray = DesignBlock "cw_gray"
          [Value "NUM" [TextE "Gray"] typeNumber] 
          (Inline True) colorColor typeColor
          (Tooltip "The color gray, varying by an amount")

cwMixed = DesignBlock "cw_mixed"
          [Dummy [TextE "Mixed"] 
           ,Value "COL1"  [Text "Color"] typeColor 
           ,Value "COL2"  [Text "Color"] typeColor] 
          inlineDef colorColor typeColor
          (Tooltip "Gives the mix of two colors")

cwLight = DesignBlock "cw_light"
          [Value "COL" [TextE "Light"] typeColor] 
          (Inline True) colorColor typeColor
          (Tooltip "Makes a color lighter")

cwDark = DesignBlock "cw_dark"
          [Value "COL" [TextE "Dark"] typeColor] 
          (Inline True) colorColor typeColor
          (Tooltip "Makes a color darker")

cwBright = DesignBlock "cw_bright"
          [Value "COL" [TextE "Bright"] typeColor] 
          (Inline True) colorColor typeColor
          (Tooltip "Makes a color brighter")

cwDull = DesignBlock "cw_dull"
          [Value "COL" [TextE "Dull"] typeColor] 
          (Inline True) colorColor typeColor
          (Tooltip "Makes a color duller")

cwTranslucent = DesignBlock "cw_translucent"
          [Value "COL" [TextE "Translucent"] typeColor] 
          (Inline True) colorColor typeColor
          (Tooltip "Makes a color more translucent")

cwRGBA = DesignBlock "cw_rgba"
          [Dummy [TextE "RGBA"] 
           ,Value "WIDTH"  [Text "Red"] typeNumber 
           ,Value "WIDTH"  [Text "Blue"] typeNumber 
           ,Value "WIDTH"  [Text "Green"] typeNumber 
           ,Value "ALPHA"  [Text "Alpha"] typeNumber] 
          inlineDef colorColor typeColor 
          (Tooltip "Makes a color with the given red, blue, green and alpha values")

-- LOGIC -------------------------------------------
conIf = DesignBlock "con_if"
        [ Value "IF"  [TextE "If"] typeBool
         ,Value "THEN" [Text "Then"] (Poly 0)
         ,Value "ELSE" [Text "Else"] (Poly 0)
         ]
         inlineDef colorPoly (Poly 0)
         (Tooltip "if condition is true then give a else b")

conAnd = DesignBlock "con_and"
        [ Value "LEFT"  [] typeBool
         ,Value "RIGHT" [TextE "&&"] typeBool
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Logical AND operation")

conOr = DesignBlock "con_or"
        [ Value "LEFT"  [] typeBool
         ,Value "RIGHT" [TextE "||"] typeBool
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Logical OR operation")

conNot = DesignBlock "con_not"
        [ Value "VALUE"  [TextE "Not"] typeBool ]
         (Inline True) colorBool typeBool
         (Tooltip "Negation of the logical value")

conEq = DesignBlock "con_eq"
        [ Value "LEFT"  [] (Poly 0)
         ,Value "RIGHT" [TextE "=="] (Poly 0)
         ]
         (Inline True) colorPoly typeBool
         (Tooltip "Are two items equal")

conNeq = DesignBlock "con_neq"
        [ Value "LEFT"  [] (Poly 0)
         ,Value "RIGHT" [TextE "/="] (Poly 0)
         ]
         (Inline True) colorPoly typeBool
         (Tooltip "Are two items not equal")

conTrue = DesignBlock "con_true" 
          [Dummy 
            [TextE "True"]
          ]
          inlineDef colorBool typeBool
          (Tooltip "True logic value")

conFalse = DesignBlock "con_false" 
          [Dummy 
            [TextE "False"]
          ]
          inlineDef colorBool typeBool
          (Tooltip "False logic value")

conGreater = DesignBlock "con_greater"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE ">"] typeNumber
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether one number is greater than the other")

conGeq = DesignBlock "con_geq"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE ">="] typeNumber
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether one number is greater than or equal to ther other")

conLess = DesignBlock "con_less"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "<"] typeNumber
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether one number is less than the other")

conLeq = DesignBlock "con_leq"
        [ Value "LEFT"  [] typeNumber
         ,Value "RIGHT" [TextE "<="] typeNumber
         ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether one number is less than or equal to ther other")

conEven = DesignBlock "con_even"
        [ Value "VALUE"  [TextE "Is Even"] typeNumber ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether the number is even")

conOdd = DesignBlock "con_odd"
        [ Value "VALUE"  [TextE "Is Odd"] typeNumber ]
         (Inline True) colorBool typeBool
         (Tooltip "Tells whether the number is odd")

conStartWith = DesignBlock "con_startwith"
          [Dummy [TextE "Starts With"] 
           ,Value "TEXTMAIN"  [Text "Text"] typeText 
           ,Value "TEXTTEST"  [Text "starts with"] typeText] 
          inlineDef colorBool typeBool 
          (Tooltip "Tells whether the given text starts with some other text")

conEndWith = DesignBlock "con_endwith"
          [Dummy [TextE "Ends With"] 
           ,Value "TEXTMAIN"  [Text "Text"] typeText 
           ,Value "TEXTTEST"  [Text "starts with"] typeText] 
          inlineDef colorBool typeBool 
          (Tooltip "Tells whether the given text ends with some other text")


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
              ,cwCombine
              ,cwRotate
              ,cwScale
              -- NUMBERS
              ,numNumber
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
              ,txtConcat
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
              ,letVar
                ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
setBlockTypes :: IO ()
setBlockTypes = mapM_ setBlockType blockTypes


