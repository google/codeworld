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
colorPoly = Color 50
colorBool = Color 100

typePicture = Type "Picture"
typeNumber = Type "Number"
typeProgram = NoType
typeColor = Type "Color"
typeBool = Type "bool"
typeSet = NoType

inlineDef = Inline False

letVar = DesignBlock "letVar"
            [Dummy [TextInput "foo" "VARNAME"
            ,Text "Let"]
           ,Value "VARVALUE" LeftField [Text ""] (Poly 0)]
          inlineDef colorPoly typeSet
          (Tooltip "Enter name of the variable")

number = DesignBlock "number" 
          [Dummy 
            [TextInput "3" "NUMBER"]
          ]
          inlineDef colorNumber typeNumber 
          (Tooltip "Enter a Number")

cwText = DesignBlock "cw_text" 
          [Dummy 
            [TextInput "Hello Panda" "TEXT"
            ,Text "Text"]
          ]
          inlineDef colorPicture typePicture 
          (Tooltip "Enter some text")

cwDrawingOf = DesignBlock "cw_drawingof"
          [Dummy [Text "Drawing "] 
           ,Value "VALUE" LeftField [] typePicture] 
          inlineDef colorProgram typeProgram 
          (Tooltip "Displays a drawing of a picture")

cwCircle = DesignBlock "cw_circle"
          [Dummy [Text "Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture
          (Tooltip "Picture of a circle")

cwSolidCircle = DesignBlock "cw_solidcircle"
          [Dummy [Text "Solid Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] typeNumber] 
          inlineDef colorPicture typePicture
          (Tooltip "Picture of a solid circle")

cwSolidRectangle = DesignBlock "cw_solidrectangle"
          [Dummy [Text "Solid Rectangle"] 
           ,Value "WIDTH" LeftField [Text "Width"] typeNumber 
           ,Value "HEIGHT" LeftField [Text "Height"] typeNumber] 
          inlineDef colorPicture typePicture 
          (Tooltip "Picture of a solid rectangle")

cwBlue = DesignBlock "cw_blue" 
          [Dummy 
            [Text "Blue"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color blue")

cwRed = DesignBlock "cw_red" 
          [Dummy 
            [Text "Red"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color red")

cwGreen = DesignBlock "cw_green" 
          [Dummy 
            [Text "Green"]
          ]
          inlineDef colorColor typeColor
          (Tooltip "The color green")

cwOrange = DesignBlock "cw_orange" 
          [Dummy 
            [Text "Orange"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color orange")

cwBrown = DesignBlock "cw_brown" 
          [Dummy 
            [Text "Brown"]
          ]
          inlineDef colorColor typeColor 
          (Tooltip "The color brown")

cwColored = DesignBlock "cw_colored"
          [Dummy [Text "colored"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "COLOR" LeftField [Text "Color"] typeColor] 
          inlineDef colorPicture typePicture
          (Tooltip "Change the color of a picture")

cwTranslate = DesignBlock "cw_translate"
          [Dummy [Text "translated"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "X" LeftField [Text "x"] typeNumber
           ,Value "Y" LeftField [Text "y"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Translate a picture")

cwScale = DesignBlock "cw_scale"
          [Dummy [Text "scaled"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "HORZ" LeftField [Text "horizontal"] typeNumber
           ,Value "VERTZ" LeftField [Text "vertical"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Scale a picture")

cwRotate = DesignBlock "cw_rotate"
          [Dummy [Text "rotated"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "ANGLE" LeftField [Text "angle"] typeNumber
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Rotate")

cwCombine = DesignBlock "cw_combine"
          [Dummy [Text "combined"] 
           ,Value "PIC1" LeftField [Text "Picture"] typePicture 
           ,Value "PIC2" LeftField [Text "Picture"] typePicture 
          ] 
          inlineDef colorPicture typePicture 
          (Tooltip "Combines two pictures")


conIf = DesignBlock "con_if"
        [ Value "IF" LeftField [Text "if"] typeBool
         ,Value "THEN" LeftField [Text "then"] (Poly 0)
         ,Value "ELSE" LeftField [Text "else"] (Poly 0)
         ]
         inlineDef colorPoly (Poly 0)
         (Tooltip "if condition is true then give a else b")

conEq = DesignBlock "con_eq"
        [ Value "LEFT" LeftField [] (Poly 0)
         ,Value "RIGHT" LeftField [Text "=="] (Poly 0)
         ]
         (Inline True) colorPoly typeBool
         (Tooltip "Are two items equal")

conTrue = DesignBlock "con_true" 
          [Dummy 
            [Text "true"]
          ]
          inlineDef colorBool typeBool
          (Tooltip "True logic value")

conFalse = DesignBlock "con_false" 
          [Dummy 
            [Text "false"]
          ]
          inlineDef colorBool typeBool
          (Tooltip "False logic value")



blockTypes = [ cwCircle
              ,number 
              ,cwSolidCircle
              ,cwSolidRectangle
              ,cwText
              ,cwBlue
              ,cwRed
              ,cwGreen
              ,cwBrown
              ,cwOrange
              ,cwColored
              ,cwTranslate
              ,cwCombine
              ,cwDrawingOf
              ,letVar
              ,conIf
              ,conEq
              ,conTrue
              ,conFalse
              ,cwRotate
              ,cwScale
                ]

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
setBlockTypes :: IO ()
setBlockTypes = mapM_ setBlockType blockTypes


