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

colorPicture = Color 160
colorNumber = Color 210
colorProgram = Color 0
colorColor = Color 290
colorPoly = Color 50

typePicture = Type "Picture"
typeNumber = Type "Number"
typeProgram = NoType
typeColor = Type "Color"
typeSet = Type ""

letVar = DesignBlock "letVar"
            [Dummy [TextInput "foo" "VARNAME"
            ,Text "Let"]
           ,Value "VARVALUE" LeftField [Text ""] typeSet]
          
          LeftCon colorPoly typeColor
          (Tooltip "Enter name of the variable")

number = DesignBlock "number" 
          [Dummy 
            [TextInput "3" "NUMBER"]
          ]
          LeftCon colorNumber typeNumber 
          (Tooltip "Enter a Number")

cwText = DesignBlock "cw_text" 
          [Dummy 
            [TextInput "Hello Panda" "TEXT"
            ,Text "Text"]
          ]
          LeftCon colorPicture typePicture 
          (Tooltip "Enter some text")

cwDrawingOf = DesignBlock "cw_drawingof"
          [Dummy [Text "Drawing of"] 
           ,Value "VALUE" LeftField [] typePicture] 
          LeftCon colorProgram typeProgram 
          (Tooltip "Displays a drawing of a picture")

cwCircle = DesignBlock "cw_circle"
          [Dummy [Text "Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] typeNumber] 
          LeftCon colorPicture typePicture
          (Tooltip "Picture of a circle")

cwSolidCircle = DesignBlock "cw_solidcircle"
          [Dummy [Text "Solid Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] typeNumber] 
          LeftCon colorPicture typePicture
          (Tooltip "Picture of a solid circle")

cwSolidRectangle = DesignBlock "cw_solidrectangle"
          [Dummy [Text "Solid Rectangle"] 
           ,Value "WIDTH" LeftField [Text "Width"] typeNumber 
           ,Value "HEIGHT" LeftField [Text "Height"] typeNumber] 
          LeftCon colorPicture typePicture 
          (Tooltip "Picture of a solid rectangle")

cwBlue = DesignBlock "cw_blue" 
          [Dummy 
            [Text "Blue"]
          ]
          LeftCon colorColor typeColor 
          (Tooltip "The color blue")

cwRed = DesignBlock "cw_red" 
          [Dummy 
            [Text "Red"]
          ]
          LeftCon colorColor typeColor 
          (Tooltip "The color red")

cwGreen = DesignBlock "cw_green" 
          [Dummy 
            [Text "Green"]
          ]
          LeftCon colorColor typeColor
          (Tooltip "The color green")

cwOrange = DesignBlock "cw_orange" 
          [Dummy 
            [Text "Orange"]
          ]
          LeftCon colorColor typeColor 
          (Tooltip "The color orange")

cwBrown = DesignBlock "cw_brown" 
          [Dummy 
            [Text "Brown"]
          ]
          LeftCon colorColor typeColor 
          (Tooltip "The color brown")

cwColored = DesignBlock "cw_colored"
          [Dummy [Text "Change color"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "COLOR" LeftField [Text "Color"] typeColor] 
          LeftCon colorPicture typePicture
          (Tooltip "Change the color of a picture")

cwTranslate = DesignBlock "cw_translate"
          [Dummy [Text "Translate"] 
           ,Value "PICTURE" LeftField [Text "Picture"] typePicture 
           ,Value "X" LeftField [Text "x"] typeNumber
           ,Value "Y" LeftField [Text "y"] typeNumber
          ] 
          LeftCon colorPicture typePicture 
          (Tooltip "Translate a picture")

cwCombine = DesignBlock "cw_combine"
          [Dummy [Text "Combine pictures"] 
           ,Value "PIC1" LeftField [Text "Picture"] typePicture 
           ,Value "PIC2" LeftField [Text "Picture"] typePicture 
          ] 
          LeftCon colorPicture typePicture 
          (Tooltip "Combines two pictures")



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
                ]




-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
setBlockTypes :: IO ()
setBlockTypes = mapM_ setBlockType blockTypes


