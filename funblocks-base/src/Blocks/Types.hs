module Blocks.Types(setBlockTypes)
  where

import Blockly.DesignBlock 

number = DesignBlock "number" 
          [Dummy 
            [TextInput "3" "NUMBER"]
          ]
          LeftCon 210 (Type "Number") "Enter a Number"

cwText = DesignBlock "cw_text" 
          [Dummy 
            [TextInput "Hello Panda" "TEXT"
            ,Text "Text"]
          ]
          LeftCon 160 (Type "Picture") "Enter some text"

cwDrawingOf = DesignBlock "cw_drawingof"
          [Dummy [Text "Drawing of"] 
           ,Value "VALUE" LeftField [] (Type "Picture")] 
          LeftCon 00 NoType 
          "Displays a drawing of a picture"

cwCircle = DesignBlock "cw_circle"
          [Dummy [Text "Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] (Type "Number")] 
          LeftCon 160 (Type "Picture") 
          "Picture of a circle"

cwSolidCircle = DesignBlock "cw_solidcircle"
          [Dummy [Text "Solid Circle"] 
           ,Value "RADIUS" LeftField [Text "Radius"] (Type "Number")] 
          LeftCon 160 (Type "Picture") 
          "Picture of a solid circle"

cwSolidRectangle = DesignBlock "cw_solidrectangle"
          [Dummy [Text "Solid Rectangle"] 
           ,Value "WIDTH" LeftField [Text "Width"] (Type "Number") 
           ,Value "HEIGHT" LeftField [Text "Height"] (Type "Number")] 
          LeftCon 160 (Type "Picture") 
          "Picture of a solid rectangle"

cwBlue = DesignBlock "cw_blue" 
          [Dummy 
            [Text "Blue"]
          ]
          LeftCon 290 (Type "Color") "The color blue"

cwRed = DesignBlock "cw_red" 
          [Dummy 
            [Text "Red"]
          ]
          LeftCon 290 (Type "Color") "The color red"

cwGreen = DesignBlock "cw_green" 
          [Dummy 
            [Text "Green"]
          ]
          LeftCon 290 (Type "Color") "The color green"

cwBrown = DesignBlock "cw_brown" 
          [Dummy 
            [Text "Brown"]
          ]
          LeftCon 290 (Type "Color") "The color brown"

cwColored = DesignBlock "cw_colored"
          [Dummy [Text "Change color"] 
           ,Value "PICTURE" LeftField [Text "Picture"] (Type "Picture") 
           ,Value "COLOR" LeftField [Text "Color"] (Type "Color")] 
          LeftCon 160 (Type "Picture") 
          "Change the color of a picture"

cwTranslate = DesignBlock "cw_translate"
          [Dummy [Text "Translate"] 
           ,Value "PICTURE" LeftField [Text "Picture"] (Type "Picture") 
           ,Value "X" LeftField [Text "x"] (Type "Number")
           ,Value "Y" LeftField [Text "y"] (Type "Number")
          ] 
          LeftCon 160 (Type "Picture") 
          "Translate a picture"

cwCombine = DesignBlock "cw_combine"
          [Dummy [Text "Combine pictures"] 
           ,Value "PIC1" LeftField [Text "Picture"] (Type "Picture") 
           ,Value "PIC2" LeftField [Text "Picture"] (Type "Picture") 
          ] 
          LeftCon 160 (Type "Picture") 
          "Combines two pictures"



blockTypes = [ cwCircle
              ,number 
              ,cwSolidCircle
              ,cwSolidRectangle
              ,cwText
              ,cwBlue
              ,cwRed
              ,cwGreen
              ,cwBrown
              ,cwColored
              ,cwTranslate
              ,cwCombine
              ,cwDrawingOf
                ]




-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
setBlockTypes :: IO ()
setBlockTypes = mapM_ setBlockType blockTypes


