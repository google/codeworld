{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Internal.Event where

import "base" Prelude
import Internal.Num
import Internal.Text
import Internal.Picture

data Event = KeyPress Text
           | KeyRelease Text
           | MousePress Number Point
           | MouseRelease Number Point
           | MouseMovement Point

instance Show Event where show _ = "<<Event>>"
