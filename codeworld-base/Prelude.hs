{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Prelude (
    module Internal.Num,
    module Internal.Text,
    module Internal.Prelude,

    -- Colors
    Color(..),
    black,
    white,
    red,
    green,
    blue,
    cyan,
    magenta,
    yellow,
    aquamarine,
    orange,
    azure,
    violet,
    chartreuse,
    rose,
    brown,
    pink,
    purple,
    gray,
    grey,
    light,
    dark,
    muted,
    translucent,

    -- Pictures
    Point,
    Vector,
    Picture,
    blank,
    line,
    polygon,
    thickLine,
    rectangle,
    solidRectangle,
    thickRectangle,
    circle,
    solidCircle,
    thickCircle,
    arc,
    sector,
    thickArc,
    text,
    color,
    translate,
    scale,
    rotate,
    pictures,
    (&),

    -- Events
    Event(..),

    -- Entry points
    IO,
    Program,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf
    ) where

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.Color
import Internal.Picture
import Internal.Event
import Internal.CodeWorld
