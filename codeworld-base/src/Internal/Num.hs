{-# LANGUAGE CPP                        #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

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

module Internal.Num (
    Number,
    (+),
    (-),
    (*),
    (/),
    (^),
    (>),
    (>=),
    (<),
    (<=),
    max,
    min,
    negate,
    abs,
    signum,
    truncate,
    round,
    ceiling,
    floor,
    quotient,
    remainder,
    reciprocal,
    pi,
    exp,
    sqrt,
    log,
    logBase,
    sin,
    tan,
    cos,
    asin,
    atan,
    atan2,
    acos,
    properFraction,
    even,
    odd,
    gcd,
    lcm,
    sum,
    product,
    maximum,
    minimum,
    isInteger,
    fromInteger,
    fromRational,
    fromInt,
    toInt,
    fromDouble,
    toDouble
    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool(..), (.), (==), (&&), map, not, otherwise)
import Numeric

{-|The type for numbers.

  Numbers can be positive or negative, whole or fractional.  For example, 5,
  3.2, and -10 are all values of the type Number.
-}
newtype Number = Number P.Double
    deriving (P.RealFrac, P.Real, P.Floating, P.RealFloat)

fromDouble :: P.Double -> Number
fromDouble x | P.isNaN x      = P.error "result is undefined"
             | P.isInfinite x = P.error "result is too large"
             | otherwise      = Number x

toDouble :: Number -> P.Double
toDouble (Number x) = x

fromInteger :: P.Integer -> Number
fromInteger = fromDouble . P.fromInteger

fromRational :: P.Rational -> Number
fromRational = fromDouble . P.fromRational

fromInt :: P.Int -> Number
fromInt = fromDouble . P.fromIntegral

toInt :: Number -> P.Int
toInt n | isInteger n = P.truncate (toDouble n)
        | otherwise   = P.error "a whole number is required"

instance P.Show Number where
    showsPrec p x | isInteger x = P.showsPrec p (P.truncate (toDouble x))
                  | otherwise   = showFFloat P.Nothing (toDouble x)

instance P.Eq Number where
    Number a == Number b = a == b

instance P.Num Number where
    fromInteger = fromInteger
    (+) = (+)
    (-) = (-)
    (*) = (*)
    negate = negate
    abs = abs
    signum = signum

instance P.Fractional Number where
    fromRational x = Number (P.fromRational x)
    (/) = (/)

instance P.Enum Number where
    succ = fromDouble . P.succ . toDouble
    pred = fromDouble . P.pred . toDouble
    toEnum = fromDouble . P.toEnum
    fromEnum = P.fromEnum . toDouble
    enumFrom = map fromDouble . P.enumFrom . toDouble
    enumFromThen (Number a) (Number b) = map fromDouble (P.enumFromThen a b)
    enumFromTo (Number a) (Number b) = map fromDouble (P.enumFromTo a b)
    enumFromThenTo (Number a) (Number b) (Number c) = map fromDouble (P.enumFromThenTo a b c)

instance P.Ord Number where
    compare (Number a) (Number b) = P.compare a b

{-| Tells whether a Number is an integer or not.

  An integer is a whole number, such as 5, 0, or -10.  Numbers with non-zero
  decimals, like 5.3, are not integers.
-}
isInteger :: Number -> Bool
isInteger (Number x) = x == P.fromIntegral (P.truncate x)

infixr 8  ^
infixl 7  *, /
infixl 6  +, -
infix  4  <, <=, >=, >

{-| Adds two numbers. -}
(+) :: Number -> Number -> Number
Number a + Number b = fromDouble (a P.+ b)

{-| Subtracts two numbers. -}
(-) :: Number -> Number -> Number
Number a - Number b = fromDouble (a P.- b)

{-| Multiplies two numbers. -}
(*) :: Number -> Number -> Number
Number a * Number b = fromDouble (a P.* b)

{-| Divides two numbers.  The second number should not be zero. -}
(/) :: Number -> Number -> Number
Number a / Number b = fromDouble (a P./ b)

{-| Raises a number to a power. -}
(^) :: Number -> Number -> Number
Number a ^ Number b = fromDouble (a P.** b)

{-| Tells whether one number is less than the other. -}
(<) :: Number -> Number -> Bool
Number a < Number b = a P.< b

{-| Tells whether one number is less than or equal to the other. -}
(<=) :: Number -> Number -> Bool
Number a <= Number b = a P.<= b

{-| Tells whether one number is greater than the other. -}
(>) :: Number -> Number -> Bool
Number a > Number b = a P.> b

{-| Tells whether one number is greater than or equal to the other. -}
(>=) :: Number -> Number -> Bool
Number a >= Number b = a P.>= b

{-| Gives the larger of two numbers. -}
max :: (Number, Number) -> Number
max (Number a, Number b) = fromDouble (P.max a b)

{-| Gives the smaller of two numbers. -}
min :: (Number, Number) -> Number
min (Number a, Number b) = fromDouble (P.min a b)

{-| Gives the opposite (that is, the negative) of a number. -}
negate :: Number -> Number
negate = fromDouble . P.negate . toDouble

{-| Gives the absolute value of a number.

  If the number if positive or zero, the absolute value is the same as the
  number.  If the number is negative, the absolute value is the opposite of
  the number.
-}
abs :: Number -> Number
abs = fromDouble . P.abs . toDouble

{-| Gives the sign of a number.

  If the number is negative, the signum is -1.  If it's positive, the signum
  is 1.  If the number is 0, the signum is 0.  In general, a number is equal
  to its absolute value ('abs') times its sign ('signum').
-}
signum :: Number -> Number
signum = fromDouble . P.signum . toDouble

{-| Gives the number without its fractional part.

  For example, truncate(4.2) is 4, while truncate(-4.7) is -4.
-}
truncate :: Number -> Number
truncate = fromInteger . P.truncate . toDouble

{-| Gives the number rounded to the nearest integer.

  For example, round(4.2) is 4, while round(4.7) is 5.
-}
round :: Number -> Number
round = fromInteger . P.round . toDouble

{-| Gives the smallest integer that is greater than or equal to a number.

  For example, ceiling(4) is 4, while ceiling(4.1) is 5.  With negative
  numbers, ceiling(-3.5) is -3, since -3 is greater than -3.5.
-}
ceiling :: Number -> Number
ceiling = fromInteger . P.ceiling . toDouble

{-| Gives the largest integer that is less than or equal to a number.

  For example, floor(4) is 4, while floor(3.9) is 3.  With negative
  numbers, floor(-3.5) is -4, since -4 is less than -3.5.
-}
floor :: Number -> Number
floor = fromInteger . P.floor . toDouble

{-| Gives the integer part of the result when dividing two numbers.

  For example, 3/2 is 1.5, but quotient(3, 2) is 1, which is the integer
  part.
-}
quotient :: (Number, Number) -> Number
quotient (a, b) = truncate (a / b)

{-| Gives the remainder when dividing two numbers.

  For example, remainder(3,2) is 1, which is the remainder when dividing
  3 by 2.
-}
remainder :: (Number, Number) -> Number
remainder (a, b) = a - b * truncate (a / b)

{-| Gives the repicrocal of a number.

  For example, reciprocal(5) is 1/5 (also written as 0.2).
-}
reciprocal :: Number -> Number
reciprocal = fromDouble . P.recip . toDouble

{-| The constant pi, which is equal to the ration between the circumference
    and diameter of a circle.

  pi is approximately 3.14159.
-}
pi :: Number
pi = fromDouble P.pi

{-| Gives the exponential of a number.  This is equal to the constant e,
    raised to the power of the number.

  The exp function increases faster and faster very quickly.  For example,
  if t is the current time in seconds, exp(t) will reach a million in about
  14 seconds.  It will reach a billion in around 21 seconds.
-}
exp :: Number -> Number
exp = fromDouble . P.exp . toDouble

{-| Gives the square root of a number.  This is the positive number that, when
    multiplied by itself, gives the original number back.

  The sqrt always increases, but slows down.  For example, if t is the
  current time, sqrt(t) will reach 5 in 25 seconds.  But it will take 100
  seconds to reach 10, and 225 seconds (almost 4 minutes) to reach 15.
-}
sqrt :: Number -> Number
sqrt = fromDouble . P.sqrt . toDouble

{-| Gives the natural log of a number.  This is the opposite of the exp
    function.

  Like sqrt, the log function always increases, but slows down.  However,
  it slows down much sooner than the sqrt function.  If t is the current time
  in seconds, it takes more than 2 minutes for log(t) to reach 5, and more
  than 6 hours to reach 10!
-}
log :: Number -> Number
log = fromDouble . P.log . toDouble

{-| Gives the logarithm of the first number, using the base of the second
    number.
-}
logBase :: (Number, Number) -> Number
logBase (Number x, Number b) = fromDouble (P.logBase b x)

{-| Converts an angle from degrees to radians. -}
toRadians :: Number -> Number
toRadians d = d / 180 * pi

{-| Converts an angle from radians to degrees. -}
fromRadians :: Number -> Number
fromRadians r = r / pi * 180

{-| Gives the sine of an angle, where the angle is measured in degrees. -}
sin :: Number -> Number
sin = fromDouble . P.sin  . toDouble . toRadians

{-| Gives the tangent of an angle, where the angle is measured in degrees.

  This is the slope of a line at that angle from horizontal.
-}
tan :: Number -> Number
tan = fromDouble . P.tan . toDouble . toRadians

{-| Gives the cosine of an angle, where the angle is measured in degrees. -}
cos :: Number -> Number
cos = fromDouble . P.cos . toDouble . toRadians

{-| Gives the inverse sine of a value, in degrees.

  This is the unique angle between -90 and 90 that has the input as its sine.
-}
asin :: Number -> Number
asin = fromRadians . fromDouble . P.asin . toDouble

{-| Gives the inverse tangent of a value, in degrees.

  This is the unique angle between -90 and 90 that has the input as its tangent.
-}
atan :: Number -> Number
atan = fromRadians . fromDouble . P.atan . toDouble

{-| Gives the angle between the positive x axis and a given point, in degrees. -}
atan2 :: (Number, Number) -> Number
atan2 (Number a, Number b) = fromRadians (fromDouble (P.atan2 a b))

{-| Gives the inverse cosine of a value, in degrees.

  This is the unique angle between 0 and 180 that has the input as its cosine.
-}
acos :: Number -> Number
acos = fromRadians . fromDouble . P.acos . toDouble

{-| Separates a number into its whole and fractional parts.

  For example, properFraction(1.2) is (1, 0.2).
-}
properFraction :: Number -> (Number, Number)
properFraction (Number x) = (fromInteger w, fromDouble p)
    where (w,p) = P.properFraction x

{-| Tells if a number is even. -}
even :: Number -> Bool
even n | isInteger n = P.even (toInt n)
       | otherwise   = False

{-| Tells if a number is odd. -}
odd :: Number -> Bool
odd n | isInteger n = P.odd (toInt n)
      | otherwise   = False

{-| Gives the greatest common divisor of two numbers.

  This is the largest number that divides each of the two parameters.
  Both parameters must be integers.
-}

gcd :: (Number, Number) -> Number
gcd (a, b)
    | isInteger a && isInteger b = fromInteger (P.gcd ia ib)
    | otherwise                  = P.error "gcd requires whole numbers"
    where ia = P.truncate (toDouble a)
          ib = P.truncate (toDouble b)

{-| Gives the least common multiple of two numbers.

  This is the smallest number that is divisible by both of the two
  parameters.  Both parameters must be integers.
-}
lcm :: (Number, Number) -> Number
lcm (a, b)
    | isInteger a && isInteger b = fromInteger (P.lcm ia ib)
    | otherwise                  = P.error "lcm requires whole numbers"
    where ia = P.truncate (toDouble a)
          ib = P.truncate (toDouble b)

{-| Gives the sum of a list of numbers. -}
sum :: [Number] -> Number
sum = fromDouble . P.sum . P.map toDouble

{-| Gives the product of a list of numbers. -}
product :: [Number] -> Number
product = fromDouble . P.product . P.map toDouble

{-| Gives the largest number from a list. -}
maximum :: [Number] -> Number
maximum = fromDouble . P.maximum . P.map toDouble

{-| Gives the smallest number from a list. -}
minimum :: [Number] -> Number
minimum = fromDouble . P.minimum . P.map toDouble
