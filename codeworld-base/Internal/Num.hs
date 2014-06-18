{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

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
    quot,
    rem,
    div,
    mod,
    quotRem,
    divMod,
    recip,
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
    subtract,
    even,
    odd,
    gcd,
    lcm,
    succ,
    pred,
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

newtype Number = Number P.Double

fromDouble :: P.Double -> Number
fromDouble x | P.isNaN x      = P.error "result is undefined"
             | P.isInfinite x = P.error "result is too large"
             | otherwise      = Number x

toDouble :: Number -> P.Double
toDouble (Number x) = x

isInteger :: Number -> Bool
isInteger (Number x) = x == P.fromIntegral (P.truncate x)

instance P.Show Number where
    showsPrec p x | isInteger x = P.showsPrec p (P.truncate (toDouble x))
                  | otherwise   = P.showsPrec p (toDouble x)

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
    succ = succ
    pred = pred
    toEnum = fromDouble . P.toEnum
    fromEnum = P.fromEnum . toDouble
    enumFrom = map fromDouble . P.enumFrom . toDouble
    enumFromThen (Number a) (Number b) = map fromDouble (P.enumFromThen a b)
    enumFromTo (Number a) (Number b) = map fromDouble (P.enumFromTo a b)
    enumFromThenTo (Number a) (Number b) (Number c) = map fromDouble (P.enumFromThenTo a b c)

instance P.Ord Number where
    compare (Number a) (Number b) = P.compare a b

infixr 8  ^
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infix  4  <, <=, >=, >

(+) :: Number -> Number -> Number
Number a + Number b = fromDouble (a P.+ b)

(-) :: Number -> Number -> Number
Number a - Number b = fromDouble (a P.- b)

(*) :: Number -> Number -> Number
Number a * Number b = fromDouble (a P.* b)

(/) :: Number -> Number -> Number
Number a / Number b = fromDouble (a P./ b)

(^) :: Number -> Number -> Number
Number a ^ Number b = fromDouble (a P.** b)

(<) :: Number -> Number -> Bool
Number a < Number b = a P.< b

(<=) :: Number -> Number -> Bool
Number a <= Number b = a P.<= b

(>) :: Number -> Number -> Bool
Number a > Number b = a P.> b

(>=) :: Number -> Number -> Bool
Number a >= Number b = a P.>= b

max :: Number -> Number -> Number
max (Number a) (Number b) = fromDouble (P.max a b)

min :: Number -> Number -> Number
min (Number a) (Number b) = fromDouble (P.min a b)

negate :: Number -> Number
negate = fromDouble . P.negate . toDouble

abs :: Number -> Number
abs = fromDouble . P.abs . toDouble

signum :: Number -> Number
signum = fromDouble . P.signum . toDouble

truncate :: Number -> Number
truncate = fromInteger . P.truncate . toDouble

round :: Number -> Number
round = fromInteger . P.round . toDouble

ceiling :: Number -> Number
ceiling = fromInteger . P.ceiling . toDouble

floor :: Number -> Number
floor = fromInteger . P.floor . toDouble

quot :: Number -> Number -> Number
quot a b = truncate (a / b)

rem :: Number -> Number -> Number
rem a b = a - b * truncate (a / b)

div :: Number -> Number -> Number
div a b = floor (a / b)

mod :: Number -> Number -> Number
mod a b = a - b * floor (a / b)

quotRem :: Number -> Number -> (Number, Number)
quotRem a b = let q = a `quot` b in (q, a - b * q)

divMod :: Number -> Number -> (Number, Number)
divMod a b = let q = a `div` b in (q, a - b * q)

recip :: Number -> Number
recip = fromDouble . P.recip . toDouble

pi :: Number
pi = fromDouble P.pi

exp :: Number -> Number
exp = fromDouble . P.exp . toDouble

sqrt :: Number -> Number
sqrt = fromDouble . P.sqrt . toDouble

log :: Number -> Number
log = fromDouble . P.log . toDouble

logBase :: Number -> Number -> Number
logBase (Number b) (Number x) = fromDouble (P.logBase b x)

toRadians :: Number -> Number
toRadians d = d / 180 * pi

sin :: Number -> Number
sin = fromDouble . P.sin  . toDouble . toRadians

tan :: Number -> Number
tan = fromDouble . P.tan . toDouble . toRadians

cos :: Number -> Number
cos = fromDouble . P.cos . toDouble . toRadians

fromRadians :: Number -> Number
fromRadians r = r / pi * 180

asin :: Number -> Number
asin = fromRadians . fromDouble . P.asin . toDouble

atan :: Number -> Number
atan = fromRadians . fromDouble . P.atan . toDouble

atan2 :: Number -> Number -> Number
atan2 (Number a) (Number b) = fromRadians (fromDouble (P.atan2 a b))

acos :: Number -> Number
acos = fromRadians . fromDouble . P.acos . toDouble

properFraction :: Number -> (Number, Number)
properFraction (Number x) = (fromInteger w, fromDouble p)
    where (w,p) = P.properFraction x

subtract :: Number -> Number -> Number
subtract a b = a - b

even :: Number -> Bool
even n | isInteger n = P.even (toInt n)
       | otherwise   = False

odd :: Number -> Bool
odd n | isInteger n = P.odd (toInt n)
      | otherwise   = False

gcd :: Number -> Number -> Number
gcd a b
    | isInteger a && isInteger b = fromInteger (P.gcd ia ib)
    | otherwise                  = P.error "gcd requires whole numbers"
    where ia = P.truncate (toDouble a)
          ib = P.truncate (toDouble b)

lcm :: Number -> Number -> Number
lcm a b
    | isInteger a && isInteger b = fromInteger (P.lcm ia ib)
    | otherwise                  = P.error "lcm requires whole numbers"
    where ia = P.truncate (toDouble a)
          ib = P.truncate (toDouble b)

succ :: Number -> Number
succ x = x + 1

pred :: Number -> Number
pred x = x - 1

sum :: [Number] -> Number
sum = fromDouble . P.sum . P.map toDouble

product :: [Number] -> Number
product = fromDouble . P.product . P.map toDouble

maximum :: [Number] -> Number
maximum = fromDouble . P.maximum . P.map toDouble

minimum :: [Number] -> Number
minimum = fromDouble . P.minimum . P.map toDouble

fromInteger :: P.Integer -> Number
fromInteger = fromDouble . P.fromInteger

fromRational :: P.Rational -> Number
fromRational = fromDouble . P.fromRational

fromInt :: P.Int -> Number
fromInt = fromDouble . P.fromIntegral

toInt :: Number -> P.Int
toInt n | isInteger n = P.truncate (toDouble n)
        | otherwise   = P.error "a whole number is required"
