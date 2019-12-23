{-# LANGUAGE BangPatterns #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
module CodeWorld.DrawState where

import CodeWorld.Color

data DrawState
  = -- | A 'Color', if already chosen.
    DrawState
      !AffineTransformation
      !(Maybe Color)

-- | @(AffineTransformation a b c d e f)@ represents an affine transformation matrix
--
-- > a c e
-- > b d f
-- > 0 0 1
--
-- References:
-- https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/transform
-- https://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations
data AffineTransformation
  = AffineTransformation !Double !Double !Double !Double !Double !Double

initialAffineTransformation :: AffineTransformation
initialAffineTransformation = AffineTransformation 1 0 0 1 0 0

mapDSAT :: (AffineTransformation -> AffineTransformation) -> DrawState -> DrawState
mapDSAT f (DrawState at mc) = DrawState (f at) mc

mapDSColor :: (Maybe Color -> Maybe Color) -> DrawState -> DrawState
mapDSColor f (DrawState at mc) = DrawState at (f mc)

initialDS :: DrawState
initialDS = DrawState initialAffineTransformation Nothing

translateDS :: Double -> Double -> DrawState -> DrawState
translateDS x y = mapDSAT $ \(AffineTransformation a b c d e f) ->
  AffineTransformation
    a
    b
    c
    d
    (a * x + c * y + e)
    (b * x + d * y + f)

scaleDS :: Double -> Double -> DrawState -> DrawState
scaleDS x y = mapDSAT $ \(AffineTransformation a b c d e f) ->
  AffineTransformation (x * a) (x * b) (y * c) (y * d) e f

rotateDS :: Double -> DrawState -> DrawState
rotateDS r = mapDSAT $ \(AffineTransformation a b c d e f) ->
  AffineTransformation
    (a * cos r + c * sin r)
    (b * cos r + d * sin r)
    (c * cos r - a * sin r)
    (d * cos r - b * sin r)
    e
    f

reflectDS :: Double -> DrawState -> DrawState
reflectDS th = mapDSAT $ \(AffineTransformation a b c d e f) ->
  AffineTransformation
    (a * cos r + c * sin r)
    (b * cos r + d * sin r)
    (a * sin r - c * cos r)
    (b * sin r - d * cos r)
    e
    f
  where r = 2 * th

setColorDS :: Color -> DrawState -> DrawState
setColorDS col = mapDSColor $ \mcol ->
  case (col, mcol) of
    (_, Nothing) -> Just col
    (RGBA _ _ _ 0, Just _) -> Just col
    (RGBA _ _ _ a1, Just (RGBA r0 g0 b0 a0)) -> Just (RGBA r0 g0 b0 (a0 * a1))

opaqueDS :: DrawState -> DrawState
opaqueDS = mapDSColor $ fmap $ \(RGBA r g b _) -> RGBA r g b 1

getColorDS :: DrawState -> Maybe Color
getColorDS (DrawState _ col) = col
