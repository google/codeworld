{-# LANGUAGE JavaScriptFFI #-}

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

module Blockly.TypeExpr ( TypeExpr(..)
                         ,createTypeExpr
                         ,getName
                         ,getChildren
                         )
  where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified JavaScript.Array as JA
import qualified Data.Text as T

newtype TypeExpr = TypeExpr JSVal

pack = textToJSString
unpack = textFromJSString

instance IsJSVal TypeExpr

instance ToJSVal TypeExpr where
  toJSVal (TypeExpr v) = return v

instance FromJSVal TypeExpr where
  fromJSVal v = return $ Just $ TypeExpr v

createTypeExpr :: T.Text -> [TypeExpr] -> TypeExpr
createTypeExpr name children = js_createTypeExpr (pack name) children
  where ch = JA.fromList $ map (\TypeExpr a -> a) children 

getName :: TypeExpr -> T.Text
getName = unpack . js_getName

getChildren :: TypeExpr -> [TypeExpr]
getChildren tp = map TypeExpr $ JA.fromList $ js_getChildren tp

foreign import javascript unsafe "new Blockly.TypeExpr($1,$2)"
  js_createTypeExpr :: JSString -> JA.JSArray -> JSVal

foreign import javascript unsafe "$1.name"
  js_getName :: TypeExpr -> JSString

foreign import javascript unsafe "$1.children"
  js_getChildren :: TypeExpr -> JA.JSArray
