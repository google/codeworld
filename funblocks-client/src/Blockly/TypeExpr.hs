{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

{-
  Copyright 2020 The CodeWorld Authors. All Rights Reserved.

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

module Blockly.TypeExpr
  ( Type (..),
    Type_,
    createType,
    fromList,
    toJSArray,
  )
where

import Data.JSString.Text
import qualified Data.Text as T
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import qualified JavaScript.Array as JA

newtype TypeExpr = TypeExpr JSVal

data Type
  = Func Type Type
  | Lit Text [Type]
  | TypeVar Text
  deriving (Show)

newtype Type_ = Type_ JSVal

createType :: Type -> Type_
createType (TypeVar text) = js_createVar (pack text)
createType (Lit text cs) = js_createLit (pack text) (toJSArray $ map createType cs)
createType (Func fst snd) = js_createFunc (createType fst) (createType snd)

fromList :: [Type] -> Type_
fromList tps = js_fromList $ toJSArray $ map createType tps

pack = textToJSString

unpack = textFromJSString

instance IsJSVal Type_

instance ToJSVal Type_ where
  toJSVal (Type_ v) = return v

instance FromJSVal Type_ where
  fromJSVal v = return $ Just $ Type_ v

toJSArray :: [Type_] -> JA.JSArray
toJSArray tps = JA.fromList $ map (\(Type_ a) -> a) tps

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "new Blockly.TypeExpr($1,$2)"
  js_createTypeExpr :: JSString -> JA.JSArray -> JSVal

foreign import javascript unsafe "Type.Var($1)"
  js_createVar :: JSString -> Type_

foreign import javascript unsafe "Type.Lit($1,$2)"
  js_createLit :: JSString -> JA.JSArray -> Type_

foreign import javascript unsafe "Type.Func($1,$2)"
  js_createFunc :: Type_ -> Type_ -> Type_

foreign import javascript unsafe "Type.fromList($1)"
  js_fromList :: JA.JSArray -> Type_

#else

js_createTypeExpr :: JSString -> JA.JSArray -> JSVal
js_createTypeExpr = error "GHCJS required"

js_createVar :: JSString -> Type_
js_createVar = error "GHCJS required"

js_createLit :: JSString -> JA.JSArray -> Type_
js_createLit = error "GHCJS required"

js_createFunc :: Type_ -> Type_ -> Type_
js_createFunc = error "GHCJS required"

js_fromList :: JA.JSArray -> Type_
js_fromList = error "GHCJS required"

#endif
