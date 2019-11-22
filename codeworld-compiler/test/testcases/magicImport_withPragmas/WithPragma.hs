-- Enable a language extension that's not enabled in CodeWorld,
-- Just to test that the pragma is preserved.

{-# LANGUAGE MultiParamTypeClasses #-}

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

class PlusOne a b where
    plusOne :: a -> b

instance PlusOne Number Number where
    plusOne a = a + 1

foo :: Number
foo = plusOne 41