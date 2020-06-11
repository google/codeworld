{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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

-- | Minimal set of imports needed to support basic CodeWorld syntax.
-- Many of these are not directly used by students, but are needed for
-- desugaring with the RebindableSyntax extension.
module Core
  ( Program,
    Number,
    Text,
    Truth,
    Bool (..),
    fromInteger,
    fromRational,
    fromString,
    ifThenElse,
    fail,
  )
where
