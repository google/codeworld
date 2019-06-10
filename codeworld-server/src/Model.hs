{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC
    -fno-warn-unused-imports
#-}

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
module Model where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import System.FilePath (FilePath)
import Data.ByteString (ByteString)

data Project = Project
    { projectName :: Text
    , projectSource :: Text
    , projectHistory :: Value
    , projectOrder :: Int
    }

instance FromJSON Project where
    parseJSON (Object v) =
        Project <$> v .: "name" <*> v .: "source" <*> v .: "history" <*> v .: "order"
    parseJSON _ = mzero

instance ToJSON Project where
    toJSON p =
        object
            [ "name" .= projectName p
            , "source" .= projectSource p
            , "history" .= projectHistory p
            , "order" .= projectOrder p
            , "type" .= ("project" :: Text)
            ]

data DirectoryMeta = DirectoryMeta
    { dirMetaName :: Text
    , dirMetaOrder :: Int
    } deriving (Show)

instance FromJSON DirectoryMeta where
    parseJSON (Object v) =
        DirectoryMeta <$> v .: "name" <*> v .: "order"
    parseJSON _ = mzero

instance ToJSON DirectoryMeta where
    toJSON p =
        object
            [ "name" .= dirMetaName p
            , "order" .= dirMetaOrder p
            , "type" .= ("directory" :: Text)
            ]

data CompileResult = CompileResult
    { compileHash :: Text
    , compileDeployHash :: Text
    }

instance ToJSON CompileResult where
    toJSON cr =
        object ["hash" .= compileHash cr, "dhash" .= compileDeployHash cr]

data Gallery = Gallery { galleryItems :: [GalleryItem] }
data GalleryItem = GalleryItem
    { galleryItemName :: Text,
      galleryItemURL :: Text,
      galleryItemCode :: Maybe Text
    }

instance ToJSON Gallery where
    toJSON g = object [ "items" .= galleryItems g ]

instance ToJSON GalleryItem where
    toJSON item = case galleryItemCode item of
        Nothing -> object base
        Just code -> object (("code" .= code) : base)
      where base = [ "name" .= galleryItemName item
                   , "url" .= galleryItemURL item
                   ]

data CWEntry = Dir DirectoryMeta | Source Project

instance ToJSON CWEntry where
    toJSON (Source project) = toJSON project
    toJSON (Dir directory) = toJSON directory
 
instance FromJSON CWEntry where
    parseJSON (Object v) = do
        type_ <- v .: "type"
        case type_ :: String of
            "directory" -> do 
                dir <- DirectoryMeta <$> v .: "name" <*> v .: "order"
                return $ Dir dir
            "project" -> do
                project <- Project <$> v .: "name" <*> v .: "source" <*> v .: "history" <*> v .: "order"
                return $ Source project
    parseJSON _ = mzero