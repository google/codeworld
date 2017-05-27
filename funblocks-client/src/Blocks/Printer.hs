{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2017 The CodeWorld Authors. All Rights Reserved.

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

module Blocks.Printer (MPrint(..),
                       Printer(..),
                       writeRN, 
                       writeLine, 
                       write, 
                       write_, 
                       indent, 
                       deindent, 
                       makeSpace, 
                       intercalation,
                       push, 
                       pop, 
                       pop', 
                       setCol,
                       getCol,
                       reset, 
                       run) where

import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Lazy (get,put)
import qualified Data.Text as T


type MPrint a = S.State (PrintState T.Text) a

type Printer = MPrint ()

newtype PrintState a = PrintState (Int,[Int],a)

-- Add text to current line. Goto newline with col 0
writeRN :: T.Text -> Printer
writeRN code = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (0,stack,lines `T.append` code `T.append` "\n")

-- Add text to current line. Goto newline with current col
writeLine :: T.Text -> Printer
writeLine code = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col,stack,lines `T.append` code `T.append` "\n" `T.append` (T.pack $ replicate col ' ') )

-- Add to current line. Increase col. Adds a space to the end
write :: T.Text -> Printer
write code = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col + T.length code + 1,stack,lines `T.append` code `T.append` " ")

-- Add to current line. Increase col.
write_ :: T.Text -> Printer
write_ code = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col + T.length code, stack,lines `T.append` code)

getCol :: MPrint Int
getCol = do
  PrintState (col,stack,lines) <- get
  return col

setCol :: Int -> Printer
setCol newCol = do
  PrintState (_,stack,lines) <- get
  put $ PrintState (newCol, stack,lines)

-- Erase a single character on the current line
-- erase :: Printer
-- erase = do
--   p@(PrintState (col,stack,lines)) <- get
--   if col == 0 then
--     p  
--   else
--     PrintState (col-1, stack, T.take (T.length lines - 1) lines)

-- Applies func to each item, with a separator between
intercalation :: T.Text -> [a] -> (a -> Printer) -> Printer
intercalation sep items func = do
  case length items  of
    1 -> func $ head items
    _ -> let --firsts = take (length items - 1) items
             --last_ = tail items
             (body,feet) = splitAt (length items - 1) items
         in do
            mapM_ (\i -> func i >> write_ sep) body
            mapM_ (\i -> func i) feet

indent :: Printer
indent = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col+4,stack,lines `T.append` (T.pack $ replicate 4 ' ') )

deindent :: Printer
deindent = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (max 0 (col-4),stack,lines)

-- Makes a space if there isn't one already
makeSpace :: Printer
makeSpace = do
  PrintState (col,stack,lines) <- get
  if col>0 && T.last lines /= ' '
  then put $ PrintState (col+1,stack,lines `T.append` " ")
  else put $ PrintState (col,col:stack,lines)


-- Save the current position
push :: Printer
push = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col,col:stack,lines)

-- Restore the old position
pop :: Printer
pop = do
  PrintState (col,stack,lines) <- get
  case stack of
    (h:stack') -> put $ PrintState (h,stack',lines)
    _ -> put $ PrintState (col, stack, lines)

-- Restores old position and goes to newline
pop' :: Printer
pop' = do
  PrintState (col,stack,lines) <- get
  case stack of
    (h:stack') -> put $ PrintState (h,stack',lines `T.append` "\n" `T.append`  (T.pack $ replicate h ' ') )
    _ -> put $ PrintState (col, stack, lines)

-- Restores old position with a newline
reset :: Printer
reset = do
  PrintState (col,stack,lines) <- get
  case stack of
    (h:_) -> put $ PrintState (h,stack,lines `T.append` "\n" `T.append`  (T.pack $ replicate h ' ') )
    _ -> put $ PrintState (col, stack, lines)


run mon = let (_,PrintState (_,_,ans)) = S.runState mon (PrintState (0,[],"")) in ans


test = do
  write "ans = "
  writeLine "if a == 3"
  writeLine "then 10"
  write "else 20"

test2 = do
  writeLine "case ans of"
  indent
  writeLine "10 -> 'h'"
  writeLine "20 -> 'a'"
  write "_ -> 'k'"
  deindent

test3 = do
  writeLine "main = do"
  indent
  push
  write "let "
  test
  pop'
  write "in "
  test2

main = do
  let a = run test3
  putStrLn $ T.unpack a
