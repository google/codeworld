{-# LANGUAGE OverloadedStrings #-}

module Blocks.Printer (MPrint(..),
                       Printer(..),
                       writeRN, 
                       writeLine, 
                       write, 
                       indent, 
                       deindent, 
                       push, 
                       pop, 
                       pop', 
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

indent :: Printer
indent = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (col+4,stack,lines `T.append` (T.pack $ replicate 4 ' ') )

deindent :: Printer
deindent = do
  PrintState (col,stack,lines) <- get
  put $ PrintState (min 0 (col-4),stack,lines)

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
