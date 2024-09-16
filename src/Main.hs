module Main where

import           Control.Monad     (unless)
import           System.IO


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, digit, char, string, spaces)
import Control.Monad (void)


main :: IO ()
main = do
  input <- read'

  unless (input == ":quit")
       $ print' (eval' input)
      >> main


read' :: IO String
read' = putStr "REPL> "
     >> hFlush stdout
     >> getLine





eval' :: String -> String
eval' input = input


print' :: String -> IO ()
print' = putStrLn 