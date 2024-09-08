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

data Command = AddBook String String Int
             | ListBooks
             | FindBook String String Int
             | RemoveBook String String Int
             deriving (Show, Eq)

commandParser :: Parser Command
commandParser = try addBookParser
            <|> try listBooksParser
            <|> try findBookParser
            <|> try removeBookParser

addBookParser :: Parser Command
addBookParser = do
    void $ string "add book"
    spaces
    title <- manyTill anyChar (char ',')
    spaces
    author <- manyTill anyChar (char ',')
    spaces
    year <- many digit
    return $ AddBook title author (read year)

listBooksParser :: Parser Command
listBooksParser = ListBooks <$ string "list books"

findBookParser :: Parser Command
findBookParser = do
    void $ string "find book"
    spaces
    title <- manyTill anyChar (char ',')
    spaces
    author <- manyTill anyChar (char ',')
    spaces
    year <- many digit
    return $ FindBook title author (read year)

removeBookParser :: Parser Command
removeBookParser = do
    void $ string "remove book"
    spaces
    title <- manyTill anyChar (char ',')
    spaces
    author <- manyTill anyChar (char ',')
    spaces
    year <- many digit
    return $ RemoveBook title author (read year)



eval' :: String -> String
eval' input = case parse commandParser "" input of
        Right cmd -> executeCommand cmd
executeCommand :: Command -> String
executeCommand (AddBook title author year) = "knyga prideta " ++ title ++ " autorius " ++ author ++ " (" ++ show year ++ ")"
executeCommand ListBooks = "knygu dar nera"
executeCommand (FindBook title author year) = "ieskoma knyga " ++ title ++ " autorius " ++ author ++ " (" ++ show year ++ ")"
executeCommand (RemoveBook title author year) = "knyga istrinta " ++ title ++ " autorius " ++ author ++ " (" ++ show year ++ ")"



print' :: String -> IO ()
print' = putStrLn 