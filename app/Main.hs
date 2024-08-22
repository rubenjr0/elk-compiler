module Main (main) where

import Data.Text.IO qualified as TIO
import Lexer
import Parser.Program
import System.Environment
import Text.Megaparsec (errorBundlePretty, parse)
import Token

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> lexFile path
    _ -> fail "Usage: ./compiler <path>"

lexFile :: FilePath -> IO ()
lexFile path = do
  contents <- TIO.readFile path
  case parse lexer path contents of
    Left err -> print $ "Lexer error: " ++ errorBundlePretty err
    Right tokens -> parseTokens tokens path

parseTokens :: [Token] -> FilePath -> IO ()
parseTokens tokens path = do
  case parse pProgram path tokens of
    Left err -> print $ "Parser error: " ++ show err
    Right program -> print program
