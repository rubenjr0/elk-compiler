{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack)
import Lexer
import System.Environment
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [path] -> return path
    _ -> fail "Usage: ./compiler <path>"
  contents <- readFile path
  let src = pack contents
  case runParser lexer path src of
    Left err -> print err
    Right tokens -> print tokens
