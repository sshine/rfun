
module Main (main) where

import Control.Monad

import Syntax
import Lexer
import Parser
import Pretty

-- cat ../data/foo.rf | ./Main
main :: IO ()
main = do
  prog <- getContents
  ast  <- return $ parse . tokenize $ prog
  putStrLn (pretty ast)
