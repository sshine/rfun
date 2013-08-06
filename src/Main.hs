
module Main (main) where

import Control.Monad

import Syntax
import Lexer
import Parser

-- cat ../data/foo.rf | ./Main
main :: IO ()
main = do
  liftM (putStrLn . show . parse . tokenize) getContents
