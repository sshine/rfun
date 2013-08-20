
module Main (main) where

import Control.Monad

import Syntax
import Lexer
import Parser
import Pretty
import Type
import Code
import PISA

-- cat ../data/foo.rf | ./Main
main :: IO ()
main = do
  src  <- getContents
  let ast          = parse . tokenize $ src
      (ast', tenv) = preprocess ast
      code         = translate ast'

  putStrLn "========== Concrete syntax (from file) =========="
  putStrLn src

  putStrLn "========== Abstract syntax (pretty-printed) =========="
  putStrLn (pretty ast)

  putStrLn "========== PISA code (pretty-printed) =========="
  putStrLn (prettyPISA code)
