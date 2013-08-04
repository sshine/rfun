
module Syntax where

import Lexer

type Id = (AlexPosn, String)

data Prog = Prog { progDefns :: [Defn] }
          deriving (Show, Eq)

data Defn = Defn { defnName  :: Id
                 , defnTerms :: [Id]
                 , defnBody  :: Exp }
          deriving (Show, Eq)

data Exp = LeftExp LeftExp
         | Let Id Id [Id] Exp AlexPosn
         | Case Id [(LeftExp, Exp)] AlexPosn
         | Foo Id Id AlexPosn
         deriving (Show, Eq)

data LeftExp = Var Id  --  Id stores posn
             | Nil AlexPosn
             | Cons [LeftExp] AlexPosn
             deriving (Show, Eq)
