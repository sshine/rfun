
module RFUN.Syntax where

import RFUN.Lexer

type Id = String

data Prog = Prog { progDefns :: [Defn] }
          deriving (Show, Eq)

data Defn = Defn { defnName  :: Id
                 , defnTerms :: [Id]
                 , defnBody  :: Exp }
          deriving (Show, Eq)

data Exp = LeftExp LeftExp
         | Let Id Id [Id] Exp AlexPosn
         | Case Id [(LeftExp, [Id], Exp)] AlexPosn
         | Foo Id Id AlexPosn
         deriving (Show, Eq)

data LeftExp = Var Id AlexPosn
             | Nil AlexPosn
             | Cons [LeftExp] AlexPosn
             deriving (Show, Eq)
