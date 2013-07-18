
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
         | Let Id Id [Id] Exp Posn
         | Case Id [(Construct, [Id], Exp)] Posn
         deriving (Show, Eq)

data LeftExp = Var Id Posn
             | Nil Posn
             | Cons [LeftExp] Posn
             deriving (Show, Eq)
