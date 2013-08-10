
module Syntax where

data Posn = Posn !Int !Int !Int
          deriving Eq

instance Show Posn where
  show (Posn offset lineNo colNo) =
    "(line " ++ show lineNo ++ ", column " ++ show colNo ++ ")"

type Id = (Posn, String)

data Prog = Prog { progDefns :: [Defn] }
          deriving (Show, Eq)

data Defn = Defn { defnName  :: Id
                 , defnTerms :: [Id]
                 , defnBody  :: Exp }
          deriving (Show, Eq)

data Exp = LeftExp LeftExp
         | Let Id Id [Id] Exp Posn
         | Case Id [(LeftExp, Exp)] Posn
         deriving (Show, Eq)

data LeftExp = Var Id  --  Id stores posn
             | Nil Posn
             | Cons [LeftExp] Posn
             deriving (Show, Eq)
