
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

{- 1
  / \
 2   3  ~~>  1
            /
           2
            \
             3 -}


noPosn :: Posn
noPosn = Posn 0 0 0

nil' :: LeftExp
nil' = Nil noPosn

-- -- Return: left (...), right (siblings, so far)
-- binarify :: LeftExp -> (LeftExp, LeftExp)
-- binarify lexp =
--   case lexp of
--     Cons []     posn -> (Cons [] posn, nil')
--     Cons (c:cs) posn -> let (c', c_sib) = binarify c
--                             c_sib' = insertSibling 
--                         in Cons (c':c_sib':[]) posn


{- 1           1
  /|\         /
 2 3 4  ~~>  2  
/|          / \
56         5   3
            \   \
             6   4 -}