
module Pretty where

import Data.List

import Syntax

commas :: [String] -> String
commas = intercalate ", "

spaces :: [String] -> String
spaces = intercalate " "

indent :: Int -> String
indent n = replicate n ' '

pretty :: Prog -> String
pretty = prettyProg

prettyProg :: Prog -> String
prettyProg = intercalate "\n\n" . map prettyDefn . progDefns

prettyDefn :: Defn -> String
prettyDefn (Defn (_, name) terms body) =
  "def " ++ name ++ " " ++
  spaces (map prettyId terms) ++ " :=\n" ++
  indent 4 ++ prettyExp 4 body

prettyId :: Id -> String
prettyId = snd

prettyExp :: Int -> Exp -> String
prettyExp n exp =
  case exp of
    LeftExp le -> prettyLeftExp n le
    Let var fID termIDs body _ ->
      let firstLetLine = "let " ++ snd var ++ " = " ++ snd fID ++ " " ++ spaces (map prettyId termIDs) ++ " in"
          separator = "\n" ++ indent (n + length firstLetLine)
          secondLetLine = prettyExp n body
      in firstLetLine ++ separator ++ secondLetLine
    Case caseId (c:cs) _ ->
      "case " ++ prettyId caseId ++ " of\n" ++
      indent (n+4) ++ prettyCase (n+2) c ++
      concatMap (prefixCase . prettyCase n) cs
  where
    prefixCase s = indent n ++ "  | " ++ s

prettyLeftExp :: Int -> LeftExp -> String
prettyLeftExp n lexp =
  case lexp of
    Var (_, vname) -> vname
    Nil _ -> "Nil"
    Cons lexps _ -> "Cons (" ++ commas (map (prettyLeftExp n) lexps) ++ ")"

prettyCase :: Int -> (LeftExp, Exp) -> String
prettyCase n (lexp, exp) =
  prettyLeftExp n lexp ++ " -> " ++ prettyExp n exp ++ "\n"
