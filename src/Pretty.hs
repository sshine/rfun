
module Pretty where

import Data.List

import Syntax

commas :: [String] -> String
commas = intercalate ", "

spaces :: Int -> String
spaces n = replicate n ' '

pretty :: Prog -> String
pretty = prettyProg

prettyProg :: Prog -> String
prettyProg = intercalate "\n\n" . map prettyDefn . progDefns

prettyDefn :: Defn -> String
prettyDefn (Defn (_, name) terms body) =
  "def " ++ name ++ " " ++
  commas (map prettyId terms) ++ " :=\n" ++
  prettyExp 4 body

prettyExp :: Int -> Exp -> String
prettyExp n exp =
  spaces n ++
  case exp of
    LeftExp le -> prettyLeftExp le
    Let lhs fID termIDs body _ ->
      "let " ++ snd lhs ++ " = " ++ snd fID ++ " " ++
      intercalate " " (map snd termIDs) ++ " in " ++ prettyExp body
    Case caseId cases _ ->
      "case " ++ snd caseId ++ " of\n" ++
      spaces n

