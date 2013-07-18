{
module RFUN.Lexer ( lex
                  , Token(..)
                  , Posn(..)
                  , getPos
                  ) where

-- Convert the AlexPosn type into Posn
data Posn = Posn { lineNum :: Int
                 , colNum  :: Int }
          deriving (Eq, Show)

posn :: AlexPosn -> Posn
posn AlexPn _ lineNum colNum = Posn lineNum colNum

data Token = DEFEQ Posn
           | EQ    Posn
           | LPAR  Posn
           | RPAR  Posn
           | DEF   Posn
           | LET   Posn
           | IN    Posn
           | CASE  Posn
           | OF    Posn
           | CONS  Posn
           | NIL   Posn
           | ID { idName :: String
                , idPosn :: Posn }
           deriving (Show, Eq)

getPos :: Token -> Posn
getPos tok =
  case tok of
    DEFEQ posn -> posn
    EQ    posn -> posn
    LPAR  posn -> posn
    RPAR  posn -> posn
    DEF   posn -> posn
    LET   posn -> posn
    IN    posn -> posn
    CASE  posn -> posn
    OF    posn -> posn
    CONS  posn -> posn
    NIL   posn -> posn
    ID _  posn -> posn

}

%wrapper "posn"

tokens :-
  $white+                                        ;
  "#".*                                          ;
  ":="                    { const . DEFEQ . posn }
  "="                     { const . EQ    . posn }
  "("                     { const . LPAR  . posn }
  ")"                     { const . RPAR  . posn }
  [a-zA-Z_] [a-zA-Z0-9_]* { flip keyword  . posn }

{

keyword :: String -> Posn -> Token
keyword s =
  let tok = case s of
        "def"  -> DEF
        "let"  -> LET
        "in"   -> IN
        "case" -> CASE
        "of"   -> OF
        "Cons" -> CONS
        "Nil"  -> NIL
        _      -> ID s
  in tok

lex :: String -> [Token]
lex = alexScanTokens

}