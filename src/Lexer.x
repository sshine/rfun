{
module Lexer ( tokenize
             , Token(..)
             , AlexPosn(..)
             , getPos
             , getName ) where

import Syntax

}

%wrapper "posn"

tokens :-

  $white+             ;
  "//".*              ;
  ":="    { tok DEFEQ }
  "="     { tok ISEQ  }
  "("     { tok LPAR  }
  ")"     { tok RPAR  }
  "->"    { tok ARROW }
  "|"     { tok PIPE  }
  ","     { tok COMMA }
  "def"   { tok DEF   }
  "let"   { tok LET   }
  "in"    { tok IN    }
  "case"  { tok CASE  }
  "of"    { tok OF    }
  "Cons"  { tok CONS  }
  "Nil"   { tok NIL   }
  [a-z_][a-zA-Z0-9_]* { \p s -> ID (posn p, s) }

{

posn :: AlexPosn -> Posn
posn (AlexPn offset lineNo colNo) = (Posn offset lineNo colNo)

tok :: (Posn -> Token) -> AlexPosn -> String -> Token
tok t = const . t . posn

-- Available tokens
data Token = DEFEQ Posn
           | ISEQ  Posn
           | LPAR  Posn
           | RPAR  Posn
           | ARROW Posn
           | PIPE  Posn
           | COMMA Posn
           | DEF   Posn
           | LET   Posn
           | IN    Posn
           | CASE  Posn
           | OF    Posn
           | CONS  Posn
           | NIL   Posn
           | ID   (Posn, String)
           deriving (Show, Eq)

-- Extract the position of a Token
getPos :: Token -> Posn
getPos tok =
  case tok of
    DEFEQ   posn -> posn
    ISEQ    posn -> posn
    LPAR    posn -> posn
    RPAR    posn -> posn
    ARROW   posn -> posn
    PIPE    posn -> posn
    COMMA   posn -> posn
    DEF     posn -> posn
    LET     posn -> posn
    IN      posn -> posn
    CASE    posn -> posn
    OF      posn -> posn
    CONS    posn -> posn
    NIL     posn -> posn
    ID (posn, _) -> posn

-- Extract the name of an ID Token
getName :: Token -> String
getName (ID (_, name)) = name
getName tok = error $ "Cannot access 'name' part of token " ++ show tok

-- Perform lexical analysis
tokenize :: String -> [Token]
tokenize = alexScanTokens

}
