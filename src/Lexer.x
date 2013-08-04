{
module Lexer ( tokenize
             , Token(..)
             , AlexPosn(..)
             , getPos
             , getName ) where

}

%wrapper "posn"

tokens :-

  $white+                 ;
  "//".*                  ;
  ":="    { const . DEFEQ }
  "="     { const . ISEQ  }
  "("     { const . LPAR  }
  ")"     { const . RPAR  }
  "->"    { const . ARROW }
  "|"     { const . PIPE  }
  ","     { const . COMMA }
  "def"   { const . DEF   }
  "let"   { const . LET   }
  "in"    { const . IN    }
  "case"  { const . CASE  }
  "of"    { const . OF    }
  "Cons"  { const . CONS  }
  "Nil"   { const . NIL   }
  [a-zA-Z_][a-zA-Z_]* { curry ID }

{

-- Available tokens
data Token = DEFEQ AlexPosn
           | ISEQ  AlexPosn
           | LPAR  AlexPosn
           | RPAR  AlexPosn
           | ARROW AlexPosn
           | PIPE  AlexPosn
           | COMMA AlexPosn
           | DEF   AlexPosn
           | LET   AlexPosn
           | IN    AlexPosn
           | CASE  AlexPosn
           | OF    AlexPosn
           | CONS  AlexPosn
           | NIL   AlexPosn
           | ID   (AlexPosn, String)
           deriving (Show, Eq)

-- Extract the position of a Token
getPos :: Token -> AlexPosn
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
