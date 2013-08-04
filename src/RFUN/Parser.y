{
module RFUN.Parser where

import RFUN.Lexer   -- lex, Token(..), getPos
import RFUN.Syntax  -- AST constructors

--parseError :: [Token] -> Either String a
--parseError []      = Left $ "Parse error: End of file"
--parseError (tok:_) = Left $ "Parse error: Token " ++ show tok

parseError :: [Token] -> a
parseError []      = error $ "Parse error: End of file"
parseError (tok:_) = error $ "Parse error: Token " ++ show tok

}

%name prog
%tokentype { Token }
%error { parseError }

%token
  ':='    { DEFEQ posn }
  '='     { ISEQ  posn }
  '('     { LPAR  posn }
  ')'     { RPAR  posn }
  '->'    { ARROW posn }
  '|'     { PIPE  posn }
  ','     { COMMA posn }
  def     { DEF   posn }
  let     { LET   posn }
  in      { IN    posn }
  case    { CASE  posn }
  of      { OF    posn }
  Cons    { CONS  posn }
  Nil     { NIL   posn }
  id      { ID posn $$ }

%%

Prog : ProgDefns            { Prog $1 }

ProgDefns :                 { [] }
          | Defn ProgDefns  { $1 : $2 }

Defn : def id Ids ':=' Exp      { Defn $2 $3 $5 }

Ids :        { [] }
    | id Ids { $1 :: $2 }

Exp : Nil     { LeftExp (Nil . getPos $ $1)     }
    | Cons    { LeftExp (Cons [] . getPos $ $1) }
