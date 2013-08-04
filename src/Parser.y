{
module Parser (parse) where

import Lexer   -- lex, Token(..), getPos
import Syntax  -- AST constructors

--parseError :: [Token] -> Either String a
--parseError []      = Left $ "Parse error: End of file"
--parseError (tok:_) = Left $ "Parse error: Token " ++ show tok

parseError :: [Token] -> a
parseError []      = error $ "Parse error: End of file"
parseError (tok:_) = error $ "Parse error: Token " ++ show tok

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  ':='    { DEFEQ $$ }
  '='     { ISEQ  $$ }
  '('     { LPAR  $$ }
  ')'     { RPAR  $$ }
  '->'    { ARROW $$ }
  '|'     { PIPE  $$ }
  ','     { COMMA $$ }
  def     { DEF   $$ }
  let     { LET   $$ }
  in      { IN    $$ }
  case    { CASE  $$ }
  of      { OF    $$ }
  Cons    { CONS  $$ }
  Nil     { NIL   $$ }
  id      { ID    $$ }

%%

Prog : ProgDefns            { Prog $1 }

ProgDefns :                 { [] }
          | Defn ProgDefns  { $1 : $2 }

Defn : def id Ids ':=' Exp      { Defn $2 $3 $5 }

Ids :        { [] }
    | id Ids { $1 : $2 }

Exp : LeftExp                   { LeftExp $1 }
    | let id '=' id Ids in Exp  { Let $2 $4 $5 $7 $1 }
    | case id of Cases          { Case $2 $4 $1 }

LeftExp : id   { Var $1 }
        | Nil  { Nil $1 }
        | Cons '(' LeftExps ')'  { Cons $3 $1 }

LeftExps : LeftExp               { [$1] }
         | LeftExp ',' LeftExps  { $1 : $3 }

Cases : Case            { [$1] }
      | Case '|' Cases  { $1 : $3 }

Case : Nil '->' Exp                    { (Nil $1, $3) }
     | Cons '(' LeftExps ')' '->' Exp  { (Cons $3 $1, $6) }
