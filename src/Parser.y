{
module RFUN.Parser (...) where

import RFUN.Lexer   -- lex, Token(..), getTokenPos
import RFUN.Syntax  -- AST constructors

}

%name prog Prog
%tokentype { Token }
%error { parseError }

%token
  ':=' { DEFEQ posn }
  '='  { EQ    posn }
  '('  { LPAR  posn }
  ')'  { RPAR  posn }
  def  { DEF   posn }
  let  { LET   posn }
  in   { IN    posn }
  case { CASE  posn }
  of   { OF    posn }
  Cons { CONS  posn }
  Nil  { NIL   posn }
  id   { ID $$ posn }


%%

Prog : EOF        { Prog [] }
     | Defn Prog  { Prog ($1 : progDefns $2) }

Defn : def id Args ':=' Exp  { Defn (idName $2) $3 $5 }

Exp : LeftExp                    { LeftExp $1 }
    | let id '=' id Args in Exp  { Let $2 $4 $5 $7 }
    | case id of Cases           { Case $2 $4 }

LeftExp : id      { Var $1 }
        | Nil  { Nil }
        | Cons '(' LeftExps ')'  { Cons $3 }

Args :          { [] }
     | id Args  { idName $1 : $2 }

Cases :                     { [] }
      | Nil '->' Exp Cases  { (Nil, [], $3) : $4 }
;