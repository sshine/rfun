
module Code where

import Syntax
import PISA

-- Constants
nil :: Int
nil = 0

cons :: Int
cons = 1

-- A collection of special-purpose registers:

-- Zero register
zeroReg :: Int
zeroReg = 0

-- Heap pointer
hpReg :: Int
hpReg = 1

-- Stack pointer
spReg :: Int
spReg = 2

-- Return offset
roReg :: Int
roReg = 3

-- Free-list pointer
flpReg :: Int
flpReg = 4

-- Register containing the address of a getFree call
cellReg :: Int
cellReg = 5

callFunction :: Label -> [PISA] -> [PISA]
callFunction flab fcode =
  [ LABEL ftop, BRA  fbot
              , SUBI spReg 1
              , EXCH roReg spReg
  , LABEL flab, SWAPBR roReg
              , NEG  roReg
              , EXCH roReg spReg
              , ADDI spReg 1 ]
  ++ fcode ++
  [ LABEL fbot, BRA ftop ]
  where
    ftop = flab ++ "_top"
    fbot = flab ++ "_bot"

-- getfree 
getFree :: [PISA]
getFree destReg =
  callFunction "getfree"
  [ LABEL "getfree"
  , BNE  flpReg zeroReg "getfree_else"
  , XOR  destReg hpReg
  , ADDI hpReg 3
  , LABEL "getfree_else"
  , EXCH destReg flpReg
  , SWAP destReg flpReg
  , 

translateLeftExp :: LeftExp -> [PISA]
translateLeftExp lexp =
  case lexp of
    Nil _        -> 
    Cons lexps _ -> 
