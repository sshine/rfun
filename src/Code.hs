
module Code where

import Control.Monad
import Control.Monad.State
import Control.Applicative

import Syntax
import PISA

-- Constants
nilC :: Int
nilC = 0

consC :: Int
consC = 1

-- A collection of special-purpose registers:

-- Zero register
regZero :: Reg
regZero = 0

-- Heap pointer
regHp :: Reg
regHp = 1

-- Stack pointer
regSp :: Reg
regSp = 2

-- Return offset
regRetOff :: Reg
regRetOff = 3

-- Free-list pointer
regFlp :: Reg
regFlp = 4

-- Register containing the address of a getFree call
-- (or the return value of any function/procedure call)
regCell :: Reg
regCell = 5

-- Minimal free register
regAlloc :: Reg
regAlloc = 6

-- Variable counting
type CodeGen a = State Int a

runCodeGen :: CodeGen a -> (a, Int)
runCodeGen m = runState m regAlloc

newReg :: CodeGen Reg
newReg = do
  reg <- get
  put (reg+1)
  return reg

-- wrap a function body in call-convention code (from article)
callwrapper :: Label -> [PISA] -> [PISA]
callwrapper flab fcode =
  [ LABEL ftop, BRA  fbot
              , SUBI regSp 1
              , EXCH regRetOff regSp
  , LABEL flab, SWAPBR regRetOff
              , NEG  regRetOff
              , EXCH regRetOff regSp
              , ADDI regSp 1 ]
  ++ fcode ++
  [ LABEL fbot, BRA ftop ]
  where
    ftop = flab ++ "_top"
    fbot = flab ++ "_bot"

-- getfree code, to be placed once in the generated code.
-- combination of article 1 (get_free pseudocode) and clean p. 155
getFree :: Reg -> [PISA]
getFree destReg =
  callwrapper undefined   --  wrap getFree
  [ LABEL "getfree"     , BNE  regFlp regZero "getfree_else"
                        , XOR  destReg regHp
                        , ADDI regHp 3
                        , BRA "getfree_end"
  , LABEL "getfree_else", BRA "getfree"
                        , EXCH destReg regFlp   --   M(regFlp)
                        , SWAP destReg regFlp   --   M(regFlp)
  , LABEL "getfree_end" , BEQ  regFlp regZero "TODO..." ]


-- Translate Nil, Cons constants
translateLeftExp :: LeftExp -> Reg -> CodeGen [PISA]
translateLeftExp (Nil _) regDest =
  return [ BRA "getfree"
         , XORI regDest nilC
         , EXCH regDest regCell ]

translateLeftExp (Cons (l:r:_) _) regDest = do
      regL <- newReg
      regR <- newReg
      leftTreeCode <- translateLeftExp l regL
      rightTreeCode <- translateLeftExp r regR
      return $ leftTreeCode ++ rightTreeCode ++
        [ BRA "getfree"        -- regCell <- get_free()
        , XORI regDest consC   -- regDest <- cons
        , ADDI regCell 1       -- regCell <- regCell + 1
        , EXCH regL regCell    -- regL <-> M(regCell)
        , ADDI regCell 1       -- regCell <- regCell + 1
        , EXCH regR regCell    -- regR <-> M(regCell)
        , SUBI regCell 2       -- regCell <- regCell - 2
        ]
