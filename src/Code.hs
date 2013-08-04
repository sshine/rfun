
module RFUN.Code where

type Reg = Int
type Imm = Int
type Mem = Int

-- Reg<->Reg
-- Reg<->Imm
-- Reg<->Mem

data Instruction = XORI Reg Imm
                 | ADDI Reg Imm
                 | SUBI Reg Imm
                 | EXCH Reg Mem
                 | SWAP Reg Reg
                 | 
