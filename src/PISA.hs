
module PISA where

type Reg = Int
type Imm = Int
type Mem = Int
type Label = String

-- Reg<->Reg
-- Reg<->Imm
-- Reg<->Mem

data PISA = -- Data instructions
    ADD  Reg Reg
  | SUB  Reg Reg
  | NEG  Reg
  | XOR  Reg Reg
  | ADDI Reg Imm
  | SUBI Reg Imm
  | XORI Reg Imm
  | ORX  Reg Reg Reg
  | ANDX Reg Reg Reg
  | SLTX Reg Reg Reg
  | EXCH Reg Reg
    -- Branching instructions
  | BRA  Label
  | RBRA Label
  | BEQ  Reg Reg Label
  | BNE  Reg Reg Label
  | BGEZ Reg Label
  | SWAPBR Reg
  | SWAP Reg Reg  -- not in PISA article, but in heap article
  | LABEL Label
  deriving (Show, Eq)