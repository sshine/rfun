
module PISA -- (PISA(..), prettyPISA, inversePISA)
       where

import Data.List

import Pretty -- spaces

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
  | FINISH
  deriving (Show, Eq)

-- Partial function defined only for instructions used in code generation
inversePISA :: [PISA] -> [PISA]
inversePISA = reverse . map inv
  where
    inv :: PISA -> PISA
    -- Duals
    inv (ADD rega regb) = SUB rega regb
    inv (SUB rega regb) = ADD rega regb
    inv (ADDI reg imm)  = SUBI reg imm
    inv (SUBI reg imm)  = ADDI reg imm
    -- Own inverses
    inv (NEG reg) = NEG reg
    inv (XOR  rega regb) = XOR rega regb
    inv (XORI reg imm)   = XORI reg imm
    inv (EXCH rega regM) = EXCH rega regM
    -- Branching
    inv (BRA label) = BRA label
    inv (BEQ rega regb label) = BEQ rega regb label
    inv (BNE rega regb label) = BNE rega regb label
    -- Don't invert these
    inv (LABEL label) = LABEL label
    inv FINISH = FINISH

-- | Pretty-print PISA code (assume only one label per address)
prettyPISA :: [PISA] -> String
prettyPISA prog = go prog
  where
    go (LABEL lab : instr : prog') = lab ++ ": " ++
                                     indent (longestLabel - length lab) ++
                                     show instr ++ "\n" ++
                                     go prog'
    go (instr : prog') = indent (longestLabel+2) ++ show instr ++ "\n" ++ go prog'
    go [] = []
    
    longestLabel :: Int
    longestLabel = foldr (\label len -> labelLength label `max` len) 0 prog

labelLength :: PISA -> Int
labelLength (LABEL label) = length label 
labelLength _ = 0

prettyInstr :: PISA -> String
prettyInstr instr =
  case instr of
    ADD r1 r2     -> "ADD "  ++ prettyRegs [r1, r2]
    SUB r1 r2     -> "SUB "  ++ prettyRegs [r1, r2]
    NEG r         -> "NEG "  ++ prettyReg r
    XOR r1 r2     -> "XOR "  ++ prettyRegs [r1, r2]
    ADDI r im     -> "ADDI " ++ prettyReg r ++ prettyImm im
    SUBI r im     -> "SUBI " ++ prettyReg r ++ prettyImm im
    XORI r im     -> "XORI " ++ prettyReg r ++ prettyImm im
    ORX r1 r2 r3  -> "ORX "  ++ prettyRegs [r1, r2, r3]
    ANDX r1 r2 r3 -> "ANDX " ++ prettyRegs [r1, r2, r3]
    SLTX r1 r2 r3 -> "SLTX " ++ prettyRegs [r2, r2, r3]
    EXCH r1 r2    -> "EXCH " ++ prettyRegs [r1, r2]
    BRA lab       -> "BRA "  ++ lab
    RBRA lab      -> "RBRA " ++ lab
    BEQ r1 r2 lab -> "BEQ "  ++ prettyRegs [r1, r2] ++ " " ++ lab
    FINISH        -> "FINISH"

prettyRegs :: [Reg] -> String
prettyRegs = intercalate " " . map prettyReg

prettyReg :: Reg -> String
prettyReg 0 = "$0"
prettyReg 1 = "$hp"
prettyReg 2 = "$sp"
prettyReg 3 = "$ro"
prettyReg 4 = "$flp"
prettyReg 5 = "$ret"
prettyReg n = "$" ++ show n

prettyImm :: Imm -> String
prettyImm = show

---------- A collection of special-purpose registers: ----------

-- Zero register
regZero :: Reg
regZero = 0

-- Heap pointer
regHp :: Reg
regHp = 1

-- Stack pointer
regSp :: Reg
regSp = 2

-- Return offset, see [Clean Translation ..., sect. 4.2]
regRo :: Reg
regRo = 3

-- Free-list pointer
regFlp :: Reg
regFlp = 4

-- Return address for getfree()
regRet :: Reg
regRet = 5

-- Minimal free register
regAllocStart :: Reg
regAllocStart = 6
