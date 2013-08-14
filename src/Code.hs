
module Code where

import Control.Monad
import Control.Monad.State
import Control.Applicative

import Syntax
import PISA

-- | The CodeGenEnv keeps track of register allocation, ...
data CodeGenEnv = CodeGenEnv { regAllocStack :: [Reg]
                             , regAllocNext  :: Reg }
              deriving (Show, Eq)


-- | The CodeGen monad
type CodeGen a = State CodeGenEnv a


-- | Translate a given program, remove monad after use
translate :: Prog -> [PISA]
translate prog =
  fst $ runState (translateProg prog) initState
  where initState = CodeGenEnv { regAllocStack = []
                               , regAllocNext = regAllocStart }


-- | Translate a given program within a CodeGen monad
translateProg :: Prog -> CodeGen [PISA]
translateProg = liftM concat . mapM translateDefn . progDefns

translateDefn :: Defn -> CodeGen [PISA]
translateDefn defn = return [] -- TODO


-- | Translate left-expressions (Nil, Cons, Var)
translateLeftExp :: LeftExp -> Reg -> CodeGen [PISA]
translateLeftExp (Nil _) regDest =
  return [ BRA "getfree"
         , XORI regDest nilC
         , EXCH regDest regCell ]

translateLeftExp (Cons (l:r:_) _) regDest = do
      regL <- allocReg
      regR <- allocReg
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
regAllocStart :: Reg
regAllocStart = 6

-- Assume that an arbitrary amount of symbolic registers exist, but allow for
-- returning used ones to a pool.  When it can be asserted, during compilation,
-- that a register contains zero and will not be needed locally, return it to
-- the allocator.  Do not regard spilling yet.  This is very manual.

allocReg :: CodeGen Reg
allocReg = do
  env <- get
  case regAllocStack env of
    [] -> do
      put $ env { regAllocNext = regAllocNext env + 1 }
      return $ regAllocNext env
    reg:stack -> do
      put $ env { regAllocStack = stack }
      return reg

freeReg :: Reg -> CodeGen ()
freeReg reg = modify $ \env -> env { regAllocStack = reg:regAllocStack env }

-- Constants
nilC :: Int
nilC = 0

consC :: Int
consC = 1

-- wrap a function body in call-convention code (from article)
callwrapper :: Label -> [PISA] -> [PISA]
callwrapper flab fcode =
  [ LABEL ftop, BRA  fbot
              , SUBI regSp 1
              , EXCH regRetOff regSp
  , LABEL fmid, SWAPBR regRetOff
              , NEG  regRetOff
              , EXCH regRetOff regSp
              , ADDI regSp 1 ]
  ++ fcode ++
  [ LABEL fbot, BRA ftop ]
  where
    ftop = flab ++ "_top"
    fmid = flab ++ "_mid"
    fbot = flab ++ "_bot"

-- getfree code, to be placed once in the generated code.
-- combination of article 1 (get_free pseudocode) and clean p. 155
getFreeCode :: Reg -> [PISA]
getFreeCode destReg =
  callwrapper "getfree"
  [ LABEL "getfree"     , BNE  regFlp regZero "getfree_else"
                        , XOR  destReg regHp
                        , ADDI regHp 3
                        , BRA "getfree_end"
  , LABEL "getfree_else", BRA "getfree"
                        , EXCH destReg regFlp   --   M(regFlp)
                        , SWAP destReg regFlp   --   M(regFlp)
  , LABEL "getfree_end" , BEQ  regFlp regZero "TODO..." ]

