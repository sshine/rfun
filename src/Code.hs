
module Code where

import Control.Monad
import Control.Monad.State
import Control.Applicative

import qualified Data.Map as M

import Syntax
import PISA

-- | The CodeGenEnv keeps track of register allocation, ...
data CodeGenEnv = CodeGenEnv { regAllocStack :: [Reg]
                             , regAllocNext  :: Reg 
                             , varRegs       :: M.Map String Reg
                             }
                deriving (Show, Eq)


-- | The CodeGen monad
type CodeGen a = State CodeGenEnv a

-- | A monadic concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


-- | Translate a given program, remove monad after use
translate :: Prog -> [PISA]
translate prog =
  fst $ runState (translateProg prog) initState
  where initState = CodeGenEnv { regAllocStack = []
                               , regAllocNext = regAllocStart
                               , varRegs = M.empty
                               }


-- | Translate a given program within a CodeGen monad
translateProg :: Prog -> CodeGen [PISA]
translateProg = liftM concat . mapM translateDefn . progDefns

translateDefn :: Defn -> CodeGen [PISA]
translateDefn defn =
  case defnTerms defn of
    [] -> do reg <- allocReg
             translateExp (defnBody defn) reg
    ts -> return []

translateExp :: Exp -> Reg -> CodeGen [PISA]
translateExp exp reg =
  case exp of
    LeftExp lexp -> translateLeftExp lexp reg
    Let (_, var) fID argIDs body _ -> do    -- let x = f y z in body
      -- swap termIDs to the stack
      code1 <- concatMapM (\(_, var) -> do
                              reg <- getVarReg var
                              return [ EXCH reg regSp
                                     , ADDI regSp 1 ]) argIDs
      -- call function f, store result in new register
      reg <- allocReg
      code2 <- return [ BRA (funCallLabel fID) 
                      , SWAP regRet reg ]
      bindVar var reg
      -- swap termIDs back from the stack (assume their uncomputation happened)
      code3 <- return $ inversePISA code1
      -- Q: where is it good to place uncomputation of function arguments? function epilogue?
      -- translate body and store result in 'reg'
      code4 <- translateExp body regRet -- since regRet is 0 due to 'SWAP regRet reg' above
      removeVar var  -- replace with local ReaderT binding
      return $ code1 ++ code2 ++ code3 ++ code4

-- This method should probably remove vname from table
getVarReg :: String -> CodeGen Reg
getVarReg vname = do
  env <- get
  return $ M.findWithDefault err vname (varRegs env)
  where
    err = error $ "Variable " ++ vname ++ " was not found."

bindVar :: String -> Reg -> CodeGen ()
bindVar var reg =
  modify $ \env -> env { varRegs = M.insert var reg (varRegs env) }

-- TODO: Until varRegs is replaced with ReaderT, assume unique variables
-- This is an insufficient assumption for recursion.
removeVar :: String -> CodeGen ()
removeVar var =
  modify $ \env -> env { varRegs = M.delete var (varRegs env) }

-- | Get the jump label for a function wrapped in call-convention code
funCallLabel :: Id -> String
funCallLabel (_posn, vname) = "f_" ++ vname

{-
Swap $x, $y, $z to stack:

EXCH $x $sp
ADDI $sp 1
EXCH $y $sp
ADDI $sp 1
EXCH $z $sp
ADDI $sp

-}

-- | Translate left-expressions (Nil, Cons, Var)
translateLeftExp :: LeftExp -> Reg -> CodeGen [PISA]
translateLeftExp (Nil _) regDest =
  return [ BRA "getfree"
         , XORI regDest nilC
         , EXCH regDest regRet ]

translateLeftExp (Cons (l:r:_) _) regDest = do
  regL <- allocReg
  regR <- allocReg
  leftTreeCode <- translateLeftExp l regL
  rightTreeCode <- translateLeftExp r regR
  return $ leftTreeCode ++ rightTreeCode ++
    [ BRA "getfree"        -- regRet <- get_free()
    , XORI regDest consC   -- regDest <- cons
    , ADDI regRet 1       -- regRet <- regRet + 1
    , EXCH regL regRet    -- regL <-> M(regRet)
    , ADDI regRet 1       -- regRet <- regRet + 1
    , EXCH regR regRet    -- regR <-> M(regRet)
    , SUBI regRet 2       -- regRet <- regRet - 2
    ]

translateLeftExp (Var (_, vname)) regDest = return []

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
regRet :: Reg
regRet = 5

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

