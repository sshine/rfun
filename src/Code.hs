{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Code where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Syntax
import PISA

-- | A concatMap for lists inside arbitrary monads
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | A State-modifier that returns the previous state
modify' f = get <* modify f

-- | The RegAllocEnv type keeps track of register allocation
data RegAllocEnv = RegAllocEnv { regAllocStack :: [Reg]
                               , regAllocNext  :: Reg
                               , nextLabel :: Int
                               }
                 deriving (Show, Eq)

-- | The VarRegs type keeps track of variable->register binding
type VarRegs = M.Map VName Reg


-- | The CodeGen monad
newtype CodeGen a =
  CodeGen { runCodeGen :: StateT RegAllocEnv (Reader VarRegs) a }
  deriving (MonadState RegAllocEnv, MonadReader VarRegs,
            Applicative, Functor, Monad)

-- Constants
nilC :: Int
nilC = 0

consC :: Int
consC = 1

-- | Translate a given program, remove monad after use
translate :: Prog -> [PISA]
translate prog =
  runReader (evalStateT (runCodeGen (translateProg prog)) regAlloc) varRegs
  where
    varRegs  = M.empty
    regAlloc = RegAllocEnv [] regAllocStart

-- | Translate a given program within a CodeGen monad
translateProg :: Prog -> CodeGen [PISA]
translateProg = concatMapM translateDefn . progDefns

-- wrap defn in function call code
translateDefn :: Defn -> CodeGen [PISA]
translateDefn defn =
  let args = defnTerms defn
  in callwrap (defnName defn) $ do
    -- pop arguments from stack into registers
    (regs, code1) <- popVars (length args)
    -- associate registers with variables
    local $ putVarsRegs args regs $ do
      -- translate function body
      reg <- allocReg
      code2 <- translateExp (defnBody defn) reg
      mapM_ freeReg $ reg : regs   --- investigate: are regs zeroed before freeing?
      return $ code1 ++ code2

translateExp :: Exp -> Reg -> CodeGen [PISA]
translateExp exp reg =
  case exp of
    LeftExp lexp -> translateLeftExp lexp reg
    Let (_, var) (_, fname) args body _ -> do    -- let x = f y z in body
      -- push function arguments to stack
      code1 <- pushVars args
      -- call function f, store result (placed on stack) in regRet
      regRet <- allocReg
      code2 <- return [ BRA fname
                      , EXCH regRet regSp
                      , SUBI regSp 1 ]
      -- bind var to reg and translate function body
      local $ putVarReg var reg $ do
        code3 <- translateExp body reg
        return $ code1 ++ code2 ++ code3

    -- case x of Nil -> e
    Case var [(Nil _, e)] _ -> do
      memReg <- getVarReg var
      eCode <- translateExp e reg
      return $ [ EXCH reg memReg
               , BNE reg regZero "error" ] -- if it wasn't a Nil
               ++ eCode

    -- case x of Cons (a, b) -> e
    Case var [(Cons [Var leftId, Var rightId] _, body)] _ -> do
      memReg <- getVarReg var
      leftReg <- allocReg
      rightReg <- allocReg
      vars <- return [leftId, rightId]
      regs <- return [leftReg, rightReg]
      local $ putVarsRegs vars regs $ do
        bodycode <- translateExp body reg
        return $ [ EXCH reg memReg   -- temporarily use reg to store constructor
                 , SUBI reg 1
                 , BNE reg regZero "error" -- if it wasn't a Cons
                 , ADDI memReg 1
                 , EXCH leftReg memReg
                 , ADDI memReg 1
                 , EXCH rightReg memReg
                 ] ++ bodycode    -- this seems done
        
    -- case x of Cons (a, b) -> e1 | Nil -> e2
    --  `~> case x of Nil -> e2 | Cons (a, b) -> e1
    Case var [cons@(Cons _ _, _), nil@(Nil _, _)] posn ->
      let exp' = Case var [nil, cons] posn
      in translateExp exp' reg

    -- case x of Nil -> nilBody | Cons (a, b) -> consBody
    Case var [(Nil _, nilBody), (Cons [Var lvar, Var rvar] _, consBody)] _ -> do
      regE <- allocReg    -- compile either nil or cons body to this register
      regT <- allocReg    -- 
      memReg <- getVarReg var

      consCode <- translateExp consBody regE
      nilCode  <- translateExp nilBody  regE
      
      consCaseL <- newLabel "consCaseL"
      assTrueL  <- newLabel "assertTrue"
      nilCaseL  <- newLabel "testNil"
      assertL   <- newLabel "assert"

      (return $ [                  BNE  reg regZero "error"
                                 , EXCH reg memReg
                , LABEL consCaseL, BEQ  reg regZero nilCaseL  -- otherwise, Cons
                                 , XORI reg 1
                                 , BNE  reg regZero "error"
                                 , XORI regT 1 ] -- need this information on assertion
         ++ consCode ++
         [ LABEL assTrueL , BRA assertL
         , LABEL nilCaseL , BRA consCaseL ]
         ++ nilCode ++
         [ LABEL assertL  , BNE regT regZero assTrueL ]) <* do
        -- Clean up registers
        freeReg regT
        --freeReg regE --this should be cleaned up when it's deconstructed


-- | Translate expression and interpret result as boolean.
-- `regE` contains [[e]] and `regC` contains [[e]]c
-- This code is an unnecessary code bit from \cite{clean}.
--
-- translateExpCond :: Exp -> Reg -> Reg -> CodeGen [PISA]
-- translateExpCond eCond regE regC = do
--   code1 <- translateExp eCond regE
--   return $ code1 ++
--            [ LABEL "cond_top", BEQ regE regZero "cond_bot"
--                              , XORI regC 1
--            , LABEL "cond_bot", BEQ regE regZero "cond_top" ]

-- | Translate left-expressions (Nil, Cons, Var)
translateLeftExp :: LeftExp -> Reg -> CodeGen [PISA]
translateLeftExp (Nil _) regDest =
  return [ BRA "getfree"   -- regRet now contains pointer to heap
         , XORI regDest nilC
         , EXCH regDest regRet ]

translateLeftExp (Cons (l:r:_) _) regDest = do
  regL <- allocReg
  regR <- allocReg
  leftTreeCode <- translateLeftExp l regL
  rightTreeCode <- translateLeftExp r regR
  return $ leftTreeCode ++ rightTreeCode ++
    [ BRA "getfree"       -- regRet <- get_free()
    , EXCH regDest regSp
    , XORI regDest consC  -- regDest <- cons
    , ADDI regRet 1       -- regRet <- regRet + 1
    , EXCH regL regRet    -- regL <-> M(regRet)
    , ADDI regRet 1       -- regRet <- regRet + 1
    , EXCH regR regRet    -- regR <-> M(regRet)
    , SUBI regRet 2       -- regRet <- regRet - 2
    ]

translateLeftExp (Var (_, vname)) regDest = do
  memReg <- getVarReg vname
  return [ EXCH regDest memReg ]  -- regDest <-> M(reg)





------------------------------------------------------------------------
-- Helper functions                                                   --
------------------------------------------------------------------------

-- | Generate new unique label name
newLabel :: String -> CodeGen String
newLabel prefix = do
  env <- modify' $ \env -> env { nextLabel = nextLabel env + 1 }
  return $ prefix ++ "_" ++ show (nextLabel env)

-- | Allocate register
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


-- | Return register to allocation
freeReg :: Reg -> CodeGen ()
freeReg reg = modify $ \env -> env { regAllocStack = reg:regAllocStack env }


-- | Return register that stores a variable
-- TODO: Perhaps remove variable from table
getVarReg :: Id -> CodeGen Reg
getVarReg (_, vname) =
  asks $ fromMaybe err . M.lookup vname
  where
    err = error $ "Variable not bound to register: " ++ vname


-- | Associate variable with register
putVarReg :: Id -> Reg -> VarRegs -> VarRegs
putVarReg = M.insert

-- | Pairwise associate sets of variables and registers
putVarsRegs :: [Id] -> [Reg] -> VarRegs -> VarRegs
putVarsRegs ids regs varRegs = foldr (uncurry M.insert) varRegs $ zip ids regs

-- putVarsRegs ids regs = local (insertMany vars regs)
--   where insertMany :: [Id] -> [Reg] -> VarRegs -> VarRegs
--         insertMany [] [] varRegs = varRegs
--         insertMany (id:ids) (reg:regs) varRegs =
--           insertMany ids regs (M.insert id reg varRegs)


-- | Push variables to the stack, free their registers
pushVars :: [Id] -> CodeGen [PISA]
pushVars = concatMapM pushVar
  where pushVar (_, vname) = do
          reg <- getVarReg vname
          freeReg reg
          return [ EXCH reg regSp, ADDI regSp 1]

-- | Pop n variables from the stack, return registers and generating code
popVars :: Int -> CodeGen ([Reg], [PISA])
popVars 0 = return ([], [])
popVars n = do
  reg <- allocReg
  (regs, code) <- popVars (n-1)
  return (regs ++ [reg], code ++ [ SUBI regSp 1, EXCH reg regSp ])


-- | Wrap a function in calling convention code
callwrap :: Id -> CodeGen [PISA] -> CodeGen [PISA]
callwrap (_, fname) codeGen = do
  ftop <- newLabel (fname ++ "_top")
  flab <- newLabel  fname
  fbot <- newLabel (fname ++ "_bot")
  fcode <- codeGen
  return $
    [ LABEL ftop, BRA  fbot
                , SUBI regSp 1
                , EXCH regRo regSp
    , LABEL flab, SWAPBR regRo
                , NEG  regRo
                , EXCH regRo regSp
                , ADDI regSp 1 ]
    ++ fcode ++
    [ LABEL fbot, BRA ftop ]

-- | Fixed instruction for allocating memory
getfreeCode :: [PISA]
getfreeCode =
  callwrap (undefined, "getfree") $
  [ LABEL "getfree_if"     , BNE  regFlp regZero "getfree_else"
                           , XOR  regRet regHp
                           , ADDI regHp 3
  , LABEL "gf_assert_true" , BRA "getfree_assert"
  , LABEL "getfree_else"   , BRA "getfree_if"
                           , EXCH regRet regFlp   --   M(regFlp)
                           , SWAP regRet regFlp   --   M(regFlp)
  , LABEL "getfree_assert" , BEQ  regFlp regZero "gf_assert_true" ]
  -- Actually: should also assert that (regRet == regHp - 3)
  -- Consequence of not adding this check is: ?

errorCode :: [PISA]
errorCode = [ SWAPBR regRo, FINISH ]
