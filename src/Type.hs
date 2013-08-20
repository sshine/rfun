
module Type where

import qualified Data.Map as M
import qualified Data.Set as S

import Syntax

data TypeEnv = TypeEnv { isNullary :: M.Map Id Bool  -- whether defn has params
                       , callGraph :: M.Map Id Id    -- which defns refer to which
                       }
             deriving (Show, Eq)

preprocess :: Prog -> (Prog, TypeEnv)
preprocess prog = (prog, initState)
  where initState = TypeEnv { isNullary = M.empty
                            , callGraph = M.empty }
