{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.NLPSolver ( NLPSolver
                        , NLPInput
                        , NLPOutput
                        , createIpoptSolver
                        , solveNlp
                        , nlpSolverUnsafeSetInput
                        , nlpSolverUnsafeGetOutput
                        ) where

import Foreign.C ( CDouble(..) )
import Foreign.Marshal ( finalizerFree, malloc, mallocArray )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, touchForeignPtr, withForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Storable ( peek )
import Unsafe.Coerce ( unsafeCoerce )

import Data.Vector.Storable ( Vector )
import qualified Data.Vector.Storable as V

import Casadi.Bindings.NLPSolver
import Casadi.Bindings.SXFunction ( c_sxFunctionInit )
import Casadi.SXFunction
import Casadi.NLPSolverOptions ( NLPSolverOption )
import Casadi.NLPSolverOptionsInternal ( nlpSolverUnsafeSetOption )
import Casadi.Types ( NLPSolver(..), SXFunction(..), SXM(..) )

data NLPInput = NLP_X_INIT -- Decision variables initial guess (n x 1)  [x_init]
              | NLP_LBX -- Decision variables lower bound (n x 1), default -inf [lbx]
              | NLP_UBX -- Decision variables upper bound (n x 1), default +inf [ubx]
              | NLP_LBG -- Constraints lower bound (m x 1), default -inf [lbg]
              | NLP_UBG -- Constraints upper bound (m x 1), default +inf [ubg]
              | NLP_LAMBDA_INIT -- Lagrange multipliers associated with G, initial guess (m x 1) [lambda_init]
              | NLP_P -- Only for parametric NLP - static parameters on which the objective and constraints might depend [p]
              | NLP_NUM_IN deriving Show

data NLPOutput = NLP_X_OPT -- Decision variables for optimal solution (n x 1) [x_opt]
               | NLP_COST -- Objective/cost function for optimal solution (1 x 1) [cost]
               | NLP_LAMBDA_G -- Lagrange multipliers associated with G at the solution (m x 1) [lambda_g]
               | NLP_LAMBDA_X -- Lagrange multipliers associated with bounds on X at the solution (n x 1) [lambda_x]
               | NLP_G -- The constraints evaluated at the optimal solution (m x 1) [g]
               | NLP_NUM_OUT deriving Show

enumNLPInputToInt :: NLPInput -> Int
enumNLPInputToInt NLP_X_INIT      = 0
enumNLPInputToInt NLP_LBX         = 1
enumNLPInputToInt NLP_UBX         = 2
enumNLPInputToInt NLP_LBG         = 3
enumNLPInputToInt NLP_UBG         = 4
enumNLPInputToInt NLP_LAMBDA_INIT = 5
enumNLPInputToInt NLP_P           = 6
enumNLPInputToInt NLP_NUM_IN      = 7

enumNLPOutputToInt :: NLPOutput -> Int
enumNLPOutputToInt NLP_X_OPT    = 0
enumNLPOutputToInt NLP_COST     = 1
enumNLPOutputToInt NLP_LAMBDA_G = 2
enumNLPOutputToInt NLP_LAMBDA_X = 3
enumNLPOutputToInt NLP_G        = 4
enumNLPOutputToInt NLP_NUM_OUT  = 5

createIpoptSolver :: SXM -> SXM -> SXM -> [NLPSolverOption] -> IO NLPSolver
createIpoptSolver (SXM designVars') (SXM objectiveFun') (SXM constraints') options = do
  let designVars   = unsafeForeignPtrToPtr designVars'
      objectiveFun = unsafeForeignPtrToPtr objectiveFun'
      constraints  = unsafeForeignPtrToPtr constraints'
  
  solverRaw <- c_createIpoptSolver designVars objectiveFun constraints >>= newForeignPtr c_deleteSolver

  touchForeignPtr designVars'
  touchForeignPtr objectiveFun'
  touchForeignPtr constraints'

  let solver = NLPSolver solverRaw
  mapM_ (nlpSolverUnsafeSetOption solver) options
  withForeignPtr (unsafeCoerce solverRaw) c_sxFunctionInit

  return solver

nlpSolverUnsafeSetInput :: NLPSolver -> NLPInput -> Vector CDouble -> IO (Maybe Int)
nlpSolverUnsafeSetInput (NLPSolver solver) idx val =
  sxFunctionUnsafeSetInput (SXFunction (unsafeCoerce solver)) (enumNLPInputToInt idx) val

nlpSolverUnsafeGetOutput :: NLPSolver -> NLPOutput -> (ForeignPtr CDouble, Int) -> IO (Maybe Int)
nlpSolverUnsafeGetOutput (NLPSolver solver) idx val =
  sxFunctionUnsafeGetOutput (SXFunction (unsafeCoerce solver)) (enumNLPOutputToInt idx) val


solveNlp :: NLPSolver -> (Vector CDouble, Vector CDouble) -> (Vector CDouble, Vector CDouble)
            -> Vector CDouble -> IO (CDouble, Vector CDouble)
solveNlp solver@(NLPSolver solverRaw) (xlb'', xub'') (gmin'', gmax'') xguess'' = do
  let trySet inputIdx val = do
        ret <- nlpSolverUnsafeSetInput solver inputIdx val
        case ret of
          Just n -> error $ "solveNlp: nlpSolverUnsafeSetInput " ++ show inputIdx ++
                    " expected length " ++ show n ++ " but got length " ++ show (V.length val)
          Nothing -> return ()
                             
  trySet NLP_X_INIT xguess''
  trySet NLP_LBX xlb''
  trySet NLP_UBX xub''
  trySet NLP_LBG gmin''
  trySet NLP_UBG gmax''

  withForeignPtr solverRaw c_solve

  let tryGet outputIdx val = do
        ret <- nlpSolverUnsafeGetOutput solver outputIdx val
        case ret of
          Just n -> error $ "solveNlp: nlpSolverUnsafeGetOutput " ++ show outputIdx ++
                    " expected length " ++ show n ++ " but got length " ++ show (snd val)
          Nothing -> return ()

  -- get the optimal output X as a storable vector
  let numDvs = V.length xguess''
  optX <- mallocArray numDvs >>= newForeignPtr finalizerFree
  tryGet NLP_X_OPT (optX, numDvs)
  let optXVec = V.unsafeFromForeignPtr0 optX numDvs

  optCost <- malloc >>= newForeignPtr finalizerFree
  tryGet NLP_COST (optCost, 1)
  let optCost' = unsafeForeignPtrToPtr optCost
  optCostOut <- peek optCost'
  touchForeignPtr optCost

  return (optCostOut, optXVec)
