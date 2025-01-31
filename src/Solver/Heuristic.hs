{-# LANGUAGE BangPatterns #-}
module Solver.Heuristic
  ( HeuristicType(..)
  , HeuristicState(..)
  , initHeuristic
  , selectVariable
  , updateOnConflict
  , updateOnPropagation
  , decayHeuristic
  ) where

import Solver.Types (AssignmentMap, HeuristicType(..))
import qualified Data.Vector.Unboxed as VU
import Data.Function (on)
import Data.List     (sortBy)
import Data.Maybe    (isNothing)

--------------------------------------------------------------------------------
-- Heuristic State
--------------------------------------------------------------------------------

{-|
  For advanced heuristics (LRB, CHB), the solver typically tracks additional stats:

  - 'varActivity' (VSIDS-like).
  - 'varConflicts': how many conflicts a variable has participated in.
  - 'varSuccess': a measure of successful propagations or "reward" signals.

  In LRB (Learning Rate Branching), each variable has a "score" based on:
    score(v) = activity(v) / (1 + conflicts(v))
  or uses the "glucose-based LBD" to adapt the score.

  In CHB (Conflict History Based), we track a "moving average" for each variable
  that updates on each conflict or success.
-}

data HeuristicState = HeuristicState
  { varActivity :: !(VU.Vector Double)
  , varConflicts :: !(VU.Vector Double)  -- for LRB
  , varSuccess   :: !(VU.Vector Double)  -- for CHB
  , activityInc  :: !Double
  , activityDec  :: !Double
  , heuristicTy  :: !HeuristicType
  }

initHeuristic :: Int -> HeuristicType -> HeuristicState
initHeuristic varCount ht = HeuristicState
  { varActivity = VU.replicate (varCount+1) 0.0
  , varConflicts = VU.replicate (varCount+1) 0.0
  , varSuccess   = VU.replicate (varCount+1) 0.0
  , activityInc  = 1.0
  , activityDec  = 0.95
  , heuristicTy  = ht
  }

--------------------------------------------------------------------------------
-- Variable Selection
--------------------------------------------------------------------------------

-- | 'selectVariable' picks the next unassigned variable according to the chosen heuristic.
selectVariable :: HeuristicState -> AssignmentMap -> IO Int
selectVariable hState asg = do
  case heuristicTy hState of
    VSIDS -> vsidsSelect hState asg
    LRB   -> lrbSelect hState asg
    CHB   -> chbSelect hState asg

--------------------------------------------------------------------------------
-- VSIDS
--------------------------------------------------------------------------------

vsidsSelect :: HeuristicState -> AssignmentMap -> IO Int
vsidsSelect hState asg = do
  let acts = varActivity hState
      indices = [1 .. (VU.length acts - 1)]
      unassigned = filter (\i -> isNothing (asg VU.! i)) indices
      scored = map (\i -> (i, acts VU.! i)) unassigned
      sorted = sortBy (compare `on` snd) scored
  if null sorted
    then return (-1)
    else return (fst (last sorted))

--------------------------------------------------------------------------------
-- LRB
--------------------------------------------------------------------------------

lrbSelect :: HeuristicState -> AssignmentMap -> IO Int
lrbSelect hState asg = do
  let acts = varActivity hState
      confs = varConflicts hState
      indices = [1..(VU.length acts - 1)]
      unassigned = filter (\i -> isNothing (asg VU.! i)) indices
      scored = map (\i ->
                     let a = acts VU.! i
                         c = confs VU.! i
                         score = a / (1 + c)  -- simple approximation
                     in (i, score)) unassigned
      sorted = sortBy (compare `on` snd) scored
  if null sorted
    then return (-1)
    else return (fst (last sorted))

--------------------------------------------------------------------------------
-- CHB
--------------------------------------------------------------------------------

chbSelect :: HeuristicState -> AssignmentMap -> IO Int
chbSelect hState asg = do
  let acts = varActivity hState
      succs = varSuccess hState
      indices = [1..(VU.length acts - 1)]
      unassigned = filter (\i -> isNothing (asg VU.! i)) indices
      scored = map (\i ->
                     let a = acts VU.! i
                         s = succs VU.! i
                         score = a + s  -- simplistic formula
                     in (i, score)) unassigned
      sorted = sortBy (compare `on` snd) scored
  if null sorted
    then return (-1)
    else return (fst (last sorted))

--------------------------------------------------------------------------------
-- Updates and Decay
--------------------------------------------------------------------------------

-- | Update heuristic on conflict. Typically, bump the activity for variables in conflict.
updateOnConflict :: [Int] -> HeuristicState -> HeuristicState
updateOnConflict vars hState =
  let acts = varActivity hState
      confs = varConflicts hState
      inc = activityInc hState
      newActs = foldr (\v acc -> acc VU.// [(v, (acc VU.! v) + inc)]) acts vars
      newConfs = foldr (\v acc -> acc VU.// [(v, (acc VU.! v) + 1.0)]) confs vars
  in hState { varActivity = newActs, varConflicts = newConfs }

-- | Update heuristic on successful propagation.
updateOnPropagation :: [Int] -> HeuristicState -> HeuristicState
updateOnPropagation vars hState =
  let succs = varSuccess hState
      newSucc = foldr (\v acc -> acc VU.// [(v, (acc VU.! v) + 1.0)]) succs vars
  in hState { varSuccess = newSucc }

-- | Decay the activity and adjust increments (VSIDS-like).
decayHeuristic :: HeuristicState -> HeuristicState
decayHeuristic hSt =
  let factor = activityDec hSt
      newActs = VU.map (* factor) (varActivity hSt)
      newInc  = activityInc hSt / factor
  in hSt { varActivity = newActs, activityInc = newInc }
