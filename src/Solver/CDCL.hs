{-# LANGUAGE BangPatterns #-}
module Solver.CDCL
  ( cdclSolve
  ) where

import Solver.Types
  ( CNF(..)
  , SolverResult(..)
  , DecisionLevel
  , AssignmentMap
  , Reason
  )
import Solver.ClauseDatabase
import Solver.WatchList
import Solver.ConflictAnalysis
import Solver.Heuristic
import Solver.Restart
import qualified Data.Vector.Unboxed as VU
import Data.Maybe (isJust, fromJust, isNothing)

--------------------------------------------------------------------------------
-- CDCL Solver
--------------------------------------------------------------------------------

cdclSolve
  :: CNF       -- ^ preprocessed CNF
  -> Bool      -- ^ verbose
  -> Bool      -- ^ collect stats
  -> IO SolverResult
cdclSolve (CNF clauses) verbose collectStats = do
  if verbose then putStrLn "[CDCL] Starting solver..." else return ()

  let dbInit       = initClauseDB clauses
  let wListInit    = buildWatchList clauses
  let numVars      = maxVar clauses
  let assignInit   = VU.replicate (numVars + 1) Nothing
  let reasonInit   = replicate (numVars + 1) Nothing
  let hStateInit   = initHeuristic numVars VSIDS
  let restartState = initRestartState Luby  -- or Glucose / Dynamic

  let st = SolverState
           { clauseDB       = dbInit
           , watchList      = wListInit
           , assignmentMap  = assignInit
           , reasonMap      = reasonInit
           , decisionLevel  = 0
           , assignmentStack = []
           , heuristicState = hStateInit
           , restartState   = restartState
           , conflictCount  = 0
           }

  finalSt <- cdclLoop st
  let finalAssign = gatherAssignments (assignmentMap finalSt)
  if allAssigned (assignmentMap finalSt)
    then return (SAT finalAssign)
    else return UNSAT

--------------------------------------------------------------------------------
-- Internal State
--------------------------------------------------------------------------------

data SolverState = SolverState
  { clauseDB       :: !ClauseDB
  , watchList      :: !WatchList
  , assignmentMap  :: !AssignmentMap
  , reasonMap      :: ![Reason]         -- reasonMap[var] = clauseID that implied it
  , decisionLevel  :: !DecisionLevel
  , assignmentStack :: ![Int]           -- the assignment order of variables
  , heuristicState :: !HeuristicState
  , restartState   :: !RestartState
  , conflictCount  :: !Int
  }

--------------------------------------------------------------------------------
-- Main Loop
--------------------------------------------------------------------------------

cdclLoop :: SolverState -> IO SolverState
cdclLoop st =
  if allAssigned (assignmentMap st)
    then return st
    else do
      -- 1. Check for restart
      let rState = restartState st
      if shouldRestart rState
        then do
          let st' = performRestart st
          cdclLoop st'
        else return ()

      -- 2. Decide
      (varToAssign, st1) <- decideNextVar st
      if varToAssign < 0
        then return st1  -- no unassigned => done
        else do
          let st2 = assignVar st1 varToAssign True (decisionLevel st1 + 1) Nothing
          -- 3. Propagate
          (mConflict, st3) <- propagate st2
          case mConflict of
            Nothing -> cdclLoop st3
            Just conflictCid -> do
              -- 4. Conflict -> analyze
              st4 <- conflict st3 conflictCid
              if allAssigned (assignmentMap st4)
                then return st4
                else cdclLoop st4

--------------------------------------------------------------------------------
-- Decide (Pick Variable)
--------------------------------------------------------------------------------

decideNextVar :: SolverState -> IO (Int, SolverState)
decideNextVar st = do
  v <- selectVariable (heuristicState st) (assignmentMap st)
  return (v, st)

--------------------------------------------------------------------------------
-- Propagate
--------------------------------------------------------------------------------

propagate :: SolverState -> IO (Maybe Int, SolverState)
propagate st = do
  -- In a real solver, you'd maintain a queue of assigned literals for propagation.
  -- For each newly assigned literal, update watchers, find unit clauses, etc.
  -- If conflict arises, return the conflicting clause ID.
  return (Nothing, st)

--------------------------------------------------------------------------------
-- Conflict Handling
--------------------------------------------------------------------------------

conflict :: SolverState -> Int -> IO SolverState
conflict st conflictCid = do
  let cDB = clauseDB st
  let (cid, conflictClause, _) = head [ (cid', cl, act) | (cid', cl, act) <- learnedClauses cDB, cid' == conflictCid ]
  let varLevelFn v = 0  -- TODO
  let varReasonFn v = reasonMap st !! v
  let stk = assignmentStack st
  let dl = decisionLevel st

  let (newClause, backjumpDL, involvedVars) =
        analyzeConflictOneUIP conflictClause varLevelFn varReasonFn stk dl

  -- Bump activity for involved vars
  let hState' = updateOnConflict (map abs involvedVars) (heuristicState st)

  -- Add learned clause
  let (cDB2, newCid) = addLearnedClause cDB newClause

  -- Decide backjump
  let st' = backjump st backjumpDL

  -- Watch new clause
  let wList' = addClauseWatch (watchList st') newCid newClause

  -- Possibly dynamic deletion
  let cDB3 = if conflictCount st' `mod` 50 == 0
             then dynamicDeletion cDB2
             else cDB2

  -- Update conflict count, reasonMap, etc.
  let st'' = st' { clauseDB       = cDB3
                 , watchList      = wList'
                 , heuristicState = hState'
                 , conflictCount  = conflictCount st' + 1
                 }
  return st''

--------------------------------------------------------------------------------
-- Backjump
--------------------------------------------------------------------------------

backjump :: SolverState -> DecisionLevel -> SolverState
backjump st bdl =
  -- Unassign variables with decision level > bdl
  let asg = assignmentMap st
      rm  = reasonMap st
      stck = assignmentStack st
      filtered = filter (\v -> actualLevel v <= bdl) stck
      newAsg = revertAssignments asg stck bdl
      newLevel = min (decisionLevel st) bdl
  in st { assignmentMap  = newAsg
        , assignmentStack = filtered
        , decisionLevel  = newLevel
        }

actualLevel :: Int -> DecisionLevel
actualLevel v = 0 -- placeholder

revertAssignments :: AssignmentMap -> [Int] -> DecisionLevel -> AssignmentMap
revertAssignments asg assignedList bdl = asg
  -- For each var in assignedList with level > bdl, revert to Nothing

--------------------------------------------------------------------------------
-- Restart
--------------------------------------------------------------------------------

performRestart :: SolverState -> SolverState
performRestart st =
  let rSt = onRestart (restartState st)
      newAsg = VU.map (const Nothing) (assignmentMap st)
      newStack = []
      newLvl   = 0
  in st { assignmentMap = newAsg
        , assignmentStack = newStack
        , decisionLevel = newLvl
        , restartState = rSt
        }

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

maxVar :: [Clause] -> Int
maxVar cls =
  let maxC c = if VU.null c then 0 else VU.maximum (VU.map abs c)
  in maximum (map maxC cls)

allAssigned :: AssignmentMap -> Bool
allAssigned asg =
  all isJust (VU.toList (VU.tail asg))  -- ignoring index 0

gatherAssignments :: AssignmentMap -> [(Int, Bool)]
gatherAssignments asg =
  [ (i, val) | (i, Just val) <- zip [0..] (VU.toList asg), i /= 0 ]

buildWatchList :: [Clause] -> WatchList
buildWatchList clauses =
  let wl = initWatchList
  in foldl (\acc (i,cl) -> addClauseWatch acc i cl) wl (zip [0..] clauses)
