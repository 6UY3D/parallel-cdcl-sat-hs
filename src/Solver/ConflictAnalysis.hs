{-# LANGUAGE BangPatterns #-}
module Solver.ConflictAnalysis
  ( analyzeConflictOneUIP
  ) where

import Solver.Types
  ( Clause
  , DecisionLevel
  , Reason
  )

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- 1-UIP Conflict Analysis
--------------------------------------------------------------------------------

{-|
  'analyzeConflictOneUIP' receives:
   - the conflicting clause
   - arrays or maps that let us find each variable's decision level and reason
   - a stack (or list) of the assignment order
   - the current decision level

  It returns:
   - the newly learned clause
   - the backjump level
   - the set of variables in the learned clause (for activity update, etc.)
-}
analyzeConflictOneUIP
  :: Clause                  -- ^ Conflict clause
  -> (Int -> DecisionLevel)  -- ^ varLevel function
  -> (Int -> Reason)         -- ^ varReason function
  -> [Int]                   -- ^ assignmentStack (from most recent to oldest)
  -> DecisionLevel           -- ^ current decision level
  -> (Clause, DecisionLevel, [Int])
analyzeConflictOneUIP conflictClause varLevel varReason assignmentStack currentDL =
  let
    -- Pseudocode for actual 1-UIP:
    -- 1. Count how many literals in 'conflictClause' are at the current DL.
    -- 2. While more than 1 literal at the current DL, pick the most recently assigned literal
    --    at current DL from the assignment stack, look up its reason clause, and merge it
    --    into the conflictClause (resolvent).
    -- 3. Decrement the count of current level literals. Repeat until only 1 remains.
    -- 4. The result is the learned clause. The backjump level is the highest
    --    level of any other literal in that clause.

    -- Placeholder returning the same conflictClause, with backjump level 0:
    backjumpLevel = 0
    learnedVars   = VU.toList conflictClause
  in (conflictClause, backjumpLevel, map abs learnedVars)
