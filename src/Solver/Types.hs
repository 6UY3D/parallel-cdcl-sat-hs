{-# LANGUAGE BangPatterns #-}
module Solver.Types
  ( Literal
  , Clause
  , CNF(..)
  , SolverResult(..)
  , SolverMode(..)
  , AssignmentMap
  , DecisionLevel
  , VarAssignment
  , Reason
  , ClauseID
  , HeuristicType(..)
  , ClauseActivity
  ) where

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- Basic Types
--------------------------------------------------------------------------------

-- | A negative Literal is a negated variable. For example, -3 represents x3 = False.
type Literal = Int

-- | Each clause is a vector of literals (for performance).
type Clause = VU.Vector Literal

-- | A CNF is simply a list of clauses.
newtype CNF = CNF { unCNF :: [Clause] }
  deriving (Show, Eq)

data SolverResult
  = SAT !VarAssignment         -- (var, val) pairs
  | UNSAT
  deriving (Show, Eq)

data SolverMode
  = Single
  | Portfolio
  | Split
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Assignments and Levels
--------------------------------------------------------------------------------

-- | A vector that holds Maybe True/False for each variable index (0 is unused).
type AssignmentMap = VU.Vector (Maybe Bool)

-- | CDCL Decision level.
type DecisionLevel = Int

-- | Final assignment for printing or returning to user.
type VarAssignment = [(Int, Bool)]

-- | 'Reason' is the clause ID that implied a literal, or Nothing if it was a decision.
type Reason = Maybe ClauseID

-- | A unique ID for each clause in the solver's database.
type ClauseID = Int

--------------------------------------------------------------------------------
-- Heuristics
--------------------------------------------------------------------------------

data HeuristicType
  = VSIDS
  | LRB
  | CHB
  deriving (Show, Eq)

-- | A learned clause (or even original clause) may have an 'activity' score for dynamic deletion.
type ClauseActivity = Double
