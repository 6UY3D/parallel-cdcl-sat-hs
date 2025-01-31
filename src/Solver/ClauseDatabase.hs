module Solver.ClauseDatabase
  ( ClauseDB(..)
  , initClauseDB
  , addLearnedClause
  , bumpClauseActivity
  , decayClauseActivities
  , dynamicDeletion
  , getAllClauses
  ) where

import Solver.Types (Clause, ClauseID, ClauseActivity)
import qualified Data.Vector.Unboxed as VU

-- | The 'ClauseDB' holds original and learned clauses together, or separate.
--   For each learned clause, we track an activity for dynamic deletion.
data ClauseDB = ClauseDB
  { originalClauses :: [(ClauseID, Clause)]
  , learnedClauses  :: [(ClauseID, Clause, ClauseActivity)]
  , clauseCounter   :: !ClauseID    -- next ID for newly learned
  , deletionEpoch   :: !Int         -- used to schedule clause deletion
  }

initClauseDB :: [Clause] -> ClauseDB
initClauseDB orig =
  let indexedOrig = zip [0..] orig
      nextId = length orig
  in ClauseDB
     { originalClauses = indexedOrig
     , learnedClauses  = []
     , clauseCounter   = nextId
     , deletionEpoch   = 0
     }

-- | Add a newly learned clause with an initial activity.
addLearnedClause :: ClauseDB -> Clause -> (ClauseDB, ClauseID)
addLearnedClause db c =
  let cid = clauseCounter db
      newLC = (cid, c, 1.0)
      newLearned = newLC : learnedClauses db
  in (db { learnedClauses = newLearned, clauseCounter = cid + 1 }, cid)

-- | Bump the activity of a learned clause whenever it is involved in a conflict.
bumpClauseActivity :: ClauseID -> Double -> ClauseDB -> ClauseDB
bumpClauseActivity cid inc db =
  let bumped = map (\(lid, cl, act) -> if lid == cid then (lid, cl, act + inc) else (lid, cl, act))
                   (learnedClauses db)
  in db { learnedClauses = bumped }

-- | Decay the activity of learned clauses by a factor.
decayClauseActivities :: Double -> ClauseDB -> ClauseDB
decayClauseActivities factor db =
  let decayed = map (\(lid, cl, act) -> (lid, cl, act * factor)) (learnedClauses db)
  in db { learnedClauses = decayed }

-- | Periodically remove clauses whose activity is too low or that are too large (e.g. high LBD).
dynamicDeletion :: ClauseDB -> ClauseDB
dynamicDeletion db =
  let threshold = 0.5
      filtered = filter (\(_,_,act) -> act >= threshold) (learnedClauses db)
      newEpoch = deletionEpoch db + 1
  in db { learnedClauses = filtered, deletionEpoch = newEpoch }

-- | Combine original and learned clauses for usage in watchers, conflict checks, etc.
getAllClauses :: ClauseDB -> [(ClauseID, Clause)]
getAllClauses db =
  originalClauses db ++ map (\(cid, c, _) -> (cid, c)) (learnedClauses db)
