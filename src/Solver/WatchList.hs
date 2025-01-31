{-# LANGUAGE BangPatterns #-}
module Solver.WatchList
  ( WatchList(..)
  , initWatchList
  , addClauseWatch
  , updateWatches
  ) where

import Solver.Types (Clause, Literal, ClauseID)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

-- | A map of literal -> list of (clauseID, the "other" watched literal).
--   Real implementations store more nuanced info to handle watch transitions.
data WatchList = WatchList
  { watchers :: M.Map Literal [(ClauseID, Literal)]
  }

initWatchList :: WatchList
initWatchList = WatchList M.empty

-- | Add watchers for a clause. Typically, watch the first two literals.
addClauseWatch :: WatchList -> ClauseID -> Clause -> WatchList
addClauseWatch wl cid clause
  | VU.length clause < 2 = wl
  | otherwise =
      let l1 = clause VU.! 0
          l2 = clause VU.! 1
      in watchLiteral (watchLiteral wl cid l1 l2) cid l2 l1

watchLiteral :: WatchList -> ClauseID -> Literal -> Literal -> WatchList
watchLiteral (WatchList w) cid lit other =
  let old = M.findWithDefault [] lit w
      new = (cid, other) : old
  in WatchList (M.insert lit new w)

-- | 'updateWatches' after a literal assignment changes. We look at watchers
--   for the literal that became false, try to find another literal to watch
--   in the clause, etc.
updateWatches
  :: WatchList
  -> (ClauseID -> Clause)  -- function to retrieve a clause by ID
  -> Literal               -- literal that was just assigned false
  -> WatchList
updateWatches wl getClause assignedFalseLit =
  let (WatchList w) = wl
      watchedList = M.findWithDefault [] assignedFalseLit w
      -- For each clause in 'watchedList', try to "move" the watch
      updatedPairs = map (updateOne getClause assignedFalseLit) watchedList
      newW = foldr (mergeWatch assignedFalseLit) w updatedPairs
  in WatchList newW

-- | For each (clauseID, otherLit), we check if 'otherLit' is satisfied or unassigned.
--   If it's satisfied or unassigned, do nothing. Otherwise, find a new literal to watch.
updateOne
  :: (ClauseID -> Clause)
  -> Literal
  -> (ClauseID, Literal)
  -> (ClauseID, Literal, Maybe Literal)  -- (cid, oldWatched, maybeNewLiteral)
updateOne getClause assignedFalseLit (cid, otherLit) =
  let clause = getClause cid
  in (cid, otherLit, findNewLit clause assignedFalseLit otherLit)

-- | Find a new literal in the clause to watch, or Nothing if none found (clause might be unit).
findNewLit :: Clause -> Literal -> Literal -> Maybe Literal
findNewLit clause assignedFalseLit otherLit =
  -- Real implementation: scan the clause for a literal that is not assigned false.
  -- If found, return it; otherwise, Nothing => unit or conflict.
  Just otherLit  -- placeholder

-- | Merge the updated watchers back into the map. If 'maybeNewLit' is Nothing,
--   we can't watch anything else => clause is possibly unit or conflicting.
mergeWatch
  :: Literal
  -> (ClauseID, Literal, Maybe Literal)
  -> M.Map Literal [(ClauseID, Literal)]
  -> M.Map Literal [(ClauseID, Literal)]
mergeWatch oldLit (cid, oldWatched, mbNew) wMap =
  let oldList = M.findWithDefault [] oldLit wMap
      filtered = filter (\(c,_) -> c /= cid) oldList
      wMap' = if null filtered
                 then M.delete oldLit wMap
                 else M.insert oldLit filtered wMap
  in case mbNew of
       Nothing -> wMap' -- Clause may be unit or in conflict
       Just newLit ->
         -- Insert into watchers for newLit
         let nlList = M.findWithDefault [] newLit wMap'
         in M.insert newLit ((cid, oldWatched) : nlList) wMap'
