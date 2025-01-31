{-# LANGUAGE BangPatterns #-}
module Solver.Parallel
  ( parallelSolve
  ) where

import Solver.Types
import Solver.CDCL (cdclSolve)
import Control.Concurrent.Async (mapConcurrently)
import Solver.WorkStealing (runWorkStealing)

parallelSolve
  :: CNF
  -> SolverMode
  -> Int
  -> Bool
  -> Bool
  -> IO SolverResult
parallelSolve cnf mode threads verbose stats =
  case mode of
    Single    -> cdclSolve cnf verbose stats
    Portfolio -> portfolioMode cnf threads verbose stats
    Split     -> splitMode cnf threads verbose stats

portfolioMode :: CNF -> Int -> Bool -> Bool -> IO SolverResult
portfolioMode cnf n verbose collectStats = do
  results <- mapConcurrently (\_ -> cdclSolve cnf verbose collectStats) [1..n]
  return $ combineResults results

splitMode :: CNF -> Int -> Bool -> Bool -> IO SolverResult
splitMode (CNF clauses) n verbose collectStats = do
  -- Placeholder splitting approach: chunk clauses equally.
  let chunkSize = max 1 (length clauses `div` n)
      subFormulas = chunk chunkSize clauses
      subCNFs = map CNF subFormulas
  runWorkStealing n subCNFs verbose collectStats

combineResults :: [SolverResult] -> SolverResult
combineResults rs =
  case filter isSat rs of
    (sat:_) -> sat
    []      -> UNSAT
  where
    isSat (SAT _) = True
    isSat _       = False

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (a,b) = splitAt n xs in a : chunk n b
