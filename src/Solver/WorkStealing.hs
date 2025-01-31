{-# LANGUAGE BangPatterns #-}
module Solver.WorkStealing
  ( runWorkStealing
  ) where

import Solver.Types
import Solver.CDCL (cdclSolve)
import Control.Concurrent.Async (Async, async, waitAnyCancel)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.IORef

-- | 'runWorkStealing' will distribute sub-CNFs among threads.
--   In real usage, you might have partial assignments representing each subproblem,
--   rather than wholly separate CNFs.
runWorkStealing
  :: Int     -- ^ numThreads
  -> [CNF]   -- ^ subproblems
  -> Bool    -- ^ verbose
  -> Bool    -- ^ stats
  -> IO SolverResult
runWorkStealing n subs verbose collectStats = do
  queue <- newTQueueIO
  forM_ subs (atomically . writeTQueue queue)
  resultRef <- newIORef Nothing  -- store first SAT result

  let worker = do
        let loop = do
              mbSub <- atomically $ tryReadTQueue queue
              case mbSub of
                Nothing   -> return ()
                Just subC -> do
                  res <- cdclSolve subC verbose collectStats
                  case res of
                    SAT _ -> writeIORef resultRef (Just res)
                    UNSAT -> return ()
                  loop
        loop

  as <- mapM (const $ async worker) [1..n]
  -- Wait for the first SAT or all tasks done
  (done, _) <- waitAnyCancel as
  -- Check final
  mbRes <- readIORef resultRef
  return $ maybe UNSAT id mbRes
