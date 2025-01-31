module Solver.Performance
  ( profileHook
  , timed
  ) where

import System.CPUTime
import Text.Printf

-- | Example: a hook to measure time spent in certain solver phases.
profileHook :: String -> IO a -> IO a
profileHook label action = do
  start <- getCPUTime
  res <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^(12) :: Double)
  putStrLn $ "[PROFILE] " ++ label ++ " took " ++ printf "%.3f" diff ++ "s"
  return res

-- | Another example utility for timing:
timed :: IO a -> IO (a, Double)
timed action = do
  start <- getCPUTime
  res <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / 1e12
  return (res, diff)
