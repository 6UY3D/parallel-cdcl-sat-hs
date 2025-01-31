module Solver.Restart
  ( RestartPolicy(..)
  , RestartState(..)
  , initRestartState
  , shouldRestart
  , onRestart
  ) where

import Solver.Types (DecisionLevel)

data RestartPolicy
  = Luby
  | Glucose
  | Dynamic
  deriving (Show, Eq)

data RestartState = RestartState
  { conflictsSinceRestart :: !Int
  , restartThreshold      :: !Int
  , restartPolicy         :: !RestartPolicy
  }

initRestartState :: RestartPolicy -> RestartState
initRestartState pol = RestartState
  { conflictsSinceRestart = 0
  , restartThreshold = 50   -- initial threshold
  , restartPolicy    = pol
  }

shouldRestart :: RestartState -> Bool
shouldRestart rs =
  conflictsSinceRestart rs >= restartThreshold rs

onRestart :: RestartState -> RestartState
onRestart rs =
  case restartPolicy rs of
    Luby     -> rs { conflictsSinceRestart = 0
                   , restartThreshold = nextLuby (restartThreshold rs) }
    Glucose  -> rs { conflictsSinceRestart = 0
                   , restartThreshold = max 50 (round $ fromIntegral (restartThreshold rs) * 1.1) }
    Dynamic  -> rs { conflictsSinceRestart = 0
                   , restartThreshold = dynamicAdjust (restartThreshold rs) }

nextLuby :: Int -> Int
nextLuby x = x + 50  -- simplistic

dynamicAdjust :: Int -> Int
dynamicAdjust x = x + 20 -- placeholder
