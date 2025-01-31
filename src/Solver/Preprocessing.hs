module Solver.Preprocessing
  ( preprocessCNF
  ) where

import Solver.Types (CNF(..), Clause)
import qualified Data.Vector.Unboxed as VU

-- | Example of simple preprocessing:
--   1. Remove trivially satisfied clauses (x and -x in the same clause).
--   2. Remove subsumed duplicates.
preprocessCNF :: CNF -> Bool -> IO CNF
preprocessCNF (CNF clauses) verbose = do
  let step1 = filter (not . trivialClause) clauses
  let step2 = removeSubsumed step1
  if verbose
    then putStrLn $ "[Preprocessing] " ++ show (length clauses) ++ " clauses => " ++ show (length step2)
    else return ()
  return $ CNF step2

-- | Clause is trivial if it contains x and -x.
trivialClause :: Clause -> Bool
trivialClause c =
  any (\lit -> VU.elem (-lit) c) c

-- | Very naive subsumption check.
removeSubsumed :: [Clause] -> [Clause]
removeSubsumed cls =
  filter (\c -> not (any (isSubsumedBy c) (filter (/= c) cls))) cls

isSubsumedBy :: Clause -> Clause -> Bool
isSubsumedBy c1 c2 = VU.all (`VU.elem` c2) c1
