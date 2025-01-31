module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure)
import Solver.Types (SolverMode(..), SolverResult(..))
import Solver.Parser (parseDimacs)
import Solver.Preprocessing (preprocessCNF)
import Solver.CDCL (cdclSolve)
import Solver.Parallel (parallelSolve)

data Options = Options
  { optFile    :: FilePath
  , optMode    :: SolverMode
  , optThreads :: Int
  , optVerbose :: Bool
  , optStats   :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optFile    = ""
  , optMode    = Single
  , optThreads = 1
  , optVerbose = False
  , optStats   = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ ...
  ]

main :: IO ()
main = do
  args <- getArgs
  -- parse options, etc.
  let opts = ...  -- (omitted for brevity)
  if null (optFile opts)
    then putStrLn "No input file" >> exitFailure
    else do
      ecnf <- parseDimacs (optFile opts)
      case ecnf of
        Left err -> putStrLn ("Error: " ++ err) >> exitFailure
        Right cnf -> do
          cnfPP <- preprocessCNF cnf (optVerbose opts)
          res <- case optMode opts of
                   Single    -> cdclSolve cnfPP (optVerbose opts) (optStats opts)
                   Portfolio -> parallelSolve cnfPP Portfolio (optThreads opts) (optVerbose opts) (optStats opts)
                   Split     -> parallelSolve cnfPP Split (optThreads opts) (optVerbose opts) (optStats opts)
          putStrLn $ "Solver Result: " ++ show res
