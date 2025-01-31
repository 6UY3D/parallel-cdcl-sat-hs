module Solver.Parser
  ( parseDimacs
  ) where

import Solver.Types (CNF(..), Clause)
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as VU

parseDimacs :: FilePath -> IO (Either String CNF)
parseDimacs path = do
  content <- readFile path
  let ls = lines content
  let filtered = filter (not . isCommentOrEmpty) ls
  case parseHeader filtered of
    Left err -> return (Left err)
    Right (vCount, cCount, rest) ->
      let parsedClauses = parseClauses rest
      in if length parsedClauses /= cCount
         then return $ Left "Clause count mismatch."
         else return $ Right (CNF parsedClauses)

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty s =
  let t = dropWhile isSpace s
  in null t || head t == 'c'

parseHeader :: [String] -> Either String (Int, Int, [String])
parseHeader (x:xs) =
  let ws = words x
  in if length ws == 4 && ws !! 0 == "p" && ws !! 1 == "cnf"
       then Right (read (ws !! 2), read (ws !! 3), xs)
       else Left "Invalid DIMACS header."
parseHeader [] = Left "No header found."

parseClauses :: [String] -> [Clause]
parseClauses [] = []
parseClauses (l:ls) =
  let ints = map read (init (words l))  -- skip trailing 0
  in VU.fromList ints : parseClauses ls
