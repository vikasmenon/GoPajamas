import Control.Parallel.Strategies (parMap, runEval, rpar)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Environment

solve :: [Char] -> [Char]
solve x = intercalate " " $ reverse $ splitOn " " x

boilerPlate :: [[Char]]
boilerPlate = ["Case #" ++ show n ++ ": " |n <- [1..]]
  
standardOutput :: [[Char]] -> [[Char]]
standardOutput = zipWith (++) boilerPlate

parallel_solve puzzles = parMap rpar solve puzzles

main :: IO ()
main = do
  (f:_) <- getArgs
  file <- readFile f
  let puzzles   = tail $ lines file
      solutions = standardOutput $ parallel_solve puzzles 
  putStrLn $ unlines $ solutions
