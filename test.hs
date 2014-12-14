import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Environment

solve :: [Char] -> [Char]
solve x = intercalate " " $ reverse $ splitOn " " x

boilerPlate :: [[Char]]
boilerPlate = ["Case #" ++ show n ++ ": " |n <- [1..]]
  
standardOutput :: [[Char]] -> [[Char]]
standardOutput = zipWith (++) boilerPlate

main :: IO ()
main = do
  (f:_) <- getArgs
  file <- readFile f
  let puzzles   = lines file
      solutions = standardOutput $ map solve puzzles
  putStrLn $ unlines $ solutions

  undefined

