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

parallel_solve func puzzles = parMap rpar func puzzles

get_puzzles fileHandle = tail $ lines fileHandle


fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

lst = map (+5) [30, 33, 32, 30, 3, 0, 1]

serial_order = map fib lst 
test_order = parMap rpar fib lst
-- test should assert serial_order == test_order irrespective of number of cores

main :: IO ()
main = do
  (f:_) <- getArgs
  file <- readFile f   
  --putStrLn $ unlines $ standardOutput $ parallel_solve solve ( get_puzzles file) --unwords
  --putStrLn $ unlines $ standardOutput $ parallel_solve format_solve ( get_puzzles file) --welcome to code jam
  putStrLn $ unlines $ standardOutput $ parallel_solve format_solve ( get_puzzles file) -- alien solve

welcome_solve sub_x [] = 0
welcome_solve (sub_x: []) str_xs = length ( filter (\x -> x==sub_x) str_xs ) 
welcome_solve (sub_x:sub_xs) (str_x:str_xs) = if sub_x==str_x
                                                 then (welcome_solve sub_xs str_xs)  + (welcome_solve (sub_x:sub_xs) str_xs) 
                                                 else welcome_solve (sub_x:sub_xs) str_xs

getLast4 (w:x:y:z: []) = [w,x,y,z]
getLast4 (x:xs) = getLast4 xs
format_solve problem 
             | solution < 1000 = getLast4 $ show $ 10000 + solution
             | otherwise  = getLast4 $ show solution
             where solution = welcome_solve "welcome to code jam" problem
             
----- Alien solve
_alien_solve word_set input_string = filter (\x -> Set.member x word_set) $ get_words input_string

splitOnFirst string_data char = _splitOnFirst [] string_data char
_splitOnFirst dt [] char = (dt,[])
_splitOnFirst dt (x:xs) char
              | x == char = (dt, xs)
              | otherwise = _splitOnFirst (dt ++ [x]) xs char

get_words [] = []
get_words (x:xs)
       | x=='(' = mapLettersWords (get_words remainder) letters
       | otherwise = mapLetterWords (get_words xs) x
       where (letters,remainder) = splitOnFirst xs ')'


mapLetter letter [] = [letter]
mapLetter letter word   = letter:word
mapLetterWords [] letter  = [[letter]] 
mapLetterWords words letter = map (mapLetter letter) words
mapLettersWords words letters = [ y | x <- (map (mapLetterWords words) letters), y<-x ]
