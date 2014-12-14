import Data.List (intercalate)
import Data.List.Split (splitOn)

solve x = intercalate " " $ reverse $ splitOn " " x


