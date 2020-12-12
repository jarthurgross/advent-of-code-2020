import Data.List
import Data.List.Split
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

main = do
  contents <- getContents
  putStrLn $ show $ manhattanDist $ snd $ foldl' makeMove ((10, 1), (0, 0)) $ map splitCharInt $ lines contents

makeMove :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
makeMove ((x', y'), (x, y)) (c, a)
  | c == 'N' = ((x', y' + a), (x, y))
  | c == 'S' = ((x', y' - a), (x, y))
  | c == 'E' = ((x' + a, y'), (x, y))
  | c == 'W' = ((x' - a, y'), (x, y))
  | c == 'R' = (rotate a (x', y'), (x, y))
  | c == 'L' = (rotate ((-a) `mod` 360) (x', y'), (x, y))
  | c == 'F' = ((x', y'), (x + a * x', y + a * y'))
  | otherwise = ((x', y'), (x, y))
  where rotate d (x, y)
          | d == 0 = (x, y)
          | d == 90 = (y, -x)
          | d == 180 = (-x, -y)
          | d == 270 = (-y, x)
          | otherwise = (x, y)

splitCharInt :: String -> (Char, Int)
splitCharInt s = (head s, readInt $ tail s)

manhattanDist :: (Int, Int) -> Int
manhattanDist (x, y) = (abs x) + (abs y)

readInt :: String -> Int
readInt = read

takeDigits :: String -> (String, String)
takeDigits "" = ("", "")
takeDigits (c:cs) = if c `elem` ['0' .. '9'] then (c:ds, ls) else ("", c:cs)
  where (ds, ls) = takeDigits cs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
