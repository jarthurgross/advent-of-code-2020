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
  putStrLn $ show $ filter (not . (uncurry isPairSum)) $ preambleNumbers 25 $ map readInt $ lines contents

readInt :: String -> Int
readInt = read

preambleNumbers :: Int -> [Int] -> [([Int], Int)]
preambleNumbers l ns = scanl' (\((x:xs), y) y' -> (xs ++ [y], y'))  (preamble, n') ns'
  where (preamble, (n':ns')) = splitAt l ns

isPairSum :: [Int] -> Int -> Bool
isPairSum [] _ = False
isPairSum (x:xs) y = (y - x) `elem` xs || isPairSum xs y

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
