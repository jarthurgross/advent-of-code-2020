import Data.List.Split
import Data.List

main = do
  contents <- getContents
  putStrLn $ show $  lines contents)

isHexDigit :: Char -> Bool
isHexDigit c = elem c $ ['0' .. '9'] ++ ['a' .. 'f']

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
