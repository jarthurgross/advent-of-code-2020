import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ foldr (\ln -> \n -> if validateLine ln then n + 1 else n) 0 $ lines contents

parseLine :: String -> (Int, Int, Char, String)
parseLine s = (a, b, head char, pswd)
  where
    rng:char:pswd:[] = words s
    a:b:[] = map (\x -> read x :: Int) $ splitOn "-" rng

validateLine :: String -> Bool
validateLine s = a <= charCount && charCount <= b
  where
    (a, b, char, pswd) = parseLine s
    charCount = count char pswd

count :: Char -> String -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0
