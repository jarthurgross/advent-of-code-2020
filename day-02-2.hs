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
validateLine s = (char == pswd !! (a - 1)) `xor` (char == pswd !! (b - 1))
  where
    (a, b, char, pswd) = parseLine s

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
