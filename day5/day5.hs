import System.IO
import Data.List

main = do
    input <- readFile "input5.txt"
    putStrLn $ show (nices input)

nices :: String -> Int
nices = length . filter isNice . lines

isNice :: String -> Bool
isNice line = forbidden line && threeVowels line && doubled line

doubled :: String -> Bool
doubled s
    | length s < 2 = False
doubled (x:y:xs) = (x == y) || (doubled (y:xs))

threeVowels x = 3 <= (length $ [c | c <- x, c `elem` "aieou"])
forbidden x = not $ any (flip isInfixOf x) ["ab", "cd", "pq", "xy"]

