import System.IO
import Data.List

main = do
    input <- readFile "input5.txt"
    putStrLn $ show (nices input)

nices :: String -> Int
nices = length . filter isNice . lines

isNice :: String -> Bool
isNice line = twoPairs line && repeated line

twoPairs :: String -> Bool
twoPairs s
    | length s < 4 = False
twoPairs (x:y:xs) = ([x,y] `isInfixOf` xs) || (twoPairs (y:xs))

repeated :: String -> Bool
repeated s
    | length s < 3 = False
repeated (a:b:c:xs) = (a == c) || (repeated (b:c:xs))

