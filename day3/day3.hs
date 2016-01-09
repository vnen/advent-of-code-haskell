import System.IO (readFile)
import Data.List (nub)

main = do
    input <- readFile "input3.txt"
    putStrLn $ show (houses input)

houses :: String -> Int
houses = length . nub . foldl deliver [(0,0)]

deliver :: [(Int, Int)] -> Char -> [(Int, Int)]
deliver ((x,y):acc) c =
    case c of
        '>' -> (x+1,y):(x,y):acc
        '<' -> (x-1,y):(x,y):acc
        '^' -> (x,y-1):(x,y):acc
        'v' -> (x,y+1):(x,y):acc

