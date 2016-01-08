import System.IO
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input2.txt"
    putStrLn $ show (totalRibbon input)

totalRibbon :: String -> Int
totalRibbon input =
    sum $ map (ribbon . dimensions) (lines input)

dimensions :: String -> (Int, Int, Int)
dimensions line = 
    let p = map read $ splitOn "x" line
    in (p !! 0, p !! 1, p !! 2)

ribbon :: (Int, Int, Int) -> Int
ribbon (l, w, h) =
    let minPer = min (per l w) $ min (per l h) (per w h)
    in minPer + l*w*h

per :: Int -> Int -> Int
per h l = 2*h + 2*l

