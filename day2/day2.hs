import System.IO
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input2.txt"
    putStrLn $ show (totalWrapping input)

totalWrapping :: String -> Int
totalWrapping input =
    sum $ map (wrapping . dimensions) (lines input)

dimensions :: String -> (Int, Int, Int)
dimensions line = 
    let (w:l:h:_) = map read $ splitOn "x" line
    in (w, l, h)

wrapping :: (Int, Int, Int) -> Int
wrapping (l, w, h) =
    let minSide = min (l*w) $ min (l*h) (w*h)
    in 2*l*w + 2*w*h + 2*h*l + minSide

