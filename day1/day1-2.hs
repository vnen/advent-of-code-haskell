import System.IO
import Data.List

main :: IO ()
main = do
    input <- readFile "input1.txt"
    putStrLn $ show (basement input)

basement :: String -> Int
basement x =
    let index = elemIndex (-1) $ scanl (\acc i -> if i == '(' then acc + 1 else acc - 1) 0 x
    in case index of
        Just n -> n
        Nothing -> 0
