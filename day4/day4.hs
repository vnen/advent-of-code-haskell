import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.MD5 as MD5

main = do
    input <- return $ C.pack "ckczppom"
    putStrLn $ show (bestNumber input)

bestNumber :: BS.ByteString -> Int
bestNumber input =
    head [x | x <- [1..], check (MD5.hash (concatI input x))]

check :: BS.ByteString -> Bool
check x =
    let (a:b:c:xs) = BS.unpack x
    in a == 0 && b == 0 && c < 16

concatI :: BS.ByteString -> Int -> BS.ByteString
concatI bs i = C.pack $ (C.unpack bs) ++ (show i)

