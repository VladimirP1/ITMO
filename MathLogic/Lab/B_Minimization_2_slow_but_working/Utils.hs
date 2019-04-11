module Utils where
import Data.Array
import Data.List
import Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import Data.Char

toByteString x = pack (Prelude.map (fromIntegral . ord) x)
fromByteString x = Prelude.map (chr . fromIntegral) (unpack x)



splitBy     :: (Char -> Bool) -> String -> [String]
splitBy p s =  case Data.List.dropWhile p s of
                      "" -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = Data.List.break p s'

splitTurnStile left = splitTurnStile' "" left
    where
        splitTurnStile' left right 
            | Data.List.isPrefixOf "|-" right = (left, let _:_:r = right in r)
            | otherwise = let (x:xs) = right in splitTurnStile' (left ++ [x]) xs

ifM cond a b = cond >>= (\x -> if x then a else b)

isInMap x hm = case Map.lookup x hm of
    Just x -> x
    Nothing -> -1

lookupFailOver x hm failover = case Map.lookup x hm of
    Just x -> x
    Nothing -> failover

aToList x = listArray (1, Data.List.length x) x
