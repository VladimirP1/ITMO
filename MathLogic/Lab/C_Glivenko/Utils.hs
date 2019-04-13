module Utils where
import Data.List
import Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Char
import Data.Maybe

toByteString x = pack (Prelude.map (fromIntegral . ord) x)
fromByteString x = Prelude.map (chr . fromIntegral) (unpack x)



splitBy     :: (Char -> Bool) -> String -> [String]
splitBy p s =  case Data.List.dropWhile p s of
                      "" -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = Data.List.break p s'

splitTurnStile = splitTurnStile' ""
    where
        splitTurnStile' left right
            | "|-" `Data.List.isPrefixOf` right = (left, let _:_:r = right in r)
            | otherwise = let (x:xs)            = right in splitTurnStile' (left ++ [x]) xs

ifM cond a b = cond >>= (\x -> if x then a else b)

isInMap x hm = fromMaybe (-1) (Map.lookup x hm)

lookupFailOver x hm failover = fromMaybe failover (Map.lookup x hm)


