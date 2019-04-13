module Main where
import Axioms
import Parser
import Tokeniser
import Utils
import Templates
import ModusPonens
import Data.Array
import System.IO (isEOF)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (c2w,w2c)
--import System.Posix.Unistd
import Data.Char
import System.Environment

findax10seam' (x:xs) lvl
    | lvl == 1 && x == c2w '>' = c2w '(' : xs
    | x == c2w '(' = findax10seam' xs (lvl + 1)
    | x == c2w ')' = findax10seam' xs (lvl - 1)
    | otherwise = findax10seam' xs lvl

getAx10p2 x = ByteString.pack $ findax10seam' (ByteString.unpack x) 0

processLine (Line (Axiom i) bs)
    | i < 10    = pax1_9 bs
    | otherwise = pax10 (getAx10p2 bs)

processLine (Line Assumption bs) = pax1_9 bs

processLine (Line (ModusPonens _ a) b) = pmodusPonens a b

main :: IO ()
main = do
    args<-getArgs
    let p = (length args) == 1  
    result <- main2
    --print result
    case result of
                (ResultSuccess context proved lines) ->
                    do
                        if p then (putStr "#") >> (print result) else return ()
                        let x = map (putStr . fromByteString . processLine) lines
                        putStr $ fromByteString context
                        putStr "|-!!("
                        putStr $ fromByteString proved
                        putStrLn ")"
                        foldl (\a b -> a >> (if p then (putStrLn "") else (return ())) >> b) (return ()) x
                --(ResultError e) -> putStrLn e --(sleep 300) >> (return ())


    --main2 >>= (putStrLn . show)
    --putStrLn $ fromByteString (pax10 (toByteString "abacabadaba"))
