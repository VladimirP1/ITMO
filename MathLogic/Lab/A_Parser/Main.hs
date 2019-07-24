module Main where
import Parser
import Tokeniser

main :: IO ()
main = do
    fmap (show . parse . tokenise) getLine >>= putStrLn
