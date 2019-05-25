module Main where

main = do
    x <- getLine
    y <- getLine
    let xX = read x :: Integer
    let yY = read y :: Integer
    putStrLn $ show (xX*yY)
