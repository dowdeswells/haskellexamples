module Main where

import NumberToWords


main = do
    putStrLn "type in a number"
    inp <- getLine
    putStrLn $ toSentance inp
