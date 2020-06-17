module Main where
import Lib

main :: IO ()
main = do
    word <- askWord
    play word
    
