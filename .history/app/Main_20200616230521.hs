module Main where
import Data.List
import Lib

main = do
    word <- askWord
    play word
    
