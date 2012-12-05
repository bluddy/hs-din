module Main where

import Loader

main = do
    map <- fullFileMap
    putStrLn $ show map
    
