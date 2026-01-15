module Main where

main :: IO ()
main = putStrLn $ unlines $ replicate 16 $ replicate 16 'S'
