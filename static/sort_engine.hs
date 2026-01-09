import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let numbers = map read args :: [Int]
    print $ sort numbers
