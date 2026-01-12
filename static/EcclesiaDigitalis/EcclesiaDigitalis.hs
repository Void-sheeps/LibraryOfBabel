module EcclesiaDigitalis where

-- Placeholder for the EcclesiaDigitalis module.
-- The user has not provided the source code for this module.

data Tempo = SetembroEterno | RealidadeLinear Int

instance Show Tempo where
    show SetembroEterno = "∞"
    show (RealidadeLinear n) = show n

main :: IO ()
main = putStrLn "EcclesiaDigitalis main function placeholder"
