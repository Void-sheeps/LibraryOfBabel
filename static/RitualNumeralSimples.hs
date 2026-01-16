-- Ritual Numeral Simples - Versão "Platô Edition"
-- log(x+1) * 42 → floor → mod 255
-- Mostra como números próximos caem no mesmo byte por compressão logarítmica

module Main where

import Text.Printf (printf)

-- Função principal: input → byte final (0-254)
f :: Double -> Int
f x = floor (log (abs x + 1) * 42) `mod` 255

-- Formata uma linha da tabela
printRow :: Double -> IO ()
printRow x = do
    let ln = log (abs x + 1)
        prod = ln * 42
        fl = floor prod :: Int
        res = fl `mod` 255
    printf "%16.0f | %9.6f | %10.4f | %6d | %3d\n" x ln prod fl res

main :: IO ()
main = do
    putStrLn "\nRitual: log(|x|+1) * 42 → floor → mod 255"
    putStrLn "──────────────────────────────────────────────────────"
    putStrLn "               x |   ln(x+1) |        *42 |  floor | mod"
    putStrLn "──────────────────────────────────────────────────────"
    mapM_ printRow inputs
    putStrLn "──────────────────────────────────────────────────────"
    putStrLn "Observação:"
    putStrLn " - Muitos valores próximos caem no mesmo byte (platô)"
    putStrLn " - Só mudanças maiores empurram o floor pro próximo inteiro"
    putStrLn " - Isso é a 'compressão' do log em ação\n"
    where
        inputs :: [Double]
        inputs = [
            1,
            1000,
            1000000,
            1768525786088,
            1768525786089,
            1768525787000,
            1768525800000,
            1768526000000,
            1770000000000,
            2000000000000,
            10000000000000,  -- 10¹³
            1e15             -- 10¹⁵
            ]
