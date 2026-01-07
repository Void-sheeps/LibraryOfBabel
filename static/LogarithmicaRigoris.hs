-- |
-- Module      : LogarithmicaRigoris
-- Description : Diferenciação entre Proxy e Pipeline
-- Principle   : Logos khōris Pathous - Adição vs Translação
-- |

module Main where

-- | Definição Taxonômica
data EstiloProcesso = Proxy (Double -> Double) | Pipeline [Double -> Double]

-- | Execução via Tabela Logarítmica
-- O Proxy apenas aplica a base; o Pipeline soma os efeitos.
processar :: EstiloProcesso -> Double -> Double
processar (Proxy f) x = f x
processar (Pipeline fs) x = foldl (\acc f -> f acc) x fs

-- | Exemplo de Uso: White Crash -> Nash Equilibrium
main :: IO ()
main = do
    let inputX = 16.0 -- Símbolo Hexadecimal
    let logProxy = Proxy (\x -> logBase 10 x)
    let pipe = Pipeline [(\x -> x * 2), (\x -> x + 1)]

    putStrLn "--- TABULA RETENTIVA: PROXY VS PIPELINE ---"
    putStrLn $ "Proxy Result (Translação): " ++ show (processar logProxy inputX)
    putStrLn $ "Pipeline Result (Soma): " ++ show (processar pipe inputX)

    putStrLn "[STATUS]: F1+Esc para colapsar a régua de cálculo."
