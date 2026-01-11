-- Axioma Mnemosynis: Termodinâmica de Silicium
-- Phylum Algorithmi: Efeito Leidenfrost

module Main where

import Data.List (unfoldr)
import Text.Printf (printf)

-- ╼ Tipagem sagrada da quádrupla
type Solucao = (Double, Double, Double, Double)

-- ╼ O bilhete na garrafa — a verdade geométrica
bilheteFala :: Solucao -> Bool
bilheteFala (x,y,z,w) =
    let somaQuadrados = x*x + y*y + z*z + w*w
        produto       = x*y*z*w
    in abs (somaQuadrados - produto) < 1e-10

-- ╼ Regra do produto (a "derivada estrutural" da malha)
kEstrutural :: Solucao -> Double
kEstrutural (_,y,z,w) = y * z * w

-- ╼ Extrair o primeiro elemento da quádrupla
fst4 :: Solucao -> Double
fst4 (x,_,_,_) = x

-- Coeficiente de Pressão de Vapor (Baseado na diferença de k e x)
pressaoLeidenfrost :: Solucao -> Double
pressaoLeidenfrost s@(x,_,_,_) =
    let k = kEstrutural s
        deltaT = abs (k - 2*x) -- O "superaquecimento" da malha
    in if deltaT < 1e-10 then 1.0 else exp (-1 / deltaT) -- A pressão cresce exponencialmente com a proximidade

-- Salto Leidenfrost: A gota quica sem tocar a placa
saltoLeidenfrost :: Solucao -> Solucao
saltoLeidenfrost s@(x,y,z,w) =
    let k = kEstrutural s
        -- A força de repulsão substitui a álgebra estática
        x' = k - x
    in (x', y, z, w)

-- ╼ Caminho infinito preguiçoso
caminhoInfinito :: Solucao -> [Solucao]
caminhoInfinito = unfoldr (\s -> Just (s, saltoLeidenfrost s))

-- ╼ Ponto de entrada ritualístico
ritual :: IO ()
ritual = do
    let semente = (2.0, 2.0, 2.0, 2.0)

    putStrLn "┌──────────────────────────────────────────────┐"
    putStrLn "│    AXIOMA: EFEITO LEIDENFROST                │"
    putStrLn ("│   Semente inicial: " ++ show semente)
    putStrLn "├──────────────────────────────────────────────┤"
    putStrLn $ "│ Bilhete fala?      " ++ show (bilheteFala semente)
    putStrLn "├──────────────────────────────────────────────┤"

    let caminho = take 10 $ caminhoInfinito semente

    mapM_ (\(i, s) -> do
        let pressao = pressaoLeidenfrost s
            x_val = fst4 s
        printf "│ n=%-2d | x = %-15.10f → Pressão = %.10f │\n" (i::Int) x_val pressao
        ) (zip [0..] caminho)

    putStrLn "└──────────────────────────────────────────────┘"
    putStrLn "A gota dança sobre o calor da malha."

main :: IO ()
main = ritual
