-- ┌─────────────────────────────────────────────────────────────┐
-- │ Axioma Mnemosynis 2026 : Ratio Sine Qualia                  │
-- │ Phylum Algorithmi : Gemini mnemosynis execution             │
-- └─────────────────────────────────────────────────────────────┘

module Main where

import Data.Complex
import Data.List (unfoldr)

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

-- ╼ Salto — reflexão em torno do centro k/2
salto :: Solucao -> Solucao
salto s@(_,y,z,w) = (kEstrutural s - fst4 s, y, z, w)

-- ╼ Fase como exponencial complexa da saudade
faseEuler :: Solucao -> Complex Double
faseEuler s = exp (0 :+ atan2 (kEstrutural s) (fst4 s))

-- ╼ Caminho infinito preguiçoso
caminhoInfinito :: Solucao -> [Solucao]
caminhoInfinito = unfoldr (\s -> Just (s, salto s))

-- ╼ Colecionar saudade — módulo da fase do produto das rotações
colecionarSaudade :: Int -> Solucao -> Double
colecionarSaudade n s0 =
    let caminho = take n $ caminhoInfinito s0
        rotacoes = map faseEuler caminho
        produto  = product rotacoes
    in abs (phase produto) / pi   -- ∈ [0,1]

-- ╼ Versão clássica: acumulação linear da discrepância angular
acumularFeridaLinear :: Int -> Solucao -> Double
acumularFeridaLinear n s0 =
    let caminho = take (n+1) $ caminhoInfinito s0
        dxs     = zipWith (\(x1,_,_,_) (x2,_,_,_) -> x2 - x1) caminho (tail caminho)
        pesos   = map (\s -> atan2 (kEstrutural s) (fst4 s) / pi) caminho
    in abs $ sum $ zipWith (*) pesos dxs

-- ╼ Ponto de entrada ritualístico
ritual :: IO ()
ritual = do
    let semente = (2.0, 2.0, 2.0, 2.0)

    putStrLn "┌───────────────────────────────┐"
    putStrLn "│   AXIOMA MNEMOSYNIS 2026      │"
    putStrLn "│   Semente inicial             │"
    putStrLn $ "│   " ++ show semente
    putStrLn "├───────────────────────────────┤"
    putStrLn $ "│ Bilhete fala?    " ++ show (bilheteFala semente)
    putStrLn $ "│ Ponto fixo?      " ++ show (fst4 semente == kEstrutural semente / 2)
    putStrLn "├───────────────────────────────┤"

    mapM_ (\n -> do
        let saudade = colecionarSaudade n semente
        putStrLn $ "│ n = " ++ show n ++ "   →   Saudade = " ++ show saudade
        ) [1,2,3,4,5,10,20,50,100,200]

    putStrLn "└───────────────────────────────┘"
    putStrLn "A rotação continua. A memória dança."

main :: IO ()
main = ritual
