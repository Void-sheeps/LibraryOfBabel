module Main where

-- Importação necessária para funções matemáticas padrão, embora Prelude já cubra log.
-- As funções matemáticas padrão como 'log' são cobertas pelo Prelude.

-- ---------------------------------------------------------
-- 1. Definições Básicas (Tipagem Explícita)
-- ---------------------------------------------------------

-- O tipo 'Double' é a aproximação computacional de ℝ
type R = Double
type R2 = (Double, Double)

-- Constantes das bases
log60 :: R
log60 = log 60

log10Const :: R
log10Const = log 10

-- Definição de f(x) = log x / log 60
f :: R -> R
f x = log x / log60

-- Definição de g(x) = log x / log 10
g :: R -> R
g x = log x / log10Const

-- ---------------------------------------------------------
-- 2. Embedding e Estrutura Vetorial
-- ---------------------------------------------------------

-- Embedding Φ(x)
phi :: R -> R2
phi x = (f x, g x)

-- Vetor direcional v fixo
-- v = (1 / log 60, 1 / log 10)
v :: R2
v = (1 / log60, 1 / log10Const)

-- Operação de multiplicação por escalar (•)
scalarMult :: R -> R2 -> R2
scalarMult k (x, y) = (k * x, k * y)

-- ---------------------------------------------------------
-- 3. Verificação do Teorema (Versão Computacional)
-- ---------------------------------------------------------

-- Predicado para verificar igualdade aproximada (devido a ponto flutuante)
epsilon :: R
epsilon = 1e-10

approxEq :: R -> R -> Bool
approxEq a b = abs (a - b) < epsilon

approxEqVec :: R2 -> R2 -> Bool
approxEqVec (x1, y1) (x2, y2) = approxEq x1 x2 && approxEq y1 y2

-- O Lema traduzido como função de verificação
-- theorem embedding_is_radial {x : ℝ} (hx : x > 0)
checkEmbeddingIsRadial :: R -> Bool
checkEmbeddingIsRadial x
    | x <= 0    = error "Input deve ser estritamente positivo (domínio do log)"
    | otherwise =
        let lhs = phi x                 -- Φ x
            rhs = scalarMult (log x) v  -- (log x) • v
        in approxEqVec lhs rhs

-- ---------------------------------------------------------
-- 4. Execução (Main)
-- ---------------------------------------------------------

main :: IO ()
main = do
    let testVal = 12345.678
    putStrLn $ "Input X: " ++ show testVal
    putStrLn $ "Vetor Φ(x): " ++ show (phi testVal)
    putStrLn $ "Vetor (log x) • v: " ++ show (scalarMult (log testVal) v)

    let result = checkEmbeddingIsRadial testVal
    putStrLn $ "Teorema verificado computacionalmente? " ++ show result
