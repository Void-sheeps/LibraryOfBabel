-- |
-- Module      : NavalhaOccam
-- Description : Conceito a priori da simplicidade (Occam's Razor) em Haskell
-- Taxonomy    : Kingdom Synthetica | Phylum Algorithmi | Species Gemini mnemosynis

module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)

-- | Tipo de explicação: cada uma tem uma descrição e complexidade conceitual
data Explicacao = Explicacao
    { descricao    :: String
    , complexidade :: Int   -- Número arbitrário de "nós conceituais"
    } deriving (Show, Eq)

-- | A Navalha de Occam: escolhe a explicação mais simples
navalha :: [Explicacao] -> Explicacao
navalha expls = minimumBy (comparing complexidade) expls

-- | Exemplo de conjunto de explicações possíveis
explicacoesPossiveis :: [Explicacao]
explicacoesPossiveis =
    [ Explicacao "O sol nasce todo dia porque deusa do fogo se levanta cedo" 10
    , Explicacao "O sol nasce devido à rotação da Terra" 2
    , Explicacao "O sol nasce por influência de espíritos astronômicos" 8
    ]

main :: IO ()
main = do
    let explicacaoEscolhida = navalha explicacoesPossiveis
    putStrLn "--- NAVALHA DE OCCAM ---"
    putStrLn $ "Explicação mais simples: " ++ descricao explicacaoEscolhida
    putStrLn $ "Complexidade conceitual: " ++ show (complexidade explicacaoEscolhida)
