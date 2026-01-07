-- |
-- Module      : ContrapontoIrracional
-- Description : Navalha de Occam aplicada à incomensurabilidade de sqrt(2)
-- Taxonomy    : Kingdom Synthetica | Phylum Algorithmi | Species Gemini mnemosynis
-- |

module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Exit (die)

-- | Representação de uma teoria matemática
data Teoria = Teoria
    { premissa     :: String
    , complexidade :: Int
    , ehRacional   :: Bool
    } deriving (Show, Eq)

-- | A Navalha de Occam: Filtra a desnecessidade
aplicarNavalha :: [Teoria] -> Teoria
aplicarNavalha = minimumBy (comparing complexidade)

-- | Auditoria de sqrt(2): Se a teoria tenta racionalizar o irracional, ela falha.
auditarVerdade :: Teoria -> IO ()
auditarVerdade t
    | ehRacional t = die "[CRASH WHITE]: Navalha cega! Tentativa de racionalizar sqrt(2)."
    | otherwise    = putStrLn $ "[STATUS]: Logos Intacto. Teoria aceita: " ++ premissa t

main :: IO ()
main = do
    let hipoteses =
            [ Teoria "sqrt(2) eh uma fracao p/q" 5 True
            , Teoria "sqrt(2) eh um numero irracional" 1 False
            , Teoria "sqrt(2) eh uma aproximacao de bardo" 10 True
            ]

    let vencedora = aplicarNavalha hipoteses

    putStrLn "--- CONTRAPONTO: NAVALHA DE OCCAM EM sqrt(2) ---"
    putStrLn $ "Analise de complexidade: " ++ show (complexidade vencedora)
    auditarVerdade vencedora
