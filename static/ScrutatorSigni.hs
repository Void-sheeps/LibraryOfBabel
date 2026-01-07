-- |
-- Module      : ScrutatorSigni
-- Description : Sensor de Osciloscópio Virtual (Empire Silicium)
-- Principle   : Logos khōris Pathous - Monitoramento de Voltagem
-- |

module Main where

import Data.List (foldl')

-- | Tipologia do Sinal (Empire Silicium Taxonomy)
data Sinal = Sinal
    { voltagem :: Double
    , tempo    :: Double
    } deriving Show

-- | Filtro de Compensação (Regra da Mão Direita para Fase)
compensarSinal :: Double -> [Sinal] -> [Sinal]
compensarSinal fator = map (\s -> s { voltagem = voltagem s * fator })

-- | Detecção do Equilíbrio de Nash (Estabilidade do Sinal)
isEstavel :: [Sinal] -> Bool
isEstavel amostras =
    let media = sum (map voltagem amostras) / fromIntegral (length amostras)
    in all (\s -> abs (voltagem s - media) < 0.01) amostras

-- | Execução do Sensor
main :: IO ()
main = do
    let amostrasRaw = [Sinal 0.5 0.1, Sinal 0.51 0.2, Sinal 0.49 0.3]
    let amostrasCompensadas = compensarSinal 10.0 amostrasRaw

    putStrLn "=== MONITORING VIA INSTRUMENTUM SCRUTANDI ==="

    if isEstavel amostrasCompensadas
        then putStrLn "[STATUS]: Nash Equilibrium detectado no sinal."
        else putStrLn "[ALERT]: White Crash iminente - Ruído analógico detectado."

    putStrLn "Comando de Saída: F1+Esc para fechar o canal de dados."
