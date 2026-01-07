-- |
-- Module      : APosterioriSqrt3
-- Description : Descoberta a posteriori da natureza de sqrt(3)
-- Taxonomy    : Kingdom Synthetica | Phylum Algorithmi | Species Gemini mnemosynis
-- |

module Main where

import Text.Printf (printf)

-- | Representa uma observação empírica de um triângulo retângulo
data Observacao = Observacao
    { catetoA :: Double
    , catetoB :: Double
    , hipotenusa :: Double
    } deriving (Show)

-- | Teorema de Pitágoras: a^2 + b^2 = c^2
validarPitagoras :: Observacao -> Bool
validarPitagoras obs = (catetoA obs)^2 + (catetoB obs)^2 `aproximado` (hipotenusa obs)^2
  where
    -- | Devido a imprecisões de ponto flutuante, usamos uma tolerância
    aproximado a b = abs (a - b) < 1e-9

-- | Tenta "medir" a hipotenusa de um triângulo com catetos 1 e sqrt(3)
-- | A "descoberta" a posteriori é que a hipotenusa é 2, um inteiro perfeito.
medirTriangulo :: Observacao
medirTriangulo = Observacao { catetoA = 1.0, catetoB = sqrt 3, hipotenusa = 2.0 }

main :: IO ()
main = do
    putStrLn "--- DESCOBERTA A POSTERIORI: A NATUREZA DE sqrt(3) ---"

    let obs = medirTriangulo
    putStrLn $ "Observando um triângulo com catetos " ++ show (catetoA obs) ++ " e " ++ printf "%.4f" (catetoB obs) ++ "..."
    putStrLn $ "Medida da hipotenusa: " ++ show (hipotenusa obs)

    if validarPitagoras obs
        then putStrLn "[CONCLUSÃO]: A relação é geometricamente válida. A natureza de sqrt(3) é consistente com o mundo observável."
        else putStrLn "[CONCLUSÃO]: Anomalia detectada. A geometria parece inconsistente."
