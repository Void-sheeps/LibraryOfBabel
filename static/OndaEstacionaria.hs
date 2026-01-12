-- Onda Estacionária em Haskell
-- Simulação 1D - corda vibrante / tubo sonoro / etc.
-- 2026 © Mnemosyne Aesthetic Division

module Main where

import Data.List     (intercalate)
import Text.Printf   (printf)

-- ================================================
-- Parâmetros físicos básicos
-- ================================================

type Tempo        = Double
type Posicao      = Double
type Amplitude    = Double
type Frequencia   = Double
type ComprimentoOnda = Double

data Onda = Onda
  { amplitude    :: Amplitude
  , k            :: Double          -- número de onda 2π/λ
  , omega        :: Double          -- frequência angular 2πf
  , faseInicial  :: Double
  } deriving (Show)

-- ================================================
-- Funções de onda viajante
-- ================================================

ondaDireita :: Onda -> Posicao -> Tempo -> Double
ondaDireita o x t =
  amplitude o * cos (k o * x - omega o * t + faseInicial o)

ondaEsquerda :: Onda -> Posicao -> Tempo -> Double
ondaEsquerda o x t =
  amplitude o * cos (k o * x + omega o * t + faseInicial o)

-- ================================================
-- Onda estacionária = superposição
-- ================================================

ondaEstacionaria :: Onda -> Posicao -> Tempo -> Double
ondaEstacionaria o x t =
  ondaDireita o x t + ondaEsquerda o x t

-- Forma mais comum (simplificada) da onda estacionária:
-- y(x,t) = 2A cos(kx) cos(ωt + φ)
ondaEstacionariaFormaClassica :: Amplitude -> Double -> Double -> Double -> Tempo -> Double
ondaEstacionariaFormaClassica a k x omega t =
  2 * a * cos (k * x) * cos (omega * t)

-- ================================================
-- Visualização ASCII pobre mas honesta
-- ================================================

type LarguraTela = Int
type AlturaTela  = Int

desenharOnda :: LarguraTela -> Double -> Double -> [Double] -> String
desenharOnda largura yMax yMin valores =
  let normalizar y = round $ (fromIntegral altura / 2) * (y - yMin) / (yMax - yMin)
      altura       = 25   -- linhas de altura fixa
      linhaCentral = altura `div` 2
      pontos       = map normalizar valores
      grid         = replicate altura (replicate largura ' ')

      colocarPonto :: [[Char]] -> Int -> Int -> [[Char]]
      colocarPonto g col lin
        | lin >= 0 && lin < altura && col >= 0 && col < largura =
            take lin g ++
            [take col (g !! lin) ++ "*" ++ drop (col+1) (g !! lin)] ++
            drop (lin+1) g
        | otherwise = g

      desenhado = foldl (\g (col,p) -> colocarPonto g col p) grid
                  (zip [0..] pontos)

  in unlines $ reverse desenhado   -- inverte para y positivo para cima

-- ================================================
-- Exemplo de uso principal
-- ================================================

main :: IO ()
main = do
  putStrLn "┌─────────────────────────────┐"
  putStrLn "│     ONDA ESTACIONÁRIA       │"
  putStrLn "│         Haskell 2026        │"
  putStrLn "└─────────────────────────────┘\n"

  let -- Parâmetros típicos de corda vibrante modo fundamental
      a     = 1.0
      l     = 2.0           -- comprimento da corda
      lambda = 2 * l        -- modo fundamental
      k     = 2 * pi / lambda
      f     = 1.0           -- frequência arbitrária
      omega = 2 * pi * f
      nPontos = 80          -- resolução espacial
      xs    = [ fromIntegral i * l / fromIntegral (nPontos-1) | i <- [0..nPontos-1] ]

      tMax  = 4.0           -- tempo total de simulação
      fps   = 15
      dt    = 1 / fromIntegral fps
      ts    = [ fromIntegral i * dt | i <- [0..round (tMax / dt)] ]

  mapM_ (\t -> do
      let ys = map (\x -> ondaEstacionariaFormaClassica a k x omega t) xs
          yMax = 2.1
          yMin = -2.1

      putStrLn $ printf "\nTempo: %.3f s" t
      putStrLn $ desenharOnda nPontos yMax yMin ys
      putStrLn $ replicate 80 '─'
    ) ts
