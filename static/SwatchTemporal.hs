{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Fixed (mod') -- <--- IMPORT NECESSÁRIO PARA O CÁLCULO DE BEATS

-- ============================================
-- AXIOMAS
-- ============================================

secondsPerDay :: Double
secondsPerDay = 86400

secondsPerBeat :: Double
secondsPerBeat = 86.4   -- 86400 / 1000

-- ============================================
-- TIPOS FUNDAMENTAIS
-- ============================================

-- Tempo civil local (Brasília, UTC-3)
data CivilTime = CivilTime
  { hour   :: Int
  , minute :: Int
  , second :: Int
  } deriving (Show, Eq)

-- Tempo Swatch (simulacro decimal)
newtype Beat = Beat Double
  deriving (Show, Eq)

-- Contexto perdido na conversão
data DayOffset
  = Ontem
  | Hoje
  | Amanhã
  deriving (Show, Eq)

-- Resultado semanticamente honesto
data Temporal a = Temporal
  { valor   :: a
  , contexto :: DayOffset
  } deriving (Show, Eq)

-- ============================================
-- FUNÇÕES AUXILIARES
-- ============================================

toSeconds :: CivilTime -> Double
toSeconds CivilTime{..} =
  fromIntegral (hour * 3600 + minute * 60 + second)

fromSeconds :: Double -> CivilTime
fromSeconds s =
  let total = round s `mod` 86400
      (h, r1) = total `divMod` 3600
      (m, s') = r1 `divMod` 60
  in CivilTime h m s'

-- ============================================
-- CONVERSÕES (PARCIAIS E CONTEXTUAIS)
-- ============================================

-- Brasília (UTC-3) -> Swatch (Biel UTC+1)
-- Diferença: +4 horas (14400 segundos)
brasiliaToBeat :: CivilTime -> Beat
brasiliaToBeat ct =
  let secondsBR   = toSeconds ct
      -- O dia em Biel começa 4 horas antes de Brasília
      secondsBiel = (secondsBR + 14400) `mod'` secondsPerDay
  in Beat (secondsBiel / secondsPerBeat)

-- Swatch -> Brasília (com perda explícita)
beatToBrasilia :: Beat -> Temporal CivilTime
beatToBrasilia (Beat b) =
  let secondsBiel = b * secondsPerBeat
      secondsBR   = secondsBiel - 14400
  in case () of
       _ | secondsBR < 0 ->
             Temporal (fromSeconds (secondsBR + secondsPerDay)) Ontem
         | secondsBR >= secondsPerDay ->
             Temporal (fromSeconds (secondsBR - secondsPerDay)) Amanhã
         | otherwise ->
             Temporal (fromSeconds secondsBR) Hoje

-- ============================================
-- OPERAÇÃO CRÍTICA: NÃO-REVERSIBILIDADE
-- ============================================

roundTrip :: CivilTime -> Temporal CivilTime
roundTrip =
  beatToBrasilia . brasiliaToBeat

-- ============================================
-- EXECUÇÃO DE TESTE
-- ============================================

main :: IO ()
main = do
  let meioDia = CivilTime 12 0 0
      meiaNoite = Beat 0

  putStrLn "=== TEMPO COMO SIMULACRO ===\n"

  putStrLn "Brasília -> Swatch (12:00 BRT):"
  -- 12:00 BRT = 16:00 BMT (Biel)
  -- 16:00 = 57600 segundos
  -- 57600 / 86.4 = @666.66...
  print (brasiliaToBeat meioDia)

  putStrLn "\nSwatch -> Brasília (@000):"
  -- @000 BMT = 00:00 BMT
  -- 00:00 BMT - 4h = 20:00 BRT (do dia anterior)
  print (beatToBrasilia meiaNoite)

  putStrLn "\nRound-trip (12:00 BRT):"
  -- Deve retornar ao meio dia, contexto 'Hoje'
  print (roundTrip meioDia)
