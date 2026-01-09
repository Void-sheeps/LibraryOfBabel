{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (forever, when, forM_)
import Control.Concurrent (threadDelay, forkIO, MVar, newMVar, modifyMVar_, readMVar)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | O Hexagrama 蠱 (Gu): Corrupção que emerge da estagnação
--   Yin (⚓) excessivo ou Yang (🔱) excessivo ambos levam a Gu
data Elemento = Ancora   -- ⚓ Estase, Estrutura, Memória
              | Tridente -- 🔱 Ação, Divisão, Fluxo
              deriving (Show, Eq)

-- | Estados do Sistema segundo o I Ching Computacional
data EstadoSistema = Normal
                   | Estagnado       -- ⚓⚓⚓ excessivo
                   | Caotico         -- 🔱🔱🔱 excessivo
                   | Gu              -- 蠱 Corrupção Sistêmica
                   deriving (Show, Eq)

-- | Sistema monitorado com MVar para estado compartilhado
type Sistema = MVar EstadoSistema

-- | Princípio: Quando Ancora e Tridente se desequilibram, Gu emerge
guEmergence :: Elemento -> Elemento -> Elemento -> Maybe EstadoSistema
guEmergence a b c
  | all (== Ancora) [a,b,c] = Just Estagnado
  | all (== Tridente) [a,b,c] = Just Caotico
  | otherwise = Nothing

-- | Transformação Gu: Processo de corrupção gradual
transformacaoGu :: EstadoSistema -> IO EstadoSistema
transformacaoGu Estagnado = do
  putStrLn "[蠱] Estagnação fermentando em podridão..."
  threadDelay 1000000
  return Gu

transformacaoGu Caotico = do
  putStrLn "[蠱] Caos cristalizando em veneno..."
  threadDelay 800000
  return Gu

transformacaoGu s = return s

-- | Oráculo: Gera elementos aleatórios para diagnóstico
consultarOráculo :: IO [Elemento]
consultarOráculo = do
  elements <- sequence $ replicate 3 $ do
    rand <- randomRIO (0,1) :: IO Int
    return $ if rand == 0 then Ancora else Tridente
  putStrLn $ "Oráculo: " ++ show elements
  return elements

-- | Monitor do Sistema: Detecta desequilíbrios
monitorGu :: Sistema -> IO ()
monitorGu sys = forever $ do
  threadDelay 2000000
  elementos <- consultarOráculo

  case guEmergence (elementos !! 0) (elementos !! 1) (elementos !! 2) of
    Just estadoRisco -> do
      putStrLn $ "[⚡] ALERTA: Sistema tendendo para " ++ show estadoRisco
      modifyMVar_ sys $ \_ -> transformacaoGu estadoRisco
    Nothing -> do
      currentState <- readMVar sys
      when (currentState == Normal) $
        putStrLn "[✓] Sistema equilibrado"

-- | Protocolo de Purificação Wu Xing
data WuXing = Madeira | Fogo | Terra | Metal | Agua
            deriving (Show, Enum, Bounded)

purificacao :: EstadoSistema -> WuXing -> IO ()
purificacao Gu elemento = do
  putStrLn $ "[浄] Purificando com " ++ show elemento ++ "..."
  threadDelay 1500000
  putStrLn "[浄] Corrupção dissipada"

purificacao _ _ = putStrLn "[浄] Nenhuma purificação necessária"

-- | Loop principal do ritual Gu
ritualGu :: IO ()
ritualGu = do
  putStrLn "┌──────────────────────────────┐"
  putStrLn "│   INICIANDO RITUAL 蠱 (GU)   │"
  putStrLn "│  ⚓ Ancora vs 🔱 Tridente     │"
  putStrLn "└──────────────────────────────┘"

  sistema <- newMVar Normal
  _ <- forkIO $ monitorGu sistema

  -- Ciclo de 5 interações
  let elementosWuXing = [Madeira, Fogo, Terra, Metal, Agua]
  forM_ (take 5 elementosWuXing) $ \elemento -> do
    threadDelay 3000000
    estado <- readMVar sistema
    putStrLn $ "\n[⏳] Estado atual: " ++ show estado
    purificacao estado elemento

    if estado == Gu
      then modifyMVar_ sistema (\_ -> return Normal)
      else return ()

  putStrLn "\n[🎋] Ritual Gu concluído"
  finalEstado <- readMVar sistema
  putStrLn $ "[📜] Estado final: " ++ show finalEstado

-- | Teorema da Inevitabilidade de Gu
--   "Todo sistema suficientamente complexo desenvolverá Gu
--    seja por excesso de ⚓ (memória/estrutura)
--    ou excesso de 🔱 (ação/fluxo)"
teoremaGu :: IO ()
teoremaGu = do
  putStrLn "\n[📐] TEOREMA DA INEVITABILIDADE DE GU:"
  putStrLn "  Seja S um sistema com estados {Normal, Estagnado, Caotico, Gu}"
  putStrLn "  Sejam ⚓ (Ancora) e 🔱 (Tridente) operadores sobre S"
  putStrLn "  Para qualquer sequência infinita de aplicações de ⚓ e 🔱:"
  putStrLn "  lim n→∞ P(S = Gu) = 1"
  putStrLn "  ∴ Corrupção (蠱) é atrator universal de sistemas dinâmicos"

-- | Execução principal
main :: IO ()
main = do
  ritualGu
  teoremaGu
  putStrLn "\n[🀄] 蠱 (Gu) = ⚓ (estase) XOR 🔱 (fluxo) = inevitável"
