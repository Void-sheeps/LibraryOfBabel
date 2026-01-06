{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import Control.Monad (replicateM)

-- | Estado da IA
data EstadoIA = EstadoIA
  { capacidadeTotal :: Int      -- capacidade máxima de tokens simultâneos
  , tokensAtuais    :: Int      -- tokens atualmente processados
  } deriving Show

-- | Simula o envio de um token
-- | Retorna True se aceito, False se recusa (saturação)
enviarToken :: EstadoIA -> IO (Bool, EstadoIA)
enviarToken estado@(EstadoIA cap atual) = do
    let chanceAceite = max 0  (fromIntegral (cap - atual) / fromIntegral cap)
    rnd <- randomRIO (0.0, 1.0) :: IO Double
    let aceito = rnd <= chanceAceite
        novoEstado = if aceito
                     then estado { tokensAtuais = min cap (atual + 1) }
                     else estado
    return (aceito, novoEstado)

-- | Simula a liberação de tokens ao longo do tempo (processamento da IA)
liberarTokens :: EstadoIA -> IO EstadoIA
liberarTokens estado@(EstadoIA cap atual) = do
    -- libera aleatoriamente 0 a 2 tokens
    liberar <- randomRIO (0, min 2 atual)
    return estado { tokensAtuais = atual - liberar }

-- | Rodada de teste: envia um token e atualiza estado
rodada :: EstadoIA -> IO EstadoIA
rodada estado = do
    (aceito, estado') <- enviarToken estado
    estado'' <- liberarTokens estado'
    putStrLn $ "Token enviado: " ++ show aceito
             ++ " | Tokens atuais: " ++ show (tokensAtuais estado'')
    return estado''

-- | Simula N rodadas de envio
simular :: Int -> EstadoIA -> IO ()
simular 0 _ = putStrLn "--- Simulação finalizada ---"
simular n estado = do
    novoEstado <- rodada estado
    simular (n-1) novoEstado

-- | Estado inicial da IA
estadoInicial :: EstadoIA
estadoInicial = EstadoIA { capacidadeTotal = 10, tokensAtuais = 0 }

-- | Execução
main :: IO ()
main = do
    putStrLn "--- Simulação de Saturação de IA Estúdio ---"
    simular 30 estadoInicial
