{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import System.Random (mkStdGen, randomR, StdGen)
import Data.Bits ((.&.), shiftR)
import Data.List (intercalate)
import Text.Printf (printf)
import Control.Monad (when)
import Data.Word (Word16)

-- ==========================================
-- TIPOS
-- ==========================================

type Odu = Int        -- 0 = aberto, 1 = fechado
type Plano = [Odu]    -- 16 Odus = 16 bits
type Nonce = Int

data Configuracao = Configuracao
  { targetFechados :: Int      -- Número mínimo de Odus fechados
  , maxTentativas  :: Maybe Int -- Limite de tentativas (Nothing = ilimitado)
  , verbose        :: Bool      -- Mostrar progresso
  } deriving (Show)

data EventoOdu = EventoOdu
  { nonceOdu      :: Nonce
  , planoOdu      :: Plano
  , desbloqueado  :: Bool
  , numFechados   :: Int
  } deriving (Show, Generic)

-- ==========================================
-- CONFIGURAÇÃO PADRÃO
-- ==========================================

configPadrao :: Configuracao
configPadrao = Configuracao
  { targetFechados = 12
  , maxTentativas  = Nothing
  , verbose        = True
  }

-- ==========================================
-- FUNÇÕES AUXILIARES
-- ==========================================

-- | Gera 16 bits de uma só vez usando um inteiro
gerarPlano :: StdGen -> (Plano, StdGen)
gerarPlano gen =
  let (w :: Int, gen') = randomR (0, 0xFFFF) gen
      plano = [ if (w `shiftR` i) .&. 1 == 1 then 1 else 0 | i <- [15,14..0] ]
  in (plano, gen')

-- | Conta Odus fechados
contarFechados :: Plano -> Int
contarFechados = sum

-- | Verifica se o plano satisfaz o target
verificarPlano :: Configuracao -> Plano -> Bool
verificarPlano cfg plano = contarFechados plano >= targetFechados cfg

-- | Formata plano para visualização
formatarPlano :: Plano -> String
formatarPlano plano = intercalate " " $ map simbolo plano
  where
    simbolo 0 = "◯"  -- aberto
    simbolo 1 = "●"  -- fechado
    simbolo _ = "?"

-- | Cria evento a partir de plano
criarEvento :: Configuracao -> Nonce -> Plano -> EventoOdu
criarEvento cfg nonce plano =
  let fechados = contarFechados plano
      desbloq = fechados >= targetFechados cfg
  in EventoOdu nonce plano desbloq fechados

-- ==========================================
-- LOOP DE BUSCA
-- ==========================================

-- | Busca o primeiro plano que desbloqueia o token
encontrarTrigger :: Configuracao -> StdGen -> Nonce -> IO (Maybe EventoOdu)
encontrarTrigger cfg gen !nonce = do
  -- Verifica limite de tentativas
  case maxTentativas cfg of
    Just limite | nonce >= limite -> return Nothing
    _ -> continuar
  where
    continuar = do
      let (plano, gen') = gerarPlano gen
          evento = criarEvento cfg nonce plano

      when (verbose cfg && nonce > 0 && nonce `mod` 10000 == 0) $
        putStrLn $ printf "Tentativa %d (fechados: %d/%d)"
                          nonce (numFechados evento) (targetFechados cfg)

      if desbloqueado evento
        then return $ Just evento
        else encontrarTrigger cfg gen' (nonce + 1)

-- ==========================================
-- ANÁLISE ESTATÍSTICA
-- ==========================================

-- Binomial com fórmula multiplicativa (mais estável que factorial direto)
binomial :: Int -> Int -> Double
binomial n k
  | k < 0 || k > n = 0
  | otherwise = fromIntegral (product [n-k+1 .. n]) / fromIntegral (product [1 .. k])

-- | Calcula probabilidade teórica de sucesso
probabilidadeTeorica :: Int -> Double
probabilidadeTeorica target = sum [ binomial 16 k * (0.5 ** 16) | k <- [target..16] ]

-- | Estima número esperado de tentativas
tentativasEsperadas :: Int -> Maybe Int
tentativasEsperadas target =
  let p = probabilidadeTeorica target
  in if p <= 0 then Nothing else Just (ceiling (1.0 / p))


-- ==========================================
-- FUNÇÃO PRINCIPAL
-- ==========================================

main :: IO ()
main = do
  let seed = 12345
      gen = mkStdGen seed
      cfg = configPadrao

  putStrLn "╔════════════════════════════════════════════════╗"
  putStrLn "║  Empire Silicium: Trigger Finder via Odu      ║"
  putStrLn "╚════════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn $ printf "Target: %d Odus fechados de 16" (targetFechados cfg)
  putStrLn $ printf "Probabilidade: %.6f%%" (probabilidadeTeorica (targetFechados cfg) * 100)
  case tentativasEsperadas (targetFechados cfg) of
    Just esperadas -> putStrLn $ printf "Tentativas esperadas: ~%d" esperadas
    Nothing -> putStrLn "Tentativas esperadas: Infinito"
  putStrLn ""
  putStrLn "Buscando trigger..."
  putStrLn ""

  resultado <- encontrarTrigger cfg gen 0

  case resultado of
    Nothing -> putStrLn "❌ Trigger não encontrado (limite atingido)"
    Just evento -> do
      putStrLn "✨ TRIGGER ENCONTRADO!"
      putStrLn ""
      putStrLn $ printf "  Nonce:         %d" (nonceOdu evento)
      putStrLn $ printf "  Odus fechados: %d/%d" (numFechados evento) (targetFechados cfg)
      putStrLn $ printf "  Plano:         %s" (formatarPlano $ planoOdu evento)
      putStrLn $ printf "  Binário:       %s" (concatMap show $ planoOdu evento)

-- ==========================================
-- EXEMPLO DE USO CUSTOMIZADO
-- ==========================================

exemploCustomizado :: IO ()
exemploCustomizado = do
  let cfg = Configuracao
        { targetFechados = 14        -- Mais difícil
        , maxTentativas  = Just 100000
        , verbose        = True
        }
      gen = mkStdGen 99999

  resultado <- encontrarTrigger cfg gen 0
  print resultado
