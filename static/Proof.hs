{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (encode)
import Data.Char (isUpper)
import Data.List (isInfixOf)

-- ==================================================
-- TIPOLOGIA
-- ==================================================
-- Cada bloco contém um nonce (PoW) e um pensamento (PoT)
data Block = Block
  { nonce     :: Int
  , pensamiento :: String
  , hashBlock :: String
  } deriving Show

-- ==================================================
-- FUNÇÃO DE HASH (SHA-256)
-- ==================================================
hashString :: String -> String
hashString s = BS.unpack . encode . hash $ BS.pack s

-- ==================================================
-- PROOF-OF-WORK
-- ==================================================
-- Encontrar um nonce tal que o hash comece com n zeros
proofOfWork :: Int -> String -> Int -> Block
proofOfWork dificuldade pensamento n
  | take dificuldade h == replicate dificuldade '0' = Block n pensamento h
  | otherwise = proofOfWork dificuldade pensamento (n+1)
  where
    h = hashString (pensamento ++ show n)

-- ==================================================
-- PROOF-OF-THOUGHT
-- ==================================================
-- Valida se o pensamento cumpre “critério cognitivo”:
-- Ex: tem pelo menos 3 letras maiúsculas consecutivas
proofOfThought :: String -> Bool
proofOfThought p = any (>=3) (map length (filter (all isUpper) (words p)))

-- ==================================================
-- CRIAR BLOCO CONCEITUAL
-- ==================================================
criarBloco :: Int -> String -> Maybe Block
criarBloco dificuldade p
  | proofOfThought p = Just (proofOfWork dificuldade p 0)
  | otherwise        = Nothing -- pensamento inválido

-- ==================================================
-- EXEMPLO DE EXECUÇÃO
-- ==================================================
main :: IO ()
main = do
  let dificuldade = 4
      pensamento  = "IDEIAS GRANDES NECESSITAM ACÃO"

  case criarBloco dificuldade pensamento of
    Just b  -> do
      putStrLn "=== BLOCO VALIDADO ==="
      putStrLn $ "Nonce: " ++ show (nonce b)
      putStrLn $ "Pensamento: " ++ pensamento
      putStrLn $ "Hash: " ++ hashBlock b
    Nothing -> putStrLn "Pensamento inválido para PoT. Bloqueio negado."
