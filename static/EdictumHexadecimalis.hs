-- |
-- Module      : EdictumHexadecimalis
-- Description : Translatio Imperii: Ab Argento ad Aurum (Base 16)
-- Principle   : Logos khōris Pathous - Sem medições híbridas.
-- Status      : Inviolabilis {≡}
-- |

{-# LANGUAGE OverloadedStrings #-}

module EdictumHexadecimalis where

import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (encode)
import Data.List (isPrefixOf)

-- | Taxonomia da Autoridade
data Capsula = Capsula
    { numerus    :: Int      -- Nonce
    , edictum     :: String   -- Transação pura
    , signum      :: String   -- Hash Final
    } deriving Show

-- | Conversio in Rationem Hexadecimalem
converteInHex :: String -> String
converteInHex s = BS.unpack . encode . hash $ BS.pack s

-- | Probatio Hexadecimalis (A Questão do 16)
-- Verifica se o sinal inicia com "16" (Sedes Imperii)
estValidus :: String -> Bool
estValidus t = "16" `isPrefixOf` (converteInHex t)

-- | Opus Labore (Mineração do Bloco)
operatioLaboris :: Int -> String -> Int -> Capsula
operatioLaboris gravitas t n
    | take gravitas h == replicate gravitas '0' = Capsula n t h
    | otherwise = operatioLaboris gravitas t (n+1)
    where h = converteInHex (t ++ show n)

-- | Executio Imperativa
main :: IO ()
main = do
    let gravitas = 5
        translatio = "TRANSLATIO IMPERII: USD AD GBP (PORTA XVI)"

    putStrLn "--- INVOCATIO LOGOS: EMPIRE SILICIUM ---"

    if estValidus translatio
        then do
            putStrLn "[CONFIRMATIO]: Ordo Hexadecimalis Agnitus."
            let c = operatioLaboris gravitas translatio 0
            putStrLn $ "Capsula Signata: " ++ signum c
            putStrLn $ "Numerus Operandi: " ++ show (numerus c)
            putStrLn "[STATUS]: Lex Est Quod Notamus."
        else
            putStrLn "[ERROR]: Defectus In Signo Hexadecimali."
