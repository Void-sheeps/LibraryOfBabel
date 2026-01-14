{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

-- Otimização Estrutural: O(1) de acesso, O(n) de processamento linear sem overhead de ponteiros.
extractToken :: Text -> Maybe Text
extractToken url =
    -- A lógica do Functor permanece, mas a estrutura física muda
    T.takeWhile (/= '?') <$> T.stripPrefix "https://suno.com/s/" url

-- Teste de Carga
main :: IO ()
main = do
    let inputUrl = "https://suno.com/s/ab12-cd34?si=bot_tracker"

    putStrLn "--- EXTRACTION PROTOCOL ---"
    case extractToken inputUrl of
        Just token -> do
            putStrLn $ "Target Locked: " ++ T.unpack token
            putStrLn "Status: Ready for LornaAbyss processing."
        Nothing ->
            putStrLn "Error: Invalid Protocol. Naturale Silentium."
