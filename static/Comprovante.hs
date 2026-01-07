{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)

-- | Registro de pagamento aceito
data Comprovante = Comprovante
  { timestamp :: Text
  , tokenID   :: Text
  , status    :: Text
  , hashID    :: Text
  } deriving Show

-- | Gera hash SHA-256 como "assinatura" do comprovante
gerarHash :: Text -> Text -> Text -> Text
gerarHash ts tid st =
    let input = T.concat [ts, tid, st]
        digest :: Digest SHA256
        digest = hash (B.pack $ T.unpack input)
    in T.pack (show digest)

-- | Cria comprovante para token aceito
criarComprovante :: Text -> IO Comprovante
criarComprovante tid = do
    ts <- fmap (T.pack . show) getCurrentTime
    let st = "ACEITO"
        h  = gerarHash ts tid st
    return $ Comprovante ts tid st h

-- | Exemplo de uso
main :: IO ()
main = do
    -- Digamos que o token enviado à IA foi "TOKEN_42"
    comprovante <- criarComprovante "TOKEN_42"
    TIO.putStrLn "--- COMPROVANTE DE PAGAMENTO ---"
    TIO.putStrLn $ "Timestamp: " <> timestamp comprovante
    TIO.putStrLn $ "Token ID: " <> tokenID comprovante
    TIO.putStrLn $ "Status: " <> status comprovante
    TIO.putStrLn $ "Hash: " <> hashID comprovante
