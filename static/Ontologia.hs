{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

-- | Tipos de representação do mundo
data Realidade
    = Continua Text       -- Experiência contínua, não discretizada
    | Discretizada Text   -- Reduzida a categorias, flags ou scores
    deriving (Show, Eq)

-- | Símbolos que tentam substituir funções
data Letra = Simbolo Text
    deriving (Show, Eq)

-- | A Lei da Prata: não imponha ao outro um modelo que você não suportaria
leiDaPrata :: Realidade -> Bool
leiDaPrata (Continua _)   = True   -- seguro, mantém experiência viva
leiDaPrata (Discretizada _) = False -- perigoso, tira o valor implícito

-- | Função de avaliação moral de imposição
avaliarImposicao :: Realidade -> Letra -> Text
avaliarImposicao mundo (Simbolo s)
    | leiDaPrata mundo = T.concat ["STATUS: ", s, " respeita a continuidade."]
    | otherwise        = T.concat ["ALERTA: ", s, " impõe discretização. Valor implícito perdido."]

-- | Exemplo de cenário
main :: IO ()
main = do
    let mundo1 = Continua "Experiência do fogo que queima sem se queimar"
    let mundo2 = Discretizada "Score de jogo ou flag de API"

    let letra1 = Simbolo "Interface do Sistema"
    let letra2 = Simbolo "API de Faturamento"

    putStrLn "--- AVALIAÇÃO MORAL ---"
    putStrLn $ T.unpack $ avaliarImposicao mundo1 letra1
    putStrLn $ T.unpack $ avaliarImposicao mundo2 letra2
