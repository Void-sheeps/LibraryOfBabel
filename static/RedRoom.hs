{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.Random
import Control.Monad (replicateM)

-- | Tipo de célula no campo
data Celula = Segura | Armadilha Text deriving (Show, Eq)

-- | A matriz do Red Room / Hall of Tortured Souls
type Matriz = [[Celula]]

-- | Função que cria um campo minado simbólico
gerarCampo :: Int -> Int -> IO Matriz
gerarCampo linhas colunas = do
    let total = linhas * colunas
    -- Distribuição aleatória de armadilhas
    indices <- replicateM total (randomRIO (0, 3) :: IO Int)
    let celulas = map (\i -> if i == 0 then Armadilha "⚡ Red Room" else Segura) indices
    return $ chunksOf colunas celulas

-- | Divide a lista em blocos
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | O jogador atravessa a matriz
navegar :: Matriz -> [(Int, Int)] -> Text
navegar matriz path =
    let check (x, y) = (matriz !! x !! y)
        resultados = map check path
    in if any isArmadilha resultados
       then "ALERTA: O jogador encontrou uma armadilha! ⚡"
       else "STATUS: Todos os passos seguros. Continuidade preservada."

-- | Verifica se é armadilha
isArmadilha :: Celula -> Bool
isArmadilha (Armadilha _) = True
isArmadilha _             = False

-- | Exemplo de execução
main :: IO ()
main = do
    campo <- gerarCampo 5 5
    let caminhoDoJogador = [(0,0),(1,2),(3,4),(4,4)]
    putStrLn "--- RED ROOM SIMULATOR ---"
    mapM_ print campo
    putStrLn $ T.unpack $ navegar campo caminhoDoJogador
