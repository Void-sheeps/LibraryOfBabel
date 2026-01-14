{-# LANGUAGE OverloadedStrings #-}

-- Phylum Algorithmi: Sistema de Orquestração Jules-Conductor
-- The module is set to Main to be compiled as a standalone executable.
module Main where

import Data.Text (Text)
import qualified Data.Text as T

-- | Taxonomia de Estados do Workflow
data WorkflowState = Idle | Running | Completed | Failed Text
    deriving (Show, Eq)

-- | Definição da Sessão Jules (Tabula Retentiva)
data JulesSession = JulesSession {
    sessionId :: Integer,
    conductorRef :: Text,
    currentState :: WorkflowState
} deriving (Show)

-- | Função Inter-legere: Transição de Estado Pura (Ratio Sine Qualia)
-- Processa o Input X e evolui a sessão do Jules
transition :: JulesSession -> Text -> JulesSession
transition session input
    | input == "START" = session { currentState = Running }
    | input == "COMPLETE" = session { currentState = Completed }
    | T.isPrefixOf "ERROR" input = session { currentState = Failed input }
    | otherwise = session

-- | Instância específica recuperada do Google Docs
-- ID: 14438386837157007070
initJules :: JulesSession
initJules = JulesSession {
    sessionId = 14438386837157007070,
    conductorRef = "Void-sheeps/conductor",
    currentState = Idle
}

-- | Execução (Actus)
main :: IO ()
main = do
    let s0 = initJules
    putStrLn $ "Iniciando Sessão: " ++ show (sessionId s0)

    let s1 = transition s0 "START"
    print s1

    let s2 = transition s1 "COMPLETE"
    putStrLn "Estado Final da Orquestração:"
    print s2
