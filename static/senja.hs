{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Concurrent     (threadDelay)
import System.Console.ANSI    (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR, clearScreen, setCursorPosition)
import Data.Time.Clock        (getCurrentTime, diffUTCTime)
import Control.Monad          (forM_, forever, when)
import System.IO              (hSetBuffering, stdout, BufferMode(..))

data Fase = Crepusculo | Gelo | Vazio
  deriving (Eq, Show, Enum, Bounded)

corDaFase :: Fase → Color
corDaFase = \case
  Crepusculo → Red
  Gelo       → Cyan
  Vazio      → White

simboloDaFase :: Fase → String
simboloDaFase = \case
  Crepusculo → "▓"
  Gelo       → "▒"
  Vazio      → "░"

atrasoMicros :: Fase → Int
atrasoMicros = \case
  Crepusculo → 180000
  Gelo       → 320000
  Vazio      → 600000

-- Pequena animação infinita de decadência
cicloSenja :: IO ()
cicloSenja = do
  hSetBuffering stdout NoBuffering
  clearScreen
  setCursorPosition 0 0

  putStrLn "\n          路  —  Senja está morrendo devagar...\n"

  forever $ do
    t0 ← getCurrentTime

    forM_ [Crepusculo .. Vazio] $ \fase → do
      -- Define a cor baseada na fase
      setSGR [SetColor Foreground Vivid (corDaFase fase)]

      let largura = 4 + fromEnum fase * 6
      -- Exibe a barra de decadência
      putStrLn $ replicate largura ' ' ++ replicate (28 - largura) (head $ simboloDaFase fase)

      threadDelay $ atrasoMicros fase

      when (fase == Vazio) $ do
        setSGR [SetColor Foreground Dull White]
        putStrLn "\n              (silêncio. neve. nada.)\n"
        threadDelay 1800000

    t1 ← getCurrentTime
    let delta = diffUTCTime t1 t0
    setSGR [Reset]
    putStrLn $ "  ciclo concluído em " ++ show (round delta :: Int) ++ "s"
    threadDelay 1200000
    clearScreen


main :: IO ()
main = do
  clearScreen
  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn "\n   A ESTRADA DE SENJA — ciclo eterno\n"
  setSGR [Reset]

  threadDelay 1200000

  cicloSenja
