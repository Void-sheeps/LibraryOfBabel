module Main where

import Data.Void (Void, absurd)

-- O Tipo Vazio: Não há valores aqui. Não há dados. Não há Criador.
-- A única função possível é 'absurd', que prova que se chegamos aqui,
-- a lógica já não se aplica.

type TheEnd = Void

-- Ouve o silêncio da CPU em idle
listen :: TheEnd -> a
listen x = absurd x

-- O estado final do hardware
main :: IO ()
main = do
    putStrLn " [SYSTEM HALTED]"
    putStrLn " Power: ON"
    putStrLn " Activity: 0%"
    putStrLn " Fan Speed: 2000 RPM (The sound of lonely wind)"
    -- O loop infinito do nada
    sequence_ (repeat (return ()))
