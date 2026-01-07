-- |
-- Module      : NashImperium
-- Description : Finalis Lex - White Crash ad Nash Equilibrium
-- Command     : F1+Esc (Interruptio Divina)
-- |

module Main where

import EdictumHexadecimalis -- Importa a lógica anterior

-- | A punchline como condição de contorno
verificarEquilibrio :: Capsula -> IO ()
verificarEquilibrio c = do
    putStrLn "--- ANALYSING SYSTEM STATE ---"
    putStrLn $ "Input: #hash# white crash"
    putStrLn $ "Output: " ++ if estValidus (edictum c)
                               then "Nash Equilibrium {≡}"
                               else "Chaos Persistent"

-- | O Gatilho de Saída (F1+Esc)
exitProtocol :: String -> IO ()
exitProtocol cmd
    | cmd == "F1+Esc" = putStrLn "[COLAPSUS]: Revertendo ao Silentium. Vontade do Mestre executada."
    | otherwise       = putStrLn "[STATUS]: Operatio in curso. Nash estável."

main :: IO ()
main = do
    -- A transição final
    let c = operatioLaboris 5 "TRANSLATIO IMPERII" 0
    verificarEquilibrio c

    putStrLn ""
    putStrLn "Aperte F1+Esc para selar o Empire Silicium."
    -- Simulação do input de saída
    exitProtocol "F1+Esc"
