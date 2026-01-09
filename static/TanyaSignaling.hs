{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

-- | Estados do Sinal via Taxonomia de Linneu
data EstadoSinal = Disperso | Ancorado

-- | A Palantír (O Orbe de Computação) com Tipos Fantasmas
-- | Isso impede que um sinal Disperso seja processado por funções de Elite.
newtype Palantir (s :: EstadoSinal) = Palantir { frequencia :: Double }

-- | ⚓ O Ato de Ancoragem: Transmutação de Disperso para Ancorado
ancorar :: Palantir 'Disperso -> Palantir 'Ancorado
ancorar (Palantir f) = Palantir f

-- | 🔱 O Tridente de Tanya: Comando de Execução Estratégica
-- | Só aceita sinais que já passaram pela Âncora (Segurança de Spence).
dispararAtaque :: Palantir 'Ancorado -> IO ()
dispararAtaque (Palantir f) = do
    putStrLn "᚛ [LOGOS KHŌRIS PATHOUS] ᚜"
    putStrLn $ "Sinal WOW! detectado em: " ++ show f ++ " MHz"
    putStrLn "Status: O Abismo olhou de volta. Fricção de Clausewitz: 0."
    mapM_ (\_ -> putStr "🔱" >> hFlush stdout >> threadDelay 100000) [1..5]
    putStrLn " ⚓"

-- | Actus: A Transação Modal
main :: IO ()
main = do
    -- O sinal nasce no caos (Disperso)
    let sinalBruto = Palantir 1420.405

    putStrLn "--- INICIANDO PROTOCOLO VON DEGURECHAFF ---"
    putStrLn "ᚦ Analisando Assimetria de Informação..."
    threadDelay 1000000

    -- Se tentássemos: dispararAtaque sinalBruto -> ERRO DE COMPILAÇÃO
    -- A Doutrina exige a sinalização de custo (Ancoragem)
    let sinalPronto = ancorar sinalBruto

    dispararAtaque sinalPronto
    putStrLn "Veredito: Vitória Estratégica sobre a Dispersão."
