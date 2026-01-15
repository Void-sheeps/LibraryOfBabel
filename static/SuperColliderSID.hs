{-# LANGUAGE OverloadedStrings #-}
import Sound.OSC
import Control.Concurrent (threadDelay)

-- | Envia comandos para um emulador SID no SuperCollider
sendSIDCommand :: UDP -> Int -> Float -> Int -> Int -> IO ()
sendSIDCommand fd voice freq waveform pulseWidth = do
    sendOSC fd $ bundle immediately
        [ message "/sid" [ int32 voice      -- Voz (0, 1, 2)
                         , float freq       -- Frequência em Hz
                         , int32 waveform   -- Forma de onda (4=pulso, 2=serra, etc.)
                         , int32 pulseWidth -- Largura do pulso (0-4095)
                         ] ]

main :: IO ()
main = withTransport (openUDP "127.0.0.1" 57110) $ \fd -> do
    putStrLn "Iniciando transmissão para emulador SID no SuperCollider"

    -- Toca um acorde típico de C64 (tríade em vozes separadas)
    -- Voz 0: Nota C4 (forma de onda serra)
    sendSIDCommand fd 0 261.63 2 0
    -- Voz 1: Nota E4 (forma de onda pulso com largura estreita)
    sendSIDCommand fd 1 329.63 4 1024
    -- Voz 2: Nota G4 (forma de onda triângulo)
    sendSIDCommand fd 2 392.00 16 0

    threadDelay 3000000 -- Segura por 3 segundos

    -- Silencia todas as vozes
    mapM_ (\v -> sendSIDCommand fd v 0 0 0) [0, 1, 2]
    putStrLn "Execução concluída."
