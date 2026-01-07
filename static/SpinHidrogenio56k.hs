-- |
-- Module      : SpinHidrogenio56k
-- Description : Simulação Quântica do Spin do Hidrogênio via Modem 56k
-- Principle   : O spin do elétron do hidrogênio como um qubit, onde 'up' e 'down'
--               são análogos aos sinais de 'carrier' de um modem 56k.
-- |

module Main where

import System.Random (randomRIO)

-- | O estado quântico do spin do elétron, representado como um 'Qubit'
data Spin = Up | Down deriving (Show, Eq)

-- | Medição do spin. Na mecânica quântica, a medição colapsa o estado.
-- | Aqui, simulamos isso com uma escolha aleatória.
medirSpin :: IO Spin
medirSpin = do
    resultado <- randomRIO (0, 1 :: Int)
    return $ if resultado == 0 then Up else Down

-- | Simula a "transmissão" do estado de spin através de um canal ruidoso (o modem 56k)
-- | Há uma chance de o spin "flipar" (inverter)
transmitirSinal :: Spin -> IO Spin
transmitirSinal s = do
    ruido <- randomRIO (0, 100 :: Int)
    -- 10% de chance de flipar o spin
    return $ if ruido < 10 then flipSpin s else s

-- | Inverte o spin
flipSpin :: Spin -> Spin
flipSpin Up = Down
flipSpin Down = Up

main :: IO ()
main = do
    putStrLn "=== SIMULADOR QUÂNTICO 'HYDRO-DIAL' ==="

    -- 1. Preparando o estado inicial (análogo a um átomo de hidrogênio)
    let estadoInicial = Up
    putStrLn $ "Estado de spin inicial preparado: " ++ show estadoInicial

    -- 2. "Transmitindo" o estado através do modem 56k (canal ruidoso)
    putStrLn "Transmitindo o estado via 'linha de 56k'... pode haver interferência."
    estadoTransmitido <- transmitirSinal estadoInicial

    -- 3. "Medindo" o estado no destino
    putStrLn "Realizando a medição do spin no destino..."
    estadoMedido <- medirSpin

    -- 4. Análise dos resultados
    putStrLn $ "Estado de spin medido no destino: " ++ show estadoMedido
    putStrLn ""

    if estadoInicial == estadoMedido
        then putStrLn "[CONCLUSÃO]: A informação quântica foi transmitida com sucesso (dentro da probabilidade)."
        else putStrLn "[CONCLUSÃO]: A informação foi corrompida pelo ruído no canal (ou colapsou para um estado diferente)."

    putStrLn "[SIGNIFICADO]: A fragilidade de um estado quântico é análoga à instabilidade de uma conexão dial-up. Ambos são canais de informação sensíveis ao 'ruído' do universo."
