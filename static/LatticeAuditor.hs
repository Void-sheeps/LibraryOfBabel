-- LatticeAuditor.hs
-- Simula o "relaxamento" de uma rede atômica para um estado de menor energia.
-- Conceito: Estabilidade como uma função da minimização de energia local.

module Main where

import System.Random

-- Define um ponto na rede 2D com um "valor de energia"
type Point = (Int, Int)
type Energy = Double
type Lattice = [[Energy]]

-- Gera uma rede inicial com energia aleatória
createLattice :: Int -> Int -> IO Lattice
createLattice width height =
    sequence [sequence [randomRIO (0.0, 100.0) | _ <- [1..width]] | _ <- [1..height]]

-- Calcula a energia média dos vizinhos de um ponto
avgNeighborEnergy :: Lattice -> Point -> Energy
avgNeighborEnergy lattice (x, y) =
    let neighbors = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        validNeighbors = filter (\(nx, ny) -> nx >= 0 && nx < length (head lattice) && ny >= 0 && ny < length lattice) neighbors
        energies = map (\(nx, ny) -> (lattice !! ny) !! nx) validNeighbors
    in sum energies / fromIntegral (length energies)

-- "Relaxa" um ponto, movendo sua energia em direção à média dos vizinhos
relaxPoint :: Lattice -> Point -> Energy
relaxPoint lattice point@(x, y) =
    let currentEnergy = (lattice !! y) !! x
        neighborAvg = avgNeighborEnergy lattice point
        -- Fator de relaxamento: quanto o ponto se move em direção à média
        relaxationFactor = 0.5
    in currentEnergy + (neighborAvg - currentEnergy) * relaxationFactor

-- Executa um passo de relaxamento em toda a rede
relaxLattice :: Lattice -> Lattice
relaxLattice lattice =
    let height = length lattice
        width = length (head lattice)
        points = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
    in [[relaxPoint lattice (x, y) | x <- [0..width-1]] | y <- [0..height-1]]

-- Verifica se a rede atingiu um estado estável (diferença de energia abaixo de um limiar)
isStable :: Lattice -> Lattice -> Bool
isStable old new =
    let diff = sum $ zipWith (\rowOld rowNew -> sum $ zipWith (\eOld eNew -> abs (eOld - eNew)) rowOld rowNew) old new
        threshold = 0.1
    in diff < threshold

-- Loop principal de relaxamento
relaxationLoop :: Lattice -> Int -> IO ()
relaxationLoop lattice step = do
    putStrLn $ "Passo de Relaxamento: " ++ show step
    let newLattice = relaxLattice lattice
    if isStable lattice newLattice
    then putStrLn "Rede Estabilizada. Auditoria de Sanidade Lógica Concluída."
    else relaxationLoop newLattice (step + 1)

main :: IO ()
main = do
    putStrLn "--- INICIANDO AUDITORIA DE SANIDADE DA REDE LÓGICA ---"
    let (width, height) = (10, 10)
    initialLattice <- createLattice width height
    relaxationLoop initialLattice 1
