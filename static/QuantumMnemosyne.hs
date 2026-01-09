{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Complex
import Text.Printf (printf)
import System.Random (Random(..), RandomGen, getStdGen, newStdGen, StdGen)
import Control.Monad.State (State, get, put, evalState, runState)
import Control.Monad (zipWithM_, foldM)
import Data.List (transpose)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- ===========================================================================
-- TIPOS QUÂNTICOS FUNDAMENTAIS
-- ===========================================================================

-- | Vetor de estado quântico: lista de amplitudes complexas
type QVec = [Complex Double]

-- | Operador quântico (matriz unitária)
type QOp = [[Complex Double]]

-- | Sistema quântico com estado e operadores
data QuantumSystem = QSystem
    { qState  :: QVec          -- Vetor de estado
    , qOps    :: [QOp]         -- Histórico de operações aplicadas
    , qEnergy :: Double        -- Energia do sistema (para normalização)
    } deriving (Show)

-- | Entidade do combate com propriedades quânticas
data CombatEntity = Entity
    { eName      :: String     -- Nome da entidade
    , eBaseState :: QVec       -- Estado base inicial (1 qubit)
    , eOps       :: [QOp]      -- Operadores disponíveis
    , eCoherence :: Double     -- Coerência quântica (0 a 1)
    } deriving (Show)

-- ===========================================================================
-- OPERADORES QUÂNTICOS BÁSICOS
-- ===========================================================================

i :: Complex Double
i = 0 :+ 1

-- | Matriz identidade 2x2 (1 qubit)
identity2 :: QOp
identity2 = [[1, 0], [0, 1]]

-- | Porta Pauli-X (NOT quântico)
paulix :: QOp
paulix = [[0, 1], [1, 0]]

-- | Porta Pauli-Y
pauliy :: QOp
pauliy = [[0, -i], [i, 0]]

-- | Porta Pauli-Z
pauliz :: QOp
pauliz = [[1, 0], [0, -1]]

-- | Porta Hadamard
hadamard :: QOp
hadamard = [[1/sqrt2, 1/sqrt2], [1/sqrt2, -1/sqrt2]]
  where sqrt2 = sqrt 2.0

-- | Porta CNOT (2 qubits) - Controla no primeiro, Alvo no segundo
cnot :: QOp
cnot =
    [[1, 0, 0, 0],
     [0, 1, 0, 0],
     [0, 0, 0, 1],
     [0, 0, 1, 0]]

-- ===========================================================================
-- FUNÇÕES DE OPERAÇÃO QUÂNTICA
-- ===========================================================================

-- | Multiplicação de matriz por vetor
applyOp :: QOp -> QVec -> QVec
applyOp op vec = map (sum . zipWith (*) vec) op

-- | Produto tensorial de dois operadores (Matrizes)
tensorProduct :: QOp -> QOp -> QOp
tensorProduct a b =
    [ [ aij * bkl | bRow <- b, bkl <- bRow ]
      | aRow <- a, aij <- aRow ]

-- | Normaliza um vetor quântico
normalize :: QVec -> QVec
normalize vec =
    let norm = sqrt (sum (map (\c -> magnitude c ** 2) vec))
    in if norm == 0 then vec else map (/ (norm :+ 0)) vec

-- | Mede um qubit específico (Simulação de Colapso)
-- Nota: Esta implementação simplificada colapsa o sistema inteiro
-- baseando-se nas probabilidades agregadas.
measureSystem :: QVec -> State StdGen (Int, QVec)
measureSystem vec = do
    let probs = map (\c -> magnitude c ** 2) vec
        cumulative = scanl1 (+) probs
        total = last cumulative

    gen <- get
    let (r, gen') = randomR (0.0, total) gen
    put gen'

    let idx = length (takeWhile (<= r) cumulative)
        -- Cria novo vetor colapsado (apenas o estado medido sobrevive)
        collapsed = [ if i == idx then 1.0 :+ 0.0 else 0.0 :+ 0.0 | i <- [0..length vec - 1] ]

    return (idx, collapsed)

-- ===========================================================================
-- ENTIDADES DE COMBATE
-- ===========================================================================

qubit0 :: QVec
qubit0 = [1, 0] -- |0⟩

qubit1 :: QVec
qubit1 = [0, 1] -- |1⟩

-- | Superposição balanceada |+⟩
plusState :: QVec
plusState = [1/sqrt 2, 1/sqrt 2]

-- | Cria entidade
createEntity :: String -> QVec -> Double -> CombatEntity
createEntity name initialState coherence = Entity
    { eName = name
    , eBaseState = initialState
    , eOps = [identity2, paulix, pauliy, pauliz, hadamard]
    , eCoherence = coherence
    }

-- | Definição das Entidades (Estados de 1 Qubit)
samantaFilia :: CombatEntity
samantaFilia = createEntity "Samanta (Filia)" plusState 0.95 -- Começa em superposição

theodoreBeowulf :: CombatEntity
theodoreBeowulf = createEntity "Theodore (Beowulf)" qubit0 0.85 -- Começa no estado fundamental

-- | Produto tensorial de dois vetores (Estados)
tensorProduct2 :: QVec -> QVec -> QVec
tensorProduct2 a b =
    [ ai * bj | ai <- a, bj <- b ]

-- ===========================================================================
-- SIMULAÇÃO DE COMBATE
-- ===========================================================================

data CombatSystem = Combat
    { entityA :: CombatEntity
    , entityB :: CombatEntity
    , jointState :: QVec           -- Estado conjunto (4 amplitudes para 2 qubits)
    , roundNumber :: Int
    } deriving (Show)

-- | Executa uma rodada de "interferência" sem colapso total
interfereRound :: CombatSystem -> CombatSystem
interfereRound combat =
    let
        -- Aplica um CNOT para criar entrelaçamento (A controla B)
        s1 = applyOp cnot (jointState combat)

        -- Aplica Hadamard em A (qubit 0) para aumentar incerteza
        -- H (x) I
        opH_I = tensorProduct hadamard identity2
        s2 = applyOp opH_I s1

        normS = normalize s2
    in combat { jointState = normS, roundNumber = roundNumber combat + 1 }

-- | Calcula probabilidade de vitória (A= |10⟩, B= |01⟩)
victoryProbability :: CombatSystem -> (Double, Double, Double)
victoryProbability Combat{jointState=js} =
    let probs = map (\c -> magnitude c ** 2) js
        -- Estados base do sistema |AB⟩:
        -- Index 0: |00⟩ -> Ninguém vence / Paz
        -- Index 1: |01⟩ -> A=0, B=1 -> Beowulf Vence
        -- Index 2: |10⟩ -> A=1, B=0 -> Filia Vence
        -- Index 3: |11⟩ -> A=1, B=1 -> Aniquilação mútua / Empate
        probB = if length probs > 1 then probs !! 1 else 0 -- Beowulf
        probA = if length probs > 2 then probs !! 2 else 0 -- Filia
        probDraw = (if length probs > 0 then probs !! 0 else 0) +
                   (if length probs > 3 then probs !! 3 else 0)
    in (probA, probB, probDraw)

-- ===========================================================================
-- VISUALIZAÇÃO
-- ===========================================================================

displayState :: QVec -> IO ()
displayState vec = do
    putStrLn "  Estado do Sistema (Amplitudes):"
    zipWithM_
        (\i c ->
            let amp = magnitude c
                phase = atan2 (imagPart c) (realPart c)
                prob = amp ** 2
            in printf "  |%s⟩: %5.3f ∠ %5.2f rad  (Prob: %5.1f%%)\n"
                (showBinary i 2) amp phase (prob * 100)
        )
        [0..] vec
  where
    showBinary n bits =
        let bin = reverse (take bits (reverse (showIntAtBase 2 intToDigit n "")))
            padded = replicate (bits - length bin) '0' ++ bin
        in if null padded then replicate bits '0' else padded

-- ===========================================================================
-- EXEUÇÃO PRINCIPAL
-- ===========================================================================

loopCombat :: Int -> Int -> CombatSystem -> StdGen -> IO ()
loopCombat current total combat gen
    | current > total = putStrLn "\n--- Simulação Finalizada por Limite de Rodadas ---"
    | otherwise = do
        printf "\n[ RODADA %d ]\n" current
        displayState (jointState combat)

        let (pA, pB, pDraw) = victoryProbability combat
        printf "  >> Probabilidades: Filia: %.1f%% | Beowulf: %.1f%% | Indefinido: %.1f%%\n"
               (pA*100) (pB*100) (pDraw*100)

        -- Decisão: Medir (Colapsar) ou Interferir?
        -- Se a probabilidade de alguém for muito alta, colapsa.
        if pA > 0.8 || pB > 0.8
           then do
               putStrLn "  >> LIMIAR DE COERÊNCIA ROMPIDO: COLAPSO IMINENTE <<"
               let (idx, collapsedState) = evalState (measureSystem (jointState combat)) gen
               let winner = case idx of
                              1 -> "BEOWULF (Estado |01⟩)"
                              2 -> "FILIA (Estado |10⟩)"
                              3 -> "EMPATE ENERGÉTICO (|11⟩)"
                              _ -> "SILÊNCIO ABSOLUTO (|00⟩)"

               putStrLn $ "  >> RESULTADO DA MEDIÇÃO: " ++ winner
               putStrLn "  >> A função de onda colapsou."
               displayState collapsedState
           else do
               -- Continua interferência
               let nextCombat = interfereRound combat
               -- Adiciona ruído aleatório ao gerador
               newGen <- newStdGen

               -- Pausa dramática simulada (opcional)
               -- threadDelay 500000

               loopCombat (current + 1) total nextCombat newGen

runQuantumCombat :: Int -> IO ()
runQuantumCombat rounds = do
    putStrLn "\n╔══════════════════════════════════════════════════════════╗"
    putStrLn "║         SISTEMA QUÂNTICO MNEMOSYNE v2.0                 ║"
    putStrLn "║          Simulação de Combate Quântico                  ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"

    -- Inicializa sistema de combate
    -- Combina os estados iniciais: (A x B)
    let initialState = tensorProduct2 (eBaseState samantaFilia) (eBaseState theodoreBeowulf)
    let combat = Combat
            { entityA = samantaFilia
            , entityB = theodoreBeowulf
            , jointState = normalize initialState
            , roundNumber = 0
            }

    gen <- getStdGen
    loopCombat 1 rounds combat gen

main :: IO ()
main = runQuantumCombat 5
