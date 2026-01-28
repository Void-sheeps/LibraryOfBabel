{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Complex
import Data.Word (Word8)
import Data.Bits (testBit)
import System.Random
import Control.Monad (forM_, replicateM, foldM)
import Control.Monad.State
import Text.Printf
import System.Console.ANSI
import Data.List (intercalate, group)

-- ============================================================================
-- 1. DEFINIÇÃO DE TIPOS E ESTRUTURAS
-- ============================================================================

data MetaState = Forma | Representacao | Simbolo | Modelo
  deriving (Show, Eq, Enum, Bounded)

data SystemStatus = Memoria | Condicional | Transmutando | QuantumLeap
  deriving (Show, Eq, Enum, Bounded)

metaStateInfo :: MetaState -> (String, Color, String)
metaStateInfo Forma          = ("◉", Yellow, "FORMA PRIMORDIAL")
metaStateInfo Representacao  = ("⟲", Cyan, "REPRESENTAÇÃO EMERGENTE")
metaStateInfo Simbolo        = ("⚡", Green, "SÍMBOLO RESONANTE")
metaStateInfo Modelo         = ("◆", Magenta, "MODELO CRISTALIZADO")

statusInfo :: SystemStatus -> (String, Color)
statusInfo Memoria      = ("✓", White) -- Using White for Gray fallback
statusInfo Condicional  = ("↻", Cyan)
statusInfo Transmutando = ("🌀", Yellow)
statusInfo QuantumLeap  = ("⚛", Blue)

-- ============================================================================
-- 2. CLASSES (Representadas como Tipos de Dados e Funções)
-- ============================================================================

type ComplexVector = Complex Double

perturb :: ComplexVector -> Double -> Double -> ComplexVector
perturb (r :+ i) dr di = (r + dr) :+ (i + di)

data Cluster = Cluster
  { center :: Double
  , size   :: Int
  , indices :: [Int]
  } deriving (Show)

data Frontier = Frontier
  { bitPattern :: [Bool]
  , tension    :: Double
  , vector     :: ComplexVector
  , coherence  :: Double
  , clusters   :: [Cluster]
  } deriving (Show)

mkFrontier :: [Word8] -> Frontier
mkFrontier bytes = Frontier {..}
  where
    bitPattern = concatMap byteToBits bytes
    byteToBits b = [testBit b i | i <- [7,6..0]]

    tension = fromIntegral (length $ filter id bitPattern) / 32.0

    vector = sum [ (fromIntegral b / 255.0) * exp (0 :+ (fromIntegral i * pi / 4))
                 | (i, b) <- zip [0..] bytes ]

    coherence = if null bitPattern then 0 else
                  let len = min 8 (length bitPattern)
                      matches = length [ () | i <- [0..len-1],
                                         i+8 < length bitPattern,
                                         bitPattern !! i == bitPattern !! (i+8) ]
                  in fromIntegral matches / fromIntegral len

    clusters = detectClusters bitPattern

detectClusters :: [Bool] -> [Cluster]
detectClusters bits = go 0 bits
  where
    go _ [] = []
    go idx (b:bs)
      | b = let (trueBits, rest) = span id (b:bs)
                s = length trueBits
                idxs = [idx .. idx + s - 1]
                c = fromIntegral (sum idxs) / fromIntegral s
            in Cluster c s idxs : go (idx + s) rest
      | otherwise = go (idx + 1) bs

data Concept = Concept
  { index         :: Int
  , name          :: String
  , bytes         :: [Word8]
  , frontier      :: Frontier
  , metaState     :: MetaState
  , entropy       :: Double
  , age           :: Int
  , memory        :: [MetaState]
  , vectorHistory :: [ComplexVector]
  } deriving (Show)

mkConcept :: Int -> String -> [Word8] -> Concept
mkConcept idx nm bs = Concept
  { index = idx
  , name = nm
  , bytes = bs
  , frontier = mkFrontier bs
  , metaState = Forma
  , entropy = magnitude (vector $ mkFrontier bs) * 2
  , age = 0
  , memory = []
  , vectorHistory = []
  }

getTopography :: Concept -> String
getTopography c = concatMap getChar (bitPattern $ frontier c)
  where
    coh = coherence (frontier c)
    depth = floor (coh * 9)
    charMap True  = ["▪", "▫", "□", "■", "▣", "▤", "▥", "▦", "▧", "▨"] !! depth
    charMap False = ["·", "○", "◌", "◎", "●", "◉", "⊙", "◈", "◇", "◆"] !! depth
    getChar b = charMap b

interpretState :: Concept -> String
interpretState c =
  let v = vector (frontier c)
      (plane, dynamics) = case (realPart v > 0, imagPart v > 0) of
        (True, True)   -> ("Plano Emergente", "Coerência Construtiva")
        (True, False)  -> ("Plano Material", "Cristalização")
        (False, True)  -> ("Plano Imaginário", "Ressonância")
        (False, False) -> ("Plano Vacuolar", "Entropia Máxima")
      (_, _, desc) = metaStateInfo (metaState c)
  in printf "[%s | %s]\n%s | ΔH=%.2f | Coerência=%.2f" desc (plane :: String) (dynamics :: String) (entropy c) (coherence $ frontier c)

getStatus :: Concept -> SystemStatus
getStatus c
  | entropy c > 8.0 || coherence (frontier c) < 0.3 = Memoria
  | coherence (frontier c) > 0.8 = Condicional
  | otherwise = Transmutando

-- ============================================================================
-- 3. FUNÇÕES DO SISTEMA
-- ============================================================================

showBanner :: IO ()
showBanner = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "╔═══════════════════════════════════════════════════════════╗"
  putStrLn "║        PIPELINE SEMIÓTICA QUÂNTICA EVOLUTIVA            ║"
  putStrLn "║           Vinicius Moura @void_sheeps - 2026            ║"
  putStrLn "╚═══════════════════════════════════════════════════════════╝"
  setSGR [Reset]
  putStrLn ""

showConceptVisualization :: Concept -> IO ()
showConceptVisualization c = do
  let status = getStatus c
      (sSym, sCol) = statusInfo status
      (mSym, mCol, _) = metaStateInfo (metaState c)
      f = frontier c
      v = vector f

  setSGR [SetColor Foreground Dull White]
  putStr "\n╭─┤ "
  setSGR [SetColor Foreground Vivid White]
  printf "CONCEITO #%d" (index c)
  setSGR [SetColor Foreground Vivid sCol]
  printf " %s" sSym
  setSGR [SetColor Foreground Dull White]
  putStrLn " │"

  putStr "├─"
  setSGR [SetColor Foreground Dull White]
  putStr " Topografia: "
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn (getTopography c)

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Estado: "
  setSGR [SetColor Foreground Vivid mCol]
  printf "%s %s\n" (show $ metaState c) mSym

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Vetor: "
  setSGR [SetColor Foreground Vivid White]
  printf "⟨%.2f + %.2fi⟩\n" (realPart v) (imagPart v)

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Entropia: "
  let eCol = if entropy c > 6 then Red else Green
  setSGR [SetColor Foreground Vivid eCol]
  printf "ΔH=%.2f\n" (entropy c)

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Coerência: "
  let cCol = if coherence f > 0.7 then Green else Yellow
  setSGR [SetColor Foreground Vivid cCol]
  printf "%.2f\n" (coherence f)

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Tensão: "
  let tCol = if tension f > 0.5 then Red else Blue
  setSGR [SetColor Foreground Vivid tCol]
  printf "%.2f\n" (tension f)

  setSGR [SetColor Foreground Dull White]
  putStr "├─"
  putStr " Clusters: "
  setSGR [SetColor Foreground Vivid Magenta]
  printf "%d\n" (length $ clusters f)

  setSGR [SetColor Foreground Dull White]
  putStr "╰─"
  putStr " Interpretação: "
  setSGR [SetColor Foreground Vivid White]
  putStrLn (interpretState c)
  setSGR [Reset]

-- Evolution Logic

evolveConcept :: Concept -> IO Concept
evolveConcept c = do
  -- 1. Transmute
  randJump <- randomRIO (0, 1.0 :: Double)
  let v = vector (frontier c)
  newState <- if randJump < 0.1
              then do
                idx <- randomRIO (0, 3)
                return (toEnum idx)
              else return $ case metaState c of
                Forma -> if realPart v > 0 then Representacao else Modelo
                Representacao -> if imagPart v > 0 then Simbolo else Modelo
                Simbolo -> if magnitude v > 1 then Modelo else Representacao
                Modelo -> Forma

  -- 2. Perturb Vector
  dr <- randomRIO (-0.2, 0.2)
  di <- randomRIO (-0.2, 0.2)
  let newV = perturb v dr di
      newF = (frontier c) { vector = newV }

  -- 3. Entropy Change
  entChange <- randomRIO (0.1, 0.5)
  let status = getStatus c
      newEnt = if status == Memoria then entropy c - entChange else entropy c + entChange
      clampedEnt = max 0.1 (min 10.0 newEnt)

  -- 4. Coherence Change
  cohChange <- randomRIO (-0.05, 0.1)
  let newCoh = max 0 (min 1 (coherence (frontier c) + cohChange))
      finalF = newF { coherence = newCoh }

  return c { metaState = newState
           , frontier = finalF
           , entropy = clampedEnt
           , age = age c + 1
           , memory = take 10 (metaState c : memory c)
           , vectorHistory = take 20 (v : vectorHistory c)
           }

simulateEvolution :: Concept -> Int -> IO Concept
simulateEvolution startConcept ticks = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "\n╔═══════════════════════════════════════════════════════════╗"
  putStrLn "║                 EVOLUÇÃO TEMPORAL                         ║"
  putStrLn "╚═══════════════════════════════════════════════════════════╝"
  setSGR [Reset]

  foldM (\currConcept tick -> do
    setSGR [SetColor Foreground Dull White]
    printf "\n┌─── TICK #%d ───\n" tick

    let oldState = metaState currConcept
        oldEnt = entropy currConcept
        oldCoh = coherence (frontier currConcept)

    nextConcept <- evolveConcept currConcept

    let status = getStatus nextConcept
        (sSym, sCol) = statusInfo status
        (mSym, mCol, _) = metaStateInfo (metaState nextConcept)

    putStr "├─ Transição: "
    setSGR [SetColor Foreground Dull White]
    printf "%s → " (show oldState)
    setSGR [SetColor Foreground Vivid mCol]
    putStrLn (show $ metaState nextConcept)

    setSGR [SetColor Foreground Dull White]
    putStr "├─ Status: "
    setSGR [SetColor Foreground Vivid sCol]
    printf "%s   %s\n" (show status) sSym

    setSGR [SetColor Foreground Dull White]
    putStrLn "├─ Mudanças:"
    putStr "   Entropia: "
    let eCol = if entropy nextConcept > oldEnt then Red else Green
    setSGR [SetColor Foreground Vivid eCol]
    printf "%.2f → %.2f\n" oldEnt (entropy nextConcept)

    setSGR [SetColor Foreground Dull White]
    putStr "   Coerência: "
    let cCol = if coherence (frontier nextConcept) > oldCoh then Green else Yellow
    setSGR [SetColor Foreground Vivid cCol]
    printf "%.2f → %.2f\n" oldCoh (coherence $ frontier nextConcept)

    setSGR [SetColor Foreground Dull White]
    putStrLn "└───"
    setSGR [Reset]

    return nextConcept
    ) startConcept [1..ticks]

showAnalysisReport :: Concept -> IO ()
showAnalysisReport c = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "\n╔═══════════════════════════════════════════════════════════╗"
  putStrLn "║                 RELATÓRIO FINAL                           ║"
  putStrLn "╚═══════════════════════════════════════════════════════════╝"
  setSGR [Reset]

  let status = getStatus c
  putStrLn "\n📊 RESUMO DA EVOLUÇÃO"
  printf "├─ Duração: %d ticks\n" (age c)

  let (mSym, mCol, _) = metaStateInfo (metaState c)
  putStr "├─ Estado Final: "
  setSGR [SetColor Foreground Vivid mCol]
  putStrLn (show $ metaState c)

  setSGR [Reset]
  let (_, sCol) = statusInfo status
  putStr "├─ Status Final: "
  setSGR [SetColor Foreground Vivid sCol]
  putStrLn (show status)

  setSGR [Reset]
  putStr "├─ Entropia Final: "
  let eCol = if entropy c > 8 then Red else if entropy c > 5 then Yellow else Green
  setSGR [SetColor Foreground Vivid eCol]
  printf "ΔH=%.2f\n" (entropy c)

  setSGR [Reset]
  putStr "└─ Coerência Final: "
  let cCol = if coherence (frontier c) > 0.7 then Green else Yellow
  setSGR [SetColor Foreground Vivid cCol]
  printf "%.2f\n" (coherence $ frontier c)

  setSGR [Reset]
  putStrLn "\n📈 HISTÓRICO DE ESTADOS"
  forM_ (zip [1..] (reverse $ memory c)) $ \(i, state) -> do
    let (sym, col, _) = metaStateInfo state
    setSGR [SetColor Foreground Vivid col]
    printf "   %02d. %s %s\n" (i :: Int) sym (show state)

  setSGR [Reset]
  putStrLn "\n🔮 INTERPRETAÇÃO FINAL"
  putStrLn ("   " ++ interpretState c)

  case status of
    Memoria -> do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "\n✅ CONCEITO CRISTALIZADO: Transformação completa alcançada."
      putStrLn "   O sistema alcançou um estado estável de memória."
    Condicional -> do
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "\n🔄 CICLO ATIVO: Evolução continua em fluxo."
      putStrLn "   O sistema mantém coerência e continua a evoluir."
    Transmutando -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "\n🌀 TRANSMUTAÇÃO: Processo transformativo em andamento."
      putStrLn "   O sistema está em fase ativa de transformação."
    QuantumLeap -> do
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn "\n⚛️ SALTO QUÂNTICO: Descontinuidade detectada."
      putStrLn "   O sistema experimentou uma transição não-linear."
  setSGR [Reset]

-- ============================================================================
-- 4. EXECUÇÃO PRINCIPAL
-- ============================================================================

main :: IO ()
main = do
  showBanner

  let datasets =
        [ (0, "Cabeça do Dino", [0, 1, 255, 240])
        , (1, "Padrão de Parede", [0, 85, 85, 255])
        , (2, "Núcleo Simétrico", [0, 60, 60, 0])
        , (3, "Baixa Entropia", [0, 0, 0, 7])
        ]

  putStrLn "FASE 1: ANÁLISE INICIAL DOS CONCEITOS"
  putStrLn "═══════════════════════════════════════════════════════════"

  concepts <- forM datasets $ \(idx, nm, bs) -> do
    let c = mkConcept idx nm bs
    showConceptVisualization c
    return c

  let selectedConcept = head concepts
  finalConcept <- simulateEvolution selectedConcept 8
  showAnalysisReport finalConcept

  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn "\n╔═══════════════════════════════════════════════════════════╗"
  putStrLn "║               ANÁLISE COMPARATIVA                        ║"
  putStrLn "╚═══════════════════════════════════════════════════════════╝"
  setSGR [Reset]

  putStrLn "\nComparação entre conceitos iniciais:"
  forM_ concepts $ \c -> do
    let complexity = if entropy c > 7 then "ALTA" else if entropy c > 4 then "MÉDIA" else ("BAIXA" :: String)
        stability = if coherence (frontier c) > 0.7 then "ESTÁVEL" else if coherence (frontier c) > 0.4 then "VARIÁVEL" else ("INSTÁVEL" :: String)
    printf "   #%d: %s | Complexidade: %s | Estabilidade: %s\n" (index c) (name c) complexity stability

  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "\n⚡ Pipeline semiótica concluída. Sistema pronto para próxima iteração."
  setSGR [Reset]
