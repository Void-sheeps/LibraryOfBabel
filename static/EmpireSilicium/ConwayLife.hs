{-# LANGUAGE OverloadedStrings #-}

module ConwayLife (mainConway) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, when, foldM)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering), hReady, stdin, hFlush)
import Data.List (nub, intersect, sort)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Text.Printf (printf)
import System.Random (randomRIO, getStdGen, StdGen, split, randomR)
import System.Console.ANSI
  ( clearScreen
  , setCursorPosition
  , setSGR
  , SGR(SetColor, Reset)
  , ConsoleLayer(Foreground, Background)
  , Color(..)
  )

-- ==========================================
-- 1. O NÚCLEO LÓGICO EXPANDIDO
-- ==========================================

-- O tabuleiro com estado temporal
data BoardState = BoardState
  { board      :: [Pos]
  , generation :: Int
  , birthTime  :: Maybe UTCTime
  , entropy    :: Double  -- Medida de complexidade
  , fibonacciPulse :: Int -- Pulsos Fibonacci desde o início
  } deriving (Show)

type Pos = (Int, Int)

-- Dimensões de visualização dinâmica
dynamicWidth, dynamicHeight :: Int
dynamicWidth = 60
dynamicHeight = 30

-- Padrões axiomáticos do Império Silício
patterns :: [(String, [Pos])]
patterns =
  [ ("Glider", glider)
  , ("R-Pentomino", rPentomino)
  , ("Gosper Glider Gun", gosperGliderGun)
  , ("Simkin Glider Gun", simkinGliderGun)
  , ("Fibonacci Spiral", fibonacciSpiral)
  , ("Golden Ratio", goldenRatioPattern)
  , ("Empty", [])
  ]

glider :: [Pos]
glider = [(1,0), (2,1), (0,2), (1,2), (2,2)]

rPentomino :: [Pos]
rPentomino = [(10,10), (11,10), (9,11), (10,11), (10,12)]

gosperGliderGun :: [Pos]
gosperGliderGun =
  [(1,5),(2,5),(1,6),(2,6)] ++
  [(11,5),(11,6),(11,7),(12,4),(12,8),(13,3),(13,9),(14,3),(14,9),
   (15,6),(16,4),(16,8),(17,5),(17,6),(17,7),(18,6)] ++
  [(21,3),(21,4),(21,5),(22,3),(22,4),(22,5),(23,2),(23,6),
   (25,1),(25,2),(25,6),(25,7)]

simkinGliderGun :: [Pos]
simkinGliderGun =
  [(0,0),(1,0),(0,1),(1,1),(10,0),(10,1),(10,2),(11,0),(12,1)]

-- Padrão baseado na sequência Fibonacci
fibonacciSpiral :: [Pos]
fibonacciSpiral = take 100 $ spiral 0 0 1 1 0
  where
    spiral x y a b n
      | n >= 100 = []
      | otherwise =
          let radius = fromIntegral (a + b)
              angle = 2 * pi * fromIntegral n / goldenRatio
              newX = x + round (radius * cos angle)
              newY = y + round (radius * sin angle)
          in (newX, newY) : spiral newX newY b (a+b) (n+1)

    goldenRatio = (1 + sqrt 5) / 2

-- Padrão baseado na proporção áurea
goldenRatioPattern :: [Pos]
goldenRatioPattern =
  [(round (phi * fromIntegral i), round (phi^2 * fromIntegral i)) | i <- [0..20]] ++
  [(round (phi^2 * fromIntegral i), round (phi * fromIntegral i)) | i <- [0..20]]
  where phi = 1.618033988749895

-- ==========================================
-- 2. REGRAS DE CONWAY COM MODIFICAÇÕES TEMPORAIS
-- ==========================================

vizinhos :: Pos -> [Pos]
vizinhos (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]

vizinhosVivos :: [Pos] -> Pos -> Int
vizinhosVivos b p = length (filter (`elem` b) (vizinhos p))

-- Regra de Conway com modulação Fibonacci
proximaGeracao :: BoardState -> IO BoardState
proximaGeracao (BoardState b gen birth ent fib) = do
  -- Aplica regras de Conway
  let sobreviventes = [p | p <- b, let n = vizinhosVivos b p, n == 2 || n == 3]
      candidatos = nub (concatMap vizinhos b)
      nascimentos = [p | p <- candidatos, not (p `elem` b), vizinhosVivos b p == 3]
      newBoard = nub (sobreviventes ++ nascimentos)

  -- Calcula entropia (medida de complexidade)
  let entropia = calcularEntropia newBoard

  -- Verifica pulso Fibonacci
  currentTime <- getCurrentTime
  let isFibonacciGen = isFibonacci gen
      newFib = if isFibonacciGen then fib + 1 else fib

  -- Aplica regras especiais em gerações Fibonacci
  let finalBoard = if isFibonacciGen
                   then aplicarRegraFibonacci newBoard gen
                   else newBoard

  return $ BoardState finalBoard (gen + 1) birth entropia newFib

-- Verifica se um número é Fibonacci
isFibonacci :: Int -> Bool
isFibonacci n = n `elem` takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Aplica regras especiais durante pulsos Fibonacci
aplicarRegraFibonacci :: [Pos] -> Int -> [Pos]
aplicarRegraFibonacci b gen
  | gen `mod` 8 == 0 = rotacionarPadrao b  -- Rotaciona a cada 8º Fibonacci
  | gen `mod` 5 == 0 = espelharPadrao b    -- Espelha a cada 5º Fibonacci
  | otherwise        = b

rotacionarPadrao :: [Pos] -> [Pos]
rotacionarPadrao = map (\(x,y) -> (-y, x))

espelharPadrao :: [Pos] -> [Pos]
espelharPadrao = map (\(x,y) -> (-x, y))

-- Calcula entropia do sistema
calcularEntropia :: [Pos] -> Double
calcularEntropia b
  | null b = 0.0
  | otherwise =
      let total = length b
          (minX, maxX) = (minimum xs, maximum xs)
          (minY, maxY) = (minimum ys, maximum ys)
          width = maxX - minX + 1
          height = maxY - minY + 1
          density = fromIntegral total / fromIntegral (width * height)
      in -density * log density
  where
    (xs, ys) = unzip b

-- ==========================================
-- 3. VISUALIZAÇÃO AVANÇADA COM CORES
-- ==========================================

data RenderMode = ASCII | Unicode | Colorful | Minimal deriving (Show, Eq)

renderBoard :: BoardState -> RenderMode -> String
renderBoard (BoardState b gen _ ent fib) mode =
  let border = replicate (dynamicWidth + 2) '═'
      header = printf "║ Geração: %6d | Entropia: %5.3f | Fibonacci: %2d ║" gen ent fib
      grid = [ [cellChar (x,y) mode b | x <- [-dynamicWidth `div` 2 .. dynamicWidth `div` 2]]
               | y <- [-dynamicHeight `div` 2 .. dynamicHeight `div` 2] ]
      gridStr = unlines $ map (\row -> "║" ++ row ++ "║") grid
  in "╔" ++ border ++ "╗\n" ++ header ++ "\n" ++ gridStr ++ "╚" ++ border ++ "╝\n"

cellChar :: Pos -> RenderMode -> [Pos] -> Char
cellChar p mode board
  | p `elem` board = case mode of
      ASCII    -> '◉'
      Unicode  -> '█'
      Colorful -> '◆'
      Minimal  -> '·'
  | otherwise = case mode of
      ASCII    -> ' '
      Unicode  -> '░'
      Colorful -> ' '
      Minimal  -> ' '

-- ==========================================
-- 4. SISTEMA DE GOVERNANÇA CONWAY
-- ==========================================

-- Monitora padrões emergentes
data EmergentPattern = Oscillator | Spaceship | StillLife | Chaos deriving (Show)

detectPattern :: BoardState -> EmergentPattern
detectPattern (BoardState b gen _ ent _)
  | ent < 0.1  = StillLife
  | ent > 0.7  = Chaos
  | isOscillator b = Oscillator
  | otherwise = Spaceship

isOscillator :: [Pos] -> Bool
isOscillator b =
  let period = findPeriod b 10
  in period > 1 && period <= 10

findPeriod :: [Pos] -> Int -> Int
findPeriod b maxPeriod = go b 1
  where
    go current period
      | period > maxPeriod = 0
      | otherwise =
          let next = proximaGeracaoSimples current
          in if next == b then period else go next (period + 1)

proximaGeracaoSimples :: [Pos] -> [Pos]
proximaGeracaoSimples b = nub (sobreviventes ++ nascimentos)
  where
    sobreviventes = [p | p <- b, let n = vizinhosVivos b p, n == 2 || n == 3]
    candidatos = nub (concatMap vizinhos b)
    nascimentos = [p | p <- candidatos, not (p `elem` b), vizinhosVivos b p == 3]

-- ==========================================
-- 5. LOOP DE SIMULAÇÃO INTERATIVO
-- ==========================================

conwaySimulation :: BoardState -> RenderMode -> IO ()
conwaySimulation initialState mode = do
  clearScreen
  setCursorPosition 0 0

  let loop state (iteration :: Int) = do
        -- Renderiza
        setCursorPosition 0 0
        putStrLn $ renderBoard state mode

        -- Mostra informações
        setCursorPosition (dynamicHeight + 5) 0
        putStrLn $ printf "Iteração: %d | Células vivas: %d" iteration (length (board state))
        putStrLn $ "Padrão detectado: " ++ show (detectPattern state)

        -- Controles
        putStrLn $ "\n[SPACE] Pausar | [R] Reiniciar | [M] Mudar modo | [Q] Sair"
        putStrLn $ "Modo atual: " ++ show mode

        -- Verifica input
        threadDelay 50000
        inputAvailable <- hReady stdin
        if inputAvailable
          then do
            key <- getChar
            case key of
              ' ' -> do
                putStrLn "\nPausado. Pressione qualquer tecla para continuar..."
                _ <- getChar
                loop state iteration
              'r' -> do
                putStrLn "\nReiniciando..."
                mainConway
              'm' -> do
                let nextMode = case mode of
                      ASCII -> Unicode
                      Unicode -> Colorful
                      Colorful -> Minimal
                      Minimal -> ASCII
                putStrLn $ "\nMudando para modo: " ++ show nextMode
                threadDelay 1000000
                conwaySimulation state nextMode
              'q' -> do
                putStrLn "\nEncerrando simulação..."
                return ()
              _ -> do
                nextState <- proximaGeracao state
                loop nextState (iteration + 1)
          else do
            nextState <- proximaGeracao state
            loop nextState (iteration + 1)

  loop initialState 0

-- ==========================================
-- 6. INTEGRAÇÃO COM O IMPÉRIO SILÍCIO
-- ==========================================

-- Cria um padrão baseado no tempo atual Fibonacci
createFibonacciBoard :: UTCTime -> IO BoardState
createFibonacciBoard now = do
  let posixSeconds = utcTimeToPOSIXSeconds now
      sec = fromIntegral (floor posixSeconds `mod` 86400)
      beat = sec / 86.4
      fibNum = round beat `mod` 20

  -- Gera padrão baseado no número Fibonacci
  let pattern = case fibNum `mod` 5 of
        0 -> glider
        1 -> rPentomino
        2 -> gosperGliderGun
        3 -> fibonacciSpiral
        _ -> goldenRatioPattern

      offset = (fibNum * 5, fibNum * 3)
      shiftedPattern = map (\(x,y) -> (x + fst offset, y + snd offset)) pattern

  currentTime <- getCurrentTime
  return $ BoardState
    { board = shiftedPattern
    , generation = 0
    , birthTime = Just currentTime
    , entropy = calcularEntropia shiftedPattern
    , fibonacciPulse = fibNum
    }

-- ==========================================
-- 7. MENU PRINCIPAL CONWAY
-- ==========================================

mainConway :: IO ()
mainConway = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen

  putStrLn "╔════════════════════════════════════════════════╗"
  putStrLn "║   SISTEMA CONWAY - IMPÉRIO SILÍCIO            ║"
  putStrLn "║   Autômato Celular Axiomático                 ║"
  putStrLn "╚════════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "Selecione o padrão inicial:"
  putStrLn ""
  putStrLn "  [1] Glider (Viagem eterna)"
  putStrLn "  [2] R-Pentomino (Caos controlado)"
  putStrLn "  [3] Gosper Glider Gun (Fábrica de gliders)"
  putStrLn "  [4] Fibonacci Spiral (Padrão áureo)"
  putStrLn "  [5] Tempo atual Fibonacci"
  putStrLn "  [6] Aleatório"
  putStrLn ""
  putStrLn "  [0] Voltar ao Império Silício"
  putStr ""
  hFlush stdout

  choice <- getLine
  currentTime <- getCurrentTime

  initialState <- case choice of
        "1" -> return $ BoardState glider 0 (Just currentTime) 0.0 0
        "2" -> return $ BoardState rPentomino 0 (Just currentTime) 0.0 0
        "3" -> return $ BoardState gosperGliderGun 0 (Just currentTime) 0.0 0
        "4" -> return $ BoardState fibonacciSpiral 0 (Just currentTime) 0.0 0
        "5" -> createFibonacciBoard currentTime
        "6" -> do
          randomBoard <- generateRandomBoard 50
          return $ BoardState randomBoard 0 (Just currentTime) 0.0 0
        _   -> return $ BoardState [] 0 Nothing 0.0 0

  case choice of
    "0" -> putStrLn "Voltando ao menu principal..."
    _   -> do
      conwaySimulation initialState Unicode
      mainConway

-- Gera um tabuleiro aleatório
generateRandomBoard :: Int -> IO [Pos]
generateRandomBoard n = do
  g <- getStdGen
  let positions = take n $ randomPositions g
  return positions
  where
    randomPositions :: StdGen -> [Pos]
    randomPositions gen =
      let (x, gen1) = randomR (-20, 20) gen
          (y, gen2) = randomR (-15, 15) gen1
          (rest, _) = split gen2
      in (x, y) : randomPositions rest
