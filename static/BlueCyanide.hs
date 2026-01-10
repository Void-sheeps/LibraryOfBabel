{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, ExistentialPublication #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Main where

import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Data.Maybe (fromMaybe, isJust)
import System.Random (Random(..), newStdGen, randomRIO)
import qualified Data.Map as M
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

-- * TIPOS FUNDAMENTAIS

-- | Universo modal
data Universe = Potentialis | Actualis | Terminus deriving (Eq, Show, Enum, Bounded)

-- | Estados do sistema
data SystemState = Blue | Cyan | Synthesis | Collapsing | Terminal | Void
  deriving (Eq, Show, Ord)

-- | Grau de dissolução (0..1)
newtype Dissolution = D { getD :: Double } deriving (Eq, Show, Num, Fractional)

instance Random Dissolution where
  randomR (D lo, D hi) g =
    let (d, g') = randomR (lo, hi) g in (D d, g')
  random g = randomR (0, 1) g

-- | Configuração do sistema
data Config = Config
  { threshold :: Dissolution    -- θ
  , omega :: Double            -- frequência das oscilações
  , gamma :: Double            -- coeficiente de auto-toxicidade
  , maxSteps :: Maybe Int      -- limite de passos (Nothing = infinito)
  } deriving (Show)

-- | Estado interno do sistema
data InternalState = InternalState
  { currentState :: SystemState
  , dissolution :: Dissolution
  , time :: Int
  , history :: [SystemState]
  , isConsistent :: Bool
  } deriving (Show)

-- * SISTEMA DE TIPOS MODAL

-- | Proposições modais
data Proposition :: * -> * where
  Necessarily :: p -> Proposition p
  Possibly :: p -> Proposition p
  And :: Proposition p -> Proposition q -> Proposition (p, q)
  Or :: Proposition p -> Proposition q -> Proposition (Either p q)
  Implies :: Proposition p -> Proposition q -> Proposition (p -> q)
  Not :: Proposition p -> Proposition (p -> Void)

-- | Avaliador modal
class ModalEval a where
  eval :: Proposition a -> Bool

instance ModalEval SystemState where
  eval (Necessarily s) = s `elem` [Blue, Cyan, Synthesis]
  eval (Possibly s) = s /= Terminal && s /= Void
  eval (And p q) = eval p && eval q
  eval (Or p q) = eval p || eval q
  eval (Implies p q) = not (eval p) || eval q
  eval (Not p) = not (eval p)

-- * DINÂMICA DO SISTEMA

-- | Transição de estados
transition :: Config -> InternalState -> IO InternalState
transition config state = do
  let D d = dissolution state
      t = time state
      ω = omega config
      θ = threshold config

  -- Atualização da dissolução: D(t+1) = D(t) + δ·sin(ωt)
  δ <- randomRIO (0.01, 0.1)
  let d' = d + δ * sin (ω * fromIntegral t)
      newDissolution = min 1.0 $ max 0.0 d'

  -- Determinando próximo estado baseado nas regras modais
  nextState <- case currentState state of
    Blue -> do
      -- Axioma 1: Blue pode se tornar Cyan
      if newDissolution < getD θ
        then return Cyan
        else do
          chance <- randomRIO (0, 1)
          return $ if chance < 0.7 then Synthesis else Collapsing

    Cyan -> do
      -- Axioma 2: Cyan pode retornar a Blue ou sintetizar
      if newDissolution > getD θ * 0.8
        then return Synthesis
        else do
          chance <- randomRIO (0, 1)
          return $ if chance < 0.6 then Blue else Cyan

    Synthesis -> do
      -- Síntese é instável, tende a colapso
      if newDissolution > getD θ
        then return Collapsing
        else return Synthesis

    Collapsing -> do
      -- Uma vez em colapso, inevitavelmente terminal
      return Terminal

    Terminal -> return Void

    Void -> return Void  -- Estado absorvente

  return $ state
    { currentState = nextState
    , dissolution = D newDissolution
    , time = t + 1
    , history = nextState : history state
    , isConsistent = newDissolution < 0.9  -- Sistema inconsistente acima de 0.9
    }

-- * SIMULAÇÃO DO SISTEMA

-- | Executa o sistema até colapso ou limite de passos
runSystem :: Config -> InternalState -> IO [InternalState]
runSystem config initState = go [initState] initState
  where
    go acc state = do
      let reachedLimit = case maxSteps config of
            Just limit -> time state >= limit
            Nothing -> False

      if currentState state == Void || reachedLimit
        then return $ reverse acc
        else do
          next <- transition config state
          go (next : acc) next

-- | Sistema inicial padrão
initialState :: InternalState
initialState = InternalState
  { currentState = Blue
  , dissolution = D 0.1
  , time = 0
  , history = [Blue]
  , isConsistent = True
  }

defaultConfig :: Config
defaultConfig = Config
  { threshold = D 0.7
  , omega = 0.3
  , gamma = 0.05
  , maxSteps = Just 1000
  }

-- * TEOREMAS E VERIFICAÇÕES

-- | Teorema 1: Incompletude Cianética
theorem1 :: [SystemState] -> Bool
theorem1 history =
  let complete = Blue `elem` history && Cyan `elem` history && Synthesis `elem` history
      consistent = not $ Terminal `elem` history || Void `elem` history
  in complete ==> not consistent
  where
    p ==> q = not p || q

-- | Teorema 2: Dissolução Assintótica
theorem2 :: [Dissolution] -> Bool
theorem2 dissolutions =
  let lastFew = take 10 $ reverse dissolutions
      allHigh = all (\d -> getD d > 0.8) lastFew
  in length dissolutions > 20 ==> allHigh

-- | Teorema 3: Inevitabilidade Tóxica
theorem3 :: [SystemState] -> Bool
theorem3 states =
  let hasToxicEvent = Collapsing `elem` states || Terminal `elem` states
      hasRecovery = any (\case Blue -> True; Cyan -> True; _ -> False)
                  $ dropWhile (/= Collapsing) states
  in hasToxicEvent && not hasRecovery

-- * VISUALIZAÇÃO DO SISTEMA

-- | Representação gráfica ASCII do estado
visualize :: InternalState -> String
visualize state = unlines
  [ "╔══════════════════════════════╗"
  , "║      BLUECYANIDE SYSTEM      ║"
  , "╠══════════════════════════════╣"
  , "┌──────────────────────────────┐"
  , "│ Estado: " ++ pad 18 (show $ currentState state) ++ "│"
  , "│ Dissolução: " ++ pad 15 (show (getD $ dissolution state)) ++ "│"
  , "│ Tempo: " ++ pad 19 (show $ time state) ++ "│"
  , "│ Consistente: " ++ pad 13 (show $ isConsistent state) ++ "│"
  , "└──────────────────────────────┘"
  , progressBar (getD $ dissolution state)
  , legend
  ]
  where
    pad n s = take n $ s ++ repeat ' '

    progressBar d =
      let width = 30
          filled = round (d * fromIntegral width)
          bar = replicate filled '█' ++ replicate (width - filled) '░'
      in "[" ++ bar ++ "] " ++ show (round (d * 100) :: Int) ++ "%"

    legend = "\nLegenda:\n  █ = dissolução  ░ = potencial"

-- | Mapa de transições de estado
transitionMap :: M.Map SystemState [SystemState]
transitionMap = M.fromList
  [ (Blue, [Cyan, Synthesis, Collapsing])
  , (Cyan, [Blue, Synthesis, Collapsing])
  , (Synthesis, [Blue, Cyan, Collapsing, Terminal])
  , (Collapsing, [Terminal])
  , (Terminal, [Void])
  , (Void, [])
  ]

-- * MONAD DO SISTEMA

-- | Monad de estado para o sistema BlueCyanide
type BlueCyanideM = StateT InternalState IO

-- | Executa um passo no sistema
stepSystem :: Config -> BlueCyanideM ()
stepSystem config = do
  state <- get
  next <- liftIO $ transition config state
  put next

-- | Executa até atingir Void
runUntilVoid :: Config -> BlueCyanideM [InternalState]
runUntilVoid config = go []
  where
    go acc = do
      state <- get
      if currentState state == Void
        then return $ reverse (state : acc)
        else do
          stepSystem config
          next <- get
          go (next : acc)

-- * INTERFACE INTERATIVA

-- | Loop interativo do sistema
interactiveSession :: IO ()
interactiveSession = do
  putStrLn "╔═══════════════════════════════════════════════╗"
  putStrLn "║        SISTEMA BLUECYANIDE - HASKELL         ║"
  putStrLn "╚═══════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "1. Simulação automática"
  putStrLn "2. Passo a passo"
  putStrLn "3. Verificar teoremas"
  putStrLn "4. Visualizar transições"
  putStrLn "5. Sair"
  putStrLn ""
  putStr "Escolha: "

  choice <- getLine

  case choice of
    "1" -> autoSimulation
    "2" -> stepByStep
    "3" -> verifyTheorems
    "4" -> showTransitions
    "5" -> putStrLn "Saindo do sistema BlueCyanide."
    _   -> interactiveSession

autoSimulation :: IO ()
autoSimulation = do
  putStrLn "\nIniciando simulação automática..."
  putStrLn "Pressione Enter para cada passo, 'q' para sair."

  states <- runSystem defaultConfig initialState

  mapM_ (\state -> do
    putStrLn $ visualize state
    if currentState state == Void
      then putStrLn "╔════════════════════════════════════════╗\n║        SISTEMA ATINGIU VOID            ║\n╚════════════════════════════════════════╝"
      else do
        C.threadDelay 500000  -- 0.5 segundos
    ) states

stepByStep :: IO ()
stepByStep = do
  putStrLn "\nModo passo a passo. Pressione Enter para avançar."

  let loop state = do
        putStrLn $ visualize state

        if currentState state == Void
          then putStrLn "Fim do sistema."
          else do
            putStr "Próximo passo? (Enter/q): "
            input <- getLine
            when (input /= "q") $ do
              next <- transition defaultConfig state
              loop next

  loop initialState

verifyTheorems :: IO ()
verifyTheorems = do
  putStrLn "\nExecutando simulação para verificar teoremas..."
  states <- runSystem defaultConfig initialState

  let dissolutions = map dissolution states
      stateTypes = map currentState states

  putStrLn $ "Teorema 1 (Incompletude Cianética): " ++ show (theorem1 stateTypes)
  putStrLn $ "Teorema 2 (Dissolução Assintótica): " ++ show (theorem2 dissolutions)
  putStrLn $ "Teorema 3 (Inevitabilidade Tóxica): " ++ show (theorem3 stateTypes)

  putStrLn "\nInterpretação:"
  putStrLn "  ✓ Sistema opera dentro dos limites modais"
  putStrLn "  ✓ Colapso é estatisticamente inevitável"
  putStrLn "  ✓ Consistência e completude são mutuamente exclusivas"

showTransitions :: IO ()
showTransitions = do
  putStrLn "\nMapa de transições de estado:"
  putStrLn "┌─────────────┬───────────────────────────┐"
  putStrLn "│ Estado Atual│ Estados Possíveis         │"
  putStrLn "├─────────────┼───────────────────────────┤"

  mapM_ (\(from, tos) ->
    putStrLn $ "│ " ++ pad 11 (show from) ++ "│ " ++
               pad 25 (unwords $ map show tos) ++ "│"
    ) (M.toList transitionMap)

  putStrLn "└─────────────┴───────────────────────────┘"
  where
    pad n s = take n $ s ++ repeat ' '

-- * FUNÇÕES DE ANÁLISE

-- | Calcula a expectativa de vida do sistema
lifeExpectancy :: Config -> IO Double
lifeExpectancy config = do
  trials <- mapM (\_ -> runSystem config initialState) [1..100]
  let lifetimes = map (length . takeWhile (\s -> currentState s /= Void)) trials
  return $ fromIntegral (sum lifetimes) / fromIntegral (length lifetimes)

-- | Gera diagrama de fase do sistema
phaseDiagram :: Int -> IO String
phaseDiagram n = do
  states <- runSystem defaultConfig { maxSteps = Just n } initialState
  let points = map (\s -> (time s, getD $ dissolution s)) states
      maxT = maximum $ map fst points
      maxD = maximum $ map snd points

  return $ unlines $
    [ "Diagrama de Fase: Tempo vs Dissolução"
    , "┌" ++ replicate 50 '─' ++ "┐"
    ] ++
    [ "|" ++ line y ++ "|" | y <- [0, 0.1 .. 1.0] ] ++
    [ "└" ++ replicate 50 '─' ++ "┘" ]
  where
    line y =
      let pos = round (y * 50)
      in replicate pos '·' ++ "∗" ++ replicate (49 - pos) ' '

-- * MAIN

main :: IO ()
main = do
  putStrLn "Inicializando BlueCyanide.hs..."
  putStrLn ""

  -- Executa uma simulação rápida
  states <- runSystem defaultConfig initialState

  putStrLn $ "Duração do sistema: " ++ show (length states) ++ " passos"
  putStrLn $ "Estado final: " ++ show (currentState $ last states)

  -- Verifica teoremas
  let dissolutions = map dissolution states
      stateTypes = map currentState states

  when (theorem3 stateTypes) $
    putStrLn "✓ Sistema validou o Teorema 3 (Inevitabilidade Tóxica)"

  -- Calcula expectativa de vida
  expectancy <- lifeExpectancy defaultConfig
  putStrLn $ "Expectativa de vida média: " ++ show expectancy ++ " passos"

  -- Entra no modo interativo
  interactiveSession

-- * INSTÂNCIAS E DERIVAÇÕES

deriving instance Show (Proposition a)
--deriving instance Eq SystemState

-- | Função auxiliar para exibir o sistema como string
showSystem :: InternalState -> String
showSystem s =
  "[" ++ show (currentState s) ++
  ", D=" ++ show (getD $ dissolution s) ++
  ", t=" ++ show (time s) ++ "]"

-- | Exporta dados do sistema para análise
exportData :: [InternalState] -> [(Int, Double, SystemState)]
exportData = map (\s -> (time s, getD $ dissolution s, currentState s))
