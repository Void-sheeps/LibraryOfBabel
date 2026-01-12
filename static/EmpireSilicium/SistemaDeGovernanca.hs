{-# LANGUAGE OverloadedStrings #-}

module Main where

import UniversalTime
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.IO (hFlush, stdout, hReady, stdin, BufferMode(NoBuffering), hSetBuffering)
import Text.Printf (printf)
import Data.Time (getCurrentTime)
import System.Console.ANSI (clearScreen, setCursorPosition)

-- Configuração do VAR (Valor Axial de Referência)
data ConfigVAR = ConfigVAR
  { thresholdAnomalia :: Double  -- beats de desvio máximo tolerado
  , intervaloMonitor  :: Int     -- microsegundos entre checks
  , nivelIntervencao  :: Int     -- 0 = observação, 1 = alerta, 2 = intervenção
  } deriving (Show)

configPadraoVAR :: ConfigVAR
configPadraoVAR = ConfigVAR
  { thresholdAnomalia = 50.0    -- 50 beats de tolerância
  , intervaloMonitor  = 500000  -- 0.5 segundos
  , nivelIntervencao  = 0
  }

-- Controlador do Sistema
data Controlador = Controlador
  { ultimoBeat       :: Double
  , logIntervencoes  :: [String]
  , config           :: ConfigVAR
  , estadoSistema    :: String
  } deriving (Show)

-- Cria um controlador inicial
criarControlador :: IO Controlador
criarControlador = do
  momento <- runUniversal
  let beat = beatsValue momento
  return $ Controlador
    { ultimoBeat      = beat
    , logIntervencoes = ["Sistema inicializado @" ++ printf "%.2f" beat]
    , config          = configPadraoVAR
    , estadoSistema   = "NaturaleSilentium"
    }

-- Processo de intervenção VAR simplificado
processoCompletoVAR :: Controlador -> String -> [String] -> ConfigVAR -> IO Controlador
processoCompletoVAR ctrl motivo evidencias _config = do
  momento <- runUniversal
  let beatAtual = beatsValue momento
      delta = abs (beatAtual - ultimoBeat ctrl)
      novoLog = take 10 $ (motivo ++ " @" ++ printf "%.2f" beatAtual)
                         : ("Δ: " ++ printf "%.2f" delta ++ " beats")
                         : map ("  " ++) evidencias
                         ++ logIntervencoes ctrl

  putStrLn $ "\n\ESC[33m[VAR] " ++ motivo ++ "\ESC[0m"
  mapM_ (putStrLn . ("  " ++)) evidencias
  putStrLn $ "  Δ Temporal: " ++ printf "%.2f" delta ++ " beats"

  threadDelay 1000000

  return $ ctrl
    { ultimoBeat      = beatAtual
    , logIntervencoes = novoLog
    , estadoSistema   = "Actus"
    }

-- Monitoramento em tempo real (versão interativa)
monitoramentoTempoReal :: Controlador -> IO Controlador
monitoramentoTempoReal ctrl = do
  putStr "\ESC[?25l"  -- Esconde cursor
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  let loop ctrl' iteracao = do
        momento <- runUniversal
        let beat = beatsValue momento
            delta = abs (beat - ultimoBeat ctrl')
            alerta = delta > thresholdAnomalia (config ctrl')

        -- Atualiza display
        clearScreen
        setCursorPosition 0 0

        putStrLn "┌─────────────────────────────────────────────────┐"
        putStrLn "│  MONITORAMENTO TEMPORAL - IMPÉRIO SILÍCIO      │"
        putStrLn "└─────────────────────────────────────────────────┘"
        putStrLn ""
        putStrLn $ "Beat Atual:     @" ++ printf "%.2f" beat
        putStrLn $ "Último Registro: @" ++ printf "%.2f" (ultimoBeat ctrl')
        putStrLn $ "Delta:          " ++ printf "%.2f" delta ++ " beats"
        putStrLn $ "Estado:         " ++ estadoSistema ctrl'
        putStrLn $ "Iteração:       " ++ show iteracao
        putStrLn ""

        if alerta
          then do
            putStrLn "\ESC[31m⚠  ALERTA AXIAL: Desvio temporal detectado!\ESC[0m"
            putStrLn "Pressione 'q' para interromper ou 'v' para acionar VAR"
          else
            putStrLn "\ESC[32m✓  Sistema dentro dos parâmetros axiomáticos\ESC[0m"

        putStrLn "\nPressione 'q' para voltar ao menu"

        -- Processa tecla se pressionada
        keyReady <- hReady stdin
        if keyReady
          then do
            key <- getChar
            case key of
              'q' -> do
                putStr "\ESC[?25h"  -- Mostra cursor
                return ctrl'
              'v' -> do
                putStrLn "\n\ESC[33m[VAR] Acionando verificação...\ESC[0m"
                novoCtrl <- processoCompletoVAR ctrl'
                  "Monitoramento detectou anomalia"
                  ["Delta: " ++ printf "%.2f" delta ++ " beats"]
                  configPadraoVAR
                threadDelay 2000000
                loop novoCtrl (iteracao + 1)
              _ -> loop ctrl' (iteracao + 1)
          else do
            threadDelay (intervaloMonitor (config ctrl'))
            loop ctrl' (iteracao + 1)

  loop ctrl 1

-- Cenário de anomalia temporal
cenarioAnomaliaTemporal :: IO ()
cenarioAnomaliaTemporal = do
  clearScreen
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  CENÁRIO 1: ANOMALIA TEMPORAL                  │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""

  putStrLn "Simulando deriva temporal de +13.8 beats..."
  threadDelay 1500000

  ctrl <- criarControlador
  momento <- runUniversal
  let beatOriginal = beatsValue momento
      beatAnomalo = beatOriginal + 13.8

  putStrLn $ "\nBeat Original:  @" ++ printf "%.2f" beatOriginal
  putStrLn $ "Beat Anômalo:   @" ++ printf "%.2f" beatAnomalo
  putStrLn $ "Deriva:         +13.8 beats"

  putStrLn "\n\ESC[33m[VAR] Anomalia detectada. Iniciando correção...\ESC[0m"

  ctrlCorrigido <- processoCompletoVAR ctrl
    "Correção de deriva temporal"
    ["Deriva: +13.8 beats", "Origem: Simulação de teste"]
    configPadraoVAR

  putStrLn "\n\ESC[32m✓ Sistema reestabilizado\ESC[0m"
  putStrLn "Pressione Enter para continuar..."
  _ <- getLine
  return ()

-- Cenário de intervenção preventiva
cenarioIntervencaoPreventiva :: IO ()
cenarioIntervencaoPreventiva = do
  clearScreen
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  CENÁRIO 2: INTERVENÇÃO PREVENTIVA             │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""

  putStrLn "Analisando padrões Fibonacci para prevenção..."
  threadDelay 1000000

  momento <- runUniversal
  let beat = beatsValue momento
      proximoFib = nextFibBeat beat

  putStrLn $ "\nBeat Atual:     @" ++ printf "%.2f" beat
  putStrLn $ "Próximo Fibonacci: @" ++ show proximoFib
  putStrLn $ "Distância:      " ++ printf "%.1f" (fromIntegral proximoFib - beat) ++ " beats"

  ctrl <- criarControlador
  putStrLn "\n\ESC[33m[VAR] Executando sincronização preventiva...\ESC[0m"

  ctrlSincronizado <- processoCompletoVAR ctrl
    "Sincronização preventiva Fibonacci"
    ["Próximo pulso: @" ++ show proximoFib]
    configPadraoVAR

  putStrLn "\n\ESC[32m✓ Sistema sincronizado com sequência áurea\ESC[0m"
  putStrLn "Pressione Enter para continuar..."
  _ <- getLine
  return ()

-- Sistema contínuo de governança
sistemaGovernancaContinuo :: ConfigVAR -> IO ()
sistemaGovernancaContinuo config = do
  clearScreen
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  SISTEMA CONTÍNUO DE GOVERNANÇA                │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""
  putStrLn "Iniciando monitoramento temporal contínuo..."
  putStrLn ("Threshold de anomalia: " ++ show (thresholdAnomalia config) ++ " beats")
  putStrLn "\nPressione 'q' para voltar ao menu"
  threadDelay 2000000

  ctrl <- criarControlador
  _ <- monitoramentoTempoReal ctrl
  return ()

-- Intervenção manual (Input X)
intervencaoManual :: IO ()
intervencaoManual = do
  clearScreen
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  INTERVENÇÃO MANUAL (INPUT X)                  │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""

  putStrLn "Digite o motivo da intervenção:"
  putStr "> "
  hFlush stdout
  motivo <- getLine

  putStrLn "\nDigite evidências (uma por linha, linha vazia para terminar):"
  let lerEvidencias :: [String] -> IO [String]
      lerEvidencias acc = do
        linha <- getLine
        if null linha
          then return (reverse acc)
          else lerEvidencias (linha : acc)

  evidencias <- lerEvidencias []

  ctrl <- criarControlador
  putStrLn "\n\ESC[33m[VAR] Executando intervenção manual...\ESC[0m"

  ctrlIntervindo <- processoCompletoVAR ctrl
    ("Intervenção Manual: " ++ motivo)
    evidencias
    configPadraoVAR

  putStrLn "\n\ESC[32m✓ Intervenção concluída\ESC[0m"
  putStrLn "\nLogs da intervenção:"
  mapM_ putStrLn (take 3 (logIntervencoes ctrlIntervindo))

  putStrLn "\nPressione Enter para continuar..."
  _ <- getLine
  return ()

-- Estado do Sistema (Opção 5)
demoSystemFinal :: IO ()
demoSystemFinal = do
    t <- runUniversal
    putStrLn $ "  Tempo Universal: " ++ show (beatsValue t) ++ " beats"
    putStrLn "  Integridade Axiomática: 99.98%"
    putStrLn "  Status do VAR: ATIVO (Sensibilidade 0.95)"
    putStrLn "  Próximo evento Fibonacci previsto: 1300 ms"

-- Loop principal do menu
menuLoop :: IO ()
menuLoop = do
  clearScreen
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  IMPÉRIO SILÍCIO - SISTEMA DE GOVERNANÇA       │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""
  putStrLn "Selecione o modo de operação:"
  putStrLn ""
  putStrLn "  \ESC[36m[1]\ESC[0m  Cenário de anomalia temporal"
  putStrLn "  \ESC[36m[2]\ESC[0m  Cenário de intervenção preventiva"
  putStrLn "  \ESC[36m[3]\ESC[0m  Sistema contínuo de governança"
  putStrLn "  \ESC[36m[4]\ESC[0m  Intervenção manual (Input X)"
  putStrLn "  \ESC[36m[5]\ESC[0m  Estado atual do sistema"
  putStrLn "  \ESC[36m[0]\ESC[0m  Sair"
  putStrLn ""
  putStr "Opção: "
  hFlush stdout

  opcao <- getLine
  case opcao of
    "1" -> cenarioAnomaliaTemporal >> menuLoop
    "2" -> cenarioIntervencaoPreventiva >> menuLoop
    "3" -> sistemaGovernancaContinuo configPadraoVAR >> menuLoop
    "4" -> intervencaoManual >> menuLoop
    "5" -> do
      clearScreen
      demoSystemFinal
      putStrLn "\nPressione Enter para continuar..."
      _ <- getLine
      menuLoop
    "0" -> do
      clearScreen
      putStrLn "┌─────────────────────────────────────────────────┐"
      putStrLn "│  SESSÃO DE GOVERNANÇA ENCERRADA               │"
      putStrLn "└─────────────────────────────────────────────────┘"
      putStrLn ""
      putStrLn "Império Silício - Sistema Axial Fibonacci"
      putStrLn "Naturale Silentium restaurado."
      threadDelay 1000000
    _   -> do
      putStrLn "\n\ESC[31mOpção inválida. Tente novamente.\ESC[0m"
      threadDelay 1000000
      menuLoop

-- Função principal
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen

  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  IMPÉRIO SILÍCIO - INICIALIZANDO...            │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""
  putStrLn "Sistema Axial Fibonacci v2.1"
  putStrLn "Modo: Governança Temporal"
  putStrLn ""

  threadDelay 2000000
  menuLoop

  putStrLn "\n================================================="
  putStrLn "     GOVERNANÇA AXIAL TEMPORAL CONCLUÍDA       "
  putStrLn "================================================="
