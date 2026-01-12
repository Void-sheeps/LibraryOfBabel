{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout, hReady, stdin)
import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- =================================================================
-- 1. MÓDULO UNIVERSAL TIME & TIPOS (Simulação)
-- =================================================================

data UniversalTime = UniversalTime { beatsValue :: Double }

-- Configuração do VAR (Verificação Assistida por Robô)
data ConfigVAR = ConfigVAR { sensibilidade :: Double }

configPadraoVAR :: ConfigVAR
configPadraoVAR = ConfigVAR { sensibilidade = 0.95 }

-- O Controlador do Sistema (Estado)
data Controlador = Controlador {
    idSessao        :: String,
    pulsosFibonacci :: [Int],
    ultimoBeat      :: Double,
    logIntervencoes :: [String]
}

-- Cria um controlador inicial
criarControlador :: IO Controlador
criarControlador = do
    return $ Controlador {
        idSessao = "CTRL-GENESIS-01",
        pulsosFibonacci = [89, 144, 233, 377, 610, 987, 1597], -- Sequência Fibonacci
        ultimoBeat = 0.0,
        logIntervencoes = []
    }

-- Simula a leitura do Tempo Universal
runUniversal :: IO UniversalTime
runUniversal = do
    t <- getPOSIXTime
    -- Converte tempo real em "Beats" (simulação)
    let beat = fromIntegral (floor (toRational t * 10)) / 10.0
    return $ UniversalTime (beat - (fromIntegral (floor (beat / 10000)) * 10000))

-- Função auxiliar para gerar agenda Fibonacci
fibSchedule :: Double -> Int -> [Int]
fibSchedule _ n = take n [1597, 2584, 4181, 6765, 10946] -- Mock de futuros pulsos

-- =================================================================
-- 2. LÓGICA DE NEGÓCIO (Cenários)
-- =================================================================

-- Processo do VAR (atualiza o controlador e gera logs)
processoCompletoVAR :: Controlador -> String -> [String] -> ConfigVAR -> IO Controlador
processoCompletoVAR ctrl motivo evidencias _ = do
    putStrLn "\n┌──────────────────────────────────────────┐"
    putStrLn $ "│ [VAR] ⚠ ANÁLISE INICIADA: " ++ motivo
    putStrLn "└──────────────────────────────────────────┘"

    mapM_ (\e -> putStrLn $ "   ↳ Evidência: " ++ e) evidencias

    putStr "   Processando correção axiomática... "
    hFlush stdout
    threadDelay 800000 -- Simula processamento
    putStrLn "OK."

    m <- runUniversal
    let novoBeat = beatsValue m

    putStrLn $ "   ✓ Ajuste temporal aplicado. Beat atual: " ++ show novoBeat

    -- Retorna controlador atualizado com o log
    return $ ctrl {
        ultimoBeat = novoBeat,
        logIntervencoes = motivo : (logIntervencoes ctrl)
    }

monitoramentoTempoReal :: Controlador -> IO ()
monitoramentoTempoReal _ = do
    putStrLn ">> [MONITOR] Sistema estável. Sincronização mantida."
    threadDelay 500000

-- Cenário 1
cenarioAnomaliaTemporal :: IO ()
cenarioAnomaliaTemporal = do
  putStrLn "\n=== CENÁRIO 1: DETECÇÃO DE ANOMALIA TEMPORAL ==="
  ctrl <- criarControlador
  momento <- runUniversal
  let beat = beatsValue momento

  let evidencias =
        [ "Fibonacci @" ++ show (floor beat) ++ " não corresponde ao padrão áureo"
        , "Derivação temporal: 0.0042 beats acima do esperado"
        ]

  _ <- processoCompletoVAR ctrl "Anomalia na sequência Fibonacci" evidencias configPadraoVAR
  return ()

-- Cenário 2
cenarioIntervencaoPreventiva :: IO ()
cenarioIntervencaoPreventiva = do
  putStrLn "\n=== CENÁRIO 2: INTERVENÇÃO PREVENTIVA ==="
  ctrl <- criarControlador

  -- Simulação matemática
  let razaoSimulada = 1.625 :: Double -- Um pouco fora de 1.618
      evidencias =
        [ "Razão Fibonacci atual: " ++ printf "%.5f" razaoSimulada
        , "Desvio da razão áurea: " ++ printf "%.5f" (abs(razaoSimulada - 1.6180339887))
        , "Tendência de divergência detectada"
        ]

  if abs(razaoSimulada - 1.6180339887) > 0.001
    then do
      putStrLn ">> [ALERTA] Desvio significativo detectado."
      _ <- processoCompletoVAR ctrl "Desvio crítico na razão Fibonacci" evidencias configPadraoVAR
      return ()
    else
      putStrLn ">> [STATUS] Sistema dentro dos parâmetros axiomáticos."

-- Sistema Contínuo (Opção 3)
sistemaGovernancaContinuo :: ConfigVAR -> IO ()
sistemaGovernancaContinuo config = do
  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  SISTEMA DE GOVERNANÇA CONTÍNUO - (CTRL+C para sair) │"
  putStrLn "└─────────────────────────────────────────────────┘"

  ctrlInicial <- criarControlador

  -- Loop limitado a 3 ciclos para não prender o menu indefinidamente nesta demo
  let loop ctrl ciclo = do
        if ciclo > 3
            then putStrLn "\n>> [INFO] Ciclo de demonstração contínua finalizado."
            else do
                momento <- runUniversal
                let beat = beatsValue momento
                putStrLn $ "\n--- Ciclo " ++ show ciclo ++ " @" ++ printf "%.2f" beat ++ " ---"

                -- Randomiza intervenção para demonstração
                let precisaIntervir = (floor beat `mod` 2 == 0)

                ctrlProximo <- if precisaIntervir
                  then do
                    putStrLn ">> [GOVERNANÇA] Micro-flutuação detectada."
                    processoCompletoVAR ctrl "Ajuste Fino Automático" [] config
                  else do
                    monitoramentoTempoReal ctrl
                    return ctrl

                threadDelay 1500000
                loop ctrlProximo (ciclo + 1)

  loop ctrlInicial 1

-- Estado do Sistema (Opção 5)
demoSystemFinal :: IO ()
demoSystemFinal = do
    t <- runUniversal
    putStrLn $ "  Tempo Universal: " ++ show (beatsValue t) ++ " beats"
    putStrLn "  Integridade Axiomática: 99.98%"
    putStrLn "  Status do VAR: ATIVO (Sensibilidade 0.95)"
    putStrLn "  Próximo evento Fibonacci previsto: 1300 ms"

-- Funções auxiliares de verificação simuladas
verificaCondicaoAnomalia :: Double -> IO Bool
verificaCondicaoAnomalia beat = return (beat > 500) -- Mock

verificaCondicaoPreventiva :: Controlador -> Double -> IO Bool
verificaCondicaoPreventiva _ _ = return False -- Mock

-- =================================================================
-- 3. MENU PRINCIPAL (SEU CÓDIGO)
-- =================================================================

main :: IO ()
main = do
  putStrLn "\n================================================="
  putStrLn "    IMPÉRIO SILÍCIO - SISTEMA DE GOVERNANÇA     "
  putStrLn "================================================="

  putStrLn "\nSelecione o modo de operação:"
  putStrLn "  [1] Executar cenário de anomalia temporal"
  putStrLn "  [2] Executar intervenção preventiva"
  putStrLn "  [3] Sistema contínuo de governança (monitoramento)"
  putStrLn "  [4] Intervenção manual (Input X)"
  putStrLn "  [5] Estado atual do sistema"
  putStrLn "  [0] Sair"

  putStr "\nOpção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> do
      cenarioAnomaliaTemporal
      main  -- Retorna ao menu

    "2" -> do
      cenarioIntervencaoPreventiva
      main

    "3" -> do
      -- Loop limitado na demo para permitir retorno ao menu
      sistemaGovernancaContinuo configPadraoVAR
      main

    "4" -> do
      putStrLn "\n[INTERVENÇÃO MANUAL - INPUT X]"
      putStrLn "Forçando transição para estado de verificação..."

      ctrl <- criarControlador

      -- Executa o VAR
      ctrlFinal <- processoCompletoVAR ctrl
        "Intervenção manual (Input X)"
        ["Intervenção iniciada por operador", "Protocolo de emergência ativado"]
        configPadraoVAR

      putStrLn "\nControlador após intervenção:"
      putStrLn $ "  Último beat: " ++ printf "%.2f" (ultimoBeat ctrlFinal)

      -- Exibe os logs gerados
      putStrLn $ "  Logs: " ++ show (take 3 (logIntervencoes ctrlFinal))

      putStrLn "\nIniciando monitoramento pós-intervenção..."
      monitoramentoTempoReal ctrlFinal
      main

    "5" -> do
      putStrLn "\n--- ESTADO ATUAL DO SISTEMA ---"
      demoSystemFinal
      main

    "0" -> putStrLn "\nSessão de governança encerrada."

    _ -> do
      putStrLn "Opção inválida."
      main

  putStrLn "\n================================================="
  putStrLn "     GOVERNANÇA AXIAL TEMPORAL CONCLUÍDA       "
  putStrLn "================================================="
