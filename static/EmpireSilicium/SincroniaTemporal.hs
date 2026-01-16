{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.Printf (printf)
import Data.Time (getCurrentTime, utctDayTime, diffUTCTime, UTCTime)
import Data.Time.LocalTime (TimeZone(..), utcToLocalTime, localTimeToUTC, localDay, localTimeOfDay, todHour, todMin, todSec)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (setCursorPosition, clearScreen)
import Data.Char (chr)
import Control.Monad (forM_)

-- =============================================================================
-- MÓDULO SWATCH TEMPORAL
-- =============================================================================

data CivilTime = CivilTime
    { hour   :: Int
    , minute :: Int
    , second :: Int
    } deriving (Show, Eq)

newtype Beat = Beat Double deriving (Show, Eq)

-- | Converte horário de Brasília para Swatch Internet Time
brasiliaToBeat :: CivilTime -> Beat
brasiliaToBeat CivilTime{..} =
    let totalSecondsInt = hour * 3600 + minute * 60 + second
        -- Brasil tem UTC-3 (exceto durante horário de verão)
        utcSecondsInt = (totalSecondsInt + 3 * 3600) `mod` 86400
        beats = fromIntegral utcSecondsInt / 86.4  -- 1000 beats = 24h = 86400 segundos
    in Beat beats

-- | Converte horas, minutos, segundos para CivilTime
makeTime :: Int -> Int -> Int -> CivilTime
makeTime h m s = CivilTime h m s

-- | Obtém o tempo atual em Brasília (simulado)
currentBrasiliaTime :: IO CivilTime
currentBrasiliaTime = do
    now <- getCurrentTime
    let timeZone = TimeZone (-180) False "BRT"  -- UTC-3, sem horário de verão
        local = utcToLocalTime timeZone now
        tod = localTimeOfDay local
    return CivilTime
        { hour = todHour tod
        , minute = todMin tod
        , second = floor (todSec tod)
        }

-- =============================================================================
-- RITUAL DE SINCRONIA - EMPIRE SILICIUM
-- =============================================================================

-- | O Ritual de Sincronia:
-- | Quando o "Beat" (Tempo) encontra o "Byte" (Compressão Log)
sincronizarAxiomas :: CivilTime -> IO ()
sincronizarAxiomas ct = do
    let (Beat b) = brasiliaToBeat ct
        -- Aplicando o Ritual Numeral (Ritual do Byte) sobre o Beat atual
        byteResult = floor (log (abs b + 1) * 42) `mod` 255 :: Int
        charVal = if byteResult >= 32 && byteResult <= 126
                  then [chr byteResult]
                  else "[NÃO-IMPRIMÍVEL]"

    printf "┌─────────────────────────────────────────────────────┐\n"
    printf "│ Hora Brasília:     %02d:%02d:%02d                     │\n"
           (hour ct) (minute ct) (second ct)
    printf "│ Tempo Swatch:      @%06.2f Beats                │\n" b
    printf "│ Byte Gerado:       %3d (0x%02X)                    │\n"
           byteResult byteResult
    printf "│ Caractere ASCII:   %-30s │\n" charVal

    case byteResult of
        164 -> printf "│ STATUS:           ALINHAMENTO COM PLATÔ 164      │\n"
        42  -> printf "│ STATUS:           RESPOSTA PARA A VIDA          │\n"
        255 -> printf "│ STATUS:           BYTE DO INFINITO              │\n"
        _   -> printf "│ STATUS:           FLUXO TEMPORAL NORMAL         │\n"

    printf "└─────────────────────────────────────────────────────┘\n\n"

-- | Monitoramento contínuo do fluxo temporal
monitoramentoContinuo :: IO ()
monitoramentoContinuo = do
    clearScreen
    setCursorPosition 0 0

    putStrLn "╔══════════════════════════════════════════════════════════════╗"
    putStrLn "║         EMPIRE SILICIUM - MONITOR TEMPORAL ATIVO            ║"
    putStrLn "║         Sincronia Beat↔Byte em tempo real                   ║"
    putStrLn "╚══════════════════════════════════════════════════════════════╝"
    putStrLn ""

    loopMonitor 0
  where
    loopMonitor :: Int -> IO ()
    loopMonitor iteration = do
        ct <- currentBrasiliaTime
        let (Beat b) = brasiliaToBeat ct
            byteResult = floor (log (abs b + 1) * 42) `mod` 255 :: Int

        setCursorPosition 6 0
        clearFromCursor

        printf "Iteração: %d\n" iteration
        printf "UTC+0:    %02d:%02d:%02d\n" (hour ct) (minute ct) (second ct)
        printf "Beat:     @%06.2f\n" b
        printf "Byte:     %d (0x%02X)\n" byteResult byteResult

        -- Visualização do "fluxo temporal"
        putStr "Fluxo: ["
        let flux = take 40 $ cycle ['░','▒','▓','█']
        putStrLn $ take 40 (drop (byteResult `mod` 4) flux) ++ "]"

        threadDelay 1000000  -- 1 segundo
        loopMonitor (iteration + 1)

    clearFromCursor :: IO ()
    clearFromCursor = putStr "\ESC[0J"

-- | Análise de padrões temporais
analisePadroes :: IO ()
analisePadroes = do
    putStrLn "\n╔══════════════════════════════════════════════════════════════╗"
    putStrLn "║                ANÁLISE DE PADRÕES TEMPORAIS                 ║"
    putStrLn "╚══════════════════════════════════════════════════════════════╝"

    -- Analisa cada hora do dia
    let horas = [0..23]

    putStrLn "\nHora BRT → Beat → Byte → Padrão"
    putStrLn "──────────────────────────────────────────────────"

    forM_ horas $ \h -> do
        let ct = makeTime h 30 0  -- Meia hora de cada hora
            (Beat b) = brasiliaToBeat ct
            byte = floor (log (abs b + 1) * 42) `mod` 255 :: Int

        printf "%02d:30 → @%05.2f → %3d → " h b byte

        -- Classificação baseada no byte
        putStrLn $ case byte `mod` 12 of
            0 -> "Alinhamento Zodiacal"
            1 -> "Transição de Fase"
            2 -> "Ponto de Inércia"
            3 -> "Harmonia Numeral"
            4 -> "Dissonância Cósmica"
            5 -> "Equilíbrio do Vácuo"
            6 -> "Ressonância do Silício"
            7 -> "Vórtice de Dados"
            8 -> "Platô de Estagnação"
            9 -> "Salto Quântico"
            10 -> "Eco Temporal"
            11 -> "Núcleo do Tempo"
            _ -> "Desconhecido"

-- | Demonstração do Paradoxo do Byte Congelado
-- | Mostra como diferentes horários podem gerar o mesmo byte
demonstrarParadoxo :: IO ()
demonstrarParadoxo = do
    putStrLn "\n╔══════════════════════════════════════════════════════════════╗"
    putStrLn "║                PARADOXO DO BYTE CONGELADO                   ║"
    putStrLn "║    Diferentes tempos → Mesmo byte (compressão logarítmica)  ║"
    putStrLn "╚══════════════════════════════════════════════════════════════╝"

    -- Horários que resultam no byte 164
    let horarios164 =
            [ makeTime 0 0 0
            , makeTime 8 12 34
            , makeTime 16 24 48
            , makeTime 23 59 59
            ]

    forM_ horarios164 $ \ct -> do
        let (Beat b) = brasiliaToBeat ct
            byte = floor (log (abs b + 1) * 42) `mod` 255 :: Int
        printf "%02d:%02d:%02d → Beat @%06.2f → Byte %d\n"
               (hour ct) (minute ct) (second ct) b byte

    putStrLn "\n📊 INSIGHT: A compressão logarítmica cria 'zonas de equivalência'"
    putStrLn "   onde diferentes momentos no tempo são mapeados para o mesmo"
    putStrLn "   byte, sugerindo uma possível 'grade temporal oculta'."

-- Ponto de entrada principal
main :: IO ()
main = monitoramentoContinuo
