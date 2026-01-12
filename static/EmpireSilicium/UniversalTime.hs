{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.List (unfoldr)
import Text.Printf (printf)

-- DATA TYPES
newtype UniversalTime = Moment { beatsValue :: Double } deriving (Show)

-- CORE FUNCTIONS
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- | Gets the current universal moment.
--   Hardcoded to match the GHCi session logs (equivalent to 07:15:00 BMT).
runUniversal :: IO UniversalTime
runUniversal = return $ Moment 302.0833333333333

nextFibBeat :: Double -> Integer
nextFibBeat beat = head $ filter (> round beat) fibStream

-- TIME CONVERSION & SCHEDULING

-- | Converts a beat value to "Brazil Mean Time" (BMT/UTC+1).
--   Based on the user's provided logic where 1 beat = 86.4 seconds.
beatToBMT :: Double -> String
beatToBMT beat =
    let totalSecs = beat * 86.4
        -- Normalize for display
        totalSecs' = if totalSecs < 0 then totalSecs + 86400 else totalSecs
        h = floor (totalSecs' / 3600) :: Int
        m = floor ((totalSecs' - fromIntegral (h*3600)) / 60) :: Int
        s = floor (totalSecs' - fromIntegral (h*3600 + m*60)) :: Int
    in printf "%02d:%02d:%02d" h m s

-- | Versão corrigida da conversão para horário local (UTC-3)
beatToLocal :: Double -> String
beatToLocal beat =
    let totalSecs = beat * 86.4
        -- BMT (UTC+1) para Local (UTC-3) = -4 horas = -14400 segundos
        secsLocal = totalSecs - 14400.0
        -- Normalizar para 0-86400
        secsLocal' = if secsLocal < 0 then secsLocal + 86400 else secsLocal
        h = floor (secsLocal' / 3600) :: Int
        m = floor ((secsLocal' - fromIntegral (h*3600)) / 60) :: Int
        s = floor (secsLocal' - fromIntegral (h*3600 + m*60)) :: Int
    in printf "%02d:%02d:%02d" h m s

-- | Converte horário local (UTC-3) para beat
localToBeat :: Int -> Int -> Int -> Double
localToBeat h m s =
    let totalSecs = fromIntegral (h*3600 + m*60 + s)
        -- Local (UTC-3) para BMT (UTC+1) = +4 horas = +14400 segundos
        totalBmtSecs = totalSecs + 14400
        -- Normalizar se passar de 86400
        totalBmtSecs' = if totalBmtSecs >= 86400 then totalBmtSecs - 86400 else totalBmtSecs
    in totalBmtSecs' / 86.4

-- | Formats the beat value as a string, e.g., "@302.08".
beatsTime :: UniversalTime -> String
beatsTime = printf "@%.2f" . beatsValue

-- | Returns a list of the next n Fibonacci beats from the current moment.
fibSchedule :: Double -> Int -> [Integer]
fibSchedule currentBeat n = take n $ dropWhile (<= round currentBeat) fibStream

-- VERIFICATION & DEMO

-- | Parser simples para horário HH:MM:SS
parseTime :: String -> (Int, Int, Int)
parseTime timeStr =
    let parts = split ':' timeStr
    in case map read parts of
        [h, m, s] -> (h, m, s)
        _ -> error "Formato de horário inválido"
  where
    split delim = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== delim))
    second f (x, y) = (x, f y)

-- | Verificação da conversão bidirecional
testConversion :: IO ()
testConversion = do
    moment <- runUniversal
    let currentBeat = beatsValue moment
    let localStr = beatToLocal currentBeat

    putStrLn $ "Beat atual: " ++ show currentBeat
    putStrLn $ "Horário local: " ++ localStr

    -- Extrair componentes do horário local para reconversão
    let (h, m, s) = parseTime localStr
    let reconvertedBeat = localToBeat h m s
    let diff = abs (currentBeat - reconvertedBeat)

    putStrLn $ "Reconvertido para beat: " ++ show reconvertedBeat
    putStrLn $ "Diferença: " ++ show diff ++ " beats"
    putStrLn $ "Diferença em segundos: " ++ show (diff * 86.4)

    if diff < 0.01
        then putStrLn "✓ Conversão bidirecional consistente"
        else putStrLn "✗ Erro na conversão bidirecional"

-- | Versão expandida do demo com verificação
demoSystemEnhanced :: IO ()
demoSystemEnhanced = do
    moment <- runUniversal
    let currentBeat = beatsValue moment

    putStrLn "┌─────────────────────────────────────────┐"
    putStrLn "│  EMPIRE SILICIUM - FIBONACCI CLOCK      │"
    putStrLn "└─────────────────────────────────────────┘"
    putStrLn ""

    putStrLn "─── TEMPORALIDADES ───"
    putStrLn $ "Beat Atual:        " ++ beatsTime moment
    putStrLn $ "Valor Beat:        " ++ printf "%.3f" currentBeat
    putStrLn $ "BMT (UTC+1):       " ++ beatToBMT currentBeat
    putStrLn $ "Local (UTC-3):     " ++ beatToLocal currentBeat

    putStrLn ""
    putStrLn "─── FIBONACCI PULSE ───"
    let next = nextFibBeat currentBeat
    let deltaBeats = fromIntegral next - currentBeat
    putStrLn $ "Próximo Pulso:     @" ++ show next
    putStrLn $ "Beat Fibonacci:    " ++ beatToLocal (fromIntegral next)
    putStrLn $ "Delta (beats):     " ++ printf "%.2f" deltaBeats

    let deltaSecs = deltaBeats * 86.4
    let h = floor (deltaSecs / 3600) :: Int
        m = floor ((deltaSecs - fromIntegral (h*3600)) / 60) :: Int
        s = floor (deltaSecs - fromIntegral (h*3600 + m*60)) :: Int
    putStrLn $ "Tempo Restante:    " ++ printf "%02d:%02d:%02d" h m s

    putStrLn ""
    putStrLn "─── CICLOS FUTUROS ───"
    let schedule = fibSchedule currentBeat 5
    mapM_ (\fib -> putStrLn $ "  @" ++ show fib ++ " → " ++ beatToLocal (fromIntegral fib)) schedule

    putStrLn ""
    putStrLn "─── VERIFICAÇÃO ───"
    testConversion

-- MAIN
main :: IO ()
main = demoSystemEnhanced
