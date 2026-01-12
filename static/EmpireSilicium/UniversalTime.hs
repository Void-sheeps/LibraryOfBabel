-- UNIVERSALTIME.HS - EMPIRE SILICIUM TIME SYSTEM
-- Código completo com todas as correções e funções melhoradas

module Main where

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.List (unfoldr)
import Text.Printf
import Control.Monad (replicateM)
import Control.Arrow (second)

-- Definição do Tipo Abstrato
newtype Moment = Moment UTCTime deriving Show

-- Stream de Fibonacci (lazy, infinita)
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- | Captura o Momento Presente (IO)
runUniversal :: IO Moment
runUniversal = Moment <$> getCurrentTime

-- | Converte Momento para valor beat (Double)
beatsValue :: Moment -> Double
beatsValue (Moment utc) =
    let secondsInDay = 86400.0
        beatsPerDay  = 1000.0
        posix        = utcTimeToPOSIXSeconds utc
        bmtSeconds   = realToFrac posix + 3600  -- UTC+1
        dayFraction  = bmtSeconds - (fromIntegral (floor (bmtSeconds / secondsInDay) :: Int) * secondsInDay)
    in dayFraction * (beatsPerDay / secondsInDay)

-- | Converte para string formatada (com 2 casas decimais)
beatsTime :: Moment -> String
beatsTime m =
    let val   = beatsValue m
        whole = floor val :: Int
        frac  = floor ((val - fromIntegral whole) * 100) :: Int
    in "@" ++ show whole ++ "." ++ (if frac < 10 then "0" else "") ++ show frac

-- | Gerador de Sequência de Fibonacci Quântica
quantumFib :: Int -> IO [Integer]
quantumFib _energy = return $ take 100 fibStream

-- | Protocolo de Despertar: Fibonacci Clock Interval
nextFibBeat :: Double -> Integer
nextFibBeat currentBeat =
    head $ dropWhile (<= floor currentBeat) fibStream

fibSchedule :: Double -> Int -> [Integer]
fibSchedule currentBeat count =
    take count $ dropWhile (<= floor currentBeat) fibStream

-- | Converte beat para horário BMT (UTC+1) com arredondamento
beatToBMT :: Double -> String
beatToBMT beat =
    let totalSecs = beat * 86.4
        h = floor (totalSecs / 3600) :: Int
        m = floor ((totalSecs - fromIntegral h * 3600) / 60) :: Int
        s = round (totalSecs - fromIntegral h * 3600 - fromIntegral m * 60) :: Int
        (m', s') = if s == 60 then (m + 1, 0) else (m, s)
        (h', m'') = if m' == 60 then (h + 1, 0) else (h, m')
        h'' = h' `mod` 24
    in printf "%02d:%02d:%02d" h'' m'' s'

-- | beat → Horário local (UTC-3) com arredondamento e normalização correta
beatToLocal :: Double -> String
beatToLocal beat =
    let totalSecs = beat * 86.4
        secsLocalRaw = totalSecs - 14400.0
        -- Normalizar para [0, 86400)
        secsLocal = secsLocalRaw - fromIntegral (floor (secsLocalRaw / 86400)) * 86400.0
        h = floor (secsLocal / 3600) :: Int
        m = floor ((secsLocal - fromIntegral h * 3600) / 60) :: Int
        s = round (secsLocal - fromIntegral h * 3600 - fromIntegral m * 60) :: Int
        -- Ajustar se s for 60
        (m', s') = if s == 60 then (m + 1, 0) else (m, s)
        (h', m'') = if m' == 60 then (h + 1, 0) else (h, m')
        h'' = h' `mod` 24
    in printf "%02d:%02d:%02d" h'' m'' s'

-- | Versão precisa (mantém fração dos segundos)
beatToLocalPrecise :: Double -> (Int, Int, Double)
beatToLocalPrecise beat =
    let totalSecs = beat * 86.4
        secsLocalRaw = totalSecs - 14400.0
        secsLocal = secsLocalRaw - fromIntegral (floor (secsLocalRaw / 86400)) * 86400.0
        h = floor (secsLocal / 3600) :: Int
        m = floor ((secsLocal - fromIntegral h * 3600) / 60) :: Int
        s = secsLocal - fromIntegral h * 3600 - fromIntegral m * 60
    in (h, m, s)

-- | Converte horário local (UTC-3) para beat
localToBeat :: Int -> Int -> Int -> Double
localToBeat h m s =
    let totalSecs = fromIntegral (h*3600 + m*60 + s)
        totalBmtSecs = totalSecs + 14400
        totalBmtSecs' = totalBmtSecs `mod` 86400
    in fromIntegral totalBmtSecs' / 86.4

-- | Parser simples para horário HH:MM:SS
parseTime :: String -> (Int, Int, Int)
parseTime timeStr =
    let parts = split ':' timeStr
    in case map read parts of
        [h, m, s] -> (h, m, s)
        _ -> error "Formato de horário inválido"
  where
    split delim = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== delim))

-- | Exibe o próximo pulso Fibonacci e horário local
printNextPulse :: Double -> IO ()
printNextPulse c = do
    let next = nextFibBeat c
    let delta = fromIntegral next - c
    putStrLn $ " [◈] Próximo Pulso Axiomático: @" ++ show next
    putStrLn $ " [◈] Latência de Silentium: " ++ printf "%.2f" delta ++ " beats"

-- | Demonstração completa do sistema
monitorSystem :: IO ()
monitorSystem = do
    moment <- runUniversal
    let current = beatsValue moment
    putStrLn $ " [◉] Status: POTENTIA | Beat Atual: " ++ beatsTime moment
    printNextPulse current
    putStrLn $ " [λ] Próximos 3 ciclos: " ++ show (fibSchedule current 3)

-- | Sistema de demonstração completo e verificado
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
        s = round (deltaSecs - fromIntegral (h*3600 + m*60)) :: Int
    putStrLn $ "Tempo Restante:    " ++ printf "%02d:%02d:%02d" h m s

    putStrLn ""
    putStrLn "─── CICLOS FUTUROS ───"
    let schedule = fibSchedule currentBeat 5
    mapM_ (\fib -> putStrLn $ "  @" ++ show fib ++ " → " ++ beatToLocal (fromIntegral fib)) schedule

    putStrLn ""
    putStrLn "─── VERIFICAÇÃO (versão precisa) ───"
    let (hPrecise, mPrecise, sPrecise) = beatToLocalPrecise currentBeat
    putStrLn $ "Horário local preciso: " ++ printf "%02d:%02d:%06.3f" hPrecise mPrecise sPrecise
    let reconvertedBeat = localToBeat hPrecise mPrecise (floor sPrecise)
    putStrLn $ "Reconvertido (floor s): " ++ printf "%.6f" reconvertedBeat
    putStrLn $ "Diferença: " ++ printf "%.6f" (abs (currentBeat - reconvertedBeat))

main :: IO ()
main = demoSystemEnhanced
