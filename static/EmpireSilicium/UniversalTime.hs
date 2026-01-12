-- UNIVERSALTIME.HS - EMPIRE SILICIUM TIME SYSTEM
-- Final version with integrated constants and enhanced demonstration

module Main where

import Control.Arrow (second)
import Control.Monad (forM_)
import Data.List (unfoldr)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Text.Printf

-- --------------------------------------------------------------------------
--                            CONSTANTES DO SISTEMA
-- --------------------------------------------------------------------------

beatsPerDay :: Double
beatsPerDay = 1000.0

secondsPerDay :: Double
secondsPerDay = 24 * 3600

secondsPerHour :: Double
secondsPerHour = 3600.0

secondsPerMinute :: Double
secondsPerMinute = 60.0

secondsPerBeat :: Double
secondsPerBeat = secondsPerDay / beatsPerDay

-- Offset em segundos para BMT (UTC+1 a partir de UTC)
bmtOffset :: Double
bmtOffset = 1 * secondsPerHour

-- Offset em segundos para Local (UTC-3 a partir de BMT/UTC+1)
localOffset :: Double
localOffset = -4 * secondsPerHour

-- --------------------------------------------------------------------------
--                            TIPOS E FUNÇÕES CORE
-- --------------------------------------------------------------------------

newtype Moment = Moment UTCTime deriving Show

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

runUniversal :: IO Moment
runUniversal = Moment <$> getCurrentTime

beatsValue :: Moment -> Double
beatsValue (Moment utc) =
    let posix = utcTimeToPOSIXSeconds utc
        bmtSeconds = realToFrac posix + bmtOffset
        dayFraction = bmtSeconds - fromIntegral (floor (bmtSeconds / secondsPerDay) :: Int) * secondsPerDay
    in dayFraction / secondsPerBeat

beatsTime :: Moment -> String
beatsTime m = printf "@%.2f" (beatsValue m)

nextFibBeat :: Double -> Integer
nextFibBeat currentBeat = head $ dropWhile (<= floor currentBeat) fibStream

fibSchedule :: Double -> Int -> [Integer]
fibSchedule currentBeat count = take count $ dropWhile (<= floor currentBeat) fibStream

-- --------------------------------------------------------------------------
--                          CONVERSÕES DE TEMPO
-- --------------------------------------------------------------------------

beatToBMT :: Double -> String
beatToBMT beat =
    let totalSecs = beat * secondsPerBeat
        h = floor (totalSecs / secondsPerHour) :: Int
        m = floor ((totalSecs - fromIntegral h * secondsPerHour) / secondsPerMinute) :: Int
        s = round (totalSecs - fromIntegral h * secondsPerHour - fromIntegral m * secondsPerMinute) :: Int
        (m', s') = if s == 60 then (m + 1, 0) else (m, s)
        (h', m'') = if m' == 60 then (h + 1, 0) else (h, m')
        h'' = h' `mod` 24
    in printf "%02d:%02d:%02d" h'' m'' s'

beatToLocal :: Double -> String
beatToLocal beat =
    let totalSecs = beat * secondsPerBeat
        secsLocalRaw = totalSecs + localOffset
        secsLocal = secsLocalRaw - fromIntegral (floor (secsLocalRaw / secondsPerDay) :: Int) * secondsPerDay
        h = floor (secsLocal / secondsPerHour) :: Int
        m = floor ((secsLocal - fromIntegral h * secondsPerHour) / secondsPerMinute) :: Int
        s = round (secsLocal - fromIntegral h * secondsPerHour - fromIntegral m * secondsPerMinute) :: Int
        (m', s') = if s == 60 then (m + 1, 0) else (m, s)
        (h', m'') = if m' == 60 then (h + 1, 0) else (h, m')
        h'' = h' `mod` 24
    in printf "%02d:%02d:%02d" h'' m'' s'

beatToLocalPrecise :: Double -> (Int, Int, Double)
beatToLocalPrecise beat =
    let totalSecs = beat * secondsPerBeat
        secsLocalRaw = totalSecs + localOffset
        secsLocal = secsLocalRaw - fromIntegral (floor (secsLocalRaw / secondsPerDay) :: Int) * secondsPerDay
        h = floor (secsLocal / secondsPerHour) :: Int
        m = floor ((secsLocal - fromIntegral h * secondsPerHour) / secondsPerMinute) :: Int
        s = secsLocal - fromIntegral h * secondsPerHour - fromIntegral m * secondsPerMinute
    in (h, m, s)

-- --------------------------------------------------------------------------
--                            DEMONSTRAÇÃO FINAL
-- --------------------------------------------------------------------------

demoSystemFinal :: IO ()
demoSystemFinal = do
  moment <- runUniversal
  let currentBeat = beatsValue moment
      (hPrecise, mPrecise, sPrecise) = beatToLocalPrecise currentBeat

  putStrLn "┌─────────────────────────────────────────────────┐"
  putStrLn "│  EMPIRE SILICIUM - FIBONACCI CLOCK v2.0         │"
  putStrLn "└─────────────────────────────────────────────────┘"
  putStrLn ""

  putStrLn "─── CONSTANTES DO SISTEMA ───"
  putStrLn $ "Beats por dia:      " ++ show (floor beatsPerDay :: Int)
  putStrLn $ "Segundos por beat:  " ++ show secondsPerBeat
  putStrLn $ "Offset BMT:         " ++ show (floor bmtOffset :: Int) ++ "s (UTC+1)"
  putStrLn $ "Offset Local:       " ++ show (floor localOffset :: Int) ++ "s (UTC-3 from BMT)"
  putStrLn ""

  putStrLn "─── ESTADO TEMPORAL ATUAL ───"
  putStrLn $ "Beat:               " ++ beatsTime moment
  putStrLn $ "BMT:                " ++ beatToBMT currentBeat
  putStrLn $ "Local (arredondado): " ++ beatToLocal currentBeat
  putStrLn $ "Local (preciso):    " ++
    printf "%02d:%02d:%06.3f" hPrecise mPrecise sPrecise
  putStrLn ""

  putStrLn "─── PROTOCOLO FIBONACCI ───"
  let nextPulse = nextFibBeat currentBeat
      deltaBeats = fromIntegral nextPulse - currentBeat
      deltaSecs = deltaBeats * secondsPerBeat

  putStrLn $ "Beat atual:          " ++ printf "%.3f" currentBeat
  putStrLn $ "Próximo pulso:       @" ++ show nextPulse
  putStrLn $ "Delta:               " ++ printf "%.3f beats (%.1f s)" deltaBeats deltaSecs

  let h = floor (deltaSecs / secondsPerHour) :: Int
      m = floor ((deltaSecs - fromIntegral h * secondsPerHour) / secondsPerMinute) :: Int
      s = floor (deltaSecs - fromIntegral (h * 3600 + m * 60)) :: Int
  putStrLn $ "Ativação em:         " ++ printf "%02d:%02d:%02d" h m s
  putStrLn $ "Horário local:       " ++ beatToLocal (fromIntegral nextPulse)
  putStrLn ""

  putStrLn "─── ANÁLISE MATEMÁTICA ───"
  let prevFib = last $ takeWhile (<= floor currentBeat) fibStream
      ratio :: Double
      ratio = fromIntegral nextPulse / fromIntegral (max 1 prevFib)
      goldenRatio :: Double
      goldenRatio = (1 + sqrt 5) / 2
  putStrLn $ "Fibonacci anterior:  @" ++ show prevFib
  putStrLn $ "Razão F(n)/F(n-1):   " ++ printf "%.6f" ratio
  putStrLn $ "Razão áurea (φ):     " ++ printf "%.6f" goldenRatio
  putStrLn $ "Diferença:           " ++ printf "%.6f" (abs (ratio - goldenRatio))

  putStrLn ""
  putStrLn "─── AGENDA FIBONACCI (próximos 7) ───"
  let schedule = fibSchedule currentBeat 7
  forM_ (zip schedule ([1..] :: [Int])) $ \(fib, idx) -> do
    let beatTime = beatToLocal (fromIntegral fib)
        diff = fromIntegral fib - currentBeat
        diffHours = diff * secondsPerBeat / secondsPerHour
    putStrLn $ printf "%2d. @%-4d → %s (Δ: %6.1f beats, %5.1f horas)"
      idx fib beatTime diff diffHours

main :: IO ()
main = demoSystemFinal
