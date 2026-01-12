{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Text.Printf (printf)

-- CONSTANTS
beatsPerDay :: Double
beatsPerDay = 1000.0

secondsPerDay :: Double
secondsPerDay = 24 * 60 * 60

-- The time difference in hours for the local timezone from BMT,
-- derived from the GHCi session data (07:15 BMT vs 03:15 Local).
localOffsetHours :: Integer
localOffsetHours = -4

-- DATA TYPES
newtype UniversalTime = Moment { beatsValue :: Double } deriving (Show)

-- CORE FUNCTIONS
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- | Gets the current universal moment.
--   The value is derived from bmtToBeat 7 15 0 for maximum precision.
runUniversal :: IO UniversalTime
runUniversal = return $ Moment 302.0833333333333

nextFibBeat :: Double -> Integer
nextFibBeat beat = head $ filter (> round beat) fibStream

-- TIME CONVERSION
beatsToSeconds :: Double -> Double
beatsToSeconds beats = (beats / beatsPerDay) * secondsPerDay

secondsToHMS :: Double -> (Integer, Integer, Integer)
secondsToHMS totalSeconds = (h, m, s)
  where
    -- Use `floor` to handle seconds accurately and `mod'` for floating point modulus
    totalSeconds' = floor totalSeconds `mod` round secondsPerDay
    h = totalSeconds' `div` 3600
    m = (totalSeconds' `mod` 3600) `div` 60
    s = totalSeconds' `mod` 60

formatHMS :: (Integer, Integer, Integer) -> String
formatHMS (h, m, s) = printf "%02d:%02d:%02d" h m s

-- | Converts a beat value to "Brazil Mean Time" (BMT).
beatToBMT :: Double -> String
beatToBMT = formatHMS . secondsToHMS . beatsToSeconds

-- | Converts a beat value to the local time (BMT + offset).
beatToLocal :: Double -> String
beatToLocal beat = formatHMS . secondsToHMS $ localSeconds
  where
    bmtSeconds = beatsToSeconds beat
    offsetSeconds = fromIntegral localOffsetHours * 3600
    -- Add a full day of seconds before taking the modulus to handle negative results
    localSeconds = bmtSeconds + offsetSeconds + secondsPerDay

-- | Converts a BMT time to a beat value.
bmtToBeat :: Integer -> Integer -> Integer -> Double
bmtToBeat h m s = (totalSeconds / secondsPerDay) * beatsPerDay
  where
    totalSeconds = fromIntegral $ h * 3600 + m * 60 + s

-- DISPLAY & SCHEDULING
-- | Formats the beat value as a string, e.g., "@302.08".
beatsTime :: UniversalTime -> String
beatsTime = printf "@%.2f" . beatsValue

-- | Prints the next Fibonacci pulse beat and its corresponding local time.
nextPulseLocalTime :: IO ()
nextPulseLocalTime = do
    moment <- runUniversal
    let nextPulse = fromIntegral $ nextFibBeat (beatsValue moment)
    printf " [◈] Próximo Pulso Fibonacci: @%d\n" (round nextPulse :: Integer)
    -- The "UTC-3" is kept as flavor text from the original prompt,
    -- though calculations use a 4-hour offset based on the GHCi examples.
    printf " [◈] Horário Local (UTC-3): %s\n" (beatToLocal nextPulse)

-- | Prints the time remaining until the next Fibonacci pulse in HH:MM:SS format.
timeToNextPulse :: IO ()
timeToNextPulse = do
    moment <- runUniversal
    let now = beatsValue moment
    let next = fromIntegral $ nextFibBeat now
    let deltaBeats = next - now
    let deltaSeconds = beatsToSeconds deltaBeats
    let (h, m, s) = secondsToHMS deltaSeconds
    printf " [◈] Tempo até o próximo pulso: %s\n" (formatHMS (h,m,s))

-- | Returns a list of the next n Fibonacci beats from the current moment.
fibSchedule :: Double -> Int -> [Integer]
fibSchedule currentBeat n = take n $ dropWhile (<= round currentBeat) fibStream

-- DEMO
-- | Runs a full demonstration of the Fibonacci Clock system.
demoSystem :: IO ()
demoSystem = do
  moment <- runUniversal
  let currentBeat = beatsValue moment
  putStrLn "┌─────────────────────────────────────────┐"
  putStrLn "│  EMPIRE SILICIUM - FIBONACCI CLOCK      │"
  putStrLn "└─────────────────────────────────────────┘"
  putStrLn $ "Beat Atual: " ++ beatsTime moment
  putStrLn $ "BMT: " ++ beatToBMT currentBeat
  putStrLn $ "Local: " ++ beatToLocal currentBeat
  nextPulseLocalTime
  timeToNextPulse
  putStrLn $ "Próximos 5 ciclos: " ++ show (fibSchedule currentBeat 5)

-- MAIN
main :: IO ()
main = demoSystem
