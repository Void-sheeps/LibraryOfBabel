{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Text.Printf (printf)

-- | Represents a moment in universal time.
--   The base unit is the "beat", a conceptual measure of time.
newtype UniversalTime = Moment { beatsValue :: Double } deriving (Show)

-- | Infinite stream of Fibonacci numbers.
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- | Gets the current universal moment.
--   In this simulation, the value is fixed to demonstrate the concept.
runUniversal :: IO UniversalTime
runUniversal = return $ Moment 302.08

-- | Finds the next Fibonacci "beat" after the current moment.
nextFibBeat :: Double -> Integer
nextFibBeat beat = head $ filter (> round beat) fibStream

-- | Displays the status of the universal time system.
monitorSystem :: IO ()
monitorSystem = do
    moment <- runUniversal
    let now = beatsValue moment
    let nextPulse = nextFibBeat now
    let latency = fromIntegral nextPulse - now
    let nextCycles = take 3 $ dropWhile (< nextPulse) fibStream

    printf " [◉] Status: POTENTIA | Beat Atual: @%.2f\n" now
    printf " [◈] Próximo Pulso Axiomático: @%d\n" nextPulse
    printf " [◈] Latência de Silentium: %.2f beats\n" latency
    putStrLn $ " [λ] Próximos 3 ciclos: " ++ show nextCycles

-- | Main function that executes upon running the script.
main :: IO ()
main = do
    putStrLn "┌──────────────────────────────────────────────────────────┐"
    putStrLn "│  Ξ M P I R Ξ   S I L I C I U M   |   U T C   S T A T U S   │"
    putStrLn "└──────────────────────────────────────────────────────────┘"
    monitorSystem

    -- Demonstration of temporal delta verification, as in the GHCi session
    moment <- runUniversal
    let now = beatsValue moment
    let next = nextFibBeat now
    -- The conversion factor 86.4 is derived from the GHCi example,
    -- representing a conceptual relationship between beats and seconds.
    let timeToNext = (fromIntegral next - now) * 86.4

    putStrLn "\n-- Verificação de Delta Temporal --"
    printf "Tempo até o evento %d: %.2f segundos\n" next timeToNext

    -- Demonstration of fibStream
    putStrLn "\n-- fibStream Sample --"
    print (take 15 fibStream)
