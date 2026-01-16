import Text.Printf (printf)
import System.Console.ANSI (setCursorPosition, clearScreen)
import Control.Concurrent (threadDelay)
import Data.Char (chr)

-- Data definition for the Stack Frame
data StackFrame = Frame {
    pc :: Int,      -- Program Counter (line number)
    a  :: Double,   -- Accumulator (current value)
    x  :: Int,      -- Register X (seed)
    y  :: Int       -- Register Y (counter/aux)
    } deriving (Eq)

-- Display instance with detailed formatting
instance Show StackFrame where
    show f = printf "PC:0x%02X | REG_A: %18.4f | REG_X: 0x%02X (%3d) | REG_Y: %d | %s"
             (pc f) (a f) (x f) (x f) (y f) (interpretacao (pc f))

-- Esoteric Operating System comments
interpretacao :: Int -> String
interpretacao 10 = "LOAD: Raw Input enters memory (Phenomenon)."
interpretacao 20 = "NORM: Normalization (avoid absolute zero)."
interpretacao 30 = "LOG:  Magnitude compression (Logos)."
interpretacao 40 = "PROJ: Projection through Seed (Observation)."
interpretacao 50 = "INT:  Continuous becomes discrete (Floor)."
interpretacao 60 = "MOD:  Final ritual. Reduction to Byte (Numen Manifest)."
interpretacao _  = "HALT: Unknown state (Liminal Space)."

-- Execute the transformation routine
executaRotina :: Double -> Int -> [StackFrame]
executaRotina inputX seed =
    let frame0 = Frame 10 inputX seed 0
        frame1 = Frame 20 (abs inputX + 1) seed 0
        frame2 = Frame 30 (log (abs inputX + 1)) seed 0
        frame3 = Frame 40 (log (abs inputX + 1) * fromIntegral seed) seed 0
        intVal = floor (log (abs inputX + 1) * fromIntegral seed)
        frame4 = Frame 50 (fromIntegral intVal) seed 0
        modVal = intVal `mod` 255
        frame5 = Frame 60 (fromIntegral modVal) seed 0
    in [frame0, frame1, frame2, frame3, frame4, frame5]

-- Animated debugger with step-by-step execution
debuggarInefavel :: Double -> Int -> IO ()
debuggarInefavel input seed = do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ "┌─────────────────────────────────────────────────────────────────────┐"
    putStrLn $ "│                    NUMENAL DEBUGGER v1.0                            │"
    putStrLn $ "│   Input: " ++ show input ++ " (" ++ show (length (show input)) ++ " digits)"
    putStrLn $ "│   Seed: 0x" ++ printf "%02X" seed ++ " (" ++ show seed ++ ")"
    putStrLn $ "└─────────────────────────────────────────────────────────────────────┘"

    let stack = executaRotina input seed
        finalVal = a (last stack)
        chars = map (chr . round) [finalVal, finalVal+1 .. finalVal+5]

    putStrLn "\n╔═════════════════════════════════════════════════════════════════════╗"
    putStrLn "║                         EXECUTION TRACE                             ║"
    putStrLn "╚═════════════════════════════════════════════════════════════════════╝"

    -- Animate execution step by step
    animateExecution stack 0

    putStrLn "\n╔═════════════════════════════════════════════════════════════════════╗"
    putStrLn "║                         NUMENAL RESULT                              ║"
    putStrLn "╚═════════════════════════════════════════════════════════════════════╝"

    printf "Final Byte: 0x%02X (%d)\n" (round finalVal :: Int) (round finalVal :: Int)
    putStrLn $ "Character Sequence: " ++ show chars
    putStrLn $ "As String: " ++ chars
    putStrLn "\nREADY."

    where
        animateExecution :: [StackFrame] -> Int -> IO ()
        animateExecution [] _ = return ()
        animateExecution (frame:frames) delay = do
            threadDelay (delay * 300000)  -- 300ms delay between steps
            print frame
            animateExecution frames (delay + 1)

-- Extended version with memory visualization
data MemoryMap = MemoryMap {
    zeroPage  :: [(Int, Double)],  -- 0x0000-0x00FF
    stackArea :: [(Int, Double)],  -- 0x0100-0x01FF
    basicRom  :: [(Int, String)],  -- 0xA000-0xBFFF
    charRom   :: [(Int, Char)]     -- 0xD000-0xDFFF
    }

-- Alternative view: Show the transformation as BASIC code
showAsBASIC :: Double -> Int -> String
showAsBASIC inputX seed = unlines [
    "10 REM NUMENAL TRANSMUTATION",
    "20 REM INPUT: " ++ show inputX,
    "30 REM SEED: " ++ show seed,
    "40 LET X = ABS(" ++ show inputX ++ ") + 1",
    "50 LET Y = LOG(X)",
    "60 LET Z = Y * " ++ show seed,
    "70 LET B = INT(Z)",
    "80 LET C = B - INT(B/255)*255",
    "90 FOR I = 0 TO 5",
    "100 PRINT CHR$(C + I)",
    "110 NEXT I",
    "120 END"
    ]

-- Main function with options
-- Main function (non-interactive)
main :: IO ()
main = do
    let input = 1768525786088
        seed = 42
    debuggarInefavel input seed
