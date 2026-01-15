{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen, randomRs, randomRIO)
import Numeric (readHex, showHex)
import Data.Char (toLower, isHexDigit, chr, ord)
import Data.List (intercalate, nub, sortBy, isPrefixOf)
import Data.Bits (xor, shiftL, (.&.), complement)
import Control.Monad (when, forM_, replicateM)
import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time (getCurrentTime, utctDay, formatTime, defaultTimeLocale)
import qualified System.Console.ANSI as ANSI
import System.IO (hSetBuffering, stdout, BufferMode(..))

-- ========================
-- TIPOS DE DADOS C64
-- ========================

-- Cores originais do Commodore 64 (16 cores)
data C64Color = Black | White | Red | Cyan | Purple | Green | Blue | Yellow
              | Orange | Brown | LightRed | DarkGray | Gray | LightGreen
              | LightBlue | LightGray
  deriving (Enum, Bounded, Eq, Show)

-- Tipos de símbolos para nosso gerador
data SymbolType =
    MathConstant      -- π, ℏ, ∞, ℇ
  | PhysicalConstant  -- c, h, e, G, α
  | NumberSet         -- ℕ, ℤ, ℚ, ℝ, ℂ
  | SIUnit            -- m, kg, s, A, Ω, ℃
  | Operator          -- +, -, ×, ÷, √, ∑, ∫
  | LogicSymbol       -- ∀, ∃, ∧, ∨, ∴
  | SetSymbol         -- ∈, ⊂, ∪, ∩, ∅
  deriving (Enum, Bounded, Show, Eq, Ord)

-- Símbolo com metadados
data C64Symbol = C64Symbol {
  symChar :: Char,          -- Caractere Unicode
  symName :: String,        -- Nome descritivo
  symType :: SymbolType,    -- Tipo de símbolo
  symCode :: Int,           -- Código Unicode
  symWeight :: Double,      -- Peso para geração
  symColor :: C64Color      -- Cor sugerida no C64
} deriving (Show)

-- ========================
-- BANCO DE DADOS DE SÍMBOLOS
-- ========================

-- Base de dados completa com constantes matemáticas/físicas e símbolos SI
symbolDatabase :: [C64Symbol]
symbolDatabase =
  [
    -- CONSTANTES MATEMÁTICAS (MathConstant)
    C64Symbol 'π' "Pi" MathConstant 0x03C0 1.0 Cyan
  , C64Symbol '∞' "Infinito" MathConstant 0x221E 0.9 LightBlue
  , C64Symbol 'ℏ' "Constante de Planck reduzida" MathConstant 0x210F 0.8 Cyan
  , C64Symbol 'ℇ' "Constante de Euler" MathConstant 0x2107 0.6 Cyan
  , C64Symbol 'ℯ' "Base do logaritmo natural" MathConstant 0x212F 0.5 Cyan

    -- CONSTANTES FÍSICAS (PhysicalConstant)
  , C64Symbol 'c' "Velocidade da luz" PhysicalConstant 0x0063 1.0 LightRed
  , C64Symbol 'α' "Constante de estrutura fina" PhysicalConstant 0x03B1 0.7 LightRed
  , C64Symbol 'G' "Constante gravitacional" PhysicalConstant 0x0047 0.6 LightRed
  , C64Symbol 'h' "Constante de Planck" PhysicalConstant 0x0068 0.8 LightRed
  , C64Symbol 'e' "Carga elementar" PhysicalConstant 0x0065 0.7 LightRed
  , C64Symbol 'k' "Constante de Boltzmann" PhysicalConstant 0x006B 0.6 LightRed
  , C64Symbol 'σ' "Constante de Stefan-Boltzmann" PhysicalConstant 0x03C3 0.5 LightRed

    -- CONJUNTOS NUMÉRICOS (NumberSet)
  , C64Symbol 'ℕ' "Números Naturais" NumberSet 0x2115 0.8 Yellow
  , C64Symbol 'ℤ' "Números Inteiros" NumberSet 0x2124 0.8 Yellow
  , C64Symbol 'ℚ' "Números Racionais" NumberSet 0x211A 0.7 Yellow
  , C64Symbol 'ℝ' "Números Reais" NumberSet 0x211D 0.9 Yellow
  , C64Symbol 'ℂ' "Números Complexos" NumberSet 0x2102 0.7 Yellow
  , C64Symbol 'ℍ' "Quatérnions" NumberSet 0x210D 0.4 Yellow

    -- UNIDADES SI (SIUnit) - símbolos especiais
  , C64Symbol 'Ω' "Ohm (resistência)" SIUnit 0x2126 0.7 Green
  , C64Symbol '℃' "Grau Celsius" SIUnit 0x2103 0.6 Green
  , C64Symbol 'Å' "Ångström" SIUnit 0x212B 0.4 Green
  , C64Symbol 'µ' "Micro (prefixo)" SIUnit 0x00B5 0.5 Green

    -- OPERADORES MATEMÁTICOS (Operator)
  , C64Symbol '∑' "Somatório" Operator 0x2211 0.9 White
  , C64Symbol '∏' "Produtório" Operator 0x220F 0.7 White
  , C64Symbol '∫' "Integral" Operator 0x222B 1.0 White
  , C64Symbol '∂' "Derivada parcial" Operator 0x2202 0.6 White
  , C64Symbol '∇' "Nabla (gradiente)" Operator 0x2207 0.5 White
  , C64Symbol '√' "Raiz quadrada" Operator 0x221A 0.8 White
  , C64Symbol '±' "Mais ou menos" Operator 0x00B1 0.6 White
  , C64Symbol '×' "Multiplicação" Operator 0x00D7 0.7 White
  , C64Symbol '÷' "Divisão" Operator 0x00F7 0.6 White

    -- SÍMBOLOS LÓGICOS (LogicSymbol)
  , C64Symbol '∀' "Para todo" LogicSymbol 0x2200 0.8 Purple
  , C64Symbol '∃' "Existe" LogicSymbol 0x2203 0.7 Purple
  , C64Symbol '∄' "Não existe" LogicSymbol 0x2204 0.5 Purple
  , C64Symbol '∧' "E lógico" LogicSymbol 0x2227 0.6 Purple
  , C64Symbol '∨' "OU lógico" LogicSymbol 0x2228 0.6 Purple
  , C64Symbol '¬' "Negação" LogicSymbol 0x00AC 0.5 Purple
  , C64Symbol '∴' "Portanto" LogicSymbol 0x2234 0.5 Purple
  , C64Symbol '∵' "Porque" LogicSymbol 0x2235 0.4 Purple

    -- SÍMBOLOS DE TEORIA DOS CONJUNTOS (SetSymbol)
  , C64Symbol '∈' "Pertence a" SetSymbol 0x2208 0.9 Orange
  , C64Symbol '∉' "Não pertence a" SetSymbol 0x2209 0.7 Orange
  , C64Symbol '⊂' "Subconjunto próprio" SetSymbol 0x2282 0.6 Orange
  , C64Symbol '⊆' "Subconjunto" SetSymbol 0x2286 0.6 Orange
  , C64Symbol '∪' "União" SetSymbol 0x222A 0.7 Orange
  , C64Symbol '∩' "Interseção" SetSymbol 0x2229 0.7 Orange
  , C64Symbol '∅' "Conjunto vazio" SetSymbol 0x2205 0.8 Orange
  , C64Symbol '∆' "Diferença simétrica" SetSymbol 0x2206 0.5 Orange

    -- CARACTERES PETSCII/ASCII PARA C64
  , C64Symbol '█' "Bloco sólido" Operator 0x2588 0.5 Gray
  , C64Symbol '▒' "Bloco xadrez" Operator 0x2592 0.4 Gray
  , C64Symbol '░' "Bloco claro" Operator 0x2591 0.3 Gray
  , C64Symbol '○' "Círculo" Operator 0x25CB 0.4 Gray
  , C64Symbol '●' "Círculo preto" Operator 0x25CF 0.4 Gray
  , C64Symbol '◆' "Diamante" Operator 0x25C6 0.3 Gray
  , C64Symbol '★' "Estrela" Operator 0x2605 0.3 Gray
  , C64Symbol '♠' "Espadas" Operator 0x2660 0.2 Gray
  , C64Symbol '♥' "Coração" Operator 0x2665 0.2 LightRed
  , C64Symbol '♦' "Ouros" Operator 0x2666 0.2 LightRed
  , C64Symbol '♣' "Paus" Operator 0x2663 0.2 Gray
  , C64Symbol '↑' "Seta para cima" Operator 0x2191 0.3 LightBlue
  , C64Symbol '↓' "Seta para baixo" Operator 0x2193 0.3 LightBlue
  , C64Symbol '←' "Seta para esquerda" Operator 0x2190 0.3 LightBlue
  , C64Symbol '→' "Seta para direita" Operator 0x2192 0.3 LightBlue
  ]

-- ========================
-- SISTEMA DE CORES C64
-- ========================

-- Mapeamento C64 para códigos ANSI 256 cores
c64ToANSI :: C64Color -> Int
c64ToANSI color = case color of
  Black       -> 16
  White       -> 231
  Red         -> 196
  Cyan        -> 51
  Purple      -> 201
  Green       -> 46
  Blue        -> 21
  Yellow      -> 226
  Orange      -> 208
  Brown       -> 130
  LightRed    -> 203
  DarkGray    -> 238
  Gray        -> 245
  LightGreen  -> 120
  LightBlue   -> 81
  LightGray   -> 252

-- Aplica cor ANSI a um caractere
applyColor :: C64Color -> C64Color -> String -> String
applyColor fg bg text =
  "\ESC[38;5;" ++ show (c64ToANSI fg) ++ "m" ++
  "\ESC[48;5;" ++ show (c64ToANSI bg) ++ "m" ++
  text ++ "\ESC[0m"

-- Cores de fundo (para bordas C64)
c64BgColor :: C64Color -> String
c64BgColor Blue = "\ESC[48;5;21m"  -- Fundo azul típico do C64
c64BgColor _    = "\ESC[0m"

-- ========================
-- GERAÇÃO DA GRADE
-- ========================

-- Hash para seed baseada em string
c64Hash :: String -> Int
c64Hash s =
  let clean = filter isHexDigit (map toLower s)
      hexVal = if null clean then "c64" else take 8 clean
      folded = foldl (\acc c -> acc * 31 + fromEnum c) 5381 hexVal
  in folded .&. 0xFFFF  -- Limita a 16 bits como no C64 real

-- Seleciona símbolo baseado em seed e posição
selectSymbol :: Int -> Int -> Int -> C64Symbol
selectSymbol seed x y =
  let index = (seed `xor` (x * 7919) `xor` (y * 65537)) `mod` length symbolDatabase
  in symbolDatabase !! index

-- Gera grade 40x25 (resolução típica C64)
generateC64Grid :: Int -> [[C64Symbol]]
generateC64Grid seed =
  let symbols = [ [selectSymbol seed x y | x <- [0..39]] | y <- [0..24] ]
  in symbols

-- ========================
-- INTERFACE C64
-- ========================

-- Bordas no estilo C64
c64Border :: String
c64Border =
  let borderChar = " "
      topBottom = applyColor Blue Blue (replicate 42 ' ') ++ "\n"
      middle = applyColor Blue Blue " " ++ replicate 40 ' ' ++ applyColor Blue Blue " " ++ "\n"
  in topBottom ++ concat (replicate 25 middle) ++ topBottom

-- Cabeçalho no estilo C64 BASIC
c64Header :: String -> String
c64Header seed =
  applyColor White Blue "    **** COMMODORE 64 BASIC V2 ****    " ++ "\n" ++
  applyColor White Blue " 64K RAM SYSTEM  38911 BASIC BYTES FREE " ++ "\n" ++
  applyColor White Blue " READY.                                  " ++ "\n" ++
  applyColor Green Black "LOAD\"MATH CONSTANTS\",8,1" ++ "\n" ++
  applyColor Green Black "SEARCHING FOR MATH CONSTANTS" ++ "\n" ++
  applyColor Green Black "LOADING" ++ "\n" ++
  applyColor Green Black "READY." ++ "\n" ++
  applyColor White Black "RUN" ++ "\n" ++
  applyColor Cyan Black "MATH CONSTANTS GENERATOR V1.0" ++ "\n" ++
  applyColor Cyan Black ("SEED: " ++ take 8 seed ++ " (" ++ show (c64Hash seed) ++ ")") ++ "\n"

-- Renderiza a grade com cores C64
renderC64Grid :: [[C64Symbol]] -> String
renderC64Grid grid =
  let renderRow row =
        applyColor Blue Blue " " ++
        concatMap (\sym -> applyColor (symColor sym) Blue [symChar sym]) row ++
        applyColor Blue Blue " "
  in unlines (map renderRow grid)

-- Imprime tela completa no estilo C64
printC64Screen :: String -> [[C64Symbol]] -> IO ()
printC64Screen seed grid = do
  ANSI.clearScreen
  putStr $ c64Header seed
  putStrLn $ applyColor White Blue " GENERATING 40x25 MATH CONSTANTS GRID..." ++ "\n"
  putStr $ renderC64Grid grid
  putStrLn $ "\n" ++ applyColor Green Black " PRESS ANY KEY TO CONTINUE..."

-- ========================
-- ESTATÍSTICAS E ANÁLISE
-- ========================

-- Calcula estatísticas da grade
gridStatistics :: [[C64Symbol]] -> String
gridStatistics grid =
  let allSymbols = concat grid
      typeCount = foldl (\acc sym ->
        M.insertWith (+) (symType sym) 1 acc) M.empty allSymbols
      total = length allSymbols
      sorted = sortBy (\a b -> compare (snd b) (snd a)) (M.toList typeCount)
  in "=== GRID STATISTICS ===\n" ++
     unlines [ show typ ++ ": " ++ show count ++ " (" ++
               show (count * 100 `div` total) ++ "%)" | (typ, count) <- sorted ] ++
     "TOTAL SYMBOLS: " ++ show total ++ "\n" ++
     "UNIQUE SYMBOLS: " ++ show (length (nub (map symChar allSymbols)))

-- ========================
-- MENU E CONTROLES
-- ========================

-- Menu principal estilo C64
c64Menu :: IO ()
c64Menu = do
  putStrLn $ applyColor White Blue
    ("    C64 MATH CONSTANTS GENERATOR\n" ++
    "    " ++ replicate 34 '─' ++ "\n") ++
    applyColor Cyan Black " 1. GENERATE NEW CONSTANTS GRID\n" ++
    applyColor Cyan Black " 2. VIEW SYMBOL DATABASE\n" ++
    applyColor Cyan Black " 3. GRID STATISTICS\n" ++
    applyColor Cyan Black " 4. CHANGE SEED\n" ++
    applyColor Cyan Black " 5. EXPORT TO TEXT FILE\n" ++
    applyColor Cyan Black " 6. FILTER BY SYMBOL TYPE\n" ++
    applyColor Cyan Black " 7. EXIT TO BASIC\n\n" ++
    applyColor Yellow Black " SELECT OPTION: "

-- Lista símbolos por categoria
listSymbolsByType :: SymbolType -> IO ()
listSymbolsByType typ = do
  let symbols = filter (\s -> symType s == typ) symbolDatabase
      count = length symbols
  putStrLn $ "\n" ++ applyColor White Black (show typ ++ " (" ++ show count ++ " symbols):")
  forM_ (take 20 symbols) $ \sym -> do
    putStrLn $ "  " ++ [symChar sym] ++ " - " ++ symName sym
  when (count > 20) $
    putStrLn $ "  ... and " ++ show (count - 20) ++ " more"

-- Exporta grade para arquivo de texto
exportGridToFile :: String -> [[C64Symbol]] -> IO ()
exportGridToFile filename grid = do
  let content = unlines [ [symChar sym | sym <- row] | row <- grid ]
  writeFile filename content
  putStrLn $ applyColor Green Black ("GRID EXPORTED TO: " ++ filename)

-- ========================
-- PROGRAMA PRINCIPAL
-- ========================

-- Função auxiliar foldl'
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  let (mode, seedStr) = case args of
        ["--interactive"] -> ("menu", "C64MATH")
        [] -> ("grid", "C64MATH")
        ["--demo"] -> ("demo", "DEMO1234")
        ["--full"] -> ("full", "FULLGRID")
        [seed] -> ("grid", seed)
        ["--grid", seed] -> ("grid", seed)
        ["--stats", seed] -> ("stats", seed)
        _ -> error $ unlines [
          "C64 MATH CONSTANTS GENERATOR - USAGE:",
          "  c64math                     Interactive menu",
          "  c64math --demo              Demo mode",
          "  c64math --full              Full display mode",
          "  c64math [SEED]              Generate grid with seed",
          "  c64math --grid [SEED]       Generate grid",
          "  c64math --stats [SEED]      Show grid statistics"
          ]

  let seedValue = c64Hash seedStr
      grid = generateC64Grid seedValue

  case mode of
    "menu" -> interactiveMode seedStr grid
    "demo" -> do
      printC64Screen seedStr grid
      _ <- getLine
      putStrLn $ gridStatistics grid
    "full" -> do
      printC64Screen seedStr grid
      _ <- getLine
      return ()
    "grid" -> do
      putStrLn $ c64Header seedStr
      putStrLn $ renderC64Grid grid
    "stats" -> putStrLn $ gridStatistics grid
    _ -> putStrLn $ applyColor Red Black "UNKNOWN MODE"

-- Modo interativo
interactiveMode :: String -> [[C64Symbol]] -> IO ()
interactiveMode seedStr grid = do
  c64Menu
  option <- getLine

  case option of
    "1" -> do
      printC64Screen seedStr grid
      _ <- getLine
      interactiveMode seedStr grid

    "2" -> do
      putStrLn $ applyColor White Blue "\nSYMBOL CATEGORIES:\n"
      mapM_ (\typ -> listSymbolsByType typ >> return ())
            [MathConstant, PhysicalConstant, NumberSet, SIUnit, Operator, LogicSymbol, SetSymbol]
      putStr "\nPress Enter to continue..."
      _ <- getLine
      interactiveMode seedStr grid

    "3" -> do
      putStrLn $ gridStatistics grid
      putStr "\nPress Enter to continue..."
      _ <- getLine
      interactiveMode seedStr grid

    "4" -> do
      putStr $ applyColor Yellow Black "Enter new seed: "
      newSeed <- getLine
      let newSeedValue = c64Hash newSeed
          newGrid = generateC64Grid newSeedValue
      putStrLn $ applyColor Green Black "Seed updated!"
      interactiveMode newSeed newGrid

    "5" -> do
      timestamp <- getCurrentTime
      let filename = "c64grid_" ++ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" timestamp ++ ".txt"
      exportGridToFile filename grid
      interactiveMode seedStr grid

    "6" -> do
      putStrLn $ applyColor White Black "\nFilter by type:"
      putStrLn "1. Math Constants (π, ∞, ℏ)"
      putStrLn "2. Physical Constants (c, α, G)"
      putStrLn "3. Number Sets (ℕ, ℝ, ℂ)"
      putStrLn "4. SI Units (Ω, ℃, µ)"
      putStrLn "5. Mathematical Operators"
      putStr $ applyColor Yellow Black "Select type (1-5): "
      typChoice <- getLine
      let filteredGrid = case typChoice of
            "1" -> map (filter (\s -> symType s == MathConstant)) grid
            "2" -> map (filter (\s -> symType s == PhysicalConstant)) grid
            "3" -> map (filter (\s -> symType s == NumberSet)) grid
            "4" -> map (filter (\s -> symType s == SIUnit)) grid
            "5" -> map (filter (\s -> symType s == Operator)) grid
            _ -> grid
      putStrLn $ renderC64Grid filteredGrid
      interactiveMode seedStr grid

    "7" -> putStrLn $ applyColor Green Black "READY."

    _ -> do
      putStrLn $ applyColor Red Black "SYNTAX ERROR"
      interactiveMode seedStr grid
