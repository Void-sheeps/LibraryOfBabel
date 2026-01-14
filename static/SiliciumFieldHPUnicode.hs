{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Sequence ((|>), ViewL(..), viewl)
import Data.Word (Word8, Word16)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Complex (Complex(..), magnitude)
import Data.List (foldl', intersperse)
import Control.Monad (when, forM_)
import System.Random (RandomGen, randomR, mkStdGen, split)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)

-- ============================================================================
-- I. TIPOS E DEFINIÇÕES UNICODE
-- ============================================================================

type Pos = (Int, Int)
type Fixed12 = Int

-- Célula com suporte Unicode
data CelulaHP = CelulaHP
  { valorFx    :: !Fixed12
  , hpOperator :: !HPCategory
  , unicodeSym :: !Char  -- Símbolo Unicode único para esta célula
  } deriving (Show, Eq)

-- Categorias HP com emojis Unicode
data HPCategory
  = HP_BASIC_MATH      -- 🧮
  | HP_ADV_MATH        -- ∫
  | HP_STATISTICS      -- 📊
  | HP_NUMERIC         -- 🔢
  | HP_SCIENCE         -- 🔬
  | HP_ENGINEERING     -- ⚙️
  | HP_ELECTRICAL      -- ⚡
  | HP_PHYSICS         -- 🌌
  | HP_CHEMISTRY       -- 🧪
  | HP_ASTRONOMY       -- 🌠
  | HP_FINANCE         -- 💰
  | HP_GAMES           -- 🎮
  | HP_GRAPHICS        -- 🎨
  | HP_UTILITIES       -- 🛠️
  | HP_EMULATION       -- 🖥️
  | HP_EXPERIMENTAL    -- 🧪⚗️
  deriving (Show, Eq, Ord, Enum, Bounded)

type CampoHP = M.Map Pos CelulaHP
type HPOperator = CampoHP -> Pos -> Fixed12

-- ============================================================================
-- II. SÍMBOLOS UNICODE PARA VISUALIZAÇÃO
-- ============================================================================

-- Símbolos Unicode por categoria
categorySymbols :: HPCategory -> Char
categorySymbols = \case
  HP_BASIC_MATH    -> '🧮'
  HP_ADV_MATH      -> '∫'
  HP_STATISTICS    -> '📊'
  HP_NUMERIC       -> '🔢'
  HP_SCIENCE       -> '🔬'
  HP_ENGINEERING     -> '⚙'
  HP_ELECTRICAL    -> '⚡'
  HP_PHYSICS       -> '🌌'
  HP_CHEMISTRY     -> '🧪'
  HP_ASTRONOMY     -> '🌠'
  HP_FINANCE       -> '💰'
  HP_GAMES         -> '🎮'
  HP_GRAPHICS      -> '🎨'
  HP_UTILITIES     -> '🛠'
  HP_EMULATION     -> '🖥'
  HP_EXPERIMENTAL  -> '⚗'

-- Grades e bordas Unicode
borderTop, borderMiddle, borderBottom, borderLeft, borderRight, borderCross :: String
borderTop    = "┌─┬─┬─┬─┬─┬─┬─┐"
borderMiddle = "├─┼─┼─┼─┼─┼─┼─┤"
borderBottom = "└─┴─┴─┴─┴─┴─┴─┘"
borderLeft   = "│"
borderRight  = "│"
borderCross  = "┼"

-- Blocos Unicode para gráficos (8 níveis)
unicodeBlocks :: [Char]
unicodeBlocks = [' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']

-- Símbolos para valores específicos
valueSymbols :: [(Double, Char)]
valueSymbols =
  [ (0.0,   '○')
  , (0.25,  '◔')
  , (0.5,   '◑')
  , (0.75,  '◕')
  , (1.0,   '●')
  , (2.0,   '◆')
  , (3.0,   '▲')
  , (4.0,   '■')
  , (5.0,   '★')
  , (6.0,   '✦')
  , (7.0,   '✶')
  , (8.0,   '✷')
  ]

-- ============================================================================
-- III. OPERADORES HP UNICODE
-- ============================================================================

hpOperators :: M.Map HPCategory HPOperator
hpOperators = M.fromList
  [ (HP_BASIC_MATH, hpBasicMathUnicode)
  , (HP_ADV_MATH, hpAdvMathUnicode)
  , (HP_STATISTICS, hpStatisticsUnicode)
  , (HP_NUMERIC, hpNumericUnicode)
  , (HP_ENGINEERING, hpEngineeringUnicode)
  , (HP_ELECTRICAL, hpElectricalUnicode)
  , (HP_PHYSICS, hpPhysicsUnicode)
  , (HP_GRAPHICS, hpGraphicsUnicode)
  ]

-- Operador: Soma com símbolos Unicode
hpBasicMathUnicode :: HPOperator
hpBasicMathUnicode campo (x,y) =
  let cel = campo M.! (x,y)
      vals = [valorFx (campo M.! (x+dx, y+dy)) |
              dx <- [-1..1], dy <- [-1..1],
              M.member (x+dx, y+dy) campo]
  in sum vals `div` max 1 (length vals)

-- Operador: Transformada de Fourier simples (com complexos Unicode)
hpAdvMathUnicode :: HPOperator
hpAdvMathUnicode campo (x,y) =
  let n = 8
      samples = [fromIntegral (valorFx (campo M.! (x+i, y))) / 4096.0 |
                 i <- [0..n-1], M.member (x+i, y) campo]
      dftVal = sum [s * exp (0 :+ (-2 * pi * fromIntegral k / fromIntegral n)) |
                    (k, s) <- zip [0..] samples]
  in doubleToFixed (magnitude dftVal)

-- Operador: Estatística com histograma Unicode
hpStatisticsUnicode :: HPOperator
hpStatisticsUnicode campo pos@(x,y) =
  let neighbors = [campo M.! (x+dx, y+dy) |
                   dx <- [-1..1], dy <- [-1..1],
                   M.member (x+dx, y+dy) campo]
      values = map valorFx neighbors
      -- Calcula desvio padrão
      mean = sum values `div` fromIntegral (length values)
      variance = sum (map (\v -> (v - mean)^2) values) `div`
                 fromIntegral (length values)
  in variance `div` 256

-- Operador: Método de Newton com símbolos
hpNumericUnicode :: HPOperator
hpNumericUnicode campo (x,y) =
  let cel = campo M.! (x,y)
      x0 = fromIntegral (valorFx cel) / 4096.0
      -- f(x) = sin(x) - 0.5
      f x = sin x - 0.5
      f' x = cos x
      x1 = x0 - f x0 / f' x0
  in doubleToFixed x1

-- Operador: Engenharia com símbolos Unicode
hpEngineeringUnicode :: HPOperator
hpEngineeringUnicode campo (x,y) =
  -- Pressão em um ponto: P = ρgh
  let cel = campo M.! (x,y)
      h = fromIntegral (valorFx cel) / 4096.0  -- Altura
      ρ = 1000.0  -- Densidade da água
      g = 9.81    -- Gravidade
      pressure = ρ * g * h
  in doubleToFixed (pressure / 1000.0)  -- Em kPa

-- Operador: Circuitos elétricos
hpElectricalUnicode :: HPOperator
hpElectricalUnicode campo (x,y) =
  -- Potência em um resistor: P = I²R
  let cel = campo M.! (x,y)
      current = fromIntegral (valorFx cel) / 4096.0
      neighbor = M.lookup (x+1, y) campo
      resistance = fromIntegral (maybe 4096 valorFx neighbor) / 4096.0
      power = current * current * resistance
  in doubleToFixed power

-- Operador: Física quântica simples
hpPhysicsUnicode :: HPOperator
hpPhysicsUnicode campo (x,y) =
  -- Função de onda: ψ(x) = exp(-x²/2)
  let cel = campo M.! (x,y)
      xVal = fromIntegral (valorFx cel) / 4096.0
      psi = exp (-(xVal * xVal) / 2.0)
  in doubleToFixed (psi * psi)  -- Densidade de probabilidade

-- Operador: Gráficos 3D Unicode
hpGraphicsUnicode :: HPOperator
hpGraphicsUnicode campo (x,y) =
  -- Superfície z = sin(x) * cos(y)
  let xVal = fromIntegral x * 0.1
      yVal = fromIntegral y * 0.1
      z = sin xVal * cos yVal
  in doubleToFixed ((z + 1.0) * 2.0)

-- ============================================================================
-- IV. CONVERSÕES E UTILITÁRIOS
-- ============================================================================

doubleToFixed :: Double -> Fixed12
doubleToFixed x = floor (x * 4096.0)

fixedToDouble :: Fixed12 -> Double
fixedToDouble x = fromIntegral x / 4096.0

clampFixed12 :: Fixed12 -> Fixed12
clampFixed12 x
  | x < (-32768) = -32768
  | x > 32767    = 32767
  | otherwise    = x

-- Converte valor para símbolo Unicode
valueToSymbol :: Fixed12 -> Char
valueToSymbol val =
  let dVal = fixedToDouble val
      absVal = abs dVal
  in case lookup (floor absVal) (map (\(v,c) -> (floor v, c)) valueSymbols) of
       Just sym -> sym
       Nothing -> if absVal < 0.25 then '○'
                  else if absVal < 0.5 then '◔'
                  else if absVal < 0.75 then '◑'
                  else if absVal < 1.0 then '◕'
                  else if absVal < 2.0 then '●'
                  else if absVal < 4.0 then '◆'
                  else '★'

-- ============================================================================
-- V. AVALIAÇÃO DO CAMPO UNICODE
-- ============================================================================

avaliarCampoHPUnicode :: CampoHP -> CampoHP
avaliarCampoHPUnicode campo =
  M.mapWithKey aplicarOperadorUnicode campo
  where
    aplicarOperadorUnicode :: Pos -> CelulaHP -> CelulaHP
    aplicarOperadorUnicode pos cel@CelulaHP{..} =
      case M.lookup hpOperator hpOperators of
        Just op -> let newVal = clampFixed12 (op campo pos)
                       newSym = valueToSymbol newVal
                   in cel { valorFx = newVal, unicodeSym = newSym }
        Nothing -> cel

-- ============================================================================
-- VI. VISUALIZAÇÃO UNICODE AVANÇADA
-- ============================================================================

-- Renderiza campo como grade Unicode com bordas
renderCampoUnicode :: CampoHP -> Int -> Int -> T.Text
renderCampoUnicode campo width height =
  T.unlines $
    [T.pack borderTop] ++
    intersperse (T.pack borderMiddle) (map renderRow [0..height-1]) ++
    [T.pack borderBottom]
  where
    renderRow y = T.concat $
      T.pack borderLeft :
      [T.pack (cellString x y) | x <- [0..width-1]] ++
      [T.pack borderRight]

    cellString x y =
      case M.lookup (x,y) campo of
        Nothing -> "   "
        Just CelulaHP{..} ->
          let sym = unicodeSym
              catSym = categorySymbols hpOperator
          in [catSym, sym, ' ']

-- Renderiza campo como heatmap Unicode
renderHeatmapUnicode :: CampoHP -> Int -> Int -> T.Text
renderHeatmapUnicode campo width height =
  T.unlines $ map renderRow [0..height-1]
  where
    renderRow y = T.pack $ concatMap (renderCell y) [0..width-1]

    renderCell y x =
      case M.lookup (x,y) campo of
        Nothing -> " "
        Just CelulaHP{..} ->
          let val = fromIntegral (valorFx + 32768) * 8 `div` 65536
              block = unicodeBlocks !! max 0 (min 8 val)
          in [block]

-- Renderiza campo como gráfico 3D ASCII art
render3DGraphUnicode :: CampoHP -> Int -> Int -> T.Text
render3DGraphUnicode campo width height =
  T.unlines $ map (render3DRow height) [height-1, height-2 .. 0]
  where
    render3DRow totalHeight y =
      T.pack $ concatMap (render3DCell y totalHeight) [0..width-1]

    render3DCell y totalHeight x =
      case M.lookup (x,y) campo of
        Nothing -> " "
        Just CelulaHP{..} ->
          let heightRatio = fromIntegral y / fromIntegral totalHeight
              valRatio = fromIntegral (valorFx + 2048) / 4096.0
              combined = (heightRatio + valRatio) / 2.0
              idx = floor (combined * 8)
          in [unicodeBlocks !! max 0 (min 8 idx)]

-- ============================================================================
-- VII. OSCILOSCÓPIO UNICODE
-- ============================================================================

data HPOscilloscopeUnicode = HPOscilloscopeUnicode
  { hpBuffersUnicode :: M.Map HPCategory (Seq.Seq Fixed12)
  , hpWritePtrUnicode :: !Int
  , hpBufferSizeUnicode :: !Int
  , hpActiveChannels :: ![HPCategory]
  }

initHPOscilloscopeUnicode :: Int -> [HPCategory] -> HPOscilloscopeUnicode
initHPOscilloscopeUnicode size cats = HPOscilloscopeUnicode
  { hpBuffersUnicode = M.fromList [(cat, Seq.replicate size 0) | cat <- cats]
  , hpWritePtrUnicode = 0
  , hpBufferSizeUnicode = size
  , hpActiveChannels = cats
  }

sampleHPOscilloscopeUnicode :: CampoHP -> HPOscilloscopeUnicode -> HPOscilloscopeUnicode
sampleHPOscilloscopeUnicode campo osc@HPOscilloscopeUnicode{..} =
  let newBuffers = M.mapWithKey updateBuffer hpBuffersUnicode
      updateBuffer cat buffer =
        let values = [valorFx cel | (_, cel) <- M.toList campo,
                     hpOperator cel == cat]
            avg = if null values then 0
                  else sum values `div` fromIntegral (length values)
            newBuffer = buffer |> avg
        in if Seq.length newBuffer > hpBufferSizeUnicode
           then Seq.drop 1 newBuffer
           else newBuffer
      newPtr = (hpWritePtrUnicode + 1) `mod` hpBufferSizeUnicode
  in osc { hpBuffersUnicode = newBuffers, hpWritePtrUnicode = newPtr }

-- Renderiza osciloscópio com caracteres Unicode
renderHPOscilloscopeUnicode :: HPOscilloscopeUnicode -> T.Text
renderHPOscilloscopeUnicode HPOscilloscopeUnicode{..} =
  T.unlines $ header : map renderChannel (M.toList hpBuffersUnicode)
  where
    header = T.pack "📈 OSCILOSCÓPIO HP UNICODE 📉"

    renderChannel :: (HPCategory, Seq.Seq Fixed12) -> T.Text
    renderChannel (cat, buffer) =
      let samples = take 60 (toList buffer)
          normalized = map (\v ->
            let scaled = ((v + 2048) * 8) `div` 4096
            in max 0 (min 8 scaled)) samples
          waveform = T.pack $ map (unicodeWaveChars !!) normalized
          catSym = categorySymbols cat
          peak = maximum samples
          rms :: Double
          rms = sqrt (fromIntegral (sum (map (^2) samples)) /
                      fromIntegral (length samples))
      in T.pack (printf "%c %-12s ┤%s├  Pico: %6.2f  RMS: %6.2f"
                catSym
                (take 12 (show cat))
                (T.unpack waveform)
                (fixedToDouble peak)
                rms)

    unicodeWaveChars = " _▁▂▃▄▅▆▇█"

-- ============================================================================
-- VIII. GERADOR DE CAMPO UNICODE
-- ============================================================================

campoHPUnicodeExemplo :: CampoHP
campoHPUnicodeExemplo = M.fromList $
  [((x, y), createCell x y) | x <- [0..6], y <- [0..4]]
  where
    createCell x y =
      let cat = toEnum ((x + y * 7) `mod` 16) :: HPCategory
          val = doubleToFixed (sin (fromIntegral x) * cos (fromIntegral y) + 1.0)
          sym = valueToSymbol val
      in CelulaHP val cat sym

-- ============================================================================
-- IX. SIMULAÇÃO UNICODE
-- ============================================================================

simularHPUnicode :: Int -> CampoHP -> IO ()
simularHPUnicode passos campoInicial = do
  TIO.putStrLn $ T.pack "╔════════════════════════════════════════════╗"
  TIO.putStrLn $ T.pack "║    SILICIUMFIELD HP - UNICODE EDITION     ║"
  TIO.putStrLn $ T.pack "║      Integração com hpcalc.org            ║"
  TIO.putStrLn $ T.pack "╚════════════════════════════════════════════╝"
  TIO.putStrLn $ T.pack ""

  let catsAtivas = [HP_BASIC_MATH, HP_ADV_MATH, HP_STATISTICS, HP_ENGINEERING,
                    HP_ELECTRICAL, HP_PHYSICS, HP_GRAPHICS]
      osc = initHPOscilloscopeUnicode 60 catsAtivas

  TIO.putStrLn $ T.pack "🔮 LEGENDA DE SÍMBOLOS:"
  TIO.putStrLn $ T.pack "  🧮 Matemática Básica  ∫ Matemática Avançada"
  TIO.putStrLn $ T.pack "  📊 Estatística        🔢 Numérico"
  TIO.putStrLn $ T.pack "  ⚙  Engenharia         ⚡ Elétrica"
  TIO.putStrLn $ T.pack "  🌌 Física             🎨 Gráficos"
  TIO.putStrLn $ T.pack ""

  loopSimulacaoUnicode passos 1 campoInicial osc
  where
    loopSimulacaoUnicode :: Int -> Int -> CampoHP -> HPOscilloscopeUnicode -> IO ()
    loopSimulacaoUnicode 0 _ campo _ = do
      TIO.putStrLn $ T.pack "\n🏁 Simulação finalizada!"
      TIO.putStrLn $ renderCampoUnicode campo 7 5

    loopSimulacaoUnicode n passo campo osc = do
      let novoCampo = avaliarCampoHPUnicode campo
          novoOsc = sampleHPOscilloscopeUnicode novoCampo osc

      when (passo `mod` 3 == 0) $ do
        TIO.putStrLn $ T.pack $ printf "🔹 Passo %d 🔹" passo
        TIO.putStrLn $ renderCampoUnicode novoCampo 7 5
        TIO.putStrLn $ T.pack "📊 Heatmap:"
        TIO.putStrLn $ renderHeatmapUnicode novoCampo 7 5
        TIO.putStrLn $ renderHPOscilloscopeUnicode novoOsc
        TIO.putStrLn $ T.pack " "

      loopSimulacaoUnicode (n-1) (passo+1) novoCampo novoOsc

-- ============================================================================
-- X. PROGRAMA PRINCIPAL UNICODE
-- ============================================================================

main :: IO ()
main = do
  -- Configuração para suportar Unicode no terminal
  TIO.putStrLn $ T.pack "🔧 Configurando terminal para Unicode..."

  TIO.putStrLn $ T.pack "🚀 Iniciando SiliciumField HP Unicode"
  TIO.putStrLn $ T.pack "📁 Conectando ao hpcalc.org conceitualmente..."
  TIO.putStrLn $ T.pack ""

  -- Mostra símbolos disponíveis
  TIO.putStrLn $ T.pack "🎨 Paleta Unicode disponível:"
  TIO.putStrLn $ T.pack $ "  Blocos: " ++ unicodeBlocks
  TIO.putStrLn $ T.pack $ "  Símbolos: " ++ map snd valueSymbols
  TIO.putStrLn $ T.pack ""

  -- Inicia simulação
  TIO.putStrLn $ T.pack "▶️  Iniciando simulação (15 passos)..."
  simularHPUnicode 15 campoHPUnicodeExemplo

  -- Estatísticas finais
  TIO.putStrLn $ T.pack "\n📈 ESTATÍSTICAS FINAIS:"
  TIO.putStrLn $ T.pack "  • Categorias HP: 16"
  TIO.putStrLn $ T.pack "  • Operadores implementados: 8"
  TIO.putStrLn $ T.pack $ "  • Símbolos Unicode: " ++ show (length unicodeBlocks + length valueSymbols)
  TIO.putStrLn $ T.pack "  • Resolução: Q4.12 (-8.000 a +7.999)"
  TIO.putStrLn $ T.pack "  • Buffer osciloscópio: 60 amostras"
  TIO.putStrLn $ T.pack ""
  TIO.putStrLn $ T.pack "✨ Simulação completa! ✨"

-- Função para exportar campo como arte Unicode
exportarArteUnicode :: CampoHP -> Int -> Int -> FilePath -> IO ()
exportarArteUnicode campo width height caminho = do
  let arte = renderCampoUnicode campo width height
  TIO.writeFile caminho arte
  TIO.putStrLn $ T.pack $ "💾 Arte salva em: " ++ caminho

-- ============================================================================
-- XI. EXEMPLO DE USO RÁPIDO
-- ============================================================================

-- Para executar rapidamente:
-- main = mainUnicode

-- Para exportar uma visualização:
-- main = do
--   let campo = campoHPUnicodeExemplo
--   exportarArteUnicode campo 7 5 "siliciumfield_arte.txt"
--   TIO.putStrLn $ render3DGraphUnicode campo 7 5
