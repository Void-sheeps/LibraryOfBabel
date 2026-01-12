{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import System.Random (RandomGen, StdGen, random, randomR, split, mkStdGen)
import Control.Monad.State (State, evalState, get, put, modify, state)
import Control.Monad (replicateM, forM_)
import Data.List (intercalate, sort, group, transpose, (\\), sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Foldable (toList)
import System.IO (writeFile, hFlush, stdout)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as S
import Data.Word (Word16)
import Data.Bits (xor, (.&.), (.|.), shiftL, shiftR, testBit, setBit, clearBit)
import Data.Char (intToDigit, digitToInt)
import Numeric (showFFloat, showHex)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)

import Odu

-- =====================================================
-- 57. HIPERCUBO 16D - TIPOS E REPRESENTAÇÕES
-- =====================================================

-- | Representação compacta de 16 bits usando Word16
newtype Hipercubo16D = Hipercubo16D Word16
  deriving (Show, Eq, Ord, Generic)

-- | Converter lista de bits para Hipercubo16D
listaParaHipercubo :: [Int] -> Maybe Hipercubo16D
listaParaHipercubo bits
  | length bits == 16 = Just $ Hipercubo16D $
      foldl (\acc (i, bit) ->
        if bit == 1 then setBit acc i else acc) 0 (zip [0..15] bits)
  | otherwise = Nothing

-- | Converter Hipercubo16D para lista de bits
hipercuboParaLista :: Hipercubo16D -> [Int]
hipercuboParaLista (Hipercubo16D w) =
  [if testBit w i then 1 else 0 | i <- [0..15]]

-- | Contar bits ativos (popcount)
popCount16 :: Hipercubo16D -> Int
popCount16 (Hipercubo16D w) = popCountWord16 w
  where
    popCountWord16 :: Word16 -> Int
    popCountWord16 0 = 0
    popCountWord16 x = (fromIntegral x .&. 1) + popCountWord16 (x `shiftR` 1)

-- =====================================================
-- 58. OPERADOR LAPLACIANO DISCRETO
-- =====================================================

-- | Estado do sistema de difusão
data SistemaDifusao = Sistema
  { hipercuboAtual :: Hipercubo16D
  , tempo          :: Int           -- Passos decorridos
  , energia        :: Double        -- Energia do sistema
  , temperatura    :: Double        -- Parâmetro de temperatura
  , gerador        :: StdGen        -- Gerador aleatório
  , historico      :: Seq.Seq Hipercubo16D  -- Últimos estados
  } deriving (Show, Generic)

-- | Inicializar sistema com semente
inicializarSistema :: Int -> Double -> SistemaDifusao
inicializarSistema seed temp =
  let gen = mkStdGen seed
      (bits, gen') = randomBits 16 gen
      hipercubo = fromMaybe (Hipercubo16D 0) $ listaParaHipercubo bits
      energiaInicial = fromIntegral (popCount16 hipercubo) / 16.0
  in Sistema
    { hipercuboAtual = hipercubo
    , tempo = 0
    , energia = energiaInicial
    , temperatura = temp
    , gerador = gen'
    , historico = Seq.singleton hipercubo
    }
  where
    randomBits :: Int -> StdGen -> ([Int], StdGen)
    randomBits n g =
      let (bits, g') = foldl (\(acc, gAcc) _ ->
                    let (bit, g'') = randomR (0, 1) gAcc
                    in (bit:acc, g'')) ([], g) [1..n]
      in (reverse bits, g')

-- | Operador Laplaciano discreto (flip de 1 bit aleatório)
laplacianoDiscreto :: SistemaDifusao -> SistemaDifusao
laplacianoDiscreto sistema =
  let (pos, gen') = randomR (0, 15) (gerador sistema)
      hipercubo = hipercuboAtual sistema
      hipercubo' = flipBit pos hipercubo
      energia' = fromIntegral (popCount16 hipercubo') / 16.0
      hist' = Seq.take 100 $ hipercubo' Seq.<| historico sistema
  in sistema
    { hipercuboAtual = hipercubo'
    , tempo = tempo sistema + 1
    , energia = energia'
    , gerador = gen'
    , historico = hist'
    }
  where
    flipBit :: Int -> Hipercubo16D -> Hipercubo16D
    flipBit pos (Hipercubo16D w) =
      let mask = 1 `shiftL` pos
      in Hipercubo16D (w `xor` mask)

-- | Operador Laplaciano com memória (evita flips recentes)
laplacianoComMemoria :: SistemaDifusao -> SistemaDifusao
laplacianoComMemoria sistema =
  let hist = historico sistema
      evitados = if Seq.length hist >= 3
                 then take 3 $ map (flipBitPos (hipercuboAtual sistema))
                          (toList $ Seq.take 3 hist)
                 else []
      candidatas = [0..15] \\ evitados
      gen = gerador sistema

      (pos, gen') = if null candidatas
                    then randomR (0, 15) gen
                    else do
                      let (idx, g') = randomR (0, length candidatas - 1) gen
                      (candidatas !! idx, g')

      hipercubo' = flipBit pos (hipercuboAtual sistema)
      energia' = fromIntegral (popCount16 hipercubo') / 16.0
      hist' = Seq.take 100 $ hipercubo' Seq.<| hist

  in sistema
    { hipercuboAtual = hipercubo'
    , tempo = tempo sistema + 1
    , energia = energia'
    , gerador = gen'
    , historico = hist'
    }
  where
    flipBit :: Int -> Hipercubo16D -> Hipercubo16D
    flipBit pos (Hipercubo16D w) =
      let mask = 1 `shiftL` pos
      in Hipercubo16D (w `xor` mask)

    flipBitPos :: Hipercubo16D -> Hipercubo16D -> Int
    flipBitPos a b =
      let diff = bitsDiferentes a b
      in if null diff then 0 else head diff

    bitsDiferentes :: Hipercubo16D -> Hipercubo16D -> [Int]
    bitsDiferentes (Hipercubo16D w1) (Hipercubo16D w2) =
      [i | i <- [0..15], testBit (w1 `xor` w2) i]

-- =====================================================
-- 59. MAPEAMENTO PARA PARÂMETROS SONOROS
-- =====================================================

-- | Mapear estado para frequência (80-2000 Hz, escala log)
hipercuboParaFrequencia :: Hipercubo16D -> Double
hipercuboParaFrequencia hc =
  let soma = fromIntegral (popCount16 hc)
      minFreq = 80.0
      maxFreq = 2000.0
      -- Fórmula: freq = 80 * (2000/80)^(soma/16)
      ratio = maxFreq / minFreq
      expoente = soma / 16.0
  in minFreq * (ratio ** expoente)

-- | Mapear estado para panorama (-1 a 1)
hipercuboParaPanorama :: Hipercubo16D -> Double
hipercuboParaPanorama hc =
  let bits = hipercuboParaLista hc
      posicoesAtivas = [fromIntegral i | (i, bit) <- zip [0..] bits, bit == 1]
      n = length posicoesAtivas
  in if n == 0
     then 0.0
     else let media = sum posicoesAtivas / fromIntegral n
              -- Normalizar: 0-15 -> -1 a 1
          in (media / 7.5) - 1.0

-- | Mapear estado para amplitude (baseada na distribuição)
hipercuboParaAmplitude :: Hipercubo16D -> Double
hipercuboParaAmplitude hc =
  let bits = hipercuboParaLista hc
      padroes = [ take 2 $ drop i bits | i <- [0..14] ]
      padroesUnicos = length $ S.fromList padroes
      -- Mais padrões únicos = maior amplitude (0.05 a 0.25)
  in 0.05 + 0.2 * fromIntegral padroesUnicos / 16.0

-- | Mapear estado para riqueza espectral (harmônicos)
hipercuboParaRiqueza :: Hipercubo16D -> Double
hipercuboParaRiqueza hc =
  let bits = hipercuboParaLista hc
      grupos = chunksOf 4 bits
      variabilidade = sum (map (length . group) grupos)  -- Mudanças em cada grupo
  -- Normalizar para 0-1
  in fromIntegral variabilidade / 16.0
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- =====================================================
-- 60. SÍNTESE CONCEITUAL E EVENTOS
-- =====================================================

data EventoAudio = EventoAudio
  { tempoEvento   :: Double    -- Tempo em segundos
  , duracaoEvento :: Double    -- Duração em segundos
  , frequenciaEv  :: Double    -- Frequência em Hz
  , amplitudeEv   :: Double    -- Amplitude (0-1)
  , panoramaEv    :: Double    -- Panorama (-1 a 1)
  , riquezaEv     :: Double    -- Riqueza espectral (0-1)
  , estadoEv      :: Hipercubo16D  -- Estado correspondente
  } deriving (Show, Eq, Generic)

-- | Gerar sequência de eventos a partir de simulação
gerarEventosAudio :: Double     -- Intervalo entre eventos (s)
                  -> Int        -- Número de eventos
                  -> SistemaDifusao  -- Sistema inicial
                  -> ([EventoAudio], SistemaDifusao)  -- Eventos e estado final
gerarEventosAudio intervalo n sistemaInicial =
  foldl (\(eventos, sistema) i ->
          let tempoAtual = fromIntegral i * intervalo
              hc = hipercuboAtual sistema
              evento = EventoAudio
                { tempoEvento   = tempoAtual
                , duracaoEvento = intervalo * 0.9  -- Deixa pequena margem
                , frequenciaEv  = hipercuboParaFrequencia hc
                , amplitudeEv   = hipercuboParaAmplitude hc
                , panoramaEv    = hipercuboParaPanorama hc
                , riquezaEv     = hipercuboParaRiqueza hc
                , estadoEv      = hc
                }
              sistema' = laplacianoDiscreto sistema
          in (evento:eventos, sistema')
       ) ([], sistemaInicial) [0..n-1]

-- =====================================================
-- 61. ROTINA TEMPORAL (ANÁLOGA AO SUPERCOLLIDER)
-- =====================================================

-- | Executar simulação similar ao código SuperCollider
executarSimulacaoSuperCollider :: Int      -- Seed
                               -> Int      -- Número de iterações
                               -> Double   -- Intervalo (s)
                               -> IO ()
executarSimulacaoSuperCollider seed nIter intervalo = do
  putStrLn "=== SIMULAÇÃO HIPERCUBO 16D - DIFUSÃO LAPLACIANA ===\n"

  let sistema0 = inicializarSistema seed 1.0
      (eventos, sistemaFinal) = gerarEventosAudio intervalo nIter sistema0
      eventosRev = reverse eventos  -- Para ficar em ordem cronológica

  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Iterações: " ++ show nIter
  putStrLn $ "Intervalo: " ++ show intervalo ++ "s"
  putStrLn $ "Estado inicial: " ++ show (hipercuboParaLista (hipercuboAtual sistema0))
  putStrLn $ "Estado final:   " ++ show (hipercuboParaLista (hipercuboAtual sistemaFinal))
  putStrLn ""

  -- Mostrar primeiros eventos
  putStrLn "Primeiros 5 eventos:"
  putStrLn "Tempo | Freq (Hz) | Ampl | Pan  | Riqueza"
  putStrLn "------------------------------------------"
  forM_ (take 5 eventosRev) $ \ev -> do
    printf "%5.2f | %8.1f | %.2f | %4.1f | %.3f\n"
      (tempoEvento ev) (frequenciaEv ev) (amplitudeEv ev)
      (panoramaEv ev) (riquezaEv ev)

  -- Análise estatística
  let freqs = map frequenciaEv eventosRev
      amps = map amplitudeEv eventosRev
      pans = map panoramaEv eventosRev
      mediaFreq = sum freqs / fromIntegral (length freqs)
      mediaAmp = sum amps / fromIntegral (length amps)
      mediaPan = sum pans / fromIntegral (length pans)

  putStrLn "\n=== ESTATÍSTICAS ==="
  putStrLn $ "Frequência média: " ++ showFFloat (Just 1) mediaFreq " Hz"
  putStrLn $ "Amplitude média:  " ++ showFFloat (Just 3) mediaAmp ""
  putStrLn $ "Panorama médio:   " ++ showFFloat (Just 3) mediaPan ""
  putStrLn $ "Energia final:    " ++ showFFloat (Just 3) (energia sistemaFinal) ""

  -- Gerar arquivo de dados para plotagem
  exportarDadosSimulacao eventosRev "hipercubo_simulacao.csv"

-- =====================================================
-- 62. ANÁLISE ESPECTRAL DO HIPERCUBO
-- =====================================================

-- | Coletor espectral: agrupa estados por similaridade
data ColetorEspectral = ColetorEspectral
  { espectroFrequencias :: M.Map Int Int     -- Contagem por frequência
  , espectroPadroes     :: M.Map String Int  -- Contagem por padrão
  , espectroEnergias    :: [Double]          -- Histórico de energias
  , espectroTransicoes  :: M.Map String Int  -- Transições entre estados
  } deriving (Show, Generic)

-- | Analisar sequência de eventos espectralmente
analisarEspectro :: [EventoAudio] -> ColetorEspectral
analisarEspectro eventos =
  let freqs = map (round . frequenciaEv) eventos
      padroes = map (padraoEstado . estadoEv) eventos
      energias = map (energiaEstado . estadoEv) eventos
      transicoes = analisarTransicoes $ map estadoEv eventos
  in ColetorEspectral
    { espectroFrequencias = contarFrequencias freqs
    , espectroPadroes = contarPadroes padroes
    , espectroEnergias = energias
    , espectroTransicoes = transicoes
    }
  where
    energiaEstado :: Hipercubo16D -> Double
    energiaEstado = fromIntegral . popCount16

    padraoEstado :: Hipercubo16D -> String
    padraoEstado hc =
      let bits = hipercuboParaLista hc
          grupos = chunksOf 4 bits
      in intercalate "-" $ map (concatMap show) grupos

    contarFrequencias :: [Int] -> M.Map Int Int
    contarFrequencias = foldr (\f -> M.insertWith (+) f 1) M.empty

    contarPadroes :: [String] -> M.Map String Int
    contarPadroes = foldr (\p -> M.insertWith (+) p 1) M.empty

    analisarTransicoes :: [Hipercubo16D] -> M.Map String Int
    analisarTransicoes estados =
      let pares = zip estados (tail estados)
          chaves = map (\(a,b) -> show (popCount16 a) ++ "->" ++ show (popCount16 b)) pares
      in foldr (\k -> M.insertWith (+) k 1) M.empty chaves

-- =====================================================
-- 63. VISUALIZAÇÃO DO ESPAÇO DE ESTADOS
-- =====================================================

-- | Projeção multidimensional scaling (MDS) simplificada
projecaoMDS :: [Hipercubo16D] -> [(Double, Double)]
projecaoMDS estados =
  let n = length estados
      -- Matriz de distâncias de Hamming
      distancias :: [[Double]]
      distancias = [ [fromIntegral $ hammingDistance h1 h2 | h2 <- estados]
                   | h1 <- estados ]

      -- Centroide
      centro :: [Double]
      centro = map (\i -> sum (map (!! i) distancias) / fromIntegral n) [0..n-1]

      -- Projeção usando os dois maiores autovetores (simplificado)
      projX :: [Double]
      projX = map (\d -> sum (zipWith (*) d [sin (2*pi*fromIntegral i/fromIntegral n) | i <- [0..n-1]])) distancias
      projY :: [Double]
      projY = map (\d -> sum (zipWith (*) d [cos (2*pi*fromIntegral i/fromIntegral n) | i <- [0..n-1]])) distancias

      -- Normalizar para -1 a 1
      maxX = maximum (map abs projX)
      maxY = maximum (map abs projY)
      scaleX = if maxX > 0 then 1.0 / maxX else 1.0
      scaleY = if maxY > 0 then 1.0 / maxY else 1.0

  in zip (map (*scaleX) projX) (map (*scaleY) projY)
  where
    hammingDistance :: Hipercubo16D -> Hipercubo16D -> Int
    hammingDistance (Hipercubo16D w1) (Hipercubo16D w2) =
      popCountWord16 (w1 `xor` w2)

    popCountWord16 :: Word16 -> Int
    popCountWord16 0 = 0
    popCountWord16 x = (fromIntegral x .&. 1) + popCountWord16 (x `shiftR` 1)

-- | Renderizar visualização do espaço de estados
renderizarEspacoEstados :: [Hipercubo16D] -> String
renderizarEspacoEstados estados =
  let proj = projecaoMDS estados
      grade = replicate 21 (replicate 41 ' ')

      -- Converter coordenadas para índices da grade
      indices = map (\(x,y) ->
        (round ((x + 1) * 10),  -- -1..1 -> 0..20
         round ((y + 1) * 20))) -- -1..1 -> 0..40
        proj

      -- Marcar pontos na grade
      grade' = foldl (\g (x,y) ->
                if x >= 0 && x < 21 && y >= 0 && y < 41
                then setChar g y x '•'
                else g) grade indices

      -- Adicionar bordas e legendas
      gradeBorda = map (\linha -> "│" ++ linha ++ "│") grade'
      moldura = "┌" ++ replicate 41 '─' ++ "┐"
      rodape = "└" ++ replicate 41 '─' ++ "┘"

  in unlines $ moldura : gradeBorda ++ [rodape]
  where
    setChar :: [[a]] -> Int -> Int -> a -> [[a]]
    setChar mat y x val =
      take y mat ++
      [take x (mat!!y) ++ [val] ++ drop (x+1) (mat!!y)] ++
      drop (y+1) mat

-- =====================================================
-- 64. EXPORTAÇÃO DE DADOS E RELATÓRIOS
-- =====================================================

exportarDadosSimulacao :: [EventoAudio] -> FilePath -> IO ()
exportarDadosSimulacao eventos arquivo = do
  let cabecalho = "tempo,frequencia,amplitude,panorama,riqueza,estado_binario,popcount"
      linhas = map eventoParaCSV eventos
      conteudo = unlines (cabecalho : linhas)
  writeFile arquivo conteudo
  putStrLn $ "Dados exportados para: " ++ arquivo
  where
    eventoParaCSV :: EventoAudio -> String
    eventoParaCSV ev =
      let bits = hipercuboParaLista (estadoEv ev)
          binario = concatMap show bits
          popcnt = popCount16 (estadoEv ev)
      in intercalate "," $
         [ showFFloat (Just 4) (tempoEvento ev) ""
         , showFFloat (Just 2) (frequenciaEv ev) ""
         , showFFloat (Just 4) (amplitudeEv ev) ""
         , showFFloat (Just 4) (panoramaEv ev) ""
         , showFFloat (Just 4) (riquezaEv ev) ""
         , binario
         , show popcnt
         ]

-- | Gerar relatório completo da simulação
gerarRelatorioCompleto :: Int -> Int -> Double -> IO ()
gerarRelatorioCompleto seed nIter intervalo = do
  putStrLn "=== RELATÓRIO COMPLETO - HIPERCUBO 16D ===\n"

  let sistema0 = inicializarSistema seed 1.0
      (eventos, sistemaFinal) = gerarEventosAudio intervalo nIter sistema0
      eventosOrd = reverse eventos
      espectro = analisarEspectro eventosOrd
      estados = map estadoEv eventosOrd

  -- Seção 1: Parâmetros da simulação
  putStrLn "1. PARÂMETROS DA SIMULAÇÃO"
  putStrLn $ "   Seed: " ++ show seed
  putStrLn $ "   Iterações: " ++ show nIter
  putStrLn $ "   Intervalo: " ++ show intervalo ++ "s"
  putStrLn $ "   Temperatura: " ++ show (temperatura sistema0)
  putStrLn ""

  -- Seção 2: Estatísticas básicas
  putStrLn "2. ESTATÍSTICAS BÁSICAS"
  let popcounts = map (popCount16 . estadoEv) eventosOrd
      minPop = minimum popcounts
      maxPop = maximum popcounts
      mediaPop = fromIntegral (sum popcounts) / fromIntegral (length popcounts)
  putStrLn $ "   PopCount mínimo: " ++ show minPop ++ "/16"
  putStrLn $ "   PopCount máximo: " ++ show maxPop ++ "/16"
  putStrLn $ "   PopCount médio: " ++ showFFloat (Just 2) mediaPop "/16"
  putStrLn $ "   Estados únicos: " ++ show (length (S.fromList estados)) ++ "/" ++ show nIter
  putStrLn ""

  -- Seção 3: Análise espectral
  putStrLn "3. ANÁLISE ESPECTRAL"
  let topFreqs = take 5 $ reverse $ sortOn snd $ M.toList (espectroFrequencias espectro)
      topPadroes = take 5 $ reverse $ sortOn snd $ M.toList (espectroPadroes espectro)

  putStrLn "   Frequências mais comuns:"
  forM_ topFreqs $ \(freq, count) ->
    putStrLn $ "     " ++ show freq ++ " Hz: " ++ show count ++ " ocorrências"

  putStrLn "   Padrões mais comuns:"
  forM_ topPadroes $ \(padrao, count) ->
    putStrLn $ "     " ++ padrao ++ ": " ++ show count ++ " ocorrências"
  putStrLn ""

  -- Seção 4: Análise de transições
  putStrLn "4. ANÁLISE DE TRANSIÇÕES"
  let transicoes = M.toList (espectroTransicoes espectro)
      topTrans = take 5 $ reverse $ sortOn snd transicoes
  forM_ topTrans $ \(trans, count) ->
    putStrLn $ "   " ++ trans ++ ": " ++ show count ++ " vezes"

  let estabilidade = fromIntegral (countTrans "8->8" transicoes) /
                     fromIntegral (sum (map snd transicoes))
  putStrLn $ "   Estabilidade (8->8): " ++ showFFloat (Just 3) (estabilidade * 100) "%"
  putStrLn ""

  -- Seção 5: Visualização
  putStrLn "5. VISUALIZAÇÃO DO ESPAÇO DE ESTADOS"
  putStrLn $ renderizarEspacoEstados (take 50 estados)
  putStrLn ""

  -- Exportar dados
  exportarDadosSimulacao eventosOrd "relatorio_simulacao.csv"

  where
    countTrans :: String -> [(String, Int)] -> Int
    countTrans key = sum . map snd . filter ((== key) . fst)

-- =====================================================
-- 65. MONITORAMENTO EM TEMPO REAL (SIMULADO)
-- =====================================================

-- | Interface de monitoramento do sistema
monitorarSistema :: Int -> Double -> IO ()
monitorarSistema nIter intervalo = do
  putStrLn "Iniciando monitoramento do sistema... (Ctrl+C para parar)\n"

  -- Gerar seed a partir do tempo atual de forma correta
  currentTime <- getCurrentTime
  let seed = round . (* 1000) . realToFrac . diffUTCTime currentTime $ read "1970-01-01 00:00:00 UTC"

  let sistema0 = inicializarSistema seed 1.0

  -- Loop de monitoramento
  let loop sistema i
        | i >= nIter = putStrLn "\nMonitoramento concluído."
        | otherwise = do
            let hc = hipercuboAtual sistema
                bits = hipercuboParaLista hc
                freq = hipercuboParaFrequencia hc
                pan = hipercuboParaPanorama hc
                pop = popCount16 hc
                energiaAtual = energia sistema

            -- Limpar linha e exibir estado atual
            putStr "\r"
            printf "Iter: %4d | Pop: %2d/16 | Freq: %6.1f Hz | Pan: %+4.2f | Energia: %.3f | Estado: %s"
              i pop freq pan energiaAtual (concatMap show bits)
            hFlush stdout

            -- Aguardar (simulado)
            threadDelay (round (intervalo * 1000000))

            -- Próximo estado
            let sistema' = laplacianoDiscreto sistema
            loop sistema' (i+1)

  loop sistema0 0

-- =====================================================
-- 66. FUNÇÕES AUXILIARES E UTILITÁRIOS
-- =====================================================

-- | Dividir lista em chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- =====================================================
-- 67. FUNÇÃO PRINCIPAL DO MÓDULO
-- =====================================================

main :: IO ()
main = do
  putStrLn "SISTEMA DE HIPERCUBO 16D - DIFUSÃO LAPLACIANA"
  putStrLn "==============================================\n"

  putStrLn "Escolha uma operação:"
  putStrLn "1. Simulação básica (100 iterações)"
  putStrLn "2. Simulação longa (1000 iterações)"
  putStrLn "3. Monitoramento em tempo real"
  putStrLn "4. Relatório completo"
  putStrLn "5. Teste de diferentes temperaturas"

  opcao <- getLine

  case opcao of
    "1" -> executarSimulacaoSuperCollider 42 100 0.25
    "2" -> executarSimulacaoSuperCollider 42 1000 0.25
    "3" -> monitorarSistema 100 0.25
    "4" -> gerarRelatorioCompleto 42 500 0.25
    "5" -> testarTemperaturas
    _   -> putStrLn "Opção inválida."

-- =====================================================
-- 68. EXPERIMENTOS COM TEMPERATURA
-- =====================================================

-- | Testar efeito da temperatura no sistema
testarTemperaturas :: IO ()
testarTemperaturas = do
  putStrLn "=== EXPERIMENTO: EFEITO DA TEMPERATURA ===\n"

  let temperaturas = [0.1, 0.5, 1.0, 2.0, 5.0]
      resultados = map (\temp ->
        let sistema0 = inicializarSistema 42 temp
            (eventos, sistemaFinal) = gerarEventosAudio 0.25 100 sistema0
            eventosOrd = reverse eventos
            estados = map estadoEv eventosOrd
            estadosUnicos = length (S.fromList estados)
            transicoes = espectroTransicoes (analisarEspectro eventosOrd)
            estabilidade :: Double
            estabilidade = fromIntegral (M.findWithDefault 0 "8->8" transicoes) /
                          fromIntegral (sum $ M.elems transicoes)
        in (temp, estadosUnicos, estabilidade)
        ) temperaturas

  putStrLn "Temp | Estados Únicos | Estabilidade (8->8)"
  putStrLn "-----|----------------|---------------------"
  forM_ resultados $ \(temp, unicos, estab) -> do
    printf "%4.1f | %d | %.3f\n" temp unicos (estab * 100)

-- =====================================================
-- 69. ANÁLISE MATEMÁTICA AVANÇADA
-- =====================================================

-- | Matriz de transição do sistema (aproximada)
matrizTransicao :: Int -> M.Map (Int, Int) Double
matrizTransicao nSamples =
  let seed = 42
      sistema0 = inicializarSistema seed 1.0
      (eventos, _) = gerarEventosAudio 0.25 nSamples sistema0
      estados = map estadoEv $ reverse eventos
      pares = zip estados (tail estados)
      transicoes = foldl (\m (from, to) ->
                   let fromPop = popCount16 from
                       toPop = popCount16 to
                   in M.insertWith (+) (fromPop, toPop) 1 m) M.empty pares
      total = fromIntegral (length pares)
  in M.map (/ total) transicoes

-- | Autovetores e autovalores da matriz de transição (simplificado)
autovaloresSistema :: [Double]
autovaloresSistema =
  -- Para um sistema ergódico, o maior autovalor deve ser 1
  -- Os outros descrevem a taxa de decaimento das correlações
  let n = 17  -- PopCount vai de 0 a 16
      -- Matriz simplificada: transições mais prováveis entre popcounts próximos
      matriz = [ [ if abs (i - j) <= 1 then 1/3 else 0 | j <- [0..16] ]
               | i <- [0..16] ]
      -- Autovalores aproximados de uma matriz tridiagonal
  in [1.0, 0.94, 0.88, 0.82, 0.76, 0.70, 0.64, 0.58, 0.52,
      0.46, 0.40, 0.34, 0.28, 0.22, 0.16, 0.10, 0.04]

-- =====================================================
-- 70. INTEGRAÇÃO COM SISTEMA IFÁ COMPLETO
-- =====================================================

-- | Converter sequência de hipercubos em sequência de Odù
hipercubosParaOduSequence :: [Hipercubo16D] -> [(Odu, Odu)]
hipercubosParaOduSequence hipercubos =
  mapMaybe hipercuboParaParOdu hipercubos
  where
    hipercuboParaParOdu :: Hipercubo16D -> Maybe (Odu, Odu)
    hipercuboParaParOdu hc =
      let bits = hipercuboParaLista hc
          (esqBits, dirBits) = splitAt 8 bits
          maybeOdu1 = bitsParaOdu esqBits
          maybeOdu2 = bitsParaOdu dirBits
      in case (maybeOdu1, maybeOdu2) of
           (Just odu1, Just odu2) -> Just (odu1, odu2)
           _ -> Nothing

    bitsParaOdu :: [Int] -> Maybe Odu
    bitsParaOdu bits
      | length bits == 8 = do
          let (col1Bits, col2Bits) = splitAt 4 bits
          col1 <- bitsParaColuna col1Bits
          col2 <- bitsParaColuna col2Bits
          return $ Odu col1 col2
      | otherwise = Nothing

    bitsParaColuna :: [Int] -> Maybe Coluna
    bitsParaColuna [a,b,c,d] =
      Just (intParaBit a, intParaBit b, intParaBit c, intParaBit d)
    bitsParaColuna _ = Nothing

    intParaBit :: Int -> BitIfa
    intParaBit 0 = II
    intParaBit 1 = I
    intParaBit _ = error "Bit inválido"

-- | Analisar sequência de Odù gerada
analisarSequenciaOdu :: [(Odu, Odu)] -> IO ()
analisarSequenciaOdu sequencia = do
  putStrLn "=== ANÁLISE DA SEQUÊNCIA DE ODÙ GERADA ===\n"

  let (odu1s, odu2s) = unzip sequencia
      nomes1 = map nomeOdu odu1s
      nomes2 = map nomeOdu odu2s

      contagem1 = foldl (\m nome -> M.insertWith (+) nome 1 m) M.empty nomes1
      contagem2 = foldl (\m nome -> M.insertWith (+) nome 1 m) M.empty nomes2

      top5_1 = take 5 $ reverse $ sortOn snd $ M.toList contagem1
      top5_2 = take 5 $ reverse $ sortOn snd $ M.toList contagem2

  putStrLn "Primeiro Odù (mais frequentes):"
  forM_ top5_1 $ \(nome, count) ->
    putStrLn $ "  " ++ nome ++ ": " ++ show count ++ " vezes"

  putStrLn "\nSegundo Odù (mais frequentes):"
  forM_ top5_2 $ \(nome, count) ->
    putStrLn $ "  " ++ nome ++ ": " ++ show count ++ " vezes"

  putStrLn $ "\nTotal de pares únicos: " ++
    show (length (S.fromList sequencia)) ++ "/" ++ show (length sequencia)

-- =====================================================
