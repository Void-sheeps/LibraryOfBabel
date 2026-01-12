{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import System.Random (RandomGen, StdGen, random, randomR, split, mkStdGen)
import Control.Monad.State (State, evalState, get, put, modify, state)
import Control.Monad (replicateM, forM_, foldM)
import Data.List (intercalate, sort, group, transpose, tails)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import System.IO (writeFile, hFlush, stdout)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Data.Word (Word8, Word16)
import Data.Bits (xor, (.&.), (.|.), shiftL, shiftR, testBit, setBit, clearBit)
import Data.Char (intToDigit, digitToInt)
import Numeric (showFFloat, showHex)
import Data.Complex (Complex((:+)), magnitude, phase)
import Data.Fixed (mod')
import Text.Printf (printf)

import Odu

-- =====================================================
-- 71. CAMPO QUÂNTICO 16x16 - SISTEMA DE SUPOSIÇÃO
-- =====================================================

-- | Probabilidade de superposição: 0.0 = 100% aberto, 1.0 = 100% fechado
--   0.5 = superposição igual (50%/50%)
type Probabilidade = Double

-- | Grade quântica 16x16
newtype CampoQuantico = CampoQuantico (VB.Vector (VB.Vector Probabilidade))
  deriving (Show, Generic)

-- | Estado colapsado: 0 = aberto, 1 = fechado
type EstadoColapsado = Int

-- | Coordenada na grade 16x16
type Coordenada = (Int, Int)  -- (x, y) onde x,y ∈ [0, 15]

-- =====================================================
-- 72. INICIALIZAÇÃO DO CAMPO
-- =====================================================

-- | Campo inicial com superposição uniforme (0.5 em todas as células)
campoInicialUniforme :: CampoQuantico
campoInicialUniforme =
  let linha = VB.replicate 16 0.5
      grade = VB.replicate 16 linha
  in CampoQuantico grade

-- | Campo inicial com padrão de interferência (ondas senoidais)
campoInicialOndas :: CampoQuantico
campoInicialOndas =
  let grade = VB.generate 16 $ \x ->
        VB.generate 16 $ \y ->
          let fx = fromIntegral x / 15.0 * 2 * pi
              fy = fromIntegral y / 15.0 * 2 * pi
              onda1 = sin fx * 0.5 + 0.5
              onda2 = sin fy * 0.5 + 0.5
              interferencia = (onda1 + onda2) / 2.0
          in max 0.0 (min 1.0 interferencia)
  in CampoQuantico grade

-- | Campo inicial baseado em um padrão de Odù
campoInicialOdu :: Odu -> CampoQuantico
campoInicialOdu odu =
  let bits = concatMap colunaToList [direita odu, esquerda odu]
      prob (I)  = 0.8  -- Tendência para fechado
      prob (II) = 0.2  -- Tendência para aberto
      probs = map prob bits
      linha = VB.fromList $ take 16 (probs ++ repeat 0.5)
      grade = VB.replicate 16 linha
  in CampoQuantico grade

-- | Campo inicial com semente aleatória
campoInicialAleatorio :: RandomGen g => g -> CampoQuantico
campoInicialAleatorio gen =
  let (probs, _) = foldl (\(acc, g) _ ->
                    let (p, g') = randomR (0.0, 1.0) g
                    in (p:acc, g')) ([], gen) [1..256]
      grade = VB.fromList $ map VB.fromList $ chunksOf 16 (reverse probs)
  in CampoQuantico grade

-- =====================================================
-- 73. FUNÇÃO DE COLAPSO QUÂNTICO
-- =====================================================

-- | Colapsar uma célula específica da grade
colapsarCelula :: RandomGen g => CampoQuantico -> Coordenada -> g -> (EstadoColapsado, g)
colapsarCelula (CampoQuantico grade) (x, y) gen =
  let prob = (grade VB.! x) VB.! y
      (r, gen') = randomR (0.0, 1.0) gen
      estado = if r < prob then 1 else 0
  in (estado, gen')

-- | Colapsar múltiplas células simultaneamente (emaranhamento)
colapsarMultiplas :: RandomGen g => CampoQuantico -> [Coordenada] -> g -> ([EstadoColapsado], g)
colapsarMultiplas campo coords gen =
  foldl (\(estados, g) coord ->
    let (estado, g') = colapsarCelula campo coord g
    in (estado:estados, g')) ([], gen) coords

-- | Função de colapso com decoerência (probabilidade diminui com tempo)
colapsarComDecoerencia :: RandomGen g => CampoQuantico -> Coordenada -> Double -> g -> (EstadoColapsado, Double, g)
colapsarComDecoerencia (CampoQuantico grade) (x, y) coherencia gen =
  let probOriginal = (grade VB.! x) VB.! y
      -- Quanto menor a coerência, mais próxima de 0.5 (aleatório)
      probAjustada = probOriginal * coherencia + 0.5 * (1.0 - coherencia)
      (r, gen') = randomR (0.0, 1.0) gen
      estado = if r < probAjustada then 1 else 0
      -- Decoerência aumenta após o colapso
      coherencia' = coherencia * 0.9
  in (estado, coherencia', gen')

-- =====================================================
-- 74. MAPEAMENTO PARA PARÂMETROS SONOROS
-- =====================================================

-- | Frequência base para cada linha x
freqBase :: Int -> Double
freqBase x = 100.0 + fromIntegral x * 50.0  -- 100Hz a 850Hz

-- | Converter célula colapsada para frequência
celulaParaFrequencia :: EstadoColapsado -> Int -> Int -> Double
celulaParaFrequencia estado x y =
  let base = freqBase x
      -- Estado 1 (fechado) aumenta a frequência em 50%
      multiplicador = 1.0 + fromIntegral estado * 0.5
  in base * multiplicador

-- | Converter coordenada x para panorama (-1 a 1)
coordenadaParaPan :: Int -> Double
coordenadaParaPan x = (fromIntegral x / 15.0) * 2.0 - 1.0

-- | Converter coordenada y para amplitude (0.02 a 0.1)
coordenadaParaAmplitude :: Int -> Double
coordenadaParaAmplitude y = 0.02 + (fromIntegral y / 15.0) * 0.08

-- | Adicionar modulação baseada no tempo de colapso
modulacaoTemporal :: Double -> Double -> Double
modulacaoTemporal freq tempo =
  let modFreq = freq * (1.0 + 0.1 * sin (tempo * 2 * pi * 3.0))
  in modFreq

-- =====================================================
-- 75. EVENTO DE COLAPSO QUÂNTICO
-- =====================================================

data EventoQuantico = EventoQuantico
  { tempoColapso   :: Double           -- Tempo em segundos
  , coordenadaEv   :: Coordenada       -- (x, y) da célula
  , estadoEvQ      :: EstadoColapsado  -- 0 ou 1
  , frequenciaEvQ  :: Double           -- Hz
  , amplitudeEvQ   :: Double           -- 0-1
  , panoramaEvQ    :: Double           -- -1 a 1
  , coerenciaEvQ   :: Double           -- Nível de coerência (0-1)
  , faseEvQ        :: Double           -- Fase quântica (0-2π)
  } deriving (Show, Eq, Generic)

-- | Gerar evento a partir de um colapso
gerarEventoQuantico :: Double -> Coordenada -> EstadoColapsado -> Double -> EventoQuantico
gerarEventoQuantico tempo (x, y) estado coherencia =
  let freqBase = celulaParaFrequencia estado x y
      freqMod = modulacaoTemporal freqBase tempo
      amplitude = coordenadaParaAmplitude y
      panorama = coordenadaParaPan x
      -- Fase baseada na posição e tempo
      fase = (fromIntegral x * fromIntegral y / 225.0 * 2 * pi + tempo) `mod'` (2 * pi)
  in EventoQuantico
    { tempoColapso = tempo
    , coordenadaEv = (x, y)
    , estadoEvQ = estado
    , frequenciaEvQ = freqMod
    , amplitudeEvQ = amplitude
    , panoramaEvQ = panorama
    , coerenciaEvQ = coherencia
    , faseEvQ = fase
    }

-- =====================================================
-- 76. SINTETIZADOR QUÂNTICO CONCEITUAL
-- =====================================================

-- | Função de onda quântica (simplificada)
funcaoOnda :: Double -> Double -> Complex Double
funcaoOnda freq fase =
  let omega = 2 * pi * freq
  in cos (omega + fase) :+ sin (omega + fase)

-- | Sobreposição de ondas (princípio da superposição)
sobreposicaoOndas :: [EventoQuantico] -> Double -> Complex Double
sobreposicaoOndas eventos t =
  let contribs = map (\ev ->
        let freq = frequenciaEvQ ev
            fase = faseEvQ ev
            amp = amplitudeEvQ ev
            coef = if estadoEvQ ev == 1 then 1.0 :+ 0.0 else 0.0 :+ 1.0
        in (amp :+ 0.0) * coef * funcaoOnda freq (fase + t * 2 * pi * freq))
        eventos
  in sum contribs

-- =====================================================
-- 77. SIMULAÇÃO DE CAMPO QUÂNTICO
-- =====================================================

data SimulacaoQuantica = SimulacaoQuantica
  { campoAtual     :: CampoQuantico
  , tempoSim       :: Double           -- Tempo de simulação (s)
  , intervaloSim   :: Double           -- Intervalo entre colapsos (s)
  , coherenciaGeral :: Double          -- Coerência global (0-1)
  , geradorSim     :: StdGen
  , historicoColapsos :: Seq.Seq EventoQuantico
  , entropiaCampo  :: Double           -- Entropia do campo
  } deriving (Show, Generic)

-- | Inicializar simulação quântica
inicializarSimulacao :: Double -> Double -> Int -> SimulacaoQuantica
inicializarSimulacao intervalo coherencia seed =
  let gen = mkStdGen seed
      campo = campoInicialUniforme
      entropia = calcularEntropiaCampo campo
  in SimulacaoQuantica
    { campoAtual = campo
    , tempoSim = 0.0
    , intervaloSim = intervalo
    , coherenciaGeral = coherencia
    , geradorSim = gen
    , historicoColapsos = Seq.empty
    , entropiaCampo = entropia
    }

-- | Executar um passo de colapso
passoColapso :: SimulacaoQuantica -> SimulacaoQuantica
passoColapso sim =
  let gen = geradorSim sim
      -- Escolher célula aleatória
      (x, gen1) = randomR (0, 15) gen
      (y, gen2) = randomR (0, 15) gen1

      -- Colapsar com decoerência
      (estado, coherencia', gen3) =
        colapsarComDecoerencia (campoAtual sim) (x, y) (coherenciaGeral sim) gen2

      -- Criar evento
      evento = gerarEventoQuantico (tempoSim sim) (x, y) estado coherencia'

      -- Atualizar campo (opcional: ajustar probabilidades vizinhas)
      campo' = atualizarVizinhos (campoAtual sim) (x, y) estado

      -- Calcular nova entropia
      entropia' = calcularEntropiaCampo campo'

      -- Atualizar histórico (mantém últimos 1000)
      historico' = Seq.take 1000 $ evento Seq.<| historicoColapsos sim

  in sim
    { campoAtual = campo'
    , tempoSim = tempoSim sim + intervaloSim sim
    , coherenciaGeral = coherencia'
    , geradorSim = gen3
    , historicoColapsos = historico'
    , entropiaCampo = entropia'
    }

-- | Atualizar probabilidades das células vizinhas após colapso
atualizarVizinhos :: CampoQuantico -> Coordenada -> EstadoColapsado -> CampoQuantico
atualizarVizinhos (CampoQuantico grade) (x, y) estado =
  let fatorInfluencia = 0.1  -- Quanto o colapso afeta vizinhos
      novoEstadoProb = if estado == 1 then 0.8 else 0.2

      atualizarCelula :: Int -> Int -> Probabilidade -> Probabilidade
      atualizarCelula cx cy probAtual
        | cx == x && cy == y = novoEstadoProb  -- Célula colapsada
        | abs (cx - x) <= 1 && abs (cy - y) <= 1 =  -- Vizinhos imediatos
            let distancia = sqrt (fromIntegral ((cx - x)^2 + (cy - y)^2))
                influencia = fatorInfluencia / (distancia + 1.0)
                ajuste = if estado == 1 then influencia else -influencia
            in max 0.0 (min 1.0 (probAtual + ajuste))
        | otherwise = probAtual  -- Células distantes não afetadas

      grade' = VB.imap (\i linha ->
                VB.imap (\j prob -> atualizarCelula i j prob) linha) grade

  in CampoQuantico grade'

-- =====================================================
-- 78. CÁLCULO DE ENTROPIA E INFORMAÇÃO
-- =====================================================

-- | Calcular entropia de Shannon do campo
calcularEntropiaCampo :: CampoQuantico -> Double
calcularEntropiaCampo (CampoQuantico grade) =
  let probs = concatMap VB.toList (VB.toList grade)
      entropiaCelula p =
        if p <= 0.0 || p >= 1.0
        then 0.0  -- Estado determinístico
        else - (p * logBase 2 p + (1 - p) * logBase 2 (1 - p))
      entropias = map entropiaCelula probs
  in sum entropias / fromIntegral (length entropias)  -- Entropia média por célula

-- | Calcular informação mútua entre regiões do campo
informacaoMutuaCampo :: CampoQuantico -> Double
informacaoMutuaCampo campo =
  let -- Dividir campo em 4 quadrantes
      entropiaTotal = calcularEntropiaCampo campo
      -- Entropia conjunta simplificada (aproximação)
      entropiaConjunta = entropiaTotal * 0.75  -- Assumindo alguma correlação
  in max 0.0 (2 * entropiaTotal - entropiaConjunta)

-- =====================================================
-- 79. VISUALIZAÇÃO DO CAMPO QUÂNTICO
-- =====================================================

-- | Renderizar campo como arte ASCII
renderizarCampoASCII :: CampoQuantico -> String
renderizarCampoASCII (CampoQuantico grade) =
  let simbolos = " .-+*#%@"
      nivelParaSimbolo p =
        let idx = floor (p * fromIntegral (length simbolos - 1))
        in simbolos !! idx

      linhas = VB.toList $ VB.map (\linha ->
        VB.toList $ VB.map nivelParaSimbolo linha) grade

      bordaSuperior = "┌" ++ replicate 16 '─' ++ "┐"
      bordaInferior = "└" ++ replicate 16 '─' ++ "┘"

      linhasComBorda = map (\linha -> "│" ++ linha ++ "│") linhas

  in unlines $ bordaSuperior : linhasComBorda ++ [bordaInferior]

-- | Gerar representação em cores (códigos ANSI)
renderizarCampoColorido :: CampoQuantico -> String
renderizarCampoColorido (CampoQuantico grade) =
  let codigoCor p =
        let nivel = floor (p * 255) :: Int
            r = nivel
            g = 255 - nivel
            b = 128 :: Int
        in printf "\ESC[38;2;%d;%d;%dm\x2588\x1b[0m" r g b

      linhas = VB.toList $ VB.map (\linha ->
        VB.toList $ VB.map codigoCor linha) grade

  in unlines $ map concat linhas

-- =====================================================
-- 80. SIMULAÇÃO COMPLETA DO SISTEMA QUÂNTICO
-- =====================================================

-- | Executar simulação por n passos
executarSimulacaoQuantica :: Int -> Double -> Double -> Int -> IO ()
executarSimulacaoQuantica nPassos intervalo coherencia seed = do
  putStrLn "=== SIMULAÇÃO DE CAMPO QUÂNTICO 16x16 ===\n"

  let sim0 = inicializarSimulacao intervalo coherencia seed
      sims = take nPassos $ iterate passoColapso sim0
      simFinal = last sims
      eventos = Seq.take 10 $ historicoColapsos simFinal

  putStrLn "Configuração:"
  putStrLn $ "  Passos: " ++ show nPassos
  putStrLn $ "  Intervalo: " ++ show intervalo ++ "s"
  putStrLn $ "  Coerência inicial: " ++ show coherencia
  putStrLn $ "  Seed: " ++ show seed
  putStrLn ""

  putStrLn "Campo inicial (ASCII):"
  putStrLn $ renderizarCampoASCII (campoAtual sim0)
  putStrLn ""

  putStrLn "Campo final (ASCII):"
  putStrLn $ renderizarCampoASCII (campoAtual simFinal)
  putStrLn ""

  putStrLn "Estatísticas:"
  putStrLn $ "  Entropia inicial: " ++ showFFloat (Just 4) (entropiaCampo sim0) ""
  putStrLn $ "  Entropia final:   " ++ showFFloat (Just 4) (entropiaCampo simFinal) ""
  putStrLn $ "  Coerência final:  " ++ showFFloat (Just 4) (coherenciaGeral simFinal) ""
  putStrLn $ "  Tempo total:      " ++ showFFloat (Just 2) (tempoSim simFinal) "s"
  putStrLn ""

  putStrLn "Últimos 5 colapsos:"
  putStrLn "Tempo | Posição | Estado | Freq (Hz) | Coerência"
  putStrLn "-------------------------------------------------"
  forM_ (take 5 $ toList eventos) $ \ev -> do
    let (x, y) = coordenadaEv ev
    printf "%5.2f | (%2d,%2d) | %6s | %8.1f | %7.3f\n"
      (tempoColapso ev) x y
      (if estadoEvQ ev == 1 then "FECHADO" else "ABERTO")
      (frequenciaEvQ ev) (coerenciaEvQ ev)

  -- Exportar dados
  exportarDadosQuanticos (toList $ historicoColapsos simFinal) "campo_quantico.csv"

-- =====================================================
-- 81. ANÁLISE ESPECTRAL DO CAMPO
-- =====================================================

-- | Transformada de Fourier 2D do campo (simplificada)
fourier2D :: CampoQuantico -> [[Complex Double]]
fourier2D (CampoQuantico grade) =
  let matriz = VB.toList $ VB.map VB.toList grade
      n = length matriz

      -- DFT 1D (for Double)
      dft1D :: [Double] -> [Complex Double]
      dft1D xs =
        [ sum [ ((xs!!k) :+ 0.0) * cis (-2 * pi * fromIntegral (j * k) / fromIntegral n)
              | k <- [0..n-1] ]
        | j <- [0..n-1] ]

      -- DFT 1D (for Complex Double)
      dft1DComplex :: [Complex Double] -> [Complex Double]
      dft1DComplex xs =
        [ sum [ (xs!!k) * cis (-2 * pi * fromIntegral (j * k) / fromIntegral n)
              | k <- [0..n-1] ]
        | j <- [0..n-1] ]

      -- Aplicar DFT às linhas
      linhasDFT = map dft1D matriz

      -- Aplicar DFT às colunas (transpor, transformar, transpor de volta)
      colunas = transpose linhasDFT
      colunasDFT = map dft1DComplex colunas

  in transpose colunasDFT

-- | Espectro de potência do campo
espectroPotencia :: CampoQuantico -> [[Double]]
espectroPotencia campo =
  let coeficientes = fourier2D campo
  in map (map (\c -> magnitude c ** 2)) coeficientes

-- =====================================================
-- 82. EXPERIMENTOS QUÂNTICOS AVANÇADOS
-- =====================================================

-- | Experimento de dupla fenda (interferência)
experimentoDuplaFenda :: IO ()
experimentoDuplaFenda = do
  putStrLn "=== EXPERIMENTO DE DUPLA FENDA QUÂNTICA ===\n"

  -- Criar campo com duas "fendas" (regiões de alta probabilidade)
  let grade = VB.generate 16 $ \x ->
        VB.generate 16 $ \y ->
          if (x == 7 || x == 8) && (y >= 6 && y <= 9)
          then 0.9  -- Fendas
          else 0.1  -- Parede

  let campo = CampoQuantico grade
      espectro = espectroPotencia campo

  putStrLn "Padrão de interferência (espectro de potência):"
  let maxVal = 1
      escala = 10.0 / maxVal  -- Normalizar para 0-10

      linhaParaASCII vals =
        map (\v -> let nivel = floor (v * escala)
                   in if nivel >= 10 then '█'
                      else if nivel >= 8 then '▓'
                      else if nivel >= 6 then '▒'
                      else if nivel >= 4 then '░'
                      else ' ') vals

  forM_ (map linhaParaASCII espectro) $ \linha ->
    putStrLn $ "│" ++ linha ++ "│"

-- | Experimento de emaranhamento quântico
experimentoEmaranhamento :: Int -> IO ()
experimentoEmaranhamento seed = do
  putStrLn "=== EXPERIMENTO DE EMARANHAMENTO QUÂNTICO ===\n"

  let gen = mkStdGen seed
      campo = campoInicialUniforme

      -- Escolher duas células distantes
      celulaA = (2, 2)
      celulaB = (13, 13)

      -- Colapsar A
      (estadoA, gen1) = colapsarCelula campo celulaA gen

      -- Se A colapsou para 1, B tem alta probabilidade de 0 (anti-correlação)
      campoModificado = if estadoA == 1
                           then ajustarProbabilidade campo celulaB 0.1
                           else ajustarProbabilidade campo celulaB 0.9

      -- Colapsar B
      (estadoB, _) = colapsarCelula campoModificado celulaB gen1
      correlacao :: Double
      correlacao = if estadoA == estadoB then 1.0 else -1.0

  putStrLn $ "Célula A " ++ show celulaA ++ " colapsou para: " ++
             (if estadoA == 1 then "FECHADO" else "ABERTO")
  putStrLn $ "Célula B " ++ show celulaB ++ " colapsou para: " ++
             (if estadoB == 1 then "FECHADO" else "ABERTO")
  printf "Correlação: %.3f\n" (correlacao :: Double)
  putStrLn $ "  (1.0 = mesma direção, -1.0 = direções opostas)"

-- =====================================================
-- 83. EXPORTAÇÃO DE DADOS
-- =====================================================

exportarDadosQuanticos :: [EventoQuantico] -> FilePath -> IO ()
exportarDadosQuanticos eventos arquivo = do
  let cabecalho = "tempo,x,y,estado,frequencia,amplitude,panorama,coerencia,fase"
      linhas = map eventoParaCSV eventos
      conteudo = unlines (cabecalho : linhas)
  writeFile arquivo conteudo
  putStrLn $ "Dados exportados para: " ++ arquivo
  where
    eventoParaCSV :: EventoQuantico -> String
    eventoParaCSV ev =
      let (x, y) = coordenadaEv ev
      in intercalate "," $
         [ showFFloat (Just 4) (tempoColapso ev) ""
         , show x
         , show y
         , show (estadoEvQ ev)
         , showFFloat (Just 2) (frequenciaEvQ ev) ""
         , showFFloat (Just 4) (amplitudeEvQ ev) ""
         , showFFloat (Just 4) (panoramaEvQ ev) ""
         , showFFloat (Just 4) (coerenciaEvQ ev) ""
         , showFFloat (Just 4) (faseEvQ ev) ""
         ]

-- =====================================================
-- 84. FUNÇÕES AUXILIARES
-- =====================================================

-- | Ajustar probabilidade de uma célula específica
ajustarProbabilidade :: CampoQuantico -> Coordenada -> Probabilidade -> CampoQuantico
ajustarProbabilidade (CampoQuantico grade) (x, y) novaProb =
  let linha = grade VB.! x
      linha' = linha VB.// [(y, novaProb)]
      grade' = grade VB.// [(x, linha')]
  in CampoQuantico grade'

-- | Chunks de lista
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Função cosseno complexo (cis)
cis :: Double -> Complex Double
cis theta = cos theta :+ sin theta

-- =====================================================
-- 85. INTERFACE PRINCIPAL
-- =====================================================

main :: IO ()
main = do
  putStrLn "SISTEMA DE CAMPO QUÂNTICO 16x16 - COLAPSOS QUÂNTICOS"
  putStrLn "===================================================\n"

  putStrLn "Escolha um experimento:"
  putStrLn "1. Simulação básica (100 colapsos)"
  putStrLn "2. Simulação longa (1000 colapsos)"
  putStrLn "3. Experimento de dupla fenda"
  putStrLn "4. Experimento de emaranhamento"
  putStrLn "5. Visualizar campo inicial"
  putStrLn "6. Simulação com decoerência acelerada"

  opcao <- getLine
  let seed = 42

  case opcao of
    "1" -> executarSimulacaoQuantica 100 0.05 0.9 seed
    "2" -> executarSimulacaoQuantica 1000 0.05 0.9 seed
    "3" -> experimentoDuplaFenda
    "4" -> experimentoEmaranhamento seed
    "5" -> do
      putStrLn "Campo inicial uniforme:"
      putStrLn $ renderizarCampoASCII campoInicialUniforme
      putStrLn "\nCampo inicial com ondas:"
      putStrLn $ renderizarCampoASCII campoInicialOndas
    "6" -> executarSimulacaoQuantica 200 0.05 0.5 seed
    _   -> putStrLn "Opção inválida"

-- =====================================================
-- 86. INTEGRAÇÃO COM SISTEMA IFÁ
-- =====================================================

-- | Converter padrão de colapsos em Odù
colapsosParaOdu :: [EventoQuantico] -> [Odu]
colapsosParaOdu eventos =
  let -- Agrupar eventos por intervalos de tempo (8 eventos por Odù)
      grupos = chunksOf 8 eventos

      -- Converter 8 estados (0/1) em um Odù
      grupoParaOdu :: [EventoQuantico] -> Maybe Odu
      grupoParaOdu evs
        | length evs == 8 = do
            let bits = map estadoEvQ evs
                (esqBits, dirBits) = splitAt 4 bits
                colunaEsq = bitsParaColuna esqBits
                colunaDir = bitsParaColuna dirBits
            return $ Odu colunaEsq colunaDir
        | otherwise = Nothing

      bitsParaColuna :: [Int] -> Coluna
      bitsParaColuna [a,b,c,d] =
        (intParaBit a, intParaBit b, intParaBit c, intParaBit d)
      bitsParaColuna _ = error "Lista deve ter 4 elementos"

      intParaBit 0 = II
      intParaBit 1 = I
      intParaBit _ = error "Bit inválido"

  in catMaybes $ map grupoParaOdu grupos

-- | Análise quântica de uma sequência de Odù
analiseQuanticaOdu :: [Odu] -> IO ()
analiseQuanticaOdu odus = do
  putStrLn "=== ANÁLISE QUÂNTICA DE SEQUÊNCIA DE ODÙ ===\n"

  let -- Converter Odù para padrão de probabilidades
      padroes = map oduParaPadrao odus
      oduParaPadrao odu =
        let bits = concatMap colunaToList [direita odu, esquerda odu]
        in map (\bit -> case bit of
                          I  -> 0.8  -- Tendência fechado
                          II -> 0.2) bits  -- Tendência aberto

      -- Calcular entropia média
      entropias = map (calcularEntropiaCampo . listaParaCampo) padroes
      entropiaMedia = sum entropias / fromIntegral (length entropias)

      listaParaCampo :: [Probabilidade] -> CampoQuantico
      listaParaCampo probs =
        let paddedProbs = take 256 (probs ++ repeat 0.5)
            grade = VB.fromList $ map VB.fromList $ chunksOf 16 paddedProbs
        in CampoQuantico grade

  putStrLn $ "Número de Odù: " ++ show (length odus)
  putStrLn $ "Entropia média: " ++ showFFloat (Just 4) entropiaMedia " bits/célula"
  putStrLn $ "Entropia máxima possível: 1.0 bit/célula"

  -- Mostrar padrão do primeiro Odù
  case odus of
    (odu:_) -> do
      let campo = listaParaCampo $ oduParaPadrao odu
      putStrLn $ renderizarCampoASCII campo
    _ -> return ()

-- | Funções auxiliares para Odu
direita :: Odu -> Coluna
direita (Odu _ d) = d

esquerda :: Odu -> Coluna
esquerda (Odu e _) = e

colunaToList :: Coluna -> [BitIfa]
colunaToList (a,b,c,d) = [a,b,c,d]
