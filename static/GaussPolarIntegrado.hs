{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (transpose, intercalate, sortBy)
import Data.Ord (comparing)
import System.Random (randomRIO, newStdGen, randomRs)
import Control.Monad (forM_, replicateM)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Text.Printf (printf)

-- ============================================================================
-- I. SISTEMA UNIFICADO DE CONVERSÃO
-- ============================================================================

newtype Graus = Graus Double deriving (Show, Eq, Ord)
newtype Radianos = Radianos Double deriving (Show, Eq, Ord)
newtype Decibeis = Decibeis Double deriving (Show, Eq, Ord)

-- | Constantes musicais fundamentais
a4Freq :: Double
a4Freq = 440.0

pi' :: Double
pi' = 3.141592653589793
tau :: Double
tau = 2.0 * pi'

-- Conversões
grausParaRad :: Graus -> Radianos
grausParaRad (Graus d) = Radianos (d * pi' / 180.0)

radParaGraus :: Radianos -> Graus
radParaGraus (Radianos r) = Graus (r * 180.0 / pi')

dbParaAmp :: Decibeis -> Double
dbParaAmp (Decibeis db) = 10.0 ** (db / 20.0)

ampParaDb :: Double -> Decibeis
ampParaDb a = Decibeis (20.0 * log10 a)
  where log10 x = log x / log 10

-- ============================================================================
-- II. GAUSSIANA MUSICAL: TEMPO-FREQUÊNCIA-AMPLITUDE
-- ============================================================================

-- | Gaussian 3D: (tempo, frequência, amplitude)
data GaussMusical = GaussMusical
    { centroTemporal   :: Double    -- μ_t (posição no tempo)
    , centroFrequencia :: Double    -- μ_f (frequência central em Hz)
    , centroAmplitude  :: Double    -- μ_a (amplitude central 0-1)
    , dispersaoTempo   :: Double    -- σ_t (largura temporal)
    , dispersaoFreq    :: Double    -- σ_f (largura espectral)
    , dispersaoAmp     :: Double    -- σ_a (variação de amplitude)
    } deriving (Show)

-- | Avalia a Gaussiana musical em um ponto (t, f)
avaliarGaussMusical :: GaussMusical -> Double -> Double -> Double
avaliarGaussMusical GaussMusical{..} t f =
    let exponenteTemporal = -0.5 * ((t - centroTemporal) / dispersaoTempo)**2
        exponenteFreq = -0.5 * ((log2 (f / centroFrequencia)) / dispersaoFreq)**2
        gaussTemporal = exp exponenteTemporal
        gaussFreq = exp exponenteFreq
    in centroAmplitude * gaussTemporal * gaussFreq
  where log2 x = log x / log 2

-- ============================================================================
-- III. MAPEAMENTO POLAR COMPLEXO
-- ============================================================================

data CoordenadaPolarComplexa = CPC
    { raio        :: Double      -- r: intensidade/amplitude
    , azimute     :: Radianos    -- θ: fase/posição angular
    , elevacao    :: Radianos    -- φ: elevação/espectro
    , faseComplexa :: Radianos   -- ψ: fase complexa adicional
    } deriving (Show)

-- | Converte coordenada polar complexa para representação matricial
polarParaMatricial :: CoordenadaPolarComplexa -> (Double, Double, Double, Double)
polarParaMatricial CPC{..} =
    let (Radianos theta) = azimute
        (Radianos phi) = elevacao
        (Radianos psi) = faseComplexa
        -- Conversão para coordenadas hiperesféricas
        x = raio * sin theta * cos phi * cos psi
        y = raio * sin theta * sin phi * cos psi
        z = raio * cos theta * cos psi
        w = raio * sin psi
    in (x, y, z, w)

-- | Gera campo Gaussiano polar
campoGaussianoPolar :: Double -> Double -> Graus -> Int -> [CoordenadaPolarComplexa]
campoGaussianoPolar mu sigma (Graus angulo) nPontos =
    let raioBase = 1.0
        (Radianos theta0) = grausParaRad (Graus angulo)
        pontos = [ fromIntegral i * 2.0 * pi' / fromIntegral nPontos | i <- [0..nPontos-1] ]

        gerarPonto :: Double -> CoordenadaPolarComplexa
        gerarPonto phi =
            let r = raioBase * gauss theta0 sigma phi
                theta = Radianos (theta0 + 0.1 * sin phi)  -- Modulação
                elev = Radianos (phi * 0.5)
                fase = Radianos (sin phi * pi')
            in CPC r theta elev fase

        gauss mu' sigma' x = exp (-0.5 * ((x - mu') / sigma')**2)
    in map gerarPonto pontos

-- ============================================================================
-- IV. INTERPRETAÇÃO DA NOTAÇÃO [,,]
-- ============================================================================

-- | Tipos de elementos na notação condensada
data ElementoNotacao
    = Vazio          -- , (silêncio/pausa)
    | Ativo Int      -- Número (1,2,3...)
    | Spark TipoSpark -- Spark especial
    deriving (Show, Eq)

data TipoSpark = SEletrico | STermico | SMecanico | SInformacional | STrancendente
    deriving (Show, Eq, Enum)

-- | Parser da notação [,,]
parseNotacao :: String -> [ElementoNotacao]
parseNotacao str =
    let chars = filter (\c -> c /= '[' && c /= ']' && c /= ' ') str
        parseChar c
            | c == ',' = Vazio
            | c `elem` ['1'..'9'] = Ativo (read [c])
            | c == 'E' = Spark SEletrico
            | c == 'T' = Spark STermico
            | c == 'M' = Spark SMecanico
            | c == 'I' = Spark SInformacional
            | c == 'X' = Spark STrancendente
            | otherwise = Vazio
    in map parseChar chars

-- | Expande notação condensada para timeline temporal
expandirTimeline :: [ElementoNotacao] -> Double -> [(Double, ElementoNotacao)]
expandirTimeline elementos duracaoUnitaria =
    let n = length elementos
        duracaoTotal = fromIntegral n * duracaoUnitaria
    in zip [0, duracaoUnitaria ..] elementos

-- ============================================================================
-- V. SÍNTESE SUPERCOLLIDER INTEGRADA
-- ============================================================================

-- | Evento de síntese pronto para SuperCollider
data EventoSintese = EventoSintese
    { tempoInicio   :: Double      -- Em segundos
    , duracao       :: Double      -- Em segundos
    , frequencia    :: Double      -- Em Hz
    , amplitude     :: Double      -- 0-1
    , tipoOnda      :: TipoOnda
    , modulacao     :: Modulacao
    , posicaoEstereo :: Double     -- -1 (esquerda) a 1 (direita)
    } deriving (Show)

data TipoOnda = Senoidal | Quadrada | Triangular | DenteSerra | Gaussiana
    deriving (Show, Eq, Enum)

data Modulacao = SemMod | FM Double Double | AM Double Double | RM Double Double
    deriving (Show)

-- | Converte elementos da notação em eventos de síntese
notacaoParaSintese :: [(Double, ElementoNotacao)] -> [EventoSintese]
notacaoParaSintese timeline =
    let converter (t, elemento) = case elemento of
            Vazio -> Nothing  -- Silêncio, sem evento
            Ativo n -> Just $ criarEventoOnda t n
            Spark tipo -> Just $ criarEventoSpark t tipo
    in catMaybes $ map converter timeline

criarEventoOnda :: Double -> Int -> EventoSintese
criarEventoOnda t n =
    let freqBase = a4Freq * (2.0 ** (fromIntegral n / 12.0))  -- Escala cromática
        dur = 0.5
        amp = 0.3 + 0.1 * sin (t * pi')
        tipo = case n `mod` 5 of
            0 -> Senoidal
            1 -> Quadrada
            2 -> Triangular
            3 -> DenteSerra
            _ -> Gaussiana
        modul = if n `mod` 3 == 0
                then FM (freqBase * 0.5) 0.1
                else SemMod
        pan = sin (t * 2.0)  -- Movimento estéreo
    in EventoSintese t dur freqBase amp tipo modul pan

criarEventoSpark :: Double -> TipoSpark -> EventoSintese
criarEventoSpark t tipo =
    let (freq, dur, amp, onda) = case tipo of
            SEletrico      -> (3000, 0.01, 0.8, DenteSerra)
            STermico       -> (200,  0.3,  0.5, Senoidal)
            SMecanico      -> (800,  0.05, 0.9, Quadrada)
            SInformacional -> (1200, 0.2,  0.4, Gaussiana)
            STrancendente  -> (880,  1.0,  0.6, Triangular)
        modul = RM (freq * 2.0) 0.3
        pan = cos (t * 3.0)
    in EventoSintese t dur freq amp onda modul pan

-- ============================================================================
-- VI. GERAÇÃO DE CÓDIGO SUPERCOLLIDER
-- ============================================================================

gerarSynthDef :: TipoOnda -> String
gerarSynthDef tipo = case tipo of
    Senoidal -> unlines
        [ "SynthDef(\\senoidal, { |out=0, freq=440, amp=0.5, gate=1, pan=0|"
        , "    var env = EnvGen.kr(Env.asr(0.01, 1, 0.1), gate, doneAction:2);"
        , "    var sig = SinOsc.ar(freq) * amp * env;"
        , "    Out.ar(out, Pan2.ar(sig, pan));"
        , "}).add;"
        ]

    Quadrada -> unlines
        [ "SynthDef(\\quadrada, { |out=0, freq=440, amp=0.5, gate=1, pan=0|"
        , "    var env = EnvGen.kr(Env.asr(0.01, 1, 0.1), gate, doneAction:2);"
        , "    var sig = Pulse.ar(freq, 0.5) * amp * env;"
        , "    Out.ar(out, Pan2.ar(sig, pan));"
        , "}).add;"
        ]

    Gaussiana -> unlines
        [ "SynthDef(\\gaussiana, { |out=0, freq=440, amp=0.5, gate=1, pan=0, sigma=0.1|"
        , "    var env = EnvGen.kr(Env.asr(0.01, 1, 0.1), gate, doneAction:2);"
        , "    var gauss = { |mu, sig, x| exp(-0.5 * ((x - mu) / sig).squared) };"
        , "    var phase = Phasor.ar(0, freq * SampleDur.ir, 0, 2*pi);"
        , "    var sig = gauss.(pi, sigma, phase) * amp * env;"
        , "    Out.ar(out, Pan2.ar(sig, pan));"
        , "}).add;"
        ]

    _ -> unlines
        [ "SynthDef(\\padrao, { |out=0, freq=440, amp=0.5, gate=1, pan=0|"
        , "    var env = EnvGen.kr(Env.asr(0.01, 1, 0.1), gate, doneAction:2);"
        , "    var sig = LFTri.ar(freq) * amp * env;"
        , "    Out.ar(out, Pan2.ar(sig, pan));"
        , "}).add;"
        ]

gerarPattern :: [EventoSintese] -> String
gerarPattern eventos =
    let eventosAgrupados = foldr agruparPorTipo M.empty eventos
        agruparPorTipo ev@EventoSintese{..} acc =
            let key = show tipoOnda
                lista = M.findWithDefault [] key acc
            in M.insert key (ev:lista) acc

        gerarPbind :: String -> [EventoSintese] -> String
        gerarPbind nome eventosTipo =
            let linhas = map (\ev ->
                    let EventoSintese{..} = ev
                    in printf "    [\\dur, %.3f, \\freq, %.2f, \\amp, %.3f, \\pan, %.2f],"
                         duracao frequencia amplitude posicaoEstereo
                  ) eventosTipo
                cabecalho = printf "Pbind(\\instrument, \\%s,\n    \\scale, Pseq([\n" nome
                rodape = "    ], inf)\n).play;\n"
            in cabecalho ++ unlines linhas ++ rodape

    in unlines $ M.elems $ M.mapWithKey gerarPbind eventosAgrupados

gerarCodigoSuperCollider :: [EventoSintese] -> String
gerarCodigoSuperCollider eventos =
    let synthDefs = nub $ map tipoOnda eventos
        synthDefsCode = unlines $ map gerarSynthDef synthDefs
        patternCode = gerarPattern eventos
        cabecalho = "// CÓDIGO SUPERCOLLIDER GERADO AUTOMATICAMENTE\n"
                 ++ "// Sistema Gauss-Polar Integrado\n"
                 ++ "// Axioma Mnemosynis 2026\n\n"
        s = "s.boot;\n\n"
    in cabecalho ++ s ++ synthDefsCode ++ "\n" ++ patternCode

-- ============================================================================
-- VII. SISTEMA PRINCIPAL INTEGRADO
-- ============================================================================

sistemaGaussPolarCompleto :: String -> Double -> Double -> Graus -> IO ()
sistemaGaussPolarCompleto notacao mu sigma angulo = do
    putStrLn "╔══════════════════════════════════════════════════╗"
    putStrLn "║    SISTEMA GAUSS-POLAR INTEGRADO v2.0           ║"
    putStrLn "║    Síntese Musical Algorítmica                  ║"
    putStrLn "╚══════════════════════════════════════════════════╝\n"

    -- 1. Parse da notação
    putStrLn "1. 📝 INTERPRETANDO NOTAÇÃO:"
    let elementos = parseNotacao notacao
    putStrLn $ "   Notação: " ++ notacao
    putStrLn $ "   Elementos: " ++ show elementos

    -- 2. Expansão temporal
    putStrLn "\n2. ⏱️  EXPANSÃO TEMPORAL:"
    let timeline = expandirTimeline elementos 0.25
    mapM_ (\(t, e) -> putStrLn $ printf "   t=%.2fs: %s" t (show e)) timeline

    -- 3. Campo Gaussiano Polar
    putStrLn $ "\n3. 🌀 CAMPO GAUSSIANO POLAR (μ=" ++ show mu ++ ", σ=" ++ show sigma ++ "):"
    let campo = campoGaussianoPolar mu sigma angulo 8
    forM_ (zip [0..] campo) $ \(i, cpc) -> do
        putStrLn $ printf "   Ponto %d: r=%.2f, θ=%.1f°, φ=%.1f°"
            i (raio cpc) (let Graus g = radParaGraus (azimute cpc) in g)
            (let Graus g = radParaGraus (elevacao cpc) in g)

    -- 4. Conversão para síntese
    putStrLn "\n4. 🎵 CONVERSÃO PARA EVENTOS DE SÍNTESE:"
    let eventos = notacaoParaSintese timeline
    forM_ (take 5 eventos) $ \ev ->
        putStrLn $ printf "   t=%.2fs: %s @ %.1fHz (amp=%.2f)"
            (tempoInicio ev) (show (tipoOnda ev)) (frequencia ev) (amplitude ev)

    -- 5. Geração do código SuperCollider
    putStrLn "\n5. 💻 CÓDIGO SUPERCOLLIDER GERADO:"
    let codigoSC = gerarCodigoSuperCollider eventos
    putStrLn "   (Trecho do código - ver arquivo completo)"
    putStrLn $ take 500 codigoSC ++ "...\n"

    -- 6. Salvar em arquivo
    writeFile "sintese_gauss_polar.scd" codigoSC
    putStrLn $ "6. 💾 Código salvo em: sintese_gauss_polar.scd"
    putStrLn "   Execute no SuperCollider com:"
    putStrLn "   s.boot;"
    putStrLn "   // Então execute o conteúdo do arquivo"

    putStrLn "\n✨ SISTEMA INTEGRADO PRONTO PARA SÍNTESE ✨"

-- ============================================================================
-- VIII. FUNÇÕES AUXILIARES
-- ============================================================================

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of Just y -> y:acc; Nothing -> acc) []

nub :: Eq a => [a] -> [a]
nub = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

-- ============================================================================
-- IX. EXEMPLOS E DEMONSTRAÇÃO
-- ============================================================================

exemploNotacao1 :: String
exemploNotacao1 = "[1,,2,E,3,,M,4,,X]"

exemploNotacao2 :: String
exemploNotacao2 = "[1,2,3,,4,5,E,T,M,,I,X]"

main :: IO ()
main = do
    putStrLn "\n🎼 SÍNTESE GAUSS-POLAR: NOTAÇÃO → SOM 🎼\n"

    putStrLn "EXEMPLO 1: Sequência simples com sparks"
    sistemaGaussPolarCompleto exemploNotacao1 pi' 0.5 (Graus 180)

    putStrLn ("\n" ++ replicate 60 '=' ++ "\n")

    putStrLn "EXEMPLO 2: Sequência complexa"
    sistemaGaussPolarCompleto exemploNotacao2 (pi'/2) 0.3 (Graus 90)

    putStrLn ("\n" ++ replicate 60 '=' ++ "\n")

    -- Demonstração adicional: Gaussian Musical
    putStrLn "DEMONSTRAÇÃO: Campo Gaussiano Musical 3D"
    let gauss = GaussMusical 0.5 440.0 0.8 0.2 0.5 0.1
    putStrLn $ "Gauss Musical: " ++ show gauss
    putStrLn "Avaliação em pontos (t, f):"
    let pontos = [(0.0, 220.0), (0.5, 440.0), (1.0, 880.0)]
    forM_ pontos $ \(t, f) ->
        putStrLn $ printf "  (%.1f, %.1fHz) → %.3f" t f (avaliarGaussMusical gauss t f)

    putStrLn "\n🎹 SISTEMA PRONTO PARA COMPOSIÇÃO ALGORÍTMICA 🎹"
