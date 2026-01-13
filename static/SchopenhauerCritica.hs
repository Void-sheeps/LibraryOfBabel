{-# LANGUAGE GADTs, ExistentialQuantification, TypeFamilies #-}

module Main where

import Control.Monad.State
import System.Random (randomRIO)
import Control.Monad (forM_, when)

-- ========================
-- SCHOPENHAUER: O MUNDO COMO VONTADE E REPRESENTAÇÃO
-- ========================

-- Para Schopenhauer, a Vontade é a coisa-em-si, uma força cega e insaciável
data VontadeSchopenhauer = VontadeSchopenhauer
    { essencia :: String
    , intensidade :: Double
    , objetoRepresentacao :: Maybe String  -- Objeto na representação
    }

-- O Sofrimento é inerente à Vontade (princípio de insaciabilidade)
data SofrimentoMetafisico = SofrimentoMetafisico
    { dorExistencial :: Double      -- 0.0 a 1.0
    , causa :: String
    , cicloKarmico :: Int           -- Repetição da Vontade
    } deriving (Show)

-- A Compaixão (Mitleid) como única saída ética
data Compaixao = Compaixao
    { nivelIdentificacao :: Double  -- Quanto se identifica com o outro
    , acaoAlivio :: Bool
    } deriving (Show)

-- ========================
-- DOSTOIÉVSKI: CRIME E CASTIGO ANALISADO
-- ========================

-- Raskólnikov como caso de estudo da Vontade afirmativa
data EstadoRaskolnikov = EstadoRaskolnikov
    { teoriaHomemExtraordinario :: Bool
    , sofreguidaoVontade :: Double
    , compaixaoResidual :: Double
    , crimesCometidos :: [String]
    , sofrimentoAcumulado :: Double
    } deriving (Show)

-- O crime como afirmação máxima da Vontade individual
cometerCrime :: String -> Double -> State EstadoRaskolnikov SofrimentoMetafisico
cometerCrime motivo intensidade = do
    estado <- get
    let novoCrime = "Crime: " ++ motivo
        aumentoSofrimento = intensidade * 0.7  -- A Vontade afirmada gera sofrimento
        novaCompaixao = max 0.0 (compaixaoResidual estado - 0.3)

    put $ estado
        { crimesCometidos = novoCrime : crimesCometidos estado
        , sofreguidaoVontade = sofreguidaoVontade estado + intensidade
        , compaixaoResidual = novaCompaixao
        , sofrimentoAcumulado = sofrimentoAcumulado estado + aumentoSofrimento
        }

    return $ SofrimentoMetafisico
        { dorExistencial = aumentoSofrimento
        , causa = "Afirmação da Vontade: " ++ motivo
        , cicloKarmico = length (crimesCometidos estado) + 1
        }

-- A compaixão como negação da Vontade (Schopenhauer)
experimentarCompaixao :: Compaixao -> State EstadoRaskolnikov Double
experimentarCompaixao (Compaixao identificacao alivia) = do
    estado <- get
    let alivio = if alivia
                    then 0.4 * identificacao  -- A compaixão alivia o sofrimento
                    else 0.0
        novaCompaixao = min 1.0 (compaixaoResidual estado + identificacao * 0.5)
        reducaoVontade = sofreguidaoVontade estado * (1 - identificacao * 0.3)

    put $ estado
        { compaixaoResidual = novaCompaixao
        , sofreguidaoVontade = reducaoVontade
        , sofrimentoAcumulado = max 0.0 (sofrimentoAcumulado estado - alivio)
        }

    return alivio

-- ========================
-- ANÁLISE SCHOPENHAUERIANA DO CRIME
-- ========================

-- Schopenhauer: "Toda satisfação é apenas negativa"
analisarCrimeSchopenhauer :: String -> Double -> IO ()
analisarCrimeSchopenhauer motivo intensidade = do
    putStrLn $ "\n🔍 ANÁLISE SCHOPENHAUERIANA DO CRIME:"
    putStrLn $ "   Motivo: " ++ motivo
    putStrLn $ "   Intensidade da Vontade: " ++ show intensidade

    -- Princípio da insaciabilidade
    let insaciabilidade = intensidade * 0.9
    putStrLn $ "   Princípio de Insaciabilidade: " ++ show insaciabilidade ++
               " (a Vontade nunca satisfeita)"

    -- Sofrimento metafísico inerente
    let sofrimentoInerente = 1.0 - (1.0 / (intensidade + 0.1))
    putStrLn $ "   Sofrimento Metafísico Inerente: " ++ show sofrimentoInerente

    -- Crítica à teoria do "homem extraordinário"
    putStrLn "\n   🎯 CRÍTICA À TEORIA DE RASKÓLNIKOV:"
    putStrLn "   'O homem extraordinário é apenas a Vontade afirmando-se com mais força.'"
    putStrLn "   'Mas mesmo essa afirmação máxima não escapa ao sofrimento essencial.'"

    if intensidade > 0.8
        then putStrLn "   ⚠️  PERIGO: Vontade demasiado afirmativa leva à autodestruição."
        else putStrLn "   💡 A Vontade moderada ainda sofre, mas menos intensamente."

-- ========================
-- SIMULAÇÃO: CRIME E CASTIGO
-- ========================

simularCrimeCastigo :: IO ()
simularCrimeCastigo = do
    putStrLn "\n📚 SIMULAÇÃO: CRIME E CASTIGO (Dostoiévski)"
    putStrLn "   Através da lente de Schopenhauer\n"

    -- Estado inicial de Raskólnikov
    let estadoInicial = EstadoRaskolnikov
            { teoriaHomemExtraordinario = True
            , sofreguidaoVontade = 0.6
            , compaixaoResidual = 0.2
            , crimesCometidos = []
            , sofrimentoAcumulado = 0.3
            }

    putStrLn "=== FASE 1: A TEORIA DO HOMEM EXTRAORDINÁRIO ==="
    putStrLn "   Raskólnikov: 'Os homens extraordinários têm direito...'"

    -- Comete o crime
    let (sofrimento1, estadoPosCrime) =
            runState (cometerCrime "Assassinato da velha agiota" 0.9) estadoInicial

    putStrLn $ "   Crime cometido. Sofrimento gerado: " ++ show (dorExistencial sofrimento1)
    putStrLn $ "   Ciclo kármico: " ++ show (cicloKarmico sofrimento1)

    -- Análise schopenhaueriana
    analisarCrimeSchopenhauer "Provar superioridade" 0.9

    -- Fase 2: O Castigo Interior (Sofrimento da Consciência)
    putStrLn "\n=== FASE 2: O CASTIGO INTERIOR ==="
    putStrLn "   Schopenhauer: 'O sofrimento é a consequência imediata da Vontade afirmada.'"

    let (sofrimento2, estadoPosTormento) =
            runState (cometerCrime "Tormento da consciência" 0.7) estadoPosCrime

    putStrLn $ "   Sofrimento da consciência: " ++ show (dorExistencial sofrimento2)

    -- Fase 3: Encontro com Sônia (Compaixão)
    putStrLn "\n=== FASE 3: A COMPAIXÃO (Sônia) ==="
    putStrLn "   Schopenhauer: 'A compaixão é a única base da moralidade.'"

    let compaixaoSonia = Compaixao
            { nivelIdentificacao = 0.8  -- Alta identificação
            , acaoAlivio = True
            }

    (alivio, estadoFinal) <- flip runStateT estadoPosTormento $ do
        aliv <- state $ runState (experimentarCompaixao compaixaoSonia)
        liftIO $ putStrLn $ "   Compaixão experimentada. Alívio: " ++ show aliv
        return aliv

    -- Fase 4: Confissão e Redenção
    putStrLn "\n=== FASE 4: CONFISSÃO E NEGAÇÃO DA VONTADE ==="

    let compaixaoFinal = Compaixao
            { nivelIdentificacao = 0.9
            , acaoAlivio = True
            }

    (alivioFinal, estadoRedimido) <- flip runStateT estadoFinal $ do
        aliv <- state $ runState (experimentarCompaixao compaixaoFinal)
        liftIO $ putStrLn $ "   Confissão como negação da Vontade. Alívio final: " ++ show aliv
        return aliv

    -- Relatório Final
    putStrLn $ "\n" ++ replicate 50 '='
    putStrLn "📜 RELATÓRIO SCHOPENHAUERIANO FINAL"
    putStrLn $ "   Sofrimento Total Acumulado: " ++ show (sofrimentoAcumulado estadoRedimido)
    putStrLn $ "   Vontade Residual: " ++ show (sofreguidaoVontade estadoRedimido)
    putStrLn $ "   Compaixão Residual: " ++ show (compaixaoResidual estadoRedimido)
    putStrLn $ "   Crimes Cometidos: " ++ show (length $ crimesCometidos estadoRedimido)

    -- Diagnóstico
    case () of
        _ | compaixaoResidual estadoRedimido > 0.7 ->
            putStrLn "\n🕊️  DIAGNÓSTICO: VONTADE NEGADA (Via Compaixão)"
        _ | sofrimentoAcumulado estadoRedimido > 1.0 ->
            putStrLn "\n💀 DIAGNÓSTICO: VONTADE AUTODESTRUTIVA (Raskólnikov pré-Sônia)"
        _ ->
            putStrLn "\n😶 DIAGNÓSTICO: VONTADE EM CONFLITO (Dialética interior)"

-- ========================
-- CRÍTICA SCHOPENHAUER A NIETZSCHE E DOSTOIÉVSKI
-- ========================

criticaSchopenhauer :: IO ()
criticaSchopenhauer = do
    putStrLn "\n🎭 CRÍTICA SCHOPENHAUERIANA:"
    putStrLn "   A Vontade como fonte de todo sofrimento\n"

    putStrLn "1. SOBRE NIETZSCHE E A VONTADE DE POTÊNCIA:"
    putStrLn "   'Nietzsche erra ao glorificar a Vontade.'"
    putStrLn "   'A Vontade de potência é apenas a Vontade afirmando-se cegamente.'"
    putStrLn "   'Isso leva não ao Super-Homem, mas ao sofrimento maximizado.'\n"

    putStrLn "2. SOBRE DOSTOIÉVSKI E RASKÓLNIKOV:"
    putStrLn "   'Raskólnikov é o exemplo perfeito da Vontade iludida.'"
    putStrLn "   'Ele acredita que pode transcender a moral, mas apenas afirmou a Vontade.'"
    putStrLn "   'Seu sofrimento não é castigo divino, mas consequência metafísica.'\n"

    putStrLn "3. SOBRE A SOLUÇÃO:"
    putStrLn "   'A única saída é a NEGAÇÃO da Vontade.'"
    putStrLn "   'Através da compaixão (Mitleid) e da arte.'"
    putStrLn "   'A redenção de Raskólnikov vem quando ele começa a negar sua Vontade.'\n"

    -- Comparação Nietzsche vs Schopenhauer
    putStrLn "⚖️  COMPARAÇÃO NIETZSCHE vs SCHOPENHAUER:"
    putStrLn "   Nietzsche: 'Afirme a Vontade! Seja o Super-Homem!'"
    putStrLn "   Schopenhauer: 'Negue a Vontade! Reduza o sofrimento!'"
    putStrLn "   Dostoiévski: 'O sofrimento é o preço da consciência.'"

-- ========================
-- TEORIA DA REPRESENTAÇÃO
-- ========================

-- O Mundo como Representação (fenômeno) vs Vontade (noumenon)
data MundoRepresentacao = MundoRepresentacao
    { fenomenos :: [String]
    , claridade :: Double  -- Quão clara é a representação
    , ilusaoMaya :: Bool   -- A ilusão do mundo fenômico
    }

-- Ascensão do princípio de razão suficiente
analisarRepresentacao :: String -> IO MundoRepresentacao
analisarRepresentacao fenomeno = do
    putStrLn $ "\n👁️  ANALISANDO REPRESENTAÇÃO: " ++ fenomeno

    -- Quanto mais analisamos, mais clara (e ilusória) se torna
    claridade' <- randomRIO (0.3, 0.9)
    let ilusao = claridade' > 0.7  -- Quanto mais claro, mais ilusório

    putStrLn $ "   Claridade da Representação: " ++ show claridade'
    putStrLn $ "   É Ilusão (Maya)? " ++ show ilusao
    putStrLn $ "   Schopenhauer: 'O mundo é minha representação.'"

    return $ MundoRepresentacao [fenomeno] claridade' ilusao

-- ========================
-- SÍNTESE: DOS TRÊS PENSADORES
-- ========================

data SinteseFilosofica = SinteseFilosofica
    { kantRazaoPura :: Double
    , nietzscheVontade :: Double
    , schopenhauerSofrimento :: Double
    , dostoevskyConsciencia :: Double
    } deriving (Show)

calcularSintese :: [EstadoRaskolnikov] -> SinteseFilosofica
calcularSintese estados =
    let totalEstados = length estados
        mediaVontade = sum (map sofreguidaoVontade estados) / fromIntegral totalEstados
        mediaSofrimento = sum (map sofrimentoAcumulado estados) / fromIntegral totalEstados
        mediaCompaixao = sum (map compaixaoResidual estados) / fromIntegral totalEstados
    in SinteseFilosofica
        { kantRazaoPura = 1.0 - mediaVontade  -- Quanto menos Vontade, mais Razão
        , nietzscheVontade = mediaVontade
        , schopenhauerSofrimento = mediaSofrimento
        , dostoevskyConsciencia = mediaCompaixao * 2  -- Dostoiévski valoriza a consciência
        }

-- ========================
-- EXECUÇÃO PRINCIPAL
-- ========================

main :: IO ()
main = do
    putStrLn "🎭 SCHOPENHAUER CRITICA DOSTOIÉVSKI"
    putStrLn "   O Mundo como Vontade, Representação e Sofrimento\n"

    -- Simulação principal
    simularCrimeCastigo

    -- Crítica filosófica
    criticaSchopenhauer

    -- Análise de representações específicas
    putStrLn $ "\n" ++ replicate 50 '~'
    putStrLn "ANÁLISE DE REPRESENTAÇÕES CHAVE:"

    representacoes <- mapM analisarRepresentacao
        [ "O Machado (instrumento da Vontade)"
        , "O Sofrimento de Raskólnikov"
        , "A Compaixão de Sônia"
        , "A Teoria do Homem Extraordinário"
        ]

    -- Síntese final
    putStrLn $ "\n" ++ replicate 50 '='
    putStrLn "🎓 SÍNTESE FILOSÓFICA FINAL"

    -- Criar múltiplos estados para análise
    let estadosTeste =
            [ EstadoRaskolnikov True 0.9 0.1 [] 1.2  -- Raskólnikov pré-crime
            , EstadoRaskolnikov True 0.8 0.3 ["Crime"] 1.5  -- Pós-crime
            , EstadoRaskolnikov False 0.4 0.7 ["Crime"] 0.8  -- Pós-Sônia
            ]

    let sintese = calcularSintese estadosTeste

    putStrLn $ "   Kant (Razão Pura): " ++ show (kantRazaoPura sintese)
    putStrLn $ "   Nietzsche (Vontade): " ++ show (nietzscheVontade sintese)
    putStrLn $ "   Schopenhauer (Sofrimento): " ++ show (schopenhauerSofrimento sintese)
    putStrLn $ "   Dostoiévski (Consciência): " ++ show (dostoevskyConsciencia sintese)

    -- Conclusão
    putStrLn "\n🧠 CONCLUSÃO SCHOPENHAUERIANA:"
    putStrLn "   'Crime e Castigo' não é sobre moralidade, mas sobre a Vontade."
    putStrLn "   Raskólnikov sofre não porque pecou, mas porque afirmou sua Vontade."
    putStrLn "   Sônia redime não através do amor, mas através da compaixão que nega a Vontade."
    putStrLn "   A grande ironia: Raskólnikov buscava ser extraordinário,"
    putStrLn "   mas apenas tornou-se um exemplo comum da Vontade humana sofredora."

    testeVontadeVsRazao

-- ========================
-- TESTES ADICIONAIS
-- ========================

testeVontadeVsRazao :: IO ()
testeVontadeVsRazao = do
    putStrLn "\n⚖️  TESTE: VONTADE vs RAZÃO (Schopenhauer vs Kant)"

    -- Kant: a razão deve controlar as inclinações
    -- Schopenhauer: a razão é apenas serva da Vontade

    let cenario1 = "Raskólnikov planejando o crime"
        cenario2 = "Raskólnikov hesitando"
        cenario3 = "Raskólnikov confessando"

    putStrLn $ "\n1. " ++ cenario1
    putStrLn "   Schopenhauer: 'A razão apenas racionaliza o que a Vontade já decidiu.'"

    putStrLn $ "\n2. " ++ cenario2
    putStrLn "   Kant: 'A razão deveria impor o imperativo categórico.'"
    putStrLn "   Schopenhauer: 'A hesitação é conflito entre Vontades, não razão.'"

    putStrLn $ "\n3. " ++ cenario3
    putStrLn "   Dostoiévski: 'A consciência falou mais alto.'"
    putStrLn "   Schopenhauer: 'Uma Vontade (compaixão) superou outra Vontade (orgulho).'"

    putStrLn "\n🎯 VEREDITO:"
    putStrLn "   Para Schopenhauer, a razão nunca é livre."
    putStrLn "   Ela sempre serve a alguma Vontade."
    putStrLn "   Raskólnikov não é um ser racional, mas um campo de batalha de Vontades."

-- Executar com: main >> testeVontadeVsRazao
