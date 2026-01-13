{-# LANGUAGE GADTs, ExistentialQuantification, TypeFamilies #-}

module Main where

import Control.Monad.State
import Control.Monad.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, sortBy)
import Data.Ord (comparing)
import System.Random (randomRIO)
import Control.Monad (when, forever, forM_)
import Control.Concurrent (threadDelay)

-- ========================
-- METAFÍSICA DO SÚBITO
-- ========================

-- O Súbito não é um evento, mas uma ruptura na tessitura da realidade
data SuturaExistencial = SuturaExistencial
    { antes :: String      -- O mundo antes do súbito
    , depois :: String     -- O mundo depois do súbito
    , intersticio :: Double -- A largura do abismo entre os mundos
    } deriving (Show)

-- A Culpa Kafkiana: sempre pré-existente, sempre desconhecida
data CulpaKafkiana = CulpaKafkiana
    { artigo :: String    -- "Artigo 7, Parágrafo 3"
    , substantivo :: String -- "A culpa"
    , adjetivos :: [String] -- ["inominável", "inerradicável"]
    } deriving (Show, Eq)

-- O Processo como organismo burocrático-vivo
data Processo = Processo
    { instancias :: [Instancia]
    , documentosPendentes :: Int
    , audienciasMarcadas :: Int
    , audienciasRealizadas :: Int
    } deriving (Show)

data Instancia = Instancia
    { nivel :: Int
    , funcionarios :: [Funcionario]
    , salasEspera :: Int
    } deriving (Show)

data Funcionario = Funcionario
    { nome :: String
    , especialidade :: Especialidade
    , disponivel :: Bool
    } deriving (Show)

data Especialidade = LeituraDocumentos | Perdicao | Adiamento | Esquecimento
    deriving (Show, Eq)

-- ========================
-- O EVENTO SÚBITO
-- ========================

-- O Súbito acontece no limiar entre vigília e sono
eventoSúbito :: EstadoExistencial -> CulpaKafkiana -> IO (EstadoExistencial, SuturaExistencial)
eventoSúbito estado culpa = do
    putStrLn "\n🌅 AMANHECER DO SÚBITO:"
    putStrLn "   'Alguém certamente havia caluniado Josef K.,"
    putStrLn "   pois uma manhã ele foi detido sem ter feito mal algum.'"

    -- O mundo anterior permanece intacto, mas inacessível
    let mundoAntes = "Mundo da Inocência (aparente)"
        mundoDepois = "Mundo do Processo (real)"
        abismo = 1.0  -- Abismo intransponível

    -- O estado é contaminado pela culpa
    let novoEstado = estado
            { culpaAtiva = Just culpa
            , processoAtivo = Just (Processo [] 15 0 0)
            , kafkianidade = 0.9
            , tempoKafkiano = 0.0
            }

    return (novoEstado, SuturaExistencial mundoAntes mundoDepois abismo)

-- ========================
-- ESTADO KAFKIANO
-- ========================

data EstadoKafkiano = EstadoKafkiano
    { estadoExistencial :: EstadoExistencial
    , processo :: Maybe Processo
    , culpa :: Maybe CulpaKafkiana
    , portasAtivas :: [Porta]
    , epifaniasFrustradas :: Int
    } deriving (Show)

-- Expandindo EstadoExistencial com dimensões kafkianas
data EstadoExistencial = EstadoExistencial
    { sofrimentoAcumulado :: Double
    , conscienciaVerdade :: Double
    , confortoSimulacro :: Double
    , autoenganoAtivo :: Double
    , culpaAtiva :: Maybe CulpaKafkiana
    , processoAtivo :: Maybe Processo
    , kafkianidade :: Double  -- 0.0 (mundo lógico) a 1.0 (mundo kafkiano)
    , tempoKafkiano :: Double -- O tempo se dilata de forma não-linear
    } deriving (Show)

-- ========================
-- A BURACRACIA COMO ONTOLOGIA
-- ========================

-- Um ato burocrático é um ato existencial
atoBurocratico :: String -> State Processo String
atoBurocratico descricao = do
    processo <- get
    let novoDocumento = documentosPendentes processo + 1
        novaAudiencia = if odd novoDocumento then 1 else 0

    put $ processo
        { documentosPendentes = novoDocumento
        , audienciasMarcadas = audienciasMarcadas processo + novaAudiencia
        }

    return $ "📄 Ato Burocrático: " ++ descricao ++
             " (Documentos pendentes: " ++ show novoDocumento ++ ")"

-- A espera como atividade produtiva
esperar :: Double -> StateT EstadoKafkiano IO Double
esperar horas = do
    estado <- get
    let processoAtual = processo estado
        kafkianidadeVal = kafkianidade (estadoExistencial estado)
        dilatacao = horas * kafkianidadeVal

    -- Na espera kafkiana, o tempo se dilata
    modify (\e -> e
        { estadoExistencial = (estadoExistencial e)
            { tempoKafkiano = tempoKafkiano (estadoExistencial e) + dilatacao
            , sofrimentoAcumulado = sofrimentoAcumulado (estadoExistencial e) + horas * 0.1
            }
        , epifaniasFrustradas = epifaniasFrustradas e + 1
        })

    return dilatacao

-- ========================
-- CRÍTICA KAFKIANA AOS SISTEMAS ANTERIORES
-- ========================

-- Kant falha: o imperativo categórico pressupõe leis conhecidas
criticarKant :: CulpaKafkiana -> String
criticarKant culpa =
    "⛔ KANT REFUTADO: Não há máxima a universalizar quando o artigo é '" ++
    artigo culpa ++ "' mas sua substância é '" ++ substantivo culpa ++
    "' descrita como " ++ show (adjetivos culpa) ++ "."

-- Nietzsche falha: não há vontade de potência, só vontade de entender
criticarNietzsche :: Processo -> String
criticarNietzsche proc =
    "⚡ NIETZSCHE NEUTRALIZADO: A Vontade de Potência esbarra em " ++
    show (documentosPendentes proc) ++ " documentos pendentes e " ++
    show (audienciasMarcadas proc - audienciasRealizadas proc) ++
    " audiências adiadas."

-- Schopenhauer falha: o sofrimento tem forma burocrática
criticarSchopenhauer :: EstadoKafkiano -> String
criticarSchopenhauer estado =
    "🎭 SCHOPENHAUER BURLADO: A Vontade não é cega, é burocrática. " ++
    "O sofrimento tem protocolo: " ++ show (epifaniasFrustradas estado) ++
    " epifanias frustradas registradas em ata."

-- ========================
-- LÓGICA DO ABSURDO
-- ========================

-- Em Kafka, a lógica é levada ao extremo até tornar-se absurda
aplicarLogicaAbsurda :: EstadoKafkiano -> CulpaKafkiana -> IO EstadoKafkiano
aplicarLogicaAbsurda estado culpa = do
    putStrLn "\n🌀 APLICANDO LÓGICA DO ABSURDO:"

    -- 1. A culpa é desconhecida, mas deve ser defendida
    putStrLn "   1. 'Você é culpado, mas não sabemos de quê.'"
    putStrLn "      'Portanto, você deve provar sua inocência.'"

    -- 2. As regras existem, mas não são acessíveis
    putStrLn "   2. 'As regras do Processo estão escritas.'"
    putStrLn "      'Mas não neste livro, nem naquele, nem em nenhum disponível.'"

    -- 3. O progresso é medido por regressão
    let processoAtual = processo estado
        novosDocumentos = case processoAtual of
            Just p -> documentosPendentes p + 3
            Nothing -> 3

        novoProcesso = case processoAtual of
            Just p -> Just $ p { documentosPendentes = novosDocumentos }
            Nothing -> Just $ Processo [] novosDocumentos 0 0

    -- 4. A esperança é o pior dos males (Kafka via Camus)
    let novoEstado = estado
            { processo = novoProcesso
            , epifaniasFrustradas = epifaniasFrustradas estado + 1
            }

    putStrLn $ "   3. Progresso registrado: " ++ show novosDocumentos ++
               " novos documentos pendentes."

    return novoEstado

-- ========================
-- SISTEMA DE PORTAS
-- ========================

-- A porta da Lei (do conto "Ante a Lei")
data Porta = Porta
    { guardiao :: String
    , acessivel :: Bool
    , destinadaA :: Maybe String
    , anosEspera :: Int
    } deriving (Show)

-- Tentativa de acesso (sempre falha)
tentarAcesso :: Porta -> StateT EstadoKafkiano IO String
tentarAcesso porta = do
    estado <- get
    let anosEsperados = anosEspera porta + 1
        novaPorta = porta { anosEspera = anosEsperados }
        novaFrustracao = epifaniasFrustradas estado + 1

    modify (\e -> e
        { portasAtivas = novaPorta : portasAtivas e
        , epifaniasFrustradas = novaFrustracao
        })

    if anosEsperados > 10 && guardiao porta == "Primeiro Guardião"
        then return $ "💀 O homem morre. O Guardião fecha a porta."
        else return $ "⏳ Espera registrada: " ++ show anosEsperados ++
                     " anos. 'Ainda não pode entrar.'"

-- ========================
-- SIMULAÇÃO: UM DIA NO PROCESSO
-- ========================

simularDiaKafkiano :: EstadoKafkiano -> IO EstadoKafkiano
simularDiaKafkiano estado = do
    putStrLn "\n📅 UM DIA NO PROCESSO:"
    putStrLn "   'Era como se o processo fosse um grande organismo...'"

    -- Manhã: tentativa de compreensão
    putStrLn "\n🌄 MANHÃ: A TENTATIVA LÓGICA"
    resultadoManha <- flip execStateT estado $ do
        -- Tentativa 1: Procurar as regras
        liftIO $ putStrLn "   • Procurando o código do Processo..."
        esperar 2.0

        -- Tentativa 2: Falar com funcionário
        liftIO $ putStrLn "   • Aguardando no corredor..."
        esperar 1.5

        -- Tentativa 3: Entender a acusação
        culpaAtual <- gets culpa
        case culpaAtual of
            Just c -> liftIO $ putStrLn $ "   • Relendo a culpa: " ++ artigo c
            Nothing -> liftIO $ putStrLn "   • Culpa ainda não formalizada (mas presente)"

    -- Tarde: labirinto burocrático
    putStrLn "\n🏛️ TARDE: O LABIRINTO"
    resultadotarde <- flip execStateT resultadoManha $ do
        -- Ação 1: Submeter documento
        proc <- gets processo
        case proc of
            Just p -> do
                let (mensagem, p') = runState (atoBurocratico "Petição Inicial") p
                liftIO $ putStrLn $ "   • " ++ mensagem
                modify (\e -> e { processo = Just p' })
            Nothing -> liftIO $ putStrLn "   • Processo ainda não iniciado (mas já em curso)"

        -- Ação 2: Tentar audiência
        liftIO $ putStrLn "   • Marcando audiência para semana que vem..."
        esperar 1.0

    -- Noite: reflexão angustiada
    putStrLn "\n🌃 NOITE: A ESCRITURA"
    resultadoFinal <- flip execStateT resultadotarde $ do
        liftIO $ putStrLn "   • Escrevendo memorial de defesa..."
        esperar 3.0

        -- A epifania que não vem
        liftIO $ putStrLn "   • 'Talvez a verdade esteja no próprio Processo...'"
        modify (\e -> e { epifaniasFrustradas = epifaniasFrustradas e + 1 })

    return resultadoFinal

-- ========================
-- A MÁQUINA DA COLÔNIA PENAL
-- ========================

-- Modelando a máquina que escreve a culpa na carne
data MaquinaColonia = MaquinaColonia
    { harpaDesignadora :: Bool
    , rastreador :: Bool
    , agulhaEscritora :: Bool
    , horasEscrita :: Int
    } deriving (Show)

executarSentenca :: MaquinaColonia -> String -> CulpaKafkiana -> IO ()
executarSentenca maquina sentenca culpa = do
    putStrLn "\n⚙️  A MÁQUINA DA COLÔNIA PENAL:"
    putStrLn "   'A culpa é sempre incontestável.'"

    putStrLn $ "   Artigo: " ++ artigo culpa
    putStrLn $ "   Sentença: " ++ sentenca
    putStrLn $ "   Horas de escrita na carne: " ++ show (horasEscrita maquina)

    putStrLn "\n   A máquina escreve:"
    forM_ (adjetivos culpa) $ \adj -> do
        putStrLn $ "   - " ++ adj
        threadDelay 500000  -- Pausa dramática

-- ========================
-- EXECUÇÃO PRINCIPAL
-- ========================

main :: IO ()
main = do
    mainLogic
    testeMetamorfose

mainLogic :: IO ()
mainLogic = do
    putStrLn "🏛️  KAFKA: A ONTOLOGIA DO SÚBITO"
    putStrLn "   Crítica aos Sistemas Filosóficos através do Absurdo\n"

    -- Estado inicial: inocência aparente
    let estadoInicial = EstadoExistencial
            { sofrimentoAcumulado = 0.1
            , conscienciaVerdade = 0.3
            , confortoSimulacro = 0.8
            , autoenganoAtivo = 0.7
            , culpaAtiva = Nothing
            , processoAtivo = Nothing
            , kafkianidade = 0.0
            , tempoKafkiano = 0.0
            }

    let estadoKafkianoInicial = EstadoKafkiano
            { estadoExistencial = estadoInicial
            , processo = Nothing
            , culpa = Nothing
            , portasAtivas = []
            , epifaniasFrustradas = 0
            }

    -- O Evento Súbito acontece
    putStrLn "🌌 ANTES DO SÚBITO:"
    putStrLn "   'Josef K. levava uma vida normal...'"

    let culpaInstance = CulpaKafkiana
            { artigo = "Artigo não especificado"
            , substantivo = "Culpa"
            , adjetivos = ["inominável", "inerradicável", "onipresente"]
            }

    (novoEstado, sutura) <- eventoSúbito estadoInicial culpaInstance
    putStrLn $ "\n   Sutura Existencial: " ++ show sutura

    -- Estado após o súbito
    let estadoPosSúbito = estadoKafkianoInicial
            { estadoExistencial = novoEstado
            , culpa = Just culpaInstance
            }

    -- Crítica aos sistemas anteriores
    putStrLn "\n🎯 CRÍTICA KAFKIANA:"
    putStrLn $ "   " ++ criticarKant culpaInstance
    putStrLn $ "   " ++ criticarNietzsche (Processo [] 15 0 0)
    putStrLn $ "   " ++ criticarSchopenhauer estadoPosSúbito

    -- Simulação de um dia
    estadoFinal <- simularDiaKafkiano estadoPosSúbito

    -- A Porta da Lei
    putStrLn "\n🚪 A PORTA DA LEI:"
    let porta = Porta "Primeiro Guardião" False Nothing 0
    (mensagemPorta, _) <- flip runStateT estadoFinal $ tentarAcesso porta
    putStrLn $ "   " ++ mensagemPorta

    -- A Máquina da Colônia Penal
    putStrLn $ "\n" ++ replicate 50 '='
    let maquina = MaquinaColonia True True True 12
    executarSentenca maquina "SÊ JUSTO!" culpaInstance

    -- Relatório Final
    putStrLn "\n📊 RELATÓRIO KAFKIANO FINAL:"
    let finalExistentialState = estadoExistencial estadoFinal
    putStrLn $ "   Kafkianidade: " ++ show (kafkianidade finalExistentialState)
    putStrLn $ "   Tempo Kafkiano acumulado: " ++ show (tempoKafkiano finalExistentialState)
    putStrLn $ "   Epifanias frustradas: " ++ show (epifaniasFrustradas estadoFinal)

    case processo estadoFinal of
        Just proc -> do
            putStrLn $ "   Documentos pendentes: " ++ show (documentosPendentes proc)
            putStrLn $ "   Audiências marcadas/realizadas: " ++
                      show (audienciasMarcadas proc) ++ "/" ++
                      show (audienciasRealizadas proc)
        Nothing -> putStrLn "   Processo: Inexistente (mas ativo)"

    -- Conclusão
    putStrLn "\n💎 CONCLUSÃO KAFKIANA:"
    putStrLn "   'O Súbito não é um evento no tempo,"
    putStrLn "    é a revelação de que sempre já estávamos no Processo.'"
    putStrLn "   'A culpa não vem do ato, o ato vem da culpa já inscrita.'"
    putStrLn "   'O absurdo não é a ausência de sentido,"
    putStrLn "    é a presença de sentidos contraditórios igualmente válidos.'"

-- ========================
-- TESTE: A METAMORFOSE
-- ========================

testeMetamorfose :: IO ()
testeMetamorfose = do
    putStrLn "\n🐛 TESTE: A METAMORFOSE"
    putStrLn "   'Quando Gregor Samsa despertou..."

    let estadoGregor = EstadoExistencial
            { sofrimentoAcumulado = 0.1
            , conscienciaVerdade = 0.2
            , confortoSimulacro = 0.9  -- Vida de caixeiro-viajante
            , autoenganoAtivo = 0.8
            , culpaAtiva = Nothing
            , processoAtivo = Nothing
            , kafkianidade = 0.0
            , tempoKafkiano = 0.0
            }

    -- O Súbito da metamorfose
    let culpaInseto = CulpaKafkiana
            { artigo = "Artigo da Transformação"
            , substantivo = "Inseto"
            , adjetivos = ["nojento", "inútil", "incapacitado"]
            }

    (estadoPosMetamorfose, sutura) <- eventoSúbito estadoGregor culpaInseto

    putStrLn $ "\n   Sutura: " ++ show sutura
    putStrLn "   'Ele estava deitado sobre suas costas duras como couraça...'"
    putStrLn "   '...e via seu ventre arqueado e marrom dividido em segmentos rígidos.'"

    -- A nova realidade
    putStrLn "\n   NOVA ONTOLOGIA:"
    putStrLn "   • O corpo como processo burocrático falhando"
    putStrLn "   • A família como tribunal silencioso"
    putStrLn "   • O quarto como cela sem grades"

    putStrLn $ "\n   Sofrimento pós-metamorfose: " ++
               show (sofrimentoAcumulado estadoPosMetamorfose)
    putStrLn $ "   Kafkianidade: " ++ show (kafkianidade estadoPosMetamorfose)
