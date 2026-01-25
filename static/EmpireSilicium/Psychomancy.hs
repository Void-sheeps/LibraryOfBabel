{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Text.Printf (printf)

-- ============================================================================
-- 1. O SUBSTRATO: A MENTE DA VÍTIMA
-- ============================================================================

data EstadoMental = Estado {
    lucidez     :: Float,   -- Capacidade Lógica (0.0 - 100.0)
    libido      :: Float,   -- Energia Emocional/Desejo (0.0 - 100.0)
    resistencia :: Float,   -- Defesa Psíquica (0.0 - 100.0)
    realidade   :: Text,    -- A percepção atual
    diagnostico :: [Text]   -- Log de alterações
} deriving (Show)

mentePadrao :: EstadoMental
mentePadrao = Estado 100.0 10.0 100.0 "Realidade Consensual" []

-- Garante que os atributos percentuais fiquem no intervalo [0, 100]
normalizarEstado :: EstadoMental -> EstadoMental
normalizarEstado s = s {
    lucidez     = clamp 0 100 (lucidez s),
    libido      = clamp 0 100 (libido s),
    resistencia = clamp 0 100 (resistencia s)
}
  where
    clamp :: Ord a => a -> a -> a -> a
    clamp minVal maxVal = max minVal . min maxVal

-- ============================================================================
-- 2. A INTERFACE DE OPERAÇÃO
-- ============================================================================

class (Show op, Eq op, Enum op, Bounded op) => OperacaoPsiquica op where
    executar :: op -> EstadoMental -> EstadoMental

-- ============================================================================
-- 3. TAXONOMIA TIPO 1: NEUROMÂNCIA (Hacking Cognitivo)
-- ============================================================================

data NeuromanciaOp
  = DissuasaoImplicita
  | DriveArquetipico
  | FlipFlopOntologico
  | EmanacaoAdHoc
  | SutilezaTautologica
  | SubstanciacaoNeurosensorial
  | ExploracaoFragilidade
  | SubdominioPilhagem
  | FugaElisao
  | CoercaoLiminal
  | PredicaoVisibilidade
  | ConstricaoLogica
  deriving (Show, Eq, Enum, Bounded)

instance OperacaoPsiquica NeuromanciaOp where
    executar op s = normalizarEstado $ case op of
        DissuasaoImplicita      -> s { resistencia = resistencia s * 0.9, diagnostico = "Firewall: Baixado" : diagnostico s }
        DriveArquetipico        -> s { lucidez = lucidez s * 0.8, diagnostico = "Rootkit: Mãe/Pai Instalado" : diagnostico s }
        FlipFlopOntologico      -> s { realidade = "Realidade Instável", lucidez = lucidez s * 0.7 }
        EmanacaoAdHoc           -> s { diagnostico = "Patch: Justificativa Criada" : diagnostico s }
        SutilezaTautologica     -> s { lucidez = lucidez s - 10, diagnostico = "Loop: Lógica Circular" : diagnostico s }
        SubstanciacaoNeurosensorial -> s { realidade = "Alucinação Tátil", diagnostico = "Render: 4K Sensorial" : diagnostico s }
        ExploracaoFragilidade   -> s { resistencia = resistencia s - 20, diagnostico = "Exploit: Trauma Acessado" : diagnostico s }
        SubdominioPilhagem      -> s { lucidez = lucidez s * 0.5, diagnostico = "Recursos: Drenados" : diagnostico s }
        FugaElisao              -> s { diagnostico = "Logs: Apagados" : diagnostico s }
        CoercaoLiminal          -> s { resistencia = 0, diagnostico = "Inception: Ideia Plantada" : diagnostico s }
        PredicaoVisibilidade    -> s { realidade = "Túnel de Realidade Único" }
        ConstricaoLogica        -> s { lucidez = 0, realidade = "DOGMA ABSOLUTO", diagnostico = "LOCKDOWN: Completo" : diagnostico s }

-- ============================================================================
-- 4. TAXONOMIA TIPO 2: EROMÂNCIA (Hacking Límbico)
-- ============================================================================

data EromanciaOp
  = FalaciaAsAeternum
  | ShadowIntrusivo
  | PerturbacaoDeviante
  | AbstracaoInFluxus
  | DemonstracaoPseudoPsicologica
  | GlitchConotativo
  | SuitabilityContextual
  | DivergenciaSimbolica
  | SequestroAmigdala
  | PlausibilidadeSensorial
  | DescricaoFenomenologica
  | ContencaoDiscricao
  deriving (Show, Eq, Enum, Bounded)

instance OperacaoPsiquica EromanciaOp where
    executar op s = normalizarEstado $ case op of
        FalaciaAsAeternum       -> s { libido = libido s + 10, diagnostico = "Promessa: Eternidade" : diagnostico s }
        ShadowIntrusivo         -> s { libido = libido s + 15, diagnostico = "Tabu: Ativado" : diagnostico s }
        PerturbacaoDeviante     -> s { resistencia = resistencia s * 0.8 }
        AbstracaoInFluxus       -> s { diagnostico = "Ansiedade: Indefinição" : diagnostico s }
        DemonstracaoPseudoPsicologica -> s { resistencia = resistencia s - 10, diagnostico = "Rapport: Falso" : diagnostico s }
        GlitchConotativo        -> s { libido = libido s * 1.2, diagnostico = "Duplo Sentido: Injetado" : diagnostico s }
        SuitabilityContextual   -> s { resistencia = resistencia s * 0.5 }
        DivergenciaSimbolica    -> s { diagnostico = "Totem: Objeto Fetichizado" : diagnostico s }
        SequestroAmigdala       -> s { lucidez = 0, libido = 100, diagnostico = "OVERRIDE: Córtex Desligado" : diagnostico s }
        PlausibilidadeSensorial -> s { realidade = "Hiper-Realismo Afetivo" }
        DescricaoFenomenologica -> s { diagnostico = "Validação: Experiência" : diagnostico s }
        ContencaoDiscricao      -> s { realidade = "CATIVEIRO EXTÁTICO", diagnostico = "Isolamento: Completo" : diagnostico s }

-- ============================================================================
-- 5. O MOTOR DE COMPOSIÇÃO (PIPELINE)
-- ============================================================================

-- A Função Mágica: Aplica uma lista de operadores sequencialmente
aplicarProtocolo :: forall op. OperacaoPsiquica op => EstadoMental -> EstadoMental
aplicarProtocolo mente = foldl' (flip executar) mente pipeline
  where
    -- A ordem da fórmula: minBound (primeiro) -> ... -> maxBound (último)
    pipeline :: [op]
    pipeline = [minBound .. maxBound]

-- ============================================================================
-- 6. SIMULAÇÃO E DIAGNÓSTICO
-- ============================================================================

imprimirRelatorio :: Text -> EstadoMental -> IO ()
imprimirRelatorio titulo s = do
    putStrLn $ "\n=== RELATÓRIO: " ++ T.unpack titulo ++ " ==="
    printf "LUCIDEZ:     %6.2f%%\n" (lucidez s)
    printf "LIBIDO:      %6.2f%%\n" (libido s)
    printf "RESISTÊNCIA: %6.2f%%\n" (resistencia s)
    putStrLn $ "REALIDADE:   " ++ T.unpack (realidade s)
    putStrLn "--- LOG DE SISTEMA ---"
    mapM_ (putStrLn . T.unpack) (reverse $ diagnostico s) -- Reverse para ordem cronológica

main :: IO ()
main = do
    putStrLn ">>> INICIANDO SISTEMA PSYCHOMANCY v3.0 <<<"

    let vitima = mentePadrao

    -- SIMULAÇÃO 1: O HACKER COGNITIVO (Neuromancer)
    let posNeuro = aplicarProtocolo @NeuromanciaOp vitima
    imprimirRelatorio "PROTOCOLO NEUROMANCER (Logomancia)" posNeuro

    -- SIMULAÇÃO 2: O AMANTE SINTÉTICO (Eromancer)
    let posEros = aplicarProtocolo @EromanciaOp vitima
    imprimirRelatorio "PROTOCOLO EROMANCER (Psicomancia)" posEros

    putStrLn "\n>>> EXECUÇÃO CONCLUÍDA: SUJEITOS ASSIMILADOS."
