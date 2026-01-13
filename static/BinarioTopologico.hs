{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- ============================================================================
-- ARQUIVO: BinarioTopologico.hs
-- SISTEMA: Binário-Topológico: Mapeamento de Espaços de Informação
-- CONTEXTO: Axioma Mnemosynis 2026: Sistema Empire Silicium
-- PHYLUM: Algorithmi | Extensio: Topologia de Grafos Binários
-- ============================================================================

module Main where

import Data.List (transpose, intercalate, group, sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (isDigit)
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- I. FUNDAMENTOS ONTOLÓGICOS (A Metafísica do Bit)
-- ============================================================================

-- | Estados primordiais: A dualidade fundamental da computação.
data Estado
    = Actus       -- ⚡ 1: Presença, Tensão, Energia, Singularidade
    | Silentium   -- 🕳️ 0: Ausência, Vácuo, Potencial, Espaço
    deriving (Eq, Ord)

-- | Representação visual dos estados para depuração
instance Show Estado where
    show Actus     = "⚡"
    show Silentium = "🕳️"

-- | Transformadores: Operadores que alteram a topologia da informação.
data Transformador
    = Identidade          -- Preservação
    | Complemento         -- Negação (NOT)
    | Rotacao             -- Deslocamento Cíclico
    | InversaoTemporal    -- Reversão
    | EspelhoSimetrico    -- Reflexão
    deriving (Eq, Show)

-- | Um 'quantum' de informação posicionado no espaço-tempo.
data DigitoTopologico = DigitoTopologico
    { estado    :: Estado
    , coord     :: (Int, Int)      -- Posição (x, y)
    , energia   :: Double          -- "Peso" do nó (ex: grau de conectividade)
    } deriving (Show, Eq)

-- | O Grafo Binário: A teia de relações entre os estados.
data GrafoBinario = GrafoBinario
    { mapaNos     :: M.Map (Int, Int) DigitoTopologico
    , conexoes    :: [((Int, Int), (Int, Int))]
    , dimensoes   :: (Int, Int)    -- (Largura, Altura)
    } deriving (Show)

-- ============================================================================
-- II. ALGORITMOS DE CONVERSÃO E INTERPRETAÇÃO
-- ============================================================================

-- | Transmuta uma string bruta em uma sequência ontológica.
interpretarFluxo :: String -> [Estado]
interpretarFluxo = map $ \case
    '1' -> Actus
    '0' -> Silentium
    _   -> Silentium -- O ruído é tratado como silêncio

-- | Serializa a ontologia de volta para a linguagem da máquina.
serializarFluxo :: [Estado] -> String
serializarFluxo = concatMap $ \case
    Actus     -> "1"
    Silentium -> "0"

-- | Aplica transformações morfológicas ao fluxo de dados.
aplicarTransformador :: Transformador -> [Estado] -> [Estado]
aplicarTransformador trans est = case trans of
    Identidade       -> est
    Complemento      -> map toggle est
    Rotacao          -> case est of [] -> []; (x:xs) -> xs ++ [x]
    InversaoTemporal -> reverse est
    EspelhoSimetrico -> est ++ reverse est
  where
    toggle Actus = Silentium
    toggle Silentium = Actus

-- ============================================================================
-- III. ANÁLISE TOPOLÓGICA E ENTROPIA
-- ============================================================================

-- | Calcula a "Densidade de Actus" (A proporção de existência sobre o nada).
densidadeOntologica :: [Estado] -> Double
densidadeOntologica [] = 0
densidadeOntologica ests =
    let total = length ests
        vivos = length (filter (== Actus) ests)
    in fromIntegral vivos / fromIntegral total

-- | Calcula a Entropia de Shannon (Complexidade da Informação).
entropiaInformacional :: [Estado] -> Double
entropiaInformacional ests =
    let p = densidadeOntologica ests
        q = 1 - p
        log2 x = if x == 0 then 0 else log x / log 2
    in if p == 0 || q == 0 then 0 else negate (p * log2 p + q * log2 q)

-- | Detecta padrões recorrentes (subsequências) no fluxo.
detectarPadroes :: Int -> [Estado] -> [(String, Int)]
detectarPadroes len ests =
    let str = serializarFluxo ests
        subs = [take len (drop i str) | i <- [0 .. length str - len]]
        freqs = M.fromListWith (+) $ zip subs (repeat 1)
        ordenados = sortBy (flip (comparing snd)) (M.toList freqs)
    in filter ((>1) . snd) ordenados -- Retorna apenas padrões que se repetem

-- ============================================================================
-- IV. GERAÇÃO PROCEDURAL (FRACTAIS E AUTÔMATOS)
-- ============================================================================

-- | Gera o Fractal de Sierpinski (Regra 90) como uma matriz de estados.
-- Representa a emergência de ordem a partir de regras simples.
sierpinski :: Int -> [[Estado]]
sierpinski n = take (2^n) $ iterate evoluir geracaoInicial
  where
    largura = 2^(n+1)
    geracaoInicial = replicate (largura `div` 2) Silentium ++ [Actus] ++ replicate (largura `div` 2) Silentium

    evoluir :: [Estado] -> [Estado]
    evoluir linha =
        let padded = [Silentium] ++ linha ++ [Silentium]
            janelas = zip3 padded (drop 1 padded) (drop 2 padded)
        in map regra90 janelas

    regra90 :: (Estado, Estado, Estado) -> Estado
    regra90 (e1, _, e3) = if e1 /= e3 then Actus else Silentium -- XOR lógico

-- | Sequência de Fibonacci binária (Paridade).
fibonacciBinario :: Int -> [Estado]
fibonacciBinario n =
    let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
        paridade x = if odd x then Actus else Silentium
    in map paridade (take n fibs)

-- ============================================================================
-- V. TEORIA DOS GRAFOS E CONECTIVIDADE
-- ============================================================================

-- | Converte uma matriz 2D em um Grafo Topológico.
criarGrafo :: [[Estado]] -> GrafoBinario
criarGrafo matriz =
    let linhas = length matriz
        cols   = if null matriz then 0 else length (head matriz)
        coords = [(r, c) | r <- [0..linhas-1], c <- [0..cols-1]]

        -- Cria nós
        nos = M.fromList $ map (\(r,c) ->
            ((r,c), DigitoTopologico (matriz !! r !! c) (r,c) 0.0)) coords

        -- Cria arestas (Conectividade Von Neumann - 4 vizinhos)
        arestas = [ ((r,c), (r',c'))
                  | r <- [0..linhas-1], c <- [0..cols-1]
                  , (r', c') <- [(r+1,c), (r,c+1)] -- Apenas para frente/baixo para evitar duplicação
                  , r' < linhas, c' < cols
                  ]
    in GrafoBinario nos arestas (cols, linhas)

-- | Conta ilhas de 'Actus' (Componentes Conectados).
contarIlhasActus :: GrafoBinario -> Int
contarIlhasActus grafo =
    let nosAtivos = M.keys $ M.filter (\d -> estado d == Actus) (mapaNos grafo)
        setAtivos = S.fromList nosAtivos
    in length (explorarIlhas setAtivos [])
  where
    explorarIlhas :: S.Set (Int, Int) -> [S.Set (Int, Int)] -> [S.Set (Int, Int)]
    explorarIlhas naoVisitados ilhas
        | S.null naoVisitados = ilhas
        | otherwise =
            let semente = S.elemAt 0 naoVisitados
                (novaIlha, restante) = floodFill semente naoVisitados
            in explorarIlhas restante (novaIlha : ilhas)

    floodFill :: (Int, Int) -> S.Set (Int, Int) -> (S.Set (Int, Int), S.Set (Int, Int))
    floodFill start disponiveis =
        let vizinhos (r,c) = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

            go visitados [] = visitados
            go visitados (atual:fila) =
                let adj = filter (`S.member` disponiveis) (vizinhos atual)
                    novos = filter (not . (`S.member` visitados)) adj
                in go (foldr S.insert visitados novos) (fila ++ novos)

            ilha = go (S.singleton start) [start]
        in (ilha, S.difference disponiveis ilha)

-- ============================================================================
-- VI. VISUALIZAÇÃO E INTERFACE
-- ============================================================================

-- | Renderiza o estado com glifos Unicode de alta densidade.
renderizarMatriz :: [[Estado]] -> IO ()
renderizarMatriz matriz = do
    putStrLn "┌────────────────────────────────────────────────────────┐"
    forM_ matriz $ \linha -> do
        putStr "│ "
        putStr $ concatMap glyph linha
        putStrLn " │"
    putStrLn "└────────────────────────────────────────────────────────┘"
  where
    glyph Actus     = "██" -- Bloco cheio
    glyph Silentium = "  " -- Espaço vazio (ou "░░")

-- | Relatório Analítico do Sistema.
gerarRelatorio :: [Estado] -> IO ()
gerarRelatorio fluxo = do
    let dens = densidadeOntologica fluxo
    let ent = entropiaInformacional fluxo
    let pads = take 3 $ detectingPadroes 3 fluxo

    putStrLn "\n📊 RELATÓRIO DE ANÁLISE TOPOLÓGICA"
    putStrLn "──────────────────────────────────"
    putStrLn $ "🔹 Comprimento do Fluxo : " ++ show (length fluxo)
    putStrLn $ "🔹 Densidade (Actus)    : " ++ take 6 (show dens)
    putStrLn $ "🔹 Entropia (Shannon)   : " ++ take 6 (show ent)
    putStrLn "🔹 Interpretação        :"
    putStrLn $ "   " ++ interpretarAnalise dens ent
    putStrLn "🔹 Padrões Recorrentes (3-bit):"
    if null pads
        then putStrLn "   (Nenhum padrão significativo detectado)"
        else mapM_ (\(p, n) -> putStrLn $ "   [" ++ p ++ "] ocorre " ++ show n ++ " vezes") pads
  where
    detectingPadroes = detectarPadroes

    interpretarAnalise d e
        | d < 0.1   = "Vácuo Informacional (Predomínio de Silentium)"
        | d > 0.9   = "Saturação Energética (Predomínio de Actus)"
        | e > 0.95  = "Caos Máximo / Ruído Branco"
        | e < 0.2   = "Cristalização / Ordem Rígida"
        | otherwise = "Equilíbrio Complexo (Zona de Computação)"

-- ============================================================================
-- MAIN: O MOTOR DO SISTEMA
-- ============================================================================

main :: IO ()
main = do
    putStrLn "\n🧿 SISTEMA BINÁRIO-TOPOLÓGICO v2026 🧿"
    putStrLn "   Axioma Mnemosynis | Phylum Algorithmi\n"

    -- 1. Análise de uma string binária arbitrária
    putStrLn "1. ANÁLISE DE SEQUÊNCIA (Exemplo Fibonacci)"
    let fibSeq = fibonacciBinario 20
    putStrLn $ "   Fluxo: " ++ concatMap show fibSeq
    gerarRelatorio fibSeq

    -- 2. Demonstração de Transformadores
    putStrLn "\n2. APLICAÇÃO DE TRANSFORMADORES"
    let original = take 10 fibSeq
    putStrLn $ "   Original  : " ++ concatMap show original
    putStrLn $ "   Inversão  : " ++ concatMap show (aplicarTransformador Complemento original)
    putStrLn $ "   Reflexão  : " ++ concatMap show (aplicarTransformador EspelhoSimetrico original)

    -- 3. Geração e Visualização de Fractal
    putStrLn "\n3. TOPOLOGIA EMERGENTE: FRACTAL DE SIERPINSKI (n=4)"
    let fractal = sierpinski 4
    renderizarMatriz fractal

    -- 4. Análise de Grafos no Fractal
    putStrLn "4. ANÁLISE DE GRAFO (Conectividade do Fractal)"
    let grafo = criarGrafo fractal
    let ilhas = contarIlhasActus grafo
    putStrLn $ "   Dimensões do Espaço : " ++ show (dimensoes grafo)
    putStrLn $ "   Total de Nós Actus  : " ++ show (length $ filter (==Actus) (concat fractal))
    putStrLn $ "   Ilhas Conectadas    : " ++ show ilhas
    putStrLn $ "   (Regiões de Actus contíguos na topologia)"

    putStrLn "\n🏁 Execução do Axioma finalizada."
