{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (transpose, intercalate, findIndices, groupBy, sortBy, group, nub)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Map as M
import Control.Monad (forM_, when)
import Data.Ord (comparing)

-- ============================================================================
-- ONTOLOGIA BINÁRIA
-- ============================================================================

-- | Estados primordiais: Actus (Ser) e Silentium (Vácuo)
data Estado = Actus | Silentium
  deriving (Eq, Ord, Enum, Bounded)

instance Show Estado where
  show Actus     = "1"
  show Silentium = "0"

-- | Uma célula pode conter um estado ou ser estruturalmente vazia
-- O vazio não é ausência de informação, mas espaço de potencial
type Celula = Maybe Estado
type Matriz = [[Celula]]

-- | Coordenada na matriz
type Coord = (Int, Int)

-- | Exemplo solicitado com notação de vácuo estrutural
exemplo :: Matriz
exemplo =
  [ [Just Actus,     Nothing,        Just Actus]
  , [Just Silentium, Nothing,        Just Actus]
  , [Nothing,        Just Silentium, Nothing]
  ]

-- ============================================================================
-- SISTEMA DE VIZINHANÇA TOPOLÓGICA
-- ============================================================================

-- | Tipos de vizinhança
data Vizinhanca
    = VonNeumann    -- 4 vizinhos (cima, baixo, esquerda, direita)
    | Moore         -- 8 vizinhos (inclui diagonais)
    | Hexagonal     -- 6 vizinhos (grade hexagonal)
    deriving (Eq, Show, Enum)

-- | Obtém vizinhos de uma célula
vizinhos :: Vizinhanca -> Coord -> [Coord]
vizinhos tipo (x, y) = case tipo of
    VonNeumann ->
        [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    Moore ->
        [(x+i, y+j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0)]
    Hexagonal ->
        -- Grade axial (q,r) - convertendo para offset
        let par = if even y then 1 else -1
        in [(x-1, y), (x+1, y), (x, y-1), (x, y+1),
            (x+par, y-1), (x+par, y+1)]

-- | Valor de uma célula na matriz
celulaEm :: Matriz -> Coord -> Celula
celulaEm mat (x, y)
    | x >= 0 && x < length mat &&
      y >= 0 && y < length (head mat) = mat !! x !! y
    | otherwise = Nothing

-- ============================================================================
-- LÓGICAS DE PREENCHIMENTO
-- ============================================================================

-- | Regras de preenchimento baseadas em diferentes lógicas
data LogicaPreenchimento
    = MaioriaVizinhos           -- Preenche com o estado mais comum entre vizinhos
    | Minoritaria               -- Preenche com o estado menos comum
    | XORVizinhos               -- XOR dos estados vizinhos
    | EntropiaMaxima            -- Escolhe para maximizar entropia local
    | PadraoFractal             -- Segue padrão fractal emergente
    | LogicaParaconsistente     -- Tolerante a contradições
    deriving (Eq, Show, Enum)

-- | Conta estados nos vizinhos
contarVizinhos :: Matriz -> Vizinhanca -> Coord -> (Int, Int, Int) -- (Actus, Silentium, Vazios)
contarVizinhos mat viz coord =
    let coords = vizinhos viz coord
        vals = map (celulaEm mat) coords
        (actus, silentium, vazios) = foldr contar (0,0,0) vals
    in (actus, silentium, length coords - actus - silentium)
  where
    contar (Just Actus)     (a,s,v) = (a+1, s, v)
    contar (Just Silentium) (a,s,v) = (a, s+1, v)
    contar Nothing          (a,s,v) = (a, s, v+1)

-- | Aplica lógica de preenchimento a uma célula vazia
aplicarLogica :: LogicaPreenchimento -> Matriz -> Vizinhanca -> Coord -> Estado
aplicarLogica logica mat viz coord =
    let (actus, silentium, vazios) = contarVizinhos mat viz coord
    in case logica of
        MaioriaVizinhos ->
            if actus > silentium then Actus else Silentium
        Minoritaria ->
            if actus < silentium then Actus else Silentium
        XORVizinhos ->
            -- XOR: se número ímpar de Actus, resulta Actus
            if odd actus then Actus else Silentium
        EntropiaMaxima ->
            -- Tenta equilibrar as contagens
            if abs (actus - silentium) <= 1
            then if even (actus + silentium) then Actus else Silentium
            else if actus > silentium then Silentium else Actus
        PadraoFractal ->
            -- Padrão baseado na posição (fractal simples)
            let (x,y) = coord
                bit = (x `xor` y) .&. 1
            in if bit == 0 then Actus else Silentium
        LogicaParaconsistente ->
            -- Aceita contradição: ambos são possíveis, escolhe baseado em contexto
            case () of
                _ | actus == silentium ->
                    let total = actus + silentium
                    in if even total then Actus else Silentium
                  | otherwise ->
                    if actus > silentium then Actus else Silentium

-- ============================================================================
-- ALGORITMOS DE PROPAGAÇÃO
-- ============================================================================

-- | Propagação iterativa até convergência
propagar :: LogicaPreenchimento -> Vizinhanca -> Matriz -> Matriz
propagar logica viz mat =
    let preencherCelula coord cel =
            case cel of
                Just estado -> Just estado  -- Mantém
                Nothing ->
                    let novoEstado = aplicarLogica logica mat viz coord
                    in Just novoEstado

        novaMatriz = [ [ preencherCelula (i,j) cel
                       | (j, cel) <- zip [0..] linha ]
                     | (i, linha) <- zip [0..] mat ]

        -- Verifica se houve mudança
        mudou = any (any (==Nothing)) mat  -- Ainda tem células vazias?

    in if not mudou
       then novaMatriz
       else propagar logica viz novaMatriz

-- | Propagação com limite de iterações
propagarLimite :: Int -> LogicaPreenchimento -> Vizinhanca -> Matriz -> Matriz
propagarLimite 0 _ _ mat = mat
propagarLimite n logica viz mat =
    let preenchida = propagar logica viz mat
        vazias = length . filter (==Nothing) . concat $ preenchida
    in if vazias == 0
       then preenchida
       else propagarLimite (n-1) logica viz preenchida

-- ============================================================================
-- ANÁLISE E MÉTRICAS
-- ============================================================================

-- | Calcula métricas da matriz
analisarMatriz :: Matriz -> (Int, Int, Int, Double)
analisarMatriz mat =
    let flattened = concat mat
        total = length flattened
        actus = length $ filter (== Just Actus) flattened
        silentium = length $ filter (== Just Silentium) flattened
        vazios = total - actus - silentium
        densidade = if actus + silentium == 0
                    then 0.0
                    else fromIntegral actus / fromIntegral (actus + silentium)
    in (actus, silentium, vazios, densidade)

-- | Detecta padrões emergentes
padroesEmergentes :: Matriz -> [String]
padroesEmergentes mat =
    let linhas = map (map (fromMaybe '?' . fmap (head . show))) mat
        -- Padrões horizontais
        padroesH = concatMap (filter ((>2) . length) . group) linhas
        -- Padrões verticais
        colunas = transpose linhas
        padroesV = concatMap (filter ((>2) . length) . group) colunas
        -- Padrões diagonais (simplificado)
        diagonais = diagonaisMatriz mat
        padroesD = concatMap (filter ((>2) . length) . group) diagonais
    in nub $ map (take 10) (padroesH ++ padroesV ++ padroesD)

-- | Extrai diagonais da matriz
diagonaisMatriz :: Matriz -> [String]
diagonaisMatriz mat =
    let n = length mat
        m = length (head mat)
        todasCoords = [(i,j) | i <- [0..n-1], j <- [0..m-1]]
        grupoDiag1 = groupBy (\a b -> fst a - snd a == fst b - snd b)
                    $ sortBy (comparing (\(i,j) -> i - j)) todasCoords
        grupoDiag2 = groupBy (\a b -> fst a + snd a == fst b + snd b)
                    $ sortBy (comparing (\(i,j) -> i + j)) todasCoords
        extrair coords = map (\(i,j) -> fromMaybe '?' . fmap (head . show) $ mat!!i!!j) coords
    in map extrair (grupoDiag1 ++ grupoDiag2)

-- ============================================================================
-- RENDERIZAÇÃO AVANÇADA
-- ============================================================================

-- | Renderização com cores (ANSI)
renderColorido :: Matriz -> [String]
renderColorido mat =
    let linhaParaStr linha = concatMap renderCelula linha
        renderCelula = \case
            Just Actus     -> "\x1b[31m1\x1b[0m"  -- Vermelho
            Just Silentium -> "\x1b[34m0\x1b[0m"  -- Azul
            Nothing        -> "\x1b[90m·\x1b[0m"  -- Cinza
    in map linhaParaStr mat

-- | Renderização ASCII simples
renderASCII :: Matriz -> [String]
renderASCII = map (concatMap mostrar)
  where
    mostrar (Just Actus)     = "1"
    mostrar (Just Silentium) = "0"
    mostrar Nothing          = "·"

-- | Renderização com bordas
renderComBordas :: Matriz -> [String]
renderComBordas mat =
    let linhas = renderASCII mat
        largura = maximum (map length linhas)
        moldura = replicate (largura + 2) '-'
    in moldura : map (\l -> "|" ++ l ++ "|") linhas ++ [moldura]

-- ============================================================================
-- DEMONSTRAÇÃO INTERATIVA
-- ============================================================================

demonstrarSistema :: IO ()
demonstrarSistema = do
    putStrLn "╔══════════════════════════════════════════════════╗"
    putStrLn "║    SISTEMA DE PREENCHIMENTO TOPOLÓGICO v1.0     ║"
    putStrLn "╚══════════════════════════════════════════════════╝\n"

    putStrLn "📊 MATRIZ ORIGINAL (com vácuos estruturais):"
    mapM_ putStrLn (renderComBordas exemplo)

    let (a,s,v,d) = analisarMatriz exemplo
    putStrLn $ "\n📈 ESTATÍSTICAS:"
    putStrLn $ "  • Actus (1): " ++ show a
    putStrLn $ "  • Silentium (0): " ++ show s
    putStrLn $ "  • Vácuos (·): " ++ show v
    putStrLn $ "  • Densidade: " ++ show d

    putStrLn "\n🌀 TESTANDO DIFERENTES LÓGICAS DE PREENCHIMENTO:"

    let logicas = [MaioriaVizinhos, Minoritaria, XORVizinhos, EntropiaMaxima, PadraoFractal]
        vizinhanca = Moore

    forM_ logicas $ \logica -> do
        putStrLn $ "\n🔧 Lógica: " ++ show logica
        let resultado = propagarLimite 10 logica vizinhanca exemplo
        mapM_ putStrLn (renderComBordas resultado)

        let (a',s',v',d') = analisarMatriz resultado
        putStrLn $ "  Resultado: Actus=" ++ show a' ++
                   ", Silentium=" ++ show s' ++
                   ", Densidade=" ++ show d'

    -- Teste com vizinhança VonNeumann
    putStrLn "\n🔄 COMPARANDO VIZINHANÇAS (com lógica de maioria):"

    let vizinhancas = [VonNeumann, Moore, Hexagonal]

    forM_ vizinhancas $ \viz -> do
        putStrLn $ "\n📍 Vizinhanca: " ++ show viz
        let resultado = propagarLimite 10 MaioriaVizinhos viz exemplo
        mapM_ putStrLn (renderASCII resultado)

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

-- | Remove duplicados de uma lista (simples)

-- | Operações bitwise (simulação)
xor :: Int -> Int -> Int
xor x y = let x' = if odd x then 1 else 0
              y' = if odd y then 1 else 0
          in if x' /= y' then 1 else 0

infixl 4 .&.
(.&.) :: Int -> Int -> Int
x .&. y = if odd x && odd y then 1 else 0

-- ============================================================================
-- MAIN
-- ============================================================================

main :: IO ()
main = do
    putStrLn "\n🧩 INICIANDO SISTEMA DE PREENCHIMENTO TOPOLÓGICO 🧩\n"
    demonstrarSistema
    putStrLn "\n✨ ANÁLISE CONCLUÍDA ✨"
