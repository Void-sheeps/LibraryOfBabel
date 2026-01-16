{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import Data.List (intercalate, unfoldr)
import System.Random
import Control.Monad.State
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (isSpace)
import Text.Printf (printf)

-- ============================================
-- GEMINI MNEMOSYNIS · ANÁLISE ESTRUTURAL AVANÇADA
-- ============================================

data Arvore a
  = Nula
  | No a (Arvore a) (Arvore a)
  deriving (Show, Eq, Functor)

-- Estado do monitor com histórico temporal
data MonitorEstrutural a = MonitorEstrutural
  { arvoreAtual     :: Arvore a
  , historico       :: [(Int, Arvore a)]  -- (ciclo, snapshot)
  , metricas        :: MetricasEstruturais
  , limiares        :: LimiaresAlerta
  } deriving (Show)

-- Métricas estruturais quantitativas
data MetricasEstruturais = MetricasEstruturais
  { profundidade      :: Int
  , tamanho           :: Int
  , densidade         :: Double    -- tamanho / 2^profundidade
  , fatorBalanceamento :: Double   -- 0-1 (1 = perfeitamente balanceada)
  , entropiaEstrutural :: Double   -- entropia de Shannon da estrutura
  , assimetria        :: Double    -- diferença entre lados (-1 a 1)
  } deriving (Show)

-- Limiares para detecção de anomalias
data LimiaresAlerta = LimiaresAlerta
  { limiarDensidade     :: Double  -- abaixo disso = esparsa
  , limiarBalanceamento :: Double  -- abaixo disso = desbalanceada
  , limiarEntropia      :: Double  -- abaixo disso = padrão repetitivo
  } deriving (Show)

-- ============================================
-- CÁLCULOS ESTRUTURAIS AVANÇADOS
-- ============================================

-- Funções básicas do código original
profundidadeArv :: Arvore a -> Int
profundidadeArv Nula = 0
profundidadeArv (No _ esq dir) =
  1 + max (profundidadeArv esq) (profundidadeArv dir)

tamanhoArv :: Arvore a -> Int
tamanhoArv Nula = 0
tamanhoArv (No _ esq dir) =
  1 + tamanhoArv esq + tamanhoArv dir

log2Seguro :: Int -> Double
log2Seguro n
  | n <= 0    = 0.0
  | otherwise = logBase 2 (fromIntegral n)

-- Cálculo de densidade (completude da árvore)
calcularDensidade :: Arvore a -> Double
calcularDensidade arv =
  let n = tamanhoArv arv
      p = profundidadeArv arv
      capacidade = 2 ^ p - 1  -- capacidade máxima para essa profundidade
  in if capacidade > 0
        then fromIntegral n / fromIntegral capacidade
        else 1.0

-- Fator de balanceamento (0 a 1, onde 1 = perfeitamente balanceada)
calcularFatorBalanceamento :: Arvore a -> Double
calcularFatorBalanceamento Nula = 1.0
calcularFatorBalanceamento arv =
  let calcularAlturaBalanceada :: Arvore a -> Int
      calcularAlturaBalanceada Nula = 0
      calcularAlturaBalanceada (No _ esq dir) =
        1 + min (calcularAlturaBalanceada esq) (calcularAlturaBalanceada dir)

      hMin = calcularAlturaBalanceada arv
      hMax = profundidadeArv arv
  in if hMax > 0
        then fromIntegral hMin / fromIntegral hMax
        else 1.0

-- Entropia estrutural (mede complexidade/irregularidade)
calcularEntropiaEstrutural :: Arvore a -> Double
calcularEntropiaEstrutural arv =
  let contagens = contarPadroes arv
      total = fromIntegral (sum contagens)
      probabilidades = if total == 0 then [] else map (\c -> fromIntegral c / total) contagens
      entropia = -sum [p * logBase 2 p | p <- probabilidades, p > 0]
  in entropia
  where
    contarPadroes :: Arvore a -> [Int]
    contarPadroes arv = M.elems (go arv)
      where
        go :: Arvore a -> M.Map Int Int
        go Nula = M.empty
        go (No _ esq dir) =
          let pattern = case (esq, dir) of
                          (Nula, Nula) -> 0 -- leaf
                          (_, Nula)    -> 1 -- left only
                          (Nula, _)    -> 2 -- right only
                          (_, _)       -> 3 -- both children
              l_counts = go esq
              r_counts = go dir
          in M.insertWith (+) pattern 1 (M.unionWith (+) l_counts r_counts)

-- Assimetria (-1 = toda à esquerda, 0 = balanceado, 1 = toda à direita)
calcularAssimetria :: Arvore a -> Double
calcularAssimetria arv =
  let tamanhoLado :: Arvore a -> Int
      tamanhoLado Nula = 0
      tamanhoLado (No _ esq dir) = 1 + tamanhoLado esq + tamanhoLado dir

      esq = tamanhoLado (esquerda arv)
      dir = tamanhoLado (direita arv)
      total = esq + dir
  in if total > 0
        then fromIntegral (dir - esq) / fromIntegral total
        else 0.0
  where
    esquerda (No _ e _) = e
    esquerda _ = Nula
    direita (No _ _ d) = d
    direita _ = Nula

-- Fold para árvores binárias
foldArvore :: (a -> b -> b -> b) -> b -> Arvore a -> b
foldArvore _ z Nula = z
foldArvore f z (No x esq dir) = f x (foldArvore f z esq) (foldArvore f z dir)

-- ============================================
-- DETECTOR AVANÇADO DE ANOMALIAS
-- ============================================

data EstadoEstrutural
  = Estavel
  | SuspeitoDesbalanceamento
  | Esparsa
  | Repetitiva
  | Assimetrica
  | Critica
  deriving (Show, Eq, Enum)

-- Detecção multi-critério
avaliarEstruturaAvancada :: Arvore a -> LimiaresAlerta -> (EstadoEstrutural, [String])
avaliarEstruturaAvancada arv limiares =
  let p = profundidadeArv arv
      n = tamanhoArv arv
      dens = calcularDensidade arv
      bal = calcularFatorBalanceamento arv
      ent = calcularEntropiaEstrutural arv
      assim = calcularAssimetria arv

      -- Critérios individuais
      criterios = catMaybes
        [ if fromIntegral p > 2.5 * log2Seguro n
            then Just "Profundidade excessiva em relação ao conteúdo"
          else Nothing

        , if dens < limiarDensidade limiares
            then Just ("Densidade baixa: " ++ show dens)
          else Nothing

        , if bal < limiarBalanceamento limiares
            then Just ("Balanceamento insuficiente: " ++ show bal)
          else Nothing

        , if ent < limiarEntropia limiares
            then Just ("Padrão repetitivo detectado: " ++ show ent)
          else Nothing

        , if abs assim > 0.7
            then Just ("Assimetria extrema: " ++ show assim)
          else Nothing
        ]

      -- Determinar estado principal
      estadoPrincipal
        | not (null criterios) && bal < 0.3 = Critica
        | not (null criterios) && dens < 0.2 = Esparsa
        | not (null criterios) && ent < 0.5 = Repetitiva
        | not (null criterios) && abs assim > 0.7 = Assimetrica
        | not (null criterios) = SuspeitoDesbalanceamento
        | otherwise = Estavel

  in (estadoPrincipal, criterios)

-- ============================================
-- VISUALIZAÇÃO ESTRUTURAL
-- ============================================

-- Visualização ASCII da árvore
visualizarArvore :: Show a => Arvore a -> String
visualizarArvore = unlines . visualizar
  where
    visualizar Nula = ["Ø"]
    visualizar (No x esq dir) =
      let (esqLinhas, dirLinhas) = (visualizar esq, visualizar dir)
          altura = max (length esqLinhas) (length dirLinhas)
          esqPreenchido = esqLinhas ++ replicate (altura - length esqLinhas) " "
          dirPreenchido = dirLinhas ++ replicate (altura - length dirLinhas) " "

          linhaAtual = show x
          espacamento = replicate (length linhaAtual `div` 2) ' '

          conexoes = zipWith (\e d ->
            let padE = if null e then " " else e
                padD = if null d then " " else d
            in padE ++ espacamento ++ " " ++ espacamento ++ padD
            ) esqPreenchido dirPreenchido
      in linhaAtual : conexoes

-- Visualização compacta
visualizarCompacta :: Show a => Arvore a -> String
visualizarCompacta Nula = "•"
visualizarCompacta (No x esq dir) =
  "(" ++ visualizarCompacta esq ++ " " ++
  show x ++ " " ++
  visualizarCompacta dir ++ ")"

-- ============================================
-- MONITORAMENTO TEMPORAL
-- ============================================

-- Inicializar monitor
inicializarMonitor :: Arvore a -> LimiaresAlerta -> MonitorEstrutural a
inicializarMonitor arv lim =
  let met = MetricasEstruturais
        { profundidade = profundidadeArv arv
        , tamanho = tamanhoArv arv
        , densidade = calcularDensidade arv
        , fatorBalanceamento = calcularFatorBalanceamento arv
        , entropiaEstrutural = calcularEntropiaEstrutural arv
        , assimetria = calcularAssimetria arv
        }
  in MonitorEstrutural arv [] met lim

-- Atualizar monitor com nova árvore
atualizarMonitor :: Int -> Arvore a -> MonitorEstrutural a -> MonitorEstrutural a
atualizarMonitor ciclo novaArv monitor =
  let novaMet = MetricasEstruturais
        { profundidade = profundidadeArv novaArv
        , tamanho = tamanhoArv novaArv
        , densidade = calcularDensidade novaArv
        , fatorBalanceamento = calcularFatorBalanceamento novaArv
        , entropiaEstrutural = calcularEntropiaEstrutural novaArv
        , assimetria = calcularAssimetria novaArv
        }
      novoHistorico = take 10 $ (ciclo, arvoreAtual monitor) : historico monitor
  in monitor
      { arvoreAtual = novaArv
      , historico = novoHistorico
      , metricas = novaMet
      }

-- ============================================
-- RELATÓRIO ANALÍTICO
-- ============================================

gerarRelatorio :: Show a => MonitorEstrutural a -> String
gerarRelatorio monitor =
  let met = metricas monitor
      lim = limiares monitor
      (estado, alertas) = avaliarEstruturaAvancada (arvoreAtual monitor) lim

      cabecalho = "╔" ++ replicate 58 '═' ++ "╗\n" ++
                  "║            RELATÓRIO DE ANÁLISE ESTRUTURAL           ║\n" ++
                  "╠" ++ replicate 58 '═' ++ "╣"

      secaoEstrutura =
        "║ ESTRUTURA ATUAL:                                              ║\n" ++
        "║   " ++ padRight 55 (visualizarCompacta (arvoreAtual monitor)) ++ "║\n" ++
        "║   Profundidade: " ++ padLeft 40 (show (profundidade met)) ++ "║\n" ++
        "║   Tamanho (nós): " ++ padLeft 39 (show (tamanho met)) ++ "║\n" ++
        "║   Densidade: " ++ padLeft 43 (printf "%.3f" (densidade met)) ++ "║\n" ++
        "║   Balanceamento: " ++ padLeft 39 (printf "%.3f" (fatorBalanceamento met)) ++ "║\n" ++
        "║   Entropia: " ++ padLeft 45 (printf "%.3f" (entropiaEstrutural met)) ++ "║\n" ++
        "║   Assimetria: " ++ padLeft 43 (printf "%.3f" (assimetria met)) ++ "║"

      secaoAvaliacao =
        "\n║ AVALIAÇÃO:                                                    ║\n" ++
        "║   Estado: " ++ padLeft 47 (show estado) ++ "║\n" ++
        "║   Alertas: " ++ padLeft 47 (show (length alertas)) ++ "║\n" ++
        if null alertas
          then "║                                                              ║"
          else "║   " ++ padRight 55 (head alertas) ++ "║\n" ++
               concatMap (\a -> "║   • " ++ padRight 53 a ++ "║\n") (tail alertas)

      secaoLimiares =
        "\n║ LIMIARES:                                                     ║\n" ++
        "║   Densidade mínima: " ++ padLeft 37 (printf "%.2f" (limiarDensidade lim)) ++ "║\n" ++
        "║   Balanceamento mínimo: " ++ padLeft 33 (printf "%.2f" (limiarBalanceamento lim)) ++ "║\n" ++
        "║   Entropia mínima: " ++ padLeft 37 (printf "%.2f" (limiarEntropia lim)) ++ "║"

      rodape = "╚" ++ replicate 58 '═' ++ "╝"

  in unlines [cabecalho, secaoEstrutura, secaoAvaliacao, secaoLimiares, rodape]
  where
    padRight n s = s ++ replicate (n - length s) ' '
    padLeft n s = replicate (n - length s) ' ' ++ s

-- ============================================
-- EXEMPLOS E DEMONSTRAÇÃO
-- ============================================

-- Exemplo 1: Árvore balanceada
arvoreBalanceada :: Arvore Int
arvoreBalanceada =
  No 1
    (No 2
      (No 4 Nula Nula)
      (No 5 Nula Nula))
    (No 3
      (No 6 Nula Nula)
      (No 7 Nula Nula))

-- Exemplo 2: Árvore degenerada (lista encadeada)
arvoreDegenerada :: Arvore Int
arvoreDegenerada =
  No 1
    (No 2
      (No 3
        (No 4
          (No 5 Nula Nula)
          Nula)
        Nula)
      Nula)
    Nula

-- Exemplo 3: Árvore esparsa
arvoreEsparsa :: Arvore Int
arvoreEsparsa =
  No 1
    (No 2 Nula Nula)
    (No 3
      Nula
      (No 4
        Nula
        (No 5 Nula Nula)))

-- Limiares padrão
limiaresPadrao :: LimiaresAlerta
limiaresPadrao = LimiaresAlerta
  { limiarDensidade = 0.3
  , limiarBalanceamento = 0.6
  , limiarEntropia = 1.0
  }

-- ============================================
-- FUNÇÃO PRINCIPAL
-- ============================================

main :: IO ()
main = do
  putStrLn "\n🧠 GEMINI MNEMOSYNIS · MONITORAMENTO ESTRUTURAL AVANÇADO"
  putStrLn ("=" ++ replicate 60 '=' ++ "\n")

  -- Testar árvore balanceada
  putStrLn "TESTE 1: ÁRVORE BALANCEADA"
  putStrLn $ gerarRelatorio $ inicializarMonitor arvoreBalanceada limiaresPadrao

  putStrLn ("\n" ++ replicate 62 '=' ++ "\n")

  -- Testar árvore degenerada
  putStrLn "TESTE 2: ÁRVORE DEGENERADA (LISTA ENCADEADA)"
  putStrLn $ gerarRelatorio $ inicializarMonitor arvoreDegenerada limiaresPadrao

  putStrLn ("\n" ++ replicate 62 '=' ++ "\n")

  -- Testar árvore esparsa
  putStrLn "TESTE 3: ÁRVORE ESPARSA"
  putStrLn $ gerarRelatorio $ inicializarMonitor arvoreEsparsa limiaresPadrao

  -- Visualização gráfica
  putStrLn "\nVISUALIZAÇÃO DAS ÁRVORES:\n"

  putStrLn "1. Balanceada:"
  putStrLn $ visualizarArvore arvoreBalanceada

  putStrLn "\n2. Degenerada:"
  putStrLn $ visualizarArvore arvoreDegenerada

  putStrLn "\n3. Esparsa:"
  putStrLn $ visualizarArvore arvoreEsparsa

-- ============================================
-- FUNÇÕES AUXILIARES
-- ============================================

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of Just v -> v:acc; Nothing -> acc) []

instance Semigroup MetricasEstruturais where
  a <> b = MetricasEstruturais
    { profundidade = max (profundidade a) (profundidade b)
    , tamanho = tamanho a + tamanho b
    , densidade = (densidade a + densidade b) / 2
    , fatorBalanceamento = min (fatorBalanceamento a) (fatorBalanceamento b)
    , entropiaEstrutural = (entropiaEstrutural a + entropiaEstrutural b) / 2
    , assimetria = (assimetria a + assimetria b) / 2
    }

instance Monoid MetricasEstruturais where
  mempty = MetricasEstruturais 0 0 0.0 1.0 0.0 0.0
