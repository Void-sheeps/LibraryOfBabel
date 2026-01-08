{-
  Baralhos.hs - Modelagem formal de baralhos como sistemas lógicos incompatíveis
  Nenhuma semântica mística, apenas estrutura

  Teorema: Cada baralho define sua própria álgebra de valores.
  Não há função de conversão bem-tipada entre diferentes baralhos.
-}

module Baralhos where

import Data.List (intercalate)
import Control.Monad (replicateM)

-- ===============================
-- TIPOS FUNDAMENTAIS (ALGEBRAS DIFERENTES)
-- ===============================

-- | Força Contextual (Truco)
newtype ForcaContextual = FC Int
  deriving (Show, Eq, Ord)

-- | Probabilidade (Poker)
newtype Probabilidade = P Double
  deriving (Show, Eq)

-- | Limiar (Blackjack)
newtype Limiar = L Int
  deriving (Show, Eq, Ord)

-- | Símbolo (Baralho Cigano)
data Simbolo = Paus | Copas | Espadas | Ouros | Lira | Estrela | Lua | Sol
  deriving (Show, Eq, Enum, Bounded)

-- | Arquétipo (Tarô)
data Arquetipo
  = OLouco      -- 0
  | OMago       -- I
  | ASacerdotisa -- II
  | AImperatriz -- III
  | OImperador  -- IV
  | OHierofante -- V
  | OEnamorados -- VI
  | OCarro      -- VII
  | AJustica    -- VIII
  | OEremita    -- IX
  | ARodaDaFortuna -- X
  | AForca      -- XI
  | OEnforcado  -- XII
  | AMorte      -- XIII (sem nome)
  | ATemperanca -- XIV
  | ODemonio    -- XV
  | ATorre      -- XVI
  | AEstrela    -- XVII
  | ALua        -- XVIII
  | OSol        -- XIX
  | OJulgamento -- XX
  | OMundo      -- XXI
  deriving (Show, Eq, Enum, Bounded)

-- ===============================
-- BARALHO DE TRUCO (ÁLGEBRA CONTEXTUAL)
-- ===============================

-- | Contextos que modificam a força
data ContextoTruco = Mao | Grito | Manilha | Flor | Envido
  deriving (Show, Eq, Enum, Bounded)

-- | Carta de Truco com força contextual
data TrucoCarta = TrucoCarta
  { faceTruco :: String
  , naipeTruco :: Simbolo
  , forcaBase :: ForcaContextual
  , contextosAtivos :: [ContextoTruco]
  } deriving (Show, Eq)

-- | Avaliar força considerando contextos
avaliarTruco :: TrucoCarta -> ForcaContextual
avaliarTruco carta =
  let FC base = forcaBase carta
      modificadores = sum (map valorContexto (contextosAtivos carta))
  in FC (base + modificadores)

valorContexto :: ContextoTruco -> Int
valorContexto Mao     = 0
valorContexto Grito   = 3
valorContexto Manilha = 5
valorContexto Flor    = 2
valorContexto Envido  = 4

-- | Criar baralho completo de Truco
baralhoTruco :: [TrucoCarta]
baralhoTruco =
  [ TrucoCarta nome naipe (FC forca) []
  | (nome, forca) <- [("4", 1), ("5", 2), ("6", 3), ("7", 4), ("10", 5),
                      ("11", 6), ("12", 7), ("1", 8), ("2", 9), ("3", 10)]
  , naipe <- [Paus, Copas, Espadas, Ouros]
  ]

-- ===============================
-- BARALHO DE POKER (ÁLGEBRA PROBABILÍSTICA)
-- ===============================

-- | Valor de carta no Poker
data ValorPoker = Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove
                | Dez | Valete | Dama | Rei | As
  deriving (Show, Eq, Ord, Enum)

-- | Carta de Poker
data PokerCarta = PokerCarta
  { valorPoker :: ValorPoker
  , naipePoker :: Simbolo
  } deriving (Show, Eq)

-- | Mão de Poker
type MaoPoker = [PokerCarta]

-- | Avaliar probabilidade da mão (simplificado)
avaliarPoker :: MaoPoker -> Probabilidade
avaliarPoker mao =
  let valor = fromIntegral (somaValores mao)
      maximo = fromIntegral (5 * fromEnum (maxBound :: ValorPoker))
  in P (valor / maximo)

somaValores :: MaoPoker -> Int
somaValores = sum . map (fromEnum . valorPoker)

-- | Criar baralho completo de Poker
baralhoPoker :: [PokerCarta]
baralhoPoker =
  [ PokerCarta valor naipe
  | valor <- [Dois .. As]
  , naipe <- [Paus, Copas, Espadas, Ouros]
  ]

-- ===============================
-- BARALHO DE BLACKJACK (ÁLGEBRA ADITIVA)
-- ===============================

-- | Carta de Blackjack com valor flexível
data BJCard = BJ
  { valorBJ :: Int
  , nomeBJ  :: String
  , flexivel :: Bool  -- True para Ás (pode valer 1 ou 11)
  } deriving (Show, Eq)

-- | Somar mão considerando flexibilidade do Ás
somarBJ :: [BJCard] -> Limiar
somarBJ cartas =
  let somaSimples = sum (map valorBJ cartas)
      numAses = length (filter flexivel cartas)
      -- Ajustar valor dos Ases
      somaAjustada = ajustarAses somaSimples numAses
  in L somaAjustada

ajustarAses :: Int -> Int -> Int
ajustarAses soma 0 = soma
ajustarAses soma nAses
  | soma + 10 <= 21 && nAses > 0 = ajustarAses (soma + 10) (nAses - 1)
  | otherwise = soma

-- | Estado da mão
estadoBJ :: [BJCard] -> ResultadoBJ
estadoBJ cartas =
  let L total = somarBJ cartas
  in case total of
      x | x == 21  -> Blackjack
        | x > 21   -> Estourou
        | otherwise -> EmJogo

data ResultadoBJ = EmJogo | Blackjack | Estourou
  deriving (Show, Eq, Ord)

-- | Criar baralho completo de Blackjack
baralhoBlackjack :: [BJCard]
baralhoBlackjack =
  [ BJ (min 10 (n + 1)) (show (n + 1)) (n == 0) | n <- [0..8] ] ++  -- 1-9
  [ BJ 10 "10" False ] ++
  [ BJ 10 "Valete" False, BJ 10 "Dama" False, BJ 10 "Rei" False ] ++
  [ BJ 1 "Ás" True ]
  >>= replicate 4  -- 4 naipes

-- ===============================
-- BARALHO CIGANO (ÁLGEBRA SEMIÓTICA)
-- ===============================

-- | Carta Cigana com interpretação simbólica
data CartaCigana = CartaCigana
  { nomeCigano :: String
  , simboloCigano :: Simbolo
  , significadoDireto :: String
  , significadoInvertido :: String
  } deriving (Show, Eq)

-- | Interpretação baseada em posição
interpretarCigano :: [CartaCigana] -> Narrativa
interpretarCigano cartas =
  let simbolos = map simboloCigano cartas
      nomes = map nomeCigano cartas
      significados = case length cartas of
        1 -> [significadoDireto (head cartas)]
        3 -> ["Passado: " ++ significadoDireto (cartas !! 0),
              "Presente: " ++ significadoDireto (cartas !! 1),
              "Futuro: " ++ significadoDireto (cartas !! 2)]
        _ -> map significadoDireto cartas
  in Narrativa significados simbolos

newtype Narrativa = Narrativa ([String], [Simbolo])
  deriving (Show)

-- ===============================
-- TARÔ (ÁLGEBRA ARQUETÍPICA)
-- ===============================

-- | Carta de Tarô
data TarotCarta
  = ArcanoMaior Arquetipo
  | ArcanoMenor Simbolo Int  -- Naipe + número (1-10) ou figura
  deriving (Show, Eq)

-- | Estado existencial modificado pelo Tarô
data EstadoExistencial = EstadoExistencial
  { consciencia :: Double
  , sombra :: Double
  , integracao :: Double
  , narrativaPessoal :: String
  } deriving (Show, Eq)

-- | Transição do estado através do Tarô
transicaoTarot :: TarotCarta -> EstadoExistencial -> EstadoExistencial
transicaoTarot (ArcanoMaior arqu) estado =
  case arqu of
    OLouco      -> estado { consciencia = consciencia estado - 0.1 }
    OMago       -> estado { consciencia = consciencia estado + 0.2 }
    AMorte      -> estado { sombra = sombra estado + 0.3 }
    OMundo      -> estado { integracao = integracao estado + 0.4 }
    _           -> estado
transicaoTarot (ArcanoMenor naipe num) estado =
  estado { narrativaPessoal = narrativaPessoal estado ++ " [" ++ show naipe ++ " " ++ show num ++ "]" }

-- ===============================
-- INCOMPATIBILIDADE FORMAL - ERROS DE TIPO
-- ===============================

{-
-- ERRO 1: Tentar somar cartas de baralhos diferentes
somarTudo :: TrucoCarta -> PokerCarta -> BJCard -> Limiar
somarTrito cartaT cartaP cartaBJ =  -- Nomes intencionalmente diferentes
  let forcaT = avaliarTruco cartaT  -- ForcaContextual
      probP = avaliarPoker [cartaP] -- Probabilidade
      limiarBJ = somarBJ [cartaBJ]  -- Limiar
  -- Como somar ForcaContextual + Probabilidade + Limiar?
  -- Não há função definida: erro de tipo
  in ???

-- ERRO 2: Usar naipe de Poker em Truco
usarNaipeErrado :: PokerCarta -> TrucoCarta
usarNaipeErrado cartaP =
  TrucoCarta
    { faceTruco = show (valorPoker cartaP)
    , naipeTruco = naipePoker cartaP  -- Mesmo tipo Simbolo?
    , forcaBase = FC 0
    , contextosAtivos = []
    }
-- Isto compila porque ambos usam Simbolo, mas é semanticamente errado
-- O sistema de tipos não captura este erro semântico
-}

-- ===============================
-- DEMONSTRAÇÃO DAS INCOMPATIBILIDADES
-- ===============================

demonstrarIncompatibilidade :: IO ()
demonstrarIncompatibilidade = do
  putStrLn "=== BARALHOS: SISTEMAS LÓGICOS INCOMPATÍVEIS ==="
  putStrLn ""

  putStrLn "1. BARALHO DE TRUCO (Álgebra Contextual):"
  let cartaTruco = TrucoCarta "4" Paus (FC 4) [Manilha, Grito]
  putStrLn $ "   Carta: " ++ show cartaTruco
  putStrLn $ "   Força avaliada: " ++ show (avaliarTruco cartaTruco)
  putStrLn ""

  putStrLn "2. BARALHO DE POKER (Álgebra Probabilística):"
  let cartaPoker = PokerCarta Valete Espadas
      maoPoker = [cartaPoker, PokerCarta Dama Espadas, PokerCarta Rei Espadas]
  putStrLn $ "   Mão: " ++ show maoPoker
  putStrLn $ "   Probabilidade: " ++ show (avaliarPoker maoPoker)
  putStrLn ""

  putStrLn "3. BARALHO DE BLACKJACK (Álgebra Aditiva):"
  let cartasBJ = [BJ 10 "10" False, BJ 1 "Ás" True]
  putStrLn $ "   Cartas: " ++ show cartasBJ
  putStrLn $ "   Soma: " ++ show (somarBJ cartasBJ)
  putStrLn $ "   Estado: " ++ show (estadoBJ cartasBJ)
  putStrLn ""

  putStrLn "4. BARALHO CIGANO (Álgebra Semiótica):"
  let cartasCiganas =
        [ CartaCigana "Cavaleiro" Paus "Ação" "Impulsividade"
        , CartaCigana "Rainha" Copas "Amor" "Ciúme"
        ]
  putStrLn $ "   Interpretação: " ++ show (interpretarCigano cartasCiganas)
  putStrLn ""

  putStrLn "5. TARÔ (Álgebra Arquetípica):"
  let estadoInicial = EstadoExistencial 0.5 0.2 0.3 "Início"
      cartaTarot = ArcanoMaior AMorte
      novoEstado = transicaoTarot cartaTarot estadoInicial
  putStrLn $ "   Estado inicial: " ++ show estadoInicial
  putStrLn $ "   Após " ++ show cartaTarot ++ ": " ++ show novoEstado
  putStrLn ""

  putStrLn "=== INCOMPATIBILIDADES FORMUAIS ==="
  putStrLn ""
  putStrLn "Não há função bem-tipada que converta:"
  putStrLn "  • ForcaContextual → Probabilidade"
  putStrLn "  • Limiar → Simbolo"
  putStrLn "  • Arquetipo → ValorBJ"
  putStrLn ""
  putStrLn "Cada baralho define sua própria álgebra."
  putStrLn "Operações entre álgebras diferentes são mal-tipadas."
  putStrLn ""
  putStrLn "=== TEOREMA ==="
  putStrLn "Sejam A, B dois baralhos com álgebras diferentes."
  putStrLn "Não existe f : A → B que preserve as operações internas."
  putStrLn "(Há perda de informação ou introdução de arbitrariedade)"

-- ===============================
-- FUNÇÃO PRINCIPAL
-- ===============================

main :: IO ()
main = demonstrarIncompatibilidade

{-
  COMENTÁRIO FILOSÓFICO:

  Cada baralho não é apenas um conjunto de cartas, mas um SISTEMA FORMAL completo:

  1. TRUCO: Álgebra contextual - o valor depende do jogo em curso, dos gritos, das manilhas.
  2. POKER: Álgebra probabilística - valor estatístico, não absoluto.
  3. BLACKJACK: Álgebra aditiva - soma simples com regra especial para o Ás.
  4. BARALHO CIGANO: Álgebra semiótica - significado depende de contexto e posição.
  5. TARÔ: Álgebra arquetípica - transformação de estados existenciais.

  A incompatibilidade não é técnica, mas ontológica:
  • O que é "valor" em um baralho não é "valor" em outro.
  • O que é "soma" em Blackjack não é "soma" em Truco.
  • O que é "interpretação" no Tarô não é "probabilidade" no Poker.

  Misturar baralhos é como misturar geometrias euclidianas e não-euclidianas:
  possível formalmente, mas semanticamente incoerente.

  O sistema de tipos Haskell captura esta incompatibilidade ontológica:
  se não há função bem-tipada, não há conversão coerente.
-}
