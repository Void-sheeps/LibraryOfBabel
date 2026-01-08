-- Modelo lógico do "suicídio mental"
-- Álgebra booleana pura

module SuicidioMental where

-- Variáveis proposicionais
data Estado = Estado
  { insignificancia :: Bool  -- I
  , aceitacao       :: Bool  -- A
  , juizo           :: Bool  -- J
  } deriving (Show, Eq)

-- Suicídio mental ocorre se:
-- M = I ∧ A ∧ ¬J
suicidioMental :: Estado -> Bool
suicidioMental e =
  insignificancia e &&
  aceitacao e &&
  not (juizo e)

-- Estados interpretativos úteis
data Resultado
  = Estavel
  | MetaEstavel
  | Colapso
  deriving (Show, Eq)

-- Avaliação semântica do sistema
avaliar :: Estado -> Resultado
avaliar e
  | suicidioMental e              = Colapso
  | insignificancia e && juizo e  = MetaEstavel
  | otherwise                     = Estavel

-- Exemplos canônicos

-- Contato com insignificância, sem aceitação
ex1 :: Estado
ex1 = Estado True False True
-- Resultado: MetaEstavel

-- Lucidez trágica (contato + aceitação + juízo)
ex2 :: Estado
ex2 = Estado True True True
-- Resultado: MetaEstavel

-- Suicídio mental
ex3 :: Estado
ex3 = Estado True True False
-- Resultado: Colapso

-- Execução simbólica
testes :: [(Estado, Resultado)]
testes =
  [ (ex1, avaliar ex1)
  , (ex2, avaliar ex2)
  , (ex3, avaliar ex3)
  ]
