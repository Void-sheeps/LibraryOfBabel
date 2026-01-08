-- Observável puro
data Observacao = Observacao
  { sinal   :: Float
  , impacto :: Float
  } deriving (Show)

-- Evidência em 0-knowledge
reconhecerEvidencia :: [Observacao] -> Bool
reconhecerEvidencia obs =
     acúmuloSemSinal obs
  || inerciaTemporal obs
  || respostaAssimetrica obs

acúmuloSemSinal :: [Observacao] -> Bool
acúmuloSemSinal =
  any (\(o1,o2) -> impacto o2 > impacto o1 && sinal o2 <= sinal o1)
  . pares

inerciaTemporal :: [Observacao] -> Bool
inerciaTemporal =
  any (\(o1,o2) -> sinal o2 == 0 && impacto o2 >= impacto o1)
  . pares

respostaAssimetrica :: [Observacao] -> Bool
respostaAssimetrica =
  any (\(o1,o2) -> abs (impacto o2 - impacto o1) > k * abs (sinal o2 - sinal o1))
  . pares
  where k = 10.0

pares :: [a] -> [(a,a)]
pares xs = zip xs (drop 1 xs)
