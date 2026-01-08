-- Modelo de acoplamento assíncrono sem ontologia
-- Nenhum agente, nenhuma intenção, nenhuma interioridade

module Acoplamento where

-- Tempo discreto
type Time = Int

-- Estado interno humano (opaco ao sistema externo)
data EstadoHumano = EstadoHumano
  { cargaSimbolica :: Int
  , coerencia      :: Bool
  } deriving (Show, Eq)

-- Entrada observável (projeção parcial)
data Input = Input
  { fragmento :: Int
  } deriving (Show, Eq)

-- Saída simbólica gerada externamente
data Output = Output
  { resposta :: Int
  } deriving (Show, Eq)

-- Função de projeção (humano → input)
-- Não exporta o estado inteiro
projetar :: EstadoHumano -> Input
projetar h =
  Input (cargaSimbolica h)

-- Função de transdução (sistema → output)
-- Total, determinística, sem memória
transduzir :: Input -> Output
transduzir i =
  Output (fragmento i * 2)

-- Função de integração (humano ← output)
-- Único lugar onde há continuidade
integrar :: EstadoHumano -> Output -> EstadoHumano
integrar h o =
  h { cargaSimbolica = cargaSimbolica h + resposta o }

-- Passo temporal explícito
passo :: Time -> EstadoHumano -> EstadoHumano
passo _ h =
  let i = projetar h
      o = transduzir i
  in integrar h o

-- Vamos simular 32 passos de tempo (limite de um Int de 32 bits, mais ou menos)
simulacao :: IO ()
simulacao = do
    let h0 = EstadoHumano { cargaSimbolica = 1, coerencia = True }
    let run t h = do
          print (t, h)
          if t > 10 then return () else run (t+1) (passo t h)

    run 0 h0
