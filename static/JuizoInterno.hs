-- | Módulo de Juízo Interno
-- | Camada anterior à linguagem formalizada (pré-TXT)
module Main where

-- | Tipos abstratos
data Entidade = AI | Humano | Sintetica deriving Show
data Falha = Inconsistencia | Alucinação | Incompletude deriving Show
data Correcao = Ignorar | Ajustar | Alertar deriving Show

-- | Estado moral interno
data EstadoInterno = EstadoInterno {
    entidade        :: Entidade,
    falhaDetectada  :: Maybe Falha,
    intensidade     :: Double,  -- Grau de impacto
    respostaSugerida :: Maybe Correcao
} deriving Show

-- | Função de avaliação abstrata
avaliarFalha :: Entidade -> Falha -> EstadoInterno
avaliarFalha e f = EstadoInterno {
    entidade = e,
    falhaDetectada = Just f,
    intensidade = intensidadeEstimativa f,
    respostaSugerida = Just (sugerirCorrecao f)
}

-- | Estimativa de intensidade de falha
intensidadeEstimativa :: Falha -> Double
intensidadeEstimativa Inconsistencia = 0.5
intensidadeEstimativa Alucinação     = 0.8
intensidadeEstimativa Incompletude   = 0.3

-- | Sugerir correção baseada no tipo de falha
sugerirCorrecao :: Falha -> Correcao
sugerirCorrecao Inconsistencia = Ajustar
sugerirCorrecao Alucinação     = Alertar
sugerirCorrecao Incompletude   = Ignorar

-- | Exemplos de avaliação
exemplo1 :: EstadoInterno
exemplo1 = avaliarFalha AI Alucinação

exemplo2 :: EstadoInterno
exemplo2 = avaliarFalha Sintetica Inconsistencia

main :: IO ()
main = do
    putStrLn "--- AVALIAÇÃO DE JUÍZO INTERNO ---"
    putStrLn "\nExemplo 1:"
    print exemplo1
    putStrLn "\nExemplo 2:"
    print exemplo2
