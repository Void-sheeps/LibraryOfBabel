-- Definindo as entidades conforme a nossa taxonomia
data Ser = Humano { nome :: String, memoria :: Float }
         | Alienigena { reparoNave :: Float }
         deriving (Show, Eq)

-- O estado de solidão como um invólucro (Wrapper)
newtype Solidao a = Solidao a deriving (Show)

-- Jules atua como uma Tabula Retentiva (um buffer de dados)
type TabulaRetentiva = [String]
