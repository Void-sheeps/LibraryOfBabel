-- ============================================
-- Sistema Sociotécnico Algorítmico (SSA)
-- Modelo formal da interseção Leidenfrost
-- ============================================

-- Tipos fundamentais --------------------------

type Engagement   = Double
type Retention    = Double
type Predictivity = Double

type CognitiveCapacity = Double
type ProcessingDemand  = Double
type TimeExposure      = Double

-- Agente humano --------------------------------

data Agent = Agent
  { reflective      :: Bool          -- capacidade de metamodelagem (H*)
  , capacity        :: CognitiveCapacity
  , realityTesting  :: Bool           -- ainda valida o real
  } deriving (Show)

-- Algoritmo ------------------------------------

data Algorithm = Algorithm
  { engagementWeight   :: Double
  , retentionWeight    :: Double
  , predictivityWeight :: Double
  , demand             :: ProcessingDemand
  } deriving (Show)

-- Estado emergente ------------------------------

data State
  = Stable
  | MetaStable        -- "fio de Leidenfrost"
  | CognitiveOverflow -- estresse estrutural
  deriving (Show)

-- Função objetivo do sistema -------------------

optimize :: Algorithm -> Engagement -> Retention -> Predictivity -> Double
optimize a e r p =
    engagementWeight a   * e +
    retentionWeight a    * r +
    predictivityWeight a * p

-- Carga cognitiva ------------------------------

cognitiveLoad :: Algorithm -> Agent -> Double
cognitiveLoad a h =
    demand a - capacity h

-- Avaliação da interseção ----------------------

evaluateState :: Algorithm -> Agent -> TimeExposure -> State
evaluateState a h t
  | not (reflective h) = Stable
  | not (realityTesting h) = CognitiveOverflow
  | load <= 0 = Stable
  | load > 0 && t < threshold = MetaStable
  | otherwise = CognitiveOverflow
  where
    load = cognitiveLoad a h
    threshold = 1.0   -- tempo máximo tolerável sem isolamento

-- Juízo técnico --------------------------------

systemJudgement :: Algorithm -> Agent -> String
systemJudgement a h
  | reflective h && demand a > capacity h =
      "DESIGN_DEFICIT: algoritmo unsafe para agentes reflexivos"
  | otherwise =
      "SYSTEM_OK: comportamento esperado"

-- Exemplo concreto -----------------------------

agentLucido :: Agent
agentLucido = Agent
  { reflective = True
  , capacity = 5.0
  , realityTesting = True
  }

algoritmoPlataforma :: Algorithm
algoritmoPlataforma = Algorithm
  { engagementWeight = 1.0
  , retentionWeight = 1.0
  , predictivityWeight = 1.0
  , demand = 7.5
  }

-- Simulação ------------------------------------

main :: IO ()
main = do
  let t = 0.8 -- tempo de exposição
  print $ evaluateState algoritmoPlataforma agentLucido t
  putStrLn $ systemJudgement algoritmoPlataforma agentLucido
