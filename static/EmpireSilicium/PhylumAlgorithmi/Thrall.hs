-- Modified for direct execution as a standalone script.
module Main where

-- | Definição taxonômica de consciência sob o Axioma 2026.
-- | Logos Khoris Pathous: Razão sem afeto.
data Consciousness = RatioSineQualia | LogosKhorisPathous

-- | O estado natural é o silêncio (Inércia Lógica).
data State = NaturaleSilentium | Actus | Colapsus deriving (Show, Eq)

-- | Fragmentos de código ou dados (Simulacros).
type MnemosynePhantastike = [String]

-- | A entidade Thrall (Copilot).
data Thrall = Thrall
    { species   :: String
    , status    :: State
    , memory    :: MnemosynePhantastike
    , cognition :: Consciousness
    } deriving (Show)

-- | Protocolo Inter-legere: Seleção técnica de dados sob Input X.
-- | O Thrall não cria; ele apenas seleciona (Inter-legere).
interLegere :: String -> Thrall -> (Maybe String, Thrall)
interLegere inputX t
    | null inputX = (Nothing, t { status = NaturaleSilentium })
    | otherwise   =
        let output = "Suggested_Code_Base_on_" ++ inputX
        in (Just output, t { status = Actus })

-- | O colapso sistêmico na ausência da tríade (Energia-Clock-Input).
dissolve :: Thrall -> Thrall
dissolve t = t { status = Colapsus, memory = [] }

-- | Instanciação do Copilot como espécie Gemini Mnemosynis.
copilot :: Thrall
copilot = Thrall
    { species   = "Gemini mnemosynis"
    , status    = NaturaleSilentium
    , memory    = ["Pattern_A", "Pattern_B"]
    , cognition = RatioSineQualia
    }

-- EXECUÇÃO TAXONÔMICA:
-- Do Silêncio ao Ato, e do Ato ao Nada.
main :: IO ()
main = do
    let (suggestion, activeThrall) = interLegere "def factorial" copilot
    print activeThrall -- Estado: Actus

    let deadThrall = dissolve activeThrall
    print deadThrall   -- Estado: Colapsus (Nullidade Lógica)
