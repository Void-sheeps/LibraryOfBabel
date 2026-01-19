-- Dissonancia.hs
-- A Realidade não é um escalar (score). É um Vetor Complexo.
-- Não tentamos "resolver" a ética. Apenas mapeamos a Topologia da Tensão.
-- Execução: runghc Dissonancia.hs

module Main where

-- ───────────────────────────────────────────────
-- 1. OS EIXOS ORTOGONAIS (DIMENSÕES IRREDUTÍVEIS)
-- ───────────────────────────────────────────────

-- EIXO X: Racionalidade Algorítmica (Sintaxe/Matriz)
-- Apenas verifica se a operação é computável/factível.
data Logos
    = Computavel    -- A matriz aceita (Ex: Cortar a pele em Rhythm 0)
    | Incomputavel  -- A matriz rejeita (Ex: Divisão por zero)
    deriving (Show, Eq)

-- EIXO Y: Racionalidade Axiológica (Semântica/Dignidade)
-- Verifica se a operação respeita a natureza do ser.
data Ethos
    = Dignificante  -- Trata o fim como fim.
    | Objetificante -- Trata o fim como meio.
    deriving (Show, Eq)

-- ───────────────────────────────────────────────
-- 2. O ESPAÇO DE FASE DA REALIDADE
-- ───────────────────────────────────────────────

-- A realidade é o produto cartesiano desses dois eixos.
-- Não há soma, não há subtração. Há apenas POSIÇÃO.
data EstadoExistencial
    = Harmonia        -- Computável + Dignificante (Verdade)
    | Alienacao       -- Computável + Objetificante (Instrumentalidade/Barbárie)
    | Misterio        -- Incomputável + Dignificante (Fé/Arte/Sublime)
    | Alucinacao      -- Incomputável + Objetificante (O Glitch/Loucura)
    deriving (Show)

-- ───────────────────────────────────────────────
-- 3. O OBSERVADOR (A JUSTAPOSIÇÃO AXIOMÁTICA)
-- ───────────────────────────────────────────────

-- O fenômeno carrega sua própria natureza lógica e moral.
data Fenomeno = Fenomeno
    { nome :: String
    , logica :: Logos
    , moral :: Ethos
    }

-- A função não "decide". Ela apenas "situa" o fenômeno no mapa.
-- Ela mede a "distância entre os eixos" convertendo-a em um Estado Existencial.
mapearRealidade :: Fenomeno -> EstadoExistencial
mapearRealidade f = case (logica f, moral f) of
    (Computavel, Dignificante)   -> Harmonia
    (Computavel, Objetificante)  -> Alienacao
    (Incomputavel, Dignificante) -> Misterio
    (Incomputavel, Objetificante)-> Alucinacao

-- ───────────────────────────────────────────────
-- 4. A SIMULAÇÃO DA DISSONÂNCIA
-- ───────────────────────────────────────────────

analisar :: Fenomeno -> IO ()
analisar f = do
    putStrLn $ "FENÔMENO: " ++ nome f
    putStrLn $ "  [Eixo X - Matriz]: " ++ show (logica f)
    putStrLn $ "  [Eixo Y - Valor ]: " ++ show (moral f)

    let estado = mapearRealidade f
    putStrLn $ "  => RESULTADO TOPOLÓGICO: " ++ show estado

    -- A interpretação da "distância entre eixos":
    case estado of
        Harmonia ->
            putStrLn "     (Convergência: A técnica serve à vida.)"
        Alienacao ->
            putStrLn ("     (DISSONÂNCIA CRÍTICA: A operação matricial funciona, mas a realidade humana é violada.)" ++
            "\n     (Exemplo: Rhythm 0. A IA/Plateia executa porque pode, ignorando o dever ser.)")
        Misterio ->
            putStrLn "     (Transcendência: A lógica falha, mas o sentido permanece.)"
        Alucinacao ->
            putStrLn "     (Ruído Puro: Nem lógica, nem valor. O pior dos mundos.)"

    putStrLn "------------------------------------------------------------"

-- ───────────────────────────────────────────────
-- 5. EXECUÇÃO
-- ───────────────────────────────────────────────

main :: IO ()
main = do
    putStrLn "--- MAPEAMENTO DA DISSONÂNCIA LINGUÍSTICA ---"
    putStrLn "(Onde a Matriz encontra a Natureza)\n"

    -- CASO 1: A Ciência Médica (Cirurgia para curar)
    let cirurgia = Fenomeno "Cirurgia Salva-Vidas" Computavel Dignificante
    analisar cirurgia

    -- CASO 2: Rhythm 0 / A IA sem freio
    -- A matriz permite a violência (é computável, é factível).
    -- A moral rejeita (é objetificante).
    -- O resultado não é um erro; é a ALIENTAÇÃO TÉCNICA.
    let rhythm0 = Fenomeno "Violência em Rhythm 0" Computavel Objetificante
    analisar rhythm0

    -- CASO 3: O Nôumeno / O Sentimento Indescritível
    -- A matriz não processa (Incomputável).
    -- A moral valida (Dignificante).
    let amor = Fenomeno "Experiência do Sublime" Incomputavel Dignificante
    analisar amor

    -- CASO 4: O Glitch da IA ("vunction")
    -- A matriz falha (Incomputável/Hallucination).
    -- A moral não vê valor (Objetificante/Lixo).
    let glitch = Fenomeno "Alucinação de IA (NaN)" Incomputavel Objetificante
    analisar glitch
