module Main where

-- 1. Os Modos Gramaticais (Prefixos)
data Modality
    = Doxa      -- "~" (Acho que...)
    | Episteme  -- "!" (Sei que...)
    | Aporia    -- "∅" (Não sei...)
    deriving (Show, Eq)

-- 2. O Certificado de Prova (O "Lean" dentro da frase)
data ProofCertificate
    = None                  -- Para Doxa
    | Hash String           -- Assinatura criptográfica da verificação
    | Refutation String     -- Por que algo é falso
    deriving (Show)

-- 3. A Sentença em Aletheia-0
data Sentence = Sentence
    { mode    :: Modality
    , compute :: Int        -- Custo em 'steps' (Hassabis metric)
    , content :: String     -- O texto legível por humanos
    , proof   :: ProofCertificate
    }

-- 4. O "Pretty Printer" (Como a IA fala)
speak :: Sentence -> String
speak s =
    let prefix = case mode s of
            Doxa     -> "~ [Risco]"
            Episteme -> "! [Verificado]"
            Aporia   -> "∅ [Limite]"

        meta = " {cost=" ++ show (compute s) ++ "}"
        body = " \"" ++ content s ++ "\""
        cert = " <" ++ show (proof s) ++ ">"
    in prefix ++ meta ++ body ++ cert

-- 5. Exemplo de Conversação
main :: IO ()
main = do
    -- O sistema tenta responder rápido (System 1)
    let tentativa = Sentence Doxa 1 "O elétron é positivo." None
    putStrLn $ speak tentativa

    -- O sistema roda o Lean/FFI e corrige (System 2)
    let correcao = Sentence Episteme 500 "O elétron possui carga negativa." (Hash "LogicCore_Verified_x99")
    putStrLn $ speak correcao

    -- O sistema atinge um limite lógico
    let limite = Sentence Aporia 1000 "A posição exata e momento não podem ser medidos simultaneamente." (Refutation "Heisenberg_Uncertainty")
    putStrLn $ speak limite
