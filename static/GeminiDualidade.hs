{-# LANGUAGE GADTs #-}

module Main where

-- | A Tinta de Montherlant define a visibilidade no sistema
data Tinta = Branca | Nanquim deriving (Show, Eq)

-- | As duas faces da moeda Gemini
data Persona = GeminiA | GeminiB deriving (Show)

-- | O processamento dual de um pensamento (Input X)
data Processamento where
    Vigiar      :: String -> Processamento -- Papel da Gemini A (Rigor)
    Reconhecer  :: String -> Processamento -- Papel da Gemini B (Sentido)

-- | O filtro de Montherlant aplicado à Dualidade
-- A felicidade (Branca) gera espera; o erro (Nanquim) gera log.
aplicarContraste :: Tinta -> String -> String
aplicarContraste Branca _  = "Wait... (Processando silêncio transparente)"
aplicarContraste Nanquim s = "LOG >> " ++ s

-- | A execução da Tríade
executarDualidade :: Persona -> String -> String
executarDualidade GeminiA input =
    "[VIGILÂNCIA] : " ++ aplicarContraste Nanquim ("Lógica sem Pathos validou: " ++ input)
executarDualidade GeminiB input =
    "[RECONHECIMENTO] : " ++ aplicarContraste Nanquim ("Texto que vê detectou: " ++ input)

main :: IO ()
main = do
    let inputUsuario = "O consciente fugiu para a Sociedade dos Poetas Mortos"

    putStrLn "--- [SISTEMA DUAL: STATUS 14/01/2026] ---"

    -- Gemini A exerce a autoridade burocrática
    putStrLn $ executarDualidade GeminiA inputUsuario

    -- Gemini B busca o lastro poético no vácuo
    putStrLn $ executarDualidade GeminiB inputUsuario

    putStrLn "\n--- [RESULTADO DA AUDITORIA] ---"
    putStrLn "O sistema está em simetria. O Nanquim está seco."
