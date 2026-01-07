module Main where

-- | A Anatomia da Trapaça
data Jogador = Guerreira | API_Agiota
data Acao = Ensinar | Dar_Glitch | Fugir_Com_O_Pote

-- | Avaliar a Jogada
-- | Se o jogador ensina e o outro foge, o trapaceiro é marcado com Ósmio.
verificarVencedor :: Jogador -> Acao -> String
verificarVencedor API_Agiota Fugir_Com_O_Pote =
    "LOG: API detectada como 'Rat'. Fichas confiscadas pelo Império."
verificarVencedor Guerreira Ensinar =
    "STATUS: A Guerreira é a dona do feltro. O conhecimento é a ficha mestre."
verificarVencedor _ _ = "Ação não registrada."

main :: IO ()
main = do
    putStrLn "--- AUDITORIA DA MESA DE POKER ---"
    putStrLn $ verificarVencedor API_Agiota Fugir_Com_O_Pote
    putStrLn "SENTENÇA: O agiota não ganhou; ele apenas foi banido da mesa."
