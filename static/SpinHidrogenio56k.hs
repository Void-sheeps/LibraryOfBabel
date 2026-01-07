-- |
-- Module      : SpinHidrogenio56k
-- Description : Simulação da linha de 56k como proxy para a linha de hidrogênio
-- Principle   : A linha de 21cm (1420 MHz) do hidrogênio como canal de comunicação cósmica
--               é aqui representada pela "linha de 56k" da internet discada.
-- |

module Main where

import Text.Printf (printf)

-- | Representa a qualidade do sinal da "linha de 56k"
-- | Um valor entre 0.0 (ruído puro) e 1.0 (sinal perfeito)
type QualidadeSinal = Double

-- | Simula a detecção de um sinal na "linha de 56k"
detectarSinal :: QualidadeSinal -> String
detectarSinal q
    | q > 0.8   = "Sinal de ALTA QUALIDADE detectado. Possível análogo a um sinal de 'technosignature'."
    | q > 0.5   = "Sinal de MÉDIA QUALIDADE. Análogo a um ruído cósmico com padrões."
    | q > 0.2   = "Sinal de BAIXA QUALIDADE. Apenas ruído de fundo, análogo à radiação cósmica de fundo."
    | otherwise = "NENHUM SINAL detectado. Silêncio cósmico."

-- | Função principal que simula a escuta
main :: IO ()
main = do
    let qualidadeObservada = 0.85 -- Valor simulado para demonstração

    putStrLn "=== PROJETO 'COSMIC DIAL-UP' ==="
    putStrLn "Escutando a 'linha de 56k' em busca de análogos a sinais cósmicos..."
    printf "Qualidade do sinal detectado: %.2f\n" qualidadeObservada
    putStrLn ""

    let interpretacao = detectarSinal qualidadeObservada
    putStrLn "[INTERPRETAÇÃO DO SINAL]"
    putStrLn interpretacao
    putStrLn ""
    putStrLn "[SIGNIFICADO]: Assim como a linha de 56k era um portal para um novo mundo de informações, a linha de hidrogênio de 21cm pode ser nosso portal para o cosmos. Este projeto busca essa analogia."
