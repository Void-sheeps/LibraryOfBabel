-- SpinHidrogenio56k.hs
-- Mapeia o ângulo de spin de um elétron de hidrogênio para um sinal analógico de modem.
-- Conceito: A "assinatura" fundamental de um sistema como sua forma mais simples de comunicação.

module Main where

import System.Random

-- O ângulo de spin do elétron de hidrogênio (um valor entre 0 e 2*pi)
type SpinAngle = Double

-- A "qualidade" do sinal de modem correspondente
data SignalQuality = RuidoBranco | SinalFraco | Handshake56k | Technosignature deriving (Show, Eq)

-- Gera um ângulo de spin aleatório
randomSpinAngle :: IO SpinAngle
randomSpinAngle = randomRIO (0.0, 2 * pi)

-- Mapeia o ângulo de spin para a qualidade do sinal
-- A "technosignature" está em uma faixa muito estreita, análoga à linha de 21cm do hidrogênio.
angleToSignal :: SpinAngle -> SignalQuality
angleToSignal angle
    -- A "linha de hidrogênio" metafórica: uma faixa muito específica
    | abs (angle - pi) < 0.01 = Technosignature
    -- Uma faixa um pouco mais ampla para um handshake bem-sucedido
    | abs (angle - pi) < 0.2  = Handshake56k
    -- Uma faixa ainda mais ampla para um sinal reconhecível, mas fraco
    | abs (angle - pi) < 0.8  = SinalFraco
    -- Todo o resto é ruído
    | otherwise               = RuidoBranco

-- Simula a observação do sinal
observeSignal :: IO ()
observeSignal = do
    angle <- randomSpinAngle
    let quality = angleToSignal angle
    putStrLn $ "Ângulo de Spin detectado: " ++ show angle
    putStrLn $ "Qualidade do Sinal: " ++ show quality
    if quality == Technosignature
        then putStrLn "[CONCLUSÃO] Assinatura tecnológica inequívoca. Origem: Desconhecida."
        else putStrLn "[CONCLUSÃO] Nenhuma assinatura anômala detectada."

main :: IO ()
main = do
    putStrLn "--- INICIANDO PROTOCOLO DE OBSERVAÇÃO 56K/H ---"
    putStrLn "Varrendo o espectro em busca de assinaturas de spin..."
    observeSignal
