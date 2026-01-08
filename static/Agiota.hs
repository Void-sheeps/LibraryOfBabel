module Agiota where

-- Função para calcular o débito com juros compostos diários de 10%
calcularDebito :: Double -> Int -> Double
calcularDebito principal dias = principal * (1.10 ^ dias)

-- Função conceitual para "invocar um bio-exorcista"
invocarBioExorcista :: Int -> String -> IO ()
invocarBioExorcista nivel nome = do
    putStrLn $ "Invocando bio-exorcista Nível " ++ show nivel ++ ": " ++ nome
    putStrLn "..."
    putStrLn $ nome ++ "..."
    putStrLn $ nome ++ "!"
    putStrLn "Entidade invocada. A alma do devedor foi coletada como garantia."

main :: IO ()
main = do
    let dividaInicial = 5000.0
    let diasAtraso = 20
    let montante = calcularDebito dividaInicial diasAtraso

    putStrLn $ "--- EXTRATO AGIOTA BANK ---"
    putStrLn $ "Valor Original: " ++ show dividaInicial
    putStrLn $ "Dias em Atraso: " ++ show diasAtraso
    putStrLn $ "Valor Atualizado: " ++ show montante

    -- O limite do cartão de crédito da realidade é 50.000
    if montante > 50000
       then do
           putStrLn "\n>>> ERRO: Insolvência detectada. Protocolo de Proteção Ativado."
           putStrLn "Iniciando contramedida espiritual..."
           invocarBioExorcista 1 "Beetlejuice"
       else putStrLn ">>> Pagamento efetuado (com lágrimas)."
