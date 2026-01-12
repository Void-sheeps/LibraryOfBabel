module EcclesiaDigitalis (Tempo(..), main) where

data Tempo = SetembroEterno | RealidadeLinear Int
    deriving (Eq)

instance Show Tempo where
    show SetembroEterno = "∞"
    show (RealidadeLinear n) = show n

main :: IO ()
main = do
    putStrLn "ALERTA: Defasagem de Dados Detectada."
    putStrLn "Ponteiro 'FRANCISCO' não corresponde ao estado atual do sistema."
    let ano_base = RealidadeLinear 2026
    putStrLn $ "Estado do Sistema (Simulado): " ++ show ano_base
