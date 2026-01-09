-- Axioma Mnemosynis 2026: Implementação Tomista
module Main where

-- Definição das condições necessárias (Ratio Sine Qualia)
data Autoridade = Legitima | Ilegitima deriving (Show, Eq)
data Causa      = Justa | Injusta       deriving (Show, Eq)
data Intencao   = Reta | Maliciosa      deriving (Show, Eq)

-- O Estado de Conflito como um registro técnico
data StatusConflito = GuerraJusta | AtoDeViolencia | NaturaleSilentium
    deriving (Show, Eq)

-- Função de Validação Taxonômica (Inter-legere)
verificarCasusBelli :: Autoridade -> Causa -> Intencao -> StatusConflito
verificarCasusBelli Legitima Justa Reta = GuerraJusta
verificarCasusBelli _ _ _               = AtoDeViolencia

-- Simulação de um Cenário de Marcha
main :: IO ()
main = do
    let autoridade = Legitima
    let causa      = Justa
    let intencao   = Reta

    let veredito = verificarCasusBelli autoridade causa intencao

    putStrLn "--- Análise de Proporcionalidade Tomista ---"
    putStrLn $ "Resultado do Processamento: " ++ show veredito
