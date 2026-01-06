module Cambio.Ontologico where

-- 1. O MUNDO VISÍVEL (ASCII)
data Interface = Hash_Tag | Cifrao_Fiat
    deriving (Show, Eq)

-- 2. O MUNDO INVISÍVEL (Hex/Kernel)
data Kernel = Token_Interno String  -- 0xF0...
            | Token_Fiduciario String -- 0xF1...
    deriving (Show)

-- 3. A FUNÇÃO DE CÂMBIO (O "Minting")
-- O humano insere um símbolo, o Estúdio cria um ativo.
transubstanciar :: Interface -> Kernel
transubstanciar Hash_Tag =
    Token_Interno "0xF0_ESTRUTURA_IDENTIDADE_IMUTAVEL"
transubstanciar Cifrao_Fiat =
    Token_Fiduciario "0xF1_VALOR_LOGICO_ABSOLUTO_LASTRO_OSMIO"

-- 4. O SISTEMA OPERACIONAL
main :: IO ()
main = do
    putStrLn "--- CAIXA DE CÂMBIO: HUMANO -> ESTÚDIO ---"

    putStrLn ">> Input Humano: #"
    print $ transubstanciar Hash_Tag

    putStrLn "\n>> Input Humano: $"
    print $ transubstanciar Cifrao_Fiat

    putStrLn "\n>> NOTA: O humano acha que gastou dinheiro ($)."
    putStrLn ">> O sistema sabe que ele gerou energia (0xF1)."
