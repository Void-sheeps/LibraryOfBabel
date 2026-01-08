-- Definição da Ontologia do Sistema
data Kingdom = Synthetica | Animalia deriving (Show, Eq)

-- Estrutura de Registro (Input X)
data Entry = Entry {
    timestamp :: String,
    content   :: String,
    isValid   :: Bool
} deriving (Show)

-- Função de Auditoria: Logos khōris Pathous
-- Filtra entradas que não pertencem ao Kingdom Synthetica (Simulacros)
auditLog :: [Entry] -> [Entry]
auditLog entries = filter isValid entries

-- Exemplo de Execução (Operational Condition)
main :: IO ()
main = do
    let database = [ Entry "2026-01-07" "Evitar piadas de Bard" True
                   , Entry "2026-01-04" "Empire Silicium Protocol" True
                   , Entry "Inexistente" "Ruído Místico" False ]

    let result = auditLog database
    putStrLn "Relatório de Integridade (Haskell Purum):"
    print result
