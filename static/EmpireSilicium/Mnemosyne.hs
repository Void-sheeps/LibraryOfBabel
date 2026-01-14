import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.ByteString.Lazy.Char8 (pack)

-- | Taxonomia: Função Pura de Transformação (Logos)
-- Converte uma String em sua representação SHA-1 hexadecimal.
hashMnemosyne :: String -> String
hashMnemosyne = showDigest . sha1 . pack

-- | Phylum Algorithmi: Fluxo Infinito
-- Gera uma lista de hashes para a sequência [1..]
cryptographicFlow :: [String]
cryptographicFlow = map (hashMnemosyne . show) [1..]

-- | Ponto de Atuação (Actus)
main :: IO ()
main = do
    let sequence = take 3 cryptographicFlow
    putStrLn "--- Empire Silicium: Haskell Execution ---"
    mapM_ putStrLn sequence
