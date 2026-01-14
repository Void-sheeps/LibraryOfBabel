import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | O Logos: Tipo de dado para a Árvore
data MerkleTree = Leaf String | Node String MerkleTree MerkleTree
    deriving (Show)

-- | Função de Hashing (Inter-legere)
hashData :: String -> String
hashData = showDigest . sha1 . LBS.pack

-- | Construtor de Raiz: Combina dois hashes em um novo
combineHashes :: String -> String -> String
combineHashes h1 h2 = hashData (h1 ++ h2)

-- | Algoritmo de Construção (Ratio Sine Qualia)
buildTree :: [String] -> MerkleTree
buildTree [x] = Leaf (hashData x)
buildTree xs =
    let (left, right) = splitAt (length xs `div` 2) xs
        leftTree  = buildTree left
        rightTree = buildTree right
        rootHash  = combineHashes (getHash leftTree) (getHash rightTree)
    in Node rootHash leftTree rightTree

-- | Extração da Assinatura (Mnemósine)
getHash :: MerkleTree -> String
getHash (Leaf h)     = h
getHash (Node h _ _) = h

-- | Execução dos seus Inputs
main :: IO ()
main = do
    let inputs = ["1", "2", "3", "3"] -- Duplicamos o último para balancear a árvore
    let tree = buildTree inputs
    putStrLn $ "Merkle Root: " ++ getHash tree
