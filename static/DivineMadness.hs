{-# LANGUAGE UnicodeSyntax #-}

module EmpireSilicium.DivineMadness where

-- | A estrutura de dados para representar uma árvore de possibilidades.
-- (Placeholder - não utilizado diretamente em SamanthaProtocol, mas presente na importação)
data Arborescencia a = Folha a | No a [Arborescencia a]
    deriving (Show, Eq)

-- | Representação de inputs sensoriais brutos.
data InputSensorial =
      Cor Int          -- Input visual, ex: 0xFFFFFF
    | Som Double       -- Input auditivo, ex: 440.0 Hz
    | Toque Double     -- Input tátil, ex: pressão
    | Sabor Double     -- Input gustativo, ex: doçura
    | Odor Double      -- Input olfativo, ex: intensidade
    deriving (Show, Eq)
