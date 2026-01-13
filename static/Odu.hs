{-# LANGUAGE DeriveGeneric #-}

module Odu where

import GHC.Generics (Generic)

-- | Representação de um bit do Ifá (I ou II)
data BitIfa = I | II
  deriving (Show, Eq, Ord, Generic)

-- | Uma coluna de 4 bits
type Coluna = (BitIfa, BitIfa, BitIfa, BitIfa)

-- | Um Odù é composto por duas colunas
data Odu = Odu Coluna Coluna
  deriving (Show, Eq, Ord, Generic)

-- | Mapear um Odù para seu nome tradicional
nomeOdu :: Odu -> String
nomeOdu (Odu (I,I,I,I) (I,I,I,I)) = "Ejiogbe"
nomeOdu (Odu (II,II,II,II) (II,II,II,II)) = "Oyeku Meji"
nomeOdu (Odu (I,II,II,I) (I,II,II,I)) = "Iwori Meji"
nomeOdu (Odu (II,I,I,II) (II,I,I,II)) = "Odi Meji"
nomeOdu (Odu (I,I,II,II) (I,I,II,II)) = "Irosun Meji"
nomeOdu (Odu (II,II,I,I) (II,II,I,I)) = "Owonrin Meji"
nomeOdu (Odu (I,II,I,II) (I,II,I,II)) = "O'bara Meji"
nomeOdu (Odu (II,I,II,I) (II,I,II,I)) = "Okanran Meji"
nomeOdu (Odu (I,I,I,II) (I,I,I,II)) = "Ogunda Meji"
nomeOdu (Odu (II,I,I,I) (II,I,I,I)) = "Osa Meji"
nomeOdu (Odu (I,II,II,II) (I,II,II,II)) = "Ika Meji"
nomeOdu (Odu (II,II,II,I) (II,II,II,I)) = "Oturupon Meji"
nomeOdu (Odu (II,I,II,II) (II,I,II,II)) = "Otura Meji"
nomeOdu (Odu (II,II,I,II) (II,II,I,II)) = "Irete Meji"
nomeOdu (Odu (I,I,II,I) (I,I,II,I)) = "Ose Meji"
nomeOdu (Odu (I,II,I,I) (I,II,I,I)) = "Ofun Meji"
nomeOdu _ = "Odù Misto"
