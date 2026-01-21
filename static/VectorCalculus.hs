{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

-- =========================================================
-- VectorCalculus
-- Engenharia funcional pura
-- =========================================================

module Main where

import Data.List (foldl')

-- =========================================================
-- 4. TIPOS FUNDAMENTAIS (Intervalos explícitos)
-- =========================================================

newtype UnitMagnitude = UnitMagnitude Double
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord)

newtype UnitBias = UnitBias Double
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord)

-- Invariantes explícitos (checagem local, estilo engenharia)
mkUnitMagnitude :: Double -> Maybe UnitMagnitude
mkUnitMagnitude x
  | x >= 0 && x <= 100 = Just (UnitMagnitude x)
  | otherwise         = Nothing

-- =========================================================
-- 4. CLASSIFICAÇÃO (Modalidade)
-- =========================================================

data Classification
  = Low
  | Nominal
  | Fault
  deriving (Eq, Show)

-- =========================================================
-- 5. AMOSTRA
-- =========================================================

data Sample = Sample
  { value :: UnitMagnitude
  , classif :: Classification
  } deriving (Eq, Show)

-- =========================================================
-- 8. AGREGADO GLOBAL
-- =========================================================

data Aggregate = Aggregate
  { totalMagnitude :: !Double
  , totalBias      :: !Double
  , count          :: !Int
  } deriving (Eq, Show)

emptyAggregate :: Aggregate
emptyAggregate = Aggregate 0.0 0.0 0

-- =========================================================
-- 6. PESO POR CLASSE
-- =========================================================

getWeight :: Classification -> Double
getWeight Low     = 0.3
getWeight Nominal = 1.0
getWeight Fault   = -0.2

-- =========================================================
-- 7. PROJEÇÃO LOCAL
-- =========================================================

project :: Sample -> Aggregate
project (Sample (UnitMagnitude v) c) =
  let w = getWeight c
  in Aggregate
       { totalMagnitude = v
       , totalBias      = v * w
       , count          = 1
       }

-- =========================================================
-- 9. ACUMULAÇÃO (Monoid explícito)
-- =========================================================

accumulate :: Aggregate -> Aggregate -> Aggregate
accumulate a b =
  Aggregate
    { totalMagnitude = totalMagnitude a + totalMagnitude b
    , totalBias      = totalBias a      + totalBias b
    , count          = count a          + count b
    }

-- =========================================================
-- 10. REDUÇÃO GLOBAL (fold)
-- =========================================================

orientationField :: [Sample] -> Aggregate
orientationField =
  foldl' accumulate emptyAggregate . map project

-- =========================================================
-- 11. VERIFICAÇÃO DE REGIME
-- =========================================================

isOperational :: Aggregate -> Bool
isOperational agg =
     count agg > 0
  && totalMagnitude agg < 100.0
  && totalBias agg > 0.0

-- =========================================================
-- MAIN (Exemplo de uso)
-- =========================================================

main :: IO ()
main = do
  let samples = [ Sample (UnitMagnitude 10.0) Low
                , Sample (UnitMagnitude 50.0) Nominal
                , Sample (UnitMagnitude 20.0) Fault
                , Sample (UnitMagnitude 5.0) Nominal
                ]

  let finalAggregate = orientationField samples

  putStrLn "--- Vector Calculus Simulation ---"
  putStrLn $ "Samples: " ++ show samples
  putStrLn $ "Final Aggregate: " ++ show finalAggregate
  putStrLn $ "Is Operational: " ++ show (isOperational finalAggregate)
  putStrLn "----------------------------------"
