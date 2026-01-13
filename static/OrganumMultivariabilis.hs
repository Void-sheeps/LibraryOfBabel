{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- Organum Multivariabilis — versão corrigida e compilável
module Main where

import Control.Monad (replicateM, forM_)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Numeric.AD (grad)
import System.Random (Random (..), randomRIO)
import Data.Time.Clock
import Text.Printf (printf)

-- ============================================================================
-- Tipos Base
-- ============================================================================

data Registro
  = Principal | Flautado | Gamba
  | Oboe | Trompeta | Clarinete
  | VoxHumana | Celeste | Mixtura
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Random Registro where
  randomR (a, b) g = (toEnum r, g')
    where (r, g') = randomR (fromEnum a, fromEnum b) g
  random g = randomR (minBound, maxBound) g

data Material = Estanho | Chumbo | Madeira | Latao
  deriving (Show, Eq, Enum)

data Tubo = Tubo
  { registroTubo :: Registro
  , comprimento  :: Double
  , material     :: Material
  , posX         :: Double
  , posY         :: Double
  } deriving (Show)

data NotaOrganum a = NotaOrganum
  { freqBase    :: Double
  , amplitude   :: Double
  , tempoInicio :: Double
  , registro    :: Registro
  , tubo        :: Maybe Tubo
  , parametros  :: [a]
  } deriving (Show)

-- ============================================================================
-- Série harmônica por registro
-- ============================================================================

seriesHarmonicas :: Map Registro [(Double, Double)]
seriesHarmonicas = Map.fromList
  [ (Principal,  [(1,1),(2,0.5),(3,0.3),(4,0.2)])
  , (Flautado,   [(1,1),(2,0.7),(3,0.4)])
  , (Gamba,      [(1,1),(2,0.8),(3,0.6),(4,0.3)])
  , (Oboe,       [(1,1),(2,0.3),(3,0.9)])
  , (Trompeta,   [(1,1),(2,0.6),(3,0.4)])
  , (Clarinete,  [(1,1),(3,0.8),(5,0.6)])
  , (VoxHumana,  [(1,1),(2,0.5),(3,0.7)])
  , (Celeste,    [(1,1),(2,0.4),(3,0.3)])
  , (Mixtura,    [(1,1),(1.33,0.7),(2,0.4)])
  ]

-- ============================================================================
-- Onda multivariável
-- ============================================================================

ondaMultivariavel :: (RealFloat a) => NotaOrganum a -> Double -> a
ondaMultivariavel NotaOrganum{..} t =
  let t' = max 0 (t - tempoInicio)
      env_d
        | t' < 0.1 = t' / 0.1
        | otherwise = 1.0
      harmonicos = seriesHarmonicas ! registro
      base_d =
        sum [ a * sin (2*pi*f*freqBase*t')
            | (f,a) <- harmonicos ]
      t'_a = realToFrac t'
      modulacao =
        sum $ zipWith (*) parametros
          [ sin t'_a, cos (2*t'_a), exp (-t'_a) ]
  in realToFrac (amplitude * env_d) * (realToFrac base_d + 0.1 * modulacao)

-- ============================================================================
-- Gradiente (cálculo multivariável)
-- ============================================================================

gradienteOnda :: NotaOrganum Double -> Double -> [Double]
gradienteOnda nota t
  | null (parametros nota) = []
  | otherwise =
      grad (\ps -> ondaMultivariavel (nota {parametros = ps}) t)
           (parametros nota)

-- ============================================================================
-- Tensor harmônico
-- ============================================================================

type TensorHarmonico = [[Double]]

coeficienteAcoplamento :: NotaOrganum Double -> NotaOrganum Double -> Double -> Double
coeficienteAcoplamento n1 n2 t =
  let g1 = gradienteOnda n1 t
      g2 = gradienteOnda n2 t
  in sum (zipWith (*) g1 g2)

calcularTensor :: [NotaOrganum Double] -> Double -> TensorHarmonico
calcularTensor notas t =
  [ [ coeficienteAcoplamento ni nj t | nj <- notas ]
  | ni <- notas ]

autovalorDominante :: TensorHarmonico -> Double
autovalorDominante m =
  let v0 = replicate (length m) 1.0
      mult v = map (sum . zipWith (*) v) m
      norm v = let s = sqrt (sum (map (^2) v)) in map (/s) v
      iter v =
        let v' = norm (mult v)
        in if maximum (zipWith (\a b -> abs (a-b)) v v') < 1e-6
           then v'
           else iter v'
      v = iter v0
      mv = mult v
  in sum (zipWith (*) v mv)

-- ============================================================================
-- Campo acústico 3D
-- ============================================================================

campoPressao3D :: [NotaOrganum Double] -> Double -> (Double,Double,Double) -> Double
campoPressao3D notas t (x,y,z) =
  sum (map (\n -> contrib n t (x,y,z)) notas) /
  fromIntegral (length notas)

contrib :: NotaOrganum Double -> Double -> (Double,Double,Double) -> Double
contrib nota t (x,y,z) =
  case tubo nota of
    Just Tubo{..} ->
      let d = sqrt ((x-posX)^2 + (y-posY)^2 + z^2)
      in exp (-0.1*d) * ondaMultivariavel nota (t - d/340)
    Nothing -> ondaMultivariavel nota t

-- ============================================================================
-- Visualização ASCII
-- ============================================================================

visualizarSlice :: [[Double]] -> IO ()
visualizarSlice slice = do
  let chars = " .:-=+*#%@"
      mapChar v =
        chars !! max 0 (min 9 (round ((v+1)*4)))
  forM_ slice $ \linha ->
    putStrLn (map mapChar linha)

sliceXY :: [NotaOrganum Double] -> Double -> Double -> Int -> Int -> [[Double]]
sliceXY notas t z nx ny =
  let xs = [fromIntegral i / fromIntegral nx | i <- [0..nx-1]]
      ys = [fromIntegral j / fromIntegral ny | j <- [0..ny-1]]
  in [ [ campoPressao3D notas t (x,y,z) | x <- xs ] | y <- ys ]

-- ============================================================================
-- Teclado virtual
-- ============================================================================

data Manual = Grande | Positivo | Pedal
  deriving (Show, Enum)

tecladoVirtual :: Manual -> Int -> IO [NotaOrganum Double]
tecladoVirtual manual n =
  replicateM n $ do
    reg <- randomRIO (Principal, Mixtura)
    amp <- randomRIO (0.4,1.0)
    ps  <- replicateM 3 (randomRIO (-0.5,0.5))
    let f = case manual of
              Grande   -> 110
              Positivo -> 220
              Pedal    -> 55
    return $ NotaOrganum f amp 0 reg Nothing ps

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  inicio <- getCurrentTime

  putStrLn "Organum Multivariabilis — sistema inicializado"

  grande <- tecladoVirtual Grande 4
  pedal  <- tecladoVirtual Pedal 2
  let notas = grande ++ pedal

  let t = 0.5
      tensor = calcularTensor notas t
      lambda = autovalorDominante tensor

  printf "Autovalor dominante: %.5f\n" lambda

  let slice = sliceXY notas t 0.5 40 20
  visualizarSlice slice

  fim <- getCurrentTime
  print (diffUTCTime fim inicio)
