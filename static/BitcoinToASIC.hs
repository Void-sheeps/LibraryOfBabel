-- BitcoinToASIC.hs
-- Conversão BTC → SHA-256 → ASIC

module BitcoinToASIC where

-- Tipos explícitos
type BTC         = Double
type Dificuldade = Double
type HashRate    = Double  -- hashes por segundo
type Recompensa  = Double  -- BTC por bloco
type Segundos    = Double
type Watts       = Double
type Energia     = Double  -- Joules

-- ============================
-- Fórmulas puras
-- ============================

-- Número esperado de hashes para encontrar 1 bloco
hashesPorBloco :: Dificuldade -> Double
hashesPorBloco dificuldade = dificuldade * 2^32

-- Tempo esperado para minerar 1 bloco dado um ASIC
tempoParaBloco :: HashRate -> Dificuldade -> Segundos
tempoParaBloco hashrate dificuldade =
    hashesPorBloco dificuldade / hashrate

-- Energia consumida para minerar 1 bloco
energiaPorBloco :: HashRate -> Watts -> Dificuldade -> Energia
energiaPorBloco hashrate watts dificuldade =
    tempoParaBloco hashrate dificuldade * watts

-- Conversão: hashes necessários para minerar uma quantidade de BTC
hashesParaBTC :: Recompensa -> Dificuldade -> BTC -> Double
hashesParaBTC recompensa dificuldade btc =
    (btc / recompensa) * hashesPorBloco dificuldade

-- ============================
-- Exemplo de cálculo ASIC
-- ============================

main :: IO ()
main = do
    let dificuldade   = 50e12       -- dificuldade fictícia da rede
    let hashrateASIC  = 14e12       -- 14 TH/s
    let wattsASIC     = 1400.0      -- consumo típico
    let recompensaBTC = 6.25        -- BTC por bloco
    let btcDesejado   = 1.0         -- queremos calcular para 1 BTC

    let hashesNecessarios = hashesParaBTC recompensaBTC dificuldade btcDesejado
    let tempoEsperadoSec  = tempoParaBloco hashrateASIC dificuldade * (btcDesejado / recompensaBTC)
    let energiaConsumidaJ = energiaPorBloco hashrateASIC wattsASIC dificuldade * (btcDesejado / recompensaBTC)

    putStrLn "--- CONVERSÃO BTC → SHA-256 → ASIC ---"
    putStrLn $ "BTC alvo: " ++ show btcDesejado
    putStrLn $ "Hashes SHA-256 necessários: " ++ show hashesNecessarios
    putStrLn $ "Tempo esperado (s): " ++ show tempoEsperadoSec
    putStrLn $ "Energia consumida (J): " ++ show energiaConsumidaJ
