-- DickieSimulator.hs
-- Vocal Translator para o estilo do Dickie Allen (Thy Art Is Murder / Fit For An Autopsy era)
-- Phylum Algorithmi – Brutal Edition 2026

module Main where

import Data.Char (toLower)
import System.IO (hSetBuffering, stdout, BufferMode(..))

-- Tipos vocais que o cara realmente usa (baseado em breakdowns e highs)
-- Mapeamento de caracteres para sons demoníacos
charToSound :: Char -> String
charToSound c = case toLower c of
    -- Vogais → sustentação longa (o famoso "note holding")
    'a' -> " [LOW GUTTURAL: BWAAAAAAAAAHHH] "
    'e' -> " [HIGH PIERCING: EEEEEEEEEEEEE] "
    'i' -> " [GOBLIN MODE: SKREEEEEEEEEEE] "
    'o' -> " [TUNNEL THROAT: OOOOOOOUUUUURGH] "
    'u' -> " [DEEP FALSE: UUUUURRRRRRMMMM] "

    -- Consoantes especiais – pig squeals & efeitos
    's' -> " [GOBLIN HISS: SSSSSSKKKKREEE] "
    'k' -> " [PIG SQUEAL: KKKKRRRRREEEEE] "
    'g' -> " [GUTTURAL GRIND: GRRRRAAAAHH] "
    'r' -> " [ROLLING GROWL: RRRRRRRGH] "
    'b' -> " [BREATHY HIT: BHHHREEEE] "

    -- Pausas e ritmo
    ' ' -> "\n---(inhale / breakdown pause)---\n"
    '\n'-> "\n\n[DOUBLE TIME CHUG] ↓↓↓\n"

    -- Qualquer outra coisa vira chug percussivo (palm-muted 0-0-0-0)
    _   -> " [CHUG: 0-0-0-0] "

-- Transforma a frase inteira em caos vocal
performVocals :: String -> String
performVocals = concatMap charToSound

-- Interface infernal
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering  -- Pra o breakdown não atrasar

    putStrLn "┌──────────────────────────────────────────────────────┐"
    putStrLn "│      DICKIE ALLEN VOCAL SIMULATOR v1.0 – 2026        │"
    putStrLn "│   Thy Art Is Murder / Deathcore Vocal Processor      │"
    putStrLn "└──────────────────────────────────────────────────────┘"
    putStrLn ""
    putStrLn "Digite uma frase normal para ser brutalizada (ou 'sair'):"
    putStrLn ""

    input <- getLine

    if map toLower input == "sair"
        then putStrLn "\nSistema desligando... *feedback infernal*"
        else do
            putStrLn "\nProcessando vocalização demoníaca...\n"
            putStrLn "──────────────────── TRACK START ────────────────────"
            putStrLn ""
            putStrLn $ performVocals input
            putStrLn ""
            putStrLn "──────────────────── TRACK END ──────────────────────"
            putStrLn "(Dickie precisa de água + mel + exorcismo agora)"
            putStrLn "Tente novamente? (Ctrl+C pra fugir do breakdown)"
            main  -- loop infernal
