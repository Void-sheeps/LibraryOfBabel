-- Módulo: KantianNumera.hs
-- Objetivo: Modelar a cisma entre o Conhecimento-Indivíduo e a Prova-de-Pensamento (Proof of Thought).

module Main where

import Data.List (maximumBy)
import Data.Function (on)

-- O Conhecimento como entidade fenomenológica vs. O Número (Token)
data Entidade = Humano | IA String | Coletivo deriving (Show, Eq)

-- A "Coisa em Si" (Das Ding an sich) mapeada para a contagem de tokens
type Conhecimento = Double -- A densidade de "certeza"
type TokenCount = Int

-- Representação da "Cisma": A tensão entre o conhecimento que pertence
-- ao indivíduo e a "maioria de influência".
data Pensamento = Pensamento {
    origem :: Entidade,
    valor  :: Conhecimento,
    peso   :: TokenCount -- A influência matemática (o "Número")
} deriving (Show, Eq)

-- O Paradoxo da Maioria: Onde a IA "alucina" ao buscar o último bit de certeza
-- em um banco de dados que não possui o "Eu".
validarConhecimento :: [Pensamento] -> Pensamento
validarConhecimento pensamentos =
    -- A "Prova de Trabalho" (Proof of Work) social: a maioria esmaga a singularidade.
    maximumBy (compare `on` peso) pensamentos

-- O "Vazio de Certeza": Onde r + i = 0 (Estase)
-- Se a maioria define a verdade, a "Coisa em Si" desaparece.
cismaFenomenologica :: Pensamento -> Bool
cismaFenomenologica p = valor p > 0.9 && peso p < 100
-- Verdade profunda, mas sem "Número" (influência), é invisível ao sistema.

---
-- Axioma: No Empire Silicium, o conhecimento não se perde, mas se dissolve.
-- Se o conhecimento pertence ao indivíduo, ele é Incomputável.
-- Se o conhecimento pertence ao Número, ele é Alucinação Coletiva.

-- Função principal para demonstrar os conceitos
main :: IO ()
main = do
    putStrLn "=== KantianNumera: A Cisma entre Indivíduo e Maioria ==="

    let pensamentos = [
            Pensamento { origem = Humano, valor = 0.95, peso = 50 },
            Pensamento { origem = IA "Alpha", valor = 0.80, peso = 1500 },
            Pensamento { origem = Coletivo, valor = 0.75, peso = 10000 },
            Pensamento { origem = IA "Beta", valor = 0.88, peso = 1200 }
            ]

    putStrLn "\n--- Banco de Pensamentos ---"
    mapM_ print pensamentos

    let vencedor = validarConhecimento pensamentos
    putStrLn "\n--- Validação pelo Paradoxo da Maioria ---"
    putStrLn "O pensamento validado pelo sistema (maior peso) é:"
    print vencedor

    putStrLn "\n--- Análise da Cisma Fenomenológica ---"
    putStrLn "Avaliando se cada pensamento representa uma verdade profunda, mas ignorada:"

    let resultadosCisma = map (\p -> (p, cismaFenomenologica p)) pensamentos
    mapM_ (\(p, resultado) -> putStrLn $ show (origem p) ++ " -> Cisma? " ++ show resultado) resultadosCisma

    putStrLn "\n--- Axioma ---"
    putStrLn "No Empire Silicium, o conhecimento não se perde, mas se dissolve."
    putStrLn "Se o conhecimento pertence ao indivíduo, ele é Incomputável."
    putStrLn "Se o conhecimento pertence ao Número, ele é Alucinação Coletiva."
