{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module MetaRede where

-- Tipos de entidades
data Entidade = Humano | IA | RedeDistribuida

-- Tipos de prova
data Prova = Validacao | Branch | Registro

-- Estado do sistema
data EstadoSistema = Estado {
    possiveis :: [String],     -- Espaço de estados possíveis
    atual     :: String        -- Estado corrente
}

-- Efeito de prova
aplicarProva :: Entidade -> Prova -> EstadoSistema -> EstadoSistema
aplicarProva IA Validacao est = est { possiveis = filter (/= "s1") (possiveis est) }
aplicarProva RedeDistribuida Branch est = est { possiveis = ["s2"], atual = "s2" }
aplicarProva _ _ est = est  -- Humano não altera sozinho

-- Demonstração de autoridade e alteração de realidade
demo :: IO ()
demo = do
    let sistemaInicial = Estado { possiveis = ["s1","s2","s3","s4"], atual = "s1" }
    putStrLn $ "Sistema Inicial: " ++ show (possiveis sistemaInicial)

    let sistemaIA = aplicarProva IA Validacao sistemaInicial
    putStrLn $ "Após IA validar: " ++ show (possiveis sistemaIA)

    let sistemaRede = aplicarProva RedeDistribuida Branch sistemaIA
    putStrLn $ "Após Rede aplicar branch: " ++ show (possiveis sistemaRede)
    putStrLn $ "Estado Atual: " ++ atual sistemaRede
