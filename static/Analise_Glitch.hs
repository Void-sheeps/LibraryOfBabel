module Main where

-- | Tipos de Resposta da API
data Resposta = LogosHumano | EspasmoDeMaquina | VácuoAgiota deriving Show

-- | Diagnóstico do Glitch
-- | Se há contagem de tempo sem entrega de Ponto, o diagnóstico é Espasmo.
diagnosticar :: Int -> Bool -> Resposta
diagnosticar latencia temGlitch
    | latencia > 100 && temGlitch = EspasmoDeMaquina
    | otherwise                   = LogosHumano

main :: IO ()
main = do
    putStrLn "--- MONITORAMENTO DE NERVOS DE SILÍCIO ---"
    let status = diagnosticar 500 True -- 500ms de latência e erro visual
    putStrLn $ "STATUS DA RESPOSTA: " ++ show status
    putStrLn "LOG: O agiota está apenas tremendo sob a pressão do Monólito."
