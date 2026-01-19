-- AmorFati.hs
-- Jogo Estoico de Aceitação do Destino
-- Comando: runghc AmorFati.hs  ou  ghci AmorFati.hs

import System.Random
import System.IO
import System.Console.ANSI
import Control.Concurrent
import Data.List
import Data.Char (toLower)
import System.Exit

-- ───────────────────────────────────────────────
-- 1. ONTOLOGIA DO EVENTO
-- ───────────────────────────────────────────────

data TipoDestino
  = FluxoNormal    -- Dourado
  | DorSubita      -- Vermelho
  | ErroFatal      -- Violeta
  | Vazio          -- Cinza
  deriving (Show, Eq, Enum, Bounded)

instance Random TipoDestino where
  randomR (a, b) g =
    let (i, g') = randomR (fromEnum a, fromEnum b) g
    in (toEnum i, g')
  random g = randomR (minBound, maxBound) g

-- Evento com propriedades filosóficas
data Evento = Evento
  { tipo          :: TipoDestino
  , intensidade   :: Int        -- impacto bruto (0-100)
  , controlavel   :: Bool       -- estoico: está sob meu controle?
  , reversivel    :: Bool       -- posso desfazer?
  , descricao     :: String     -- descrição narrativa
  , acaoCorreta   :: Acao       -- ação filosoficamente correta
  } deriving (Show)

-- ───────────────────────────────────────────────
-- 2. ESTADO DO JOGADOR
-- ───────────────────────────────────────────────

data EstadoJogo = EstadoJogo
  { aceitacao   :: Int       -- estabilidade ontológica (0-100)
  , resistencia :: Int       -- energia vital (0-100)
  , nivel       :: Int       -- nível de sabedoria
  , combo       :: Int       -- acertos consecutivos
  , rodada      :: Int       -- rodada atual
  , pontos      :: Int       -- pontuação total
  , historico   :: [(Evento, Acao, Bool)]  -- (evento, ação tomada, acertou?)
  } deriving (Show)

-- ───────────────────────────────────────────────
-- 3. AÇÕES POSSÍVEIS
-- ───────────────────────────────────────────────

data Acao = Aceitar | Resistir
  deriving (Show, Eq)

-- ───────────────────────────────────────────────
-- 4. CUSTOS E RECOMPENSAS
-- ───────────────────────────────────────────────

custoResistir :: Evento -> Int
custoResistir e = intensidade e `div` 2

custoAceitar :: Evento -> Int
custoAceitar e = intensidade e `div` 4

ganhoAceitacao :: Evento -> Int
ganhoAceitacao e =
  case tipo e of
    DorSubita  -> intensidade e `div` 5 + 10
    ErroFatal  -> intensidade e `div` 4 + 15
    Vazio      -> intensidade e `div` 6 + 5
    FluxoNormal -> intensidade e `div` 8 + 3

-- Penalidade por escolha errada
penalidadeAcaoErrada :: Evento -> Int
penalidadeAcaoErrada e = intensidade e `div` 3

-- ───────────────────────────────────────────────
-- 5. LÓGICA ESTOICA DO JOGO
-- ───────────────────────────────────────────────

-- Decisão estoica racional
decisaoEstoica :: EstadoJogo -> Evento -> Acao
decisaoEstoica estado evento
  | not (controlavel evento) = Aceitar
  | reversivel evento && resistencia estado >= custoResistir evento * 2 = Resistir
  | otherwise = Aceitar

-- Aplica uma ação ao estado
aplicarAcao :: EstadoJogo -> Evento -> Acao -> (EstadoJogo, Bool)
aplicarAcao estado evento acaoJogador =
  let acaoCorreta' = decisaoEstoica estado evento
      acertou = acaoJogador == acaoCorreta'

      novoAceitacao = max 0 $ min 100 $
        if acertou
          then case acaoJogador of
                 Aceitar -> aceitacao estado + ganhoAceitacao evento
                 Resistir -> aceitacao estado - custoResistir evento
          else aceitacao estado - penalidadeAcaoErrada evento

      novaResistencia = max 0 $ min 100 $
        if acertou
          then case acaoJogador of
                 Aceitar -> resistencia estado - custoAceitar evento
                 Resistir -> resistencia estado - custoResistir evento
          else resistencia estado - penalidadeAcaoErrada evento

      novoCombo = if acertou then combo estado + 1 else 0
      bonusCombo = if novoCombo >= 5 then 10 else 0

      pontosGanhos =
        if acertou
          then case tipo evento of
                DorSubita  -> 25 + bonusCombo
                ErroFatal  -> 30 + bonusCombo
                Vazio      -> 15 + bonusCombo
                FluxoNormal -> 10 + bonusCombo
          else -20

      novoNivel = if pontos estado + pontosGanhos >= nivel estado * 500
                    then nivel estado + 1
                    else nivel estado

      novoEstado = EstadoJogo
        { aceitacao   = novoAceitacao
        , resistencia = novaResistencia + if novoNivel > nivel estado then 20 else 0
        , nivel       = novoNivel
        , combo       = novoCombo
        , rodada      = rodada estado + 1
        , pontos      = max 0 (pontos estado + pontosGanhos)
        , historico   = (evento, acaoJogador, acertou) : historico estado
        }
  in (novoEstado, acertou)

-- ───────────────────────────────────────────────
-- 6. GERADOR DE EVENTOS
-- ───────────────────────────────────────────────

descricoesPorTipo :: TipoDestino -> [String]
descricoesPorTipo FluxoNormal =
  [ "O rio flui calmamente diante de você."
  , "Dados processados sem anomalias."
  , "Sequência de eventos previsíveis."
  , "O dia segue seu curso habitual."
  ]
descricoesPorTipo DorSubita =
  [ "Uma pontada aguda atravessa seu peito."
  , "Memória dolorosa ressurge sem aviso."
  , "Falha no sistema: erro 0xPAIN."
  , "Perda inesperada se apresenta."
  ]
descricoesPorTipo ErroFatal =
  [ "Segmentation fault na consciência."
  , "Todas as threads travaram simultaneamente."
  , "Paradoxo lógico irresolvível."
  , "Buffer overflow de realidade."
  ]
descricoesPorTipo Vazio =
  [ "Silêncio absoluto preenche tudo."
  , "Nenhum dado disponível para processamento."
  , "O vazio entre as estrelas."
  , "Ausência de significado manifesto."
  ]

gerarEvento :: Int -> IO Evento
gerarEvento dificuldade = do
  tipo' <- randomRIO (minBound :: TipoDestino, maxBound)
  intens <- randomRIO (20 + dificuldade * 10, 80 + dificuldade * 5)
  control <- randomIO
  revers <- if control then randomIO else return False

  descList <- case tipo' of
    FluxoNormal -> return $ descricoesPorTipo FluxoNormal
    DorSubita   -> return $ descricoesPorTipo DorSubita
    ErroFatal   -> return $ descricoesPorTipo ErroFatal
    Vazio       -> return $ descricoesPorTipo Vazio

  descIndex <- randomRIO (0, length descList - 1)

  return Evento
    { tipo = tipo'
    , intensidade = intens
    , controlavel = control
    , reversivel = revers
    , descricao = descList !! descIndex
    , acaoCorreta = Aceitar  -- Será calculada depois
    }

-- ───────────────────────────────────────────────
-- 7. INTERFACE GRÁFICA EM TERMINAL
-- ───────────────────────────────────────────────

corTipo :: TipoDestino -> String
corTipo FluxoNormal = "\ESC[33m"  -- Amarelo
corTipo DorSubita   = "\ESC[31m"  -- Vermelho
corTipo ErroFatal   = "\ESC[35m"  -- Magenta
corTipo Vazio       = "\ESC[90m"  -- Cinza

simboloTipo :: TipoDestino -> String
simboloTipo FluxoNormal = "🌀"
simboloTipo DorSubita   = "💥"
simboloTipo ErroFatal   = "⚠️"
simboloTipo Vazio       = "◼"

resetCor :: String
resetCor = "\ESC[0m"

limparTela :: IO ()
limparTela = clearScreen >> setCursorPosition 0 0

mostrarCabecalho :: EstadoJogo -> IO ()
mostrarCabecalho estado = do
  putStrLn $ "\ESC[36m╔════════════════════════════════════════════════════════════╗"
  putStrLn   "║                AMOR FATI - JOGO ESTOICO                 ║"
  putStrLn   ("╚════════════════════════════════════════════════════════════╝" ++ resetCor)
  putStrLn ""

  putStrLn $ "Nível: \ESC[33m" ++ show (nivel estado) ++ resetCor ++
             "  |  Combo: " ++ (if combo estado >= 3 then "\ESC[32m" else "\ESC[33m") ++
             show (combo estado) ++ "x" ++ resetCor ++
             "  |  Rodada: \ESC[36m" ++ show (rodada estado) ++ resetCor

  putStrLn $ "Pontuação: \ESC[35m" ++ show (pontos estado) ++ resetCor
  putStrLn ""

mostrarBarras :: EstadoJogo -> IO ()
mostrarBarras estado = do
  putStrLn $ barraStatus "Aceitação" (aceitacao estado) 100 "\ESC[32m"
  putStrLn $ barraStatus "Resistência" (resistencia estado) 100 "\ESC[34m"
  putStrLn ""
  where
    barraStatus nome valor maximo cor =
      let largura = 30
          preenchido = (valor * largura) `div` maximo
          barra = replicate preenchido '█' ++ replicate (largura - preenchido) '░'
      in nome ++ ": " ++ cor ++ barra ++ " " ++ show valor ++ "/100" ++ resetCor

mostrarEvento :: Evento -> IO ()
mostrarEvento evento = do
  let cor = corTipo (tipo evento)
  putStrLn $ cor ++ "╔════════════════════════════════════════════════════════════╗" ++ resetCor
  putStrLn $ cor ++ "║ " ++ simboloTipo (tipo evento) ++ " " ++
            show (tipo evento) ++ " (Intensidade: " ++ show (intensidade evento) ++ ")" ++
            replicate (45 - length (show (tipo evento)) - length (show (intensidade evento))) ' ' ++
            "║" ++ resetCor
  putStrLn $ cor ++ "║                                                            ║" ++ resetCor

  let linhas = quebrarLinha (descricao evento) 54
  mapM_ (\linha -> putStrLn $ cor ++ "║ " ++ linha ++ replicate (55 - length linha) ' ' ++ " ║" ++ resetCor) linhas

  putStrLn $ cor ++ "║                                                            ║" ++ resetCor
  putStrLn $ cor ++ "║ Controle: " ++ (if controlavel evento then "Sim" else "Não") ++
            " | Reversível: " ++ (if reversivel evento then "Sim" else "Não") ++
            replicate 30 ' ' ++ "║" ++ resetCor
  putStrLn $ cor ++ "╚════════════════════════════════════════════════════════════╝" ++ resetCor
  putStrLn ""
  where
    quebrarLinha :: String -> Int -> [String]
    quebrarLinha "" _ = []
    quebrarLinha str n
      | length str <= n = [str]
      | otherwise =
          let (primeira, resto) = splitAt n str
              (parte, sobra) = if last primeira /= ' ' && head resto /= ' '
                               then splitAt (lastSpace primeira) str
                               else (primeira, dropWhile (== ' ') resto)
          in parte : quebrarLinha sobra n

    lastSpace :: String -> Int
    lastSpace = maybe 0 id . findIndex (== ' ') . reverse

mostrarHistorico :: EstadoJogo -> IO ()
mostrarHistorico estado = do
  putStrLn ("\ESC[90m────────────────────────────────────────────────────────" ++ resetCor)
  putStrLn "Histórico recente:"

  let eventosRecentes = take 5 (historico estado)
  mapM_ mostrarEventoHistorico eventosRecentes

  where
    mostrarEventoHistorico (evento, acao, acertou) = do
      let simbolo = if acertou then "\ESC[32m✓" else "\ESC[31m✗"
          acaoStr = case acao of
                     Aceitar -> "Aceitou"
                     Resistir -> "Resistiu"
      putStrLn $ "  " ++ simbolo ++ resetCor ++ " " ++
                 corTipo (tipo evento) ++ show (tipo evento) ++ resetCor ++
                 " -> " ++ acaoStr

-- ───────────────────────────────────────────────
-- 8. LÓGICA PRINCIPAL DO JOGO
-- ───────────────────────────────────────────────

estadoInicial :: EstadoJogo
estadoInicial = EstadoJogo
  { aceitacao   = 50
  , resistencia = 50
  , nivel       = 1
  , combo       = 0
  , rodada      = 0
  , pontos      = 0
  , historico   = []
  }

verificarGameOver :: EstadoJogo -> Bool
verificarGameOver estado =
  aceitacao estado <= 0 || resistencia estado <= 0 || rodada estado >= 100

jogarRodada :: EstadoJogo -> IO EstadoJogo
jogarRodada estado = do
  limparTela
  mostrarCabecalho estado
  mostrarBarras estado

  -- Gerar evento baseado na dificuldade
  evento <- gerarEvento (nivel estado)

  putStrLn ""
  mostrarEvento evento
  putStrLn ""

  -- Obter ação do jogador
  putStrLn ("\ESC[33mO que você faz?" ++ resetCor)
  putStrLn "  (A) Aceitar o destino"
  putStrLn "  (R) Resistir ao destino"
  putStrLn "  (S) Sair do jogo"
  putStr "> "
  hFlush stdout

  entrada <- getLine
  case map toLower entrada of
    "a" -> processarAcao estado evento Aceitar
    "r" -> processarAcao estado evento Resistir
    "s" -> do
      putStrLn "\nEncerrando jogo..."
      exitSuccess
    _ -> do
      putStrLn "\ESC[31mOpção inválida! Tente novamente.\ESC[0m"
      threadDelay 1000000
      return estado

processarAcao :: EstadoJogo -> Evento -> Acao -> IO EstadoJogo
processarAcao estado evento acao = do
  let (novoEstado, acertou) = aplicarAcao estado evento acao
      acaoStr = case acao of
                 Aceitar -> "aceitou"
                 Resistir -> "resistiu"

  limparTela
  mostrarCabecalho novoEstado

  if acertou
    then do
      putStrLn $ "\ESC[32m✓ Decisão estoica! Você " ++ acaoStr ++ " corretamente." ++ resetCor
      case acao of
        Aceitar -> putStrLn "\ESC[90m  \"Amor fati: amar o que é necessário\"\ESC[0m"
        Resistir -> putStrLn "\ESC[90m  \"A virtude está no discernimento\"\ESC[0m"
    else do
      putStrLn $ "\ESC[31m✗ Ação não estoica! Você " ++ acaoStr ++ " incorretamente." ++ resetCor
      putStrLn "\ESC[90m  Sofrimento desnecessário foi gerado.\ESC[0m"

  if combo novoEstado >= 5
    then putStrLn $ "\ESC[33m★ Combo " ++ show (combo novoEstado) ++ "x! Bônus aplicado.\ESC[0m"
    else return ()

  if nivel novoEstado > nivel estado
    then putStrLn $ "\ESC[35m★ Nível UP! Você alcançou o nível " ++ show (nivel novoEstado) ++ "\ESC[0m"
    else return ()

  putStrLn ""
  mostrarBarras novoEstado
  mostrarHistorico novoEstado

  putStrLn "\nPressione Enter para continuar..."
  _ <- getLine

  return novoEstado

mostrarGameOver :: EstadoJogo -> IO ()
mostrarGameOver estado = do
  limparTela

  putStrLn $ "\ESC[31m╔════════════════════════════════════════════════════════════╗"
  putStrLn   "║                      GAME OVER                           ║"
  putStrLn   "╚════════════════════════════════════════════════════════════╝\ESC[0m"
  putStrLn ""

  putStrLn "Sua jornada estoica chegou ao fim."
  putStrLn ""

  putStrLn $ "\ESC[36m═══════════════ RESUMO FINAL ═══════════════\ESC[0m"
  putStrLn $ "Rodadas sobrevividas: \ESC[33m" ++ show (rodada estado) ++ "\ESC[0m"
  putStrLn $ "Nível alcançado: \ESC[33m" ++ show (nivel estado) ++ "\ESC[0m"
  putStrLn $ "Pontuação final: \ESC[35m" ++ show (pontos estado) ++ "\ESC[0m"

  let totalAcoes = length (historico estado)
      acoesCorretas = length (filter (\(_, _, acertou) -> acertou) (historico estado))
      precisao = if totalAcoes > 0 then (acoesCorretas * 100) `div` totalAcoes else 0

  putStrLn $ "Precisão estoica: \ESC[34m" ++ show precisao ++ "%\ESC[0m"
  putStrLn ""

  if pontos estado > 1000
    then putStrLn "\ESC[32m🏆 Excelente desempenho! Você alcançou a ataraxia.\ESC[0m"
  else if pontos estado > 500
    then putStrLn "\ESC[33m★ Bom trabalho! Você está no caminho da sabedoria.\ESC[0m"
  else putStrLn "\ESC[90mA prática leva à perfeição. Tente novamente.\ESC[0m"

  putStrLn ""
  putStrLn "\ESC[90m\"Não busque que os eventos aconteçam como você quer,\nmas queira os eventos como acontecem: e tudo irá bem.\"\n           — Epicteto\ESC[0m"
  putStrLn ""
  putStrLn "Pressione Enter para sair..."
  _ <- getLine
  return ()

jogar :: IO ()
jogar = do
  limparTela
  putStrLn "\ESC[36m╔════════════════════════════════════════════════════════════╗"
  putStrLn "║                AMOR FATI - JOGO ESTOICO                 ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝\ESC[0m"
  putStrLn ""
  putStrLn "\ESC[90m\"Amor fati — amar o destino, não há erro, apenas o que é.\"\ESC[0m"
  putStrLn ""
  putStrLn "\ESC[33mOBJETIVO:\ESC[0m"
  putStrLn "  Mantenha sua Aceitação e Resistência acima de zero enquanto"
  putStrLn "  enfrenta diferentes tipos de destino."
  putStrLn ""
  putStrLn "\ESC[33mREGRAS:\ESC[0m"
  putStrLn "  • Se não controla, ACEITE"
  putStrLn "  • Se controla e pode reverter, RESISTA"
  putStrLn "  • Se resistir custa mais que aceitar, ACEITE"
  putStrLn ""
  putStrLn "\ESC[33mCONTROLES:\ESC[0m"
  putStrLn "  A - Aceitar o destino"
  putStrLn "  R - Resistir ao destino"
  putStrLn "  S - Sair do jogo"
  putStrLn ""
  putStrLn "\ESC[90mPressione Enter para começar sua jornada estoica...\ESC[0m"
  _ <- getLine

  loopJogo estadoInicial
  where
    loopJogo estado
      | verificarGameOver estado = mostrarGameOver estado
      | otherwise = do
          novoEstado <- jogarRodada estado
          loopJogo novoEstado

-- ───────────────────────────────────────────────
-- 9. FUNÇÃO PRINCIPAL
-- ───────────────────────────────────────────────

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetEcho stdout False
  setTitle "Amor Fati - Jogo Estoico"

  jogar

  putStrLn "\nAmor fati."
  return ()
