{-# LANGUAGE OverloadedStrings #-}

module RevelacaoDoAgiota where

import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, splitOn)
import qualified Data.Map as Map

-- ================== A MATRIZ COMPLETA (4x5) ==================

-- | As quatro linhas da realidade econômica
matrizEconomiaDasAlmas :: [[(String, String)]]
matrizEconomiaDasAlmas =
  [ -- LINHA 1: LIMIAR (estrutura da dívida)
    [ ("Filosofia", "Memento mori do capital: lembrete da própria finitude financeira")
    , ("Antropologia", "Ritual de passagem: de devedor a inadimplente")
    , ("Linguística", "Performativos constativos: 'Você vai pagar' como ato de fala que cria realidade")
    , ("Psicologia", "Ansiedade antecipatória do vencimento")
    , ("Cultura", "Porta giratória do banco como limiar físico")
    ]

  , -- LINHA 2: LIMINAL (experiência da dívida)
    [ ("Filosofia", "Estado de exceção econômico: suspensão das leis normais do consumo")
    , ("Antropologia", "Comunitas dos endividados: solidariedade na precariedade")
    , ("Linguística", "Eufemismos do calote: 'renegociação', 'parcelamento', 'refinanciamento'")
    , ("Psicologia", "Dissociação entre presente e futuro financeiro")
    , ("Cultura", "Madrugadas contando moedas")
    ]

  , -- LINHA 3: CORNO (estigma da dívida)
    [ ("Filosofia", "A alienação do próprio suor: trabalhar para pagar juros")
    , ("Antropologia", "Tabu do nome sujo: exclusão dos circuitos de crédito")
    , ("Linguística", "Estigmatização lexical: 'caloteiro', 'mau pagador', 'nome sujo'")
    , ("Psicologia", "Vergonha transformada em raiva direcionada a si mesmo")
    , ("Cultura", "Figura do trouxa na piada de português")
    ]

  , -- LINHA 4: AGIOTA (operador da dívida)
    [ ("Filosofia", "Dialética do Senhor e Escravo (Hegel com juros de 15% a.d.)")
    , ("Antropologia", "Potlatch Destrutivo: a troca onde você dá a alma e recebe nada")
    , ("Linguística", "Imperativo Categórico da Ameaça (ex: 'Cadê o meu?')")
    , ("Psicologia", "Síndrome de Estocolmo Financeira (agradecer por mais prazo)")
    , ("Cultura", "O Cobrador (Rubem Fonseca): a violência como acerto de contas")
    ]
  ]

-- ================== TIPOS PARA ANÁLISE ==================

data CategoriaEconomica =
    Filosofia
  | Antropologia
  | Linguistica
  | Psicologia
  | Cultura
  deriving (Eq, Show, Enum, Bounded)

data EstadoDivida =
    Limiar       -- No limiar do vencimento
  | Liminal      -- No estado liminar da negociação
  | Corno        -- Marcado pelo estigma da inadimplência
  | Agiota       -- Operando a máquina de endividamento
  deriving (Eq, Show, Enum)

-- | Representação da dívida como relação de poder
data Divida = Divida
  { devedor      :: String
  , credor       :: String
  , valorOriginal :: Double
  , jurosAoDia   :: Double  -- Em decimal: 0.15 para 15%
  , diasAtraso   :: Int
  , estado       :: EstadoDivida
  } deriving (Show)

-- ================== FUNÇÕES DE ANÁLISE ==================

-- | Calcular dívida atualizada com juros compostos diários
calcularDividaAtual :: Divida -> Double
calcularDividaAtual (Divida _ _ valor juros dias _) =
  valor * (1 + juros) ^ dias

-- | Converter estado da dívida em descrição filosófica
interpretarEstado :: EstadoDivida -> String
interpretarEstado estado = case estado of
  Limiar  -> "Estado limiar: fronteira entre solvência e insolvência"
  Liminal -> "Estado liminal: suspensão das normas econômicas usuais"
  Corno   -> "Estado estigmatizado: marcado pelo fracasso financeiro"
  Agiota  -> "Estado de operador: aquele que extrai valor do desespero"

-- | Localizar interseção na matriz
localizarNaMatriz :: EstadoDivida -> CategoriaEconomica -> Maybe String
localizarNaMatriz estado categoria = do
  let linhaIndex = fromEnum estado
  let colIndex = fromEnum categoria
  if linhaIndex < length matrizEconomiaDasAlmas
     then let linha = matrizEconomiaDasAlmas !! linhaIndex
          in if colIndex < length linha
             then Just (snd (linha !! colIndex))
             else Nothing
     else Nothing

-- ================== API DO AGIOTA (SIMULAÇÃO) ==================

-- | Gerar mensagem de cobrança personalizada
gerarMensagemCobrança :: Divida -> String
gerarMensagemCobrança divida =
  let valorAtual = calcularDividaAtual divida
      dias = diasAtraso divida
      nome = devedor divida
  in unlines
     [ "📱 *MENSAGEM AUTOMÁTICA DO SISTEMA*"
     , ""
     , "Olá, " ++ nome ++ "."
     , ""
     , "Sua dívida de R$ " ++ show (valorOriginal divida) ++
       " já está em R$ " ++ show valorAtual ++ " (" ++ show dias ++ " dias)."
     , ""
     , "Lembramos que os juros são de " ++
       show (jurosAoDia divida * 100) ++ "% ao dia."
     , ""
     , "📞 Para renegociar: (11) 9XXXX-XXXX"
     , "💀 Caso contrário: consequências serão aplicadas."
     , ""
     , "Atenciosamente,"
     , "Sistema Automático de Cobrança"
     ]

-- | Analisar perfil do devedor
analisarPerfilDevedor :: Divida -> [(String, String)]
analisarPerfilDevedor divida =
  let risco = case diasAtraso divida of
                d | d < 5  -> "Baixo"
                | d < 15 -> "Médio"
                | otherwise -> "Alto"

      psicologia = case estado divida of
        Limiar  -> "Ansiedade antecipatória"
        Liminal -> "Dissociação financeira"
        Corno   -> "Raiva internalizada"
        Agiota  -> "Não aplicável"

      categoriaCultural = case estado divida of
        Limiar  -> "Ainda no jogo do crédito"
        Liminal -> "Jogando com as regras suspensas"
        Corno   -> "Excluído dos circuitos formais"
        Agiota  -> "Operador da exclusão"

  in [ ("Risco de Calote", risco)
     , ("Perfil Psicológico", psicologia)
     , ("Status Cultural", categoriaCultural)
     , ("Juros Acumulados", show (calcularDividaAtual divida - valorOriginal divida))
     ]

-- ================== FUNÇÃO DE REVELAÇÃO ==================

-- | A função principal que revela a lógica do agiota
main :: IO ()
main = do
  putStrLn "="
  putStrLn "🔍 REVELAÇÃO DO AGIOTA: A MATRIZ ESCONDIDA"
  putStrLn "="

  putStrLn "\n📊 MATRIZ ECONOMIA DAS ALMAS (4x5):"
  putStrLn $ replicate 60 '-'

  -- Exibir a matriz
  let estados = ["LIMIAR", "LIMINAL", "CORNO", "AGIOTA"]
  mapM_ (\(estadoNome, linha) -> do
          putStrLn $ "\n[" ++ estadoNome ++ "]"
          mapM_ (\(cat, desc) ->
                  putStrLn $ "  " ++ cat ++ ": " ++ desc) linha
        ) (zip estados matrizEconomiaDasAlmas)

  putStrLn "\n" ++ replicate 60 '='
  putStrLn ">>> ALERTA DE RASTREAMENTO <<<"
  putStrLn "A 'API Agiota' foi localizada na interseção catastrófica:"
  putStrLn ""
  putStrLn "1. É um LIMIAR: O momento exato do vencimento do boleto."
  putStrLn "2. É um estado LIMINAL: A madrugada de insônia esperando a cobrança."
  putStrLn "3. Gera o estado de CORNO: Aquele que trabalha para pagar juros abusivos."
  putStrLn ""
  putStrLn "STATUS: A API Agiota é a função que converte o sujeito em objeto de dívida."

  putStrLn "\n" ++ replicate 60 '='
  putStrLn "💸 EXEMPLO PRÁTICO:"

  -- Criar exemplo de dívida
  let exemploDivida = Divida
        { devedor = "João da Silva"
        , credor = "Agiota Digital Ltda."
        , valorOriginal = 1000.00
        , jurosAoDia = 0.15  -- 15% ao dia
        , diasAtraso = 10
        , estado = Corno
        }

  putStrLn $ "\n" ++ gerarMensagemCobrança exemploDivida

  putStrLn "📈 ANÁLISE DO PERFIL:"
  mapM_ (\(k, v) -> putStrLn $ "  " ++ k ++ ": " ++ v)
        (analisarPerfilDevedor exemploDivida)

  putStrLn "\n" ++ replicate 60 '='
  putStrLn "🎭 CONCLUSÃO FILOSÓFICA:"
  putStrLn ""
  putStrLn "\"O agiota não vende dinheiro. Vende tempo."
  putStrLn "Mas vende o tempo do outro como se fosse seu."
  putStrLn ""
  putStrLn "A dívida é a materialização do futuro em números,"
  putStrLn "e o agiota é o vigia dessa prisão temporal.\""

-- ================== UTILITÁRIOS ==================

-- | Encontrar todas as descrições de uma categoria
extrairCategoria :: CategoriaEconomica -> [(EstadoDivida, String)]
extrairCategoria categoria =
  let indicesEstados = [Limiar .. Agiota]
      colIndex = fromEnum categoria
  in map (\estado ->
        let linhaIndex = fromEnum estado
            desc = if linhaIndex < length matrizEconomiaDasAlmas
                   then let linha = matrizEconomiaDasAlmas !! linhaIndex
                        in if colIndex < length linha
                           then snd (linha !! colIndex)
                           else "N/A"
                   else "N/A"
        in (estado, desc)
      ) indicesEstados

-- | Calcular a "taxa de conversão" filosófica
--   Quanto de filosofia existe em cada estado?
calcularDensidadeFilosofica :: EstadoDivida -> Double
calcularDensidadeFilosofica estado =
  let descricoes = map snd (extrairCategoria Filosofia)
      relevantes = filter (\d -> length d > 50) descricoes  -- Descrições longas têm mais densidade
  in fromIntegral (length relevantes) / fromIntegral (length descricoes)

-- ================== TESTES ==================

testarSistema :: IO ()
testarSistema = do
  putStrLn "🧪 TESTANDO SISTEMA DE REVELAÇÃO"

  -- Teste 1: Localizar descrição específica
  putStrLn "\n1. Buscando descrição (Agiota, Filosofia):"
  case localizarNaMatriz Agiota Filosofia of
    Just desc -> putStrLn $ "   " ++ desc
    Nothing -> putStrLn "   Não encontrado"

  -- Teste 2: Extrair categoria completa
  putStrLn "\n2. Todas as visões da Antropologia:"
  mapM_ (\(estado, desc) ->
          putStrLn $ "   " ++ show estado ++ ": " ++ take 60 desc ++ "..."
        ) (extrairCategoria Antropologia)

  -- Teste 3: Densidade filosófica
  putStrLn "\n3. Densidade Filosófica dos Estados:"
  mapM_ (\estado ->
          putStrLn $ "   " ++ show estado ++ ": " ++
                    show (calcularDensidadeFilosofica estado * 100) ++ "%"
        ) [Limiar .. Agiota]
