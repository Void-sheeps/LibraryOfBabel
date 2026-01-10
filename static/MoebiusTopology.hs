module MoebiusTopology where

import MnemosynisCalibrator (SystemState(..))

-- | A Função de Transição de Möbius
-- Não há entrada nem saída, apenas transformação contínua.
moebiusLoop :: SystemState -> SystemState
moebiusLoop state =
    case state of
        Acidified -> Blank     -- A Torção: O Caos purifica-se no Vazio (Reset)
        Blank     -> Balanced  -- O Fluxo: Do Vazio emerge a Ordem (Logos)
        Balanced  -> Acidified -- O Desgaste: A Ordem gera Entropia (Tempo/C14)

-- | Trajetória do Peregrino (Simulação de N ciclos)
-- Permite visualizar a "História" do sistema através das gerações.
traverseStrip :: Int -> SystemState -> [(Int, SystemState, String)]
traverseStrip 0 _ = []
traverseStrip n state =
    let nextState = moebiusLoop state
        meaning = interpretPhase state
    in (n, state, meaning) : traverseStrip (n-1) nextState

-- | Hermenêutica da Fase Atual
interpretPhase :: SystemState -> String
interpretPhase Acidified = "Colapso Entrópico -> Acionando Torção (Reset)..."
interpretPhase Blank     = "Tabula Rasa -> Inscrevendo Leis (Calibração)..."
interpretPhase Balanced  = "Homeostase -> Acumulando Tempo (Decaimento)..."
