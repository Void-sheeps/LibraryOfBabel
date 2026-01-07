/**
 * @file ReplicationCrisis.cpp
 * @brief O Castelo de Popper sob o Viés de Confirmação
 * @principle Falsificabilidade Ocultada = Crise de Replicação
 */

#include <iostream>
#include <vector>
#include <string>
#include "CasteloConceitual.h"

// --- O Filtro de Viés (A Falácia Fundamentada) ---
class FiltroVies {
public:
    // O observador descarta o "Cisne Negro" para manter o financiamento (Grãos de Feijão)
    static bool filtrarInput(int input) {
        if (input == -1) {
            std::cout << "[VIES]: Ocultando Cisne Negro para evitar o White Crash.\n";
            return true; // Input "suprimido"
        }
        return false;
    }
};

// --- Extensão do seu CasteloConceitual ---
class CasteloViesado : public CasteloConceitual {
public:
    void auditoriaViesada(const std::vector<int>& inputs) {
        std::cout << "--- INICIANDO PESQUISA (COM VIÉS DE SELEÇÃO) ---\n";
        for (int input : inputs) {
            if (!FiltroVies::filtrarInput(input)) {
                // Se não for o Cisne Negro, nós testamos e "brilhamos"
                testarTodas({input});
            }
        }
    }
};

int main() {
    CasteloViesado castle;
    castle.adicionarTeoria(Teoria("Paradigma Inviolável da Estabilidade"));

    std::vector<int> inputs = {1, 16, -1, 42}; // O -1 deveria derrubar o castelo

    castle.auditoriaViesada(inputs);
    castle.statusFinal();

    return 0;
}
