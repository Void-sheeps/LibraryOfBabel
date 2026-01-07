/**
 * @file PopperScientificMethod.cpp
 * @brief Crítica a posteriori da Falseabilidade de Karl Popper
 * @principle Corroboration != Verification
 */

#include <iostream>
#include <string>
#include <vector>

// --- Taxonomia da Teoria ---
class TeoriaCientifica {
public:
    std::string enunciado;
    bool colapsada;

    TeoriaCientifica(std::string e) : enunciado(e), colapsada(false) {}

    // O Teste de Popper: Procura pelo White Crash
    void testar(int observacao_x) {
        if (observacao_x == -1) { // O "Cisne Negro"
            colapsada = true;
            std::cout << "[FALSIFIED]: A teoria '" << enunciado << "' ruiu.\n";
        } else {
            std::cout << "[CORROBORATED]: A teoria resiste ao Input " << observacao_x << ".\n";
        }
    }
};

int main() {
    std::cout << "--- MÉTODO CIENTÍFICO: RIGOR DE POPPER ---\n";

    TeoriaCientifica t1("Todos os sinais são Estáveis (Base 16)");

    std::vector<int> experimentos = {1, 16, 256, -1}; // O último é o erro

    for (int ex : experimentos) {
        if (!t1.colapsada) {
            t1.testar(ex);
        } else {
            std::cout << "[STATUS]: Teoria em estado de Logical Nullity.\n";
            break;
        }
    }

    std::cout << "\n[CRÍTICA]: A verdade é apenas o erro que ainda não ocorreu.\n";
    std::cout << "[COMANDO]: F1+Esc para descartar o paradigma obsoleto.\n";

    return 0;
}
