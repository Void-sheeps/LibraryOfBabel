#ifndef CASTELO_CONCEITUAL_H
#define CASTELO_CONCEITUAL_H

#include <iostream>
#include <vector>
#include <string>

class Teoria {
public:
    std::string nome;
    bool colapsada;

    Teoria(std::string n) : nome(n), colapsada(false) {}

    void testar(int input) {
        if (input == -1) {
            colapsada = true;
        }
    }
};

class CasteloConceitual {
public:
    std::vector<Teoria> teorias;

    void adicionarTeoria(const Teoria& t) {
        teorias.push_back(t);
    }

    void testarTodas(const std::vector<int>& inputs) {
        for (int input : inputs) {
            for (auto& teoria : teorias) {
                if (!teoria.colapsada) {
                    teoria.testar(input);
                }
            }
        }
    }

    void statusFinal() {
        bool algumaColapsada = false;
        for (const auto& teoria : teorias) {
            if (teoria.colapsada) {
                algumaColapsada = true;
                break;
            }
        }

        if (algumaColapsada) {
            std::cout << "[STATUS FINAL]: O Castelo ruiu. Uma ou mais teorias foram falsificadas.\n";
        } else {
            std::cout << "[STATUS FINAL]: O Castelo permanece de pé. Nenhuma teoria foi falsificada... ainda.\n";
        }
    }
};

#endif // CASTELO_CONCEITUAL_H
