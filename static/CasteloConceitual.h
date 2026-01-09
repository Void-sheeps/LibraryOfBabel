#ifndef CASTELO_CONCEITUAL_H
#define CASTELO_CONCEITUAL_H

#include <iostream>
#include <vector>
#include <string>

class Teoria {
public:
    std::string enunciado;
    bool colapsada;

    Teoria(std::string e) : enunciado(e), colapsada(false) {}
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
                if (input == -1) {
                    teoria.colapsada = true;
                }
            }
        }
    }

    void statusFinal() {
        std::cout << "--- STATUS FINAL DO CASTELO ---\n";
        for (const auto& teoria : teorias) {
            std::cout << "Teoria: " << teoria.enunciado << " | Estado: " << (teoria.colapsada ? "Colapsada" : "Estável") << std::endl;
        }
    }
};

#endif // CASTELO_CONCEITUAL_H
