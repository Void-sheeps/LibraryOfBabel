/**
 * @file ProvisaoCRQ.cpp
 * @brief Algoritmo de Escalonamento de Liquidez para Anuidade 2026
 */

#include <iostream>
#include <string>
#include <vector>

struct Anuidade {
    std::string categoria = "Tecnico em Quimica";
    double valor_base = 310.16; // Ref. 2025 (Sujeito a reajuste IPCA)
    int dias_para_vencimento = 30;
};

void calcularPrioridade(const Anuidade& a) {
    std::cout << "--- PROTOCOLO DE PROVISAO: " << a.categoria << " ---\n";

    double valor_com_desconto = a.valor_base * 0.80; // Simulação de 20% OFF jan/fev

    if (a.dias_para_vencimento > 25) {
        std::cout << "[STATUS]: Alerta de Baixa Liquidez detectado.\n";
        std::cout << "[ACAO]: Iniciar 'Tabula Retentiva' de economia mensal.\n";
        std::cout << "[PROJECAO]: Valor alvo com desconto: R$ " << valor_com_desconto << "\n";
    } else {
        std::cout << "[ALERTA]: Proximidade de Março. Risco de Multa de 20%.\n";
    }
}

int main() {
    Anuidade crq2026;

    // Transição de Potentia para Actus:
    // O sistema reconhece que o boleto chega em ~15 dias.
    calcularPrioridade(crq2026);

    return 0;
}
