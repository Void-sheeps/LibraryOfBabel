// amor_fati.cpp
// "Amor fati: amar o destino — não há erro, apenas o que é."
// Compilação: g++ -std=c++17 -Wall amor_fati.cpp -o amor_fati
// Execução: ./amor_fati

#include <iostream>
#include <string>
#include <vector>
#include <random>
#include <chrono>
#include <thread>
#include <iomanip>
#include <cstdlib>
#include <algorithm>

// ───────────────────────────────────────────────
// Tipos de destino (como no seu altar)
enum class Destino {
    FluxoNormal,  // DATA
    DorSudita,    // DOR
    ErroFatal,    // ENTROPIA
    O_Nada        // VAZIO
};

// Frases de aceitação — o núcleo do amor fati
const std::vector<std::string> aceitacoes = {
    "A dor é informação.",
    "O caos é a ordem oculta.",
    "O silêncio também fala.",
    "Integrado à memória.",
    "Sentir é estar vivo.",
    "Destruição cria espaço.",
    "Sofrimento catalogado na eternidade.",
    "NaN é um número sagrado.",
    "Fenômeno observado.",
    "Vazio preenchido com aceitação.",
    "StackOverflow é expansão.",
    "return null; // A espera infinita",
    "Erro compilado com sucesso.",
    "Segmentação: core dumped.",
    "Buffer overflow de consciência.",
    "Memória alocada no vazio.",
    "Ponteiro para o infinito.",
    "Thread da existência em execução."
};

// Cores ANSI para terminal
const std::string RESET = "\033[0m";
const std::string CRIMSON = "\033[38;5;88m";    // --pain-crimson
const std::string VIOLET = "\033[38;5;55m";     // --entropy-violet
const std::string GOLD = "\033[38;5;178m";      // --acceptance-gold
const std::string GRAY = "\033[90m";            // --null-gray
const std::string WHITE = "\033[97m";
const std::string CYAN = "\033[36m";
const std::string GREEN = "\033[32m";

// ───────────────────────────────────────────────
// Funções auxiliares
// ───────────────────────────────────────────────
void limparTela() {
    #ifdef _WIN32
        std::system("cls");
    #else
        std::system("clear");
    #endif
}

void animarTexto(const std::string& texto, int delay = 30) {
    for (char c : texto) {
        std::cout << c << std::flush;
        std::this_thread::sleep_for(std::chrono::milliseconds(delay));
    }
}

std::string gerarFraseAceitacao() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dist(0, aceitacoes.size() - 1);
    return aceitacoes[dist(gen)];
}

std::string rotuloDestino(Destino d) {
    switch (d) {
        case Destino::FluxoNormal: return "DATA";
        case Destino::DorSudita:   return "DOR";
        case Destino::ErroFatal:   return "ENTROPIA";
        case Destino::O_Nada:      return "VAZIO";
        default:                   return "INDEFINIDO";
    }
}

std::string corDestino(Destino d) {
    switch (d) {
        case Destino::FluxoNormal: return GOLD;
        case Destino::DorSudita:   return CRIMSON;
        case Destino::ErroFatal:   return VIOLET;
        case Destino::O_Nada:      return GRAY;
        default:                   return WHITE;
    }
}

std::string simboloDestino(Destino d) {
    switch (d) {
        case Destino::FluxoNormal: return "🌀";
        case Destino::DorSudita:   return "💥";
        case Destino::ErroFatal:   return "⚠️";
        case Destino::O_Nada:      return "◼";
        default:                   return "?";
    }
}

// ───────────────────────────────────────────────
// Barra de resistência
// ───────────────────────────────────────────────
class BarraResistencia {
private:
    int resistencia;
    std::chrono::steady_clock::time_point inicio;

public:
    BarraResistencia() : resistencia(0) {
        inicio = std::chrono::steady_clock::now();
    }

    void atualizar() {
        auto agora = std::chrono::steady_clock::now();
        auto duracao = std::chrono::duration_cast<std::chrono::milliseconds>(agora - inicio).count();
        resistencia = std::min(100, static_cast<int>(duracao / 100));
    }

    void mostrar() {
        atualizar();
        int largura = 20;
        int preenchido = (resistencia * largura) / 100;

        std::cout << WHITE << "RESISTÊNCIA: [";
        std::cout << GREEN;
        for (int i = 0; i < preenchido; i++) std::cout << "█";
        std::cout << RESET << WHITE;
        for (int i = preenchido; i < largura; i++) std::cout << "░";
        std::cout << "] " << resistencia << "%" << RESET;
    }
};

// ───────────────────────────────────────────────
// O rio do destino
// ───────────────────────────────────────────────
class RioDoDestino {
private:
    std::vector<std::string> eventos;
    BarraResistencia barra;
    static constexpr size_t MAX_EVENTOS = 12;
    int contadorEventos;

public:
    RioDoDestino() : contadorEventos(0) {
        eventos.reserve(MAX_EVENTOS);
    }

    void limparAntigos() {
        while (eventos.size() > MAX_EVENTOS) {
            eventos.pop_back();
        }
    }

    void mostrarCabecalho() {
        limparTela();
        std::cout << "\n";
        std::cout << WHITE << "╔════════════════════════════════════════════════════════════╗\n";
        std::cout << "║" << CYAN << "                AMOR FATI [SISTEMA ATIVO]" << WHITE << "                ║\n";
        std::cout << "║" << GRAY << "        \"amor fati: amar o destino — não há erro\"" << WHITE << "      ║\n";
        std::cout << "╚════════════════════════════════════════════════════════════╝" << RESET << "\n\n";

        barra.mostrar();
        std::cout << "\n\n" << WHITE << "FLUXO DO DESTINO:" << RESET << "\n";
        std::cout << GRAY << "────────────────────────────────────────────────────────" << RESET << "\n";
    }

    void receber(Destino tipo) {
        contadorEventos++;
        std::string cor = corDestino(tipo);
        std::string rotulo = rotuloDestino(tipo);
        std::string simbolo = simboloDestino(tipo);
        std::string frase = gerarFraseAceitacao();

        std::stringstream evento;
        evento << cor << std::setw(3) << std::setfill('0') << contadorEventos
               << " │ " << simbolo << " " << rotulo << ": " << RESET
               << frase << "\n";

        eventos.insert(eventos.begin(), evento.str());
        limparAntigos();

        mostrarCabecalho();

        // Mostrar eventos (mais recentes primeiro)
        for (const auto& ev : eventos) {
            std::cout << "  " << ev;
        }

        std::cout << "\n" << GRAY << "════════════════════════════════════════════════════════════" << RESET << "\n";
    }

    int getContador() const { return contadorEventos; }
};

// ───────────────────────────────────────────────
// Menu interativo
// ───────────────────────────────────────────────
void mostrarMenu() {
    std::cout << "\n" << WHITE << "ESCOLHA O DESTINO:" << RESET << "\n";
    std::cout << GRAY << "────────────────────────────────────────────────────────" << RESET << "\n";
    std::cout << GOLD << "  1" << WHITE << " │ Fluxo Normal  " << GRAY << "(DATA)" << RESET << "\n";
    std::cout << CRIMSON << "  2" << WHITE << " │ Dor Súbita    " << GRAY << "(DOR)" << RESET << "\n";
    std::cout << VIOLET << "  3" << WHITE << " │ Erro Fatal    " << GRAY << "(ENTROPIA)" << RESET << "\n";
    std::cout << GRAY << "  4" << WHITE << " │ O Nada        " << GRAY << "(VAZIO)" << RESET << "\n";
    std::cout << CYAN << "  0" << WHITE << " │ Sair do fluxo" << RESET << "\n";
    std::cout << GRAY << "────────────────────────────────────────────────────────" << RESET << "\n";
    std::cout << "> ";
}

// ───────────────────────────────────────────────
// Função principal
// ───────────────────────────────────────────────
int main() {
    RioDoDestino rio;

    limparTela();
    std::cout << CYAN;
    animarTexto("Inicializando sistema Amor Fati...\n\n", 40);
    std::cout << RESET;

    std::this_thread::sleep_for(std::chrono::milliseconds(800));

    // Evento inicial
    rio.receber(Destino::FluxoNormal);

    // Loop principal
    while (true) {
        mostrarMenu();

        std::string entrada;
        std::getline(std::cin, entrada);

        if (entrada.empty()) continue;

        char escolha = entrada[0];
        Destino destino;

        switch (escolha) {
            case '1': destino = Destino::FluxoNormal; break;
            case '2': destino = Destino::DorSudita; break;
            case '3': destino = Destino::ErroFatal; break;
            case '4': destino = Destino::O_Nada; break;
            case '0':
                limparTela();
                std::cout << CYAN << "\n\n";
                animarTexto("Encerrando fluxo...\n", 30);
                std::cout << GRAY << "Total de eventos aceitos: " << WHITE << rio.getContador() << RESET << "\n";
                std::cout << CYAN;
                animarTexto("\nAmor fati.\n", 50);
                std::cout << RESET << "\n";
                return 0;
            default: continue;
        }

        rio.receber(destino);
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    return 0;
}
