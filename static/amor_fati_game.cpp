// amor_fati_game.cpp
// "Amor Fati: O Jogo da Aceitação"
// Uma simulação ludificada do Estoicismo Cibernético.
//
// Compilação: g++ -std=c++17 -pthread amor_fati_game.cpp -o amor_fati_game
// Execução: ./amor_fati_game

#include <iostream>
#include <string>
#include <vector>
#include <random>
#include <chrono>
#include <thread>
#include <iomanip>
#include <cstdlib>
#include <algorithm>
#include <atomic>
#include <future>
#include <mutex>

// ───────────────────────────────────────────────
// DEFINIÇÕES ESTÉTICAS (ANSI CODES)
// ───────────────────────────────────────────────
const std::string RESET   = "\033[0m";
const std::string BOLD    = "\033[1m";
const std::string CRIMSON = "\033[38;5;88m";  // Dor
const std::string VIOLET  = "\033[38;5;93m";  // Entropia
const std::string GOLD    = "\033[38;5;220m"; // Fluxo/Glória
const std::string GRAY    = "\033[90m";       // Vazio
const std::string WHITE   = "\033[97m";
const std::string CYAN    = "\033[36m";
const std::string GREEN   = "\033[32m";
const std::string RED     = "\033[31m";

// ───────────────────────────────────────────────
// ESTRUTURAS DO DESTINO
// ───────────────────────────────────────────────

enum class TipoDestino {
    FluxoNormal,  // Eventos cotidianos
    DorSudita,    // A provação estoica
    ErroFatal,    // A quebra da lógica (NaN)
    O_Nada        // O silêncio
};

struct Evento {
    TipoDestino tipo;
    std::string nome;
    std::string fraseFilosofica;
    bool deveAceitar; // A "Vontade do Destino" (Simon Says)
    int pontos;
    int tempoLimiteMs;
};

struct Jogador {
    int aceitacao;    // HP
    int resistencia;  // Stamina/Mana
    int pontuacao;
    int combo;
    int nivel;
};

// ───────────────────────────────────────────────
// BANCO DE DADOS MNEMOSYNE
// ───────────────────────────────────────────────

const std::vector<std::string> frasesAceitacao = {
    "A dor é informação de alta prioridade.",
    "O caos é apenas uma ordem não compilada.",
    "O silêncio é a sintaxe do infinito.",
    "Integrado à memória com sucesso.",
    "Sentir é processar dados brutos.",
    "A destruição libera endereços de memória.",
    "NaN é um número sagrado.",
    "StackOverflow é expansão de consciência.",
    "O erro fatal é uma oportunidade de reboot."
};

// ───────────────────────────────────────────────
// MOTOR DO JOGO
// ───────────────────────────────────────────────

class AmorFatiEngine {
private:
    Jogador jogador;
    bool rodando;
    std::mt19937 rng;

    // Utilitários de Tela
    void limparTela() {
        #ifdef _WIN32
            std::system("cls");
        #else
            std::system("clear");
        #endif
    }

    void desenharBarra(std::string label, int valor, int max, std::string cor) {
        int tamanho = 20;
        int preenchido = (valor * tamanho) / max;
        std::cout << label << " [";
        std::cout << cor;
        for(int i=0; i<preenchido; ++i) std::cout << "█";
        std::cout << GRAY;
        for(int i=preenchido; i<tamanho; ++i) std::cout << "░";
        std::cout << RESET << "] " << valor << "%" << std::endl;
    }

    // Gerador de Eventos
    Evento gerarEvento() {
        std::uniform_int_distribution<int> distTipo(0, 3);
        std::uniform_int_distribution<int> distFrase(0, frasesAceitacao.size() - 1);
        std::uniform_int_distribution<int> distAcao(0, 100);

        Evento e;
        e.tipo = static_cast<TipoDestino>(distTipo(rng));
        e.fraseFilosofica = frasesAceitacao[distFrase(rng)];

        // Determina dificuldade baseada no nível
        int baseTempo = std::max(1000, 3000 - (jogador.nivel * 200));
        e.tempoLimiteMs = baseTempo;

        // 80% das vezes devemos aceitar (Amor Fati), 20% rejeitar (Discenimento)
        e.deveAceitar = distAcao(rng) < 80;

        switch (e.tipo) {
            case TipoDestino::FluxoNormal:
                e.nome = GOLD + "FLUXO DE DADOS" + RESET;
                e.pontos = 100;
                break;
            case TipoDestino::DorSudita:
                e.nome = CRIMSON + "DOR SÚBITA (CRITICAL)" + RESET;
                e.pontos = 300; // Aceitar a dor vale mais
                break;
            case TipoDestino::ErroFatal:
                e.nome = VIOLET + "ENTROPIA / GLITCH" + RESET;
                e.pontos = 500; // Aceitar o erro vale muito
                break;
            case TipoDestino::O_Nada:
                e.nome = GRAY + "O VAZIO (VOID)" + RESET;
                e.pontos = 150;
                break;
        }
        return e;
    }

public:
    AmorFatiEngine() : rng(std::random_device{}()) {
        jogador = {100, 100, 0, 0, 1};
        rodando = true;
    }

    void titulo() {
        std::cout << VIOLET << R"(
    ___    __  _______  ____     _______  __________
   /   |  /  |/  / __ \/ __ \   / ____/ |/ /_  __/ /
  / /| | / /|_/ / / / / /_/ /  / /_  /    / / / / /
 / ___ |/ /  / / /_/ / _, _/  / __/ /   |/ / / /_/
/_/  |_/_/  /_/\____/_/ |_|  /_/   /_/|_/ /_/ (_)
        )" << RESET << "\n";
        std::cout << "      " << GRAY << "O Jogo da Aceitação Cibernética" << RESET << "\n\n";
    }

    void gameLoop() {
        while (rodando && jogador.aceitacao > 0) {
            limparTela();
            titulo();

            // HUD
            std::cout << BOLD << "NÍVEL: " << jogador.nivel << " | SCORE: " << jogador.pontuacao
                      << " | COMBO: " << jogador.combo << "x" << RESET << "\n";
            desenharBarra("ACEITAÇÃO  ", jogador.aceitacao, 100, GREEN);
            desenharBarra("RESISTÊNCIA", jogador.resistencia, 100, CYAN);
            std::cout << "--------------------------------------------------\n\n";

            // Gerar e Mostrar Evento
            Evento ev = gerarEvento();
            std::cout << "O Destino apresenta: " << ev.nome << "\n";
            std::cout << "Log do Sistema: \"" << GRAY << ev.fraseFilosofica << RESET << "\"\n\n";

            std::cout << "A Necessidade exige: ";
            if (ev.deveAceitar)
                std::cout << GREEN << "[ A ] CEITAR" << RESET << "\n";
            else
                std::cout << RED << "[ R ] EJEITAR (Ilusão)" << RESET << "\n";

            std::cout << "\nTempo para reação: " << std::fixed << std::setprecision(1)
                      << (ev.tempoLimiteMs / 1000.0) << "s\n";
            std::cout << "> ";

            // Lógica de Input com Timeout (Async)
            auto futuroInput = std::async(std::launch::async, [](){
                char c;
                std::cin >> c;
                return std::toupper(c);
            });

            // Esperar pelo input ou timeout
            std::future_status status = futuroInput.wait_for(std::chrono::milliseconds(ev.tempoLimiteMs));

            if (status == std::future_status::timeout) {
                // Timeout
                std::cout << "\n\n" << RED << "TIMEOUT! O fluxo passou por você." << RESET << "\n";
                jogador.aceitacao -= 10;
                jogador.combo = 0;
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            }
            else if (status == std::future_status::ready) {
                // Input recebido
                char input = futuroInput.get();
                bool acerto = false;

                if (ev.deveAceitar && input == 'A') acerto = true;
                else if (!ev.deveAceitar && input == 'R') acerto = true;

                if (acerto) {
                    std::cout << "\n\n" << GREEN << "ALINHAMENTO BEM SUCEDIDO." << RESET << "\n";
                    int pontosGanhos = ev.pontos * (1 + (0.1 * jogador.combo));
                    jogador.pontuacao += pontosGanhos;
                    jogador.combo++;
                    jogador.resistencia = std::min(100, jogador.resistencia + 5);

                    // Nível sobe a cada 1000 pontos
                    if (jogador.pontuacao / 1000 > jogador.nivel - 1) {
                        jogador.nivel++;
                        jogador.aceitacao = std::min(100, jogador.aceitacao + 20); // Heal
                    }
                } else {
                    std::cout << "\n\n" << RED << "DISSONÂNCIA COGNITIVA DETECTADA." << RESET << "\n";
                    jogador.aceitacao -= (ev.tipo == TipoDestino::ErroFatal ? 20 : 10);
                    jogador.combo = 0;
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(800));
            }
        }
        gameOver();
    }

    void gameOver() {
        limparTela();
        std::cout << RED << R"(
  ___ ___  __  __ ___   _____   _____ ___
 / __/ _ \|  \/  | __| |__ \ \ / / __| _ \
| (_| (_) | |\/| | _|  |___|\ V /| _||   /
 \___\___/|_|  |_|___|       \_/ |___|_|_\
        )" << RESET << "\n\n";

        std::cout << "Sua resistência falhou. O Real colapsou sobre você.\n";
        std::cout << "Score Final: " << GOLD << jogador.pontuacao << RESET << "\n";
        std::cout << "Nível Alcançado: " << jogador.nivel << "\n\n";
        std::cout << "O sistema será reiniciado em estado de tábula rasa.\n";
    }
};

int main() {
    AmorFatiEngine jogo;
    jogo.gameLoop();
    return 0;
}
