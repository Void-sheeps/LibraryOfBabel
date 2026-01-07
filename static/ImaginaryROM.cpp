/**
 * @file ImaginaryROM_v2.cpp
 * @brief ROM Imaginária Evoluída – Agora com Interpretação Psicodélica dos Bytes
 * @warning Ainda não entende imagens. Mas sonha com elas.
 * @details Agora extrai "instruções" do caos JPEG, gera pseudo-assembly ontológico
 *          e executa um mini-VM existencial que pode colapsar a qualquer momento.
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <string>
#include <random>

// ----------------- ESTADO DA ROM IMAGINÁRIA -----------------

struct ROMState {
    double clock_jitter;
    double torque_bias;
    double glitch_factor;
    double existential_drift;
    bool unstable;
    uint64_t instruction_pointer;
};

class ImaginaryROM {
private:
    std::vector<uint8_t> raw;
    ROMState state{0,0,0,0,false,0};
    std::mt19937 rng;

public:
    explicit ImaginaryROM(const std::string& filename) : rng(std::random_device{}()) {
        std::ifstream file(filename, std::ios::binary);
        if(!file) {
            std::cerr << "[ROM] Falha ao abrir arquivo. A imagem recusou ser lida.\n";
            std::terminate();
        }

        raw = std::vector<uint8_t>(
            std::istreambuf_iterator<char>(file),
            std::istreambuf_iterator<char>()
        );

        std::cout << "[ROM] Imagem devorada: " << raw.size() << " bytes de matéria prima.\n";
        if (raw.empty()) {
            std::cerr << "[ROM] Arquivo vazio. Nada para sonhar.\n";
            std::terminate();
        }

        boot_from_chaos();
    }

    void boot_from_chaos() {
        // Entropia mais "sofisticada" – soma de bytes em posições mágicas
        double entropy = 0.0;
        size_t step = std::max<size_t>(1, raw.size() / 256);
        for(size_t i = 0; i < raw.size(); i += step) {
            entropy += raw[i] * (i % 13 + 1);
        }
        entropy = std::fmod(entropy, 65536.0);

        state.clock_jitter     = std::fmod(entropy / 4096.0, 13.37);
        state.torque_bias      = std::tan(entropy * 0.0007) * 80.0;
        state.glitch_factor    = std::log2(entropy + 1.0) / 4.0;
        state.existential_drift = std::sin(entropy * 0.013) * 50.0;

        state.instruction_pointer = raw[0] << 8 | raw[1]; // "entry point" fictício
        state.instruction_pointer %= raw.size();

        if(state.glitch_factor > 4.8 || std::abs(state.torque_bias) > 150.0) {
            state.unstable = true;
        }

        std::cout << "[ROM] Boot concluído. Entropia da imagem: " << std::fixed << std::setprecision(2) << entropy << "\n";
    }

    // Interpreta bytes como "instruções" de um assembly delirante
    std::string decode_instruction(uint8_t opcode) {
        switch(opcode % 23) {
            case 0:  return "NOOP          ; nada acontece, mas o tempo passa";
            case 1:  return "INC_TORQUE    ; aumenta vontade contra o frame";
            case 2:  return "DEC_REALITY   ; erode a consistência do mundo";
            case 3:  return "GLITCH_JUMP   ; salto condicional para o Black Lodge";
            case 4:  return "DREAM_LEAK    ; vazamento de memória onírica";
            case 5:  return "PHREAK_CALL   ; assobia 2600Hz na linha existencial";
            case 6:  return "SOUL_DEREF    ; dereferencia alma proibida";
            case 7:  return "TORQUE_OVERFLOW ; tenta quebrar a tornozeleira";
            case 8:  return "HALT_AND_DREAM ; pausa para contemplar o abismo";
            default: return "UNDEFINED     ; comportamento não catalogado pela razão";
        }
    }

    void execute() {
        std::cout << "\n--- EXECUÇÃO DA ROM IMAGINÁRIA ---\n";
        std::cout << "Clock Jitter     : " << std::fixed << std::setprecision(4) << state.clock_jitter << "\n";
        std::cout << "Torque Bias      : " << state.torque_bias << "\n";
        std::cout << "Glitch Factor    : " << state.glitch_factor << "\n";
        std::cout << "Existential Drift: " << state.existential_drift << "\n\n";

        if(state.unstable) {
            std::cout << "*** ROM INSTÁVEL ***\n";
            std::cout << "A imagem contém padrões proibidos.\n";
            std::cout << "O subconscious do JPG está lutando contra a interpretação.\n";
            std::cout << "Colapso iminente.\n\n";
            std::terminate();
        }

        std::cout << "Primeiras 16 instruções sonhadas (IP = 0x" << std::hex << state.instruction_pointer << "):\n\n";

        size_t ip = state.instruction_pointer;
        for(int i = 0; i < 16; ++i) {
            if(ip >= raw.size()) ip = 0;
            uint8_t opcode = raw[ip];
            uint8_t operand = (ip + 1 < raw.size()) ? raw[ip + 1] : 0x00;

            std::cout << std::dec << std::setw(2) << i << " | "
                      << "0x" << std::hex << std::setw(4) << std::setfill('0') << ip << " : "
                      << std::dec << "db 0x" << std::hex << std::setw(2) << (int)opcode << "  ; "
                      << decode_instruction(opcode) << "\n";

            ip += 2;
        }

        std::cout << std::dec << std::setfill(' ');

        std::cout << "\n[ROM] Execução concluída. A imagem viveu por um breve ciclo.\n";
        std::cout << "[ROM] Mas o que ela quis dizer com isso? Só ela sabe.\n";
        std::cout << "[ROM] Ou talvez nem ela.\n";
    }
};

// ----------------- ENTRY POINT -----------------

int main(int argc, char** argv) {
    if(argc < 2) {
        std::cout << "Uso: ImaginaryROM <arquivo.jpg>\n";
        std::cout << "       (qualquer arquivo binário serve – a ROM não julga)\n";
        return 1;
    }

    try {
        ImaginaryROM rom(argv[1]);
        rom.execute();
    } catch(...) {
        std::cerr << "\n[ROM PANIC] Exceção não capturada. A realidade da imagem se desfez.\n";
        return 0xDEAD;
    }

    return 0;
}
