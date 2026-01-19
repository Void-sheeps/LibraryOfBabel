#include <iostream>
#include <vector>
#include <string>
#include <iomanip>
#include <algorithm>

namespace EmpireSilicium {
    // Estrutura para Mensuração Quantitativa (Dados Brutos)
    struct QuantitativeMetrics {
        double valence;    // Quão positiva ou negativa (-1.0 a 1.0)
        double arousal;    // Nível de energia/excitação (0.0 a 1.0)
        double dominance;  // Nível de controle/submissão (0.0 a 1.0)
    };

    // Estrutura para Mensuração Qualitativa (Categorização Semântica)
    struct QualitativeAnalysis {
        std::string primaryEmotion;
        std::string artisticCorrelation; // Link com o experimento Arts & Culture
        std::vector<std::string> subtext;
    };

    class ArtAnalyzer {
    private:
        // SUGGESTION 3: Use named constants instead of magic numbers.
        // static constexpr makes them compile-time constants shared by all instances.
        static constexpr int LOGIC_SCALE_WIDTH = 20;
        static constexpr double LOGIC_SCALE_MULTIPLIER = 10.0;

    public:
        // SUGGESTION 1 & 2: Pass structs by const reference and mark the method as const.
        void analyzeQuadrant(int quadrantID, const QuantitativeMetrics& quant, const QualitativeAnalysis& qual) const {
            std::cout << "--- Análise de Quadrante [" << quadrantID << "] ---\n";

            // Output Quantitativo
            std::cout << "[QUANTITATIVO]: "
                      << "Valência: " << std::fixed << std::setprecision(2) << quant.valence
                      << " | Excitação: " << quant.arousal << "\n";
            // Output Qualitativo
            std::cout << "[QUALITATIVO]: " << qual.primaryEmotion << "\n"
                      << ">> Correlação: " << qual.artisticCorrelation << "\n";

            renderLogicScale(quant.valence);
            std::cout << "--------------------------------------\n\n";
        }

    private:
        // SUGGESTION 2: Mark helper methods as const as well if they don't modify state.
        void renderLogicScale(double val) const {
            // The formula now uses the named constants.
            int scale_position = static_cast<int>((val + 1.0) * LOGIC_SCALE_MULTIPLIER);

            // Clamp the value to be within the scale's bounds to prevent out-of-bounds access
            scale_position = std::max(0, std::min(LOGIC_SCALE_WIDTH - 1, scale_position));

            std::cout << "Escala Lógica: [";
            for (int i = 0; i < LOGIC_SCALE_WIDTH; ++i) {
                std::cout << (i == scale_position ? "|" : "-");
            }
            std::cout << "]\n";
        }
    };
} // namespace EmpireSilicium

int main() {
    // The analyzer can be const now, as its methods are const.
    const EmpireSilicium::ArtAnalyzer analyzer;

    // Simulação de análise de uma obra (ex: O Grito ou Noite Estrelada)
    // C++17's designated initializers could make this even clearer, but this is fine.

    // Quadrante 1: Melancolia/Tristeza
    analyzer.analyzeQuadrant(1,
        {-0.85, 0.30, 0.20},
        {"Melancolia", "Associação com tons azuis frios e pinceladas longas.", {"Solitude", "Inércia"}}
    );

    // Quadrante 2: Êxtase/Alegria
    analyzer.analyzeQuadrant(2,
        {0.90, 0.85, 0.70},
        {"Euforia", "Cores vibrantes e saturação elevada detectada.", {"Energia", "Expansão"}}
    );

    return 0;
}
