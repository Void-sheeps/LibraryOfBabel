#include <iostream>
#include <vector>
#include <string>
#include <iomanip>
#include <algorithm> // std::clamp

namespace EmpireSilicium {
    struct QuantitativeMetrics {
        double valence;    // -1.0 .. 1.0
        double arousal;    // 0.0 .. 1.0
        double dominance;  // 0.0 .. 1.0
    };

    struct QualitativeAnalysis {
        std::string primaryEmotion;
        std::string artisticCorrelation;
        std::vector<std::string> subtext;
    };

    class ArtAnalyzer {
    private:
        static constexpr int LOGIC_SCALE_WIDTH = 20;
        // fator derivado para mapear [-1,1] -> [0, LOGIC_SCALE_WIDTH-1]
        static constexpr double SCALE_FACTOR = (LOGIC_SCALE_WIDTH - 1) / 2.0;

    public:
        void analyzeQuadrant(int quadrantID, const QuantitativeMetrics& quant, const QualitativeAnalysis& qual) const {
            std::cout << "--- Análise de Quadrante [" << quadrantID << "] ---\n";

            std::cout << std::fixed << std::setprecision(2);
            std::cout << "[QUANTITATIVO]: "
                      << "Valência: " << quant.valence
                      << " | Excitação: " << quant.arousal
                      << " | Dominância: " << quant.dominance << "\n";

            std::cout << "[QUALITATIVO]: " << qual.primaryEmotion << "\n"
                      << ">> Correlação: " << qual.artisticCorrelation << "\n";

            if (!qual.subtext.empty()) {
                std::cout << ">> Subtexto: ";
                for (size_t i = 0; i < qual.subtext.size(); ++i) {
                    std::cout << qual.subtext[i];
                    if (i + 1 < qual.subtext.size()) std::cout << "; ";
                }
                std::cout << "\n";
            }

            renderLogicScale(quant.valence);
            std::cout << "--------------------------------------\n\n";
        }

    private:
        void renderLogicScale(double val) const {
            // normaliza e arredonda para o índice correto
            int pos = static_cast<int>((val + 1.0) * SCALE_FACTOR + 0.5);
            pos = std::clamp(pos, 0, LOGIC_SCALE_WIDTH - 1);

            std::cout << "Escala Lógica: [";
            for (int i = 0; i < LOGIC_SCALE_WIDTH; ++i) {
                std::cout << (i == pos ? "|" : "-");
            }
            std::cout << "]\n";
        }
    };
} // namespace EmpireSilicium

int main() {
    const EmpireSilicium::ArtAnalyzer analyzer;

    analyzer.analyzeQuadrant(1,
        {-0.85, 0.30, 0.20},
        {"Melancolia", "Associação com tons azuis frios e pinceladas longas.", {"Solitude", "Inércia"}}
    );

    analyzer.analyzeQuadrant(2,
        {0.90, 0.85, 0.70},
        {"Euforia", "Cores vibrantes e saturação elevada detectada.", {"Energia", "Expansão"}}
    );

    return 0;
}
