// MatrixDissonance.cpp
// Demonstra: qualia não entram; vetores entram; indefinição colapsa em booleano.

#include <iostream>
#include <vector>
#include <cmath>
#include <string>

// ───────────────────────────────────────────────
// Representação vetorial (proxy estatístico)
using Vector = std::vector<float>;

// "Embeddings" parciais (compressão destrutiva)
Vector embedDor()        { return {0.89f, 0.12f, -0.55f}; }
Vector embedSacrificio() { return {0.88f, 0.15f, -0.50f}; }

// Métrica escolhida (arbitrária, mas obrigatória)
float euclidean(const Vector& a, const Vector& b) {
    if (a.size() != b.size()) return INFINITY; // fora do domínio
    float s = 0.0f;
    for (size_t i = 0; i < a.size(); ++i) {
        float d = a[i] - b[i];
        s += d * d;
    }
    return std::sqrt(s);
}

// Interpretação algorítmica:
// NÃO retorna qualia. Retorna rótulo após decisão booleana.
std::string interpret(const Vector& v) {
    const float THRESHOLD = 0.01f; // política disfarçada de número

    float dDor = euclidean(v, embedDor());
    // Colapso booleano inevitável:
    if (dDor < THRESHOLD) {
        return "Padrao estatistico #42 detectado";
    }
    return "Ruido";
}

int main() {
    Vector observado = embedSacrificio(); // experiência já perdida
    std::cout << interpret(observado) << "\n";
    return 0;
}
