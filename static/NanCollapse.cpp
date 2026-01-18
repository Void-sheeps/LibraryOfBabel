#include <iostream>
#include <cmath>
#include <limits>

int main() {
    float score = std::numeric_limits<float>::quiet_NaN();
    const float TH = 0.01f;

    if (score < TH) {
        std::cout << "ACEITO\n";
    } else {
        std::cout << "FALLBACK\n";
    }
    return 0;
}
