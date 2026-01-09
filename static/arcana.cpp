/*
 * ARCANUM CODEX - Sistema Bibliográfico Lovecraftiano
 * Integra: ISBN, Matemática (Φ), Filatelia, Português-Latim, Cthulhu Mythos
 * Compilar: g++ -std=c++20 -o arcana arcana.cpp
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <regex>
#include <random>
#include <fstream>
#include <memory>

// ================== CONSTANTES ARCANAS ==================
constexpr double PHI = (1.0 + std::sqrt(5.0)) / 2.0;  // Razão Áurea
constexpr double PI = 3.14159265358979323846;
constexpr char CT[] = u8" Ch'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn! ";

// ================== ESTRUTURAS DE DADOS ==================

// Representa um ISBN com validação
class ISBN {
private:
    std::string codigo;
    bool valido;

    bool validarChecksum10(const std::string& isbn) {
        if (isbn.length() != 10) return false;
        int soma = 0;
        for (int i = 0; i < 9; i++) {
            if (!isdigit(isbn[i])) return false;
            soma += (isbn[i] - '0') * (10 - i);
        }
        char digito = isbn[9];
        soma += (digito == 'X' || digito == 'x') ? 10 : (digito - '0');
        return (soma % 11 == 0);
    }

    bool validarChecksum13(const std::string& isbn) {
        if (isbn.length() != 13) return false;
        int soma = 0;
        for (int i = 0; i < 13; i++) {
            if (!isdigit(isbn[i])) return false;
            soma += (isbn[i] - '0') * (i % 2 == 0 ? 1 : 3);
        }
        return (soma % 10 == 0);
    }

public:
    ISBN(const std::string& codigo) : codigo(codigo) {
        // Remover hífens
        std::string limpo;
        std::copy_if(codigo.begin(), codigo.end(),
                    std::back_inserter(limpo),
                    [](char c) { return c != '-'; });

        if (limpo.length() == 10) {
            valido = validarChecksum10(limpo);
        } else if (limpo.length() == 13) {
            valido = validarChecksum13(limpo);
        } else {
            valido = false;
        }
    }

    bool isValid() const { return valido; }
    std::string getCodigo() const { return codigo; }

    std::string getPrefixoGrupo() const {
        std::regex pattern(R"((\d+)-)");
        std::smatch match;
        if (std::regex_search(codigo, match, pattern)) {
            return match[1];
        }
        return "Desconhecido";
    }

    std::string interpretarPrefixo() const {
        std::string prefixo = getPrefixoGrupo();
        std::map<std::string, std::string> interpretacoes = {
            {"0", "Inglês (EUA, UK, etc.)"},
            {"3", "Alemanha, Áustria, Suíça"},
            {"85", "Brasil"},
            {"387", "Springer-Verlag (NY)"},
            {"540", "Springer-Verlag (Berlim)"},
            {"978", "Bookland (EAN para livros)"}
        };
        return interpretacoes.count(prefixo) ? interpretacoes[prefixo] : "Prefixo desconhecido";
    }
};

// Representa uma obra literária
class Obra {
protected:
    std::string titulo;
    std::string autor;
    std::vector<ISBN> isbns;
    int anoPublicacao;

public:
    Obra(const std::string& t, const std::string& a, int ano)
        : titulo(t), autor(a), anoPublicacao(ano) {}

    virtual ~Obra() = default;

    void adicionarISBN(const ISBN& isbn) {
        isbns.push_back(isbn);
    }

    virtual void exibir() const {
        std::cout << "\n📚 OBRA: " << titulo
                  << "\n✍️  Autor: " << autor
                  << "\n📅 Ano: " << anoPublicacao
                  << "\n🏷️  ISBNs: ";
        for (const auto& isbn : isbns) {
            std::cout << "\n   • " << isbn.getCodigo()
                      << " [" << (isbn.isValid() ? "✓ Válido" : "✗ Inválido") << "]"
                      << " (" << isbn.interpretarPrefixo() << ")";
        }
    }

    virtual std::string gerarHashConceitual() const {
        std::string base = titulo + autor + std::to_string(anoPublicacao);
        size_t hash = std::hash<std::string>{}(base);
        return std::to_string(hash);
    }
};

// Obras Lovecraftianas
class ObraLovecraft : public Obra {
private:
    std::string entidadeMythos;
    std::string nivelHorror; // "Cosmico", "Terrivel", "Indescritivel"

public:
    ObraLovecraft(const std::string& t, int ano,
                  const std::string& entidade = "Cthulhu",
                  const std::string& nivel = "Cosmico")
        : Obra(t, "H.P. Lovecraft", ano),
          entidadeMythos(entidade), nivelHorror(nivel) {}

    void exibir() const override {
        Obra::exibir();
        std::cout << "\n👁️  Entidade do Mythos: " << entidadeMythos
                  << "\n💀 Nível de Horror: " << nivelHorror
                  << "\n🔮 Pronúncia correta: "
                  << (entidadeMythos == "Cthulhu" ? "Khlûl'-hloo" : "Indicível");
    }

    std::string gerarHashConceitual() const override {
        std::string base = titulo + entidadeMythos + nivelHorror;
        size_t hash = std::hash<std::string>{}(base);
        return "CTH-" + std::to_string(hash).substr(0, 8);
    }

    std::string invocarEncantamento() const {
        std::map<std::string, std::string> encantamentos = {
            {"Cthulhu", "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn!"},
            {"Nyarlathotep", "Nyarlathotep, o Caos Rastejante, abre os portões!"},
            {"Azathoth", "IA! IA! AZATHOTH! O Demiurgo Cego e Idiota!"},
            {"Yog-Sothoth", "Yog-Sothoth conhece o portão. Yog-Sothoth é o portão!"}
        };
        return encantamentos.count(entidadeMythos) ?
               encantamentos.at(entidadeMythos) :
               "Encantamento desconhecido...";
    }
};

// Dicionário Português-Latim
class Dicionario : public Obra {
private:
    int numEntradas;
    std::string editora;
    std::string codigoPostal;

public:
    Dicionario(const std::string& titulo, int ano, int entradas,
               const std::string& editora, const std::string& codigo)
        : Obra(titulo, "Vários Autores", ano),
          numEntradas(entradas), editora(editora), codigoPostal(codigo) {}

    void exibir() const override {
        Obra::exibir();
        std::cout << "\n🏛️  Editora: " << editora
                  << "\n📮 Código Postal: " << codigoPostal
                  << "\n📖 Entradas: " << numEntradas << " palavras";

        // Exemplo de tradução
        std::cout << "\n🔤 Exemplo de tradução:"
                  << "\n   Português: 'Abismo' → Latim: 'Abyssus'"
                  << "\n   Português: 'Cosmos' → Latim: 'Universum'"
                  << "\n   Português: 'Horror' → Latim: 'Horror'";
    }

    std::string traduzir(const std::string& palavra) const {
        std::map<std::string, std::string> dicionario = {
            {"abismo", "Abyssus"},
            {"cosmos", "Universum"},
            {"horror", "Horror"},
            {"estrela", "Stella"},
            {"antigo", "Antiquus"},
            {"deus", "Deus"},
            {"sonho", "Somnium"},
            {"portal", "Porta"},
            {"conhecimento", "Scientia"},
            {"proibido", "Vetitus"}
        };

        std::string lower = palavra;
        std::transform(lower.begin(), lower.end(), lower.begin(), ::tolower);

        return dicionario.count(lower) ? dicionario.at(lower) : "[Non inveni]";
    }
};

// ================== SISTEMA MATEMÁTICO-FILATÉLICO ==================

class SistemaPhi {
private:
    double phi;

public:
    SistemaPhi() : phi(PHI) {}

    void analisarPhi() const {
        std::cout << std::fixed << std::setprecision(10);
        std::cout << "\n📐 ANÁLISE DA RAZÃO ÁUREA (Φ)"
                  << "\nΦ = " << phi
                  << "\nΦ² = " << (phi * phi)
                  << "\n1/Φ = " << (1.0 / phi)
                  << "\nΦ - 1 = " << (phi - 1.0)
                  << "\n\n⚖️  Propriedade única: Φ² = Φ + 1"
                  << "\n   " << (phi * phi) << " = " << (phi + 1.0)
                  << " [" << (std::abs(phi*phi - (phi+1)) < 1e-10 ? "✓ Correto" : "✗ Errado") << "]";
    }

    void aplicarEmFilatelia() const {
        std::cout << "\n\n🏷️  APLICAÇÃO EM FILATELIA (Φ ≠ ∅)"
                  << "\nA Razão Áurea aparece em:"
                  << "\n• Proporções de selos clássicos"
                  << "\n• Composição de imagens em selos comemorativos"
                  << "\n• Layout de folhas de selos (retângulos áureos)"
                  << "\n• Design de franquias postais históricas";

        // Exemplo de retângulo áureo
        double altura = 100.0; // mm
        double largura = altura * phi;

        std::cout << "\n\n📏 Exemplo de Retângulo Áureo para selo:"
                  << "\nAltura: " << altura << " mm"
                  << "\nLargura: " << largura << " mm"
                  << "\nProporção: " << (largura / altura) << " (≈Φ)";
    }

    std::vector<double> gerarSequenciaFibonacci(int n) const {
        std::vector<double> seq;
        if (n >= 1) seq.push_back(0);
        if (n >= 2) seq.push_back(1);

        for (int i = 2; i < n; i++) {
            seq.push_back(seq[i-1] + seq[i-2]);
        }
        return seq;
    }

    void mostrarRelacaoFibonacci(int n) const {
        auto seq = gerarSequenciaFibonacci(n);
        std::cout << "\n\n🔢 Sequência de Fibonacci (primeiros " << n << " termos):\n";
        for (size_t i = 0; i < seq.size(); i++) {
            std::cout << std::setw(4) << seq[i] << " ";
            if ((i + 1) % 10 == 0) std::cout << "\n";
        }

        if (n >= 3) {
            double razao = seq.back() / seq[seq.size()-2];
            std::cout << "\n\n📈 Razão F(" << (n-1) << ")/F(" << (n-2) << ") = "
                      << razao << " ≈ Φ (" << phi << ")"
                      << "\nDiferença: " << std::abs(razao - phi);
        }
    }
};

// ================== CATÁLOGO INTEGRADO ==================

class CatalogoArcanum {
private:
    std::vector<std::shared_ptr<Obra>> obras;
    SistemaPhi sistemaPhi;

public:
    void adicionarObra(const std::shared_ptr<Obra>& obra) {
        obras.push_back(obra);
    }

    void exibirCatalogo() const {
        std::cout << "\n" << std::string(70, '=')
                  << "\n📚 CATÁLOGO ARCANUM - OBRAS REGISTRADAS"
                  << "\n" << std::string(70, '=')
                  << "\nTotal de obras: " << obras.size();

        int i = 1;
        for (const auto& obra : obras) {
            std::cout << "\n\n" << std::string(50, '-')
                      << "\n[" << i++ << "] ";
            obra->exibir();
            std::cout << "\n🔑 Hash conceitual: " << obra->gerarHashConceitual();
        }
    }

    void buscarPorISBN(const std::string& codigoISBN) const {
        std::cout << "\n🔎 Buscando ISBN: " << codigoISBN;

        ISBN isbnBusca(codigoISBN);
        std::cout << "\nStatus: " << (isbnBusca.isValid() ? "✓ Válido" : "✗ Inválido");

        bool encontrado = false;
        for (const auto& obra : obras) {
            // Nota: Na implementação real, cada obra teria método para verificar ISBN
            // Simplificação para demonstração
            if (obra->gerarHashConceitual().find(codigoISBN.substr(0, 3)) != std::string::npos) {
                std::cout << "\n📖 Encontrado em: " << typeid(*obra).name();
                encontrado = true;
            }
        }

        if (!encontrado) {
            std::cout << "\n⚠️  Não encontrado no catálogo atual";
        }
    }

    void executarAnaliseCompleta() const {
        std::cout << "\n" << std::string(70, '=')
                  << "\n🔮 ANÁLISE ARCANA COMPLETA"
                  << "\n" << std::string(70, '=');

        // 1. Análise matemática
        sistemaPhi.analisarPhi();
        sistemaPhi.aplicarEmFilatelia();
        sistemaPhi.mostrarRelacaoFibonacci(15);

        // 2. Relação entre as obras
        std::cout << "\n\n" << std::string(70, '-')
                  << "\n🧩 RELAÇÕES CONCEITUAIS ENTRE AS OBRAS:"
                  << "\n\n• O dicionário Português-Latim fornece as raízes linguísticas"
                  << "\n  para os nomes lovecraftianos (ex: 'Cthulhu' tem ecos latinos)"
                  << "\n\n• A Razão Áurea (Φ) aparece em:"
                  << "\n  - Design de edições especiais de Lovecraft"
                  << "\n  - Proporções em ilustrações do Mythos"
                  << "\n  - Estruturas narrativas (clímax na proporção áurea)"
                  << "\n\n• Os ISBNs formam uma rede bibliográfica que conecta:"
                  << "\n  - Edições alemãs (3-540) com edições brasileiras (85)"
                  << "\n  - Obras acadêmicas (387) com ficção popular";

        // 3. Conclusão filosófica
        std::cout << "\n\n" << std::string(70, '-')
                  << "\n💭 CONCLUSÃO FILOSÓFICA:"
                  << "\n\n\"A busca pelo conhecimento (Φ ≠ ∅) nos leva das"
                  << "\nraízes linguísticas (Latim) aos horrores cósmicos (Cthulhu),"
                  << "\npassando pela perfeição matemática que estrutura"
                  << "\ntanto os selos postais quanto os pesadelos literários.\""
                  << "\n\n\t\t— SISTEMA ARCANUM, " << __DATE__;
    }

    void gerarArtefatoFinal() const {
        std::cout << "\n\n" << std::string(70, '=')
                  << "\n🎴 ARTEFATO FINAL: SELO FILATÉLICO LOVEcraftIANO"
                  << "\n" << std::string(70, '=');

        // ASCII Art do selo
        std::cout << R"(
  ┌──────────────────────────────────────────┐
  │   PORTAL DE R'LYEH                       │
  │                                          │
  │      ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░       │
  │   ░░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░   │
  │ ░░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░ │
  │ ░░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░ │
  │   ░░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░   │
  │      ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░       │
  │               Φ = 1.618...               │
  │      Cthulhu Fhtagn! • R'lyeh • 1926     │
  │              ISBN 85-98966-96-0          │
  └──────────────────────────────────────────┘
        )";

        std::cout << "\n\n📝 METADADOS DO ARTEFATO:"
                  << "\n• Dimensões: 100mm × 161.8mm (Retângulo Áureo)"
                  << "\n• Cores: Verde abissal, Púrpura cósmica"
                  << "\n• Valor facial: Φ unidades cósmicas"
                  << "\n• Tiragem: Limitada a 13 exemplares"
                  << "\n• Referências: "
                  << "\n  1. H.P. Lovecraft - 'O Chamado de Cthulhu'"
                  << "\n  2. Porto Editora - Dicionário Português-Latim"
                  << "\n  3. Springer-Verlag - Edições acadêmicas"
                  << "\n  4. Φ ≠ ∅ - Princípio da Existência Matemática";
    }
};

// ================== FUNÇÃO PRINCIPAL ==================

int main() {
    std::cout << "=" << std::string(68, '=') << "=\n"
              << " ARCANUM CODEX v1.0 - Sistema de Análise Bibliográfica\n"
              << " Integrando: ISBN • Φ • Cthulhu Mythos • Filatelia • Latim\n"
              << "=" << std::string(68, '=') << "=\n";

    // Criar catálogo
    CatalogoArcanum catalogo;

    // 1. Adicionar obra Lovecraftiana
    auto lovecraft = std::make_shared<ObraLovecraft>(
        "O Chamado de Cthulhu", 1928, "Cthulhu", "Cosmico"
    );
    lovecraft->adicionarISBN(ISBN("0-387-96787-7"));
    lovecraft->adicionarISBN(ISBN("3-540-96787-7"));
    lovecraft->adicionarISBN(ISBN("978-85-98966-96-0"));
    catalogo.adicionarObra(lovecraft);

    // 2. Adicionar dicionário Português-Latim
    auto dicionario = std::make_shared<Dicionario>(
        "Dicionário de Português-Latim", 1995, 50000,
        "Porto Editora", "4099 Porto Codex"
    );
    dicionario->adicionarISBN(ISBN("972-0-00000-0")); // ISBN exemplo
    catalogo.adicionarObra(dicionario);

    // 3. Exibir catálogo
    catalogo.exibirCatalogo();

    // 4. Demonstrações específicas
    std::cout << "\n\n" << std::string(70, '=')
              << "\n🎭 DEMONSTRAÇÕES ESPECÍFICAS"
              << "\n" << std::string(70, '=');

    // Demonstração Lovecraft
    std::cout << "\n\n" << std::string(50, '-')
              << "\n🐙 ENCANTAMENTO LOVECRAFTIANO:";
    std::cout << "\n" << reinterpret_cast<const char*>(CT);
    std::cout << "\n" << lovecraft->invocarEncantamento();

    // Demonstração Dicionário
    std::cout << "\n\n" << std::string(50, '-')
              << "\n📖 TRADUÇÕES PORTUGUÊS-LATIM:";
    std::vector<std::string> palavras = {"abismo", "cosmos", "horror", "conhecimento", "proibido"};
    for (const auto& palavra : palavras) {
        std::cout << "\n   " << palavra << " → " << dicionario->traduzir(palavra);
    }

    // 5. Busca por ISBN
    std::cout << "\n\n" << std::string(50, '-')
              << "\n🔍 VALIDAÇÃO DE ISBNs DO QUERY ORIGINAL:";

    std::vector<std::string> isbnsTeste = {
        "0-387-96787-7",
        "3-540-96787-7",
        "978-85-98966-96-0",
        "972-0-00000-0" // Inválido de propósito
    };

    for (const auto& isbnStr : isbnsTeste) {
        ISBN isbn(isbnStr);
        std::cout << "\n\nISBN: " << isbnStr
                  << "\n  Válido: " << (isbn.isValid() ? "✓ Sim" : "✗ Não")
                  << "\n  Prefixo: " << isbn.getPrefixoGrupo()
                  << " → " << isbn.interpretarPrefixo();
    }

    // 6. Análise completa
    catalogo.executarAnaliseCompleta();

    // 7. Gerar artefato final
    catalogo.gerarArtefatoFinal();

    // 8. Mensagem final
    std::cout << "\n\n" << std::string(70, '=')
              << "\n✨ ANÁLISE CONCLUÍDA"
              << "\n" << std::string(70, '=')
              << "\n\n\"Do ISBN à invocação, do Φ ao horror cósmico,\n"
              << "este sistema demonstra que o conhecimento é uma teia\n"
              << "que conecta matemática, literatura e colecionismo.\"\n\n";

    std::cout << "Data da análise: " << __DATE__ << " " << __TIME__ << "\n";

    return 0;
}
