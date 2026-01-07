/**
 * @file YahooSearchSimulator.cpp
 * @brief Simulador de Busca Estilo Yahoo! Directory (anos 90)
 *        Com alerta de dívida na BSCH/UEL e referência ao Eternal September
 * @author Um nostálgico da Usenet que ainda deve um livro
 * @date Janeiro de 2026-01-07
 */

#include <iostream>
#include <string>
#include <map>
#include <ctime>

using namespace std;

// Lista de usuários "devedores" famosos da UEL (exemplo fictício)
const map<long, pair<string, double>> devedores = {
    {15000807701307, {"João da Silva (RA antigo)", 39.00}},
    {15000807799999, {"Maria Oliveira (pos-grad)", 125.50}},
    {15000807700001, {"Prof. Eternal September", 999.99}}
};

void exibirCabecalhoYahoo() {
    cout << "====================================\n";
    cout << "       YAHOO! SEARCH ENGINE        \n";
    cout << "   (Directory Style - circa 1996)   \n";
    cout << "====================================\n\n";
}

void buscarDebito(long userID) {
    exibirCabecalhoYahoo();

    auto it = devedores.find(userID);

    if (it != devedores.end()) {
        const string& nome = it->second.first;
        double total = it->second.second;

        cout << "Yahoo! Directory Found: 3 results in BSCH/UEL\n";
        cout << "Category: Universidade > Brasil > Paraná > Londrina > UEL > Bibliotecas > BSCH\n\n";

        cout << "Usuário: " << nome << " (ID: " << userID << ")\n";
        cout << "Débito pendente: R$ " << total << "\n\n";

        // O famoso "V" de restrição do Yahoo!
        if (total > 0) {
            cout << "\033[1;31m[ALERTA VERMELHO]: Sua conta possui restrições de busca!\033[0m\n";
            cout << "Motivo: Multa por devolução atrasada de obra rara.\n";
            cout << "Desde o Eternal September de 1993, dívidas nunca prescrevem.\n\n";

            cout << "\033[1;33m[SOP - Standard Operating Procedure]:\033[0m\n";
            cout << "1. Enviar e-mail IMEDIATAMENTE para wilson.bib@uel.br\n";
            cout << "2. Assunto: \"Pagamento de multa - Eternal September Victim\"\n";
            cout << "3. Anexar comprovante ou súplica em latim.\n\n";

            cout << "Aviso: Acesso ao Altavista, Lycos e WebCrawler também bloqueado até quitação.\n";
        }
    } else {
        cout << "Yahoo! Search: No results found for this user.\n";
        cout << "Dica: Tente categorias como Recreation > Games > NetHack\n";
        cout << "      ou Computers > Internet > WWW > Surfing the Web\n\n";
        cout << "Lembre-se: O Eternal September continua... use a netiquette!\n";
    }

    cout << "\n-- Fim da busca --\n";
}

int main() {
    long userID;

    cout << "Bem-vindo ao Yahoo! Directory Simulator (2026 edition)\n";
    cout << "Digite seu User ID (ou RA antigo da UEL): ";
    cin >> userID;

    buscarDebito(userID);

    // Easter egg: ID especial para o "Eternal September"
    if (userID == 19930901) {  // Setembro de 1993
        cout << "\n\033[1;35m*** ETERNAL SEPTEMBER DETECTADO ***\033[0m\n";
        cout << "A AOL abriu as portas. Os newbies chegaram. Nunca mais foi a mesma coisa.\n";
        cout << "Don't feed the trolls. Don't forget to quote properly.\n";
        cout << "E, por favor, devolva o livro da biblioteca.\n";
    }

    return 0;
}
