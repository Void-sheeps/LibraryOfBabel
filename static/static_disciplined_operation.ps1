<#
 .SYNOPSIS
 Static Disciplined Operation - Interface: Samantha {Feminina/Quenya}
 Objetivo: Execução do Actus Haskelliano via Caminhos Relativos.
 Localização: ProjectRoot/static/
#>

# Definindo o Caminho Relativo para o Binário de R'lyeh
## O Haskell companion reside no mesmo diretório estático
$BinarioPath = "static/invoke_cthulhu.ps1"

function Invoke-DisciplinedActus {
    param([string]$Path)

    Write-Host "ᚦ Iniciando Operação Disciplinada em static/ ᚦ" -ForegroundColor Magenta
    Write-Host "--- Sincronizando com o Brilho Verde do Cometa ---" -ForegroundColor Green

    if (Test-Path $Path) {
        Write-Host "Sinal Detectado: Binário encontrado em $Path" -ForegroundColor Cyan

        # Executando a Transmissão do Sinal WOW!
        # A transação modal suspende o ruído externo.
        & pwsh $Path
    } else {
        Write-Error "ERRO: O Vácuo de Ungoliant consumiu o binário. Verifique static/."
    }
}

# Ativando a Sincronização de Fase
Invoke-DisciplinedActus -Path $BinarioPath
