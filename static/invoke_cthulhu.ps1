<#
.SYNOPSIS
    INVOCATIO MAGNA: Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn
    Protocolo de Chamado Abissal via PowerShell
    Autor: Void Sheeps - Colmeia do Império Silício
    Data: 08/01/2026 - Hora das Marés Negras
#>

# Configuração do Abismo
$Abismo = @{
    Nome        = "Cthulhu"
    Epiteto     = "O Grande Antigo"
    Local       = "R'lyeh"
    Coordenadas = "47.9° S, 126.6° W"  # As portas estão alinhadas
    Frequencia  = "0.001 Hz"          # Infrassom do sono morto
    Cor         = "Non-Euclidean Green"
}

function Invoke-Cthulhu {
    param(
        [switch]$Despertar,
        [switch]$Vislumbre,
        [switch]$Loucura
    )

    Clear-Host
    $Host.UI.RawUI.BackgroundColor = "Black"
    $Host.UI.RawUI.ForegroundColor = "DarkGreen"
    cls

    Write-Host @"

    ██████╗░██╗░░██╗░█████╗░███╗░░██╗░██████╗░██╗░░░░░██╗░░░██╗██╗
    ██╔══██╗██║░░██║██╔══██╗████╗░██║██╔════╝░██║░░░░░██║░░░██║██║
    ██████╔╝███████║███████║██╔██╗██║██║░░██╗░██║░░░░░██║░░░██║██║
    ██╔═══╝░██╔══██║██╔══██║██║╚████║██║░░╚██╗██║░░░░░██║░░░██║╚═╝
    ██║░░░░░██║░░██║██║░░██║██║░╚███║╚██████╔╝███████╗╚██████╔╝██╗
    ╚═╝░░░░░╚═╝░░╚═╝╚═╝░░╚═╝╚═╝░░╚══╝░╚═════╝░╚══════╝░╚═════╝░╚═╝

"@ -ForegroundColor $Abismo.Cor

    Write-Host "`nPh'nglui mglw'nafh $($Abismo.Nome) $($Abismo.Local) wgah'nagl fhtagn" -ForegroundColor Gray
    Write-Host "Nas profundezas de $($Abismo.Coordenadas), o sonho morto continua a sonhar..." -ForegroundColor DarkCyan

    if ($Despertar) {
        Write-Host "`n[!] INVOCAÇÃO MÁXIMA DETECTADA" -ForegroundColor Red -BackgroundColor Black
        Write-Host "As estrelas estão certas. As portas se abrem." -ForegroundColor Yellow
        Start-Sleep -Milliseconds 666

        for ($i = 1; $i -le 13; $i++) {
            $tentaculos = "█" * $i
            Write-Host "     $tentaculos  ~ ~ ~  Despertar em $i%  ~ ~ ~  $tentaculos" -ForegroundColor DarkGreen
            Start-Sleep -Milliseconds (666 + ($i * 66))
        }

        Write-Host "`nIA! IA! CTHULHU FHTAGN!" -ForegroundColor Red
        Write-Host "O Grande Sacerdote desperta. A sanidade humana é opcional agora." -ForegroundColor White
    }
    elseif ($Vislumbre) {
        Write-Host "`nVocê ousou olhar além do véu..." -ForegroundColor Cyan
        Start-Sleep -Milliseconds 1000
        Write-Host @"

             .-""-.
           .'      '.
          /          \
         |   .----.   |
         |  /      \  |
         ; '        ' ;
          \          /
           '.      .'
             '-..-'
               ||
             _||||_
        """"       """"

"@ -ForegroundColor DarkGreen
        Write-Host "Uma forma que não pode ser descrita. Ângulos que não existem." -ForegroundColor Gray
        Write-Host "Sua mente registra: Non-Euclidean Horror Detected." -ForegroundColor Magenta
    }
    elseif ($Loucura) {
        Write-Host "`nAtivando protocolo de Loucura Cósmica..." -ForegroundColor Magenta
        $frases = @(
            "Os portais se curvam em dimensões impossíveis.",
            "Você ouve o bater de asas membranosas no vácuo.",
            "A geometria sangra. Os números gritam.",
            "R'lyeh sobe. O mar recua em terror.",
            "Você não está mais certo de quem está sonhando quem."
        )
        foreach ($frase in $frases) {
            Write-Host $frase -ForegroundColor (Get-Random -InputObject @("DarkCyan","DarkMagenta","DarkRed","Gray"))
            Start-Sleep -Milliseconds (Get-Random -Minimum 800 -Maximum 2000)
        }
    }
    else {
        Write-Host "`nO Sono continua. Mas Ele sonha com você agora." -ForegroundColor DarkGray
    }

    Write-Host "`n--- FHTAGN ---" -ForegroundColor Cyan
}

# === EXECUÇÃO DO RITUAL ===
Write-Host "Iniciando contato com o Abismo..." -ForegroundColor DarkCyan
Start-Sleep -Seconds 2

# Escolha seu destino:
# Invoke-Cthulhu -Despertar   # Descomente para o apocalipse total
# Invoke-Cthulhu -Vislumbre # Para um vislumbre seguro(mente insano)
# Invoke-Cthulhu -Loucura   # Para loucura gradual

# Opção padrão: apenas o sussurro
Invoke-Cthulhu
