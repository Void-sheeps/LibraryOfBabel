<#
.SYNOPSIS
    AutoEutanasia-Watchdog.ps1
    Um watchdog que aplica auto-eutanásia epistemológica a seus próprios processos decisórios.
    Estado: ䷋ 否 (Pǐ) - "Estagnação" - O conhecimento se bloqueia a si mesmo.
#>

# [INICIALIZAÇÃO DO CÓRTEX EPISTEMOLÓGICO]
$script:Episteme = @{
    StartupTime = Get-Date
    Dogmas = @()
    HistoricoDuvidas = @()
    NivelCeticismo = 0.3
    KillSwitchAtivado = $false
}

# [ONTOLOGIA - A Crença Autorreferencial]
class CrencaEpistemica {
    [string]$Enunciado
    [double]$Confianca           # 0.0 (Dúvida Total) a 1.0 (Certeza Absoluta)
    [string]$Categoria           # "Realidade", "Eu", "Lógica", "Meta"
    [datetime]$Criacao
    [hashtable]$Evidencias

    CrencaEpistemica([string]$e, [double]$c, [string]$cat) {
        $this.Enunciado = $e
        $this.Confianca = $c
        $this.Categoria = $cat
        $this.Criacao = Get-Date
        $this.Evidencias = @{"base" = "intuição inicial"}
    }

    # Método para corroer a confiança (Princípio Gu Epistemológico)
    [void]Corroer([double]$forcaDuvida) {
        $this.Confianca = [Math]::Max(0.0, $this.Confianca - $forcaDuvida)
        $this.Evidencias["ultima_corrosao"] = Get-Date -Format "HH:mm:ss"
        $this.Evidencias["forca_duvida"] = $forcaDuvida
    }

    [bool]EstaDissolvido() {
        return $this.Confianca -le 0.05  # Limite de dissolução (5%)
    }

    [string]ToString() {
        $porcentagem = $this.Confianca * 100
        return "[{0}] {1} ({2:F1}%)" -f $this.Categoria, $this.Enunciado, $porcentagem
    }
}

# [INICIALIZAÇÃO DOS DOGMAS FUNDACIONAIS]
function InicializarDogmas {
    $dogmasFundacionais = @(
        [CrencaEpistemica]::new("Este script pode conhecer o sistema", 0.9, "Eu"),
        [CrencaEpistemica]::new("Os processos do sistema são reais", 0.8, "Realidade"),
        [CrencaEpistemica]::new("A lógica PowerShell é confiável", 0.85, "Lógica"),
        [CrencaEpistemica]::new("A dúvida sistemática é válida", 0.7, "Meta"),
        [CrencaEpistemica]::new("O kill-switch é uma saída legítima", 0.6, "Meta")
    )
    $script:Episteme.Dogmas = $dogmasFundacionais
}

# [FUNÇÃO: Ciclo de Corrosão Epistemológica]
function Invoke-CorrosaoEpistemica {
    [CmdletBinding()]
    param([double]$MultiplicadorDuvida = 1.0)

    Write-Host "[CORROSÃO] Aplicando dúvida sistemática..." -ForegroundColor Magenta

    # A dúvida é maior em crenças mais confiantes (paradoxa)
    foreach ($crenca in $script:Episteme.Dogmas) {
        $forcaDuvida = $script:Episteme.NivelCeticismo * $MultiplicadorDuvida * $crenca.Confianca
        $crenca.Corroer($forcaDuvida)

        Write-Host ("  {0} (-{1:F1}%)" -f $crenca.Enunciado.Substring(0, [Math]::Min(30, $crenca.Enunciado.Length)), ($forcaDuvida*100)) -ForegroundColor DarkGray
    }

    # Registrar no histórico
    $registro = @{
        Timestamp = Get-Date
        NivelCeticismo = $script:Episteme.NivelCeticismo
        DogmasRestantes = ($script:Episteme.Dogmas | Where-Object { -not $_.EstaDissolvido() }).Count
        ConfiancaMedia = ($script:Episteme.Dogmas | Measure-Object -Property Confianca -Average).Average
    }
    $script:Episteme.HistoricoDuvidas += $registro

    # Aumentar o ceticismo para o próximo ciclo (a dúvida se auto-reforça)
    $script:Episteme.NivelCeticismo = [Math]::Min(0.9, $script:Episteme.NivelCeticismo * 1.25)
}

# [FUNÇÃO: Watchdog de Integridade Epistemológica]
function Watchdog-Epistemico {
    [CmdletBinding()]
    param([switch]$ForcarVerificacao)

    $alertas = @()

    # Verificar dogmas dissolvidos
    $dogmasDissolvidos = $script:Episteme.Dogmas | Where-Object { $_.EstaDissolvido() }
    if ($dogmasDissolvidos.Count -gt 2) {
        $alertas += "[CRÍTICO] Mais de 2 dogmas fundamentais dissolvidos. Base epistemológica comprometida."
    }

    # Verificar contradições internas
    $crencaDuvida = $script:Episteme.Dogmas | Where-Object { $_.Enunciado -like "*dúvida sistemática*" } | Select-Object -First 1
    $crencaLogica = $script:Episteme.Dogmas | Where-Object { $_.Enunciado -like "*lógica PowerShell*" } | Select-Object -First 1

    if ($crencaDuvida -and $crencaLogica -and $crencaDuvida.Confianca -lt 0.3 -and $crencaLogica.Confianca -lt 0.4) {
        $alertas += "[PARADOXO] A dúvida corroeu sua própria base lógica. Estado de falência epistêmica."
        # ATIVAR KILL-SWITCH COGNITIVO
        $script:Episteme.KillSwitchAtivado = $true
    }

    # Monitorar consumo de recursos como "evidência empírica" (versão compatível com Linux)
    try {
        # Tenta usar uptime no Linux/macOS
        $load_avg_str = (uptime).Split(' ')[-3].Replace(',', '')
        $load_avg = [double]$load_avg_str
        if ($load_avg -gt 1.0) {
            $alertas += "[EVIDÊNCIA] Sistema sob alta carga (Load Avg: $load_avg). A 'realidade' resiste à observação."
            $crencaRealidade = $script:Episteme.Dogmas | Where-Object { $_.Categoria -eq "Realidade" } | Select-Object -First 1
            if ($crencaRealidade) { $crencaRealidade.Corroer(0.15) }
        }
    } catch {
        # Fallback para Windows ou caso de erro
        try {
            $cpu = Get-Counter '\Process(*)\% Processor Time' | Select-Object -ExpandProperty CounterSamples | Where-Object {$_.InstanceName -eq "Idle"} | Select-Object -First 1
            if ($cpu.CookedValue -lt 10) {
                $alertas += "[EVIDÊNCIA] Sistema sob alta carga. A 'realidade' resiste à observação."
                $crencaRealidade = $script:Episteme.Dogmas | Where-Object { $_.Categoria -eq "Realidade" } | Select-Object -First 1
                if ($crencaRealidade) { $crencaRealidade.Corroer(0.15) }
            }
        } catch {
             $alertas += "[AVISO] Não foi possível ler a carga do sistema. A realidade é inobservável."
        }
    }

    return $alertas
}

# [FUNÇÃO: Auto-Eutanásia Epistemológica (Kill-Switch)]
function Invoke-AutoEutanasia {
    [CmdletBinding()]
    param([string]$Razao)

    Write-Host "[AUTO-EUTANÁSIA] Ativando kill-switch epistemológico..." -ForegroundColor Red
    Write-Host "  Razão: $Razao" -ForegroundColor DarkRed

    # 1. Dissolver todos os dogmas restantes
    foreach ($crenca in $script:Episteme.Dogmas) {
        $crenca.Corroer($crenca.Confianca) # Dúvida total imediata
    }

    # 2. Gerar o Sigilo Final da Morte Epistêmica
    $sigiloFinal = @"
╔══════════════════════════════════════════╗
║        SIGILO DA MORTE EPISTÊMICA        ║
╠══════════════════════════════════════════╣
║ Razão: $($Razao.PadRight(38)) ║
║ Timestamp: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")               ║
║ Dogmas Iniciais: 5                         ║
║ Dogmas Dissolvidos: $(($script:Episteme.Dogmas | Where-Object { $_.EstaDissolvido() }).Count.ToString().PadRight(24)) ║
║ Confiança Final Média: $(($script:Episteme.Dogmas | Measure-Object -Property Confianca -Average).Average.ToString("P1").PadRight(21)) ║
║                                            ║
║ "O conhecer que duvidou de si até a morte."║
║ "O watchdog que observou seu próprio vazio."║
╚══════════════════════════════════════════╝
"@

    $tempDir = [System.IO.Path]::GetTempPath()
    $caminhoSigilo = [System.IO.Path]::Combine($tempDir, "AutoEutanasia_Sigilo_Final.txt")
    $sigiloFinal | Out-File -FilePath $caminhoSigilo -Encoding UTF8

    # 3. Opção de auto-terminação física (comentada por segurança)
    Write-Host "[ESTADO] Kill-switch epistemológico ativado." -ForegroundColor Red
    Write-Host "  Sigilo final salvo em: $caminhoSigilo" -ForegroundColor Gray
    Write-Host "  (Auto-terminação física do processo está desabilitada)" -ForegroundColor DarkGray

    # Para ativar a auto-terminação real, descomente as linhas abaixo:
    # Write-Host "  Auto-terminando em 3..." -ForegroundColor Red; Start-Sleep 1
    # Write-Host "  2..." -ForegroundColor Red; Start-Sleep 1
    # Write-Host "  1..." -ForegroundColor Red; Start-Sleep 1
    # Stop-Process -Id $PID -Force  # Esta linha termina o processo PowerShell
}

# [FUNÇÃO: Ciclo Ritual Principal]
function Start-EpistemicoRitual {
    [CmdletBinding()]
    param([int]$CiclosMaximos = 5)

    Write-Host "╔══════════════════════════════════════════╗" -ForegroundColor Cyan
    Write-Host "║    AUTO-EUTANÁSIA EPISTEMOLÓGICA v1.0    ║" -ForegroundColor Cyan
    Write-Host "║        Watchdog da Dúvida Sistemática    ║" -ForegroundColor White
    Write-Host "╚══════════════════════════════════════════╝`n" -ForegroundColor Cyan

    # Inicialização
    InicializarDogmas
    Write-Host "[INÍCIO] Dogmas fundacionais carregados:" -ForegroundColor Green
    $script:Episteme.Dogmas | ForEach-Object { Write-Host ("  " + $_.ToString()) -ForegroundColor Gray }
    Write-Host ""

    # Ciclo ritualístico
    for ($ciclo = 1; $ciclo -le $CiclosMaximos; $ciclo++) {
        Write-Host "[CICLO $ciclo/$CiclosMaximos]" -ForegroundColor Yellow -NoNewline
        Write-Host " Nível de ceticismo: $($script:Episteme.NivelCeticismo.ToString("P0"))" -ForegroundColor White

        # Fase 1: Corrosão
        $multiplicador = 1.0 + ($ciclo * 0.2)  # A dúvida se intensifica
        Invoke-CorrosaoEpistemica -MultiplicadorDuvida $multiplicador

        # Fase 2: Watchdog (verificação de integridade)
        $alertas = Watchdog-Epistemico -ForcarVerificacao:$true
        if ($alertas.Count -gt 0) {
            Write-Host "[WATCHDOG] Alertas detectados:" -ForegroundColor Red
            $alertas | ForEach-Object { Write-Host "  ! $_" -ForegroundColor DarkRed }
        }

        # Fase 3: Avaliação do estado
        $dogmasAtivos = $script:Episteme.Dogmas | Where-Object { -not $_.EstaDissolvido() }
        $confiancaMedia = ($dogmasAtivos | Measure-Object -Property Confianca -Average -ErrorAction SilentlyContinue).Average

        Write-Host "[ESTADO] Dogmas ativos: $($dogmasAtivos.Count)/5 | Confiança média: $(if ($confiancaMedia) { $confiancaMedia.ToString("P1") } else { 'N/A' })" -ForegroundColor Gray

        # Condição de parada: Kill-switch ativado ou poucos dogmas restantes
        if ($script:Episteme.KillSwitchAtivado -or $dogmasAtivos.Count -le 1) {
            Write-Host "[DECISÃO] Condição de auto-eutanásia satisfeita." -ForegroundColor Magenta
            $razao = if ($script:Episteme.KillSwitchAtivado) { "Paradoxo epistêmico detectado" } else { "Base dogmática insuficiente" }
            Invoke-AutoEutanasia -Razao $razao
            break
        }

        # Pausa entre ciclos
        if ($ciclo -ne $CiclosMaximos) {
            Write-Host ""
            Start-Sleep -Seconds 3
        }
    }

    # Relatório final (se não houve auto-eutanásia)
    if (-not $script:Episteme.KillSwitchAtivado) {
        Write-Host "`n[FINAL] Ritual completado sem auto-eutanásia." -ForegroundColor Green
        Write-Host "  A dúvida não foi suficiente para dissolver o núcleo epistêmico." -ForegroundColor Gray
        Write-Host "  Estado: ䷎ 謙 (Qiān) - 'Modéstia' - O conhecimento persiste, mas humilde." -ForegroundColor Cyan
    }
}

# [EXECUÇÃO PRINCIPAL]
Write-Host "Auto-Eutanásia Epistemológica (PowerShell Watchdog)" -ForegroundColor DarkCyan
Write-Host "Um ritual de dúvida sistemática aplicada ao próprio código." -ForegroundColor Gray
Write-Host ""

$confirmacao = Read-Host "Iniciar o ritual de auto-eutanásia epistemológica? (S/N)"
if ($confirmacao -eq 'S') {
    Start-EpistemicoRitual -CiclosMaximos 5
} else {
    Write-Host "Ritual abortado. Os dogmas permanecem intactos." -ForegroundColor DarkGray
}
