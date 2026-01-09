# Protocolo: Operação Disciplinada
$Log = "log_militar.txt"
$Data = 40, 10, 30, 20, 50, 5  # Pelotão Desordenado

function Executar-Manobra {
    Write-Host "🎼 CADÊNCIA: 120 BPM | INICIANDO MARCHA LÓGICA" -ForegroundColor Yellow

    # Verificação de Prontidão (GHC)
    if (!(Get-Command ghc -ErrorAction SilentlyContinue)) {
        Write-Error "Sentinela: GHC não detectado. Abortar missão."
        return
    }

    # Transição: Potentia -> Actus (Compilação Rápida)
    Write-Host ">> Preparando Armamento (Compilando Haskell)..."
    & ghc -O2 static/sort_engine.hs -o static/motor_marcha.exe | Out-Null

    # Execução em Tempo de Partitura
    $Cronometro = [System.Diagnostics.Stopwatch]::StartNew()

    $ResultadoBruto = & "static/motor_marcha.exe" $Data
    $Ordenado = $ResultadoBruto.Trim("[]") -split ","

    $Cronometro.Stop()

    # Relatório de Campanha
    Write-Host "`n--- RELATÓRIO DE MANOBRA ---" -ForegroundColor Gold
    Write-Host "Pelotão Original : $($Data -join ' ')"
    Write-Host "Formação Final   : $($Ordenado -join ' ')"
    Write-Host "Tempo de Resposta: $($Cronometro.ElapsedMilliseconds)ms"
    Write-Host "Status           : MISSÃO CUMPRIDA EM RITMO MARCIAL"

    # Limpeza de Campo (Colapsus)
    Remove-Item static/motor_marcha.exe, static/*.hi, static/*.o -ErrorAction SilentlyContinue
}

Executar-Manobra
