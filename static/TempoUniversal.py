# AXIOMA: 1 beat = 86.4s (Naturale Silentium dividido por 1000)
SECONDS_PER_BEAT = 86.4

def brasilia_to_swatch(hora_str):
    """
    Transforma o tempo local (Brasília) no simulacro decimal @beats.
    """
    try:
        h, m, s = map(int, hora_str.split(':'))
        # Cálculo de segundos totais em UTC-3
        total_seconds_br = h * 3600 + m * 60 + s

        # Normalização para Biel (UTC+1): +4 horas = 14400 segundos
        total_seconds_biel = (total_seconds_br + 14400) % 86400

        # Conversão para escala decimal [0..999]
        beats = total_seconds_biel / SECONDS_PER_BEAT
        return f"@{beats:06.2f}"
    except ValueError:
        return "LOGICAL_NULLITY"

def swatch_to_brasilia(beats_val):
    """
    Retorna do Actus decimal para a Potentia cronológica de Brasília.
    """
    beats = float(beats_val) % 1000.0
    total_seconds_biel = beats * SECONDS_PER_BEAT

    # Recuo de 4h para Brasília
    total_seconds_br = total_seconds_biel - 14400

    # Tratamento de Colapsus temporal (virada de dia)
    offset_msg = ""
    if total_seconds_br < 0:
        total_seconds_br += 86400
        offset_msg = " (Ontem)"
    elif total_seconds_br >= 86400:
        total_seconds_br -= 86400
        offset_msg = " (Amanhã)"

    total_seconds_br = int(round(total_seconds_br))
    h, resto = divmod(total_seconds_br, 3600)
    m, s = divmod(resto, 60)

    return f"{h:02d}:{m:02d}:{s:02d}{offset_msg}"

# Execução sob Input X
if __name__ == "__main__":
    # O Ponto de Gödel da sincronia:
    print(f"Meio-dia em Brasília: {brasilia_to_swatch('12:00:00')}")
    print(f"Meia-noite em Biel (@000): {swatch_to_brasilia(0)}")
