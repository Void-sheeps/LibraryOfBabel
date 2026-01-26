; =============================================================================
; FCMP_SAFE
; Compara FAC1 com FAC2
; Retorna:
;   A = $FF  → FAC1 < FAC2
;   A = $00  → FAC1 = FAC2
;   A = $01  → FAC1 > FAC2
;
; Preserva FAC1 (5 bytes)
; Destrói: X, TEMP
; =============================================================================

FAC1    = $61      ; FAC1: expoente em FAC1+0
SGNFLG  = $66      ; Flag de sinal do FAC1 ($00=+, $FF=-)
FSUB    = $BA8C    ; ROM: FAC1 ← FAC1 - FAC2
TEMP    = $FB      ; ZP temporário (5 bytes)

FCMP_SAFE:
        ; ── Backup FAC1 ───────────────────────────────────────
        ldx #0
SAVE_LOOP:
        lda FAC1,x
        sta TEMP,x
        inx
        cpx #5
        bne SAVE_LOOP

        ; ── Subtração real: FAC1 = FAC1 - FAC2 ────────────────
        jsr FSUB

        ; ── Análise do resultado ──────────────────────────────
        lda FAC1          ; expoente
        beq CMP_EQ        ; zero ⇒ iguais

        lda SGNFLG        ; sinal
        bmi CMP_LT        ; negativo ⇒ FAC1 < FAC2

        lda #$01          ; positivo ⇒ FAC1 > FAC2
        bne CMP_DONE

CMP_LT:
        lda #$FF          ; FAC1 < FAC2
        bne CMP_DONE

CMP_EQ:
        lda #$00          ; iguais

CMP_DONE:
        pha               ; salva resultado

        ; ── Restaura FAC1 ─────────────────────────────────────
        ldx #0
REST_LOOP:
        lda TEMP,x
        sta FAC1,x
        inx
        cpx #5
        bne REST_LOOP

        pla               ; A = -1, 0 ou +1
        rts

; --- Exemplo de uso ---
;   jsr FCMP_SAFE
;   beq ValuesEqual     ; Z=1 → iguais
;   cmp #$01
;   beq Greater
;   cmp #$FF
;   beq Less
