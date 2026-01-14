; ====================================================================
; Axioma 6502: Interceptação Segura de IRQ (C64 Architecture)
; ====================================================================

.org $0801
; Basic header (SYS 2064)
.byte $0B, $08, $0A, $00, $9E, $32, $30, $36, $34, $00, $00, $00

init:
    sei                     ; Silêncio Absoluto (Disable Interrupts)

    ; 1. Salvar o vetor original de IRQ (que está na RAM em $0314)
    lda $0314
    sta old_irq_lo
    lda $0315
    sta old_irq_hi

    ; 2. Instalar nosso vetor "The Hook"
    lda #<my_irq_handler
    sta $0314
    lda #>my_irq_handler
    sta $0315

    cli                     ; O sistema volta a respirar
    rts                     ; Retorna ao BASIC (mas o IRQ fica rodando)

; ====================================================================
; O Handler (Inter-legere)
; ====================================================================
my_irq_handler:
    ; Nota: O Kernel do C64 JÁ salvou A, X, Y na stack antes de chamar $0314.
    ; Se fosse um hardware "nu", teríamos que salvar.
    ; Assumindo hardware nu para fins didáticos de "chaining":

    ; -- Executar Payload (Efeito Visual/Lógico) --
    inc $D020              ; C64: Pisca a borda da tela (Border Color)
                           ; Prova visual de vida (Actus)

    ; -- Protocolo de Encadeamento (The Trampoline) --

    ; OPÇÃO A: Chamar o Kernel Original e deixar ele terminar.
    ; No C64, o endereço original (normalmente $EA31) espera que os registradores
    ; estejam limpos ou prontos.

    ; Pula para o endereço salvo (JMP Indirect).
    ; O handler original fará o RTI.
    jmp (old_irq_lo)

; ====================================================================
; Armazenamento (Safe Zone - Fora da Zero Page crítica)
; ====================================================================
old_irq_lo: .byte $00
old_irq_hi: .byte $00

; ====================================================================
; NOTAS DO SISTEMA:
; 1. Não usamos PHA/PLA aqui porque estamos pulando para uma rotina
;    que espera tratar o IRQ "como se nada tivesse acontecido".
; 2. Se você quisesse não chamar o original, VOCÊ deveria fazer:
;    pla / tay / pla / tax / pla / rti (em hardware nu).
;    No C64, o vetor $0314 é uma sub-rotina, então o retorno é via JMP.
; ====================================================================
