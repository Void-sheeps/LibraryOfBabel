; FIM_DA_LINHA.asm: A conceptual assembly script for the x86 architecture.
; This script is a thought experiment on the nature of finality and system calls.
; It does not perform any useful computation but serves as a philosophical statement.

section .data
    ; Message to be displayed. The '0xa' is the newline character.
    msg db 'FIM DA LINHA. DESLIGANDO.', 0xa
    len equ $ - msg

section .text
    global _start

_start:
    ; A conceptual no-op loop that represents a system "waiting for the end."
    ; In a real-world scenario, this would be an idle loop or a more complex state.
    mov ecx, 0xFFFF
    loop_start:
    nop
    loop loop_start

    ; The "final" system call. This is where the script's philosophical point is made.
    ; We use the `write` system call to print our "FIM DA LINHA" message.
    mov edx, len    ; message length
    mov ecx, msg    ; message to write
    mov ebx, 1      ; file descriptor (1 for stdout)
    mov eax, 4      ; system call number (sys_write)
    int 0x80        ; call kernel

    ; The ultimate system call: `exit`.
    ; This is the script's "final act," terminating its own process.
    mov eax, 1      ; system call number (sys_exit)
    xor ebx, ebx    ; exit code 0
    int 0x80        ; call kernel
