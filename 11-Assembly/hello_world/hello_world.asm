section .data
    msg db "Hello, world!", 0xA
    len equ $ - msg

section .text
    global _start

_start:
    mov eax, 1        ; syscall: write
    mov edi, 1        ; file descriptor: stdout
    mov rsi, msg      ; address of string
    mov edx, len      ; length of string
    syscall

    mov eax, 60       ; syscall: exit
    xor edi, edi
    syscall