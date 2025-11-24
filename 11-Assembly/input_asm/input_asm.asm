section .data
    prompt db "Enter a number (0 to quit): ", 0
    prompt_len equ $ - prompt

    posMsg db "Number is positive", 10, 0
    posMsg_len equ $ - posMsg

    negMsg db "Number is negative", 10, 0
    negMsg_len equ $ - negMsg

section .bss
    input resb 16        ; buffer for user input

section .text
    global _start

_start:
main_loop:
    ; Print prompt
    mov eax, 4           ; sys_write
    mov ebx, 1           ; stdout
    mov ecx, prompt
    mov edx, prompt_len
    int 0x80

    ; Read input
    mov eax, 3           ; sys_read
    mov ebx, 0           ; stdin
    mov ecx, input
    mov edx, 16
    int 0x80

    ; Convert ASCII to integer (simple: first char only)
    movzx eax, byte [input]   ; load first character
    sub eax, '0'              ; convert ASCII → number

    ; Check for exit condition
    cmp eax, 0
    je exit_program

    ; Conditional check
    cmp eax, 0
    jg is_positive
    jl is_negative

is_positive:
    mov eax, 4
    mov ebx, 1
    mov ecx, posMsg
    mov edx, posMsg_len
    int 0x80
    jmp main_loop

is_negative:
    mov eax, 4
    mov ebx, 1
    mov ecx, negMsg
    mov edx, negMsg_len
    int 0x80
    jmp main_loop

exit_program:
    mov eax, 1           ; sys_exit
    xor ebx, ebx
    int 0x80