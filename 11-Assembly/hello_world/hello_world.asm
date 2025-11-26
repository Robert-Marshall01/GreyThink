section .data
    msg1 db "1: Hello, world!", 0xA
    len1 equ $ - msg1

    msg2 db "2: Hello, world!", 0xA
    len2 equ $ - msg2

    msg3 db "3: Hello, world!", 0xA
    len3 equ $ - msg3

section .text
    global _start

_start:
    ; RNG from timestamp counter
    rdtsc                  ; edx:eax = timestamp
    xor edx, edx
    mov ecx, 3
    div ecx                ; remainder in edx (0–2)

    cmp edx, 0
    je one
    cmp edx, 1
    je two
    jmp three

one:
    mov eax, 4             ; sys_write
    mov ebx, 1             ; fd = stdout
    mov ecx, msg1          ; buffer
    mov edx, len1          ; length
    int 0x80
    jmp done

two:
    mov eax, 4
    mov ebx, 1
    mov ecx, msg2
    mov edx, len2
    int 0x80
    jmp done

three:
    mov eax, 4
    mov ebx, 1
    mov ecx, msg3
    mov edx, len3
    int 0x80

done:
    mov eax, 1             ; sys_exit
    xor ebx, ebx
    int 0x80