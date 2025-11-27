; keypress_asm.asm
; NASM program: prints the key the user presses
; Assemble: nasm -f elf32 keypress_asm.asm -o keypress_asm.o
; Link:    ld -m elf_i386 keypress_asm.o -o keypress_asm
; Run:     ./keypress_asm

section .data
    prompt db "Press any key: ", 0
    pressed db "You pressed: ", 0
    newline db 10, 0

section .bss
    key resb 1   ; reserve 1 byte for the key

section .text
    global _start

_start:
    ; Print prompt
    mov eax, 4          ; sys_write
    mov ebx, 1          ; stdout
    mov ecx, prompt
    mov edx, 15         ; length of "Press any key: "
    int 0x80

    ; Read 1 byte from stdin
    mov eax, 3          ; sys_read
    mov ebx, 0          ; stdin
    mov ecx, key
    mov edx, 1
    int 0x80

    ; Print "You pressed: "
    mov eax, 4
    mov ebx, 1
    mov ecx, pressed
    mov edx, 13         ; length of "You pressed: "
    int 0x80

    ; Print the key itself
    mov eax, 4
    mov ebx, 1
    mov ecx, key
    mov edx, 1
    int 0x80

    ; Print newline
    mov eax, 4
    mov ebx, 1
    mov ecx, newline
    mov edx, 1
    int 0x80

    ; Exit
    mov eax, 1          ; sys_exit
    xor ebx, ebx
    int 0x80