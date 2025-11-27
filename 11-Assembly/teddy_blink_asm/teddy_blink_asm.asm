; teddy_blink_asm.asm - Teddy bear animation with blinking eyes, talking mouth,
; clear screen, and 3-second sleep delay.
; Assemble: nasm -f elf teddy_blink_asm.asm
; Link:     ld -m elf_i386 -o teddy_blink_asm teddy_blink_asm.o
; Run:      ./teddy_blink_asm
SECTION .data
clearSeq db 0x1B,"[2J",0x1B,"[H",0   ; ANSI clear + home

bearClosedEyesOpen:
    db "      .--.      ",10
    db "     | o_o |    ",10
    db "    (   ^   )   ",10
    db "     |  --- |   ",10
    db "      |     |   ",10
    db "     |       |  ",10
    db "    (         ) ",10
    db "     |_______|  ",10
    db "      |     |   ",10
    db "     (       )  ",10
    db "      -------   ",10,0

bearClosedEyesClosed:
    db "      .--.      ",10
    db "     | -_- |    ",10
    db "    (   ^   )   ",10
    db "     |  --- |   ",10
    db "      |     |   ",10
    db "     |       |  ",10
    db "    (         ) ",10
    db "     |_______|  ",10
    db "      |     |   ",10
    db "     (       )  ",10
    db "      -------   ",10,0

bearOpenEyesOpen:
    db "      .--.      ",10
    db "     | o_o |    ",10
    db "    (   ^   )   ",10
    db "     |  ___ |   ",10
    db "      |     |   ",10
    db "     |       |  ",10
    db "    (         ) ",10
    db "     |_______|  ",10
    db "      |     |   ",10
    db "     (       )  ",10
    db "      -------   ",10,0

bearOpenEyesClosed:
    db "      .--.      ",10
    db "     | -_- |    ",10
    db "    (   ^   )   ",10
    db "     |  ___ |   ",10
    db "      |     |   ",10
    db "     |       |  ",10
    db "    (         ) ",10
    db "     |_______|  ",10
    db "      |     |   ",10
    db "     (       )  ",10
    db "      -------   ",10,0

msg    db "I'm a sad plushy!",10,0
prompt db "Press 'q' to quit...",10,0

; nanosleep struct: {seconds, nanoseconds}
timespec:
    dd 3          ; tv_sec = 3 seconds
    dd 0          ; tv_nsec = 0

SECTION .bss
char resb 1

SECTION .text
global _start

_start:
    ; Initial frame
    call clear_screen
    mov ecx, bearClosedEyesOpen
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80

    ; Message below teddy
    mov ecx, msg
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80

    call sleep3

animate:
    ; Frame 1: mouth open, eyes open
    call clear_screen
    mov ecx, bearOpenEyesOpen
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    ; Message below teddy
    mov ecx, msg
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    call sleep3

    ; Frame 2: mouth closed, eyes open
    call clear_screen
    mov ecx, bearClosedEyesOpen
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    mov ecx, msg
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    call sleep3

    ; Frame 3: mouth open, eyes closed
    call clear_screen
    mov ecx, bearOpenEyesClosed
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    mov ecx, msg
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    call sleep3

    ; Frame 4: mouth closed, eyes closed
    call clear_screen
    mov ecx, bearClosedEyesClosed
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    mov ecx, msg
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    call sleep3

    ; Prompt user
    mov ecx, prompt
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80

    ; Read input
    mov eax, 3
    mov ebx, 0
    mov ecx, char
    mov edx, 1
    int 0x80

    cmp byte [char], 'q'
    jne animate

    ; Exit
    mov eax, 1
    xor ebx, ebx
    int 0x80

; Helper: strlen
; Input: ECX = pointer to null-terminated string
; Output: EDX = length
strlen:
    push ecx
    mov edx, 0
.len_loop:
    cmp byte [ecx+edx], 0
    je .len_done
    inc edx
    jmp .len_loop
.len_done:
    pop ecx
    ret

; Clear screen using ANSI escape
clear_screen:
    mov ecx, clearSeq
    call strlen
    mov eax, 4
    mov ebx, 1
    int 0x80
    ret

; Sleep for 3 seconds using nanosleep
sleep3:
    mov eax, 162        ; sys_nanosleep
    mov ebx, timespec   ; req
    mov ecx, 0          ; rem = NULL
    int 0x80
    ret