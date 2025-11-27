; Assemble: nasm -f elf64 house_gui.asm -o house_gui.o
; Link:     gcc -no-pie house_gui.o -o house_gui -lSDL2
; Run:      ./house_gui
;
; Dependencies:
;   sudo apt install nasm gcc libsdl2-dev

        default rel
        global  main

        extern  SDL_Init
        extern  SDL_Quit
        extern  SDL_CreateWindow
        extern  SDL_CreateRenderer
        extern  SDL_DestroyRenderer
        extern  SDL_DestroyWindow
        extern  SDL_CreateTexture
        extern  SDL_DestroyTexture
        extern  SDL_UpdateTexture
        extern  SDL_RenderClear
        extern  SDL_RenderCopy
        extern  SDL_RenderPresent
        extern  SDL_PollEvent
        extern  SDL_Delay

%define SDL_INIT_VIDEO         0x00000020
%define SDL_WINDOWPOS_CENTERED 0x2FFF0000
%define SDL_WINDOW_SHOWN       0x00000004
%define SDL_RENDERER_ACCELERATED 0x00000002
%define SDL_PIXELFORMAT_ARGB8888 373694468
%define SDL_TEXTUREACCESS_STREAMING 1
%define SDL_QUIT               0x100

SECTION .data
title:      db "House GUI (NASM + SDL2)",0
win_w:      dq 320
win_h:      dq 240

COLOR_BG:   dd 0xFF202428
COLOR_BODY: dd 0xFFB06B3B
COLOR_ROOF: dd 0xFF882D2D
COLOR_DOOR: dd 0xFF553322
COLOR_WIN:  dd 0xFF80BCD6

event_buf:  times 56 db 0

SECTION .bss
pixels:     resd 100*100

SECTION .text
main:
    push rbp
    mov rbp,rsp

    ; SDL_Init
    mov edi,SDL_INIT_VIDEO
    call SDL_Init

    ; SDL_CreateWindow
    mov rdi,title
    mov rsi,SDL_WINDOWPOS_CENTERED
    mov rdx,SDL_WINDOWPOS_CENTERED
    mov rcx,[win_w]
    mov r8,[win_h]
    mov r9d,SDL_WINDOW_SHOWN
    call SDL_CreateWindow
    mov r12,rax

    ; SDL_CreateRenderer
    mov rdi,r12
    mov esi,-1
    mov edx,SDL_RENDERER_ACCELERATED
    call SDL_CreateRenderer
    mov r13,rax

    ; Draw house pixels
    call draw_house

    ; SDL_CreateTexture
    mov rdi,r13
    mov esi,SDL_PIXELFORMAT_ARGB8888
    mov edx,SDL_TEXTUREACCESS_STREAMING
    mov ecx,100
    mov r8d,100
    call SDL_CreateTexture
    mov r14,rax

    ; SDL_UpdateTexture
    mov rdi,r14
    xor rsi,rsi
    lea rdx,[pixels]
    mov ecx,400
    call SDL_UpdateTexture

.loop:
    ; Poll events
    lea rdi,[event_buf]
    call SDL_PollEvent
    test eax,eax
    jz .render
    mov eax,[event_buf]
    cmp eax,SDL_QUIT
    je .end

.render:
    mov rdi,r13
    call SDL_RenderClear
    mov rdi,r13
    mov rsi,r14
    xor rdx,rdx
    xor rcx,rcx
    call SDL_RenderCopy
    mov rdi,r13
    call SDL_RenderPresent
    mov edi,16
    call SDL_Delay
    jmp .loop

.end:
    mov rdi,r14
    call SDL_DestroyTexture
    mov rdi,r13
    call SDL_DestroyRenderer
    mov rdi,r12
    call SDL_DestroyWindow
    call SDL_Quit
    mov eax,0
    leave
    ret

; ------------------------------------------------------------
; draw_house: fills pixels[] with house art
; ------------------------------------------------------------
draw_house:
    push rbp
    mov rbp,rsp
    lea rdi,[pixels]

    mov eax,[COLOR_BG]
    mov ecx,100*100
.fill_bg:
    mov [rdi],eax
    add rdi,4
    dec ecx
    jnz .fill_bg

    ; House body rectangle: x=25..74, y=40..79
    mov r10d,40
.body_y:
    cmp r10d,80
    jge .roof
    mov r11d,25
.body_x:
    cmp r11d,75
    jge .next_body
    mov eax,r10d
    imul eax,100
    add eax,r11d
    lea rdi,[pixels+rax*4]
    mov ebx,[COLOR_BODY]
    mov [rdi],ebx
    inc r11d
    jmp .body_x
.next_body:
    inc r10d
    jmp .body_y

.roof:
    ; Roof triangle rotated π radians (apex at y=40, base at y=20)
    mov r10d,20
.roof_y:
    cmp r10d,40
    jge .door
    ; half_width grows as y increases (inverted triangle)
    mov eax,r10d
    sub eax,20
    add eax,10
    mov r15d,eax
    mov r11d,50
    sub r11d,r15d
    mov r14d,50
    add r14d,r15d
.roof_x:
    cmp r11d,r14d
    jge .next_roof
    mov eax,r10d
    imul eax,100
    add eax,r11d
    lea rdi,[pixels+rax*4]
    mov ecx,[COLOR_ROOF]
    mov [rdi],ecx
    inc r11d
    jmp .roof_x
.next_roof:
    inc r10d
    jmp .roof_y

.door:
    ; Door: x=47..52, y=60..79
    mov r10d,60
.door_y:
    cmp r10d,80
    jge .window
    mov r11d,47
.door_x:
    cmp r11d,53
    jge .next_door
    mov eax,r10d
    imul eax,100
    add eax,r11d
    lea rdi,[pixels+rax*4]
    mov edx,[COLOR_DOOR]
    mov [rdi],edx
    inc r11d
    jmp .door_x
.next_door:
    inc r10d
    jmp .door_y

.window:
    ; Window: x=30..39, y=50..59
    mov r10d,50
.win_y:
    cmp r10d,60
    jge .done
    mov r11d,30
.win_x:
    cmp r11d,40
    jge .next_win
    mov eax,r10d
    imul eax,100
    add eax,r11d
    lea rdi,[pixels+rax*4]
    mov r8d,[COLOR_WIN]
    mov [rdi],r8d
    inc r11d
    jmp .win_x
.next_win:
    inc r10d
    jmp .win_y

.done:
    pop rbp
    ret

section .note.GNU-stack noalloc noexec nowrite align=4