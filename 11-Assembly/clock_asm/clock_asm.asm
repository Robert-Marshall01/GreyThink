; clock_asm.asm — Linux x86-64 digital clock using pure syscalls
; Build:
;   nasm -f elf64 clock_asm.asm -o clock_asm.o
;   ld clock_asm.o -o clock_asm
; Run:
;   ./clock_asm

BITS 64
GLOBAL _start

SECTION .data
; Output buffer: "\rTime: 00:00:00\n"
outbuf db 13, "Time: 00:00:00", 10
outlen equ $-outbuf

; nanosleep request: 1 second
req_sec  dq 1
req_nsec dq 0

SECTION .bss
ts_sec   resq 1
ts_nsec  resq 1

SECTION .text
_start:
.loop:
    ; clock_gettime(CLOCK_REALTIME=0, &ts)
    mov rax, 228          ; syscall number: clock_gettime
    xor rdi, rdi          ; CLOCK_REALTIME = 0
    lea rsi, [rel ts_sec]
    syscall

    ; rax = seconds since epoch
    mov rax, [ts_sec]

    ; seconds-of-day = rax % 86400
    mov rcx, 86400
    xor rdx, rdx
    div rcx               ; rdx = seconds-of-day
    mov r8, rdx

    ; hours = r8 / 3600
    mov rax, r8
    mov rcx, 3600
    xor rdx, rdx
    div rcx
    mov r9, rax           ; hours
    mov r8, rdx           ; leftover seconds

    ; minutes = leftover / 60
    mov rax, r8
    mov rcx, 60
    xor rdx, rdx
    div rcx
    mov r10, rax          ; minutes
    mov r11, rdx          ; seconds

    ; Write HH at outbuf[7], outbuf[8]
    mov eax, r9d
    lea rdi, [rel outbuf+7]
    call write_two

    ; Write MM at outbuf[10], outbuf[11]
    mov eax, r10d
    lea rdi, [rel outbuf+10]
    call write_two

    ; Write SS at outbuf[13], outbuf[14]
    mov eax, r11d
    lea rdi, [rel outbuf+13]
    call write_two

    ; write(1, outbuf, outlen)
    mov rax, 1
    mov rdi, 1
    lea rsi, [rel outbuf]
    mov rdx, outlen
    syscall

    ; nanosleep(&req, NULL)
    mov rax, 35
    lea rdi, [rel req_sec]
    xor rsi, rsi
    syscall

    jmp .loop

; Helper: write two-digit zero-padded number into buffer
; EAX = value (0..99), RDI = pointer to two chars
write_two:
    mov ecx, 10
    xor edx, edx
    div ecx              ; eax = tens, edx = ones
    add al, '0'
    mov [rdi], al
    mov al, dl
    add al, '0'
    mov [rdi+1], al
    ret