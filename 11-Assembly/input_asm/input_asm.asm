section .data
    prompt      db "Enter a number (0 to quit): ", 0
    prompt_len  equ $ - prompt

    posMsg      db "Number is positive :) ", 10, 0
    posMsg_len  equ $ - posMsg

    negMsg      db "Number is negative :( ", 10, 0
    negMsg_len  equ $ - negMsg

    zeroMsg     db "Number is zero :| ", 10, 0
    zeroMsg_len equ $ - zeroMsg

section .bss
    input   resb 32        ; a bit larger buffer

section .text
    global _start

_start:
main_loop:
    ; Print prompt
    mov     eax, 4
    mov     ebx, 1
    mov     ecx, prompt
    mov     edx, prompt_len
    int     0x80

    ; Read input
    mov     eax, 3
    mov     ebx, 0
    mov     ecx, input
    mov     edx, 32
    int     0x80

    ; Parse ASCII -> integer (supports leading spaces, +/-, multi-digit)
    mov     esi, input         ; pointer to buffer
    xor     eax, eax           ; value accumulator = 0
    xor     ebx, ebx           ; sign flag: 0=positive, 1=negative

    ; Skip leading spaces/tabs
.skip_ws:
    mov     dl, [esi]
    cmp     dl, ' '
    je      .adv_ws
    cmp     dl, 9              ; tab
    jne     .sign_check
.adv_ws:
    inc     esi
    jmp     .skip_ws

.sign_check:
    mov     dl, [esi]
    cmp     dl, '-'
    jne     .plus_check
    mov     ebx, 1             ; mark negative
    inc     esi
    jmp     .first_digit_check

.plus_check:
    cmp     dl, '+'
    jne     .first_digit_check
    inc     esi

.first_digit_check:
    ; Explicit guard: if first non-space char is '0' and next is newline or end, treat as zero
    mov     dl, [esi]
    cmp     dl, '0'
    jne     .parse_loop
    mov     dh, [esi+1]
    cmp     dh, 10             ; newline '\n'
    je      is_zero
    cmp     dh, 0              ; null
    je      is_zero
    ; Otherwise continue normal parsing (e.g., "0" followed by more digits)
    ; fall through

.parse_loop:
    mov     dl, [esi]
    cmp     dl, 10             ; newline
    je      .parsed
    cmp     dl, 0              ; null terminator
    je      .parsed

    ; Validate digit
    cmp     dl, '0'
    jb      .parsed            ; non-digit -> stop
    cmp     dl, '9'
    ja      .parsed            ; non-digit -> stop

    ; digit := dl - '0' in ecx (full width), then accumulate: value = value*10 + digit
    movzx   ecx, dl
    sub     ecx, '0'
    imul    eax, eax, 10
    add     eax, ecx

    inc     esi
    jmp     .parse_loop

.parsed:
    ; Apply sign if needed
    cmp     ebx, 0
    je      .classify
    neg     eax

.classify:
    cmp     eax, 0
    je      is_zero
    jl      is_negative
    ; if not zero and not negative, it's positive
    jmp     is_positive

is_positive:
    mov     eax, 4
    mov     ebx, 1
    mov     ecx, posMsg
    mov     edx, posMsg_len
    int     0x80
    jmp     main_loop

is_negative:
    mov     eax, 4
    mov     ebx, 1
    mov     ecx, negMsg
    mov     edx, negMsg_len
    int     0x80
    jmp     main_loop

is_zero:
    mov     eax, 4
    mov     ebx, 1
    mov     ecx, zeroMsg
    mov     edx, zeroMsg_len
    int     0x80
    jmp     exit_program

exit_program:
    mov     eax, 1
    xor     ebx, ebx
    int     0x80