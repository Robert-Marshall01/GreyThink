section .data
    prompt1    db "Enter first number: "
    len1       equ $-prompt1

    prompt2    db "Enter second number: "
    len2       equ $-prompt2

    promptOp   db "Enter operation (+ - * /): "
    lenOp      equ $-promptOp

    resultMsg  db "Result: "
    lenRes     equ $-resultMsg

    zeroDivMsg db "ZeroDivisionError"
    lenZeroDiv equ $-zeroDivMsg

    invalidMsg db "Invalid Input",10
    lenInvalid equ $-invalidMsg

    decimalPoint db "."
    lenDec       equ 1

    newline    db 10
    lenNL      equ 1

section .bss
    num1     resb 16
    num2     resb 16
    op       resb 2
    resval   resd 1        ; integer result / quotient
    remval   resd 1        ; remainder after integer division
    denval   resd 1        ; denominator
    outbuf   resb 32       ; for itoa
    fracbuf  resb 4        ; up to 4 fractional digits
    fraclen  resb 1        ; number of fractional digits computed

section .text
global _start

; --- Utility: read string ---
read_str:
    mov eax,3            ; sys_read
    mov ebx,0
    int 0x80
    ret

; --- Utility: write string ---
write_str:
    mov eax,4            ; sys_write
    mov ebx,1
    int 0x80
    ret

; --- atoi with validation, negative support, and at-least-one-digit check ---
atoi_validate:
    xor eax,eax              ; result = 0
    xor esi,esi              ; sign flag = 0 (positive)
    xor edx,edx              ; digit_count = 0

    mov bl,[ecx]             ; check first character
    cmp bl,'-'
    jne .parse_digits
    mov esi,1                ; mark as negative
    inc ecx                  ; skip '-'

.parse_digits:
.next_digit:
    mov bl,[ecx]
    cmp bl,0                 ; end of string
    je .done
    cmp bl,10                ; newline terminator
    je .done
    cmp bl,'0'
    jb .invalid
    cmp bl,'9'
    ja .invalid

    sub bl,'0'
    movzx ebx,bl
    imul eax,eax,10
    add eax,ebx
    inc edx                  ; digit_count++
    inc ecx
    jmp .next_digit

.done:
    cmp edx,0                ; require at least one digit
    je .invalid

    test esi,esi             ; was negative?
    jz .return
    neg eax                  ; apply sign
.return:
    ret

.invalid:
    mov ecx,invalidMsg
    mov edx,lenInvalid
    call write_str
    mov eax,-1
    ret

; --- itoa (buffered, handles negatives and zero, balanced stack, does not touch ESI/EDI) ---
itoa:
    mov ebx,10
    mov ebp,ecx             ; ebp = buffer start
    add ecx,15              ; ecx = tail (room for digits + sign)
    mov byte [ecx],0        ; null terminator at tail

    ; Determine sign and normalize EAX
    cmp eax,0
    jl .neg_value
    push dword 0            ; sign = positive
    jmp .check_zero
.neg_value:
    neg eax
    push dword 1            ; sign = negative

.check_zero:
    ; If value is zero, emit '0' directly
    test eax,eax
    jnz .convert_loop
    dec ecx
    mov byte [ecx],'0'
    jmp .post_convert

.convert_loop:
    ; Convert digits backward into buffer
    xor edx,edx
    div ebx                 ; EAX = EAX / 10, EDX = remainder
    add dl,'0'
    dec ecx
    mov [ecx],dl
    test eax,eax
    jnz .convert_loop

.post_convert:
    ; Prepend '-' if negative
    pop edx                 ; edx = 0 (pos) or 1 (neg)
    test edx,edx
    jz .length_calc
    dec ecx
    mov byte [ecx],'-'

.length_calc:
    ; edx = length = tail - ecx
    mov edx,ebp
    add edx,15              ; tail pointer
    sub edx,ecx             ; length
    ; ecx points to start of string
    ret

_start:
; --- First number ---
read_num1:
    mov ecx,prompt1
    mov edx,len1
    call write_str
    mov ecx,num1
    mov edx,16
    call read_str
    mov ecx,num1
    call atoi_validate
    cmp eax,-1
    je read_num1
    mov ebp,eax          ; numerator

; --- Second number ---
read_num2:
    mov ecx,prompt2
    mov edx,len2
    call write_str
    mov ecx,num2
    mov edx,16
    call read_str
    mov ecx,num2
    call atoi_validate
    cmp eax,-1
    je read_num2
    mov edi,eax          ; denominator

; --- Operator ---
read_op:
    mov ecx,promptOp
    mov edx,lenOp
    call write_str
    mov ecx,op
    mov edx,2
    call read_str
    mov al,[op]
    cmp al,'+'
    je do_add
    cmp al,'-'
    je do_sub
    cmp al,'*'
    je do_mul
    cmp al,'/'
    je do_div
    mov ecx,invalidMsg
    mov edx,lenInvalid
    call write_str
    jmp read_op

; --- Operations ---
do_add:
    mov eax,ebp
    add eax,edi
    mov [resval],eax
    jmp print_result

do_sub:
    mov eax,ebp
    sub eax,edi
    mov [resval],eax
    jmp print_result

do_mul:
    mov eax,ebp
    imul eax,edi
    mov [resval],eax
    jmp print_result

do_div:
    mov eax,ebp
    mov ecx,edi
    cmp ecx,0
    je zero_div

    xor edx,edx
    div ecx             ; eax=quotient, edx=remainder
    mov [resval],eax
    mov [remval],edx
    mov [denval],ecx
    jmp print_division

zero_div:
    mov ecx,zeroDivMsg
    mov edx,lenZeroDiv
    call write_str
    mov ecx,newline
    mov edx,lenNL
    call write_str
    jmp exit_program

; --- Print integer result ---
print_result:
    mov ecx,resultMsg
    mov edx,lenRes
    call write_str

    mov eax,[resval]
    mov ecx,outbuf
    call itoa
    call write_str

    mov ecx,newline
    mov edx,lenNL
    call write_str
    jmp exit_program

; --- Print division result with buffered fractional digits ---
print_division:
    mov ecx,resultMsg
    mov edx,lenRes
    call write_str

    ; print quotient
    mov eax,[resval]
    mov ecx,outbuf
    call itoa
    call write_str

    ; load remainder and check
    mov esi,[remval]
    cmp esi,0
    je .no_fraction

    ; print decimal point
    mov ecx,decimalPoint
    mov edx,lenDec
    call write_str

    ; compute up to 4 fractional digits into fracbuf without syscalls
    xor edi,edi              ; edi = index (0..3)
.compute_loop:
    cmp edi,4
    je .done_compute
    cmp esi,0
    je .done_compute

    mov eax,esi
    imul eax,10
    xor edx,edx
    mov ecx,[denval]
    div ecx                  ; eax=digit 0..9, edx=new remainder

    mov bl,al
    add bl,'0'
    mov [fracbuf + edi],bl   ; store ASCII digit

    mov esi,edx              ; carry forward remainder
    inc edi
    jmp .compute_loop

.done_compute:
    ; fraclen = number of digits computed
    mov [fraclen],edi

    ; print all digits at once
    mov ecx,fracbuf
    mov edx,[fraclen]
    call write_str

    ; newline
    mov ecx,newline
    mov edx,lenNL
    call write_str
    jmp exit_program

.no_fraction:
    mov ecx,newline
    mov edx,lenNL
    call write_str
    jmp exit_program

exit_program:
    mov eax,1
    xor ebx,ebx
    int 0x80
