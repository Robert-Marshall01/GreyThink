section .data
    prompt1    db "Enter first number (0-9): "
    len1       equ $-prompt1

    prompt2    db "Enter second number (0-9): "
    len2       equ $-prompt2

    promptOp   db "Enter operation (+ - * /): "
    lenOp      equ $-promptOp

    resultMsg  db "Result: "
    lenRes     equ $-resultMsg

    newline    db 10
    lenNL      equ $-newline

section .bss
    num1   resb 2
    num2   resb 2
    op     resb 2
    resval resd 1
    buf    resb 2

section .text
global _start

_start:
    ; --- First number ---
    mov eax,4
    mov ebx,1
    mov ecx,prompt1
    mov edx,len1
    int 0x80

    mov eax,3
    mov ebx,0
    mov ecx,num1
    mov edx,2
    int 0x80

    mov al,[num1]
    sub al,'0'
    movzx esi, al        ; first operand in ESI

    ; --- Second number ---
    mov eax,4
    mov ebx,1
    mov ecx,prompt2
    mov edx,len2
    int 0x80

    mov eax,3
    mov ebx,0
    mov ecx,num2
    mov edx,2
    int 0x80

    mov al,[num2]
    sub al,'0'
    movzx edi, al        ; second operand in EDI

    ; --- Operator ---
read_op:
    mov eax,4
    mov ebx,1
    mov ecx,promptOp
    mov edx,lenOp
    int 0x80

    mov eax,3
    mov ebx,0
    mov ecx,op
    mov edx,2
    int 0x80

    mov al,[op]
    cmp al,'+'
    je do_add
    cmp al,'-'
    je do_sub
    cmp al,'*'
    je do_mul
    cmp al,'/'
    je do_div
    jmp read_op

; --- Operations ---
do_add:
    mov eax, esi
    add eax, edi
    mov [resval], eax
    jmp print_result

do_sub:
    mov eax, esi
    sub eax, edi
    mov [resval], eax
    jmp print_result

do_mul:
    mov eax, esi
    imul eax, edi
    mov [resval], eax
    jmp print_result

do_div:
    mov eax, esi
    mov ecx, edi
    cmp ecx,0
    je zero_div
    xor edx, edx
    div ecx
    mov [resval], eax
    jmp print_result

zero_div:
    xor eax, eax
    mov [resval], eax
    jmp print_result

; --- Print result ---
print_result:
    ; header
    mov eax,4
    mov ebx,1
    mov ecx,resultMsg
    mov edx,lenRes
    int 0x80

    mov eax,[resval]
    cmp eax,10
    jl one_digit

    ; two digits
    xor edx,edx
    mov ecx,10
    div ecx          ; EAX=quotient, EDX=remainder

    add al,'0'
    mov [buf],al
    add dl,'0'
    mov [buf+1],dl

    mov eax,4
    mov ebx,1
    mov ecx,buf
    mov edx,2
    int 0x80
    jmp done

one_digit:
    add al,'0'
    mov [buf],al
    mov eax,4
    mov ebx,1
    mov ecx,buf
    mov edx,1
    int 0x80

done:
    mov eax,4
    mov ebx,1
    mov ecx,newline
    mov edx,lenNL
    int 0x80

    mov eax,1
    xor ebx,ebx
    int 0x80