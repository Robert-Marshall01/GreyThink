; ===================================================
;  Grey Compiler -- x86-64 Assembly Output
;  Module: main
;  Target: Windows x64
;  Assembler: NASM
; ===================================================

; -- External C runtime symbols --
extern printf
extern puts
extern scanf
extern malloc
extern free
extern exit
extern fflush

default rel

section .rdata
    __fmt_int:    db "%lld", 0
    __fmt_intln:  db "%lld", 10, 0
    __fmt_float:  db "%g", 0
    __fmt_floatln:db "%g", 10, 0
    __fmt_str:    db "%s", 0
    __fmt_strln:  db "%s", 10, 0
    __fmt_true:   db "true", 0
    __fmt_false:  db "false", 0
    __fmt_nil:    db "nil", 0
    __fmt_newline:db 10, 0
    __fmt_scanf_int: db "%lld", 0
    __fmt_scanf_str: db "%255s", 0

    __str_0: db "=== Grey Native Compilation Test ===", 0
    __str_1: db "Factorial of 10:", 0
    __str_2: db "Fibonacci of 15:", 0
    __str_3: db "=== All tests passed ===", 0

section .text
global main

; ── function __main__ ──
main:
    push rbp
    mov rbp, rsp
    sub rsp, 208
.entry:
    mov qword [rbp-8], 0  ; alloca %t34
    mov rax, 10
    mov [rbp-8], rax  ; store -> %t34
    mov qword [rbp-16], 0  ; alloca %t35
    mov rax, 20
    mov [rbp-16], rax  ; store -> %t35
    mov rax, [rbp-8]  ; load %t34
    mov [rbp-24], rax  ; store %t36
    mov rax, [rbp-16]  ; load %t35
    mov [rbp-32], rax  ; store %t37
    sub rsp, 32
    mov rcx, [rbp-24]  ; load %t36
    mov rdx, [rbp-32]  ; load %t37
    call _grey_add
    add rsp, 32
    mov [rbp-40], rax  ; store %t38
    mov qword [rbp-48], 0  ; alloca %t39
    mov rax, [rbp-40]  ; load %t38
    mov [rbp-48], rax  ; store -> %t39
    sub rsp, 32
    mov rcx, 10
    call _grey_factorial
    add rsp, 32
    mov [rbp-56], rax  ; store %t40
    mov qword [rbp-64], 0  ; alloca %t41
    mov rax, [rbp-56]  ; load %t40
    mov [rbp-64], rax  ; store -> %t41
    sub rsp, 32
    mov rcx, 15
    call _grey_fibonacci
    add rsp, 32
    mov [rbp-72], rax  ; store %t42
    mov qword [rbp-80], 0  ; alloca %t43
    mov rax, [rbp-72]  ; load %t42
    mov [rbp-80], rax  ; store -> %t43
    mov qword [rbp-88], 0  ; alloca %t44
    mov rax, 10
    mov [rbp-88], rax  ; store -> %t44
    mov qword [rbp-96], 0  ; alloca %t45
    mov rax, 1024
    mov [rbp-96], rax  ; store -> %t45
    sub rsp, 32
    lea rcx, [rel __fmt_strln]
    lea rdx, [rel __str_0]
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    mov rax, [rbp-48]  ; load %t39
    mov [rbp-112], rax  ; store %t47
    mov r10, [rbp-112]  ; load %t47
    sub rsp, 32
    lea rcx, [rel __fmt_intln]
    mov rdx, r10
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    sub rsp, 32
    lea rcx, [rel __fmt_strln]
    lea rdx, [rel __str_1]
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    mov rax, [rbp-64]  ; load %t41
    mov [rbp-136], rax  ; store %t50
    mov r10, [rbp-136]  ; load %t50
    sub rsp, 32
    lea rcx, [rel __fmt_intln]
    mov rdx, r10
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    sub rsp, 32
    lea rcx, [rel __fmt_strln]
    lea rdx, [rel __str_2]
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    mov rax, [rbp-80]  ; load %t43
    mov [rbp-160], rax  ; store %t53
    mov r10, [rbp-160]  ; load %t53
    sub rsp, 32
    lea rcx, [rel __fmt_intln]
    mov rdx, r10
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    mov rax, [rbp-88]  ; load %t44
    mov [rbp-176], rax  ; store %t55
    mov r10, [rbp-176]  ; load %t55
    sub rsp, 32
    lea rcx, [rel __fmt_intln]
    mov rdx, r10
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    mov rax, [rbp-96]  ; load %t45
    mov [rbp-192], rax  ; store %t57
    mov r10, [rbp-192]  ; load %t57
    sub rsp, 32
    lea rcx, [rel __fmt_intln]
    mov rdx, r10
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    sub rsp, 32
    lea rcx, [rel __fmt_strln]
    lea rdx, [rel __str_3]
    xor eax, eax
    call printf
    add rsp, 32
    sub rsp, 32
    xor rcx, rcx
    call fflush
    add rsp, 32
    xor eax, eax
    mov rsp, rbp
    pop rbp
    ret

; ── function add ──
global _grey_add
_grey_add:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov [rbp-8], rcx  ; param a
    mov [rbp-16], rdx  ; param b
.entry:
    mov qword [rbp-24], 0  ; alloca %t1
    mov rax, [rbp-8]  ; load %arg0
    mov [rbp-24], rax  ; store -> %t1
    mov qword [rbp-32], 0  ; alloca %t2
    mov rax, [rbp-16]  ; load %arg1
    mov [rbp-32], rax  ; store -> %t2
    mov rax, [rbp-24]  ; load %t1
    mov [rbp-40], rax  ; store %t3
    mov rax, [rbp-32]  ; load %t2
    mov [rbp-48], rax  ; store %t4
    mov rax, [rbp-40]  ; load %t3
    mov rcx, [rbp-48]  ; load %t4
    add rax, rcx
    mov [rbp-56], rax  ; store %t5
    mov rax, [rbp-56]  ; load %t5
    mov rsp, rbp
    pop rbp
    ret

; ── function factorial ──
global _grey_factorial
_grey_factorial:
    push rbp
    mov rbp, rsp
    sub rsp, 80
    mov [rbp-8], rcx  ; param n
.entry:
    mov qword [rbp-16], 0  ; alloca %t6
    mov rax, [rbp-8]  ; load %arg0
    mov [rbp-16], rax  ; store -> %t6
    mov rax, [rbp-16]  ; load %t6
    mov [rbp-24], rax  ; store %t7
    mov rax, [rbp-24]  ; load %t7
    mov rcx, 1
    cmp rax, rcx
    setle al
    movzx rax, al
    mov [rbp-32], rax  ; store %t8
    mov rax, [rbp-32]  ; load %t8
    test rax, rax
    jnz .then_1
    jmp .if_merge_2
.then_1:
    mov rax, 1
    mov rsp, rbp
    pop rbp
    ret
.if_merge_2:
    mov rax, [rbp-16]  ; load %t6
    mov [rbp-40], rax  ; store %t9
    mov rax, [rbp-16]  ; load %t6
    mov [rbp-48], rax  ; store %t10
    mov rax, [rbp-48]  ; load %t10
    mov rcx, 1
    sub rax, rcx
    mov [rbp-56], rax  ; store %t11
    sub rsp, 32
    mov rcx, [rbp-56]  ; load %t11
    call _grey_factorial
    add rsp, 32
    mov [rbp-64], rax  ; store %t12
    mov rax, [rbp-40]  ; load %t9
    mov rcx, [rbp-64]  ; load %t12
    imul rax, rcx
    mov [rbp-72], rax  ; store %t13
    mov rax, [rbp-72]  ; load %t13
    mov rsp, rbp
    pop rbp
    ret

; ── function fibonacci ──
global _grey_fibonacci
_grey_fibonacci:
    push rbp
    mov rbp, rsp
    sub rsp, 176
    mov [rbp-8], rcx  ; param n
.entry:
    mov qword [rbp-16], 0  ; alloca %t14
    mov rax, [rbp-8]  ; load %arg0
    mov [rbp-16], rax  ; store -> %t14
    mov rax, [rbp-16]  ; load %t14
    mov [rbp-24], rax  ; store %t15
    mov rax, [rbp-24]  ; load %t15
    mov rcx, 0
    cmp rax, rcx
    setle al
    movzx rax, al
    mov [rbp-32], rax  ; store %t16
    mov rax, [rbp-32]  ; load %t16
    test rax, rax
    jnz .then_3
    jmp .if_merge_4
.then_3:
    mov rax, 0
    mov rsp, rbp
    pop rbp
    ret
.if_merge_4:
    mov rax, [rbp-16]  ; load %t14
    mov [rbp-40], rax  ; store %t17
    mov rax, [rbp-40]  ; load %t17
    mov rcx, 1
    cmp rax, rcx
    sete al
    movzx rax, al
    mov [rbp-48], rax  ; store %t18
    mov rax, [rbp-48]  ; load %t18
    test rax, rax
    jnz .then_5
    jmp .if_merge_6
.then_5:
    mov rax, 1
    mov rsp, rbp
    pop rbp
    ret
.if_merge_6:
    mov qword [rbp-56], 0  ; alloca %t19
    mov rax, 0
    mov [rbp-56], rax  ; store -> %t19
    mov qword [rbp-64], 0  ; alloca %t20
    mov rax, 1
    mov [rbp-64], rax  ; store -> %t20
    mov qword [rbp-72], 0  ; alloca %t21
    mov rax, 2
    mov [rbp-72], rax  ; store -> %t21
    jmp .while_cond_7
.while_cond_7:
    mov rax, [rbp-72]  ; load %t21
    mov [rbp-80], rax  ; store %t22
    mov rax, [rbp-16]  ; load %t14
    mov [rbp-88], rax  ; store %t23
    mov rax, [rbp-80]  ; load %t22
    mov rcx, [rbp-88]  ; load %t23
    cmp rax, rcx
    setle al
    movzx rax, al
    mov [rbp-96], rax  ; store %t24
    mov rax, [rbp-96]  ; load %t24
    test rax, rax
    jnz .while_body_8
    jmp .while_exit_9
.while_body_8:
    mov rax, [rbp-56]  ; load %t19
    mov [rbp-104], rax  ; store %t25
    mov rax, [rbp-64]  ; load %t20
    mov [rbp-112], rax  ; store %t26
    mov rax, [rbp-104]  ; load %t25
    mov rcx, [rbp-112]  ; load %t26
    add rax, rcx
    mov [rbp-120], rax  ; store %t27
    mov qword [rbp-128], 0  ; alloca %t28
    mov rax, [rbp-120]  ; load %t27
    mov [rbp-128], rax  ; store -> %t28
    mov rax, [rbp-64]  ; load %t20
    mov [rbp-136], rax  ; store %t29
    mov rax, [rbp-136]  ; load %t29
    mov [rbp-56], rax  ; store -> %t19
    mov rax, [rbp-128]  ; load %t28
    mov [rbp-144], rax  ; store %t30
    mov rax, [rbp-144]  ; load %t30
    mov [rbp-64], rax  ; store -> %t20
    mov rax, [rbp-72]  ; load %t21
    mov [rbp-152], rax  ; store %t31
    mov rax, [rbp-152]  ; load %t31
    mov rcx, 1
    add rax, rcx
    mov [rbp-160], rax  ; store %t32
    mov rax, [rbp-160]  ; load %t32
    mov [rbp-72], rax  ; store -> %t21
    jmp .while_cond_7
.while_exit_9:
    mov rax, [rbp-64]  ; load %t20
    mov [rbp-168], rax  ; store %t33
    mov rax, [rbp-168]  ; load %t33
    mov rsp, rbp
    pop rbp
    ret

