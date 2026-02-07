/* ======================================================= */
/*  Grey Compiler -- Generated C Code                       */
/*  Module: main                                           */
/*  This file was automatically generated. Do not edit.     */
/* ======================================================= */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* -- Grey Runtime ----------------------------- */

typedef struct {
    int64_t* data;
    int64_t  length;
    int64_t  capacity;
} GreyArray;

typedef struct {
    const char** keys;
    int64_t*     values;
    int64_t      count;
} GreyStruct;

static GreyArray* grey_array_new(int64_t capacity) {
    GreyArray* arr = (GreyArray*)malloc(sizeof(GreyArray));
    arr->data = (int64_t*)calloc(capacity > 0 ? capacity : 4, sizeof(int64_t));
    arr->length = 0;
    arr->capacity = capacity > 0 ? capacity : 4;
    return arr;
}

static void grey_array_push(GreyArray* arr, int64_t value) {
    if (arr->length >= arr->capacity) {
        arr->capacity *= 2;
        arr->data = (int64_t*)realloc(arr->data, arr->capacity * sizeof(int64_t));
    }
    arr->data[arr->length++] = value;
}

static int64_t grey_array_get(GreyArray* arr, int64_t index) {
    if (index < 0 || index >= arr->length) { fprintf(stderr, "Index out of bounds\n"); exit(1); }
    return arr->data[index];
}

static void grey_array_set(GreyArray* arr, int64_t index, int64_t value) {
    if (index < 0 || index >= arr->length) { fprintf(stderr, "Index out of bounds\n"); exit(1); }
    arr->data[index] = value;
}

static GreyStruct* grey_struct_new(int64_t count) {
    GreyStruct* s = (GreyStruct*)malloc(sizeof(GreyStruct));
    s->keys = (const char**)calloc(count, sizeof(const char*));
    s->values = (int64_t*)calloc(count, sizeof(int64_t));
    s->count = 0;
    return s;
}

static void grey_struct_set(GreyStruct* s, const char* key, int64_t value) {
    for (int64_t i = 0; i < s->count; i++) {
        if (strcmp(s->keys[i], key) == 0) { s->values[i] = value; return; }
    }
    s->keys[s->count] = key;
    s->values[s->count] = value;
    s->count++;
}

static int64_t grey_struct_get(GreyStruct* s, const char* key) {
    for (int64_t i = 0; i < s->count; i++) {
        if (strcmp(s->keys[i], key) == 0) return s->values[i];
    }
    fprintf(stderr, "Field not found: %s\n", key); exit(1);
    return 0;
}

static int64_t grey_pow(int64_t base, int64_t exp) {
    int64_t result = 1;
    for (int64_t i = 0; i < exp; i++) result *= base;
    return result;
}

static int64_t grey_abs(int64_t x) { return x < 0 ? -x : x; }
static int64_t grey_min(int64_t a, int64_t b) { return a < b ? a : b; }
static int64_t grey_max(int64_t a, int64_t b) { return a > b ? a : b; }

static GreyArray* grey_range(int64_t start, int64_t end, int inclusive) {
    int64_t count = end - start + (inclusive ? 1 : 0);
    if (count < 0) count = 0;
    GreyArray* arr = grey_array_new(count);
    for (int64_t i = start; inclusive ? i <= end : i < end; i++)
        grey_array_push(arr, i);
    return arr;
}

/* -- End Runtime ------------------------------ */

/* -- String Constants ------------------------- */
static const char __grey_str_0[] = "println";
static const char __grey_str_1[] = "=== Grey Native Compilation Test ===";
static const char __grey_str_2[] = "Factorial of 10:";
static const char __grey_str_3[] = "Fibonacci of 15:";
static const char __grey_str_4[] = "=== All tests passed ===";
static const char __grey_str_5[] = "then_1";
static const char __grey_str_6[] = "if_merge_2";
static const char __grey_str_7[] = "then_3";
static const char __grey_str_8[] = "if_merge_4";
static const char __grey_str_9[] = "then_5";
static const char __grey_str_10[] = "if_merge_6";
static const char __grey_str_11[] = "while_cond_7";
static const char __grey_str_12[] = "while_body_8";
static const char __grey_str_13[] = "while_exit_9";

/* -- Forward Declarations --------------------- */
static int64_t grey_fn_add(int64_t v_arg0, int64_t v_arg1);
static int64_t grey_fn_factorial(int64_t v_arg0);
static int64_t grey_fn_fibonacci(int64_t v_arg0);

int main(int argc, char** argv) {
    int64_t v_t34 = 0;
    int64_t v_t35 = 0;
    int64_t v_t36 = 0;
    int64_t v_t37 = 0;
    int64_t v_t38 = 0;
    int64_t v_t39 = 0;
    int64_t v_t40 = 0;
    int64_t v_t41 = 0;
    int64_t v_t42 = 0;
    int64_t v_t43 = 0;
    int64_t v_t44 = 0;
    int64_t v_t45 = 0;
    int64_t v_t46 = 0;
    int64_t v_t47 = 0;
    int64_t v_t48 = 0;
    int64_t v_t49 = 0;
    int64_t v_t50 = 0;
    int64_t v_t51 = 0;
    int64_t v_t52 = 0;
    int64_t v_t53 = 0;
    int64_t v_t54 = 0;
    int64_t v_t55 = 0;
    int64_t v_t56 = 0;
    int64_t v_t57 = 0;
    int64_t v_t58 = 0;
    int64_t v_t59 = 0;
    
    /* block: entry */
    lbl_entry:;
    v_t34 = 10LL;
    v_t35 = 20LL;
    v_t36 = v_t34;
    v_t37 = v_t35;
    v_t38 = grey_fn_add(v_t36, v_t37);
    v_t39 = v_t38;
    v_t40 = grey_fn_factorial(10LL);
    v_t41 = v_t40;
    v_t42 = grey_fn_fibonacci(15LL);
    v_t43 = v_t42;
    v_t44 = 10LL;
    v_t45 = 1024LL;
    printf("%s\n", __grey_str_1);
    fflush(stdout);
    v_t47 = v_t39;
    printf("%lld\n", (long long)v_t47);
    fflush(stdout);
    printf("%s\n", __grey_str_2);
    fflush(stdout);
    v_t50 = v_t41;
    printf("%lld\n", (long long)v_t50);
    fflush(stdout);
    printf("%s\n", __grey_str_3);
    fflush(stdout);
    v_t53 = v_t43;
    printf("%lld\n", (long long)v_t53);
    fflush(stdout);
    v_t55 = v_t44;
    printf("%lld\n", (long long)v_t55);
    fflush(stdout);
    v_t57 = v_t45;
    printf("%lld\n", (long long)v_t57);
    fflush(stdout);
    printf("%s\n", __grey_str_4);
    fflush(stdout);
    return 0;
    return 0;
}

static int64_t grey_fn_add(int64_t v_arg0, int64_t v_arg1) {
    int64_t v_t1 = 0;
    int64_t v_t2 = 0;
    int64_t v_t3 = 0;
    int64_t v_t4 = 0;
    int64_t v_t5 = 0;
    
    /* block: entry */
    lbl_entry:;
    v_t1 = v_arg0;
    v_t2 = v_arg1;
    v_t3 = v_t1;
    v_t4 = v_t2;
    v_t5 = (v_t3) + (v_t4);
    return v_t5;
}

static int64_t grey_fn_factorial(int64_t v_arg0) {
    int64_t v_t6 = 0;
    int64_t v_t7 = 0;
    int64_t v_t8 = 0;
    int64_t v_t9 = 0;
    int64_t v_t10 = 0;
    int64_t v_t11 = 0;
    int64_t v_t12 = 0;
    int64_t v_t13 = 0;
    
    /* block: entry */
    lbl_entry:;
    v_t6 = v_arg0;
    v_t7 = v_t6;
    v_t8 = (v_t7) <= (1LL);
    if (v_t8) goto lbl_then_1; else goto lbl_if_merge_2;
    /* block: then_1 */
    lbl_then_1:;
    return 1LL;
    /* block: if_merge_2 */
    lbl_if_merge_2:;
    v_t9 = v_t6;
    v_t10 = v_t6;
    v_t11 = (v_t10) - (1LL);
    v_t12 = grey_fn_factorial(v_t11);
    v_t13 = (v_t9) * (v_t12);
    return v_t13;
}

static int64_t grey_fn_fibonacci(int64_t v_arg0) {
    int64_t v_t14 = 0;
    int64_t v_t15 = 0;
    int64_t v_t16 = 0;
    int64_t v_t17 = 0;
    int64_t v_t18 = 0;
    int64_t v_t19 = 0;
    int64_t v_t20 = 0;
    int64_t v_t21 = 0;
    int64_t v_t22 = 0;
    int64_t v_t23 = 0;
    int64_t v_t24 = 0;
    int64_t v_t25 = 0;
    int64_t v_t26 = 0;
    int64_t v_t27 = 0;
    int64_t v_t28 = 0;
    int64_t v_t29 = 0;
    int64_t v_t30 = 0;
    int64_t v_t31 = 0;
    int64_t v_t32 = 0;
    int64_t v_t33 = 0;
    
    /* block: entry */
    lbl_entry:;
    v_t14 = v_arg0;
    v_t15 = v_t14;
    v_t16 = (v_t15) <= (0LL);
    if (v_t16) goto lbl_then_3; else goto lbl_if_merge_4;
    /* block: then_3 */
    lbl_then_3:;
    return 0LL;
    /* block: if_merge_4 */
    lbl_if_merge_4:;
    v_t17 = v_t14;
    v_t18 = (v_t17) == (1LL);
    if (v_t18) goto lbl_then_5; else goto lbl_if_merge_6;
    /* block: then_5 */
    lbl_then_5:;
    return 1LL;
    /* block: if_merge_6 */
    lbl_if_merge_6:;
    v_t19 = 0LL;
    v_t20 = 1LL;
    v_t21 = 2LL;
    goto lbl_while_cond_7;
    /* block: while_cond_7 */
    lbl_while_cond_7:;
    v_t22 = v_t21;
    v_t23 = v_t14;
    v_t24 = (v_t22) <= (v_t23);
    if (v_t24) goto lbl_while_body_8; else goto lbl_while_exit_9;
    /* block: while_body_8 */
    lbl_while_body_8:;
    v_t25 = v_t19;
    v_t26 = v_t20;
    v_t27 = (v_t25) + (v_t26);
    v_t28 = v_t27;
    v_t29 = v_t20;
    v_t19 = v_t29;
    v_t30 = v_t28;
    v_t20 = v_t30;
    v_t31 = v_t21;
    v_t32 = (v_t31) + (1LL);
    v_t21 = v_t32;
    goto lbl_while_cond_7;
    /* block: while_exit_9 */
    lbl_while_exit_9:;
    v_t33 = v_t20;
    return v_t33;
}
