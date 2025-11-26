SHELL := /bin/sh

all: build

build:
	nasm -f elf32 input_asm.asm -o input_asm.o
	ld -m elf_i386 input_asm.o -o input_asm

clean:
	rm -f input_asm input_asm.o

.PHONY: all build clean
