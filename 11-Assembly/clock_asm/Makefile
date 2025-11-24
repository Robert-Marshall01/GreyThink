# Simple Makefile to build using NASM (Linux/macOS)
.PHONY: all clean

ASM=clock_asm.asm
OUT=clock_asm

all: build

build:
	@if [ -f $(ASM) ]; then \
		if command -v nasm >/dev/null 2>&1; then \
			nasm -f elf64 $(ASM) -o $(OUT).o && ld $(OUT).o -o $(OUT) || echo "Build failed (nasm/ld)"; \
		else \
			echo "nasm not found; install nasm to build"; \
		fi \
	else \
		echo "$(ASM) not found"; \
	fi

clean:
	rm -f $(OUT) $(OUT).o
