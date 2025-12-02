# Makefile for building and running the Fortran demos

FC = gfortran
FFLAGS = -std=f2008 -O2 -fdefault-real-8
TARGET = fortran_calculus.exe
SRC = fortran_calculus.f90

.PHONY: all build run clean

all: build

build:
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

run: build
	./$(TARGET)

clean:
	rm -f $(TARGET) *.o *.mod
