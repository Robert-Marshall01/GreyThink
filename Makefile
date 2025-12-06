# Makefile for GreyThink - Multi-language Support Example
# This Makefile includes example build/run targets for various languages.
# Edit or extend as needed for new programs or language directories.

.PHONY: all clean fortran c csharp python rust cpp

all: fortran c csharp python rust cpp

# Example Fortran build (assumes main.f90 in fortran/ directory)
fortran:
	@echo "Building Fortran programs..."
	@if [ -f fortran/main.f90 ]; then \
	  mkdir -p bin; \
	  gfortran fortran/main.f90 -o bin/main_fortran; \
	  echo "Built Fortran executable in bin/main_fortran"; \
	else \
	  echo "No Fortran program found (fortran/main.f90 missing)"; \
	fi

# Example C build (assumes main.c in c/ directory)
c:
	@echo "Building C programs..."
	@if [ -f c/main.c ]; then \
	  mkdir -p bin; \
	  gcc c/main.c -o bin/main_c; \
	  echo "Built C executable in bin/main_c"; \
	else \
	  echo "No C program found (c/main.c missing)"; \
	fi

# Example C# build (assumes Program.cs in csharp/ directory)
csharp:
	@echo "Building C# programs..."
	@if [ -f csharp/Program.cs ]; then \
	  dotnet build csharp; \
	else \
	  echo "No C# program found (csharp/Program.cs missing)"; \
	fi

# Example Python run (assumes main.py in python/ directory)
python:
	@echo "Running Python program..."
	@if [ -f python/main.py ]; then \
	  python3 python/main.py; \
	else \
	  echo "No Python program found (python/main.py missing)"; \
	fi

# Example Rust build (assumes Cargo.toml in rust/ directory)
rust:
	@echo "Building Rust programs..."
	@if [ -f rust/Cargo.toml ]; then \
	  cd rust && cargo build --release; \
	else \
	  echo "No Rust project found (rust/Cargo.toml missing)"; \
	fi

# Example C++ build (assumes main.cpp in cpp/ directory)
cpp:
	@echo "Building C++ programs..."
	@if [ -f cpp/main.cpp ]; then \
	  mkdir -p bin; \
	  g++ cpp/main.cpp -o bin/main_cpp; \
	  echo "Built C++ executable in bin/main_cpp"; \
	else \
	  echo "No C++ program found (cpp/main.cpp missing)"; \
	fi

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf bin
	@if [ -d rust ]; then \
	  cd rust && cargo clean; \
	fi
	@if [ -d csharp/bin ]; then \
	  rm -rf csharp/bin csharp/obj; \
	fi
	@echo "Clean complete."
