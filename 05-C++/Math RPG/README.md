# Math RPG (GreyThink)

A small command-line trigonometry quiz/game written in C++.

This repository contains a compact interactive program, `math_rpg.cpp`, that generates a variety of math problems (trigonometry identities, vector math, complex numbers, cycloids, projectile motion, conic sections, etc.) and gives feedback. The game deducts HP for incorrect answers and presents problems in a friendly textual format.

This project is intended as an educational tool and a fun demo of mathematical utilities in C++.

---

## Repository

- Name: GreyThink
- Owner: `Robert-Marshall01`
- Contact: RobertCMarshall2007@outlook.com

## Contents

- `math_rpg.cpp` — main source file (single-file C++ program)

## Build (recommended)

Prerequisites: a C++17-capable compiler (g++/clang++), build tools.

Windows PowerShell (example):

```powershell
# Compile
g++ -std=c++17 -O2 -o "math_rpg.exe" -Wall -Wextra "math_rpg.cpp"

# Run
.\math_rpg.exe
```

Linux / macOS:

```bash
# Compile
g++ -std=c++17 -O2 -o math_rpg math_rpg.cpp

# Run
./math_rpg
```

Notes:
- I sometimes compile to `math_rpg_build.exe` during development to avoid overwriting a running instance; you can use the commands above to produce your preferred executable name.

## Usage

The game is interactive. It shows an HP bar and presents math problems. Answer numeric questions by typing a decimal or a radian expression using `pi` (e.g. `pi/4`, `3pi/2`) — the parser also accepts the Unicode π character in many common encodings.

For two-value questions (coordinates, or modulus and argument), enter two values separated by space. The program checks answers rounded to the nearest hundredth.

Example cycloid prompt after changes:

```
For cycloid with r=5, compute x and y at t=3π/2 (≈ 4.71) (enter x and y separated by space):
```

Expected rounded answer (x y): `28.56 5.00`

## Tests / Automation

This repository includes a small, lightweight unit test (`tests/test_math.cpp`) and a deterministic sample/run mode for automated verification. The GitHub Actions workflow compiles the project and runs the tests on both Linux and Windows.

Run tests locally (Windows PowerShell):

```powershell
g++ -std=c++17 -O2 -o tests\test_math.exe tests\test_math.cpp
.\tests\test_math.exe
```

Or compile the main program and run the deterministic sample mode (prints questions and expected answers):

```powershell
g++ -std=c++17 -O2 -o math_rpg_build.exe math_rpg.cpp
.\math_rpg_build.exe --sample 12345 5
```

## Known issues / Disclaimer

This project is a small educational/demo program. While I've reviewed the formulas and arithmetic, there may still be bugs or arithmetic errors in some questions or edge cases. Use the software for learning and entertainment; do not rely on it for high-stakes calculations. If you find an error, please open an issue with steps to reproduce.

## Contributing

If you want to contribute:

- Fork the repo and open a PR.
- Follow any style or formatting guidelines you prefer (I recommend `clang-format` for C++).

Before submitting a PR, make sure the project still compiles on Linux/Windows.

## License

This project is licensed under the MIT License — see the included `LICENSE` file for details.

## Acknowledgements

Created by Robert Marshall (GitHub: `Robert-Marshall01`). Contact: RobertCMarshall2007@outlook.com

Note: This project was initially generated with the assistance of an AI and was subsequently reviewed and modified by Robert Marshall.

---

If you'd like, I can also add a `.gitignore`, a simple CI workflow, and a small non-interactive test harness to make publishing to GitHub painless.
# Math RPG (Trigonometry)

A small console Math RPG where the player answers trigonometry problems. Wrong answers deal damage. The goal is to survive as long as you can.

## Summary

- Player starts with 3000 HP.
- The HP bar is shown as 30 sticks (1 stick = 100 HP):

  HP [IIIIIIIIIIIIIIIIIIIIIIIIIIIIII] 3000 / 3000

- When computing the number of sticks for display we use ceil to always round up to the next 100 (e.g., 609 HP => 7 sticks).
- Colors (ANSI escapes):
  - Green: > 50% HP
  - Yellow: <= 50% and > 20%
  - Red: <= 20%

## Damage rules

- Base damage on a wrong answer is 1400 HP.
- A random multiplier between 0.5 and 2.0 is applied: damage = round(1400 * multiplier).
- Damage is clamped to the range 700–2800 HP.
- If HP reaches 0 you lose.

## Questions

- The game generates random trig questions (sin, cos, tan) at common unit-circle angles (0°, 30°, 45°, 60°, 90°, ...).
- The program asks for decimal answers (e.g. `0.7071`).
- Answers are accepted if they are within a small tolerance (absolute or relative ~1e-3).

## Files

- `math_rpg.cpp` — The C++ source for the game.
- `README.md` — This file.

## Compile & Run (Windows PowerShell)

If you have g++ installed (MinGW/TDM-GCC, MSYS2, etc.), from the project folder run:

```powershell
g++ -std=c++17 -O2 -o "math_rpg.exe" -Wall -Wextra; .\math_rpg.exe
```

You can type `quit` or `exit` at a prompt to leave the game.

## Notes & Compatibility

- The program prints ANSI color escape sequences for the HP bar. Modern Windows Terminal and recent PowerShell/ConHost versions support ANSI sequences. If your terminal doesn't show colors, the bar will still display correctly but without color.
- The program expects decimal numeric input; symbolic answers like `sqrt(2)/2` are not parsed.

## Next steps / Enhancements

- Accept symbolic answers (e.g., `sqrt(2)/2`) via a small parser or a table of accepted canonical strings.
- Add scoring, XP, levels, and rewards for streaks of correct answers.
- Add save/load, menus, or a GUI using SDL2 or a simple web UI.

## License

This project is provided as-is for learning and personal use.
