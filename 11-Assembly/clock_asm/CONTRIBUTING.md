# Contributing

Thanks for considering contributing to `clock_asm` — small projects benefit a lot from clear, small changes.

How to contribute

- Fork the repository and create a branch for your change.
- Prefer small, focused pull requests (one change per PR).
- If your change affects building or requires a specific assembler/toolchain, update `ARCHITECTURE.md` to add or update a tested entry and set `tested: true` after verifying locally.
- Include a short description of how you tested the change in the PR description.

Testing and Toolchain

- If you add support for a new assembler or platform, include the exact assembler/linker versions you used and any commands in the PR (or update `ARCHITECTURE.md`).
- If you add a build script, keep it idempotent and safe to run on a clean checkout.

Style

- Use clear, descriptive commit messages.
- Keep assembly formatting consistent with existing files.

Reporting issues

- Open an issue with a minimal reproduction and environment details (OS, assembler, version). Use the `ARCHITECTURE.md` template to report toolchain details.

Thanks again — your changes make the project better for everyone.
