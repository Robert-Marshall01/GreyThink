# Password Strength Checker (Rust)

A tiny CLI tool to evaluate password strength and give suggestions.

Usage:

- Build with Cargo (requires Rust):

```powershell
cd your directory
cargo build --release
```

- Run:

```powershell
# pass password as argument
cargo run -- "MySecretP@ssw0rd"

# or run and type password when prompted
cargo run
Enter password: MySecretP@ssw0rd
```

Run the GUI
-----------

To start the graphical interface (egui/eframe), run the `gui` binary:

```powershell
# launches the native GUI window
cargo run --bin gui
```

The program prints a masked version of the password, a strength classification and suggestions to improve it.

Notes:
- Uses `clap` for CLI flags and `rpassword` for hidden input when prompting.
- JSON output via `serde`/`serde_json` is supported with `--json`.
- Quick common-password check included.

CLI flags
---------

- `-p, --password <PASSWORD>` : provide password inline (not recommended on shared shells)
- `--json` : print result as JSON
- `--min-score <N>` : exit with code 3 if password score is less than N

You can also provide the password as the first positional argument (no flag) for convenience. Examples:

```powershell
# positional
cargo run -- "MySecretP@ssw0rd"

# or with option
cargo run -- -p "MySecretP@ssw0rd"
```

Hidden input is used when no password is supplied (no echo) via the `rpassword` crate.

Next steps you might want:
- Expand the common-password or breached-password list to improve detection.
- Add a `--no-prompt` flag to prevent prompting in scripts.
- Add richer scoring details or export formats.

AI-generated code notice

This code (source files in this repository) was generated with the assistance of an AI tool. It is provided as a helpful starting point but may contain bugs, security issues, or imperfect edge-case handling. Please review and test thoroughly before using the code in production environments. If you find problems or want changes, I can help fix or harden the implementation.

## Privacy

This application does NOT collect, transmit, or store your passwords. All password processing (masking, scoring and checks) is performed locally on your machine. The program only prints a masked version of the password for display and returns strength/suggestion information.

If you export results (for example using `--json`) or integrate this tool with external services or scripts, take care not to send sensitive data to third parties. By default there are no network calls that send passwords off your machine.

## Disclaimer

This tool is provided "as-is" for informational and convenience purposes. The authors and maintainers are not responsible for any misuse, damages, or legal issues resulting from how you use the program or its output. You are responsible for using the tool in compliance with applicable laws and organizational policies.

## Third-party licenses

A summary of licenses for third-party dependencies is provided in `THIRD-PARTY-LICENSES.md`. That file was generated with `cargo license` and lists each dependency and its declared license(s). Include this file with any distribution of compiled binaries to satisfy attribution requirements.


