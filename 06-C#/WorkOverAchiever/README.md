# GreyThink

Personal .NET application.

> Note: This README (and parts of the project) were generated with the help of an AI and later modified by the author. It may contain inaccuracies or bugs — use caution and verify behavior before relying on it.

## Overview

This repository contains the `GreyThink` project, a .NET 8 application developed by Robert Marshall.

## Requirements

- .NET 8 SDK
- Visual Studio 2022/2023 or `dotnet` CLI

## Dependencies / Install

1. Install .NET 8 SDK

- Download and install the .NET 8 SDK for your OS from the official .NET downloads (e.g. `https://dotnet.microsoft.com`).
- Verify installation:

```bash
dotnet --info
# or
dotnet --list-sdks
```

2. Restore NuGet packages

From the repository root (where the solution or project folders are located) run:

```bash
# restore for the solution (if you have a solution file)
dotnet restore

# or restore for the project directly
dotnet restore WorkOverAchiever/WorkOverAchiever.csproj
```

3. (Optional) Restore .NET tools

If the project uses local/dotnet tools, run:

```bash
dotnet tool restore
```

4. Visual Studio users

- Make sure the "Desktop development with C#" workload is installed to build/run WPF apps.
- Open the solution in Visual Studio and use the Restore, Build and Run commands from the IDE.

Notes

- Do not commit `bin/`, `obj/`, or NuGet package binaries. Use a `.gitignore` for .NET.
- If you need reproducible restores, include a lock file (`packages.lock.json`) or `global.json` to pin the SDK.

## Build and run

From the project directory (`WorkOverAchiever/WorkOverAchiever`):

```bash
dotnet restore
dotnet build --configuration Release
dotnet run --project WorkOverAchiever.csproj
```

Or open the solution in Visual Studio and run.

## Author

- Name: Robert-Marshall01
- Email: RobertCMarshall2007@outlook.com

## License

This project is licensed under the MIT License - see the `LICENSE` file for details.

