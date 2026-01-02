# Contributing to GreyAV

Thank you for your interest in contributing to GreyAV! This document provides guidelines and instructions for contributing.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [How to Contribute](#how-to-contribute)
- [Pull Request Process](#pull-request-process)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment. We expect all contributors to:

- Be respectful of differing viewpoints and experiences
- Accept constructive criticism gracefully
- Focus on what is best for the community
- Show empathy towards other community members

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/greyav.git
   cd greyav
   ```
3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/greyav/greyav.git
   ```

## Development Setup

### Prerequisites

- Python 3.8 or higher
- Git
- (Optional) Docker for container testing

### Setting Up Development Environment

```bash
# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate  # Linux/macOS
# or .venv\Scripts\activate  # Windows

# Install dependencies
pip install -r requirements.txt

# Install development dependencies
pip install -e ".[dev]"

# Verify installation
python3 greyav.py --help
```

### Running Tests

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=greyav --cov-report=html

# Run specific test file
pytest tests/test_scanner.py
```

## How to Contribute

### Reporting Bugs

Before submitting a bug report:

1. Check existing issues to avoid duplicates
2. Collect information about the bug:
   - Python version (`python3 --version`)
   - Operating system and version
   - GreyAV version (`python3 greyav.py update`)
   - Steps to reproduce
   - Expected vs actual behavior
   - Error messages and stack traces

**Bug Report Template**:
```markdown
**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Run command '...'
2. With file '...'
3. See error

**Expected behavior**
What you expected to happen.

**Environment**
- OS: [e.g., Ubuntu 22.04]
- Python: [e.g., 3.11.4]
- GreyAV: [e.g., 5.0.0]

**Additional context**
Add any other context about the problem here.
```

### Suggesting Features

We welcome feature suggestions! Please include:

1. Clear description of the feature
2. Use case and motivation
3. Potential implementation approach (optional)
4. Whether you're willing to implement it

### Contributing Code

1. **Find an issue** to work on, or create one for discussion
2. **Comment on the issue** to indicate you're working on it
3. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```
4. **Make your changes** following our coding standards
5. **Test your changes** thoroughly
6. **Commit with clear messages**:
   ```bash
   git commit -m "feat: add ransomware detection for XYZ variant"
   ```
7. **Push and create a Pull Request**

## Pull Request Process

### Before Submitting

- [ ] Code follows the project's style guidelines
- [ ] Self-review of code completed
- [ ] Comments added for complex logic
- [ ] Documentation updated if needed
- [ ] Tests added for new functionality
- [ ] All tests pass locally
- [ ] No sensitive data (keys, passwords) in commits

### PR Title Format

Use conventional commit format:

- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation only
- `style:` - Formatting, no code change
- `refactor:` - Code restructuring
- `test:` - Adding tests
- `chore:` - Maintenance tasks

Examples:
- `feat: add YARA rule support for custom signatures`
- `fix: resolve memory leak in real-time monitor`
- `docs: update installation instructions for NixOS`

### Review Process

1. Maintainers will review your PR within 7 days
2. Address any requested changes
3. Once approved, a maintainer will merge your PR

## Coding Standards

### Python Style

- Follow [PEP 8](https://pep8.org/) style guide
- Use [Black](https://black.readthedocs.io/) for formatting (line length: 100)
- Use type hints for function signatures
- Maximum line length: 100 characters

```python
# Good
def scan_file(
    file_path: Path,
    use_heuristics: bool = True,
    threshold: int = 50
) -> ScanResult:
    """
    Scan a file for malware.
    
    Args:
        file_path: Path to the file to scan
        use_heuristics: Enable heuristic analysis
        threshold: Heuristic score threshold (0-100)
    
    Returns:
        ScanResult containing detection information
    """
    ...
```

### Documentation

- Use docstrings for all public functions and classes
- Follow Google-style docstring format
- Keep README.md updated for new features

### Security Considerations

When contributing security-related code:

- Never hardcode secrets or credentials
- Validate and sanitize all user inputs
- Use secure comparison for sensitive data
- Log security events appropriately (redact sensitive info)
- Consider timing attacks for cryptographic operations

## Testing

### Writing Tests

```python
# tests/test_scanner.py
import pytest
from greyav import GreyAV

class TestScanner:
    def test_scan_clean_file(self, tmp_path):
        """Test scanning a clean file returns no threats."""
        clean_file = tmp_path / "clean.txt"
        clean_file.write_text("Hello, world!")
        
        av = GreyAV()
        result = av.scan_file(str(clean_file))
        
        assert result["threat_detected"] is False
    
    def test_scan_eicar(self, tmp_path):
        """Test EICAR test file is detected."""
        eicar = tmp_path / "eicar.txt"
        eicar.write_text(
            "X5O!P%@AP[4\\PZX54(P^)7CC)7}$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"
        )
        
        av = GreyAV()
        result = av.scan_file(str(eicar))
        
        assert result["threat_detected"] is True
```

### Test Categories

- **Unit tests**: Test individual functions/methods
- **Integration tests**: Test component interactions
- **Security tests**: Test security-sensitive functionality

## Project Structure

```
greyav/
├── greyav.py              # Main CLI and scanner
├── auto_threat_manager.py # Automatic threat response
├── behavioral_engine.py   # Behavioral analysis
├── tests/                 # Test directory
│   ├── test_scanner.py
│   ├── test_quarantine.py
│   └── conftest.py
└── ...
```

## Getting Help

- **GitHub Issues**: For bugs and feature requests
- **Discussions**: For questions and general discussion
- **Security Issues**: See [SECURITY.md](SECURITY.md)

---

Thank you for contributing to GreyAV! Together, we're building the future of asymmetric cyber defense. 🛡️
