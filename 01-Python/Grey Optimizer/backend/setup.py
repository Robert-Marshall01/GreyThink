"""
Grey Optimizer Backend Setup

Install with: pip install -e .
"""

from setuptools import setup, find_packages

setup(
    name="grey-optimizer",
    version="1.0.0",
    description="Production-ready Linux resource optimizer",
    author="Grey Optimizer Team",
    python_requires=">=3.11",
    packages=find_packages(),
    install_requires=[
        "aiohttp>=3.9.0",
        "aiosqlite>=0.19.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-asyncio>=0.21.0",
            "mypy>=1.0.0",
        ]
    },
    entry_points={
        "console_scripts": [
            "grey-optimizer=grey_optimizer.daemon:main",
        ]
    },
    classifiers=[
        "Development Status :: 4 - Beta",
        "Environment :: Console",
        "Intended Audience :: System Administrators",
        "License :: OSI Approved :: MIT License",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3.11",
        "Topic :: System :: Systems Administration",
    ],
)
