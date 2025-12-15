Contributing
============

Thanks for your interest in contributing to this project! We welcome
issues, bug reports, feature requests, documentation improvements, and
pull requests.

How to contribute
-----------------

1. Search existing issues to see if your bug or feature has already been
   reported or discussed. If not, open a new issue describing the problem or
   proposed change.

1. For code changes, fork the repository and create a topic branch:

```bash
git checkout -b fix/short-description
```

1. Write tests and run them locally when applicable. Keep changes focused and
   small so they’re easy to review.

1. Format code consistently. This project follows common Python conventions
   (PEP 8). You can use `black` or `ruff`/`flake8` as needed.

1. Commit messages should be clear and descriptive. Use the present tense and
   reference the issue number when appropriate.

1. Open a pull request against the `main` branch and describe the change,
   why it’s needed, and any relevant notes for reviewers.

Review process
--------------

Pull requests will be reviewed by project maintainers. Please be responsive to
review comments and update your PR as requested. Small, well-scoped PRs get
merged faster.

Code of conduct
---------------

By participating in this project you agree to abide by the project's
`CODE_OF_CONDUCT.md`.

Licensing
---------

By contributing, you agree that your contributions will be licensed under the
same terms as this project (see `LICENSE`). If your contribution contains
substantial third-party content, disclose its origin and license.

Local development
-----------------

- Create and activate a virtualenv: `python3 -m venv .venv && . .venv/bin/activate`
- Install requirements: `pip install -r requirements.txt`
- Run the sample interaction: `python3 python_ai_2.py --run-sample`

Thank you for contributing!
