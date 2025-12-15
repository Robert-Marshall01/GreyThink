# Third-party licenses

This project depends on several third-party packages and the Hugging Face model
`google/flan-t5-base`. Below are links to the packages and their license files. If
you redistribute this project including any of these packages or model weights,
please ensure you comply with those licenses.

Key dependencies

- [Transformers (Hugging Face)](https://github.com/huggingface/transformers) (Apache-2.0)
- [Hugging Face Hub](https://github.com/huggingface/huggingface_hub) (Apache-2.0)
- [PyTorch](https://github.com/pytorch/pytorch) (BSD-style; see repo for details)
- [SentencePiece](https://github.com/google/sentencepiece) (Apache-2.0)
- [tokenizers](https://github.com/huggingface/tokenizers) (MIT)

Model

- [google/flan-t5-base](https://huggingface.co/google/flan-t5-base) (model card & license)
  - The model is distributed under the Apache License 2.0. A copy of the license is
    included in `models/COPYING.APACHE2.txt` and the model NOTICE is at
    `models/google-flan-t5-base.NOTICE.txt`.

Automated license reporting

To generate a full, exact list of installed packages and their licenses in your
environment, you can use `pip-licenses`:

```bash
pip install pip-licenses
pip-licenses --format=markdown > THIRD_PARTY_LICENSES_GENERATED.md
```

Note: This file is a summary and does not replace the authoritative license
texts. Always consult the upstream project repos and package metadata for the
official license terms.
