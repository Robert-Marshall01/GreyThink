# Flan-T5 Chatbot

A minimal CLI chatbot using `google/flan-t5-base` (Flan T5 family) via Hugging Face Transformers.

## Setup

Create a virtualenv and install dependencies:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

Note: If you have a CUDA GPU and a recent PyTorch build, the script will use `float16` on CUDA to reduce memory.

## Usage

- Interactive chat:

```bash
python python_ai_2.py
```

- Run a quick sample (loads the model, runs one prompt, and exits):

```bash
python python_ai_2.py --run-sample
```

- Change model or generation options:

```bash
python python_ai_2.py --model google/flan-t5-base --max-length 128 --temperature 0.7
```

## Notes

- The first run will download the model weights from Hugging Face.
- For multi-turn chat, the script keeps the last few exchanges as context.

**Weights are not included in this repository.** To download the model weights at runtime, the code uses `transformers.from_pretrained("google/flan-t5-base")`, which will fetch weights automatically from Hugging Face when first run.

## Disclaimer

This program and much of the code and content in this repository were generated with the assistance of an AI model. It is intended for experimentation and leisure use only and is not designed, tested, or certified for professional, safety-critical, medical, legal, financial, or other high-stakes applications. Do not rely on it for professional advice or critical decisions.

## Licensing and citation

This project uses the Hugging Face model `google/flan-t5-base`, which is distributed under the Apache License, Version 2.0. To comply with that license when distributing this project or derivatives that include the model, include the Apache 2.0 license text and preserve the NOTICE file.

- License text: `models/COPYING.APACHE2.txt`
- Model NOTICE: `models/google-flan-t5-base.NOTICE.txt`
- Suggested citation/acknowledgement: `CITATION.md`

Please consult the model page for the latest authorship and citation details: [google/flan-t5-base](https://huggingface.co/google/flan-t5-base)

## Troubleshooting

If `python -m venv .venv` fails with an error like `Operation not permitted: 'lib' -> '/path/to/.venv/lib64'`, your project folder may be on a filesystem that disallows symlinks (common for external drives like exFAT). Two options:

- Create the virtual environment in your home directory (recommended):

```bash
mkdir -p ~/.venvs
python3 -m venv ~/.venvs/python_ai_2
. ~/.venvs/python_ai_2/bin/activate
pip install -r requirements.txt
```

- Or install dependencies into your user site-packages (no venv):

```bash
python3 -m pip install --user -r requirements.txt
python3 python_ai_2.py --run-sample
```

Either approach avoids the symlink restriction on some mounted filesystems.
