# Model files (models/)

This directory contains license and NOTICE files related to the `google/flan-t5-base`
model referenced by this project. It intentionally does NOT include model weight
files. Do not commit model weights to this repository.

To download weights at runtime using Hugging Face Transformers:

```python
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
model = AutoModelForSeq2SeqLM.from_pretrained("google/flan-t5-base")
tokenizer = AutoTokenizer.from_pretrained("google/flan-t5-base")
```

If you distribute model weights alongside this code, ensure you also include:

- `COPYING.APACHE2.txt` (Apache 2.0 license)
- `google-flan-t5-base.NOTICE.txt` (model NOTICE / attribution)
