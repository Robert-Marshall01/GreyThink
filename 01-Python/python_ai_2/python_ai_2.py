#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
"""Simple CLI chatbot using google/flan-t5-base.

Usage:
  python python_ai_2.py         # interactive chat
  python python_ai_2.py --run-sample
  python python_ai_2.py --model google/flan-t5-base --max-length 128
"""

import argparse
import time
from typing import List

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    import torch
    from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
else:
    try:
        import torch  # type: ignore
        from transformers import AutoModelForSeq2SeqLM, AutoTokenizer  # type: ignore
    except ImportError:  # pragma: no cover
        torch = None  # type: ignore
        AutoModelForSeq2SeqLM = None  # type: ignore
        AutoTokenizer = None  # type: ignore


class FlanT5Chatbot:
    """Flan-T5 based chatbot wrapper.

    Provides prompt construction, generation, and lightweight conversation history management.
    """
    def __init__(
        self,
        model_name: str = "google/flan-t5-base",
        device: str | None = None,
        max_length: int = 256,
    ):
        self.model_name = model_name
        if device is not None:
            self.device = device
        else:
            self.device = "cuda" if torch and torch.cuda.is_available() else "cpu"
        self.max_length = max_length

        print(f"Loading tokenizer for {model_name}...")
        self.tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)

        print(f"Loading model {model_name} on {self.device}...")
        # Use fp16 on CUDA to save memory if available
        from_pretrained_kwargs = {}
        if str(self.device).startswith("cuda") and torch is not None:
            from_pretrained_kwargs["torch_dtype"] = torch.float16
        self.model = AutoModelForSeq2SeqLM.from_pretrained(model_name, **from_pretrained_kwargs)
        self.model.to(self.device)
        self.model.eval()

        self.history: List[tuple[str, str]] = []  # list of (user, assistant)

    def build_prompt(self, user_input: str) -> str:
        """Construct the model prompt from instruction, recent history, and the user's question."""
        # Use a concise instruction + Q/A format (avoids 'User:'/'Assistant:' labels)
        instruction = (
            "You are a helpful, concise, and factual assistant." 
            " Answer the question directly and accurately." 
            " If you don't know, say you don't know." 
            " For 'what is' questions, give a one- to two-sentence definition." 
            " For 'benefits' or 'advantages' questions, list 2-3 concise points."
        )
        # do not include canned examples; rely on the instruction and recent history
        parts = [instruction]
        for u, a in self.history[-4:]:  # keep last 4 exchanges
            parts.append(f"Previous Question: {u}")
            parts.append(f"Previous Answer: {a}")
        parts.append(f"Question: {user_input}")
        parts.append("Answer:")
        return "\n".join(parts)

    def _clean_reply(self, text: str) -> str:
        """Post-process model output, strip labels and normalize whitespace."""
        # Remove prompt labels and stray prefixes
        # If the model returned the full prompt + assistant answer, extract last assistant block
        if "Assistant:" in text:
            text = text.rsplit("Assistant:", 1)[-1]
        # Remove any remaining 'User:' or 'Assistant:' labels
        text = text.replace('User:', '').replace('Assistant:', '')
        # Strip and normalize whitespace
        reply = "\n".join([line.strip() for line in text.strip().splitlines() if line.strip()])
        # Avoid returning just a user's name or an empty reply
        if not reply or len(reply) < 2:
            return "I don't have enough information to answer that."
        return reply.strip()

    def generate(self, user_input: str, temperature: float = 0.0, top_p: float = 0.95, num_beams: int = 4) -> str:
        """Generate a response for the given `user_input` using the loaded model.

        Uses beam search by default (low temperature) and sampling when `temperature` > 0.
        A short reply triggers a single re-generation attempt with stronger beam settings.
        """
        prompt = self.build_prompt(user_input)
        inputs = self.tokenizer(
            prompt,
            return_tensors="pt",
            truncation=True,
            max_length=1024,
        ).to(self.device)
        with torch.no_grad():
            gen_kwargs = {
                "max_new_tokens": self.max_length,
                "eos_token_id": self.tokenizer.eos_token_id,
                "early_stopping": True,
            }
            if temperature and temperature > 0.0:
                gen_kwargs.update({"temperature": temperature, "do_sample": True, "top_p": top_p})
            else:
                gen_kwargs.update({"num_beams": num_beams, "do_sample": False})

            outputs = self.model.generate(
                **inputs,
                **gen_kwargs,
            )
        text = self.tokenizer.decode(outputs[0], skip_special_tokens=True)
        reply = self._clean_reply(text)
        # If the reply is too short or looks like a single token, try one stronger beam re-generation
        if reply and (len(reply) < 20 or reply.count(' ') < 3):
            extended_prompt = (
                prompt + "\nPlease answer in one or two clear sentences. "
                "Provide a factual explanation when applicable."
            )
            inputs2 = self.tokenizer(
                extended_prompt,
                return_tensors="pt",
                truncation=True,
                max_length=1024,
            ).to(self.device)
            with torch.no_grad():
                outputs2 = self.model.generate(
                    **inputs2,
                    max_new_tokens=min(self.max_length * 2, 512),
                    num_beams=8,
                    early_stopping=True,
                    eos_token_id=self.tokenizer.eos_token_id,
                    do_sample=False,
                )
            text2 = self.tokenizer.decode(outputs2[0], skip_special_tokens=True)
            reply2 = self._clean_reply(text2)
            # prefer the longer, more informative reply
            if len(reply2) > len(reply):
                reply = reply2
        # Save to history
        self.history.append((user_input, reply))
        return reply


def interactive_loop(bot: FlanT5Chatbot, args: argparse.Namespace) -> None:
    """Run a simple readline-style interactive loop with the chatbot."""
    print("Starting interactive chat (type 'exit' or Ctrl-C to quit)")
    try:
        while True:
            user = input("You: ")
            if not user:
                continue
            if user.strip().lower() in {"exit", "quit"}:
                print("Goodbye")
                break
            start = time.time()
            reply = bot.generate(user, temperature=args.temperature, top_p=args.top_p)
            elapsed = time.time() - start
            print(f"Assistant ({elapsed:.2f}s): {reply}\n")
    except (KeyboardInterrupt, EOFError):
        print("\nGoodbye")


def main():
    """CLI entrypoint: parse args, create the chatbot, and run sample or interactive loop."""
    parser = argparse.ArgumentParser(description="Flan-T5 Chatbot (google/flan-t5-base)")
    parser.add_argument("--model", default="google/flan-t5-base", help="HF model name")
    parser.add_argument("--device", default=None, help="device to use (auto by default)")
    parser.add_argument("--max-length", type=int, default=128, help="max new tokens to generate")
    parser.add_argument("--temperature", type=float, default=0.0)
    parser.add_argument("--top-p", type=float, default=0.95)
    parser.add_argument("--run-sample", action="store_true", help="Load model and run a sample query then exit")
    args = parser.parse_args()

    bot = FlanT5Chatbot(model_name=args.model, device=args.device, max_length=args.max_length)

    if args.run_sample:
        print("Running sample queries...")
        samples = [
            "Hello! Can you briefly explain what transformers are?",
            "Who are you?",
            "What is an electrician?",
            "Who is Jake Paul?",
            "What is communism?",
            "What is socialism?",
            "What is capitalism?",
            "What are the benefits of Firefox over Chrome?",
            "What are the benefits of Linux over Windows?",
        ]
        for q in samples:
            print(f"Q: {q}")
            ans = bot.generate(q, temperature=args.temperature, top_p=args.top_p)
            print(f"A: {ans}\n")
        return

    interactive_loop(bot, args)


if __name__ == "__main__":
    main()
