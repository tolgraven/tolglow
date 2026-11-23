#!/usr/bin/env python3
"""Verify a prepared adapter locally. --train opts in to loading weights on one CUDA device."""
import argparse
import hashlib
import json
from pathlib import Path
from prepare import ROOT, chat_encoder, tokenizer_files, tokenize


def inspect(directory):
    recipe = json.loads((directory / "recipe.json").read_text())
    models = json.loads((ROOT / "models.json").read_text())["models"]
    model = next((m for m in models if m["id"] == recipe["base"] and m["revision"] == recipe["revision"]
                  and m["role"] in {"mac-pilot", "phone-pilot"}), None)
    if not model:
        raise ValueError("Unsupported model revision")
    for split, stats in recipe["splits"].items():
        payload = (directory / f"{split}.jsonl").read_bytes()
        if hashlib.sha256(payload).hexdigest() != stats["jsonl_sha256"]:
            raise ValueError(f"Prepared split changed: {split}")
    return recipe, model


def labeled_examples(directory, split, encoder):
    rows = []
    for i, line in enumerate((directory / f"{split}.jsonl").read_text().splitlines()):
        ids, start = tokenize({"id": i, **json.loads(line)}, encoder)
        rows.append({"input_ids": ids, "attention_mask": [1] * len(ids),
                     "labels": [-100] * start + ids[start:]})
    return rows


def train(directory, destination, cache, recipe, model):
    import torch
    from peft import LoraConfig, get_peft_model, prepare_model_for_kbit_training
    from transformers import (AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig,
                              DataCollatorForSeq2Seq, Trainer, TrainingArguments)

    if destination.exists():
        raise ValueError("Choose a new adapter destination")
    if not torch.cuda.is_available() or not torch.cuda.is_bf16_supported():
        raise ValueError("This recipe needs a BF16-capable CUDA device")
    encoder = chat_encoder(tokenizer_files(model, cache))
    train_rows = labeled_examples(directory, "train", encoder)
    validation = labeled_examples(directory, "valid", encoder)
    base = {"pretrained_model_name_or_path": model["id"], "revision": model["revision"], "trust_remote_code": False}
    tokenizer = AutoTokenizer.from_pretrained(**base)
    if tokenizer.pad_token_id is None:
        tokenizer.pad_token = tokenizer.eos_token
    quant = BitsAndBytesConfig(load_in_4bit=True, bnb_4bit_quant_type="nf4",
                               bnb_4bit_use_double_quant=True, bnb_4bit_compute_dtype=torch.bfloat16)
    network = AutoModelForCausalLM.from_pretrained(**base, device_map={"": 0}, quantization_config=quant)
    network.config.use_cache = False
    network = prepare_model_for_kbit_training(network, use_gradient_checkpointing=True)
    layers = network.config.num_hidden_layers
    lora = LoraConfig(r=8, lora_alpha=16, lora_dropout=0.0, target_modules=["q_proj", "v_proj"],
                      layers_to_transform=list(range(max(0, layers-16), layers)), layers_pattern="layers",
                      task_type="CAUSAL_LM")
    network = get_peft_model(network, lora)
    options = TrainingArguments(output_dir=str(destination), seed=17, data_seed=17, learning_rate=1e-4,
                                num_train_epochs=1, per_device_train_batch_size=1, per_device_eval_batch_size=1,
                                gradient_checkpointing=True, bf16=True, eval_strategy="epoch",
                                save_strategy="epoch", logging_steps=8, report_to=[], push_to_hub=False)
    trainer = Trainer(model=network, args=options, train_dataset=train_rows, eval_dataset=validation,
                      data_collator=DataCollatorForSeq2Seq(tokenizer, pad_to_multiple_of=8, label_pad_token_id=-100))
    trainer.train()
    network.save_pretrained(destination)
    (destination / "training-recipe.json").write_text(json.dumps(recipe, indent=2) + "\n")


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("prepared", type=Path)
    cli.add_argument("--train", action="store_true")
    cli.add_argument("--output", type=Path)
    cli.add_argument("--tokenizer-cache", type=Path)
    args = cli.parse_args()
    recipe, model = inspect(args.prepared)
    print(recipe["adapter"], model["id"], model["revision"], recipe["splits"]["train"]["processed_tokens"], "training tokens")
    if args.train:
        if args.output is None or args.tokenizer_cache is None:
            cli.error("--train requires --output and --tokenizer-cache")
        train(args.prepared, args.output, args.tokenizer_cache, recipe, model)


if __name__ == "__main__":
    main()
