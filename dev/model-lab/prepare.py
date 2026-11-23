#!/usr/bin/env python3
"""Prepare one pinned tokenizer and one adapter dataset without loading a model."""
import argparse
from collections import Counter
import hashlib
import json
import math
from pathlib import Path
import tempfile
import urllib.request

from assemble import MODES, SPLITS, encode, read_split, verify_payloads

ROOT = Path(__file__).resolve().parent
PILOTS = ('phone-pilot', 'mac-pilot')


def tokenizer_files(model, cache):
    """Fetch only digest-locked tokenizer metadata. The two pilot cards use Qwen chat."""
    cache = cache / model['revision']
    cache.mkdir(parents=True, exist_ok=True)
    base = f"https://huggingface.co/{model['id']}/resolve/{model['revision']}"
    for name in ('tokenizer.json', 'tokenizer_config.json'):
        file = cache / name
        if not file.exists():
            with urllib.request.urlopen(f'{base}/{name}', timeout=60) as response:
                data = response.read()
            if hashlib.sha256(data).hexdigest() != model['tokenizer_sha256'][name]:
                raise ValueError(f'upstream tokenizer checksum mismatch: {name}')
            with tempfile.NamedTemporaryFile(dir=cache, delete=False) as tmp:
                tmp.write(data)
            Path(tmp.name).replace(file)
        if hashlib.sha256(file.read_bytes()).hexdigest() != model['tokenizer_sha256'][name]:
            raise ValueError(f'cached tokenizer checksum mismatch: {file}')
    return cache


def chat_encoder(path):
    from jinja2.sandbox import ImmutableSandboxedEnvironment
    from tokenizers import Tokenizer

    tokenizer = Tokenizer.from_file(str(path / 'tokenizer.json'))
    config = json.loads((path / 'tokenizer_config.json').read_text(encoding='utf-8'))
    template = ImmutableSandboxedEnvironment(trim_blocks=True, lstrip_blocks=True).from_string(config['chat_template'])

    def encode_chat(messages, generate=False):
        text = template.render(messages=messages, tools=None, add_generation_prompt=generate)
        return tokenizer.encode(text, add_special_tokens=False).ids

    return encode_chat


def tokenize(row, encode_chat):
    chat = row['messages']
    full = encode_chat(chat)
    prefix = encode_chat(chat[:-1], generate=True)
    if full[:len(prefix)] != prefix or len(prefix) >= len(full):
        raise ValueError(f"assistant boundary changed in {row['id']}")
    return full, len(prefix)


def write_split(source, dest, split, adapter, context, encode_chat, with_references):
    counts = Counter(records=0, processed_tokens=0, assistant_tokens=0)
    sizes, skipped = [], []
    # Both backends consume the same bounded examples. Keep held-out answers
    # in their labeled files; prompt-only inputs contain no final message.
    path = dest / f'{split}.jsonl'
    with path.open('w', encoding='utf-8', newline='\n') as output, (dest / f'{split}-ids.jsonl').open('w', encoding='utf-8', newline='\n') as ids:
        for row in read_split(source / f'{split}.jsonl.gz'):
            if MODES[row['mode']] != adapter or (row['mode'] == 'shader-reference' and not with_references):
                continue
            ids_full, offset = tokenize(row, encode_chat)
            if len(ids_full) > context:
                skipped.append({'id': row['id'], 'tokens': len(ids_full)})
                continue
            counts.update(records=1, processed_tokens=len(ids_full), assistant_tokens=len(ids_full)-offset)
            sizes.append(len(ids_full))
            output.write(encode({'messages': row['messages']})+'\n')
            ids.write(encode({'id': row['id'], 'family': row['family'], 'mode': row['mode']})+'\n')
    if not sizes:
        raise ValueError(f'{adapter} has no usable {split} rows. Check data scope or context.')
    return {**counts, 'max_tokens': max(sizes), 'p95_tokens': sorted(sizes)[math.ceil(.95*len(sizes))-1],
            'over_context': skipped, 'jsonl_sha256': hashlib.sha256(path.read_bytes()).hexdigest()}


def prepare(source, dest, model, adapter, context, cache, with_references=False):
    if dest.exists():
        raise ValueError('output directory already exists')
    if context < 64:
        raise ValueError('context must be at least 64 tokens')
    encode_chat = chat_encoder(tokenizer_files(model, cache))
    manifest = json.loads((source / 'manifest.json').read_text())
    verify_payloads(source, manifest)
    dest.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=dest.parent) as temp:
        build = Path(temp) / 'adapter'
        build.mkdir()
        report = {'base': model['id'], 'revision': model['revision'], 'scope': manifest['scope'], 'adapter': adapter,
                  'context': context, 'include_reference_shaders': with_references, 'splits': {}}
        for split in SPLITS:
            report['splits'][split] = write_split(source, build, split, adapter, context, encode_chat, with_references)
        (build/'recipe.json').write_text(json.dumps(report, indent=2, sort_keys=True)+'\n')
        # MLX command line defaults to dry load. The operator enables training
        # explicitly after converting the exact model snapshot to MLX 4-bit.
        mlx = {'train': False, 'seed': 17, 'fine_tune_type': 'lora', 'num_layers': 16,
               'learning_rate': 1e-4, 'batch_size': 1, 'grad_accumulation_steps': 1, 'grad_checkpoint': True,
               'iters': report['splits']['train']['records'], 'steps_per_report': 8, 'steps_per_eval': 32,
               'val_batches': report['splits']['valid']['records'], 'max_seq_length': context,
               'mask_prompt': True, 'lora_parameters': {'rank': 8, 'scale': 16.0, 'dropout': 0.0}}
        # JSON is also valid YAML for mlx_lm.lora --config.
        (build/'mlx-config.json').write_text(json.dumps(mlx, indent=2)+'\n')
        build.rename(dest)
    return report


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('dataset', type=Path)
    cli.add_argument('output', type=Path)
    cli.add_argument('--pilot', choices=PILOTS, required=True)
    cli.add_argument('--adapter', choices=('show', 'shader', 'code'), required=True)
    cli.add_argument('--context', type=int, default=4096)
    cli.add_argument('--tokenizer-cache', type=Path, required=True)
    cli.add_argument('--with-references', action='store_true', help='Also use Three.js chunk reconstruction tasks for the shader adapter')
    args = cli.parse_args()
    models = json.loads((ROOT/'models.json').read_text())['models']
    model = next(m for m in models if m['role'] == args.pilot)
    report = prepare(args.dataset, args.output, model, args.adapter, args.context, args.tokenizer_cache, args.with_references)
    for split, metrics in report['splits'].items():
        print(split, metrics['records'], 'records;', metrics['processed_tokens'], 'tokens;',
              metrics['assistant_tokens'], 'supervised;', len(metrics['over_context']), 'long rows held out')


if __name__ == '__main__':
    main()
