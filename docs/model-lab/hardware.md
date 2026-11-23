# Budgeting a local model run

On an owned 48 GB M4 Pro MacBook Pro, allow **3.35–13.56 hours and $0.03–$0.31 of electricity** for a pair of Qwen2.5-Coder-Instruct adapters, one 1.5B and one 7B. The same pair on a single rented 80 GB H100 SXM works out to **0.51–1.22 hours and $2.53–$6.03**. These are planning scenarios for one million processed training tokens per model. Count the tokenized corpus and substitute measured throughput before booking time. A held-out quality comparison determines whether the longer run buys a better lighting assistant. [Qwen 1.5B](https://huggingface.co/Qwen/Qwen2.5-Coder-1.5B-Instruct) · [Qwen 7B](https://huggingface.co/Qwen/Qwen2.5-Coder-7B-Instruct)

## Where the numbers come from

Two published training measurements provide scale. The MLX team reports about 475 tokens/s tuning Llama 7B on WikiSQL on an M2 Ultra. Axolotl reports 9,104 tokens/s for its H100 Llama 3.1 8B QLoRA sequence-parallel-degree-one configuration at a 17,408-token context. Both use different data, models, sequence lengths and configurations from our pilot. We have no matching local training measurements for the machines below. The low/high rates are explicit sensitivity assumptions for short, small-batch examples. They span poor packing and more efficient sustained execution. The endpoints are illustrative and actual throughput may fall outside them. [MLX training result](https://github.com/ml-explore/mlx-examples/tree/main/lora#results) · [Axolotl workload and methodology](https://huggingface.co/blog/axolotl-ai-co/long-context-with-sequence-parallelism-in-axolotl#llama-31-8b-qlora-finetune)

The table assumes a frozen 4-bit base with low-rank adapters, 512–1,024 token examples, microbatch 1–2, one million processed tokens and one epoch. Time includes ten minutes for startup/export and 20% additional time for evaluation and checkpoints for each model. Count prompt and response tokens after template application, truncation and packing. A response-only loss still computes the prompt. Match the token convention in your timed log, including padding if the logger counts padded positions. Bandwidth specifications and inference decode rates cannot predict backward-pass time.

| Device and billable allocation | 1.5B assumed train tok/s | 1.5B hours | 1.5B USD | 7B assumed train tok/s | 7B hours | 7B USD |
|---|---:|---:|---:|---:|---:|---:|
| Owned M4 Pro, 48 GB, 35–90 W | 90–420 | 0.96–3.87 | 0.008–0.087 | 35–150 | 2.39–9.69 | 0.021–0.218 |
| Owned M3 Ultra Studio, 96+ GB, 90–260 W | 300–1,400 | 0.41–1.28 | 0.009–0.083 | 150–600 | 0.72–2.39 | 0.016–0.155 |
| Lambda, 1 × A100 SXM 40 GB, $1.99/h | 800–4,000 | 0.25–0.58 | 0.50–1.16 | 250–1,200 | 0.44–1.50 | 0.90–2.99 |
| Lambda, 1 × H100 SXM 80 GB, $4.89/h | 1,500–7,500 | 0.21–0.39 | 1.06–1.96 | 500–2,500 | 0.30–0.83 | 1.47–4.08 |
| CoreWeave, 4 × GB200, 186 GB each, $42/node-hour | 2,000–11,000 | 0.20–0.33 | 8.27–14.00 | 800–4,000 | 0.25–0.58 | 10.50–24.50 |

Rates checked **2026-11-23** against published on-demand SKU tables. Lambda has single-GPU instances at both quoted rates, billed in one-minute increments from healthy launch through termination. CoreWeave's North American GB200 NVL72 instance allocates two superchips containing four GPUs in total. The table charges the whole $42/h instance even when a single training process uses one GPU. The published $10.50 single-GPU figure applies to its inference platform. Check availability, provisioning delays, billing minimums and contract terms at booking. The GB200 estimates prorate the published whole-node price by elapsed time. If the agreement has a one-hour minimum, each standalone run under one hour costs at least $42. Multiple simultaneous *independent* adapter trials could share that allocation. The estimate assumes one active worker and makes no multi-GPU speedup assumption. Taxes, persistent storage and transfer have separate costs. [Lambda current rates](https://lambda.com/pricing) · [Lambda allocations](https://docs.lambda.ai/on-demand-cloud/overview/) · [Lambda billing](https://docs.lambda.com/public-cloud/billing/) · [CoreWeave GPU count and whole-instance rates](https://www.coreweave.com/pricing)

Owned-machine costs cover wall electricity at an **assumed $0.25/kWh**. For the 7B run this gives 0.084–0.872 kWh on the MacBook and 0.065–0.621 kWh on the Studio. Replace assumed watts with a wall meter reading during a comparable workload and use the marginal price on your electricity bill. Memory pressure, other running apps, ambient temperature and thermal throttling affect both time and power. Hardware purchase, battery wear and staff time are separate budget items.

## What fits on 48 GB and 8 GB

The 7B model has 7.61B parameters. At four bits its base weights alone have an ideal 3.81 GB payload in decimal units. The 14B model has 14.7B parameters and a corresponding 7.35 GB ideal base. Quantization scales, higher-precision tensors, adapters, gradients, optimizer state, activations, allocator caches, the OS and Metal buffers add to the peak. A conventional full mixed-precision Adam run using 16 bytes per parameter would consume about 122 GB for the 7B model before activations. For 48 GB, start the 7B adapter at microbatch one with a 512-token context, then compare peak memory and swap at 1,024 tokens. Evaluate 14B for quality at short context first. If you later tune it, the 15–70 training tok/s scenario implies **4.93–22.39 hours** and **$0.04–$0.50 per million tokens** under the same overhead assumptions. Require measured working-set headroom with your show applications open before sharing their machine. A 48 GB MacBook Pro may have an M4 Pro or M4 Max chip. Record its actual chip/GPU core count in results. The table assumes M4 Pro. M3 Ultra Studio configurations start at 96 GB. [Qwen 7B parameter count](https://huggingface.co/Qwen/Qwen2.5-Coder-7B-Instruct) · [Qwen 14B parameter count](https://huggingface.co/Qwen/Qwen2.5-Coder-14B-Instruct) · [MacBook Pro specifications](https://support.apple.com/en-us/121553) · [Mac Studio specifications](https://support.apple.com/en-us/122211)

An 8 GB iPhone is an inference target for the exported 1.5B model with a 0.5B fallback under thermal or memory pressure. The Mac/Linux training recipes need a host trainer. Apple's adapter toolkit for its separate Foundation Models API also requires a Mac with at least 32 GB or a Linux GPU. An iOS process can have a lower memory limit than installed RAM. Measure peak resident memory, headroom, thermal state, prompt prefill and steady decode on the actual phone. For 100,000 **output** tokens, the 1.5B planning scenario of 5–25 decode tok/s takes 1.11–5.56 hours of active generation. At an assumed 3–8 W that uses 0.003–0.044 kWh. The 0.5B scenario of 10–40 tok/s takes 0.69–2.78 hours and uses 0.002–0.022 kWh. These figures exclude prompt prefill, idle time between requests and time to train the adapter on the host. [Apple adapter toolkit requirements](https://developer.apple.com/apple-intelligence/foundation-models-adapter) · [App memory headroom](https://developer.apple.com/documentation/os/os_proc_available_memory) · [iOS memory pressure](https://developer.apple.com/documentation/xcode/identifying-high-memory-use-with-jetsam-event-reports)

Qwen3.5 challengers need their own timings at the selected checkpoint, context length, precision and backend. A new tokenizer can change the sequence count that survives truncation. Re-tokenize the same split, hold the evaluation prompts fixed and calibrate the observed token rate before comparing dollar cost per passing evaluation. Pass the checkpoint name to `--size` with its own `--rates LOW HIGH` when measured. [Qwen3.5 family](https://huggingface.co/collections/Qwen/qwen35)

## Recalculate offline

From the repository root, the Python standard-library calculator accepts the actual processed tokens per epoch. It multiplies by epochs, divides by measured throughput, adds proportional evaluation time and fixed setup time, then charges the entire allocation. Its default overhead and setup are zero. The examples below supply this table’s assumptions explicitly.

```sh
python3 dev/model-lab/budget.py estimate --hardware m4-pro-48gb --size 7B \
  --tokens 1000000 --epochs 1 --setup-minutes 10 --overhead 0.20 --usd-kwh 0.25
python3 dev/model-lab/budget.py estimate --hardware h100-80gb --size 1.5B \
  --tokens 250000 --epochs 2 --setup-minutes 10 --overhead 0.20 --instance-usd-h 4.89
python3 dev/model-lab/budget.py estimate --hardware iphone-8gb --size 0.5B --tokens 100000
```

To calibrate **without training**, reuse non-overlapping windows from an existing comparable run. Subtract successive cumulative token and elapsed-wall-time counters after warmup, preserving stalls. Aggregate over wall seconds across the whole allocation and use summed token deltas across workers. Example windows of 48,000 tokens in 160 s and 72,000 in 300 s give 260.87 tok/s. Replace the example with real slow/fast replicate ranges using `--rates LOW HIGH`. Record model revision, chip SKU, context, packing, adapter rank, batch size, accumulation, runtime revision, watts and peak memory with the log. When only an inference trace exists, calibrate the phone decode row from that trace and leave the training rows provisional.

```sh
python3 dev/model-lab/budget.py calibrate --window 48000 160 --window 72000 300
python3 dev/model-lab/budget.py estimate --hardware m3-ultra --size 7B \
  --tokens 1000000 --rates 240 280 --setup-minutes 10 --overhead 0.20 --watts 110 190
python3 -m unittest discover -s dev/model-lab -p test_budget.py
```
