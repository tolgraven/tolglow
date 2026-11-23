#!/usr/bin/env python3
"""Offline time and cost estimates for the Tolglow model lab."""

import argparse
from dataclasses import dataclass
import json
import math


@dataclass(frozen=True)
class Hardware:
    task: str
    rates: dict  # Model size -> planning range in tokens per wall second.
    watts: tuple = (0, 0)  # Assumed whole-device wall power for owned equipment.
    instance_usd_h: float = 0
    billing_increment_s: int = 0


HARDWARE = {
    "iphone-8gb": Hardware("decode", {"0.5B": (10, 40), "1.5B": (5, 25)}, (3, 8)),
    "m4-pro-48gb": Hardware("train", {"1.5B": (90, 420), "7B": (35, 150),
                                      "14B": (15, 70)}, (35, 90)),
    "m3-ultra": Hardware("train", {"1.5B": (300, 1400), "7B": (150, 600),
                                  "14B": (65, 300)}, (90, 260)),
    "a100-40gb": Hardware("train", {"1.5B": (800, 4000), "7B": (250, 1200)},
                           instance_usd_h=1.99, billing_increment_s=60),
    "h100-80gb": Hardware("train", {"1.5B": (1500, 7500), "7B": (500, 2500)},
                           instance_usd_h=4.89, billing_increment_s=60),
    "gb200-node": Hardware("train", {"1.5B": (2000, 11000), "7B": (800, 4000)},
                           instance_usd_h=42),
}


def number(value, name):
    value = float(value)
    if not math.isfinite(value) or value < 0:
        raise ValueError(f"{name} must be finite and nonnegative")
    return value


def positive(value, name):
    value = number(value, name)
    if value == 0:
        raise ValueError(f"{name} must be positive")
    return value


def count(value, name):
    n = int(value)
    if n <= 0 or (not isinstance(value, str) and n != value):
        raise ValueError(f"{name} must be a positive integer")
    return n


def calibrated_rate(windows):
    """Pool non-overlapping windows by elapsed wall time, including slow steps."""
    if not windows:
        raise ValueError("provide at least one timed window")
    tokens = sum(count(t, "window tokens") for t, _ in windows)
    seconds = sum(positive(s, "window seconds") for _, s in windows)
    return tokens / seconds


def estimate(hardware, size, tokens, epochs=1, *, rates=None, setup_minutes=0,
             overhead=0, usd_kwh=0.25, watts=None, instance_usd_h=None):
    if rates is None and size not in hardware.rates:
        raise ValueError(f"available sizes for this {hardware.task} scenario: {', '.join(hardware.rates)}")
    epochs = count(epochs, "epochs")
    if hardware.task == "decode" and epochs != 1:
        raise ValueError("epochs apply to training only")
    processed = count(tokens, "tokens") * epochs
    low, high = (positive(v, "throughput") for v in (rates if rates is not None else hardware.rates[size]))
    if low > high:
        raise ValueError("minimum throughput must not exceed maximum")
    fixed = number(setup_minutes, "setup minutes") * 60
    factor = 1 + number(overhead, "overhead fraction")
    elapsed = tuple(fixed + (processed / rate) * factor for rate in (high, low))
    hourly = number(hardware.instance_usd_h if instance_usd_h is None else instance_usd_h, "instance price")
    watt_range = tuple(number(w, "watts") for w in (hardware.watts if watts is None else watts))
    if watt_range[0] > watt_range[1]:
        raise ValueError("minimum watts must not exceed maximum")
    energy = tuple(seconds / 3600 * w / 1000 for seconds, w in zip(elapsed, watt_range))
    tariff = number(usd_kwh, "electricity price")
    quantum = hardware.billing_increment_s
    billable = (math.ceil(s / quantum) * quantum if quantum else s for s in elapsed)
    rent = tuple(s * hourly / 3600 for s in billable)
    return {"task": hardware.task, "processed_tokens": processed,
            "rate_source": "supplied" if rates is not None else "planning scenario",
            "tokens_per_second": [low, high], "elapsed_hours": [s / 3600 for s in elapsed],
            "whole_instance_usd": list(rent), "local_kwh": list(energy),
            "local_electricity_usd": [kwh * tariff for kwh in energy]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    cal = commands.add_parser("calibrate", help="combine token and wall-time deltas from existing logs")
    cal.add_argument("--window", nargs=2, action="append", metavar=("TOKENS", "SECONDS"), required=True)
    plan = commands.add_parser("estimate", help="calculate a range for a selected model and whole device")
    plan.add_argument("--hardware", choices=HARDWARE, required=True)
    plan.add_argument("--size", required=True, help="model size or custom checkpoint name with --rates")
    plan.add_argument("--tokens", type=int, required=True, help="processed tokens per epoch, or decoded output tokens")
    plan.add_argument("--epochs", type=int, default=1)
    plan.add_argument("--rates", nargs=2, type=float, metavar=("MIN", "MAX"))
    plan.add_argument("--setup-minutes", type=float, default=0)
    plan.add_argument("--overhead", type=float, default=0, help="additional fraction for evaluation and checkpoints")
    plan.add_argument("--watts", nargs=2, type=float, metavar=("MIN", "MAX"))
    plan.add_argument("--usd-kwh", type=float, default=0.25)
    plan.add_argument("--instance-usd-h", type=float, help="price of the entire allocated instance per hour")
    args = vars(parser.parse_args())
    try:
        if args.pop("command") == "calibrate":
            result = {"tokens_per_second": calibrated_rate(args["window"])}
        else:
            device = args.pop("hardware")
            result = {"hardware": device, **estimate(HARDWARE[device], **args)}
    except ValueError as error:
        parser.error(str(error))
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
