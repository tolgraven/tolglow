#!/usr/bin/env python3
"""Measure packet error caused by candidate storage formats, independent of GPU speed."""
import bisect
import json
import math
import struct
from collections import Counter
from itertools import product


def integer_code(value, maximum):
    return math.floor(max(0.0, min(1.0, value)) * maximum + 0.5)


def unorm(value, maximum):
    return integer_code(value, maximum) / maximum


def half(value):
    return struct.unpack('<e', struct.pack('<e', value))[0]


def decode_e4m3(bits):
    exponent, mantissa = bits >> 3, bits & 7
    if exponent == 0:
        return math.ldexp(mantissa, -9)
    return math.ldexp(1 + mantissa / 8, exponent - 7)


# Only the nonnegative finite range up to one is used in this normalized probe.
FP8_VALUES = tuple(decode_e4m3(bits) for bits in range(0x39))


def fp8(value):
    high = bisect.bisect_left(FP8_VALUES, value)
    if high == 0 or FP8_VALUES[high] == value:
        return FP8_VALUES[high]
    low = high - 1
    below, above = value - FP8_VALUES[low], FP8_VALUES[high] - value
    index = low if below < above or (below == above and low % 2 == 0) else high
    return FP8_VALUES[index]


def error_summary(expected, actual):
    counts = Counter(abs(before - after) for before, after in zip(expected, actual, strict=True))
    total = sum(counts.values())
    return {'samples': total, 'changed': total - counts[0],
            'max_error_codes': max(counts), 'distinct_output_codes': len(set(actual))}


def exact_code_round_trip(store, maximum):
    samples = range(maximum + 1)
    actual = [integer_code(store(code / maximum), maximum) for code in samples]
    return error_summary(samples, actual)


def composed_output(store):
    values = range(0, 256, 4)
    alphas = (0.04, 0.29, 0.7, 0.98)
    expected, actual = [], []
    for current, effect, opacity in product(values, values, alphas):
        first = (current / 255) * (1 - opacity) + (effect / 255) * opacity
        # A second layer consumes the first blend, then the packet quantizes once.
        expected.append(integer_code(first * 0.61 + 0.1287, 255))
        actual.append(integer_code(store(first) * 0.61 + 0.1287, 255))
    return error_summary(expected, actual)


def main():
    stores = {'UNORM8': lambda value: unorm(value, 255),
              'UNORM16': lambda value: unorm(value, 65535),
              'FP16': half, 'FP8 E4M3': fp8}
    report = {name: {'8-bit wire round-trip': exact_code_round_trip(store, 255),
                     '16-bit wire round-trip': exact_code_round_trip(store, 65535),
                     '8-bit after intermediate store': composed_output(store)}
              for name, store in stores.items()}
    assert report['UNORM8']['8-bit wire round-trip']['changed'] == 0
    assert report['UNORM16']['16-bit wire round-trip']['changed'] == 0
    assert report['FP16']['8-bit wire round-trip']['changed'] == 0
    assert report['FP16']['8-bit after intermediate store']['changed'] > 0
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
