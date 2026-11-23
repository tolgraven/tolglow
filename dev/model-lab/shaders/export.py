"""Export the pinned GLSL sources as deterministic, whole-upstream chat splits."""
import argparse
import hashlib
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent
SYSTEM = ("Write a self-contained GLSL ES 3.00 fragment pass for a Tolglow studio "
          "render target. Keep native fixture and show values as inputs. "
          "Return only the complete shader, with source attribution.")
HEADER = "#version 300 es\nprecision highp float;\nin vec2 vUv;\nout vec4 result;"
INPUTS = {"worldPosition": ("vec3", [0.31, 0.73, 0.42]), "beat": ("float", 0.37),
          "emittedColor": ("vec3", [0.18, 0.49, 0.78]), "wet": ("float", 0.68),
          "originColor": ("vec3", [0.05, 0.13, 0.2]), "targetColor": ("vec3", [0.85, 0.73, 0.61]),
          "lightTexture": ("sampler2D", "seven-pixel isolated emitter, linear filtering"),
          "resolution": ("vec2", [7, 1]), "direction": ("vec2", [1, 0]),
          "extinctionPerM": ("float", 0.8), "mixedDensity": ("float", 0.16),
          "pathMetres": ("float", 6.0), "emissionGain": ("float", 1.4)}


def verified(root, path, expected):
    data = (root / Path(path).name).read_bytes()
    if hashlib.sha256(data).hexdigest() != expected:
        raise ValueError(f"Source checksum mismatch: {root.name}/{path}")
    return data.decode("utf-8")


def body(raw):
    # Strip only packaging, leaving upstream source and inline notices intact.
    if raw.startswith("export default"):
        raw = raw.split("`", 2)[1]
    raw = re.sub(r"^#version 120[ \t]*$", "", raw, flags=re.M)
    raw = re.sub(r"^#pragma glslify: export\(\w+\)\s*$", "", raw, flags=re.M)
    if "#pragma glslify:" in raw:
        raise ValueError("Unresolved glslify dependency")
    return raw.strip().replace("texture2D(", "texture(")


def tasks(family, path, raw):
    stem = Path(path).stem
    export = re.search(r"#pragma glslify: export\((\w+)\)", raw)
    fn = export[1] if export else None
    if family == "noise":
        n = int(re.search(r"(\d)D", stem)[1])
        point = {2: "worldPosition.xz + vec2(beat * 0.25, 0.0)",
                 3: "worldPosition + vec3(0.0, beat * 0.25, 0.0)",
                 4: "vec4(worldPosition, beat * 0.25)"}[n]
        if stem.startswith("cellular"):
            expression = f"vec2 distances = cellular({point});\n  float mode = smoothstep(0.0, 0.45, distances.y - distances.x);"
        else:
            sample = "cnoise" if stem.startswith("classic") else "snoise"
            expression = f"float mode = clamp(0.5 + 0.5 * {sample}({point}), 0.0, 1.0);"
        yield stem, f"Use {stem} over a fixture's world position and beat phase to modulate emitted linear RGB. Translate the field by a quarter unit per beat. Keep dry gain at one and mix toward the noise gain with wet. For cellular noise use the gap F2−F1 with smoothstep from 0 to 0.45.", ["worldPosition", "beat", "emittedColor", "wet"], expression + "\n  result = vec4(emittedColor * mix(1.0, mode, wet), 1.0);"
    elif family == "easing":
        yield stem, f"Use the {stem} easing curve on progress from beat 0 to beat 1 to interpolate between two linear RGB fixture keyframes. Preserve intentional curve overshoot until the output sink. Clamp only the input progress.", ["originColor", "targetColor", "beat"], f"float t = {fn}(clamp(beat, 0.0, 1.0));\n  result = vec4(mix(originColor, targetColor, t), 1.0);"
    elif family == "transfer":
        for name, function, operation in [("encode", "sRGBTransferOETF", "encode a final linear-light fixture preview to sRGB"),
                                          ("decode", "sRGBTransferEOTF", "decode an sRGB fixture swatch to linear light")]:
            yield name, f"Use Three.js r187 color transfer functions to {operation}. Preserve the alpha channel at one. Return RGB without tone mapping.", ["emittedColor"], f"result = {function}(vec4(emittedColor, 1.0));"
    elif family == "blur":
        yield stem, f"Apply the upstream {stem}-tap separable Gaussian filter to a Studio lighting buffer along a supplied pixel direction. Use linear filtering, normalized UVs and the actual source resolution. Preserve the RGB radiance and set output alpha to one.", ["lightTexture", "resolution", "direction"], f"result = vec4({fn}(lightTexture, vUv, resolution, direction).rgb, 1.0);"
    elif family == "palette":
        for name, field, inputs in [("beat-lookup", "fract(beat)", ["beat"]),
                                    ("fixture-world-phase", "fract(0.2 * worldPosition.y + beat)", ["worldPosition", "beat"]),
                                    ("fog-return", "1.0 - exp(-extinctionPerM * mixedDensity * pathMetres)", ["extinctionPerM", "mixedDensity", "pathMetres"])]:
            text = {"beat-lookup": "Color one LED cycle from fractional show beat.",
                    "fixture-world-phase": "Offset the color phase by one cycle per five metres of fixture height.",
                    "fog-return": "Use Beer–Lambert absorptance over the path through the venue as the color phase."}[name]
            yield name, text + " Use the cosine-palette function with a=b=0.5, c=1, phase offsets (0,0.22,0.55). Multiply the resulting linear RGB by emissionGain.", inputs + ["emissionGain"], f"float phase = {field};\n  result = vec4({fn}(phase, vec3(0.5), vec3(0.5), vec3(1.0), vec3(0.0, 0.22, 0.55)) * emissionGain, 1.0);"


def records():
    sources = json.loads((ROOT / "sources.json").read_text())
    for family, src in sources.items():
        origin = ROOT / "upstream" / family
        paths = dict(src["sha256"])
        notice = verified(origin, src["notice"], paths.pop(src["notice"]))
        base = f"https://github.com/{src['repository']}/blob/{src['commit']}/"
        for path, checksum in sorted(paths.items()):
            raw = verified(origin, path, checksum)
            for key, task, inputs, main in tasks(family, path, raw):
                fixture = {name: INPUTS[name][1] for name in inputs}
                uniforms = "\n".join(f"uniform {INPUTS[k][0]} {k};" for k in inputs)
                provenance = {"url": base + path, "repository": src["repository"], "revision": src["commit"],
                              "sha256": checksum, "license": src["license"], "license_url": base + src["notice"],
                              "notice": notice, "attribution": src["attribution"]}
                shader = (f"{HEADER}\n{uniforms}\n/* Upstream: {base}{path}\n{src['attribution']}\n{notice.strip()}\n*/\n"
                          f"{body(raw)}\n\nvoid main() {{\n  {main}\n}}\n")
                request = f"{task}\nInputs for the pass: {uniforms}\nExample: {json.dumps(fixture, sort_keys=True)}"
                yield src["split"], {"id": f"glsl/{family}/{key}", "family": "glsl/three-r187" if family == "transfer" else f"glsl/{family}", "mode": "shader",
                                     "source": provenance, "fixture": fixture,
                                     "messages": [{"role": "system", "content": SYSTEM}, {"role": "user", "content": request},
                                                  {"role": "assistant", "content": shader}]}


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("output", type=Path)
    cli.add_argument("--with-references", action="store_true", help="also emit the Three.js retrieval index and include it in training")
    cli.add_argument("--three-root", type=Path, default=ROOT.parents[2] / "node_modules/three")
    args = cli.parse_args()
    out = args.output
    out.mkdir(parents=True, exist_ok=True)
    splits = {split: [] for split in ["train", "valid", "test"]}
    for split, row in records():
        splits[split].append(row)
    if args.with_references:
        from reference import reference_rows
        references = list(reference_rows(args.three_root))
        splits["train"].extend(references)
        splits["shader-reference"] = references
    for split, rows in splits.items():
        payload = "".join(json.dumps(r, sort_keys=True, ensure_ascii=False) + "\n" for r in rows)
        (out / f"{split}.jsonl").write_bytes(payload.encode("utf-8"))
        print(f"{split}: {len(rows)} rows, sha256 {hashlib.sha256(payload.encode()).hexdigest()}")


if __name__ == "__main__":
    main()
