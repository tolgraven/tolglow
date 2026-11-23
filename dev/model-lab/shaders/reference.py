"""Index verified upstream GLSL includes with the consuming Three.js shader stages."""
import hashlib
import json
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
INCLUDE = re.compile(r"^\s*#include\s+<([\w]+)>", re.M)
MODULE = re.compile(r"export\s+(?:default|(const)\s+(vertex|fragment)\s*=)\s*/\*\s*glsl\s*\*/\s*`([^`]*)`\s*;", re.S)
FLAGS = re.compile(r"^\s*#\s*(?:if(?:n?def)?|elif)\s+([^\n]+)", re.M)
# These bodies cite other shader implementations with separate terms or unclear
# standalone provenance. Keep the include names so downstream context stays accurate.
OMIT = {"common", "lights_physical_pars_fragment", "tonemapping_pars_fragment",
        "transmission_pars_fragment", "envmap_common_pars_fragment", "dithering_pars_fragment",
        "colorspace_pars_fragment"}  # already in the numeric anchors


def unit_sources(package):
    manifest = json.loads((HERE / "three-r187.json").read_text())
    info = json.loads((package / "package.json").read_text())
    if info["name"] != "three" or info["version"] != manifest["version"]:
        raise ValueError(f"Expected the installed three@{manifest['version']}")
    chunks, stages, files = {}, {}, {}
    for rel, git_blob in manifest["file_blobs"].items():
        raw = (package / rel).read_bytes()
        prefix = f"blob {len(raw)}\0".encode()
        if hashlib.sha1(prefix + raw).hexdigest() != git_blob:
            raise ValueError(f"Installed shader differs from the pinned Git blob: {rel}")
        module = raw.decode()
        name = Path(rel).name.removesuffix(".glsl.js")
        matches = list(MODULE.finditer(module))
        if not matches or any("${" in match[3] for match in matches):
            raise ValueError(f"Unsupported JS wrapper at {rel}")
        for match in matches:
            stage = match[2]
            key = f"{name}/{stage}" if stage else name
            (stages if stage else chunks)[key] = match[3].strip()
            files[key] = {"path": rel, "sha256": hashlib.sha256(raw).hexdigest(), "git_blob": git_blob}
    return manifest, chunks, stages, files


def consumers_by_chunk(chunks, stages):
    result = {k: set() for k in chunks}
    for program, shader in stages.items():
        pending, seen = list(INCLUDE.findall(shader)), set()
        while pending:
            chunk = pending.pop()
            if chunk not in chunks:
                raise ValueError(f"Unresolved #include <{chunk}> in {program}")
            if chunk not in seen:
                seen.add(chunk)
                result[chunk].add(program)
                pending.extend(INCLUDE.findall(chunks[chunk]))
    return result


def topic(name):
    prefixes = [
        ("fog envmap cube equirect background", "venue atmosphere and environment lighting"),
        ("lights lightmap meshlambert meshphong meshphysical meshtoon iridescence", "surface lighting and material response"),
        ("shadow depth distance vsm logdepth", "shadows and receiver depth"),
        ("normal bumpmap displacementmap meshmatcap tangent", "surface normals and spatial detail"),
        ("roughness metalness specular clearcoat aomap emissive opaque transmission", "material inputs and emitted light"),
        ("color colorspace tonemapping dithering map gradientmap", "color and presentation"),
        ("alpha clipping", "coverage, transparency and cutaways"),
        ("uv packing", "coordinates and packed data"),
        ("batching instancing morph skin project begin worldpos", "fixture mesh transforms"),
    ]
    return next((label for keys, label in prefixes if any(name.startswith(p) for p in keys.split())), "scene geometry and materials")


def reference_rows(package):
    manifest, chunks, stages, files = unit_sources(package)
    consumers = consumers_by_chunk(chunks, stages)
    policy = json.loads((HERE / "sources.json").read_text())["transfer"]
    notice = (HERE / "upstream/transfer/LICENSE").read_text().strip()
    seen = set()
    for key, glsl in sorted({**chunks, **stages}.items()):
        if key in OMIT or glsl in seen:
            continue
        seen.add(glsl)
        path = files[key]["path"]
        url = f"https://github.com/{manifest['repository']}/blob/{manifest['commit']}/{path}"
        parents = sorted(consumers.get(key, []))
        includes = list(dict.fromkeys(INCLUDE.findall(glsl)))
        guards = sorted({s for condition in FLAGS.findall(glsl) for s in re.findall(r"\b[A-Z][A-Z_0-9]+\b", condition)})
        unit = "material stage" if key in stages else "include"
        stage = key.rsplit("/", 1)[1] if key in stages else ", ".join(sorted({p.split("/")[1] for p in parents}))
        if not stage:
            stage = "renderer-injected or variant-dependent"
        label = topic(key)
        context = {"kind": unit, "stage": stage, "topic": label, "consumed_by": parents,
                   "includes": includes, "feature_macros": guards, "requires_renderer_bindings": True}
        if unit == "material stage":
            question = f"Retrieve the Three.js r187 {key} material stage for a Studio renderer developer tracing {label}. Include its original Three.js include directives."
        elif guards:
            question = f"Show the r187 <{key}> include for {label}. Preserve its renderer-variant branches, including {', '.join(guards[:3])}."
        elif parents:
            question = f"In Three.js r187, which GLSL does <{key}> insert into {parents[0]}? Keep the code in the context of that renderer stage."
        else:
            question = f"Retrieve the Three.js r187 <{key}> chunk for a developer inspecting {label} in the Studio renderer."
        answer = f"/* {manifest['repository']} r187 · {path}\n{notice}\n*/\n{glsl}\n"
        source = {**files[key], "url": url, "revision": manifest["commit"], "repository": manifest["repository"],
                  "license": "MIT", "license_url": f"https://github.com/{manifest['repository']}/blob/{manifest['commit']}/LICENSE",
                  "notice": notice, "attribution": policy["attribution"]}
        yield {"id": f"glsl/ref/three-r187/{key}", "family": "glsl/three-r187", "mode": "shader-reference",
               "source": source, "context": context,
               "messages": [{"role": "system", "content": "Retrieve renderer source with its upstream notice. Shader chunks are consumed by Three.js and require its generated bindings and include expansion."},
                            {"role": "user", "content": question}, {"role": "assistant", "content": answer}]}
