# Fixture discovery, import and rig patching

The fixture library provides an explicit discovery → import → review → publish →
patch workflow. Search results and imported files never activate fixtures. A
published fixture type is reusable registry data; a patched fixture is one
physical instance with an address, position and calibration.

The implementation covers ordinary OFL, QLC+ QXF and a bounded subset of GDTF
1.1/1.2. It does not claim complete support for these specifications or for all
proprietary console formats. The broader fixture roadmap remains in
[GitHub Issues](https://github.com/tolgraven/tolglow/issues), with product requirements in
[product/integrations.md](product/integrations.md).

## Operator workflow

1. Open **Rig → Fixture library**. **Search saved** uses the local catalog.
   **Search OFL** makes an explicit network request and saves matching catalog
   entries. **Load manufacturers**, then **Save this catalog**, provides an
   alternative way to populate the offline index.
2. Select a result to fetch its definition, choose the physical fixture's DMX
   mode, and import it for review. Alternatively upload an OFL JSON, QXF XML or
   `.gdtf` ZIP file. A file with several modes presents a mode selector before
   producing a draft.
3. Review the source, license, checksum, channel offsets and diagnostics. The
   form edits type identity, human labels, footprint, coarse/fine offsets and
   inversion without code. The advanced EDN editor retains typed identifiers,
   capabilities and original source data. **Create a fixture type** starts an
   editable RGBW PAR definition.
4. **Validate & publish type** publishes the reviewed definition. This does not
   patch an instance or launch a cue. Changed content requires a higher
   definition version; identical current content is idempotent.
5. Set an instance ID, universe, start address, position in metres, orientation
   in degrees and RGBW mixing policy, then **Add fixture to rig**. Moving axes
   require explicit documented calibration. Existing rig edits must be saved or
   discarded first. The host validates the entire patch before committing it.

Unknown physical controls cannot be silently omitted during patching. For
example, a retained reset or shutter channel must have an explicit mapping; the
importer does not infer an operating value. Mapping a channel does not implement
its named capabilities or establish that a particular normalized value is safe
for that fixture. Consult its mode documentation when authoring those values.

The panel preserves draft edits when the show changes but clears the old
publication and cache projection. HTTP and file-reading callbacks carry a
runtime/epoch/generation token, so late results cannot mark an old definition as
published in the replacement show.

## Supported import scope

| Format | Converted into canonical fixture data | Retained without equivalent runtime semantics | Rejected or requires further implementation |
| --- | --- | --- | --- |
| OFL JSON | Ordinary explicit channel lists, null unused slots, one coarse plus one fine byte, known single-capability intensity/color/movement/focus/zoom/iris/fog attributes, selected mode footprint | Original JSON; physical metadata; capability labels/ranges/source; unsupported controls with warnings | Matrix/insert expansion; missing referenced channels; resolution beyond 16 bits; repeated attributes needing separate heads/pixels |
| QLC+ QXF XML | Explicit mode entries; zero-based QXF offsets converted to one-based offsets; ordinary coarse/fine groups; RGBW/amber/UV, intensity, pan/tilt, focus, zoom and iris | Original XML; capability labels/ranges and presets; unsupported groups with warnings | Undefined channels; duplicate/negative mode offsets; capability ranges outside 0..255; more than one fine byte; repeated attributes needing heads/pixels |
| GDTF 1.1/1.2 ZIP or description XML | Ordinary single-break Offset-based 8/16-bit channels; ordered sparse coarse/fine pairs; known dimmer/RGBW/amber/UV/pan/tilt/focus/zoom/iris/fog attributes; function/set range data | Original description XML; channel defaults; physical endpoints; channel/function XML trees; asset filename/size inventory; wheels, models and physical descriptions | Virtual channels; multiple or overwritten breaks; more than 16-bit resolution; offsets outside 1..512 within the break; overlapping offsets; multiple logical attributes; conditional mode functions; channel relations; repeated attributes needing head geometry; geometry-reference expansion; other GDTF versions |

An import result contains `:valid?`, `:definition`, `:candidate`, `:diagnostics`
and `:provenance`. Error diagnostics suppress `:definition` while preserving an
editable candidate. Some malformed source structures throw an actionable import
error instead of producing a partial candidate. Missing mode selection is
converted by the studio upload service into a normal result containing `:modes`
and a `:mode-required` diagnostic.

`valid?` means the supported data conversion succeeded. It is not an assertion
of physical accuracy, complete specification validation or successful device
operation. Warnings can coexist with a valid definition. Patching adds the
stricter requirement that every retained required physical channel be mapped,
that moving-axis ranges be explicit, and that the full rig remain valid.

### OFL details

A generic attribute is inferred from a recognized single `capability`. A
channel described through multiple capabilities is retained for review rather
than automatically translated into a named function selector. Fine-channel
aliases preserve their explicit offsets, including nonadjacent coarse/fine
bytes. A null mode slot contributes to the footprint but is not a required
control. Duplicate semantic attributes cannot represent multiple fixture heads
in this compact descriptor and produce an error.

OFL capability data is retained; the importer is not a full OFL JSON-schema
validator. Physical intensity curves, color calibration, wheels, shutter/reset
semantics and matrix geometry require dedicated conversion and renderer work.

### QXF details

`Group Byte="0"` is coarse and `Byte="1"` is fine. Channel `Number` is converted
from zero-based mode numbering. The importer preserves capability presets as
data and does not execute preset behaviors. Head/pixel grouping, fixture optics,
physical calibration and richer preset semantics are not reconstructed from
QXF. Fixture manufacturer/model labels are retained, while stable ID/version
and channel mappings are validated by the registry.

### GDTF details

`Offset="1,5"` means coarse byte 1 and fine byte 5; the pair is not reordered.
The GDTF `Uint/n` convention mirrors a value to the destination resolution, while
`Uint/ns` shifts bytes: at 16 bits, `255/1` becomes 65535 and `255/1s` becomes
65280. Function ranges end immediately before the next function start; nested
channel sets remain within their parent range. Invalid ordering or out-of-range
values are rejected.

Multiple ranges for one recognized attribute are retained with a warning. The
renderer still writes a normalized generic channel value; it does not select a
named function, apply physical curves, honor mode masters, evaluate relations,
or apply GDTF default/initial-function behavior. DMX profiles, emitters, filters,
gobos, wheel animation, prism behavior, model geometry, photometry and spectral
simulation are not implemented by this importer. XML preservation provides the
source for subsequent work, not equivalent execution.

Geometry-reference presence currently rejects the fixture import conservatively,
even when a reference might be unrelated to the chosen mode. Automatic expansion
into separately positioned heads and breaks remains future work. `.mvr` scene
archives and proprietary console libraries are not accepted as GDTF files.

## Network catalog and provenance

The OFL provider uses a fixed `https://open-fixture-library.org` origin and these
observed endpoints:

| Operation | Request | Relevant response |
| --- | --- | --- |
| Manufacturer index | `GET /api/v1/manufacturers` | Manufacturer-key map with name and fixture count |
| Manufacturer catalog | `GET /api/v1/manufacturers/{key}` | Manufacturer metadata, fixture entries and redirects |
| Search | `POST /api/v1/get-search-results` with `searchQuery`, `manufacturersQuery`, `categoriesQuery` | Array of full fixture keys and labels |
| Definition | `GET /{manufacturer}/{fixture}.json` | OFL fixture JSON with available channels and modes |

The server source describes `fixtures/register.json` as generated data; it is
not assumed to exist as a downloadable committed GitHub file. Search stores
catalog metadata, while fetching stores the source document. Fetching an
already cached document makes no request unless `:refresh? true` is explicit.

Provenance records the source URL or upload label, format, SHA-256 checksum,
checksum basis and supplied retrieval metadata. OFL cache entries retain the
project's MIT license notice and license URL. Uploads do not inherit an invented
license from their file format. A GDTF archive checksum covers the original ZIP;
XML-only import checksums cover the description text. Parsed OFL maps use a
canonical JSON checksum; original OFL text uses its source text.

The injectable provider is a request-map → response-map function. The default
provider rejects redirects and unrecognized paths, bounds responses, and uses a
whole-request deadline including response-body reads. It honors an explicitly
configured HTTP proxy or the HTTPS proxy environment; `:proxy-url nil` disables
proxy use. No network request occurs when namespaces load or a manager is
created.

## Bounds and ownership

| Boundary | Default bound |
| --- | --- |
| OFL/QXF text upload | 4 MiB |
| GDTF compressed archive | 16 MiB |
| GDTF expanded archive total | 32 MiB |
| GDTF description XML | 4 MiB |
| GDTF archive entries | 256 |
| GDTF XML structure | 50,000 elements, depth 64 |
| Library catalog | 20,000 entries |
| Cached documents | 64 |
| General library document-byte budget | 16 MiB |
| Studio host document-byte budget | 4 MiB |

GDTF files are streamed in memory without extracting archive members. Duplicate
or traversal-style entry names are rejected. Model/image contents are counted
against the expansion limit but discarded; only the asset inventory is kept.
DTD and external-entity resolution are disabled for XML imports. The GDTF
parser additionally checks structural limits after parsing bounded XML.

The cache owns immutable state inside one explicit service manager. Network
work runs under that manager's lock, outside atom retry functions and outside
the show render lock. Restore validates portable structure, capacity, source
identity, document checksums and derived metadata before replacing anything.
Close releases the provider once and rejects subsequent operations.

The cache byte budget measures stored source documents, not the complete EDN
show file. Catalog metadata and repeated source/license data also consume show
space. Full-show persistence enforces its separate total document-size bound;
there is no silent truncation to fit that bound.

## API and implementation map

- [studio/fixtures.clj](../tolglow/studio/fixtures.clj): immutable cache and
  explicit provider calls; `search` is offline, `search-remote` is explicit.
- [studio/fixture_service.clj](../tolglow/studio/fixture_service.clj):
  `create-manager`, `snapshot`, `search!`, `refresh!`, `fetch!`, `import!`,
  `export-state`, `validate-state`, `restore!` and `close!`; pure `import-file`
  and `instantiate`.
- [integrations/fixtures.clj](../tolglow/integrations/fixtures.clj) and
  [integrations/gdtf.clj](../tolglow/integrations/gdtf.clj): data-only converters.
- [fixture_library.cljs](../src/studio/tolglow/studio/fixture_library.cljs):
  Reagent views and re-frame events/subscriptions/effects for discovery and
  editing. The browser never evaluates submitted EDN as code.
- `GET /api/fixtures` returns cache metadata. `POST /api/fixtures/search`,
  `/refresh`, `/fetch` and `/import` access the explicit library service.
  `/publish` accepts `{:source "<one EDN definition>"}`; `/patch` accepts a
  published fixture type ID and explicit instance options. These two mutations
  cross the runtime's guarded command queue.

A compact patch example:

```clojure
{:fixture-type-id "rgbw-par"
 :id "par-left"
 :universe 1 :address 101
 :position [-2 3 0] :rotation-deg [0 0 0]
 :rgbw-policy "extract-min"
 :calibration {}}
```

Canonical channels use `{:red 1 :pan [5 8]}` or
`{:pan {:offset 5 :fine-offset 8 :inverted-from 1}}`. Offsets are one-based
relative to the start address. Crossing channel 512 advances the universe,
including fine bytes. An explicit footprint reserves every byte in the mode,
including unused holes, so another fixture cannot overlap those holes. IDs may
be plain strings or namespaced keywords; typed EDN preserves the distinction.

[studio/rig.clj](../tolglow/studio/rig.clj) supplies shared validation for the
complete rig and saved shows. It preserves imported scene metadata, checks the
actual fixture patch, and rejects nonportable values. Room dimensions must be
positive finite metres when a room is supplied. Trusses have unique stable IDs,
finite positions and optional rotations, and positive lengths. Room and truss
data remain optional for a headless rig. The current preview draws at most eight
trusses as simple yaw-oriented metal beams; it does not reconstruct structural
lattice geometry or photometric material properties.

## Verification and remaining acceptance work

The focused backend suite currently covers 31 tests and 288 assertions across
library, service, GDTF and registry behavior. It exercises injected catalog
requests, offline reuse, bounded eviction, malformed input, mode selection,
archive/XML bounds, concurrent cache access, atomic restore, exact coarse/fine
patching and required calibration. The production frontend and development
compile both completed without warnings before host workflow verification.

A live read-only OFL check fetched the Robe catalog (11 entries), searched
LEDBeam (2 matches), fetched a four-mode definition, imported its first mode
with preserved unmapped-channel diagnostics, and validated exported cache state.
This proves those catalog operations against the observed service, not every
OFL fixture. GDTF coverage currently uses specification-grounded generated
archives; a licensed corpus of real vendor files and physical fixtures remains
necessary for broader interoperability claims.

The host-handler suite adds 3 tests and 59 assertions through the actual Ring
handler, captured runtime and command queue: draft-only imports, typed candidate
source, manual and namespaced type IDs, publication, calibration, whole-footprint
collisions and unchanged rig state after rejected operations. Shared rig and
existing persistence tests pass together at 24 tests and 217 assertions, covering
scene preservation and malformed geometry rejection during full-show restoration.

Actual browser operation is separate evidence from converter and host tests.
Keep its latest results in the work tracker. Future
acceptance requires explicit head/matrix geometry, mode/range semantics,
photometric calibration and format-specific fixtures before broadening the
support claim. Remote provider authentication, more fixture repositories,
proprietary formats and MVR rig import need their own adapters and evidence.
