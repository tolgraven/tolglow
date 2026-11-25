# Studio Transit values

The Studio codec uses Cognitect Transit JSON on Clojure and ClojureScript.
`tolglow.transit/encode` returns a UTF-8 JSON text message, and `decode`
reconstructs the value. Each message has its own Transit cache. Keywords,
strings, sets and vector keys can be used directly. For example, `:rig/spot`
and `"rig/spot"` can occur as two keys in the same map.

The tags `thi.ng.color/{rgba,hsla,hsva,cmyka,int24,int32,css}.v1` carry the
fields of the corresponding thi.ng.color record, including alpha and any
additional record entries. Both runtimes construct the original color type. A
browser receiving HSLA can call `thi.ng.color.core/adjust-alpha` and send the
result back with its hue and saturation intact. The typed fields take finite
numeric components, valid packed integers, or a CSS string as appropriate.
Interpolate fixture emissions in the renderer's declared linear color space.

A Clojure ratio uses the `tolglow/ratio.v1` tag with a pair of decimal strings.
A denominator must be positive and each part may contain up to 128 characters.
The ClojureScript reader produces a `BeatFraction` that retains both integers.
Use `fraction-value` to obtain a JavaScript number for display or animation.
Send the original value when passing a fractional duration through an edit.
For example, one third of a beat returns to the JVM as exactly `1/3`.

## Native values

Each tag below is stable. A reader rejects a malformed representation instead
of constructing a partial value. The ClojureScript records live in
`tolglow.transit`.

| Tag | JVM | ClojureScript | Representation |
| --- | --- | --- | --- |
| `org.jogamp.vecmath/point3d.v1` | `Point3d` | `Point3d` record `{:x :y :z}` | `[x y z]` numbers |
| `org.jogamp.vecmath/vector3d.v1` | `Vector3d` | `Vector3d` record `{:x :y :z}` | `[x y z]` numbers |
| `tolglow/instant.v1` | `java.time.Instant` | `Instant` record `{:epoch-second :nano}` | `["<epoch seconds>" nano]` |
| `m` (Transit) | `java.util.Date` | `js/Date` | epoch milliseconds |
| `b` (Transit) | `byte[]` | `Uint8Array` | base64 |
| `tolglow/byte-buffer.v1` | `ByteBuffer` (read-only on decode) | `ByteView` value `{:order :bytes}` | `["big"\|"little" <b>]` |
| `tolglow/int8-array.v1` | `byte[]` (read only) | `Int8Array` | little-endian `<b>` |
| `tolglow/uint8-clamped-array.v1` | `byte[]` (read only) | `Uint8ClampedArray` | `<b>` |
| `tolglow/int16-array.v1` | `short[]` | `Int16Array` | little-endian `<b>` |
| `tolglow/uint16-array.v1` | `char[]` | `Uint16Array` | little-endian `<b>` |
| `tolglow/int32-array.v1` | `int[]` | `Int32Array` | little-endian `<b>` |
| `tolglow/uint32-array.v1` | `int[]` (read only) | `Uint32Array` | little-endian `<b>` |
| `tolglow/int64-array.v1` | `long[]` | `BigInt64Array` | little-endian `<b>` |
| `tolglow/uint64-array.v1` | `long[]` (read only) | `BigUint64Array` | little-endian `<b>` |
| `tolglow/float32-array.v1` | `float[]` | `Float32Array` | little-endian `<b>` |
| `tolglow/float64-array.v1` | `double[]` | `Float64Array` | little-endian `<b>` |
| `tolglow/queue.v1` | `PersistentQueue` | `PersistentQueue` | items in order |

**Wire change.** Before #441, `short[]`, `char[]`, `int[]`, `long[]`,
`float[]` and `double[]` went out as plain Transit arrays and arrived in the
browser as vectors. They now arrive as typed arrays, which do not support
`nth`, `count` or `=` like vectors. No Studio projection currently carries one;
`tolglow.studio.state-stream-test` walks the stream view and snapshot with
capacity, budget, activity and output response enabled to keep it that way.
Convert a primitive array to a vector at the projection boundary when a
consumer expects one.

`<b>` is a nested standard Transit binary value. Vector components arrive as
doubles on the JVM, so `[0,0,-1]` written by JavaScript reads as `-1.0`.

**Timestamps.** An `Instant` keeps its full nanosecond field across both
runtimes and back. The browser record satisfies `inst?`, and `inst-ms` floors
sub-millisecond time to its containing millisecond, matching
`Instant/toEpochMilli`. Use `(js/Date. (inst-ms t))` for display. Sub-millisecond
digits remain in `:nano` and return unchanged to the JVM. Browser Instants
compare and sort chronologically. The browser rejects, with an
`"Instant is outside the js/Date range"` error, an epoch second outside
−8,640,000,000,000 to 8,639,999,999,999, so `(js/Date. (inst-ms t))` is always
valid. The JVM accepts the full `Instant` range. A `Date` and `js/Date` carry
milliseconds only. Event timestamps such as debug rows remain `Date`.

**DMX and bytes.** JVM `byte[]` and `Uint8Array` share Transit's `b` tag. The
bits are unchanged, so DMX level 255 is `255` in the browser and byte `-1` on
the JVM. A `ByteBuffer` sends its remaining bytes and byte order without moving
the source position. The JVM receives a read-only heap buffer with position 0.
Direct and offset characteristics are not carried. A browser `ByteView` compares
and hashes by order and byte contents, so equal decodes are `=` in app-db.
Treat its `:bytes` as read-only.

**Arrays.** The JVM has no unsigned arrays. `Uint32Array` and `BigUint64Array`
reach the JVM as `int[]` and `long[]` with identical bits, and a JVM reply
returns them as `Int32Array` and `BigInt64Array`. `Int8Array` and
`Uint8ClampedArray` reach the JVM as `byte[]`, and a reply returns them on the
`b` tag as an unsigned `Uint8Array`. An `Int8Array` element of −1 therefore
comes back as 255. Neither 8-bit type survives a round trip through the JVM.
`boolean[]` and object arrays are written as Transit arrays and read as vectors.

**Collections.** Maps, sets, vectors, lists and lazy sequences use standard
Transit tags. Sorted maps and sets arrive as hash maps and sets because a
comparator is not data.

**Inbound commands.** Studio HTTP bodies and state and debug stream messages
use `decode-command`. It turns every numeric array tag into a vector of the
numbers the browser held (unsigned values stay non-negative) and refuses
`tolglow/byte-buffer.v1`, so commands cannot place mutable arrays or buffers in
model state. Standard `b` binary values remain `byte[]`, as before. Compute
workers and show import use `decode`.

Message framing, maximum request sizes, authorization and command validation
belong to the Ring and WebSocket endpoints. Show files use this codec but
accept only the portable values listed in [show files](show-files.md); native
vectors, instants, buffers and numeric arrays are rejected there.
