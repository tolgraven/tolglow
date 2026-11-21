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

Message framing, maximum request sizes, authorization and command validation
belong to the Ring and WebSocket endpoints. Existing show files use EDN.
