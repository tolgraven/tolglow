# Browser render workers

Afterglow owns expression semantics, shader emission, fixture partitioning and deadline policy. This directory runs the host's compiled WebGL2 transform-feedback plans in an explicitly enrolled browser worker. Keep graphics resources inside that worker. The re-frame Connections panel and the Ring adapter belong to Tolglow.

Keep fixture buffers resident between jobs. Validate shapes and allocation limits before creating GPU objects. A prepared program can be evicted. Report eviction, GPU failure and context loss to the coordinator so it can select the CPU path. A successful result carries little-endian float32 fields for the leased fixture range.

Run `node --test test/js/compute/*.test.mjs` with `CHROMIUM_PATH` pointing to a Chromium executable. The paired two-universe output test is documented in [distributed rendering](../../docs/performance/distributed-compute.md).
