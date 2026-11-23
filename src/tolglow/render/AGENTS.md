# Show render connections

This namespace admits Studio worker connections to Afterglow's distributed pool. Require the configured Studio origin and a separate compute credential. Keep that credential in the host process and the temporary client handshake. Use a bounded asynchronous queue for each connected peer. Closing a show or a socket must release its timers, queued frames and pool membership.

The production coordinator, shader semantics and physical output ownership live in Afterglow. Run `lein test tolglow.render.workers-test` with the pinned Afterglow checkout and follow the [distributed output test](../../../docs/performance/distributed-compute.md) to compare actual Art-Net bytes.
