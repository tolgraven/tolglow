# Section patterns in Studio Debug

Tolglow `bdcd24d6170c687d2d063a7a1c45b934a59cc1f0` and Afterglow `e1eb9ff50985a9e3e304b54ec7a97db735956c5a`. In preview-only output with no sink, generated PCM reversed its bass-note pattern while keeping time and level. Studio displayed a Pattern ID of `:midnight-groove` and the enabled track-patterns subscriber that launched Ember. The other registered pattern did not match. Studio marked one delivery at beat 8. The optimized bundle hash and event are in the [browser receipt](browser-receipt.json) and [owner trace](owner-trace.edn).

Separate live mpv replays on the same reference implementation used a generated WAV. Two uninterrupted runs delivered the same pattern at an owner bar. A real seek revoked the queued forecast. On reconnect, the replacement session delivered once and revoked its lock on close. The show clock was advanced under test control. Mixed music, licensed tracks and end-to-end latency remain separate qualification.
