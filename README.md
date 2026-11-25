# Buffered output during beam rejection

The in-memory sink captured seven physical publication slots with independent buffering. Both interpolation midpoints stay dark for the rejected follower. The unrelated fixture continues to change, and the moving fixture retains its interpolated movement bytes. All five exact endpoints match their complete committed buffers.

The four captures used rich and compact render paths with global and fixture-scoped output response. They agreed byte for byte. The still and 7.2-second clip use these packets, slowed by 32× to show their order. There was no physical output.

The tested source was Afterglow `0986a868d4f2a86eb9f42f5619e5a92b3c76395f` and Tolglow `c0163e03ef4bb5e181e5a6416dde313d390757c4`. The PR is based on current Afterglow `master`; candidate `7a54b5d548ea5b6935d5683a9b412bc5376b3952` has identical source and tests. The focused suite passed 107 tests and 1,481 assertions. Hardware behavior and geometry between two allowed frame samples remain open. The separate inverted idle-output issue is tracked in Tolglow #524.
