# GitHub-built Mac app with default interpolation and zero work light

The official arm64 package from [run 45535243051](https://github.com/tolgraven/tolglow/actions/runs/45535243051) represents PR #489 head `d5664f5270780061c767863bb6f5f1b211e4873d`. GitHub built synthetic PR merge `f8714c9a42fe8787f70f4a85ec77d1ce819c92ae` with Afterglow `7a54b5d548ea5b6935d5683a9b412bc5376b3952`. The unmodified archive passed strict signature verification, had no broken symlinks and declared `se.tolgraven.tolglow.studio`.

The package JAR, its running local server and the local release match for both production renderer scripts. The main Studio application script differs between the package and the local build. This receipt observes the package's own application script. The metadata records both digests.

On physical M4 hardware the preview got ready, presented frames and drew on the Apple GPU worker with eight active lights. It reported `interpolate` and work light 0. The instrumented observer captured 29 link events and 58 compiled shader stages with translated MSL. The final snapshot was unfocused after presentation. The qualification covers startup and translation, and supplies no foreground cadence sample. The app and its ephemeral disconnected loopback show stopped afterward.
