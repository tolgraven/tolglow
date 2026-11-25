# Shared controller preview in Studio and the REPL

Captured from Tolglow `ee529b2d9ea9ffeb6c1d2c0e95a3edf70a50aa38` and Afterglow `e1eb9ff50985a9e3e304b54ec7a97db735956c5a`. The release candidate `23a94cf4cc6336064c9468a4cf0860bf4085b9c4` has identical application and Studio inputs. The intervening master change is the native Mac Room view attachment.

The preview-only show captured a Seaboard M device profile through an injected MIDI transport. Studio showed Lift on the connected controller, then saved a settings draft that disabled it. The running REPL and Ring returned equal typed responses for the declared, connected and disabled cases. The exact optimized Studio bundle was served unchanged. Six browser checks passed without page errors or requests outside loopback.
