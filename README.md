# Studio grouped XYZ alignment

The screenshot and clip were captured from the preview-only Ring Studio at `76397241`. The opened XYZ group displays the full three-line authored Z label. The three knobs and numeric inputs stay aligned, and a keyboard edit changes only X. The capture used Chromium touch emulation at 820 × 900.

The combined PR candidate `7668c874` separately passed the same browser fixture at 390 px and 820 px. Both widths kept the existing labels complete, with zero spread among knob centers and input tops. Keyboard and vector edits reached the actual running Ring show. The paired build passed 300 Studio tests and 1,630 assertions, with zero compiler warnings in both optimized modules. A physical iPad was not used.
