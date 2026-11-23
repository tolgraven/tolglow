# Keeping an empty gateway available

The trace follows the Linux stage publisher and Tolglow's enrolled OSC sensor path over 5.6 seconds of controlled show time. One operator arm at 0.50 s covers two groups, with an empty dance floor between them. Stopping publication at 4.10 s tests loss of the gateway itself. Before the change, the returning group lost its accent. With the change it launches its cue and the show still disarms when the stage stops.

The two JSON files contain one row every 25 ms. Inputs enter at the stage's floor reducer, traverse the OSC encoder/decoder and reach the show through its actual credential check and armed sensor mapping. A separate private D-Bus and UDP test in the product PR exercises encrypted guest writes across the empty interval. Neither measurement requires a physical transmitter.

Use compare.clj from this directory to run each revision. From the corresponding project's linux/audience directory, run the command below with a paired Afterglow checkout at 14abdf9630baba74bbf70029a07609a14686eb01. The original Tolglow revision is e486dd9ef7377a8239aed4844500cf2edf16c9b3. The updated revision is 6b836ea47c87cf38fa193a6f964928c69e62aec9.

```sh
AUDIENCE_REPORT=/tmp/empty-floor.json AFTERGLOW_ROOT=/path/to/afterglow \
  lein with-profile +paired run -m clojure.main /path/to/compare.clj
```
