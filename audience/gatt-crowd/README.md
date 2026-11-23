# From 96 guest sessions to 768 show cues

This Linux rehearsal enrolls 96 simulated centrals with separate P-256 keys through the gateway’s exported GATT characteristics on a private D-Bus. A test service plays BlueZ’s adapter role. The gateway decrypts ordinary and six-axis samples, averages them and sends real OSC datagrams to an enrolled, armed Tolglow receiver. The receiver runs its normal sensor frame advance independently at a target of 40 Hz. Four concurrent senders share the bus.

One person reports full effort throughout. The global fraction goes from 1 to 1/96 as 95 resting guests join, then returns to 1. The script admits eight step onsets from each guest. The 24 pulse sharers and 32 six-axis contributors are distinct subsets that can overlap. When the last remaining person withdraws both readings, their effort continues and all pulse and axis values disappear. A final disconnect leaves zero guests.

**All 768 expected cue-launch commands arrived.** JSON includes the aggregate data, counts, admission metrics and time to receive the last command in each complete 96-person burst. Burst time includes preparing and writing the encrypted inputs and the show admitting their mapped commands. This test covers four seconds of onsets at 120 BPM. It does not exercise physical Bluetooth, fixture output, device battery or the Vision/Watch step detector. The separate [venue procedure](https://github.com/tolgraven/tolglow/blob/master/docs/nearby-audience-qualification.md) covers those measurements.

To repeat, use Linux with a session dbus-daemon, JDK 21 and Leiningen. Work from the Tolglow and Afterglow revisions in results.json. Run from Tolglow’s linux/audience directory. Each run generates fresh keys and discards them at exit. Only synthetic aggregate readings are written.

```sh
AUDIENCE_REPORT=/tmp/gatt-crowd.json AFTERGLOW_ROOT=/path/to/afterglow \
  lein with-profile +paired run -m clojure.main /path/to/rehearse.clj
```
