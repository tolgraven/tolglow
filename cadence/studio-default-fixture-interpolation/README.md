# Default Studio fixture interpolation

This checks the production Studio release from `6121005a` in Chrome 158 on the physical M4 MacBook Air. The checked-out release assets were served through the packaged loopback backend from [run 45529197893](https://github.com/tolgraven/tolglow/actions/runs/45529197893). The SHA-256 of the served Studio script matched the local release bytes.

- The fresh Studio selected **Smooth · one frame delay** before input. The real Apple M4 ANGLE Metal worker reported interpolation active, presented and rendered all eight heads.
- Selecting **Follow show frames** changed the worker to hold. Selecting Smooth restored interpolation. The worker presented newer frames during both steps and the show supplied changing pan values for the first head.
- The preview-only show used Ember and Sweep at a configured 40 Hz. The receipt covers startup mode and interaction. It does not contain a display or show compute cadence measurement.

The focused Studio suite passed 311 tests and 1,720 assertions. The production Studio and compute-worker releases built with zero compiler warnings. The probe closed its own tab and loopback server.
