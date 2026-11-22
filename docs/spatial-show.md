# Fixture selection and contact actions

A cue can choose its target fixtures at launch. A moving ball, a fixed region of the venue, or excess light measured around an HDR source can supply that selection. Afterglow calculates membership in venue coordinates. Tolglow runs a binding as an ordinary, scoped show command and retains the identity of each instance it owns.

For example, this setting runs a registered cue on each fixture as the moving sphere reaches it. Substitute a cue that exists in the show's registry.

```clojure
{:enabled? true
 :run-when :demand
 :bounds {:min [-4 0 -4] :max [4 4 4]}
 :gravity [0 -2 0]
 :volumes {:throw {:shape :ball :position [-2 1 0] :velocity [3 1 0]
                  :radius 0.6 :restitution 0.95 :hardness 0.7}}
 :bindings {:pass {:volume-id :throw :action {:cue-id :wash} :duration :contact}}}
```

Send the map as `{:scope :spatial :values settings}` through the show's `:settings` operation. Subsequent edits replace a volume or binding by ID. A nil value at that ID removes it. Use `:duration {:beats 1/2}` for a timed action. A contact binding remembers which instances it launched so another performer's launch of the same cue survives contact exit. Random actions can restrict registry candidates by kind and category. Candidates must be usable by the contacted fixture. Add an ordinary event subscription to handle `:spatial/enter`, `:spatial/exit` or `:spatial/bounce` in show code.

A manual launch can supply `:launch {:selector {:volume-id :throw}}`. Fixture IDs are captured in patch order. An explicit empty selection lights no fixtures. Preferences and selection history are available to the Studio and Push pickers. The physics clock is bounded and deterministic. Shared time inputs, room bounds and fixture positions can all be exercised without a rendering window.

The aggregate query `[:spatial/scene]` returns signals by volume. `[:spatial/body :throw]` and `[:spatial/members :throw]` serve individual consumers. Demand mode freezes simulated time after the last consumer leaves, provided no enabled binding or matching event subscriber needs it. Choose `:run-when :enabled` to continue simulating with the screen closed. Position, velocity, speed, direction, rotation, deformation, membership and action ownership are portable show data. Network leases belong to the connected client.

Open **Rig → Spatial** to edit, throw and inspect. The subscriptions guide covers linked query lifetimes, immutable Transit projections and resynchronization. The cue inspector shows the affected instance and effect inputs. [Spatial Studio](spatial-studio.md) · [Studio subscriptions](studio-subscriptions.md) · [Cue inspector](cue-inspector.md)

## Review and verification

- A single `[:spatial/scene]` lease covers every visible preview body. A rig with 32 or more volumes can open the preview without exhausting the socket’s limit of 32 queries. The selected body can still expose its own details and members. [Studio subscriptions](studio-subscriptions.md)
- A public query can be registered after a viewer subscribes. Validation distinguishes an unavailable first value from a valid `nil`. A bad policy, cyclic dependency or throwing handler leaves unrelated views running. The integration review exercised the Ring/Jetty transport with 10 tests and 92 assertions. [WebSocket tests](../test/tolglow/studio/state_stream_test.clj)
- Fast fixture crossings are swept along each straight segment of a bounce. The Afterglow regression captured four contact transitions during a single 5 ms step. Continuous collision detection between two moving balls within a step is outside that coverage. [Afterglow spatial tests](https://github.com/tolgraven/afterglow/blob/59da3d6e42a95a9498674d40e4e3ff310f3f10d0/test/afterglow/spatial_test.clj)
- Browser checks covered the Studio editor, phone layout and Push webview. Native review also traced the iPhone and Watch relay and checked that OSC enrollment uses the selected host, port and device. Physical lighting, controller feedback, phone calibration and cross-network Watch delivery still need hardware verification. [Spatial controls](spatial-studio.md)
