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

## Match a strip to a fixture's extent

Declare `:bounds-m {:min [0 -0.2 -1] :max [0 0.2 1]}` on a fixture type, a patch placement or a builder head. These are metre coordinates in the fixture or head's local space. In Studio, add the field in **Fixture Library → Advanced definition · EDN** before publishing and patching the type.

Resolve a strip segment from the current rig before launching a cue. The reference can be a fixture or an individual head with declared bounds. For a fixture with only head channels, the first patched head stores the body's original placement and extent. The selection projects the rotated bounds onto the requested venue axis and includes the named strip's pixel centers on either boundary. Use the strip's fixture or pixel-layout ID.

The projection reduces angles in degrees before computing the rotation coefficients. Selection compares the original pixel centers with the projected edges.

```clojure
(require '[tolglow.show :as show] '[tolglow.spatial.targets :as targets])

(let [selector (targets/strip-segment-selector
                (show/snapshot runtime)
                {:strip-id :stage-strip :reference-id :front-panel :axis :x})]
  (show/submit! runtime :command
                {:op :cue/launch :cue-id :wash :launch {:selector selector}}))
```

Studio clients can send the same command with the resolved `:fixture-ids` selector to `POST /api/command`. Resolve the segment again for the next launch after changing the reference or pixel positions. A strip with no matching pixels yields an empty selector.

## Launch a sculpted volume as a cue

**Rig → Volumes** places a sculpted region on a cue page, starts it, and shows its current shape in the rig. The same operation runs headless. For example, this ball paints blue toward its centre, follows a show variable for hue and follows the ordinary cue lifecycle.

```clojure
(show/submit! runtime :spatial-cue
  {:op :spawn :id :rolling-wash :name "Rolling wash" :page-id :main
   :volume {:shape :ball :position [-2 1 0] :velocity [1.5 0 0]
            :gravity [0 -2 0] :restitution 0.9 :radius 0.6
            :selection #{:position}
            :falloff {:inner-m 0.2 :outer-m 0.3 :curve :smoothstep}}
   :paint {:color [0.0 0.0 1.0]}
   :sources {:color {:hsl [{:variable [:wash-hue] :default 0.65} 0.8 0.5]}}})
```

The receipt gives the `:cue-id`, `:instance-id`, `:page-id` and `:cell`. Omit `:cell` for the first free grid cell or supply `[column row]`. A collision with an occupied cell or definition ID rejects the whole operation. Launching the registered cue again creates a fresh instance with the saved controls. A timed launch can use `:launch {:mode :one-shot :duration {:beats 1/2}}`. The Ring endpoint accepts the same map as a Transit POST to `/api/spatial/cues`.

- **Shape and motion.** Coordinates, sizes, radii and feather distances use metres. A rounded box has a centre in `:position`, full dimensions in `:size`, and `:rounding` from zero to one. A ball with a `:geometry` uses the local shape as its field and the outer `:radius` for collisions. A sphere or rounded box exposes both shape choices on its cue. `:scale` stretches the field along its local axes. Use a union, intersection or difference for a composite field. The rig editor can configure each operand. [Shape definitions](spatial-studio.md)
- **Fixture selection.** `#{:position}` selects heads by their world origin. Add `:beam` to include illuminated beams that enter the field, or `:aimable` to include heads that can point into it. Multiple modes take their greatest weight for each head. The beam calculation reads the last committed fixture output, fog extinction and receiver surfaces. A launch with `:targets` further restricts the field. Set `:paint {:aim :volume}` to steer reachable heads toward points inside the moving volume.
- **Paint and scene blending.** A head inside the solid centre gets `:strength` times the final group opacity. A head within the edge feather receives a fraction. Its lower-priority look remains visible underneath. `:paint` accepts native color, dimmer, focus, direction or other registered fixture fields. Supply `:members [{:cue-id :wash} {:effect-id :sweep :launch {:params {:speed 0.3}}}]` or `:scene-id :saved-macro` to apply an existing look inside the volume. Each member gets one owned instance. All its native writes and slot transforms compose before the field weight is applied. A cue outside this group keeps its own identity.
- **Mixed rigs.** Paint and member writes skip heads without the attribute, as every untargeted output does ([mixed rigs](effect-recipes.md#mixed-rigs-heads-without-the-attribute)), so a dimmer-only head or hazer never stops the cue lighting its RGB heads. A head outside the volume keeps its resting output. A head with both colour and dimmer channels rests with the dimmer open and the colour at zero, so its dimmer channel reads full while it emits nothing. [Mixed-rig regression](../test/tolglow/spatial_cues_test.clj)
- **Live controls.** Cue parameters expose strength, edge feather, velocity, bounciness, size, position and the declared paint. `:sources` can take a scalar signal, a show variable, a running cue parameter, three component sources or an HSL triple. The ordinary parameter commands, modulation controls and device mappings apply. The active shape, paint, membership count and any invalid-volume fault appear in the leased `[:spatial/scene]` view. An invalid room or shape edit removes the affected field from output and emits a `:spatial/invalid-volume` event. Correcting it resumes output. If a scene member rejects its parameter overrides, the cue reports that rejection and retries when the referenced registry definition changes.
- **Large patches.** A patch of 4,096 or more fixtures samples each field over contiguous fixture ranges on workers owned by the show. Afterglow's samplers still evaluate every head, and the ranges merge in the serial order, so the weights, aim points and their iteration order match the serial path. The default uses one fewer worker than the JVM's available processors, counting the render thread, and smaller patches stay serial. `(show/create-runtime {:spatial-sampling {:workers 1}})` keeps every patch serial. `:partitions` and `:min-fixtures` set the range count and the threshold. Membership of configured volumes and ball motion still run on the render thread. No timing has been recorded for this path. [Sampler regressions](../test/tolglow/spatial/sampling_test.clj)
- **Save and restore.** Saving a show captures the grid cue, field, sources, launching cue/effect references and owned member runs. Loading it remaps runtime instance identities. Stopping the restored parent releases its owned scene and leaves other performers’ launches running. [Show files](show-files.md)

## Review and verification

- A single `[:spatial/scene]` lease covers every visible preview body. A rig with 32 or more volumes can open the preview without exhausting the socket’s limit of 32 queries. The selected body can still expose its own details and members. [Studio subscriptions](studio-subscriptions.md)
- A public query can be registered after a viewer subscribes. Validation distinguishes an unavailable first value from a valid `nil`. A bad policy, cyclic dependency or throwing handler leaves unrelated views running. The integration review exercised the Ring/Jetty transport with 10 tests and 92 assertions. [WebSocket tests](../test/tolglow/studio/state_stream_test.clj)
- Fast fixture crossings are swept along each straight segment of a bounce. The Afterglow regression captured four contact transitions during a single 5 ms step. Continuous collision detection between two moving balls within a step is outside that coverage. [Afterglow spatial tests](https://github.com/tolgraven/afterglow/blob/59da3d6e42a95a9498674d40e4e3ff310f3f10d0/test/afterglow/spatial_test.clj)
- Browser checks covered the Studio editor, phone layout and Push webview. Native review also traced the iPhone and Watch relay and checked that OSC enrollment uses the selected host, port and device. Physical lighting, controller feedback, phone calibration and cross-network Watch delivery still need hardware verification. [Spatial controls](spatial-studio.md)
