# Shape a change in emitted light

Output response runs after effects have been composed. Attack controls each rising emitter and release controls each falling emitter. A moving head can pan immediately while its color continues to fade. A cue can end while its last light remains visible. A shutter that closes can cut off that light immediately. The shared output budget still caps the result.

In Studio, open the performance controls and choose a **Response** for attack and release independently.

| Response | Stage value | Behavior |
| --- | --- | --- |
| Exponential smoothing | `{:beats 1/2}` | Over half a beat, close about 63% of the remaining gap. |
| Reach in duration | `{:mode :linear :beats 1/2}` | Reach a held target after half a beat, including across uneven render intervals. |
| Blend per frame | `{:blend 1/4}` | Close one quarter of the remaining gap on each new frame. Zero holds the current light and one follows the target immediately. |

For example, a light rising from darkness to full over one beat in **Reach in duration** mode reaches half output after half a beat. If the target changes at that point, the renderer starts the next transition from the level already reached. Each interval advances toward the latest sampled target. Earlier intervals contribute their actual elapsed time. A continuously changing target may keep the light in motion until the target settles. Each emitter tracks its own target, so changing blue leaves a steady red fade on schedule.

Use `:ms`, `:beats`, `:bars` or `:phrases` for either timed response. Musical durations follow the show’s transport and stay in place when its beat stops advancing. **Blend per frame** follows each advancing render timestamp once. Re-rendering a timestamp adds no second blend. A slower render rate produces fewer blend steps over the same wall time.

## Scope and map the response

```clojure
(require '[tolglow.show :as show])

(show/submit! runtime :settings
  {:scope :output-response
   :values {:enabled? true
            :global {:attack {:mode :linear :beats 1/2}}
            :groups {:front {:release {:bars 1/2}}}
            :fixtures {:solo-spot {:release {:blend 1/8}}}
            :cues {:glint {:attack {:mode :linear :ms 125}}}
            :compositions {:closing-macro {:release {:mode :linear :beats 2}}}}})
```

Group IDs select patched groups or the membership of a named lighting scene. Fixture IDs select individual patched heads. Cue and composition IDs follow the heads where those running instances contribute emission after blending. Compositions can be macros or sequences. Their release can continue on those heads after the last slot or cue has disappeared. A new rising look follows its own applicable attack. Scopes shape the combined emitter output of the selected heads.

The global stage supplies the default. A matching local stage takes precedence, and overlapping local stages use the smallest requested step for that interval independently for attack and release. Blackout, zero master and output calibration retain their existing precedence. Seeking backwards and replacing the show clear the old light history.

Numeric amounts can come from a running cue parameter or a show variable. The fallback applies while the source is unavailable.

```clojure
{:attack {:mode :linear :beats {:cue-id :feel :param :rise :default 1/4}}
 :release {:blend {:variable [:user :persistence] :default 0.125}}}
```

Hardware testing and venue brightness measurements are separate from software timing. See [scope and budget controls](group-lighting-scenes.md) and [render measurements](performance/lighting-controls.md).
