# Musical timeline

The timeline shows what has just happened in a show and what is about to
happen, on the transport's beat clock. Now is fixed at the centre. The left
half has played and the right half is upcoming. Studio draws it in **Perform**,
and the REPL reads the same data. Issue
[#646](https://github.com/tolgraven/tolglow/issues/646) tracks this work.

[Beat-clock scheduling](scheduler.md) · [Captured audio projection](waveform-projections.md) ·
[REPL and Studio parity](repl-studio-parity.md) · [Source](../src/tolglow/timeline.clj)

## Read it from the REPL

```clojure
(require '[tolglow.show :as show])

(def view (show/timeline runtime {:bars 4}))   ; four bars either side; default 8, at most 32

(:position view)                               ; => {:bar 32 :beat 2.25 :phrase 8}
(map (juxt :event-id :state :beat) (:events view))
(map (juxt :event-id :state :start-beat :end-beat :curve) (:ramps view))
(get-in view [:sections :items])
(:meters view)
```

Studio leases the same map over the state stream as the query
`[:timeline/view {:bars n}]`. It is projected on the stream worker from each
committed frame, never on the lighting deadline, and only while a timeline is
mounted. Nothing in the projection evaluates an effect, a waveform or a clock.

Positions are **zero-based** transport coordinates, as in the scheduler's `:at`:
`{:bar 64 :beat 2}` is beat 258 in 4/4. Each event, ramp and section carries
its position, so a display does not turn beats into bars itself.

## What the projection contains

```clojure
{:version 1
 :beat 130.25 :bpm 124.0 :playing? true :time-ms 63024.2 :epoch 0
 :position {:bar 32 :beat 2.25 :phrase 8}
 :meter {:beats-per-bar 4 :bars-per-phrase 4}
 :window {:bars 8 :past-beats 32.0 :future-beats 32.0 :from-beat 92.0 :to-beat 168.0}
 :grid {:lines [{:beat 128.0 :kind :phrase :bar 32 :phrase 8}
                {:beat 129.0 :kind :beat}
                {:beat 132.0 :kind :bar :bar 33} ...]}
 :events [{:event-id "wash" :state :fired :beat 128.0 :due-beat 128.0 :at-beat 128.0
           :op :cue/launch :category :cue :title "Wash in" :position {:bar 32 :beat 0.0 :phrase 8}
           :source {:adapter :agent :session-id "run-7"} :cancellable? false ...}
          {:event-id "drop" :state :pending :beat 144.0 :origin :scheduled :cancellable? true ...}]
 :ramps [{:event-id "swell" :state :running :start-beat 128.0 :end-beat 160.0 :unit :beats
          :from 0.2 :to 0.8 :curve :ease-in-out :shape [0.0 0.0078 0.0313 ... 1.0]
          :destination {:target :param :instance-id "0:3" :param :level} :cancellable? true ...}]
 :sections {:items [{:source :causal :state :confirmed :start-beat 112.0 :open? true
                     :label "Bass change" :confidence 0.76 ...}
                    {:source :reference :state :predicted :start-beat 144.0 :end-beat 176.0
                     :label "B" :confidence 0.7 ...}]
            :match {:status :locked :reference {:id ... :version ...} :confirmations 3}}
 :audio {:available? true :timing-valid? true :column-ms 60.0
         :legend [:start-beat :end-beat :min :max :rms :low :mid :high]
         :columns {1050 [130.12 130.24 -0.41 0.44 0.17 0.16 0.05 0.02] ...}
         :reference {:available? false :reason :no-reference-waveform}}
 :meters [{:id :rms :label "RMS" :available? true :value 0.17 :peak 0.44 :dbfs -15.1}
          {:id :energy-now :label "Now" :available? true :value 0.53 :relative 1.06 :class :typical}
          {:id :audio/energy :label "Energy" :available? true :value 0.21}
          {:id :brightness :label "Light" :available? true :value 0.29 ...}
          {:id :activity :label "Activity" :available? true :value 0.07 ...}]
 :feel nil}
```

**Window.** `:past-beats` and `:future-beats` are the visible span. The listed
events, ramps, sections and grid lines cover that span plus one bar on each
side, snapped to bars, so those lists change at a bar crossing or when the
schedule itself changes. `:beat`, `:time-ms`, the meters and the newest audio
columns change every frame.

**Grid.** Beat, bar and phrase lines counted from beat zero in the transport
meter, the origin the scheduler uses. A 7/8 bar is 3.5 quarter-note beats. Above
16 bars a side the projection lists bars and phrases only.

**Events.** Each entry of the [schedule projection](scheduler.md#projection)
appears once. A pending event sits at its due beat. An outcome (`:fired`,
`:rejected`, `:missed` or `:cancelled`) sits at the beat it happened at.
Pending subscriber steps are listed with `:origin :subscriber` and
`:cancellable? false`, because `:schedule/cancel` matches scheduled events.

**Ramps.** A ramp is a span, not a point. A scheduled `:param/ramp` is
`:pending` from its due beat for its planned length. A running ramp spans the
beats the owner started it at and will end it at. A finished ramp stays as
`:completed` or `:cancelled`, with `:stopped-beat` where it was stopped.
`:shape` is 17 samples of the owner's easing function at even progress, so a
display draws the same curve without its own easing code. Progress needs no
field: now lies inside the span at the owner's progress. An `:ms` ramp runs on
the owner's millisecond clock. Its beats are placed at the current tempo and
re-projected every frame, and a running one also carries `:remaining-ms`.

**Sections.** Confirmed changes from the causal section analyzer are kept as a
bounded history in `[:variables :audio :sections]`. Each runs to the next
boundary, and the newest is `:open?`. When a section reference is attached and
its lock holds, the matched recording's sections are placed through the lock's
position and rate and the transport tempo: `:matched` behind now and
`:predicted` ahead. `:match` reports the lock status. When the lock breaks, the
reference sections are gone on the next frame.

**Audio.** Captured columns from the capture worker's 30-second history, each
placed by the beats the transport reported while that audio arrived. Columns are
keyed by a stable number, so a state-stream delta carries only new audio. A
stopped transport gives new audio no beat, and it is not placed. Capture with
invalid timing is reported and not placed.

**Meters.** `:rms` and `:energy-now` come from the audio analyzer's latest
features, `:audio/energy` is the public energy signal, and `:brightness` and
`:activity` are the light and activity budgets: the conductor's rolling
statistics and targets when it runs, otherwise the measured scene. Audio
features older than two seconds are `:available? false` with no value.

**Feel.** The active [track-feel personality](personalities.md): the
`:active` entry of `(:personalities (show/snapshot runtime))`, `nil` when no
feel is chosen. Select one in Perform's **Track feel** row or with
`(show/submit! runtime :personality {:op :select :id :squares})`, and the slot
follows on the next frame. It reads that entry as data: `:id`, `:name` (or a
`:label`), the icon as SVG text under `:icon` (or `:icon-svg`, or a data URI
under `:icon` / `:icon-url`), and the palette. Palette entries become CSS
colours here, not in the browser: `[r g b]` in 0..1 becomes `#rrggbb`, a
gradient or split shows its first colour, and a string passes through only as
colour text (hex or a colour function). An image URL must be a data URI, so
the slot never makes Studio fetch anything. Other keys (forms, motion,
concurrency) are dropped, and nothing drawable gives `nil`.

## The Studio panel

Open **Perform**. The **Timeline** panel sits above the cue grid and the Room.

- **Lanes**, top to bottom: **Events**, **Audio**, **Sections** and **Grid**.
  Select a lane's name to collapse it.
- **Zoom** with **+** and **−** in the panel header, from ±1 to ±32 bars. The
  default is ±8.
- **Events** are icons coloured by action category. Overlapping events take
  separate lines, up to six. When those are full (a dense stretch, a narrow
  screen), an event drops its caption and may cover a neighbour's caption,
  which is shortened; touch targets and ramp bars do not overlap. Only what
  still does not fit shares the last line. Fired events are dimmed, a rejected
  event is red and a cancelled or missed one is dashed.
- **Ramps** are bars from start to end with the easing curve inside. The part
  left of now is filled: that is the ramp's progress.
- Select an event or ramp to see what it does, where it sits, who posted it and
  its outcome. **Cancel event** and **Stop ramp** submit
  `{:op :schedule/cancel :event-ids [id]}`, the command behind
  `(show/cancel-events! runtime {:event-ids [id]})`.
- **Meters** stand beside the lanes, with the feel slot above them.

Studio only maps beats to pixels. Between committed frames it moves the
playhead in a straight line from where it is to the latest committed beat, over
one frame period, as the Room does for fixture values. It never runs ahead of a
committed sample. A pause, seek, loop or new show epoch snaps. Each lane
renders its items together with the slide that puts the playhead under the
centre line, so a zoom or resize never paints new positions under the old
slide. After a zoom the previous window stays on screen until the first sample
of the new one arrives. The playhead holds at the last sample meanwhile, so an
event that has fired is never drawn as scheduled behind now.

## Check it with the synthetic show

```sh
npm run studio:build
lein run -m tolglow.qa.timeline 4191 target/timeline-qa/control
node dev/studio/timeline-browser-qa.mjs http://127.0.0.1:4191 target/timeline-qa/out target/timeline-qa/control
```

The recording step uses Chrome's screencast and the system ffmpeg
(`/opt/homebrew/bin/ffmpeg`, or `FFMPEG_PATH`) to write `timeline.mp4`.

`tolglow.qa.timeline` is a preview-only show. It runs the real owner,
scheduler, capture worker and section analyzer on generated PCM that follows
the transport, posts a phrase of events and ramps every eight bars and simulates
a recording match. The simulated recording lasts four hours at 120 BPM. Its
matches carry that end as `:until-position-ms`. After the end the lock reports
"No match · playback-boundary" and no section is predicted, so the browser
check needs a host younger than that. The browser script asserts the layout and behaviour in
headless Chrome and writes stills, a recording and `report.json`. Create
`stop` in the control directory to end the show. Headless desktop Chrome is not
evidence for a foreground display or for iPad Safari.

## Remaining gaps

- **Matched-recording waveform.** The track profile has sections and beats but
  no waveform summary, so there is nothing to draw ahead of now.
  `[:audio :reference]` reports `:no-reference-waveform`, and
  `timeline/view` takes a `:reference-waveform` input that nothing supplies yet.
- **Match confidence.** The reference lock keeps a status and a confirmation
  count, and drops the match score. The lane shows the status.
- **After a loop or backward seek**, recent outcomes keep the beats they
  happened at. Those beats are then ahead of the transport and the timeline
  leaves them out. Audio is placed from the new position only.
- **Authoring and moving** events on the timeline is not built. Post and move
  them from the REPL, an agent or a reactive action.
- The zoom, collapsed lanes and selection are not saved with the UI preferences.
- Push does not draw the timeline. Its pages can read the same projection.
