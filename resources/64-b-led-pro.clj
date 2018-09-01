(ns afterglow.fixtures.american-dj
  "Translated definition for the fixture 64B LED PRO
  from American DJ.

  This was created by Afterglow from the QLC+ Fixture Definintion
  (.qxf) file, and will almost certainly need some manual adjustment
  in order to enable full Afterglow capabilities.

  If you have more than one fixture definition for this manufacturer,
  you can consolidate them into a single file if you like, with a
  single copy of this namespace definition, since it is the same for
  all fixture definitions translated by Afterglow.

  Once you have completed the fixture definition, and are happy with
  the way everything is being controlled by Afterglow, please consider
  submitting it for inclusion with Afterglow, either as a Pull Request
  at https://github.com/brunchboy/afterglow/pulls if you are
  comfortable putting that together, or just on the Gitter chat if
  that's easier for you: https://gitter.im/brunchboy/afterglow

  The original fixture defintition was created by John
  using Q Light Controller Plus version 4.3.2.
  QLC+ Fixture Type: Color Changer."
  (:require [afterglow.channels :as chan]
            [afterglow.effects.channel :as chan-fx]))

(defn 64-b-led-pro
  "64B LED PRO.

  Please flesh out this documentation if you are submitting this for
  inclusion into Afterglow. See, for example, the Blizzard fixture
  definitions:
  http://cdn.rawgit.com/brunchboy/afterglow/master/api-doc/afterglow.fixtures.blizzard.html"
  ([]
   (64-b-led-pro :7-channel))
  ([mode]
   (merge {:name "64B LED PRO"
           :mode mode}
          (case mode
                :7-channel
                {:channels [(chan/color 1 :red)
                            (chan/color 2 :green)
                            (chan/color 3 :blue)
                            (chan/functions :colour-macros 4
                              0 {:type :colour-macros-no-macro
                                 :label "No Macro"
                                 :range :variable}
                              8 {:type :colour-macros-macro-1
                                 :label "Macro 1"
                                 :range :variable}
                              16 {:type :colour-macros-macro-2
                                  :label "Macro 2"
                                  :range :variable}
                              24 {:type :colour-macros-macro-3
                                  :label "Macro 3"
                                  :range :variable}
                              32 {:type :colour-macros-macro-4
                                  :label "Macro 4"
                                  :range :variable}
                              40 {:type :colour-macros-macro-5
                                  :label "Macro 5"
                                  :range :variable}
                              48 {:type :colour-macros-macro-6
                                  :label "Macro 6"
                                  :range :variable}
                              56 {:type :colour-macros-macro-7
                                  :label "Macro 7"
                                  :range :variable}
                              64 {:type :colour-macros-macro-8
                                  :label "Macro 8"
                                  :range :variable}
                              72 {:type :colour-macros-macro-9
                                  :label "Macro 9"
                                  :range :variable}
                              80 {:type :colour-macros-macro-10
                                  :label "Macro 10"
                                  :range :variable}
                              88 {:type :colour-macros-macro-11
                                  :label "Macro 11"
                                  :range :variable}
                              96 {:type :colour-macros-macro-12
                                  :label "Macro 12"
                                  :range :variable}
                              104 {:type :colour-macros-macro-13
                                   :label "Macro 13"
                                   :range :variable}
                              112 {:type :colour-macros-macro-14
                                   :label "Macro 14"
                                   :range :variable}
                              120 {:type :colour-macros-macro-15
                                   :label "Macro 15"
                                   :range :variable}
                              128 {:type :colour-macros-macro-16
                                   :label "Macro 16"
                                   :range :variable}
                              136 {:type :colour-macros-macro-17
                                   :label "Macro 17"
                                   :range :variable}
                              144 {:type :colour-macros-macro-18
                                   :label "Macro 18"
                                   :range :variable}
                              152 {:type :colour-macros-macro-19
                                   :label "Macro 19"
                                   :range :variable}
                              160 {:type :colour-macros-macro-20
                                   :label "Macro 20"
                                   :range :variable}
                              168 {:type :colour-macros-macro-21
                                   :label "Macro 21"
                                   :range :variable}
                              176 {:type :colour-macros-macro-22
                                   :label "Macro 22"
                                   :range :variable}
                              184 {:type :colour-macros-macro-23
                                   :label "Macro 23"
                                   :range :variable}
                              192 {:type :colour-macros-macro-24
                                   :label "Macro 24"
                                   :range :variable}
                              200 {:type :colour-macros-macro-25
                                   :label "Macro 25"
                                   :range :variable}
                              208 {:type :colour-macros-macro-26
                                   :label "Macro 26"
                                   :range :variable}
                              216 {:type :colour-macros-macro-27
                                   :label "Macro 27"
                                   :range :variable}
                              224 {:type :colour-macros-macro-28
                                   :label "Macro 28"
                                   :range :variable}
                              232 {:type :colour-macros-macro-29
                                   :label "Macro 29"
                                   :range :variable}
                              240 {:type :colour-macros-macro-30
                                   :label "Macro 30"
                                   :range :variable}
                              248 {:type :colour-macros-macro-31
                                   :label "Macro 31"
                                   :range :variable})
                            (chan/functions :strobe-speed 5
                              0 {:type :strobe-speed-no-strobe
                                 :label "No strobe"
                                 :range :variable}
                              16 {:type :strobe-speed-strobe-macro-speed
                                  :label "Strobe/Macro Speed"
                                  :range :variable})
                            (chan/functions :slow-fast 6
                              0 {:type :slow-fast-nothing
                                 :label "Nothing"
                                 :range :variable}
                              32 {:type :slow-fast-dim-bright
                                  :label "Dim > Bright"
                                  :range :variable}
                              64 {:type :slow-fast-bright-dim
                                  :label "Bright > Dim"
                                  :range :variable}
                              96 {:type :slow-fast-dim-bright-dim
                                  :label "Dim > Bright > Dim"
                                  :range :variable}
                              128 {:type :slow-fast-colour-mixing
                                   :label "Colour Mixing"
                                   :range :variable}
                              160 {:type :slow-fast-3-colour-change
                                   :label "3 Colour Change"
                                   :range :variable}
                              192 {:type :slow-fast-7-colour-change
                                   :label "7 Colour Change"
                                   :range :variable}
                              224 {:type :slow-fast-sound-active
                                   :label "Sound Active"
                                   :range :variable})
                            (chan/dimmer 7)]}
                :6-channel
                {:channels [(chan/color 1 :red)
                            (chan/color 2 :green)
                            (chan/color 3 :blue)
                            (chan/functions :colour-macros 4
                              0 {:type :colour-macros-no-macro
                                 :label "No Macro"
                                 :range :variable}
                              8 {:type :colour-macros-macro-1
                                 :label "Macro 1"
                                 :range :variable}
                              16 {:type :colour-macros-macro-2
                                  :label "Macro 2"
                                  :range :variable}
                              24 {:type :colour-macros-macro-3
                                  :label "Macro 3"
                                  :range :variable}
                              32 {:type :colour-macros-macro-4
                                  :label "Macro 4"
                                  :range :variable}
                              40 {:type :colour-macros-macro-5
                                  :label "Macro 5"
                                  :range :variable}
                              48 {:type :colour-macros-macro-6
                                  :label "Macro 6"
                                  :range :variable}
                              56 {:type :colour-macros-macro-7
                                  :label "Macro 7"
                                  :range :variable}
                              64 {:type :colour-macros-macro-8
                                  :label "Macro 8"
                                  :range :variable}
                              72 {:type :colour-macros-macro-9
                                  :label "Macro 9"
                                  :range :variable}
                              80 {:type :colour-macros-macro-10
                                  :label "Macro 10"
                                  :range :variable}
                              88 {:type :colour-macros-macro-11
                                  :label "Macro 11"
                                  :range :variable}
                              96 {:type :colour-macros-macro-12
                                  :label "Macro 12"
                                  :range :variable}
                              104 {:type :colour-macros-macro-13
                                   :label "Macro 13"
                                   :range :variable}
                              112 {:type :colour-macros-macro-14
                                   :label "Macro 14"
                                   :range :variable}
                              120 {:type :colour-macros-macro-15
                                   :label "Macro 15"
                                   :range :variable}
                              128 {:type :colour-macros-macro-16
                                   :label "Macro 16"
                                   :range :variable}
                              136 {:type :colour-macros-macro-17
                                   :label "Macro 17"
                                   :range :variable}
                              144 {:type :colour-macros-macro-18
                                   :label "Macro 18"
                                   :range :variable}
                              152 {:type :colour-macros-macro-19
                                   :label "Macro 19"
                                   :range :variable}
                              160 {:type :colour-macros-macro-20
                                   :label "Macro 20"
                                   :range :variable}
                              168 {:type :colour-macros-macro-21
                                   :label "Macro 21"
                                   :range :variable}
                              176 {:type :colour-macros-macro-22
                                   :label "Macro 22"
                                   :range :variable}
                              184 {:type :colour-macros-macro-23
                                   :label "Macro 23"
                                   :range :variable}
                              192 {:type :colour-macros-macro-24
                                   :label "Macro 24"
                                   :range :variable}
                              200 {:type :colour-macros-macro-25
                                   :label "Macro 25"
                                   :range :variable}
                              208 {:type :colour-macros-macro-26
                                   :label "Macro 26"
                                   :range :variable}
                              216 {:type :colour-macros-macro-27
                                   :label "Macro 27"
                                   :range :variable}
                              224 {:type :colour-macros-macro-28
                                   :label "Macro 28"
                                   :range :variable}
                              232 {:type :colour-macros-macro-29
                                   :label "Macro 29"
                                   :range :variable}
                              240 {:type :colour-macros-macro-30
                                   :label "Macro 30"
                                   :range :variable}
                              248 {:type :colour-macros-macro-31
                                   :label "Macro 31"
                                   :range :variable})
                            (chan/functions :strobe-speed 5
                              0 {:type :strobe-speed-no-strobe
                                 :label "No strobe"
                                 :range :variable}
                              16 {:type :strobe-speed-strobe-macro-speed
                                  :label "Strobe/Macro Speed"
                                  :range :variable})
                            (chan/functions :slow-fast 6
                              0 {:type :slow-fast-nothing
                                 :label "Nothing"
                                 :range :variable}
                              32 {:type :slow-fast-dim-bright
                                  :label "Dim > Bright"
                                  :range :variable}
                              64 {:type :slow-fast-bright-dim
                                  :label "Bright > Dim"
                                  :range :variable}
                              96 {:type :slow-fast-dim-bright-dim
                                  :label "Dim > Bright > Dim"
                                  :range :variable}
                              128 {:type :slow-fast-colour-mixing
                                   :label "Colour Mixing"
                                   :range :variable}
                              160 {:type :slow-fast-3-colour-change
                                   :label "3 Colour Change"
                                   :range :variable}
                              192 {:type :slow-fast-7-colour-change
                                   :label "7 Colour Change"
                                   :range :variable}
                              224 {:type :slow-fast-sound-active
                                   :label "Sound Active"
                                   :range :variable})]}
                :3-channel
                {:channels [(chan/color 1 :red)
                            (chan/color 2 :green)
                            (chan/color 3 :blue)]}))))
