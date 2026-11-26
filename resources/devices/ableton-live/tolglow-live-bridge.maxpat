{
  "patcher": {
    "fileversion": 1,
    "appversion": {
      "major": 8,
      "minor": 6,
      "revision": 0,
      "architecture": "x64",
      "modernui": 1
    },
    "classnamespace": "box",
    "rect": [
      50,
      50,
      980,
      690
    ],
    "openinpresentation": 1,
    "default_fontsize": 12.0,
    "default_fontface": 0,
    "default_fontname": "Arial",
    "boxes": [
      {
        "box": {
          "id": "title",
          "maxclass": "comment",
          "patching_rect": [
            20,
            15,
            760,
            30
          ],
          "text": "TOLGLOW · Live bridge source patch · Live 11+ / Max for Live",
          "presentation": 1,
          "presentation_rect": [
            10,
            8,
            580,
            25
          ],
          "fontsize": 16
        }
      },
      {
        "box": {
          "id": "instructions",
          "maxclass": "comment",
          "patching_rect": [
            20,
            48,
            880,
            43
          ],
          "text": "Place before an existing Drum Rack. Indices are zero-based. Pair an existing MIDI clip, then start the bridge. Mode changes are acknowledged by the studio. Default MIDI pass-through is Live.",
          "linecount": 3,
          "presentation": 1,
          "presentation_rect": [
            10,
            38,
            580,
            43
          ]
        }
      },
      {
        "box": {
          "id": "start-toggle",
          "maxclass": "toggle",
          "patching_rect": [
            20,
            110,
            24,
            24
          ],
          "presentation": 1,
          "presentation_rect": [
            10,
            90,
            24,
            24
          ],
          "parameter_enable": 0
        }
      },
      {
        "box": {
          "id": "start-label",
          "maxclass": "comment",
          "patching_rect": [
            50,
            110,
            160,
            22
          ],
          "text": "Bridge on / off",
          "presentation": 1,
          "presentation_rect": [
            42,
            90,
            140,
            22
          ]
        }
      },
      {
        "box": {
          "id": "start-select",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            150,
            70,
            22
          ],
          "text": "sel 1 0",
          "numinlets": 1,
          "numoutlets": 3
        }
      },
      {
        "box": {
          "id": "start-message",
          "maxclass": "message",
          "patching_rect": [
            20,
            185,
            430,
            22
          ],
          "text": "start 8750 http://127.0.0.1:8500/api/live tolglow-m4l"
        }
      },
      {
        "box": {
          "id": "stop-message",
          "maxclass": "message",
          "patching_rect": [
            465,
            185,
            45,
            22
          ],
          "text": "stop"
        }
      },
      {
        "box": {
          "id": "node",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            230,
            285,
            22
          ],
          "text": "node.script node_bridge.js @autostart 1",
          "numinlets": 1,
          "numoutlets": 2
        }
      },
      {
        "box": {
          "id": "node-route",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            270,
            290,
            22
          ],
          "text": "route request ready diagnostic stopped",
          "numinlets": 1,
          "numoutlets": 5
        }
      },
      {
        "box": {
          "id": "prepend-request",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            310,
            120,
            22
          ],
          "text": "prepend request",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "low",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            350,
            70,
            22
          ],
          "text": "deferlow",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "max-js",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            390,
            150,
            22
          ],
          "text": "js max_adapter.js",
          "numinlets": 1,
          "numoutlets": 2
        }
      },
      {
        "box": {
          "id": "ui-route",
          "maxclass": "newobj",
          "patching_rect": [
            190,
            390,
            215,
            22
          ],
          "text": "route thru status diagnostic",
          "numinlets": 1,
          "numoutlets": 4
        }
      },
      {
        "box": {
          "id": "log",
          "maxclass": "newobj",
          "patching_rect": [
            420,
            430,
            135,
            22
          ],
          "text": "print tolglow.live",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "init",
          "maxclass": "newobj",
          "patching_rect": [
            330,
            230,
            100,
            22
          ],
          "text": "live.thisdevice",
          "numinlets": 1,
          "numoutlets": 3
        }
      },
      {
        "box": {
          "id": "init-msg",
          "maxclass": "message",
          "patching_rect": [
            330,
            270,
            40,
            22
          ],
          "text": "init"
        }
      },
      {
        "box": {
          "id": "metro",
          "maxclass": "newobj",
          "patching_rect": [
            530,
            185,
            90,
            22
          ],
          "text": "qmetro 100",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "poll-msg",
          "maxclass": "message",
          "patching_rect": [
            530,
            230,
            40,
            22
          ],
          "text": "poll"
        }
      },
      {
        "box": {
          "id": "raw-in",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            470,
            60,
            22
          ],
          "text": "midiin",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "gate",
          "maxclass": "newobj",
          "patching_rect": [
            110,
            470,
            65,
            22
          ],
          "text": "gate 1 1",
          "numinlets": 2,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "raw-out",
          "maxclass": "newobj",
          "patching_rect": [
            205,
            470,
            65,
            22
          ],
          "text": "midiout",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "notein",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            535,
            60,
            22
          ],
          "text": "notein",
          "numinlets": 1,
          "numoutlets": 3
        }
      },
      {
        "box": {
          "id": "note-pack",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            575,
            95,
            22
          ],
          "text": "pack 0 0 1",
          "numinlets": 3,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "note-prepend",
          "maxclass": "newobj",
          "patching_rect": [
            20,
            615,
            100,
            22
          ],
          "text": "prepend note",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "ctlin",
          "maxclass": "newobj",
          "patching_rect": [
            190,
            535,
            60,
            22
          ],
          "text": "ctlin",
          "numinlets": 1,
          "numoutlets": 3
        }
      },
      {
        "box": {
          "id": "cc-pack",
          "maxclass": "newobj",
          "patching_rect": [
            190,
            575,
            95,
            22
          ],
          "text": "pack 0 0 1",
          "numinlets": 3,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "cc-prepend",
          "maxclass": "newobj",
          "patching_rect": [
            190,
            615,
            95,
            22
          ],
          "text": "prepend cc",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "track",
          "maxclass": "number",
          "patching_rect": [
            650,
            150,
            65,
            22
          ],
          "minimum": 0,
          "maximum": 9999,
          "parameter_enable": 0,
          "presentation": 1,
          "presentation_rect": [
            190,
            112,
            65,
            22
          ]
        }
      },
      {
        "box": {
          "id": "track-label",
          "maxclass": "comment",
          "patching_rect": [
            650,
            115,
            110,
            22
          ],
          "text": "Track index",
          "presentation": 1,
          "presentation_rect": [
            190,
            90,
            115,
            22
          ]
        }
      },
      {
        "box": {
          "id": "slot",
          "maxclass": "number",
          "patching_rect": [
            750,
            150,
            65,
            22
          ],
          "minimum": 0,
          "maximum": 9999,
          "parameter_enable": 0,
          "presentation": 1,
          "presentation_rect": [
            290,
            112,
            65,
            22
          ]
        }
      },
      {
        "box": {
          "id": "slot-label",
          "maxclass": "comment",
          "patching_rect": [
            750,
            115,
            110,
            22
          ],
          "text": "Clip-slot index",
          "presentation": 1,
          "presentation_rect": [
            290,
            90,
            115,
            22
          ]
        }
      },
      {
        "box": {
          "id": "rack",
          "maxclass": "number",
          "patching_rect": [
            850,
            150,
            65,
            22
          ],
          "minimum": 0,
          "maximum": 9999,
          "parameter_enable": 0,
          "presentation": 1,
          "presentation_rect": [
            390,
            112,
            65,
            22
          ]
        }
      },
      {
        "box": {
          "id": "rack-label",
          "maxclass": "comment",
          "patching_rect": [
            850,
            115,
            110,
            22
          ],
          "text": "Drum Rack index",
          "presentation": 1,
          "presentation_rect": [
            390,
            90,
            115,
            22
          ]
        }
      },
      {
        "box": {
          "id": "pair-button",
          "maxclass": "button",
          "patching_rect": [
            650,
            230,
            24,
            24
          ],
          "presentation": 1,
          "presentation_rect": [
            500,
            110,
            24,
            24
          ]
        }
      },
      {
        "box": {
          "id": "pair-label",
          "maxclass": "comment",
          "patching_rect": [
            680,
            230,
            160,
            22
          ],
          "text": "Pair destination",
          "presentation": 1,
          "presentation_rect": [
            530,
            110,
            130,
            22
          ]
        }
      },
      {
        "box": {
          "id": "track-store",
          "maxclass": "newobj",
          "patching_rect": [
            650,
            270,
            50,
            22
          ],
          "text": "int 0",
          "numinlets": 2,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "pair-pack",
          "maxclass": "newobj",
          "patching_rect": [
            650,
            310,
            95,
            22
          ],
          "text": "pack 0 0 1",
          "numinlets": 3,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "pair-prepend",
          "maxclass": "newobj",
          "patching_rect": [
            650,
            350,
            100,
            22
          ],
          "text": "prepend pair",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "rack-default",
          "maxclass": "newobj",
          "patching_rect": [
            850,
            185,
            90,
            22
          ],
          "text": "loadmess 1",
          "numinlets": 1,
          "numoutlets": 1
        }
      },
      {
        "box": {
          "id": "mode-lighting",
          "maxclass": "message",
          "patching_rect": [
            650,
            430,
            100,
            22
          ],
          "text": "mode lighting",
          "presentation": 1,
          "presentation_rect": [
            190,
            155,
            100,
            24
          ]
        }
      },
      {
        "box": {
          "id": "mode-live",
          "maxclass": "message",
          "patching_rect": [
            770,
            430,
            80,
            22
          ],
          "text": "mode live",
          "presentation": 1,
          "presentation_rect": [
            305,
            155,
            80,
            24
          ]
        }
      },
      {
        "box": {
          "id": "mode-note",
          "maxclass": "comment",
          "patching_rect": [
            650,
            465,
            290,
            50
          ],
          "text": "Lighting consumes MIDI into common cue commands. Live passes MIDI onward. Acknowledgement controls the MIDI-through gate.",
          "linecount": 3,
          "presentation": 1,
          "presentation_rect": [
            10,
            190,
            580,
            42
          ]
        }
      },
      {
        "box": {
          "id": "record-note",
          "maxclass": "comment",
          "patching_rect": [
            350,
            545,
            580,
            70
          ],
          "text": "Recording route: explicit Clip note API only. CC envelope writing is unsupported here; use the separate native MIDI route if needed. Clip/chain colors use the nearest Live palette. Empty pads are reported, not populated automatically.",
          "linecount": 4,
          "presentation": 1,
          "presentation_rect": [
            10,
            240,
            620,
            60
          ]
        }
      },
      {
        "box": {
          "id": "stopped-message",
          "maxclass": "message",
          "text": "stopped",
          "patching_rect": [
            320,
            270,
            56,
            22
          ]
        }
      },
      {
        "box": {
          "id": "lane-comment",
          "maxclass": "comment",
          "numinlets": 1,
          "numoutlets": 0,
          "patching_rect": [
            620,
            20,
            420,
            48
          ],
          "text": "Tolglow lanes 1-30: automatable parameters for captured controller lanes. Clip envelopes on these play back as coarse/fine CC pairs (lane k = CC n / n+32)."
        }
      },
      {
        "box": {
          "id": "lane-dial-1",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            620,
            80,
            40,
            48
          ],
          "varname": "lane-1",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 1",
              "parameter_shortname": "Lane 1",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-1",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            620,
            134,
            40,
            22
          ],
          "text": "prepend lane 1"
        }
      },
      {
        "box": {
          "id": "lane-dial-2",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            664,
            80,
            40,
            48
          ],
          "varname": "lane-2",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 2",
              "parameter_shortname": "Lane 2",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-2",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            664,
            134,
            40,
            22
          ],
          "text": "prepend lane 2"
        }
      },
      {
        "box": {
          "id": "lane-dial-3",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            708,
            80,
            40,
            48
          ],
          "varname": "lane-3",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 3",
              "parameter_shortname": "Lane 3",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-3",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            708,
            134,
            40,
            22
          ],
          "text": "prepend lane 3"
        }
      },
      {
        "box": {
          "id": "lane-dial-4",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            752,
            80,
            40,
            48
          ],
          "varname": "lane-4",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 4",
              "parameter_shortname": "Lane 4",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-4",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            752,
            134,
            40,
            22
          ],
          "text": "prepend lane 4"
        }
      },
      {
        "box": {
          "id": "lane-dial-5",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            796,
            80,
            40,
            48
          ],
          "varname": "lane-5",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 5",
              "parameter_shortname": "Lane 5",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-5",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            796,
            134,
            40,
            22
          ],
          "text": "prepend lane 5"
        }
      },
      {
        "box": {
          "id": "lane-dial-6",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            840,
            80,
            40,
            48
          ],
          "varname": "lane-6",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 6",
              "parameter_shortname": "Lane 6",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-6",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            840,
            134,
            40,
            22
          ],
          "text": "prepend lane 6"
        }
      },
      {
        "box": {
          "id": "lane-dial-7",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            884,
            80,
            40,
            48
          ],
          "varname": "lane-7",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 7",
              "parameter_shortname": "Lane 7",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-7",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            884,
            134,
            40,
            22
          ],
          "text": "prepend lane 7"
        }
      },
      {
        "box": {
          "id": "lane-dial-8",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            928,
            80,
            40,
            48
          ],
          "varname": "lane-8",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 8",
              "parameter_shortname": "Lane 8",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-8",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            928,
            134,
            40,
            22
          ],
          "text": "prepend lane 8"
        }
      },
      {
        "box": {
          "id": "lane-dial-9",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            972,
            80,
            40,
            48
          ],
          "varname": "lane-9",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 9",
              "parameter_shortname": "Lane 9",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-9",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            972,
            134,
            40,
            22
          ],
          "text": "prepend lane 9"
        }
      },
      {
        "box": {
          "id": "lane-dial-10",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            1016,
            80,
            40,
            48
          ],
          "varname": "lane-10",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 10",
              "parameter_shortname": "Lane 10",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-10",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            1016,
            134,
            40,
            22
          ],
          "text": "prepend lane 10"
        }
      },
      {
        "box": {
          "id": "lane-dial-11",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            620,
            170,
            40,
            48
          ],
          "varname": "lane-11",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 11",
              "parameter_shortname": "Lane 11",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-11",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            620,
            224,
            40,
            22
          ],
          "text": "prepend lane 11"
        }
      },
      {
        "box": {
          "id": "lane-dial-12",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            664,
            170,
            40,
            48
          ],
          "varname": "lane-12",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 12",
              "parameter_shortname": "Lane 12",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-12",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            664,
            224,
            40,
            22
          ],
          "text": "prepend lane 12"
        }
      },
      {
        "box": {
          "id": "lane-dial-13",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            708,
            170,
            40,
            48
          ],
          "varname": "lane-13",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 13",
              "parameter_shortname": "Lane 13",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-13",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            708,
            224,
            40,
            22
          ],
          "text": "prepend lane 13"
        }
      },
      {
        "box": {
          "id": "lane-dial-14",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            752,
            170,
            40,
            48
          ],
          "varname": "lane-14",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 14",
              "parameter_shortname": "Lane 14",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-14",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            752,
            224,
            40,
            22
          ],
          "text": "prepend lane 14"
        }
      },
      {
        "box": {
          "id": "lane-dial-15",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            796,
            170,
            40,
            48
          ],
          "varname": "lane-15",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 15",
              "parameter_shortname": "Lane 15",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-15",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            796,
            224,
            40,
            22
          ],
          "text": "prepend lane 15"
        }
      },
      {
        "box": {
          "id": "lane-dial-16",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            840,
            170,
            40,
            48
          ],
          "varname": "lane-16",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 16",
              "parameter_shortname": "Lane 16",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-16",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            840,
            224,
            40,
            22
          ],
          "text": "prepend lane 16"
        }
      },
      {
        "box": {
          "id": "lane-dial-17",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            884,
            170,
            40,
            48
          ],
          "varname": "lane-17",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 17",
              "parameter_shortname": "Lane 17",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-17",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            884,
            224,
            40,
            22
          ],
          "text": "prepend lane 17"
        }
      },
      {
        "box": {
          "id": "lane-dial-18",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            928,
            170,
            40,
            48
          ],
          "varname": "lane-18",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 18",
              "parameter_shortname": "Lane 18",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-18",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            928,
            224,
            40,
            22
          ],
          "text": "prepend lane 18"
        }
      },
      {
        "box": {
          "id": "lane-dial-19",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            972,
            170,
            40,
            48
          ],
          "varname": "lane-19",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 19",
              "parameter_shortname": "Lane 19",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-19",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            972,
            224,
            40,
            22
          ],
          "text": "prepend lane 19"
        }
      },
      {
        "box": {
          "id": "lane-dial-20",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            1016,
            170,
            40,
            48
          ],
          "varname": "lane-20",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 20",
              "parameter_shortname": "Lane 20",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-20",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            1016,
            224,
            40,
            22
          ],
          "text": "prepend lane 20"
        }
      },
      {
        "box": {
          "id": "lane-dial-21",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            620,
            260,
            40,
            48
          ],
          "varname": "lane-21",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 21",
              "parameter_shortname": "Lane 21",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-21",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            620,
            314,
            40,
            22
          ],
          "text": "prepend lane 21"
        }
      },
      {
        "box": {
          "id": "lane-dial-22",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            664,
            260,
            40,
            48
          ],
          "varname": "lane-22",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 22",
              "parameter_shortname": "Lane 22",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-22",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            664,
            314,
            40,
            22
          ],
          "text": "prepend lane 22"
        }
      },
      {
        "box": {
          "id": "lane-dial-23",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            708,
            260,
            40,
            48
          ],
          "varname": "lane-23",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 23",
              "parameter_shortname": "Lane 23",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-23",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            708,
            314,
            40,
            22
          ],
          "text": "prepend lane 23"
        }
      },
      {
        "box": {
          "id": "lane-dial-24",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            752,
            260,
            40,
            48
          ],
          "varname": "lane-24",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 24",
              "parameter_shortname": "Lane 24",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-24",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            752,
            314,
            40,
            22
          ],
          "text": "prepend lane 24"
        }
      },
      {
        "box": {
          "id": "lane-dial-25",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            796,
            260,
            40,
            48
          ],
          "varname": "lane-25",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 25",
              "parameter_shortname": "Lane 25",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-25",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            796,
            314,
            40,
            22
          ],
          "text": "prepend lane 25"
        }
      },
      {
        "box": {
          "id": "lane-dial-26",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            840,
            260,
            40,
            48
          ],
          "varname": "lane-26",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 26",
              "parameter_shortname": "Lane 26",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-26",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            840,
            314,
            40,
            22
          ],
          "text": "prepend lane 26"
        }
      },
      {
        "box": {
          "id": "lane-dial-27",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            884,
            260,
            40,
            48
          ],
          "varname": "lane-27",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 27",
              "parameter_shortname": "Lane 27",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-27",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            884,
            314,
            40,
            22
          ],
          "text": "prepend lane 27"
        }
      },
      {
        "box": {
          "id": "lane-dial-28",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            928,
            260,
            40,
            48
          ],
          "varname": "lane-28",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 28",
              "parameter_shortname": "Lane 28",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-28",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            928,
            314,
            40,
            22
          ],
          "text": "prepend lane 28"
        }
      },
      {
        "box": {
          "id": "lane-dial-29",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            972,
            260,
            40,
            48
          ],
          "varname": "lane-29",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 29",
              "parameter_shortname": "Lane 29",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-29",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            972,
            314,
            40,
            22
          ],
          "text": "prepend lane 29"
        }
      },
      {
        "box": {
          "id": "lane-dial-30",
          "maxclass": "live.dial",
          "numinlets": 1,
          "numoutlets": 2,
          "outlettype": [
            "",
            "float"
          ],
          "parameter_enable": 1,
          "patching_rect": [
            1016,
            260,
            40,
            48
          ],
          "varname": "lane-30",
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Tolglow lane 30",
              "parameter_shortname": "Lane 30",
              "parameter_type": 0,
              "parameter_mmin": 0.0,
              "parameter_mmax": 1.0,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                0.0
              ],
              "parameter_unitstyle": 1
            }
          }
        }
      },
      {
        "box": {
          "id": "lane-prepend-30",
          "maxclass": "newobj",
          "numinlets": 1,
          "numoutlets": 1,
          "outlettype": [
            ""
          ],
          "patching_rect": [
            1016,
            314,
            40,
            22
          ],
          "text": "prepend lane 30"
        }
      }
    ],
    "lines": [
      {
        "patchline": {
          "source": [
            "start-toggle",
            0
          ],
          "destination": [
            "start-select",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "start-toggle",
            0
          ],
          "destination": [
            "metro",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "start-select",
            0
          ],
          "destination": [
            "start-message",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "start-select",
            1
          ],
          "destination": [
            "stop-message",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "start-message",
            0
          ],
          "destination": [
            "node",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "stop-message",
            0
          ],
          "destination": [
            "node",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node",
            0
          ],
          "destination": [
            "node-route",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node-route",
            0
          ],
          "destination": [
            "prepend-request",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node-route",
            1
          ],
          "destination": [
            "log",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node-route",
            2
          ],
          "destination": [
            "log",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node-route",
            3
          ],
          "destination": [
            "log",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "prepend-request",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "low",
            0
          ],
          "destination": [
            "max-js",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "max-js",
            0
          ],
          "destination": [
            "node",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "max-js",
            1
          ],
          "destination": [
            "ui-route",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ui-route",
            0
          ],
          "destination": [
            "gate",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ui-route",
            1
          ],
          "destination": [
            "log",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ui-route",
            2
          ],
          "destination": [
            "log",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "init",
            0
          ],
          "destination": [
            "init-msg",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "init-msg",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "metro",
            0
          ],
          "destination": [
            "poll-msg",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "poll-msg",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "raw-in",
            0
          ],
          "destination": [
            "gate",
            1
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "gate",
            0
          ],
          "destination": [
            "raw-out",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "notein",
            0
          ],
          "destination": [
            "note-pack",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "notein",
            1
          ],
          "destination": [
            "note-pack",
            1
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "notein",
            2
          ],
          "destination": [
            "note-pack",
            2
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ctlin",
            0
          ],
          "destination": [
            "cc-pack",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ctlin",
            1
          ],
          "destination": [
            "cc-pack",
            1
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "ctlin",
            2
          ],
          "destination": [
            "cc-pack",
            2
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "note-pack",
            0
          ],
          "destination": [
            "note-prepend",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "note-prepend",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "cc-pack",
            0
          ],
          "destination": [
            "cc-prepend",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "cc-prepend",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "track",
            0
          ],
          "destination": [
            "track-store",
            1
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "pair-button",
            0
          ],
          "destination": [
            "track-store",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "track-store",
            0
          ],
          "destination": [
            "pair-pack",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "slot",
            0
          ],
          "destination": [
            "pair-pack",
            1
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "rack",
            0
          ],
          "destination": [
            "pair-pack",
            2
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "rack-default",
            0
          ],
          "destination": [
            "rack",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "pair-pack",
            0
          ],
          "destination": [
            "pair-prepend",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "pair-prepend",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "mode-lighting",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "mode-live",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "node-route",
            3
          ],
          "destination": [
            "stopped-message",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "stopped-message",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-1",
            0
          ],
          "destination": [
            "lane-prepend-1",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-1",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-2",
            0
          ],
          "destination": [
            "lane-prepend-2",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-2",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-3",
            0
          ],
          "destination": [
            "lane-prepend-3",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-3",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-4",
            0
          ],
          "destination": [
            "lane-prepend-4",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-4",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-5",
            0
          ],
          "destination": [
            "lane-prepend-5",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-5",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-6",
            0
          ],
          "destination": [
            "lane-prepend-6",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-6",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-7",
            0
          ],
          "destination": [
            "lane-prepend-7",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-7",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-8",
            0
          ],
          "destination": [
            "lane-prepend-8",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-8",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-9",
            0
          ],
          "destination": [
            "lane-prepend-9",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-9",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-10",
            0
          ],
          "destination": [
            "lane-prepend-10",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-10",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-11",
            0
          ],
          "destination": [
            "lane-prepend-11",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-11",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-12",
            0
          ],
          "destination": [
            "lane-prepend-12",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-12",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-13",
            0
          ],
          "destination": [
            "lane-prepend-13",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-13",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-14",
            0
          ],
          "destination": [
            "lane-prepend-14",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-14",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-15",
            0
          ],
          "destination": [
            "lane-prepend-15",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-15",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-16",
            0
          ],
          "destination": [
            "lane-prepend-16",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-16",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-17",
            0
          ],
          "destination": [
            "lane-prepend-17",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-17",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-18",
            0
          ],
          "destination": [
            "lane-prepend-18",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-18",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-19",
            0
          ],
          "destination": [
            "lane-prepend-19",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-19",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-20",
            0
          ],
          "destination": [
            "lane-prepend-20",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-20",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-21",
            0
          ],
          "destination": [
            "lane-prepend-21",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-21",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-22",
            0
          ],
          "destination": [
            "lane-prepend-22",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-22",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-23",
            0
          ],
          "destination": [
            "lane-prepend-23",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-23",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-24",
            0
          ],
          "destination": [
            "lane-prepend-24",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-24",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-25",
            0
          ],
          "destination": [
            "lane-prepend-25",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-25",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-26",
            0
          ],
          "destination": [
            "lane-prepend-26",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-26",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-27",
            0
          ],
          "destination": [
            "lane-prepend-27",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-27",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-28",
            0
          ],
          "destination": [
            "lane-prepend-28",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-28",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-29",
            0
          ],
          "destination": [
            "lane-prepend-29",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-29",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-dial-30",
            0
          ],
          "destination": [
            "lane-prepend-30",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "lane-prepend-30",
            0
          ],
          "destination": [
            "low",
            0
          ]
        }
      }
    ],
    "dependency_cache": [
      {
        "name": "max_adapter.js",
        "type": "TEXT",
        "implicit": 1
      },
      {
        "name": "live_api.js",
        "type": "TEXT",
        "implicit": 1
      },
      {
        "name": "parameter_observers.js",
        "type": "TEXT",
        "implicit": 1
      },
      {
        "name": "node_bridge.js",
        "type": "TEXT",
        "implicit": 1
      }
    ]
  }
}
