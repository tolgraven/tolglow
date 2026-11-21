{
  "patcher": {
    "fileversion": 1,
    "appversion": {
      "major": 8,
      "minor": 6,
      "revision": 5,
      "architecture": "x64",
      "modernui": 1
    },
    "rect": [
      20,
      80,
      420,
      360
    ],
    "openinpresentation": 1,
    "boxes": [
      {
        "box": {
          "id": "label",
          "maxclass": "comment",
          "text": "Audio bus",
          "patching_rect": [
            18,
            12,
            90,
            22
          ],
          "presentation": 1,
          "presentation_rect": [
            4,
            4,
            72,
            22
          ]
        }
      },
      {
        "box": {
          "id": "bus",
          "maxclass": "live.numbox",
          "parameter_enable": 1,
          "varname": "audio_bus",
          "patching_rect": [
            16,
            64,
            75,
            22
          ],
          "presentation": 1,
          "presentation_rect": [
            80,
            4,
            65,
            22
          ],
          "saved_attribute_attributes": {
            "valueof": {
              "parameter_longname": "Audio bus",
              "parameter_shortname": "Bus",
              "parameter_type": 1,
              "parameter_mmin": 1,
              "parameter_mmax": 128,
              "parameter_initial_enable": 1,
              "parameter_initial": [
                1
              ]
            }
          }
        }
      },
      {
        "box": {
          "id": "ready",
          "maxclass": "newobj",
          "text": "live.thisdevice",
          "patching_rect": [
            160,
            30,
            100,
            22
          ]
        }
      },
      {
        "box": {
          "id": "bus-name",
          "maxclass": "newobj",
          "text": "sprintf tolglow-audio-%ld",
          "patching_rect": [
            16,
            105,
            158,
            22
          ]
        }
      },
      {
        "box": {
          "id": "rename",
          "maxclass": "newobj",
          "text": "prepend set",
          "patching_rect": [
            16,
            145,
            100,
            22
          ]
        }
      },
      {
        "box": {
          "id": "route",
          "maxclass": "newobj",
          "text": "receive~ ---inactive",
          "patching_rect": [
            16,
            190,
            128,
            22
          ]
        }
      },
      {
        "box": {
          "id": "output",
          "maxclass": "newobj",
          "text": "plugout~",
          "patching_rect": [
            18,
            250,
            100,
            22
          ]
        }
      }
    ],
    "lines": [
      {
        "patchline": {
          "source": [
            "ready",
            0
          ],
          "destination": [
            "bus",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "bus",
            0
          ],
          "destination": [
            "bus-name",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "bus-name",
            0
          ],
          "destination": [
            "rename",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "rename",
            0
          ],
          "destination": [
            "route",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "route",
            0
          ],
          "destination": [
            "output",
            0
          ]
        }
      },
      {
        "patchline": {
          "source": [
            "route",
            0
          ],
          "destination": [
            "output",
            1
          ]
        }
      }
    ],
    "parameters": {
      "bus": [
        "Audio bus",
        "Bus",
        0
      ]
    }
  }
}
