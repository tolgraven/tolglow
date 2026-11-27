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
   1100,
   240
  ],
  "openinpresentation": 1,
  "default_fontsize": 12.0,
  "default_fontface": 0,
  "default_fontname": "Arial",
  "description": "Tolglow parameters (#625). Drop on a Tolglow run track. Save the device as \"Tolglow parameters\" (or \"Tolglow parameters 2\" .. for more than 32 slots).",
  "dependency_cache": [
   {
    "name": "parameter_device.js",
    "type": "TEXT",
    "implicit": 1
   }
  ],
  "boxes": [
   {
    "box": {
     "id": "title",
     "maxclass": "comment",
     "text": "Tolglow parameters: one run channel's effect parameters (#625). Tolglow names and ranges these slots through the companion.",
     "patching_rect": [
      20,
      10,
      700,
      20
     ],
     "numinlets": 1,
     "numoutlets": 0
    }
   },
   {
    "box": {
     "id": "init",
     "maxclass": "newobj",
     "text": "live.thisdevice",
     "patching_rect": [
      20,
      40,
      100,
      22
     ],
     "numinlets": 1,
     "numoutlets": 3,
     "outlettype": [
      "bang",
      "int",
      "int"
     ]
    }
   },
   {
    "box": {
     "id": "receive",
     "maxclass": "newobj",
     "text": "receive tolglow-parameter-device",
     "patching_rect": [
      140,
      40,
      200,
      22
     ],
     "numinlets": 0,
     "numoutlets": 1,
     "outlettype": [
      ""
     ]
    }
   },
   {
    "box": {
     "id": "device-js",
     "maxclass": "newobj",
     "text": "js parameter_device.js",
     "patching_rect": [
      20,
      90,
      150,
      22
     ],
     "numinlets": 1,
     "numoutlets": 1,
     "outlettype": [
      ""
     ],
     "saved_object_attributes": {
      "filename": "parameter_device.js",
      "parameter_enable": 0
     }
    }
   },
   {
    "box": {
     "id": "status",
     "maxclass": "newobj",
     "text": "print tolglow-parameters",
     "patching_rect": [
      20,
      130,
      160,
      22
     ],
     "numinlets": 1,
     "numoutlets": 0
    }
   },
   {
    "box": {
     "id": "slot-dial-1",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      360,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      4,
      4,
      40,
      48
     ],
     "varname": "slot-1",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 1",
       "parameter_shortname": "1",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 0
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-2",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      404,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      48,
      4,
      40,
      48
     ],
     "varname": "slot-2",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 2",
       "parameter_shortname": "2",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 1
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-3",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      448,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      92,
      4,
      40,
      48
     ],
     "varname": "slot-3",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 3",
       "parameter_shortname": "3",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 2
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-4",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      492,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      136,
      4,
      40,
      48
     ],
     "varname": "slot-4",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 4",
       "parameter_shortname": "4",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 3
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-5",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      536,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      180,
      4,
      40,
      48
     ],
     "varname": "slot-5",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 5",
       "parameter_shortname": "5",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 4
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-6",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      580,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      224,
      4,
      40,
      48
     ],
     "varname": "slot-6",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 6",
       "parameter_shortname": "6",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 5
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-7",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      624,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      268,
      4,
      40,
      48
     ],
     "varname": "slot-7",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 7",
       "parameter_shortname": "7",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 6
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-8",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      668,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      312,
      4,
      40,
      48
     ],
     "varname": "slot-8",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 8",
       "parameter_shortname": "8",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 7
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-9",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      712,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      356,
      4,
      40,
      48
     ],
     "varname": "slot-9",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 9",
       "parameter_shortname": "9",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 8
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-10",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      756,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      400,
      4,
      40,
      48
     ],
     "varname": "slot-10",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 10",
       "parameter_shortname": "10",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 9
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-11",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      800,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      444,
      4,
      40,
      48
     ],
     "varname": "slot-11",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 11",
       "parameter_shortname": "11",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 10
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-12",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      844,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      488,
      4,
      40,
      48
     ],
     "varname": "slot-12",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 12",
       "parameter_shortname": "12",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 11
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-13",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      888,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      532,
      4,
      40,
      48
     ],
     "varname": "slot-13",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 13",
       "parameter_shortname": "13",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 12
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-14",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      932,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      576,
      4,
      40,
      48
     ],
     "varname": "slot-14",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 14",
       "parameter_shortname": "14",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 13
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-15",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      976,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      620,
      4,
      40,
      48
     ],
     "varname": "slot-15",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 15",
       "parameter_shortname": "15",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 14
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-16",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      1020,
      40,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      664,
      4,
      40,
      48
     ],
     "varname": "slot-16",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 16",
       "parameter_shortname": "16",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 15
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-17",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      360,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      4,
      60,
      40,
      48
     ],
     "varname": "slot-17",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 17",
       "parameter_shortname": "17",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 16
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-18",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      404,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      48,
      60,
      40,
      48
     ],
     "varname": "slot-18",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 18",
       "parameter_shortname": "18",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 17
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-19",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      448,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      92,
      60,
      40,
      48
     ],
     "varname": "slot-19",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 19",
       "parameter_shortname": "19",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 18
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-20",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      492,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      136,
      60,
      40,
      48
     ],
     "varname": "slot-20",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 20",
       "parameter_shortname": "20",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 19
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-21",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      536,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      180,
      60,
      40,
      48
     ],
     "varname": "slot-21",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 21",
       "parameter_shortname": "21",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 20
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-22",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      580,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      224,
      60,
      40,
      48
     ],
     "varname": "slot-22",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 22",
       "parameter_shortname": "22",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 21
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-23",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      624,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      268,
      60,
      40,
      48
     ],
     "varname": "slot-23",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 23",
       "parameter_shortname": "23",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 22
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-24",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      668,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      312,
      60,
      40,
      48
     ],
     "varname": "slot-24",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 24",
       "parameter_shortname": "24",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 23
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-25",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      712,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      356,
      60,
      40,
      48
     ],
     "varname": "slot-25",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 25",
       "parameter_shortname": "25",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 24
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-26",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      756,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      400,
      60,
      40,
      48
     ],
     "varname": "slot-26",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 26",
       "parameter_shortname": "26",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 25
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-27",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      800,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      444,
      60,
      40,
      48
     ],
     "varname": "slot-27",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 27",
       "parameter_shortname": "27",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 26
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-28",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      844,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      488,
      60,
      40,
      48
     ],
     "varname": "slot-28",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 28",
       "parameter_shortname": "28",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 27
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-29",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      888,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      532,
      60,
      40,
      48
     ],
     "varname": "slot-29",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 29",
       "parameter_shortname": "29",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 28
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-30",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      932,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      576,
      60,
      40,
      48
     ],
     "varname": "slot-30",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 30",
       "parameter_shortname": "30",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 29
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-31",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      976,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      620,
      60,
      40,
      48
     ],
     "varname": "slot-31",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 31",
       "parameter_shortname": "31",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 30
      }
     }
    }
   },
   {
    "box": {
     "id": "slot-dial-32",
     "maxclass": "live.dial",
     "numinlets": 1,
     "numoutlets": 2,
     "outlettype": [
      "",
      "float"
     ],
     "parameter_enable": 1,
     "patching_rect": [
      1020,
      96,
      40,
      48
     ],
     "presentation": 1,
     "presentation_rect": [
      664,
      60,
      40,
      48
     ],
     "varname": "slot-32",
     "saved_attribute_attributes": {
      "valueof": {
       "parameter_longname": "slot 32",
       "parameter_shortname": "32",
       "parameter_type": 0,
       "parameter_mmin": 0.0,
       "parameter_mmax": 1.0,
       "parameter_initial_enable": 1,
       "parameter_initial": [
        0.0
       ],
       "parameter_unitstyle": 1,
       "parameter_invisible": 0,
       "parameter_order": 31
      }
     }
    }
   }
  ],
  "lines": [
   {
    "patchline": {
     "source": [
      "init",
      0
     ],
     "destination": [
      "device-js",
      0
     ]
    }
   },
   {
    "patchline": {
     "source": [
      "receive",
      0
     ],
     "destination": [
      "device-js",
      0
     ]
    }
   },
   {
    "patchline": {
     "source": [
      "device-js",
      0
     ],
     "destination": [
      "status",
      0
     ]
    }
   }
  ]
 }
}
