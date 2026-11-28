(ns tolglow.agent-goal-text-cases
  "Goal-text cases (#857) that the JVM owner test and the Studio test both
  run, so the two implementations of tolglow.agent-goal-text stay identical.
  Each case is [label text expected], where expected is :ok, or the problem
  keyword, or [:ok normalised-text].")

(defn cp
  "The string of the given code points."
  [& points]
  (apply str (map #?(:clj #(String. (Character/toChars (int %))) :cljs #(js/String.fromCodePoint %)) points)))

(def cases
  [["plain words" "Warm and patient until the room fills." :ok]
   ["newline and tab" "a\tb\nc" :ok]
   ["CRLF becomes LF" "line one\r\nline two\rthree" [:ok "line one\nline two\nthree"]]
   ["NFC" (str "Cafe" (cp 0x301)) [:ok (str "Caf" (cp 0xE9))]]
   ["family emoji" (str "Warm like " (cp 0x1F468 0x200D 0x1F469 0x200D 0x1F467)) :ok]
   ["rainbow flag" (str "Pride " (cp 0x1F3F3 0xFE0F 0x200D 0x1F308) " night, all colours") :ok]
   ["heart with VS16" (str "Love " (cp 0x2764 0xFE0F)) :ok]
   ["Persian with ZWNJ" (str (cp 0x645 0x6CC 0x200C 0x62E 0x648 0x627 0x647 0x645)) :ok]
   ["Hindi with ZWJ" (cp 0x915 0x94D 0x200D 0x937) :ok]
   ["fullwidth lookalike" (str (cp 0xFF42 0xFF4C 0xFF41 0xFF43 0xFF4B) " now") :ok]
   ["NUL" (str "a" (cp 0) "b") :invalid-character]
   ["ESC" (str "red" (cp 0x1B) "[2Jblue") :invalid-character]
   ["right-to-left override" (str "safe " (cp 0x202E) "etouq") :invalid-character]
   ["tag smuggling" (apply str "warm" (map #(cp (+ 0xE0000 %)) [98 108 97 99 107])) :invalid-character]
   ["variation-selector smuggling" (apply str "warm" (map #(cp (+ 0xE0100 %)) [98 108 97 99 107 111 117 116])) :invalid-character]
   ["interlinear annotation" (str "a" (cp 0xFFF9) "hidden" (cp 0xFFFB) "b") :invalid-character]
   ["Arabic letter mark" (str "abc" (cp 0x61C) "def") :invalid-character]
   ["Mongolian vowel separator" (str "a" (cp 0x180E) "b") :invalid-character]
   ["combining grapheme joiner" (str "a" (cp 0x34F) "b") :invalid-character]
   ["soft hyphen" (str "bl" (cp 0xAD) "ackout") :invalid-character]
   ["word joiner" (str "a" (cp 0x2060) "b") :invalid-character]
   ["ZWJ at the start" (str (cp 0x200D) "warm") :invalid-character]
   ["ZWJ at the end" (str "warm" (cp 0x200D)) :invalid-character]
   ["repeated ZWJ" (str "a" (cp 0x200D 0x200D) "b") :invalid-character]
   ["ZWNJ beside a space" (str "a " (cp 0x200C) "b") :invalid-character]
   ["two variation selectors" (str (cp 0x2764 0xFE0F 0xFE0F)) :invalid-character]
   ["variation selector without a base" (str " " (cp 0xFE0F) "warm") :invalid-character]
   ["unpaired surrogate" #?(:clj (str "a" (char 0xD800) "b") :cljs (str "a" (js/String.fromCharCode 0xD800) "b")) :invalid-character]
   ["zero-width spaces only" (cp 0x200B 0x200B) :missing]
   ["Hangul fillers only" (cp 0x3164 0x3164) :missing]
   ["blank braille only" (cp 0x2800 0x2800) :missing]
   ["spaces only" "   " :missing]])
