(ns overtone.synths
  (:use overtone.live))

;; Based on Dawn by Schemawound: http://sccode.org/1-c
(defsynth fallout-wind [decay 30 attack 30]
  (let [lfo  (+ 0.5 (* 0.5 (sin-osc:kr [(ranged-rand 0.5 1000) (ranged-rand 0.5 1000)] :phase (* 1.5 Math/PI))))
        lfo3 (+ 0.5 (* 0.5 (sin-osc:kr [(ranged-rand 0.1 0.5) (ranged-rand 0.1 0.5)] :phase (* 1.5 Math/PI))))
        lfo2 (+ 0.5 (* 0.5 (sin-osc:kr [(* (ranged-rand 0.5 1000) lfo lfo3) (* (ranged-rand 0.5 1000) (- 1 lfo) (- 1 lfo3))] :phase (* 1.5 Math/PI))))
        fillers (map (fn [_] (* lfo2 (sin-osc:ar (ranged-rand 40 1000) :phase 0))) (range 0 100))]
    (out:ar 0  (* (mix:ar fillers)
                  (env-gen:kr (perc attack decay) :action FREE)))))

;; Based on SpaceOrgan from emacs-live snippets
(defsynth space-organ [out-bus 0 tone 1 duration 3 amp 1]
  (let [f     (map #(midicps (duty:kr % 0 (dseq 2 4))) [1])
        tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/8) 2 4)) f [tone])]
    (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE)))))

(comment (space-organ :tone 30))


(defsynth alien-wail [freq 380 out-bus 0 vibrato-speed 6 vibrato-depth 4 lag-val 0.5 gate 1]
  (let [trig (impulse:kr 3)
        freqs (dseq  [400 750 2400 2600 2900])
        freqs (demand:kr trig 0 freqs)
        amps  (dseq [1 0.28 0.08 0.1 0.01])
        amps  (demand:kr trig 0 amps)
        qs (dseq [0.1 0.1 0.04 0.04 0.04])
        qs (demand:kr trig 0 qs)
        vibrato (* vibrato-depth (sin-osc:kr vibrato-speed))
        in (saw:ar (lag:kr (+ freq vibrato) 0.2))
        env (env-gen:kr (env-asr 1) gate FREE)
        snd (* (lag:kr amps lag-val) (bpf:ar in (lag:kr freqs lag-val) (lag:kr qs lag-val)))]
    (out out-bus (* snd env))))

(alien-wail :freq 200)
(kill alien-wail)

;;Based on Formant Synthesis Singers by Bruno Ruviaro http://sccode.org/1-4Uz
(defsynth sing [freq 380
                freq0 400  freq1 750 freq2 2400 freq3 2600 freq4 2900
                amps0 1 amps1 0.28 amps2 0.08 amps3 0.1 amps4 0.01
                qs0 0.1 qs1 0.1 qs2 0.04 qs3 0.04 qs4 0.04
                out-bus 0 vibrato-speed 6 vibrato-depth 4 lag-val 0.5 gate 1]
  (let [freqs-list (map #(lag:kr % lag-val) [freq0 freq1 freq2 freq3 freq4])
        amps-list  (map #(lag:kr (dbamp %) lag-val) [amps0 amps1 amps2 amps3 amps4] )
        qs-list    (map #(lag:kr % lag-val) [qs0 qs1 qs2 qs3 qs4])

        vibrato (* vibrato-depth (sin-osc:kr vibrato-speed))
        in (saw:ar (lag:kr (+ freq vibrato) 0.2))
        env (env-gen:kr (env-asr 1) gate FREE)
        snd (mix (* amps-list (bpf:ar in freqs-list qs-list)))]
    (out out-bus (* snd env))))

(def voice-settings
  {:soprano
   {:A [[800 1150 2900 3900 4950] [0 -6 -32 -20 -50]  [80 90 120 130 140]]
    :E [[350 2000 2800 3600 4950] [0 -20 -15 -40 -56] [60 100 120 150 200]]
    :I [[270 2140 2950 3900 4950] [0 -12 -26 -26 -44] [60 90 100 120 120]]
    :O [[450 800 2830 3800 4950]  [0 -11 -22 -22 -50] [70 80 100 130 135]]
    :U [[325 700 2700 3800 4950]  [0 -16 -35 -40 -60] [50 60 170 180 200]]}
   :alto
   {:A [[800 1150 2800 3500 4950] [0 -4 -20 -36 -60]  [80 90 120 130 140]]
    :E [[400 1600 2700 3300 4950] [0 -24 -30 -35 -60] [60 80 120 150 200]]
    :I [[350 1700 2700 3700 4950] [0 -20 -30 -36 -60] [50 100 120 150 200]]
    :O [[450 800 2830 3500 4950]  [0 -9 -16 -28 -55]  [70 80 100 130 135]]
    :U [[325 700 2530 3500 4950]  [0 -12 -30 -40 -64] [50 60 170 180 200]]}
   :counter-tenor
   {:A [[660 1120 2750 3000 3350] [0 -6 -23 -24 -38]  [80 90 120 130 140]]
    :E [[440 1800 2700 3000 3300] [0 -14 -18 -20 -20] [70 80 100 120 120]]
    :I [[270 1850 2900 3350 3590] [0 -24 -24 -36 -36] [40 90 100 120 120]]
    :O [[430 820 2700 3000 3300]  [0 -10 -26 -22 -34] [40 80 100 120 120]]
    :U [[370 630 2750 3000 3400]  [0 -20 -23 -30 -34] [40 60 100 120 120]]}
   :tenor
   {:A [[650 1080 2650 2900 3250] [0 -6 -7 -8 -22]    [80 90 120 130 140]]
    :E [[400 1700 2600 3200 3580] [0 -14 -12 -14 -20] [70 80 100 120 120]]
    :I [[290 1870 2800 3250 3540] [0 -15 -18 -20 -30] [40 90 100 120 120]]
    :O [[400 800 2600 2800 3000]  [0 -10 -12 -12 -26] [40 80 100 120 120]]
    :U [[350 600 2700 2900 3300]  [0 -20 -17 -14 -26] [40 60 100 120 120]]}
   :bass
   {:A [[600 1040 2250 2450 2750] [0 -7 -9 -9 -20]    [60 70 110 120 130]]
    :E [[400 1620 2400 2800 3100] [0 -12 -9 -12 -18]  [40 80 100 120 120]]
    :I [[250 1750 2600 3050 3340] [0 -30 -16 -22 -28] [60 90 100 120 120]]
    :O [[400 750 2400 2600 2900]  [0 -11 -21 -20 -40] [40 80 100 120 120]]
    :U [[350 600 2400 2675 2950]  [0 -20 -32 -28 -36] [40 80 100 120 120]]}})

(defn settings-for [voice note]
  (let [[freq0 freq1 freq2 freq3 freq4] (get-in voice-settings [voice note 0])
        [amps0 amps1 amps2 amps3 amps4]  (get-in voice-settings [voice note 1])
        [qs0 qs1 qs2 qs3 qs4] (get-in voice-settings [voice note 2])

        settings {:freq0 freq0 :freq1 freq1 :freq2 freq2 :freq3 freq3 :freq4 freq4
                  :amps0 amps0 :amps1 amps1 :amps2 amps2 :amps3 amps3 :amps4 amps4
                  :qs0 (/ qs0 freq0) :qs1 (/ qs1 freq1) :qs2 (/ qs2 freq2) :qs3 (/ qs3 freq3) :qs4 (/ qs4 freq4)}]
    (interleave (keys settings) (vals settings))))

(def bass    (sing :freq 100))
(def tenor   (sing :freq 280))
(def alto    (sing :freq 380))
(def soprano (sing :freq 580))
(def counter-tenor (sing :freq 280))

(def v alto)

(apply ctl v (settings-for :bass :A))
(apply ctl v (settings-for :bass :E))
(apply ctl v (settings-for :bass :I))
(apply ctl v (settings-for :bass :O))
(apply ctl v (settings-for :bass :U))

(kill counter-tenor)
(kill bass)
(kill tenor)
(kill alto)
(kill soprano)
