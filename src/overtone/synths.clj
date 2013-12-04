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
