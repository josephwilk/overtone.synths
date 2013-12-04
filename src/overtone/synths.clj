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

(defsynth space-organ [out-bus 0 tone 1 duration 3 amp 1]
  (let [f     (map #(midicps (duty:kr % 0 (dseq 2 4))) [1])
        tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/8) 2 4)) f [tone])]
    (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE)))))

(comment (space-organ :tone 30))
