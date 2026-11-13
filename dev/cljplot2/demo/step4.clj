(ns cljplot2.demo.step4
  "Step 4 demo: scales, axes and grid lines (plain and debug).")

(def plots
  {"linear-bands" {:labels {:title "Scales and axes" :x "x (linear)" :y "y (bands)"}
                   :scales {:x {:domain [0.13 7.31]} :y {:domain ["a" "b" "c"]}}}
   "wide-labels" {:labels {:x "x" :y "y"}
                  :scales {:x {:domain [-5 5]} :y {:domain [0 1e6]}}}})

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step4/` by [[cljplot2.demo/run!]]."
  (for [[nm plot] plots
        [suffix options] [["" {:width 600 :height 400}] ["-debug" {:width 600 :height 400 :debug true}]]]
    [(str nm "-600x400" suffix ".png") plot options]))
