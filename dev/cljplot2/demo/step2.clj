(ns cljplot2.demo.step2
  "Step 2 demo: Fig 1.1 layout with real text (plain and debug)."
  (:require [cljplot2.examples :as examples]))

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step2/` by [[cljplot2.demo/render!]]."
  (for [[file-name options] [["fig-1-1-800x600.png" {:width 800 :height 600}]
                             ["fig-1-1-800x600-debug.png" {:width 800 :height 600 :debug true}]
                             ["fig-1-1-400x300.png" {:width 400 :height 300}]]]
    [file-name {:custom-grid examples/fig-1-1-labelled-grid} options]))
