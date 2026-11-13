(ns cljplot2.demo.step1
  "Step 1 demo: debug images of the solved grids."
  (:require [cljplot2.examples :as examples]))

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step1/` by [[cljplot2.demo/run!]]."
  (for [[file-name grid width height] [["fig-1-1-800x600.png" examples/fig-1-1-grid 800 600]
                                       ["fig-1-1-400x300.png" examples/fig-1-1-grid 400 300]
                                       ["nested-1000x600.png" examples/nested-grid 1000 600]]]
    [file-name {:custom-grid grid} {:width width :height height :debug true}]))
