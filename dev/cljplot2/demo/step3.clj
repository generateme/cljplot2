(ns cljplot2.demo.step3
  "Step 3 demo: empty panel with titles (plain and debug).")

(def plot
  {:labels {:title "Empty panel" :x "x title" :y "y title"}})

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step3/` by [[cljplot2.demo/render!]]."
  [["panel-600x400.png" plot {:width 600 :height 400}]
   ["panel-600x400-debug.png" plot {:width 600 :height 400 :debug true}]])
