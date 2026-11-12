(ns cljplot2.canvas
  "Graphics lifecycle of clojure2d canvases.

  `clojure2d.core/with-canvas` doesn't release graphics when its body throws;
  every canvas cljplot2 draws or measures on goes through [[call-with-graphics]] instead."
  (:require [clojure2d.core :as c2d])
  (:import [java.awt Graphics2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn call-with-graphics
  "Call `(f canvas)` with `canvas` in graphics context and return its result;
  the graphics are released even when `f` throws."
  [canvas f]
  (let [canvas (c2d/make-graphics canvas)]
    (try (f canvas)
         (finally (c2d/flush-graphics canvas)))))

(defn call-with-isolated-graphics
  "Call `(f canvas')` where `canvas'` draws through a copy of `canvas`'s graphics (`canvas` must be in graphics context).
  Anything `f` changes - clip, transform, font, color, stroke - is discarded afterwards, even when `f` throws."
  [canvas f]
  (let [^Graphics2D g (.create ^Graphics2D (:graphics canvas))]
    (try (f (assoc canvas :graphics g :transform-stack (atom [])))
         (finally (.dispose g)))))
