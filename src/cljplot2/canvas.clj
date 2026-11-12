(ns cljplot2.canvas
  "Graphics lifecycle of clojure2d canvases.

  `clojure2d.core/with-canvas` doesn't release graphics when its body throws;
  every canvas cljplot2 draws or measures on goes through [[call-with-graphics]] instead."
  (:require [clojure2d.core :as c2d]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn call-with-graphics
  "Call `(f canvas)` with `canvas` in graphics context and return its result;
  the graphics are released even when `f` throws."
  [canvas f]
  (let [canvas (c2d/make-graphics canvas)]
    (try (f canvas)
         (finally (c2d/flush-graphics canvas)))))
