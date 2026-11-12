(ns cljplot2.subplot
  "Plot content `{:kind :plot :plot p}`: a whole plot in a grid cell (multiplot). Natural size 0x0 - it takes what
  its tracks give. The pipeline's `:subplots` stage builds `p` at the item's box size (see
  [[cljplot2.pipeline]]); [[cljplot2.render]] draws the built plot translated to the box and clipped to it.
  `p` is built with its own theme (no inheritance from the outer plot) and the outer rendering hint."
  (:require [fastmath.vector :as v]
            [cljplot2.measure :as measure]))

(defn plot-content?
  "Is `content` plot content?"
  [content]
  (= :plot (:kind content)))

(defmethod measure/validate-content :plot
  [{:keys [plot]}]
  (when-not (map? plot)
    (throw (ex-info "Plot content :plot must be a plot map." {:key :plot}))))

(defmethod measure/natural-size :plot [_ _] (v/vec2 0.0 0.0))
