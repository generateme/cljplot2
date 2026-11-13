(ns cljplot2.content.subplot
  "Plot content `{:kind :plot :plot p}`: a whole plot in a grid cell (multiplot). Natural size 0x0 - it takes what
  its tracks give. The pipeline's `:subplots` stage builds `p` at the item's box size (see
  [[cljplot2.pipeline]]); [[cljplot2.render]] draws the built plot translated to the box and clipped to it.
  `p` is built with its own theme (no inheritance from the outer plot) and the outer rendering hint."
  (:require [cljplot2.schema :as schema]
            [fastmath.vector :as v]
            [cljplot2.graphics.measure :as measure]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn plot-content?
  "Is `content` plot content?"
  [content]
  (= :plot (:kind content)))

(def content-schema
  "`{:kind :plot}` content: `:plot`, a plot map (validated when the sub-plot is built)."
  [:map [:plot :map]])

(defmethod measure/validate-content :plot [content] (schema/explain! [[content-schema content [:content]]]))

(defmethod measure/natural-size :plot [_ _] (v/vec2 0.0 0.0))
