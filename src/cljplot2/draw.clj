(ns cljplot2.draw
  "Drawing grid item content - the counterpart of [[cljplot2.measure/natural-size]]."
  (:require [cljplot2.measure :as measure]))

(defmulti draw-content
  "Draw item `content` onto `canvas` (in graphics context) inside `box` (`{:x :y :w :h}`),
  dispatched like [[cljplot2.measure/natural-size]].

  `env` keys: `:theme` - the plot theme.

  Content kinds without a method draw nothing."
  (fn [content _canvas _box _env] (measure/content-kind content)))

(defmethod draw-content :default [_ _ _ _] nil)
