(ns cljplot2.measure
  "Natural sizes of grid item content.

  Text metrics depend on the rendering hint (fractional metrics), not on canvas size,
  so content is measured on a 1x1 scratch canvas created with the target canvas's hint."
  (:require [clojure2d.core :as c2d]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def rendering-hints
  "Rendering hints accepted for plots."
  #{:low :mid :high :highest})

(defn scratch-canvas
  "1x1 canvas for measuring, with the given rendering `hint` (one of [[rendering-hints]]).
  Use within `clojure2d.core/with-canvas`."
  [hint]
  (c2d/canvas 1 1 hint))

(defmulti natural-size
  "Natural size of item `content` as `Vec2` `[width height]`, dispatched on `(:kind content)`.

  `env` keys:

  * `:canvas` - scratch canvas in graphics context (absent when measuring without one)
  * `:box` - the item's box from the previous layout round, `nil` in the first round
  * `:scales` - scales finalized for the previous layout round

  Content without `:kind` (including `nil`) has zero natural size."
  (fn [content _env] (:kind content)))

(defmethod natural-size nil [_ _] (v/vec2 0.0 0.0))

(defmethod natural-size :placeholder
  [{:keys [size] :as content} _]
  (let [[w h] size]
    (when-not (and (number? w) (number? h)
                   (>= (double w) 0.0) (>= (double h) 0.0))
      (throw (ex-info "Placeholder :size must be two non-negative numbers." {:size size :content-kind (:kind content)})))
    (v/vec2 w h)))

(defmethod natural-size :default
  [content _]
  (throw (ex-info "Unknown content kind." {:kind (:kind content)})))
