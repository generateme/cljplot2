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
  Draw or measure on it within a graphics context, see [[call-with-scratch-canvas]]."
  [hint]
  (c2d/canvas 1 1 hint))

(defn call-with-scratch-canvas
  "Call `(f canvas)` with a [[scratch-canvas]] in graphics context; the graphics are released even when `f` throws."
  [hint f]
  (let [canvas (c2d/make-graphics (scratch-canvas hint))]
    (try (f canvas)
         (finally (c2d/flush-graphics canvas)))))

(defmulti natural-size
  "Natural size of item `content` as `Vec2` `[width height]`, dispatched on `(:kind content)`.

  `env` keys:

  * `:canvas` - scratch canvas in graphics context (absent when measuring without one)
  * `:box` - the item's box from the previous layout round, `nil` in the first round
  * `:scales` - scales finalized for the previous layout round

  `nil` content (an empty box) has zero natural size; other content without a known `:kind` is an error."
  (fn [content _env] (if (nil? content) ::empty (:kind content))))

(defmethod natural-size ::empty [_ _] (v/vec2 0.0 0.0))

(defmethod natural-size :placeholder
  [{:keys [size] :as content} _]
  (let [[w h] size]
    (when-not (and (vector? size) (number? w) (number? h))
      (throw (ex-info "Placeholder :size must be a vector of two numbers." {:size size :kind (:kind content)})))
    (v/vec2 w h)))

(defmethod natural-size :default
  [content _]
  (throw (ex-info "Unknown content kind." {:kind (:kind content)})))
