(ns cljplot2.graphics.measure
  "Natural sizes of grid item content.

  Text metrics depend on the rendering hint (fractional metrics), not on canvas size,
  so content is measured on a 1x1 scratch canvas created with the target canvas's hint."
  (:require [cljplot2.schema :as schema]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.graphics.canvas :as canvas]))

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
  (canvas/call-with-graphics (scratch-canvas hint) f))

(defn spans-overlap?
  "Do two labels with centres `ci`, `cj` and extents `ei`, `ej` along one axis overlap by more than 1 px (natural sizes
  are rounded up)?"
  [^double ci ^double ei ^double cj ^double ej]
  (< (Math/abs (- cj ci)) (- (* 0.5 (+ ei ej)) 1.0)))

(defn kept-label-indices
  "Indices of labels (`centres`, `extents` along one axis, in tick order) to draw without collisions: the first, then
  each label not overlapping the last kept one ([[spans-overlap?]]); empty for no labels."
  [centres extents]
  (loop [i 0 last-kept nil kept []]
    (if (>= i (count centres))
      kept
      (if (and last-kept (spans-overlap? (double (centres last-kept)) (double (extents last-kept))
                                         (double (centres i)) (double (extents i))))
        (recur (inc i) last-kept kept)
        (recur (inc i) i (conj kept i))))))

(defn label-boxes-overlap?
  "Do pixel boxes `a` and `b` (`{:x :y :w :h}`) overlap by more than 1 px both across and along (natural sizes are
  rounded up)?"
  [a b]
  (let [over (fn [a0 aw b0 bw] (- (min (+ (double a0) (double aw)) (+ (double b0) (double bw))) (max (double a0) (double b0))))]
    (and (> (double (over (:x a) (:w a) (:x b) (:w b))) 1.0)
         (> (double (over (:y a) (:h a) (:y b) (:h b))) 1.0))))

(defn kept-box-indices
  "Indices of label `boxes` (`{:x :y :w :h}`, in tick order) to draw without collisions: each box overlapping no box
  kept before it ([[label-boxes-overlap?]]) - around a circle the last label is checked against the first too; empty
  for no boxes."
  [boxes]
  (reduce (fn [kept i]
            (if (some #(label-boxes-overlap? (boxes i) (boxes %)) kept) kept (conj kept i)))
          [] (range (count boxes))))

(defn content-kind
  "Dispatch value of item content: `::empty` for `nil` (an empty box), otherwise its `:kind`."
  [content]
  (if (nil? content) ::empty (:kind content)))

(defmulti validate-content
  "Check item `content` once, before layout; throw `ex-info` naming the offending key.
  Dispatched on [[content-kind]]; kinds without a method aren't checked here.
  [[natural-size]] and [[cljplot2.graphics.draw/draw-content]] may assume validated content."
  content-kind)

(defmethod validate-content :default [_] nil)

(defmulti natural-size
  "Natural size of item `content` as `Vec2` `[width height]`, dispatched on [[content-kind]].

  `env` keys:

  * `:canvas` - scratch canvas in graphics context (absent when measuring without one)
  * `:box` - the item's box from the previous layout round, `nil` in the first round
  * `:scales` - scales finalized for the previous layout round
  * `:theme` - the plot theme, see [[cljplot2.theme]]

  `nil` content (an empty box) has zero natural size; other content without a known `:kind` is an error."
  (fn [content _env] (content-kind content)))

(defmethod natural-size ::empty [_ _] (v/vec2 0.0 0.0))

(def ^:private placeholder-schema [:map [:size [:tuple number? number?]]])

(defmethod natural-size :placeholder
  [{:keys [size] :as content} _]
  (schema/explain! [[placeholder-schema content [:content]]])
  (v/vec2 (size 0) (size 1)))

(defmethod natural-size :default
  [content _]
  (schema/fail! "Unknown content kind." {:key :kind :value (:kind content)}))
