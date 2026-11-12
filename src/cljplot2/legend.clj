(ns cljplot2.legend
  "Legends (D7), derived from the aesthetic scales (see [[cljplot2.aesthetic]]) and drawn as grid content.

  [[legends]]: one legend per aesthetic dimension with entries (ordinal scales, linear size; linear color has no
  legend yet, `:legend false` on the plot or the scale hides it). Dimensions mapped by every chart with the same
  accessor (equal keyword or the same function) and with equal entry labels merge into one legend whose entries
  combine their attributes. Title: the scale's `:title` when given (`nil` - none), else the accessor's name when it
  is a keyword.

  Content `{:kind :legend :title t :entries [{:label l :attributes {dim attribute}}] :charts [i ...]}`: title
  (`:legend-title` role), then one row per entry - a key box (theme `[:legend :key-size]`), gap, label
  (`:legend-label` role). Each chart `i` (index into the plot's `:charts`) draws its glyph into every key with its
  `:legend` function, in order (overlay). Theme `:legend` `:fill`, `:border-color`, `:border-width` box it."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.aesthetic :as aesthetic]
            [cljplot2.canvas :as canvas]
            [cljplot2.draw :as draw]
            [cljplot2.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; derivation

(defn- dim-legend
  "Legend candidate of aesthetic `k`: scale, entries and the set of accessors charts map it with; `nil` without entries."
  [k scale-specs mappings]
  (when-let [spec (get scale-specs k)]
    (when-let [entries (seq (aesthetic/legend-entries k spec))]
      {:dim k :spec spec :entries (vec entries) :accessors (set (keep #(get % k) mappings))})))

(defn- merge-key
  [{:keys [dim entries accessors]}]
  (if (= 1 (count accessors)) [(first accessors) (mapv :label entries)] [::own dim]))

(defn- title
  [members]
  (if-let [titled (first (filter #(contains? (:spec %) :title) members))]
    (get-in titled [:spec :title])
    (let [acc (first (:accessors (first members)))]
      (when (and (= 1 (count (:accessors (first members)))) (keyword? acc)) (name acc)))))

(defn- legend-content
  [members theme charts mappings]
  (let [dims (set (map :dim members))
        na {:color (get-in theme [:palette :na])}
        attribute-fns (mapv (fn [{:keys [dim spec]}] (aesthetic/attribute-fn dim spec (na dim))) members)]
    {:kind :legend
     :title (title members)
     :entries (mapv (fn [i]
                      {:label (:label (get-in (first members) [:entries i]))
                       :attributes (into {} (keep (fn [[{:keys [dim entries]} f]]
                                                    (when-some [a (f (:value (entries i)))] [dim a])))
                                         (map vector members attribute-fns))})
                    (range (count (:entries (first members)))))
     :charts (vec (keep-indexed (fn [i [chart mapping]]
                                  (when (and (:legend chart) (some #(some? (get mapping %)) dims)) i))
                                (map vector charts mappings)))}))

(defn legends
  "Legend contents of a plot built up to `:scale-specs`, in aesthetic order (`:color`, `:shape`, `:size`); empty when
  the plot's `:legend` is `false` or nothing with a legend is mapped."
  [{:keys [scale-specs split charts theme legend]}]
  (if (false? legend)
    []
    (let [mappings (mapv :mapping (:charts split))
          groups (reduce (fn [acc candidate]
                           (let [k (merge-key candidate)
                                 i (first (keep-indexed (fn [i [gk]] (when (= gk k) i)) acc))]
                             (if i (update-in acc [i 1] conj candidate) (conj acc [k [candidate]]))))
                         [] (keep #(dim-legend % scale-specs mappings) aesthetic/dims))]
      (mapv (fn [[_ members]] (legend-content members theme charts mappings)) groups))))

;; content

(defmethod measure/validate-content :legend
  [{:keys [title entries charts]}]
  (when-not (or (nil? title) (string? title))
    (throw (ex-info "Legend :title must be a string or nil." {:key :title :value title})))
  (when-not (and (vector? entries) (every? #(and (map? %) (string? (:label %)) (map? (:attributes %))) entries))
    (throw (ex-info "Legend :entries must be a vector of {:label string :attributes map}." {:key :entries})))
  (when-not (and (vector? charts) (every? nat-int? charts))
    (throw (ex-info "Legend :charts must be a vector of chart indices." {:key :charts}))))

(defn- title-label [text] {:kind :label :role :legend-title :text text})
(defn- entry-label [text] {:kind :label :role :legend-label :text text})

(defn- legend-geometry
  "Sizes of a legend's parts, measured on `env` `:canvas`: `:title-size` (Vec2), `:labels` (Vec2s), `:row-height`,
  `:key-size`, `:gap`, `:border` (rounded border width), `:w`, `:h` (natural size)."
  [{:keys [title entries]} env]
  (let [{:keys [gap key-size border-width]} (get-in env [:theme :legend])
        gap (double gap) key-size (double key-size)
        border (double (m/round (double border-width)))
        ^Vec2 title-size (if title (measure/natural-size (title-label title) env) (v/vec2 0.0 0.0))
        labels (mapv #(measure/natural-size (entry-label (:label %)) env) entries)
        widest (double (reduce max 0.0 (map #(.x ^Vec2 %) labels)))
        row-height (double (reduce max key-size (map #(.y ^Vec2 %) labels)))]
    {:title-size title-size :labels labels :row-height row-height :key-size key-size :gap gap :border border
     :w (+ (* 2.0 border) (m/max (.x title-size) (+ key-size gap widest)))
     :h (+ (* 2.0 border) (.y title-size) (* row-height (count entries)))}))

(defmethod measure/natural-size :legend
  [content env]
  (let [{:keys [w h]} (legend-geometry content env)]
    (v/vec2 w h)))

(defn- draw-key!
  "Overlay the glyphs of the legend's charts in key `box`, each isolated and clipped to the box."
  [c attributes {:keys [x y w h] :as box} chart-indices env]
  (doseq [i chart-indices
          :let [chart (get-in env [:charts i])]]
    (canvas/call-with-isolated-graphics
     c (fn [c']
         (.clip ^Graphics2D (:graphics c') (Rectangle. (int x) (int y) (int (m/ceil w)) (int (m/ceil h))))
         ((:legend chart) chart attributes c' box env)))))

(defmethod draw/draw-content :legend
  [{:keys [title entries charts] :as content} c {:keys [x y w h] :as box} env]
  (let [{:keys [fill border-color border-width]} (get-in env [:theme :legend])
        env (assoc env :canvas c)
        {:keys [^Vec2 title-size labels ^double row-height ^double key-size ^double gap ^double border]}
        (legend-geometry content env)
        x0 (+ (double x) border)
        y0 (+ (double y) border)
        inner-w (- (double w) (* 2.0 border))
        label! (fn [label-content box] (canvas/call-with-isolated-graphics c #(draw/draw-content label-content % box env)))]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c (long x) (long y) (long w) (long h)))
    (draw/inner-border! c box border-color border-width)
    (when title
      (label! (title-label title) {:x x0 :y y0 :w inner-w :h (.y title-size)}))
    (dorun (map-indexed (fn [^long i [entry ^Vec2 label-size]]
                          (let [ry (+ y0 (.y title-size) (* i row-height))]
                            (draw-key! c (:attributes entry) {:x x0 :y ry :w key-size :h row-height} charts env)
                            (label! (entry-label (:label entry))
                                    {:x (+ x0 key-size gap) :y ry :w (.x label-size) :h row-height})))
                        (map vector entries labels)))))
