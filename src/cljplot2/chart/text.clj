(ns cljplot2.chart.text
  "Text: labels at positions - an annotation chart (see [[cljplot2.data.annotation]]: literal options or data through
  the chart's own `:mapping`, `:extent`).

  Literal: `:x`, `:y` (a position or a vector), `:text` (a value or a vector; converted with `str`), vectors of 1 or n
  items paired. `:x` / `:y` also take `[:panel f]` - fraction `f` (0..1) of the panel from its low-value end, not in
  the extents (a vector starting with `:panel` is always such a position). Data: `:x :y :label` in the chart's own `:mapping`, one label per row (rows with an invalid position
  skipped); ordinal aesthetics (`:color`) through the chart's own mapping colour the text and give legend keys.
  Positions are numbers, `java.time` values or levels.

  End labels (data mode): `:select` `:last` / `:first` keeps one label per group (aesthetics, `:group`) and label
  text - the row with the largest / smallest x (numeric or temporal; ties - the later row; other x - the last / first
  row in data order). Without an `:anchor` / `:offset` it places the text past the point - `:last` `[:left :middle]`
  offset `[5 0]`, `:first` `[:right :middle]` `[-5 0]` (flipped plots: `[:center :bottom]` `[0 5]`, `[:center :top]`
  `[0 -5]`) - and keeps room for the labels on that side of x (`:padding-px`: the widest label - the tallest when
  flipped - and the offset).

  Options: `:anchor` - the point of the text box placed at the position, `[h v]` (`h` one of `:left :center :right`,
  `v` one of `:top :middle :bottom`; default `[:center :middle]`); `:offset [dx dy]` - pixels, `dy` up (default
  `[0 0]`); `:rotation` - degrees, clockwise (default 0; the anchor applies to the rotated text's bounding box);
  `:font`, `:size`, `:font-style`, `:color` - over the text role `[:text :annotation]` (the text chart's theme style;
  a theme `[:chart :text]` entry is rejected); `:extent`, `:data` /
  `:mapping`. Text stays upright when the plot is flipped; only the anchor position enters the extents."
  (:require [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- panel-position? [p] (and (vector? p) (= 2 (count p)) (= :panel (first p))))

(def ^:private panel-position
  (schema/named-type ::panel-position #(and (panel-position? %) (number? (second %)) (<= 0.0 (double (second %)) 1.0))
                     "should be [:panel f], f in [0 1]"))

(def ^:private text-position [:or annotation/position panel-position])

(def ^:private text-positions
  ;; a vector starting with :panel is always a panel position, never a vector of positions
  [:multi {:dispatch #(and (vector? %) (= :panel (first %)))}
   [true panel-position]
   [false (annotation/positions-schema text-position)]])

(def ^:private style-keys [:font :size :font-style :color])

(def schemas
  "Schemas of the text chart: `:options`, `:mapping` (merged); the style is the text role `[:text :annotation]`."
  {:options (-> [:map {:closed true}]
                (into (filter #(contains? (set style-keys) (first %)) theme/text-style-overrides))
                (into data/common-options)
                (into annotation/options)
                (conj (schema/nilable :x text-positions)
                      (schema/nilable :y text-positions)
                      (schema/nilable :text any?)
                      (schema/optional :anchor [:tuple [:enum :left :center :right] [:enum :top :middle :bottom]])
                      (schema/optional :offset [:tuple schema/finite schema/finite])
                      (schema/optional :rotation schema/finite)
                      (schema/optional :select [:enum :last :first])))
   :mapping [:map]})

(defn- one? [v] (not (and (vector? v) (not (panel-position? v)))))

(defn- label-positions
  "`[{:x :y :text} ..]` of positions `xs`, `ys` and texts `ts` (vectors of 1 or n, paired)."
  [xs ys ts]
  (mapv (fn [[x y t]] {:x x :y y :text t}) (annotation/broadcast [:x :y :text] [xs ys ts])))

(defn- selected
  "The labels of `labels` (valid positions, data order) kept by `select` (`nil` - all): per label text the one with the
  largest (`:last`) / smallest (`:first`) x key (see [[cljplot2.data.rows/x-key]]; ties - the later label), or the
  last / first in data order when some x has no key."
  [labels select]
  (if-not select
    labels
    (let [by-text (vals (group-by :text labels))
          later? (if (= :last select) >= <=)
          pick (fn [ls]
                 (let [ks (mapv #(data/x-key (:x %)) ls)]
                   (if (every? some? ks)
                     (ls (reduce (fn [best i] (if (later? (double (ks i)) (double (ks best))) i best)) 0 (range (count ls))))
                     (if (= :last select) (peek ls) (first ls)))))]
      (mapv pick by-text))))

(defn- select-placement
  "`:anchor` / `:offset` defaults of `select` (`:last` / `:first`), flipped plots (`flip?`) placing along the vertical."
  [select flip?]
  (case [select (boolean flip?)]
    [:last false] {:anchor [:left :middle] :offset [5 0]}
    [:first false] {:anchor [:right :middle] :offset [-5 0]}
    [:last true] {:anchor [:center :bottom] :offset [0 5]}
    [:first true] {:anchor [:center :top] :offset [0 -5]}))

(defn- label-room
  "`:padding-px` `{:x [lo hi]}` of selected labels `texts` drawn with `options` (style keys, `:select`, `:offset`):
  the widest text (the tallest when `flip?`) plus the offset along x, on the high side for `:last`, the low side for
  `:first`; `nil` without texts."
  [texts options {:keys [theme flip? hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role :annotation} (select-keys options style-keys))
             along (reduce max (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})]
                                              (if flip? (.y size) (.x size))))
                                    texts))
             [dx dy] (:offset options)
             room (+ (double along) (m/abs (double (if flip? dy dx))))]
         {:x (if (= :last (:select options)) [0.0 room] [room 0.0])})))))

(defn compute
  "Literal: `{:labels [{:x :y :text}] :options o}` in every panel, extents of the positions. Data: per panel the
  labels of each group (with `:aesthetics`), with `:select` - one per group and text, the `:anchor` / `:offset`
  defaults in `o` and `:padding-px` room."
  [{:keys [options]} {:keys [mapping] :as chart-split} env]
  (if (annotation/literal? options [:x :y :text])
    (let [_ (when (:select options)
              (schema/fail! "Text :select picks labels from data rows (a :mapping); literal labels are placed as given."
                            {:key :select :value (:select options)}))
          many (fn [k] (annotation/as-vector (get options k) one?))
          _ (doseq [k [:x :y :text]
                    :when (empty? (many k))]
              (schema/fail! (str "Text " k " is required (literal options).") {:key k :value nil}))
          labels (label-positions (many :x) (many :y) (many :text))]
      (annotation/literal-stats {:groups [{:labels labels}] :options options} options
                                {:x (map :x labels) :y (map :y labels)}))
    (let [_ (annotation/require-mapping! "Text" mapping [[:x :y :label]])
          [x-of y-of t-of] (map #(data/row-accessor % (get mapping %)) [:x :y :label])
          select (:select options)
          options (cond->> options select (merge (select-placement select (:flip? env))))
          labels-of (fn [rows] (selected (vec (for [r rows
                                                    :let [x (x-of r) y (y-of r)]
                                                    :when (and (annotation/position? x) (annotation/position? y))]
                                                {:x x :y y :text (t-of r)}))
                                         select))
          stats (annotation/data-stats chart-split options env
                                       (fn [gs]
                                         (let [groups (mapv (fn [g] {:labels (labels-of (:rows g)) :aesthetics (:aesthetics g)}) gs)
                                               ls (mapcat :labels groups)]
                                           [{:groups groups :options options} {:x (map :x ls) :y (map :y ls)}])))]
      (cond-> stats
        select (assoc :padding-px (label-room (distinct (map :text (mapcat :labels (mapcat :groups (vals (:data stats))))))
                                              options env))))))

(defn- base-style
  "Text style keys of `options` over the theme role's colour (the base for the group's `:color`)."
  [options theme]
  (merge {:color (get-in theme [:text :annotation :color])} (select-keys options style-keys)))

(defn draw
  "Draw the labels of one panel through [[cljplot2.content.label/draw-at!]]; unplaceable positions are skipped."
  [_chart {:keys [groups options]} canvas _box scales env]
  (let [anchor (get options :anchor [:center :middle])
        [dx dy] (get options :offset [0 0])
        rotation (get options :rotation 0)
        [hk vk] (if (scale/flipped? scales) [:y :x] [:x :y])
        h-pixel (annotation/axis-pixel-fn (get scales hk))
        v-pixel (annotation/axis-pixel-fn (get scales vk))]
    (doseq [{:keys [labels aesthetics]} groups
            :let [st (group/style (base-style options (:theme env)) aesthetics scales (:theme env))]
            :when st
            {:keys [x y text]} labels
            :let [pos {:x x :y y}
                  px (+ (double (h-pixel (pos hk))) (double dx))
                  py (- (double (v-pixel (pos vk))) (double dy))]
            :when (and (m/valid-double? px) (m/valid-double? py))]
      (label/draw-at! canvas (merge {:kind :label :role :annotation :text (str text) :rotation rotation} st)
                      anchor px py env))))

(defn legend
  "Key glyph: the letter `a` in the text style, centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (base-style options (:theme env)) (select-keys attributes [:color]))]
    (label/draw-at! canvas (merge {:kind :label :role :annotation :text "a"} st) [:center :middle]
                    (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h))) env)))
