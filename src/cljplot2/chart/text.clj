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
  flipped, `:rotation` included - and the offset).

  Options: `:anchor` - the point of the text box placed at the position, `[h v]` (`h` one of `:left :center :right`,
  `v` one of `:top :middle :bottom`; default `[:center :middle]`); `:offset [dx dy]` - pixels, `dy` up (default
  `[0 0]`); `:rotation` - degrees, clockwise (default 0; the anchor applies to the rotated text's bounding box), or `:radial` -
  in a polar plot each label along its angle, labels on the left half turned to read upright, anchored away from the
  centre unless `:anchor` is given (0 in cartesian plots);
  A mapped `:angle` (degrees, clockwise; `nil` or not a finite number - `:rotation`) turns each label on its own,
  over `:rotation` and `:radial`. `:anchor-frame` - `:box` (default: `:anchor` is a point of the rotated label's
  bounding box) or `:text` (a point of the unrotated text, the text turned about it - `[:left :middle]` starts every
  label at its position whatever its angle, as radial labels around a circle).
  `:font`, `:size`, `:font-style`, `:color`, `:padding` (pixels around the text - unlike other charts' position
  `:padding {:x [lo hi]}`), `:background` (a color, alpha allowed, filling the text plus padding - a label box, ggplot
  `geom_label`; the legend key keeps the background with the theme's padding) - over the text role `[:text :annotation]`
  (the text chart's theme style; a theme `[:chart :text]` entry is rejected); `:extent`, `:data` /
  `:mapping`. Text stays upright when the plot is flipped; only the anchor position enters the extents.
  `:reserve-room true` keeps pixel room past the data on every side (`:padding-px` of x and y) for the labels at any
  angle: the largest label's diagonal plus its offset - labels around a circle stay inside the panel."
  (:require [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- numeric-rotation
  "The `:rotation` option in degrees: `:radial` (polar only) counts as 0."
  [options]
  (let [r (get options :rotation 0)] (if (number? r) r 0)))

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

(def ^:private style-keys [:font :size :font-style :color :padding :background])

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
                      (schema/optional :rotation [:or schema/finite [:= :radial]])
                      (schema/optional :select [:enum :last :first])
                      (schema/optional :anchor-frame [:enum :box :text])
                      (schema/optional :reserve-room schema/bool)))
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
  "`:padding-px` `{:x [lo hi]}` of selected labels `texts` drawn with `options` (style keys, `:rotation`, `:select`, `:offset`):
  the widest text (the tallest when `flip?`) plus the offset along x, on the high side for `:last`, the low side for
  `:first`; `nil` without texts."
  [texts options {:keys [theme flip? hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role :annotation :rotation (numeric-rotation options)} (select-keys options style-keys))
             along (reduce max (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})]
                                              (if flip? (.y size) (.x size))))
                                    texts))
             [dx dy] (:offset options)
             room (+ (double along) (m/abs (double (if flip? dy dx))))]
         {:x (if (= :last (:select options)) [0.0 room] [room 0.0])})))))

(defn- reserved-room
  "`:padding-px` `{:x [r r] :y [r r]}` for labels `texts` drawn with `options` at any angle: `r` - the largest text's
  diagonal (unrotated, style keys of `options`) plus the larger offset component; `nil` without texts."
  [texts options {:keys [theme hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role :annotation :rotation 0} (select-keys options style-keys))
             diagonal (reduce max (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})]
                                                 (m/hypot (.x size) (.y size))))
                                       texts))
             [dx dy] (:offset options [0 0])
             r (+ (double diagonal) (max (m/abs (double dx)) (m/abs (double dy))))]
         {:x [r r] :y [r r]})))))

(defn- with-reserved-room
  "Chart `stats` with [[reserved-room]] for the texts of its labels merged into its `:padding-px` (per side the
  larger) when `options` ask for `:reserve-room`."
  [stats options env]
  (if-not (:reserve-room options)
    stats
    (let [texts (distinct (map :text (mapcat (fn [panel] (mapcat :labels (:groups panel))) (vals (:data stats)))))
          room (reserved-room texts options env)]
      (cond-> stats
        room (update :padding-px #(merge-with (fn [a b] (mapv max a b)) % room))))))

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
      (with-reserved-room (annotation/literal-stats {:groups [{:labels labels}] :options options} options
                                                    {:x (map :x labels) :y (map :y labels)})
        options env))
    (let [_ (annotation/require-mapping! "Text" mapping [[:x :y :label]])
          [x-of y-of t-of] (map #(data/row-accessor % (get mapping %)) [:x :y :label])
          angle-of (when (some? (:angle mapping)) (data/row-accessor :angle (:angle mapping)))
          select (:select options)
          options (cond->> options select (merge (select-placement select (:flip? env))))
          labels-of (fn [rows] (selected (vec (for [r rows
                                                    :let [x (x-of r) y (y-of r)]
                                                    :when (and (annotation/position? x) (annotation/position? y))]
                                                (cond-> {:x x :y y :text (t-of r)}
                                                  angle-of (assoc :angle (angle-of r)))))
                                         select))
          stats (annotation/data-stats chart-split options env
                                       (fn [gs]
                                         (let [groups (mapv (fn [g] {:labels (labels-of (:rows g)) :aesthetics (:aesthetics g)}) gs)
                                               ls (mapcat :labels groups)]
                                           [{:groups groups :options options} {:x (map :x ls) :y (map :y ls)}])))]
      (-> (cond-> stats
            select (assoc :padding-px (label-room (distinct (map :text (mapcat :labels (mapcat :groups (vals (:data stats))))))
                                                  options env)))
          (with-reserved-room options env)))))

(defn- base-style
  "Text style keys of `options` over the theme role's colour (the base for the group's `:color`)."
  [options theme]
  (merge {:color (get-in theme [:text :annotation :color])} (select-keys options style-keys)))

(defn draw
  "Draw the labels of one panel through [[cljplot2.content.label/draw-at!]]; unplaceable positions are skipped."
  [_chart {:keys [groups options]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        radial? (and geometry (= :radial (:rotation options)))
        [dx dy] (get options :offset [0 0])
        [hk vk] (if (scale/flipped? scales) [:y :x] [:x :y])
        h-pixel (annotation/axis-pixel-fn (get scales hk))
        v-pixel (annotation/axis-pixel-fn (get scales vk))
        ;; range position -> [pixel x, pixel y, rotation, anchor]
        place (if geometry
                (fn [^double h ^double v]
                  (let [^Vec2 p (coord/to-pixel geometry h v)]
                    [(+ (.x p) (double dx)) (- (.y p) (double dy))
                     (if radial? (coord/radial-rotation h) (numeric-rotation options))
                     (get options :anchor (if radial? (coord/outward-anchor h) [:center :middle]))]))
                (let [rotation (numeric-rotation options)
                      anchor (get options :anchor [:center :middle])]
                  (fn [^double h ^double v] [(+ h (double dx)) (- v (double dy)) rotation anchor])))]
    (doseq [{:keys [labels aesthetics]} groups
            :let [st (group/style (base-style options (:theme env)) aesthetics scales (:theme env))]
            :when st
            {:keys [x y text angle]} labels
            :let [pos {:x x :y y}
                  [px py rotation anchor] (place (double (h-pixel (pos hk))) (double (v-pixel (pos vk))))
                  rotation (if (and (number? angle) (m/valid-double? (double angle))) angle rotation)]
            :when (and (m/valid-double? (double px)) (m/valid-double? (double py)))]
      ((if (= :text (:anchor-frame options)) label/draw-at-frame! label/draw-at!)
       canvas (merge {:kind :label :role :annotation :text (str text) :rotation rotation} st) anchor px py env))))

(defn legend
  "Key glyph: the letter `a` in the text style (the role's padding, so it fits the key), centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (dissoc (base-style options (:theme env)) :padding) (select-keys attributes [:color]))]
    (label/draw-at! canvas (merge {:kind :label :role :annotation :text "a"} st) [:center :middle]
                    (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h))) env)))
