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
  `:font`, `:size`, `:font-style`, `:color`, `:inset` (pixels between the text and its box edge), `:background` (a color, alpha allowed, filling the
  text plus its inset - a label box, ggplot `geom_label`; the legend key keeps the background with the role's inset) - over the text role `[:text role]`
  of `:role` (default `:annotation`; the text chart's theme style; a theme `[:chart :text]` entry is rejected); `:extent`, `:data` /
  `:mapping`. Text stays upright when the plot is flipped; only the anchor position enters the extents.
  `:reserve-room true` keeps pixel room past the data on every side (`:padding-px` of x and y) for the labels at any
  angle: the largest label's diagonal plus its offset - labels around a circle stay inside the panel. `:reserve-room
  :anchor` keeps on each side only the room the labels reach past their positions, from `:anchor`, `:offset` and the
  largest label's size (e.g. `[:left :middle]` - the width on the high x side, half the height below and above;
  flipped plots exchange the axes); labels with a `:rotation` or a mapped `:angle` - as `true`.

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
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
  "Gives the `:rotation` option in degrees.

  Parameters:

  - `options` (map): the chart options; `:rotation` - degrees or `:radial` (default 0).

  Returns the rotation; `:radial` (polar only) counts as 0."
  [options]
  (let [r (data/option options :rotation 0)] (if (number? r) r 0)))

(defn- panel-position?
  "Tests whether a position is a panel position `[:panel f]`.

  Parameters:

  - `p`: a position.

  Returns a truthy value for a two-element vector starting with `:panel`."
  [p]
  (and (vector? p) (m/== 2 (count p)) (= :panel (first p))))

(def ^:private panel-position
  "Schema of a panel position `[:panel f]`, `f` in `[0 1]` - a fraction of the panel from its low-value end."
  (schema/named-type ::panel-position #(and (panel-position? %) (number? (second %)) (m/<= 0.0 (double (second %)) 1.0))
                     "should be [:panel f], f in [0 1]"))

(def ^:private text-position
  "Schema of one text position: a data position or a panel position."
  [:or annotation/position panel-position])

(def ^:private text-positions
  "Schema of the literal `:x` / `:y`: a position or a vector of 1 or n; a vector starting with `:panel` is always a panel position, never a vector of positions."
  [:multi {:dispatch #(and (vector? %) (= :panel (first %)))}
   [true panel-position]
   [false (annotation/positions-schema text-position)]])

(def ^:private style-keys
  "Text style keys the chart takes as options, over its text role."
  [:font :size :font-style :color :inset :background])

(defn- text-role
  "Gives the theme text role of the chart's labels.

  Parameters:

  - `options` (map): the chart options.

  Returns `:role`, else `:annotation`."
  [options]
  (data/option options :role :annotation))

(def schemas
  "Schemas of the text chart: `:options`, `:mapping` (merged); the style is the text role `[:text role]` of `:role` (default `:annotation`)."
  {:options (-> [:map {:closed true}]
                (into (filter #(contains? (set style-keys) (first %)) theme/text-style-overrides))
                (into data/common-options)
                (into annotation/options)
                (conj (schema/optional :role {:description "theme text role of the labels (a role of [:text], e.g. a builder's), the style under the inline style keys"
                                              :default :annotation}
                                       keyword?)
                      (schema/nilable :x {:description "literal x position(s): a position, [:panel f] (fraction f of the panel, not in the extents) or a vector of 1 or n (wins over the mapping)"}
                                      text-positions)
                      (schema/nilable :y {:description "literal y position(s): a position, [:panel f] (fraction f of the panel, not in the extents) or a vector of 1 or n (wins over the mapping)"}
                                      text-positions)
                      (schema/nilable :text {:description "literal label text: a value or a vector of 1 or n (converted with str)"} any?)
                      (schema/optional :anchor {:description "point of the text box placed at the position, [h v] (h :left :center :right, v :top :middle :bottom)"
                                                :cljplot2/default-doc "[:center :middle]; with :select just past the point; polar :radial - away from the centre"}
                                       [:tuple [:enum :left :center :right] [:enum :top :middle :bottom]])
                      (schema/optional :offset {:description "shift [dx dy] of the text in pixels, dy up"
                                                :cljplot2/default-doc "[0 0]; with :select 5 pixels past the point"}
                                       [:tuple schema/finite schema/finite])
                      (schema/optional :rotation {:description "text rotation in degrees, clockwise, or :radial (polar: along each label's angle, upright; 0 in cartesian plots) (a mapped :angle wins)"
                                                  :default 0}
                                       [:or schema/finite [:= :radial]])
                      (schema/optional :select {:description "end labels from data: keep per group and text only the row with the largest (:last) or smallest (:first) x"}
                                       [:enum :last :first])
                      (schema/optional :anchor-frame {:description "what :anchor points into: :box (the rotated label's bounding box) or :text (the unrotated text, turned about that point)"
                                                      :default :box}
                                       [:enum :box :text])
                      (schema/optional :reserve-room {:description "keep pixel room past the data for the labels: true - the largest label's diagonal on every side, :anchor - only where the anchored labels reach"
                                                      :default false}
                                       [:or schema/bool [:= :anchor]])))
   :mapping [:map
             (schema/nilable :x {:description "x position of a label, one per row"} schema/accessor)
             (schema/nilable :y {:description "y position of a label, one per row"} schema/accessor)
             (schema/nilable :label {:description "label text, one per row (converted with str)"} schema/accessor)
             (schema/nilable :angle {:description "label rotation in degrees, clockwise (nil or not finite - :rotation)"} schema/accessor)
             (schema/nilable :color {:description "text colour per group (gives legend keys)"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic (labels per group with :select)"} schema/accessor)]})

(defn- one?
  "Tests whether a literal value is one item rather than a vector of items.

  Parameters:

  - `v`: the literal value.

  Returns a truthy value for anything but a vector, and for a panel position."
  [v]
  (not (and (vector? v) (not (panel-position? v)))))

(defn- label-positions
  "Gives the literal labels.

  Parameters:

  - `xs`, `ys`, `ts` (vectors of 1 or n): the positions and texts.

  Returns `[{:x :y :text} ..]`, the vectors paired (see [[cljplot2.data.annotation/broadcast]]).

  Throws as [[cljplot2.data.annotation/broadcast]] for vectors of different lengths."
  [xs ys ts]
  (mapv (fn [[x y t]] {:x x :y y :text t}) (annotation/broadcast [:x :y :text] [xs ys ts])))

(defn- selected
  "Gives the labels kept by `:select`.

  Parameters:

  - `labels` (vector of maps): labels with valid positions, in data order.
  - `select`: `:last`, `:first` or `nil` - all.

  Returns per label text the label with the largest (`:last`) / smallest (`:first`) x key (see [[cljplot2.data.rows/x-key]]; ties - the later label), or the last / first in data order when some x has no key; `labels` for `nil`."
  [labels select]
  (if-not select
    labels
    (let [by-text (vals (group-by :text labels))
          later? (if (= :last select) m/>= m/<=)
          pick (fn [ls]
                 (let [ks (mapv #(data/x-key (:x %)) ls)]
                   (if (every? some? ks)
                     (ls (reduce (fn [best i] (if (later? (double (ks i)) (double (ks best))) i best)) 0 (range (count ls))))
                     (if (= :last select) (peek ls) (first ls)))))]
      (mapv pick by-text))))

(defn- select-placement
  "Gives the `:anchor` / `:offset` defaults of end labels.

  Parameters:

  - `select`: `:last` or `:first`.
  - `flip?`: is the plot flipped (labels placed along the vertical)?

  Returns `{:anchor [h v] :offset [dx dy]}` - just past the point, 5 pixels away."
  [select flip?]
  (case select
    :last (if flip? {:anchor [:center :bottom] :offset [0 5]} {:anchor [:left :middle] :offset [5 0]})
    :first (if flip? {:anchor [:center :top] :offset [0 -5]} {:anchor [:right :middle] :offset [-5 0]})))

(defn- label-room
  "Gives the pixel room end labels need past the data.

  Parameters:

  - `texts` (sequence): the selected labels' texts.
  - `options` (map): style keys, `:role`, `:rotation`, `:select`, `:offset`.
  - `env` (map): `:theme`, `:flip?`, `:hint`.

  Returns `:padding-px` `{:x [lo hi]}`: the widest text (the tallest when flipped) plus the offset along x, on the high side for `:last`, the low side for `:first`; `nil` without texts."
  [texts options {:keys [theme flip? hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation (numeric-rotation options)} (select-keys options style-keys))
             along (v/mx (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})]
                                        (if flip? (.y size) (.x size))))
                              texts))
             [dx dy] (:offset options)
             room (m/+ along (m/abs (double (if flip? dy dx))))]
         {:x (if (= :last (:select options)) [0.0 room] [room 0.0])})))))

(defn- reserved-room
  "Gives the pixel room labels need at any angle.

  Parameters:

  - `texts` (sequence): the labels' texts.
  - `options` (map): style keys, `:role`, `:offset`.
  - `env` (map): `:theme`, `:hint`.

  Returns `:padding-px` `{:x [r r] :y [r r]}`, `r` - the largest text's diagonal (unrotated) plus the larger offset component; `nil` without texts."
  [texts options {:keys [theme hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation 0} (select-keys options style-keys))
             diagonal (v/mx (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})]
                                           (m/hypot (.x size) (.y size))))
                                 texts))
             [dx dy] (data/option options :offset [0 0])
             r (m/+ diagonal (m/max (m/abs (double dx)) (m/abs (double dy))))]
         {:x [r r] :y [r r]})))))

(defn- anchored-room
  "Gives the pixel room labels reach past their positions as anchored.

  Parameters:

  - `texts` (sequence): the labels' texts.
  - `options` (map): style keys, `:role`, `:anchor`, `:offset` (no rotation).
  - `env` (map): `:theme`, `:hint`, `:flip?` - x room on the vertical scale and y room on the horizontal one.

  Returns `:padding-px` `{:x [lo hi] :y [lo hi]}`: per side the part of the largest text box (width and height measured separately) past the position; `nil` without texts."
  [texts options {:keys [theme hint flip?]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation 0} (select-keys options style-keys))
             sizes (map (fn [t] (measure/natural-size (assoc spec :text (str t)) {:canvas c :theme theme})) texts)
             w (v/mx (map (fn [^Vec2 size] (.x size)) sizes))
             h (v/mx (map (fn [^Vec2 size] (.y size)) sizes))
             [ha va] (data/option options :anchor [:center :middle])
             [dx dy] (data/option options :offset [0 0])
             x0 (m/- (double dx) (case ha :left 0.0 :center (m/* 0.5 w) :right w))
             y0 (m/- (double dy) (case va :bottom 0.0 :middle (m/* 0.5 h) :top h))
             horizontal [(m/max 0.0 (m/- x0)) (m/max 0.0 (m/+ x0 w))]
             vertical [(m/max 0.0 (m/- y0)) (m/max 0.0 (m/+ y0 h))]]
         (if flip? {:x vertical :y horizontal} {:x horizontal :y vertical}))))))

(defn- with-reserved-room
  "Adds the room `:reserve-room` asks for to chart stats.

  Parameters:

  - `stats` (map): the chart's stats.
  - `options` (map): the chart options (`:reserve-room`, `:rotation`, ...).
  - `mapping` (map or `nil`): the chart's mapping (`:angle`).
  - `env` (map): the compute environment.

  Returns `stats` with [[reserved-room]] (`:reserve-room :anchor` without a rotation or a mapped angle - [[anchored-room]]) for the texts of its labels merged into its `:padding-px` (per side the larger); `stats` unchanged without `:reserve-room`."
  [stats options mapping env]
  (if-not (:reserve-room options)
    stats
    (let [texts (distinct (map :text (mapcat (fn [panel] (mapcat :labels (:groups panel))) (vals (:data stats)))))
          anchored? (and (= :anchor (:reserve-room options)) (nil? (:angle mapping))
                         (contains? #{nil 0 0.0} (:rotation options)))
          room (if anchored? (anchored-room texts options env) (reserved-room texts options env))]
      (cond-> stats
        room (update :padding-px #(merge-with (fn [a b] (mapv m/max a b)) % room))))))

(defn compute
  "Computes the labels of the chart.

  Parameters:

  - `chart` (map): the chart value (`:options` - literal `:x`, `:y`, `:text`, `:select`, ...).
  - `chart-split` (map): `:groups`, `:mapping` (the chart's own: `:x`, `:y`, `:label`, `:angle`, aesthetics).
  - `env` (map): the compute environment (`:flip?`, `:theme`, `:hint` ...).

  Returns literal - `{:groups [{:labels [{:x :y :text}]}] :options o}` in every panel, extents of the positions; data - per panel the labels of each group (with `:aesthetics`), with `:select` one per group and text, the `:anchor` / `:offset` defaults in `o` and `:padding-px` room; `:reserve-room` room (see [[with-reserved-room]]).

  Throws `ex-info {:key :select}` for `:select` with literal labels, `{:key k}` for a missing literal `:x`, `:y` or `:text`, `{:key :mapping}` without a mapped `:x :y :label`."
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
        options nil env))
    (let [_ (annotation/require-mapping! "Text" mapping [[:x :y :label]])
          [x-of y-of t-of] (map #(data/row-accessor % (get mapping %)) [:x :y :label])
          angle-of (when (some? (:angle mapping)) (data/row-accessor :angle (:angle mapping)))
          select (:select options)
          placed-options (cond->> options select (merge (select-placement select (:flip? env))))
          labels-of (fn [rows] (selected (vec (for [r rows
                                                    :let [x (x-of r) y (y-of r)]
                                                    :when (and (annotation/position? x) (annotation/position? y))]
                                                (cond-> {:x x :y y :text (t-of r)}
                                                  angle-of (assoc :angle (angle-of r)))))
                                         select))
          stats (annotation/data-stats chart-split placed-options env
                                       (fn [gs]
                                         (let [groups (mapv (fn [g] {:labels (labels-of (:rows g)) :aesthetics (:aesthetics g)}) gs)
                                               ls (mapcat :labels groups)]
                                           [{:groups groups :options placed-options} {:x (map :x ls) :y (map :y ls)}]))
                                       {:x #(not (annotation/position? (x-of %))) :y #(not (annotation/position? (y-of %)))})]
      (-> (cond-> stats
            select (assoc :padding-px (label-room (distinct (map :text (mapcat :labels (mapcat :groups (vals (:data stats))))))
                                                  placed-options env)))
          (with-reserved-room placed-options mapping env)))))

(defn- base-style
  "Gives the base text style of the labels, the group's `:color` drawn over it.

  Parameters:

  - `options` (map): the chart options (style keys, `:role`).
  - `theme` (map): the merged theme.

  Returns the colour of the chart's text role with the style keys of `options` over it."
  [options theme]
  (merge {:color (get-in theme [:text (text-role options) :color])} (select-keys options style-keys)))

(defn draw
  "Draws the labels of one panel.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:groups` (labels per group) and `:options`.
  - `canvas`: the clojure2d canvas.
  - `_box` (map): the panel box.
  - `scales` (map): the finalized scales.
  - `env` (map): the drawing environment (`:theme`).

  Labels are drawn through [[cljplot2.content.label/draw-at!]] (`:anchor-frame :text` - [[cljplot2.content.label/draw-at-frame!]]), turned by a mapped `:angle`, else `:rotation` (`:radial` along each label's angle in a polar plot); unplaceable positions are skipped."
  [_chart {:keys [groups options]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        radial? (and geometry (= :radial (:rotation options)))
        [^double dx ^double dy] (data/option options :offset [0 0])
        [hk vk] (if (scale/flipped? scales) [:y :x] [:x :y])
        h-pixel (annotation/axis-pixel-fn (get scales hk))
        v-pixel (annotation/axis-pixel-fn (get scales vk))
        ;; range position -> [pixel x, pixel y, rotation, anchor]
        place (if geometry
                (fn [^double h ^double v]
                  (let [^Vec2 p (coord/to-pixel geometry h v)]
                    [(m/+ (.x p) dx) (m/- (.y p) dy)
                     (if radial? (coord/radial-rotation h) (numeric-rotation options))
                     (data/option options :anchor (if radial? (coord/outward-anchor h) [:center :middle]))]))
                (let [rotation (numeric-rotation options)
                      anchor (data/option options :anchor [:center :middle])]
                  (fn [^double h ^double v] [(m/+ h dx) (m/- v dy) rotation anchor])))]
    (doseq [{:keys [labels aesthetics]} groups
            :let [st (group/style (base-style options (:theme env)) aesthetics scales (:theme env))]
            :when st
            {:keys [x y text angle]} labels
            :let [pos {:x x :y y}
                  [^double px ^double py placed-rotation anchor] (place (double (h-pixel (pos hk))) (double (v-pixel (pos vk))))
                  rotation (if (schema/finite? angle) angle placed-rotation)]
            :when (and (m/valid-double? px) (m/valid-double? py))]
      ((if (= :text (:anchor-frame options)) label/draw-at-frame! label/draw-at!)
       canvas (merge {:kind :label :role (text-role options) :text (str text) :rotation rotation} st) anchor px py env))))

(defn legend
  "Draws the key glyph: the letter `a` in the text style (the role's inset, so it fits the key), centred in the key box.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `:x`, `:y`, `:w`, `:h`.
  - `env` (map): the drawing environment (`:theme`)."
  [{:keys [options]} attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (merge (dissoc (base-style options (:theme env)) :inset) (select-keys attributes [:color]))]
    (label/draw-at! canvas (merge {:kind :label :role (text-role options) :text "a"} st) [:center :middle]
                    (m/+ x (m/* 0.5 w)) (m/+ y (m/* 0.5 h)) env)))
