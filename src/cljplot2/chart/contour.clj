(ns cljplot2.chart.contour
  "Contour: lines (or filled regions) where a 2D field crosses levels - a kernel density of points, an interpolated
  gridded `:value` or a function of a point `[x y]` (see [[cljplot2.data.field]] for the sources and their options), sampled
  on a grid over the domain of every panel together (cartesian plots only; marching squares, see
  [[cljplot2.data.contour]]). Vertices are projected one by one: lines follow non-linear scales.

  Options (style defaults from theme `[:chart :contour]`):

  * `:levels` - an integer: that many levels evenly spread over the range of every sampled value (the ends left
    out; default 10), or a vector of level values
  * `:filled` - `true`: fill the region at or above each level (lowest first, so the bands stack; holes stay open)
    instead of drawing lines; each band colored by its middle value (between its level and the next, the top band up
    to the largest value); grouped filled contours are translucent by default (alpha factor × 0.5), the groups overlap
  * colors: by default each level its color through the plot's color scale over the field's value range (a computed
    color, legend titled like [[cljplot2.chart.field]]; the lowest level isn't the gradient's end); option `:color` -
    one color for every line; a grouping mapping (`:color` with
    categorical values, `:group`, other ordinal aesthetics) - a field per group (points / gridded data of the group)
    with its lines in the group's style, levels shared by the groups
  * `:line-width`, `:dash`, `:alpha-factor` (multiplies the colours' alpha, a fraction)
  * `:labels` (line contours only; default none) - `true`: each line labelled with its level (a number to at most 2
    decimals, see [[cljplot2.content.label/value-text]]), or a function of `{:level v}` returning the text (`nil` - no
    label, e.g. every second level): horizontal text centred halfway along the line (by its pixel length), text role
    `:contour-label` (a translucent background over the line, which isn't cut); a line shorter than twice its label
    gets none, a label leaving the panel or overlapping one already drawn is skipped, lines not drawn (no color) none (lowest levels first); labels are drawn after all
    lines. With `:filled true` - `ex-info {:key :labels}`.

  On a `:log` axis the field is sampled (a density estimated) in log10 coordinates, see [[cljplot2.data.field]].

  Functions: [[computed-aesthetics]] - the computed color; [[compute]], [[draw]], [[legend]] - the chart contract; [[line-midpoint]] - where a line's label goes."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.common :as common]
            [cljplot2.content.label :as label]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.text :as text]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Style entries of the contour chart (theme `[:chart :contour]`): its own `:color`, the line style without its colour, `:alpha-factor`."
  (-> [(schema/optional :color {:description "One colour for every line or band, instead of colouring them by level; a mapped `:color` wins."
                                :cljplot2/default-doc "Lines coloured by level; grouped lines without a mapped colour take the theme's colour, lines grouped by `:color` their group's."
                                :cljplot2/value-meanings {:none "Nothing drawn, unless `:color` is mapped."} :cljplot2/example :black}
                        schema/color)]
      (into (remove #(= :color (first %))) polyline/style-entries)
      (conj (schema/optional :alpha-factor {:description "How opaque the lines or bands are: a factor on the colours' own alpha."
                                            :cljplot2/default-doc "The theme's; grouped filled contours take half of it." :cljplot2/example 0.3}
                             schema/fraction))))

(def schemas
  "Schemas of the contour chart: `:style` (theme `[:chart :contour]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into field/source-options)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (conj (schema/optional :levels {:description "The values at which the chart draws contours."
                                                 :default 10 :cljplot2/example 4
                                                 :cljplot2/form-meanings {:int "That many levels, evenly spread over the range of the sampled values, the ends left out."
                                                                          :vector "The level values, e.g. `[0.1 0.2 0.4]`."}}
                                        [:or [:int {:min 1 :max 1000}] [:vector {:min 1} schema/finite]])
                       (schema/optional :filled {:description "Whether the chart fills bands between levels instead of drawing lines."
                                                 :default false
                                                 :cljplot2/value-meanings {true "Fills the region at or above each level; coloured by level, each band takes the colour of its middle value." false "Draws lines."}}
                                        :boolean)
                       (schema/nilable :labels {:description "Writes its level on each line; filled contours reject the option."
                                                :default false
                                                :cljplot2/value-meanings {true "The level as a number." false "No labels."}
                                                :cljplot2/form-meanings {::schema/function "A function of `{:level v}` that returns the text; `nil` writes none."}}
                                       [:or :boolean schema/function])))
             [#(when (and (:labels %) (:filled %)) "labels need line contours, not :filled true") [:labels]])
   :mapping [:maybe [:map
                     (schema/nilable :x {:description "The x of the points whose density the chart estimates, or of the gridded values."} schema/accessor)
                     (schema/nilable :y {:description "The y of the points whose density the chart estimates, or of the gridded values."} schema/accessor)
                     (schema/nilable :value {:description "The field's value at each point of a full lattice of x and y, interpolated between them; unmapped, the field is the density of the points."}
                                     schema/accessor)
                     (schema/nilable :color {:description "Splits the rows into categorical groups, each its own field with lines in its colour; unmapped, the lines are coloured by level."}
                                     schema/accessor)
                     (schema/nilable :dash {:description "Splits the rows into groups, each its own field with lines in its dash style."} schema/accessor)
                     (schema/nilable :group {:description "Splits the rows into groups that look the same, each its own field."} schema/accessor)]]})

(def ^{:private true :const true :tag 'double} grouped-fill-alpha
  "Alpha factor of grouped filled contours without an `:alpha-factor` option: the groups' regions overlap."
  0.5)

(defn- grouped?
  "Tests whether a merged mapping groups the chart: a mapped aesthetic or `:group`.

  Parameters:

  - `mapping` (map): the merged mapping.

  Returns the grouping dimensions (truthy), else `nil`."
  [mapping]
  (seq (group/group-dims mapping)))

(defn computed-aesthetics
  "Gives the contour chart's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color title}` - lines (or bands) coloured by level, titled as the field's value (see [[cljplot2.data.field/value-title]]); `{}` with a fixed `:color` option or a grouping mapping (the groups colour the lines)."
  [options mapping]
  (if (or (:color options) (grouped? mapping))
    {}
    {:color (field/value-title options mapping)}))

(defn- group-fields
  "Gives the fields of a contour chart: one per panel, or per group in a panel when grouped.

  Parameters:

  - `options` (map): the chart's options (the source, see [[cljplot2.data.field]]).
  - `mapping` (map): the merged mapping.
  - `groups` (map): `{panel rows}`.
  - `env` (map): the compute environment (`:plot-scales`, grouping settings).

  Returns a sequence of `[panel aesthetics field]` (`aesthetics` - `{}` ungrouped) for every panel or group with rows; `field` is `nil` when none can be made (e.g. one point under a bandwidth selector, see [[cljplot2.data.field/density]]). A function field isn't grouped (every panel gets it)."
  [options mapping groups env]
  (let [kind (field/source-kind options mapping)
        plot-scales (:plot-scales env)]
    (if (and (grouped? mapping) (not= kind :function))
      (let [{:keys [panels]} (group/split groups mapping env)]
        (for [[panel gs] panels {:keys [aesthetics rows]} gs
              :when (seq rows)]
          [panel aesthetics (field/source-field kind options mapping rows plot-scales)]))
      (for [[panel rows] groups
            :when (or (= kind :function) (seq rows))]
        [panel {} (field/source-field kind options mapping rows plot-scales)]))))

(defn compute
  "Computes the contour chart's stats: the lines (or filled regions) of every level, per panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:levels`, `:filled`, `:labels`, the source and the style).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged).
  - `env` (map): `:theme`, `:plot-scales` and grouping settings.

  Returns per panel `{:contours [{:level l :color-value v :aesthetics a :lines [{:points :closed} ..]} ..] :style st :filled b :by-level b :labels l}` (filled: `:rings [[..] ..]` instead of `:lines`); extents x / y - the sampled domain, color - the value range with the levels (by level: the lowest line or band isn't the gradient's end), or the groups' values; no padding, `:na`, `:dropped` (rows without finite x / y; `:field` - panels, or groups when grouped, with rows but no field).

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite `:x`, `:y` (or gridded `:value`), see [[cljplot2.data.field/check-mapped!]].

  See also [[cljplot2.data.contour/isolines]], [[cljplot2.data.contour/level-rings]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (field/check-mapped! (field/source-kind options mapping) mapping groups)
  (let [items (group-fields options mapping groups env)
        ;; read by the domain, the sampling, the extents and the NA check
        fields (filterv #(some? (nth % 2)) items)
        domain (field/union-domain (map (comp :domain #(nth % 2)) fields))
        size (field/grid-size (:n options))
        transforms (field/axis-transforms plot-scales)
        sampled (mapv (fn [[panel aesthetics {:keys [f]}]] [panel aesthetics (field/sample f domain size transforms)]) fields)
        [lo hi :as value-range] (field/value-range (map #(nth % 2) sampled))
        levels (contour/levels value-range (data/option options :levels 10))
        filled (true? (:filled options))
        by-level (not (or (:color options) (grouped? mapping)))
        ;; a filled band's color - the middle of its level and the next (the top band: up to the largest value)
        color-values (if (and filled (seq levels))
                       (mapv (fn [^double l ^double nxt] (m/* 0.5 (m/+ l nxt))) levels (concat (rest levels) [(m/max (double hi) (double (peek levels)))]))
                       levels)
        in-data (fn [grid points] (mapv #(field/to-data grid %) points))
        panels (reduce (fn [acc [panel aesthetics grid]]
                         (update acc panel (fnil into [])
                                 (for [[level color-value] (map vector levels color-values)]
                                   (if filled
                                     {:level level :color-value color-value :aesthetics aesthetics
                                      :rings (mapv #(in-data grid %) (contour/level-rings grid level))}
                                     {:level level :color-value color-value :aesthetics aesthetics
                                      :lines (mapv #(update % :points (partial in-data grid)) (contour/isolines grid level))}))))
                       {} sampled)
        st (cond-> (theme/resolved-style theme chart)
             (and filled (grouped? mapping) (nil? (:alpha-factor options))) (update :alpha-factor (fn [factor] (m/* grouped-fill-alpha (double (or factor 1.0))))))
        [[x0 x1] [y0 y1]] domain
        group-extents (when (grouped? mapping)
                        (group/aesthetic-extents (update-vals (group-by first fields)
                                                              (fn [fs] (map (fn [[_ a]] {:aesthetics a}) fs)))
                                                 plot-scales))]
    {:data (update-vals panels (fn [cs] {:contours cs :style st :filled filled :by-level by-level :labels (:labels options)}))
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {:x {:kind :numeric :min (double x0) :max (double x1)}
                                              :y {:kind :numeric :min (double y0) :max (double y1)}}
                                       (and by-level (seq levels))
                                       ;; the value range (with any given level outside it): the lowest line or band
                                       ;; isn't the gradient's end (a near-white line on a light panel)
                                       (assoc :color {:kind :numeric :min (m/min (double lo) (double (first levels)))
                                                      :max (m/max (double hi) (double (peek levels)))})
                                       group-extents (merge (get group-extents panel)))]))
                    (keys panels))
     :padding data/zero-padding
     :na (if (some (fn [[_ a]] (some nil? (vals a))) fields) (set (for [[_ a] fields [k v] a :when (nil? v)] k)) #{})
     :dropped (assoc (field/dropped-positions (field/source-kind options mapping) mapping groups)
                     :field (count (remove #(some? (nth % 2)) items)))}))

(defn- level-style
  "Gives the line or fill style of one contour.

  Parameters:

  - `contour` (map): `:color-value` (the level, or a filled band's middle), `:aesthetics` (its group's).
  - `st` (map): the chart's style.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.
  - `by-level`: colour by the value through the colour scale.

  Returns the style: by level - `st` with the value's colour (`nil` for a value outside the colour scale); else the group's style over `st` (see [[cljplot2.data.group/style]])."
  [{:keys [color-value aesthetics]} st scales env by-level]
  (if by-level
    (when-let [col (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
      (some->> (col color-value) (assoc st :color)))
    (group/style st aesthetics scales (:theme env))))

(defn line-midpoint
  "Finds the point halfway along a pixel polyline.

  Parameters:

  - `points` (vector of `[x y]`): the polyline's pixel points.

  Returns `[point length]`: the point halfway along the polyline (by length) and its total length; `nil` for fewer than two points or a non-finite one."
  [points]
  (when (and (m/>= (count points) 2) (every? data/finite-point? points))
    (let [segments (vec (polyline/segment-lengths points))
          last-segment (m/long-dec (count segments))
          total (v/sum segments)
          half (m/* 0.5 total)]
      (loop [i 0
             walked 0.0]
        (let [len (double (segments i))
              [^double x0 ^double y0] (points i)
              [^double x1 ^double y1] (points (m/long-inc i))]
          (if (or (m/>= (m/+ walked len) half) (m/== i last-segment))
            (let [t (if (m/pos? len) (m/constrain (m// (m/- half walked) len) 0.0 1.0) 0.0)]
              [[(m/+ x0 (m/* t (m/- x1 x0))) (m/+ y0 (m/* t (m/- y1 y0)))] total])
            (recur (m/long-inc i) (m/+ walked len))))))))

(defn- level-text
  "Gives the label text of a contour level.

  Parameters:

  - `labels`: the `:labels` option - `true` or a function of `{:level v}`.
  - `level` (number): the level.

  Returns the function's result as text (`nil` - no label; a formula or runs value kept), for `true` the level as a number of at most 2 decimals (see [[cljplot2.content.label/value-text]])."
  [labels level]
  (if (fn? labels) (some-> (labels {:level level}) text/label-text) (label/value-text level)))

(defn- inside?
  "Tests whether a pixel box lies within the panel.

  Parameters:

  - `box` (map): the pixel box `{:x :y :w :h}`.
  - `panel` (map): the panel box `{:w :h}`, at the origin.

  Returns true when the box is inside the panel, edges included."
  [{:keys [^double x ^double y ^double w ^double h]} {panel-w :w panel-h :h}]
  (and (m/>= x 0.0) (m/>= y 0.0)
       (m/<= (m/+ x w) (double panel-w)) (m/<= (m/+ y h) (double panel-h))))

(defn- draw-labels!
  "Draws the level labels of line contours, lowest level first.

  Each label is centred halfway along its line (see [[line-midpoint]]); a line shorter than twice its label gets none, a label leaving the panel or overlapping one already drawn is skipped.

  Parameters:

  - `canvas`: the panel's canvas.
  - `contours` (sequence): the drawn line contours, `{:level :lines}`.
  - `to-pixels` (function): data points to a vector of pixel points.
  - `labels`: the `:labels` option (see [[level-text]]).
  - `env` (map): the drawing environment; `:panel` - the panel box.

  Returns the boxes of the labels drawn."
  [canvas contours to-pixels labels {:keys [panel] :as env}]
  (reduce (fn [drawn {:keys [level lines]}]
            (if-let [text (level-text labels level)]
              (let [content {:kind :label :role :contour-label :text text}]
                (reduce (fn [kept {:keys [points closed]}]
                          (if-let [[[x y] ^double length] (line-midpoint (to-pixels (if closed (conj points (first points)) points)))]
                            (let [box (label/box-at canvas content [:center :middle] x y env)]
                              (if (or (m/< length (m/* 2.0 (double (:w box)))) (not (inside? box panel))
                                      (some #(measure/boxes-overlap? box %) kept))
                                kept
                                (do (label/draw-at! canvas content [:center :middle] x y env)
                                    (conj kept box))))
                            kept))
                        drawn lines))
              drawn))
          [] (sort-by :level contours)))

(defn draw
  "Draws one panel's contours: filled regions lowest level first, or lines (then their `:labels`).

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats (see [[compute]]).
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box (labels stay inside it).
  - `scales` (map): the panel's finalized scales.
  - `env` (map): the drawing environment.

  A contour whose colour is outside the colour scale or `:none` isn't drawn and gets no label."
  [_chart {:keys [contours style filled by-level labels]} canvas box scales env]
  (let [projection (scale/projection scales)
        to-pixels (fn [points] (mapv (fn [[x y]] (let [^fastmath.vector.Vec2 p (projection x y)] [(.x p) (.y p)])) points))
        alpha-factor (:alpha-factor style)
        ;; contours with a drawn color, lowest level first: only their lines get labels
        visible (keep (fn [contour]
                        (let [st (level-style contour style scales env by-level)]
                          (when (and st (not (theme/none? (:color st)))) [contour st])))
                      (sort-by :level contours))]
    (doseq [[contour st] visible]
      (if filled
        (do (c2d/set-color canvas (cond-> (:color st) alpha-factor (draw/faded alpha-factor)))
            (c2d/shape canvas (polyline/rings-path (map to-pixels (:rings contour))) false))
        (polyline/stroke-runs! canvas
                               (map (fn [{:keys [points closed]}] (to-pixels (if closed (conj points (first points)) points)))
                                    (:lines contour))
                               (assoc st :color (cond-> (:color st) alpha-factor (draw/faded alpha-factor))))))
    (when (and labels (not filled))
      (draw-labels! canvas (map first visible) to-pixels labels (assoc env :panel box)))))

(defn legend
  "Draws the contour chart's key glyph.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the legend entry's mapped attributes (`:color`, `:dash`).
  - `canvas`: the legend's canvas.
  - `box` (map): the key box.
  - `env` (map): the drawing environment (`:theme`).

  Draws a line in the key's colour and the chart's line style; for filled contours a filled square."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :dash])]
    (if (:filled options)
      (draw/bar-key! canvas box {:color (:color st) :stroke-width 0} false)
      (polyline/key-glyph! canvas box st :none))))
