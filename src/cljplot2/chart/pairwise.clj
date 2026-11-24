(ns cljplot2.chart.pairwise
  "Pairwise statistic: a statistic of the panel's `:x` / `:y` samples - an error, an association, an effect size, a distance (see [[cljplot2.data.pairwise/statistics]]) or a user function - drawn as one cell filling the panel; with [[cljplot2.multiplot/pairs]] a matrix of them (lower cells - the statistic of the column variable against the row variable, upper cells - of the row's against the column's, so an asymmetric statistic shows both orders).

  Unlike [[cljplot2.chart.correlation]] the values have no fixed range: a colour shows the value on the colour scale's domain - `:domain`, else the values' range over the panels (in `pairs` the matrix's range, shared by every cell).

  Mapping: numeric `:x`, `:y`; a pair takes the rows where both are finite (pairwise-complete rows, see [[cljplot2.data.pairwise/pair-samples]]); fewer than 2 rows, a non-finite result or a statistic that throws give no value (`NA`, counted in `:dropped :value`).

  `:glyph`:

  * `:number` (default) - `<label>: value` (text role `:pairwise`, shrunk to fit a narrow panel); with a grouping mapping (`:group`, ordinal aesthetics such as a categorical `:color`) a line per group under it, in the group's style
  * `:shade` - the whole panel in the value's colour
  * `:shade-number` - both: the number over the shade, black or white by the fill (text role colour `:contrast`)

  Options (style default from theme `[:chart :pairwise]`): `:color` (one fixed colour), `:method`, `:label`, `:glyph`, `:domain`, `:format`.

  Functions: [[computed-aesthetics]] - the computed colour; [[with-shared-domain]] - the domain `pairs` shares; [[compute]], [[draw]], [[legend]] - the chart contract."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def glyphs
  "The glyphs a value is drawn as (option `:glyph`)."
  [:number :shade :shade-number])

(def ^:private style-entries
  "Style entries of the pairwise chart (theme `[:chart :pairwise]`)."
  [(schema/optional :color {:description "one fixed colour: the shade's, the :number text's"
                             :cljplot2/default-doc (str "ungrouped shades: the value on the colour scale; grouped shades: the theme's; :number: the "
                                                        ":pairwise text role's colour, group lines mapped")}
                    schema/color)])

(def schemas
  "Schemas of the pairwise chart: `:style` (theme `[:chart :pairwise]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :method {:description "the statistic: a key of pairwise/statistics or a function of two samples (f xs ys)"
                                                :default :pearson}
                                       pairwise/method-schema)
                      (schema/nilable :label {:description "the statistic's label (text, colour bar title)"
                                              :cljplot2/default-doc "the statistic's label, value for a function"}
                                      string?)
                      (schema/optional :glyph {:description "how the value is drawn: :number (text, a line per group), :shade (the panel in the value's colour) or :shade-number (both)"
                                               :default :number}
                                       (into [:enum] glyphs))
                      (schema/nilable :domain {:description "the colour domain [lo hi] (pairs: shared by every cell)"
                                               :cljplot2/default-doc "the values' range over the panels; in pairs the matrix's range"}
                                      schema/finite-interval)
                      (schema/nilable :format {:description "the text of a value, a function of the number"
                                               :cljplot2/default-doc "at most 2 decimals, whole numbers without them"}
                                      schema/function)))
   :mapping [:maybe [:map
                     [:x {:description "first sample, the statistic's xs (numbers)"} schema/accessor]
                     [:y {:description "second sample, the statistic's ys (numbers)"} schema/accessor]
                     (schema/nilable :color {:description "categorical groups, each a line of the :number glyph in its colour"} schema/accessor)
                     (schema/nilable :group {:description "groups without a visual aesthetic, each a line of the :number glyph"} schema/accessor)]]})

(defn- grouped?
  "Tests whether a mapping groups the chart: an ordinal aesthetic or `:group` (see [[cljplot2.data.group/group-dims]]).

  Parameters:

  - `mapping` (map): the merged mapping.

  Returns the grouping dimensions (truthy), else `nil`."
  [mapping]
  (seq (group/group-dims mapping)))

(defn computed-aesthetics
  "Gives the pairwise chart's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color {:title label}}` - the value on the colour scale (the theme's gradient), with its colour bar titled by the statistic's label; `{}` when a fixed `:color`, the `:number` glyph or a mapped aesthetic (the plot's colour then belongs to the mapping) colours the cells."
  [options mapping]
  (if (or (:color options) (= :number (get options :glyph :number)) (grouped? mapping))
    {}
    {:color {:title (pairwise/statistic-label (get options :method :pearson) (:label options))}}))

(defn with-shared-domain
  "Gives a pairwise chart with the colour domain of a whole matrix, for every cell of [[cljplot2.multiplot/pairs]] to share.

  Parameters:

  - `chart` (map): a chart value; only a pairwise chart without `:domain` changes.
  - `rows` (sequence): the matrix's rows.
  - `variables` (vector of keywords): the matrix's variables.

  Returns `chart` with `:domain` - the range of its statistic over every ordered pair of different variables (see [[cljplot2.data.pairwise/matrix-rows]]); unchanged for another kind, an explicit `:domain` or no finite value; a single distinct value gives a domain of width 1 around it."
  [chart rows variables]
  (if (or (not= :pairwise (:kind chart)) (some? (get-in chart [:options :domain])))
    chart
    (let [values (keep (fn [{:keys [value]}] (when (m/valid-double? (double value)) (double value)))
                       (pairwise/matrix-rows rows {:variables variables :method (get-in chart [:options :method] :pearson)}))]
      (if (empty? values)
        chart
        (let [lo (v/mn values)
              hi (v/mx values)]
          (assoc-in chart [:options :domain] (if (m/< lo hi) [lo hi] [(m/- lo 0.5) (m/+ hi 0.5)])))))))

(defn compute
  "Computes the pairwise chart's stats: the statistic of every panel, and of its groups when grouped.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:method`, `:label`, `:glyph`, `:domain`, `:format`, the style).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged; `:x`, `:y`, grouping aesthetics).
  - `env` (map): `:theme`, `:plot-scales` and grouping settings.

  Returns per panel `{:value v :groups [{:aesthetics a :value v} ..] :glyph g :label l :format f :by-value? b :style st :text-color c}`; no x / y extents; a colour extent when coloured by value - `:domain`, else the finite panel values' range (none without one); the groups' aesthetic extents when grouped; `:na`, `:dropped` (`:x`, `:y` - non-finite values, `:value` - panels without a value).

  See also [[cljplot2.data.pairwise/statistic-value]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [method (get options :method :pearson)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        ;; groups: ordinal aesthetics and :group only - a continuous color colors other charts' marks
        continuous (group/continuous-color groups mapping env)
        grouping (if continuous (:mapping continuous) mapping)
        grouped (grouped? grouping)
        split (when grouped (group/split groups grouping env))
        by-value? (contains? (computed-aesthetics options mapping) :color)
        all-rows (mapcat val groups)
        st (theme/resolved-style theme chart)
        value-of (fn [rows-of] (let [[xs ys] (pairwise/pair-samples rows-of x-of y-of)] (pairwise/statistic-value method xs ys)))
        values (update-vals groups value-of)
        color-extent (if-let [[lo hi] (:domain options)]
                       {:kind :numeric :min (double lo) :max (double hi)}
                       (aesthetic/value-extent :color (:color plot-scales) (filter #(m/valid-double? (double %)) (vals values))))]
    {:data (into {} (map (fn [[panel panel-rows]]
                           [panel {:value (values panel)
                                   :groups (when grouped
                                             (mapv (fn [{:keys [aesthetics] group-rows :rows}] {:aesthetics aesthetics :value (value-of group-rows)})
                                                   (get-in split [:panels panel])))
                                   :glyph (get options :glyph :number)
                                   :label (pairwise/statistic-label method (:label options))
                                   :format (or (:format options) label/value-text)
                                   :by-value? by-value?
                                   :style st
                                   :text-color (:color options)}]))
                 groups)
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {}
                                       (and by-value? color-extent) (assoc :color color-extent)
                                       grouped (merge (get (group/aesthetic-extents (:panels split) plot-scales) panel)))]))
                    (keys groups))
     :na (if grouped (group/na-dims (:panels split)) #{})
     :dropped {:x (count (remove (comp schema/finite? x-of) all-rows))
               :y (count (remove (comp schema/finite? y-of) all-rows))
               :value (count (remove #(m/valid-double? (double %)) (vals values)))}}))

;; drawing

(defn- value-text
  "Gives the text of a value.

  Parameters:

  - `format-fn` (function): the text of a number.
  - `v` (double): the value.

  Returns `(format-fn v)` as a string, `NA` for `##NaN`."
  [format-fn ^double v]
  (if (m/nan? v) "NA" (str (format-fn v))))

(defn- cell-fill
  "Gives the fill of a shaded cell.

  Parameters:

  - `v` (double): the panel's value.
  - `computed` (map): the panel's stats - `:by-value?`, `:style`.
  - `scales` (map): the panel's scales (`:color` when coloured by value).
  - `env` (map): the drawing environment (the theme's NA colour).

  Returns the value's colour through the colour scale when coloured by value (`nil` for `##NaN` or a value outside the scale - nothing drawn), else the style's `:color`."
  [^double v {:keys [by-value? style]} scales env]
  (if by-value?
    (when-not (m/nan? v)
      ((aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na])) v))
    (:color style)))

(defn- text-lines
  "Gives the text lines of a cell: `<label>: value`, and a line per group.

  Parameters:

  - `computed` (map): the panel's stats - `:value`, `:groups`, `:label`, `:format`.
  - `text-color`: the colour of the first line (`nil` - the text role's).
  - `scales` (map): the panel's scales (the groups' styles).
  - `env` (map): the drawing environment.

  Returns a vector of `{:text :color}`; a group whose aesthetics fall outside their scales gets no line."
  [{:keys [label] format-fn :format panel-value :value panel-groups :groups} text-color scales env]
  (into [{:text (str label ": " (value-text format-fn panel-value)) :color text-color}]
        (keep (fn [{:keys [aesthetics] group-value :value}]
                (when-let [st (group/style {:color :black} aesthetics scales (:theme env))]
                  {:text (str (or (some-> (first (vals aesthetics)) str) "NA") ": " (value-text format-fn group-value)) :color (:color st)})))
        panel-groups))

(defn draw
  "Draws one panel's cell (see the namespace doc).

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats (see [[compute]]).
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box, `:w` and `:h` in pixels.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.

  Returns `nil`. A missing value draws `NA` and no shade; the text takes the `:color` option, else the `:pairwise` text role's colour (`:contrast` - black or white by what is under it: the shade or the panel)."
  [_chart {:keys [glyph text-color] ^double v :value :as computed} canvas {:keys [^double w ^double h] :as box} scales env]
  (let [fill (when (#{:shade :shade-number} glyph) (cell-fill v computed scales env))
        text-on (fn [backdrop] (or text-color (draw/text-color (:theme env) :pairwise backdrop)))]
    (when fill
      (c2d/set-color canvas fill)
      (c2d/rect canvas 0 0 w h))
    (cond
      (and (m/nan? v) (= :shade glyph))
      (label/draw-fitted-lines! canvas {:lines [{:text "NA" :color (text-on (draw/painted-color canvas (m/* 0.5 w) (m/* 0.5 h)))}]
                                        :template "" :role :pairwise} box env)
      (#{:number :shade-number} glyph)
      (label/draw-fitted-lines! canvas {:lines (text-lines computed (text-on (or (draw/painted-color canvas (m/* 0.5 w) (m/* 0.5 h)) fill)) scales env)
                                        :template "" :role :pairwise} box env))
    nil))

(defn legend
  "Draws the key glyph of a grouped chart: a filled square in the key's colour.

  Parameters:

  - `_chart`: the chart value (unused).
  - `attributes` (map): the legend entry's attributes (`:color`).
  - `canvas`: the legend's canvas.
  - `box` (map): the key box.
  - `_env`: unused.

  Returns `nil`."
  [_chart attributes canvas box _env]
  (draw/bar-key! canvas box {:color (:color attributes) :stroke-width 0} false))
