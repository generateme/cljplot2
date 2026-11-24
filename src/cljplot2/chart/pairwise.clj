(ns cljplot2.chart.pairwise
  "Pairwise statistic: a statistic of the panel's `:x` / `:y` samples - an error, an association, an effect size, a distance (see [[cljplot2.data.pairwise/statistics]]) or a user function - drawn as one cell filling the panel; with [[cljplot2.multiplot/pairs]] a matrix of them. A cell shows the statistic of its x variable against its y variable `(f xs ys)` - in `pairs` the column variable against the row variable - so the mirrored cell of an asymmetric statistic shows the reverse order.

  Unlike [[cljplot2.chart.correlation]] the values have no fixed range: a colour shows the value on the colour scale's domain - by default the values' range over the panels, symmetric around 0 for a signed statistic (`:signed?`, coloured on the diverging [[cljplot2.chart.statistic-cell/diverging-range]]); `:scales {:color {:domain ..}}` fixes it; in `pairs` the matrix's range, shared by every cell (see [[matrix-settings]]).

  Mapping: numeric `:x`, `:y`; a paired statistic takes the rows where both are finite (pairwise-complete rows), an independent-sample one each variable's own finite values (see [[cljplot2.data.pairwise/statistic-samples]]); fewer than 2 values, a non-finite result or a statistic that throws give no value (`NA`, counted in `:dropped :value`).

  `:glyph`:

  * `:number` (default) - `<label>: value` (text role `:pairwise`, shrunk to fit a narrow panel - one size for panels of one size); with a grouping mapping (`:group`, ordinal aesthetics such as a categorical `:color`) a line per group under it, in the group's colour
  * `:shade` - the whole panel in the value's colour
  * `:shade-number` - both: the text over the shade, black or white by the fill (text role colour `:contrast`), group lines too

  Options (style default from theme `[:chart :pairwise]`): `:color` (one fixed colour of the shade, or of the `:number` text), `:method`, `:label`, `:glyph`, `:format`.

  Functions: [[value-domain]] - the colour domain of values; [[computed-aesthetics]] - the computed colour; [[matrix-settings]] - what `pairs` shares between cells; [[compute]], [[draw]], [[legend]] - the chart contract (shared cell parts: [[cljplot2.chart.statistic-cell]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.statistic-cell :as cell]
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
  [(schema/optional :color {:description "one fixed colour: the shade's (text over it by contrast), the :number text's"
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
                      (schema/nilable :format {:description "the text of a value, a function of the number (a throw or nil - NA)"
                                               :cljplot2/default-doc "at most 2 decimals, whole numbers without them"}
                                      schema/function)
                      (schema/nilable :text-template {:description "the widest text a cell's line holds, so cells get one font size (set by pairs)"
                                                      :cljplot2/internal true}
                                      string?)))
   :mapping [:maybe [:map
                     [:x {:description "first sample, the statistic's xs (numbers)"} schema/accessor]
                     [:y {:description "second sample, the statistic's ys (numbers)"} schema/accessor]
                     (schema/nilable :color {:description "categorical groups, each a line of the :number glyph in its colour"} schema/accessor)
                     (schema/nilable :group {:description "groups without a visual aesthetic, each a line of the :number glyph"} schema/accessor)]]})

(defn value-domain
  "Gives the colour domain of a statistic's values.

  Parameters:

  - `values` (sequence of numbers): the values; non-finite ones are left out.
  - `signed?`: zero is the statistic's neutral value (see [[cljplot2.data.pairwise/signed-statistic?]]).

  Returns `[lo hi]`: the values' range, `[-M M]` for a signed statistic (M - the largest magnitude); a single value (or M 0) widened by 0.5 on each side; `nil` without a finite value."
  [values signed?]
  (let [finite (into [] (comp (filter schema/finite?) (map double)) values)]
    (when (seq finite)
      (if signed?
        (let [mag (m/max 0.5 (v/mx (map #(m/abs (double %)) finite)))]
          [(m/- mag) mag])
        (let [lo (v/mn finite)
              hi (v/mx finite)]
          (if (m/< lo hi) [lo hi] [(m/- lo 0.5) (m/+ hi 0.5)]))))))

(defn computed-aesthetics
  "Gives the pairwise chart's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color {:title label}}` - the value on the colour scale (the theme's gradient; a signed statistic adds `:range` [[cljplot2.chart.statistic-cell/diverging-range]]), its colour bar titled by the statistic's label; `{}` when the chart doesn't take the value's colour (see [[cljplot2.chart.statistic-cell/colored-by-value?]])."
  [options mapping]
  (if (cell/colored-by-value? options mapping)
    (let [method (get options :method :pearson)]
      {:color (cond-> {:title (pairwise/statistic-label method (:label options))}
                (pairwise/signed-statistic? method) (assoc :range cell/diverging-range))})
    {}))

(defn- formatted-value
  "Gives the text of a value.

  Parameters:

  - `format-fn` (function): the text of a number.
  - `v` (double): the value.

  Returns `(format-fn v)` as a string; `NA` for `##NaN`, a `nil` text or a `format-fn` that throws."
  [format-fn ^double v]
  (if (m/nan? v)
    "NA"
    (or (try (some-> (format-fn v) str) (catch Throwable _ nil)) "NA")))

(defn- text-template
  "Gives the widest line of cells: the longest `<label>: value` text.

  Parameters:

  - `label` (string): the statistic's label.
  - `format-fn` (function): the text of a value.
  - `values` (sequence of doubles): the values the cells show.

  Returns the longest text (by characters), `nil` without values."
  [label format-fn values]
  (when (seq values)
    (apply max-key count (map #(str label ": " (formatted-value format-fn (double %))) values))))

(defn- slot-cells
  "Gives the `[x y]` variable pairs of a pairs slot's cells: the column variable on x, the row variable on y.

  Parameters:

  - `variables` (vector of keywords): the matrix's variables.
  - `slot` (keyword): `:lower` (below the diagonal) or `:upper`.

  Returns a sequence of `[x y]`."
  [variables slot]
  (for [[^long i y] (map-indexed vector variables)
        [^long j x] (map-indexed vector variables)
        :when (if (= slot :lower) (m/> i j) (m/< i j))]
    [x y]))

(defn- color-encoding
  "Gives what a pairs slot chart colours by, for comparing the slots' colour bars.

  Parameters:

  - `chart` (map): a chart value.
  - `mapping` (map): a cell's mapping.

  Returns `[:pairwise method]` for a value-coloured pairwise chart, `[kind computed-colour]` for another chart computing a colour, `nil` otherwise."
  [chart mapping]
  (when-let [computed (:color (aesthetic/computed-aesthetics chart mapping))]
    (if (= :pairwise (:kind chart))
      [:pairwise (get-in chart [:options :method] :pearson)]
      [(:kind chart) computed])))

(defn matrix-settings
  "Gives what the cells of a [[cljplot2.multiplot/pairs]] matrix share: the colour domain of a value-coloured pairwise statistic and one text size of pairwise cells.

  Each pairwise statistic is computed once over the cells it is drawn in (see [[cljplot2.data.pairwise/cell-values]]).

  Parameters:

  - `slots` (map): `{:lower charts :upper charts}`, vectors of chart values.
  - `rows` (sequence of maps): the matrix's rows.
  - `variables` (vector of keywords): the matrix's variables.
  - `mapping` (map): a cell's mapping (`:x`, `:y` and the matrix's extra mapping).

  Returns `{:domain d :slots s}`: `d` - the colour domain of the value-coloured pairwise statistic over its cells (see [[value-domain]]), `nil` without one; `s` - the slots with every pairwise chart given `:text-template` (its widest cell text).

  Throws `ex-info {:key :upper}` when the slots colour by different encodings (two pairwise statistics, or a pairwise statistic and another computed colour such as correlation's): a matrix has one colour bar."
  [slots rows variables mapping]
  (let [placed (for [slot [:lower :upper] chart (get slots slot)] [slot chart])
        encodings (distinct (keep (fn [[_ chart]] (color-encoding chart mapping)) placed))
        pairwise-cells (reduce (fn [acc [slot chart]]
                                 (if (= :pairwise (:kind chart))
                                   (update acc (get-in chart [:options :method] :pearson) (fnil into []) (slot-cells variables slot))
                                   acc))
                               {} placed)
        values (into {} (map (fn [[method cells]] [method (pairwise/cell-values rows method (distinct cells))])) pairwise-cells)]
    (when (and (m/> (count encodings) 1) (some #(= :pairwise (first %)) encodings))
      (schema/fail! (str "A pairs matrix has one colour bar: its lower and upper charts colour by different statistics " (vec encodings)
                         " - shade one statistic (a :number glyph or a fixed :color doesn't colour).")
                    {:key :upper :value (vec encodings)}))
    {:domain (when-let [[_ method] (first (filter #(= :pairwise (first %)) encodings))]
               (value-domain (map :value (vals (values method))) (pairwise/signed-statistic? method)))
     :slots (update-vals slots
                         (fn [charts]
                           (mapv (fn [chart]
                                   (if (= :pairwise (:kind chart))
                                     (let [{:keys [method] label-option :label format-fn :format} (:options chart)
                                           m (or method :pearson)]
                                       (assoc-in chart [:options :text-template]
                                                 (text-template (pairwise/statistic-label m label-option) (or format-fn label/value-text)
                                                                (map :value (vals (values m))))))
                                     chart))
                                 charts)))}))

(defn compute
  "Computes the pairwise chart's stats: the statistic of every panel, and of its groups when grouped.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:method`, `:label`, `:glyph`, `:format`, the style).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged; `:x`, `:y`, grouping aesthetics).
  - `env` (map): `:theme`, `:plot-scales` and grouping settings.

  Returns per panel `{:value v :groups [{:aesthetics a :value v} ..] :glyph g :label l :format f :by-value? b :style st :text-color c :template t}` (`t` - the widest line of the panels, so they get one text size); no x / y extents; a colour extent when coloured by value - the finite panel values' domain (see [[value-domain]]; a plot `:scales {:color {:domain ..}}` wins); the groups' aesthetic extents when grouped; `:na`, `:dropped` (`:x`, `:y` - non-finite values, `:value` - panels without a value).

  See also [[cljplot2.data.pairwise/statistic-value]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [method (get options :method :pearson)
        label-text (pairwise/statistic-label method (:label options))
        format-fn (or (:format options) label/value-text)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        {:keys [grouped split]} (cell/grouping groups mapping env)
        by-value? (cell/colored-by-value? options mapping)
        st (theme/resolved-style theme chart)
        value-of (fn [rows-of] (let [[xs ys] (pairwise/statistic-samples method rows-of x-of y-of)] (pairwise/statistic-value method xs ys)))
        values (update-vals groups value-of)
        panel-groups (when grouped
                       (update-vals (:panels split)
                                    (fn [gs] (mapv (fn [{:keys [aesthetics] group-rows :rows}] {:aesthetics aesthetics :value (value-of group-rows)}) gs))))
        template (let [own (text-template label-text format-fn (concat (vals values) (map :value (mapcat val panel-groups))))
                       given (:text-template options)]
                   (if (and given (m/>= (count given) (count own))) given own))
        [lo hi :as domain] (when by-value? (value-domain (vals values) (pairwise/signed-statistic? method)))]
    {:data (into {} (map (fn [panel]
                           [panel {:value (values panel)
                                   :groups (get panel-groups panel)
                                   :glyph (get options :glyph :number)
                                   :label label-text
                                   :format format-fn
                                   :by-value? by-value?
                                   :style st
                                   :text-color (:color options)
                                   :template template}]))
                 (keys groups))
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {}
                                       domain (assoc :color (aesthetic/value-extent :color (:color plot-scales) [lo hi]))
                                       grouped (merge (get (group/aesthetic-extents (:panels split) plot-scales) panel)))]))
                    (keys groups))
     :na (if grouped (group/na-dims (:panels split)) #{})
     :dropped (assoc (cell/dropped (mapcat val groups) mapping) :value (count (remove #(schema/finite? %) (vals values))))}))

;; drawing

(defn draw
  "Draws one panel's cell (see the namespace doc).

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats (see [[compute]]).
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box, `:w` and `:h` in pixels.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.

  Returns `nil`. A missing value draws `NA` and no shade. Text over a shade - every line - takes the `:pairwise` text role's colour against the fill (`:contrast` - black or white); without a shade the first line takes the `:color` option, else the text role's colour against the panel, group lines their group's colour."
  [_chart {:keys [glyph text-color style by-value? template groups] label-text :label format-fn :format ^double v :value} canvas {:keys [^double w ^double h] :as box} scales env]
  (let [theme (:theme env)
        fill (when (#{:shade :shade-number} glyph) (cell/value-fill v by-value? (:color style) scales env))
        backdrop (or fill (draw/painted-color canvas (m/* 0.5 w) (m/* 0.5 h)) (draw/plot-backdrop theme))
        role-color (draw/text-color theme :pairwise backdrop)
        text-of #(formatted-value format-fn (double %))
        draw-lines! (fn [lines] (label/draw-fitted-lines! canvas {:lines lines :template template :role :pairwise} box env))]
    (when fill
      (c2d/set-color canvas fill)
      (c2d/rect canvas 0 0 w h))
    (cond
      (and (m/nan? v) (= :shade glyph))
      (draw-lines! [{:text "NA" :color role-color}])
      (#{:number :shade-number} glyph)
      (let [first-line {:text (str label-text ": " (text-of v)) :color (if fill role-color (or text-color role-color))}
            lines (cell/value-lines first-line groups text-of role-color scales env)]
        (draw-lines! (if fill (mapv #(assoc % :color role-color) lines) lines))))
    nil))

(def legend
  "Draws the key glyph of a grouped chart: a filled square in the key's colour (see [[cljplot2.chart.statistic-cell/legend]])."
  cell/legend)
