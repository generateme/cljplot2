(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).
  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]]."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private options-schema
  "Keys of [[arrange]]'s `options` (their values are validated with the plot, at build)."
  [:map {:closed true}
   (schema/optional :columns any?) (schema/optional :rows any?) (schema/optional :widths any?)
   (schema/optional :heights any?) (schema/optional :align any?) (schema/optional :equal-panels any?)
   (schema/optional :title any?) (schema/optional :theme any?)])

(defn arrange
  "Plot map arranging `plots` in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

  `options`:

  * `:columns`, `:rows` - grid shape (default one column; one given, the other computed)
  * `:widths`, `:heights` - relative column / row weights (default equal); a `:widths` entry `:measured` sizes a
    column of `:legend-only` plots to their legends (e.g. a shared legend column)
  * `:align` - aligns the panels of sub-plots in rows and columns (default true; `false` - each sub-plot laid out
    on its own), see [[cljplot2.layout.align]]
  * `:equal-panels` - `true`: after the first layout the columns and rows are resized so every aligned panel has the
    same width and height (cells with axes get the extra room; fixed / `:measured` columns keep their width) - a
    matrix of panels, e.g. [[pairs]]; a `:widths` / `:heights` entry may also be `[:fixed px]`
  * `:title` - overall title above the cells (`:labels {:title t}`)
  * `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]`
    around them; sub-plots keep their own themes

  An unknown option key throws `ex-info {:key :arrange}` (with a spelling suggestion when one is close).
  The grid is generated at build time from the merged theme ([[cljplot2.layout.plot-grid/arrange-grid]], cells `[:plot i]`),
  where the spec is validated (`ex-info {:key :arrange}`)."
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (when-let [{:keys [in value message]} (first (schema/errors options-schema options [:options]))]
     (schema/fail! (str "Invalid arrange option " (pr-str (last in)) ": " message ".") {:key :arrange :in in :value value}))
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :align :equal-panels]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))

;; scatter-plot matrix

(def ^:const max-pairs-variables
  "Most variables [[pairs]] takes (each cell is a sub-plot)."
  20)

(defn- variable-domain
  "`[lo hi]` of the finite `values` widened by 5 % of the span on both sides (a single value: ± 0.5)."
  [values]
  (let [lo (double (reduce min values)) hi (double (reduce max values))
        pad (if (== lo hi) 0.5 (* 0.05 (- hi lo)))]
    [(- lo pad) (+ hi pad)]))

(defn- variable-domains
  "`{variable [lo hi]}` (see [[variable-domain]]) of `rows`; a variable without finite numbers, or with a value that
  is neither a number nor `nil`, throws `ex-info {:key :variables}`."
  [rows variables]
  (into {} (map (fn [k]
                  (let [acc (data/row-accessor k k)
                        vs (keep acc rows)
                        finite (filter #(and (number? %) (m/valid-double? (double %))) vs)]
                    (when (or (empty? finite) (some #(not (number? %)) vs))
                      (schema/fail! (str "Pairs variable " k " needs numeric values.") {:key :variables :value k}))
                    [k (variable-domain finite)])))
        variables))

(defn- slot-charts
  "Charts of a `pairs` slot value: a chart, a vector of charts, `nil` (none)."
  [slot]
  (cond (nil? slot) [] (map? slot) [slot] :else (vec slot)))

(defn pairs
  "Scatter-plot matrix of the numeric `variables` of `data` as an [[arrange]] plot map: cell (row i, column j) plots
  variable j on x against variable i on y.

  `options`:

  * `:variables` - 2 to [[max-pairs-variables]] numeric columns (keywords), in matrix order
  * `:mapping` - extra mapping of every cell (grouping aesthetics such as `:color`, `:group`)
  * `:lower` / `:upper` - charts below / above the diagonal: a chart, a vector of charts, `nil` (an empty cell);
    defaults `(chart/scatter)` and `(chart/correlation {:glyph :number})`
  * `:diagonal` - charts of the diagonal cells (x - the variable, y their own magnitude scale with its axis hidden),
    default `(chart/density)`; `:names` - the variable's name in the middle of the cell
  * `:labels` - `{variable text}`, display names (default the variable's name)
  * `:theme` - every cell's and the matrix's theme; `:title` - a title above the matrix

  All cells of a column share the x domain of its variable, all cells of a row the y domain of its (the variable's
  range widened by 5 %, no nice extension, no chart padding). Tick labels and axis titles (the variable names) only on the outer cells
  - x under the bottom row, y left of the left column - the inner sides keep grid lines (`:axis :grid`). With a
  grouping mapping the cells' legends are hidden and a column of `:legend-only` plots (the first one with the legend
  of the lower charts) is sized to the legend; the matrix has `:equal-panels`. Throws `ex-info {:key :variables}` for
  a wrong variable count, a missing or non-numeric variable.

  Equivalents: GGally `ggpairs` - the defaults (scatter below, correlation text above - per group when grouped -,
  density on the diagonal); R `corrgram` / `pairs` panel functions - `chart/correlation` glyphs in `:lower` / `:upper`
  (`:shade`, `:pie`, `:ellipse`, ...) with `:diagonal :names`. A correlation matrix as a heatmap (one tile per pair)
  is `chart/tile` over `fastmath.stats/correlation-matrix` rows instead."
  [data {:keys [variables mapping lower upper diagonal labels theme title]
         :or {lower ::default upper ::default diagonal ::default}}]
  (let [variables (vec variables)
        n (count variables)
        _ (when-not (<= 2 n max-pairs-variables)
            (schema/fail! (str "Pairs needs 2 to " max-pairs-variables " :variables, got " n ".") {:key :variables :value variables}))
        rows (data/rows data)
        domains (variable-domains rows variables)
        name-of (fn [k] (get labels k (name k)))
        lower (if (= ::default lower) [(chart/scatter)] (slot-charts lower))
        upper (if (= ::default upper) [(chart/correlation {:glyph :number})] (slot-charts upper))
        diagonal (if (= ::default diagonal) [(chart/density)] diagonal)
        grouped? (boolean (seq (group/group-dims mapping)))
        axis (fn [outer?] (if outer? true :grid))
        cell (fn [^long i ^long j]
               (let [vx (variables j) vy (variables i)
                     diag? (= i j) bottom? (= i (dec n)) left? (zero? j)
                     names? (and diag? (= :names diagonal))
                     charts (cond names? [(chart/text {:x [:panel 0.5] :y [:panel 0.5] :text (name-of vx) :size 13 :font-style :bold})]
                                  diag? (slot-charts diagonal)
                                  (> i j) lower
                                  :else upper)]
                 (cond-> {:data rows
                          :mapping (cond-> (assoc mapping :x vx) (not diag?) (assoc :y vy))
                          :charts charts
                          ;; exact domains (already padded): no chart padding on top
                          :scales {:x {:domain (domains vx) :nice false :expand [0.0 0.0] :axis (axis bottom?)}
                                   :y (cond diag? {:axis :grid}
                                            :else {:domain (domains vy) :nice false :expand [0.0 0.0] :axis (axis left?)})}
                          :labels {:x (when bottom? (name-of vx)) :y (when left? (name-of vy))}
                          :legend false}
                   names? (assoc-in [:scales :y] {:domain [0 1] :expand [0.0 0.0] :axis :grid})
                   names? (update :mapping dissoc :y)
                   theme (assoc :theme theme))))
        legend-cell (fn [^long i] (cond-> {:legend-only true}
                              (zero? i) (assoc :data rows :mapping (assoc mapping :x (variables 1) :y (variables 0))
                                               :charts (if (seq lower) lower [(chart/scatter)]))
                              theme (assoc :theme theme)))
        plots (vec (for [i (range n) j (range (if grouped? (inc n) n))]
                     (if (== (long j) n) (legend-cell i) (cell i j))))]
    (arrange plots (cond-> {:columns (if grouped? (inc n) n) :equal-panels true}
                     grouped? (assoc :widths (conj (vec (repeat n 1)) :measured))
                     title (assoc :title title)
                     theme (assoc :theme theme)))))
