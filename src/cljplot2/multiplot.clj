(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).

  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]]. Sub-plots keep their own themes.

  Functions: [[arrange]] (plots in a grid, options [[arrange-options]]), [[pairs]] (a scatter-plot matrix, options [[pairs-options]]); [[builder-entries]] - the option entries every builder takes."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.chart.pairwise :as pairwise]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def builder-entries
  "Option entries every builder takes (a shared group, `:builder`): the plot title, scales deep-merged into the builder's and a theme; their values are validated with the built plot."
  (schema/in-fragment
   :builder
   [(schema/optional :title {:description "plot title (a text or label options)"} any?)
    (schema/optional :scales {:description "scale specs deep-merged into the builder's (validated with the plot)"} any?)
    (schema/optional :theme {:description "theme overrides of the built plot"} any?)]))

(defn- builder-entries-of
  "Some of the [[builder-entries]], for a builder taking only part of the shared group.

  Parameters:

  - `ks` (sequence of keywords): the keys wanted, each one of `:title`, `:scales`, `:theme`.

  Returns their entries in the order of `ks`."
  [ks]
  (let [by-key (into {} (map (juxt first identity)) builder-entries)]
    (mapv by-key ks)))

(def arrange-options
  "Schema of the `options` of [[arrange]]: the plot `:arrange` layout entries but `:plots` (the shared group `:arrange`, see [[cljplot2.layout.plot-grid/arrange-entries]]) and the builder's `:title` and `:theme` (a multiplot takes no `:scales` - each sub-plot has its own)."
  (-> [:map {:closed true}]
      (into (remove #(= :plots (first %))) plot-grid/arrange-entries)
      (into (builder-entries-of [:title :theme]))))

(defn arrange
  "Builds a plot map arranging plots in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

  Parameters:

  - `plots` (sequence of plot maps): the cells, row by row.
  - `options` (map, optional):
    - `:columns`, `:rows` - grid shape (default one column; one given, the other computed);
    - `:widths`, `:heights` - relative column / row weights (default equal); a `:widths` entry `:measured` sizes a column of `:legend-only` plots to their legends (e.g. a shared legend column), an entry may also be `[:fixed px]`;
    - `:align` - align the panels of sub-plots in rows and columns (default `true`; `false` - each sub-plot laid out on its own), see [[cljplot2.layout.align]];
    - `:equal-panels` - `true`: after the first layout the columns and rows are resized so every aligned panel has the same width and height (cells with axes get the extra room; fixed / `:measured` columns keep their width) - a matrix of panels, e.g. [[pairs]]; the whole multiplot is built twice;
    - `:title` - overall title above the cells (`:labels {:title t}`);
    - `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]` around them; sub-plots keep their own themes.

  Returns a plot map; the grid is generated at build time from the merged theme ([[cljplot2.layout.plot-grid/arrange-grid]], cells `[:plot i]`), where the spec is validated (`ex-info {:key :arrange}`).

  Options are validated first ([[arrange-options]]): an invalid value or an unknown key throws `ex-info {:key k}`, `k` the option (with a spelling suggestion when a known key is close)."
  {:cljplot2/options arrange-options}
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (schema/check-options! arrange-options options)
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :align :equal-panels]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))

;; scatter-plot matrix

(def ^{:private true :const true :tag 'long} max-pairs-variables
  "Most variables [[pairs]] takes (each cell is a sub-plot)."
  20)

(defn- variable-domain
  "The domain of a pairs variable: its range widened by 5 % of the span on both sides.

  Parameters:

  - `values` (non-empty sequence of finite numbers): the variable's values.

  Returns `[lo hi]`; a single distinct value is widened by 0.5 on both sides."
  [values]
  (let [lo (v/mn values)
        hi (v/mx values)
        pad (if (m/== lo hi) 0.5 (m/* 0.05 (m/- hi lo)))]
    [(m/- lo pad) (m/+ hi pad)]))

(defn- variable-domains
  "The domains of the pairs variables.

  Parameters:

  - `rows` (sequence of maps): the data rows.
  - `variables` (sequence of `[variable option]`): the variables (keywords), each with the option it was given in (`:variables`, `:row-variables` or `:column-variables`).

  Returns `{variable [lo hi]}` (see [[variable-domain]]); `nil` and non-finite numbers are left out. Throws `ex-info {:key option}` for a variable without finite numbers or with a value that is neither a number nor `nil`."
  [rows variables]
  (into {} (map (fn [[k option]]
                  (let [acc (data/row-accessor k k)
                        vs (keep acc rows)
                        finite (filter schema/finite? vs)]
                    (when (or (empty? finite) (some (fn [v] (not (number? v))) vs))
                      (schema/fail! (str "Pairs variable " k " needs numeric values.") {:key option :value k}))
                    [k (variable-domain finite)])))
        variables))

(defn- slot-charts
  "Gives the charts of a pairs slot.

  Parameters:

  - `slot` (chart, sequence of charts or `:none`): a given `:lower`, `:upper` or `:diagonal` value (not `nil` - the caller resolves the default).

  Returns a vector of charts, empty for `:none`."
  [slot]
  (cond (= :none slot) [] (map? slot) [slot] :else (vec slot)))

(defn- single-chart?
  "Tests whether a pairs slot holds a single chart value (a map), not a sequence of charts.

  Parameters:

  - `v`: the slot value.

  Returns `true` for a map, else `false`."
  [v]
  (map? v))

(def ^:private slot-schema
  "Schema of a pairs slot: a chart, a sequence of charts, `:none` (empty cells) or `nil` (the default charts)."
  [:maybe [:multi {:dispatch #(cond (= :none %) :none (single-chart? %) :chart :else :charts)
                   :cljplot2/type-doc ":none, a chart or a vector of charts"}
           [:none [:= :none]] [:chart pipeline/chart-schema] [:charts [:sequential pipeline/chart-schema]]]])

(defn- square?
  "Tests whether pairs options give a square matrix: the same variables, in the same order, on rows and columns.

  Parameters:

  - `options` (map): valid [[pairs]] options.

  Returns `true` when `:row-variables` and `:column-variables` (each defaulting to `:variables`) are equal, else `false`."
  [{:keys [variables row-variables column-variables]}]
  (= (vec (or row-variables variables)) (vec (or column-variables variables))))

(defn- pairs-variables-problem
  "Gives why pairs options name no variables, or `nil`.

  Parameters:

  - `options` (map): valid [[pairs]] entries.

  Returns a message when `:variables` is missing and `:row-variables` or `:column-variables` too."
  [{:keys [variables row-variables column-variables]}]
  (when (and (nil? variables) (or (nil? row-variables) (nil? column-variables)))
    "give :variables, or both :row-variables and :column-variables"))

(defn- pairs-upper-problem
  "Gives why pairs options can't have upper cells, or `nil`.

  Parameters:

  - `options` (map): valid [[pairs]] entries.

  Returns a message when `:upper` is set but the matrix isn't square (see [[square?]]): it has no triangles, every cell of two different variables takes `:lower`."
  [{:keys [upper] :as options}]
  (when (and (some? upper) (not (square? options)))
    ":upper needs a square matrix (the same :row-variables and :column-variables in the same order); other matrices draw :lower in every cell of two different variables"))

(def pairs-options
  "Schema of the `options` of [[pairs]] (the variables' count and values are checked against the data)."
  (-> [:map {:closed true}
       (schema/nilable :variables {:description "the numeric columns, in matrix order (rows and columns)"
                                   :cljplot2/default-doc "required unless :row-variables and :column-variables are given"}
                       [:sequential {:min 2 :max max-pairs-variables} keyword?])
       (schema/nilable :row-variables {:description "the numeric columns of the matrix rows, top to bottom (on y)"
                                       :cljplot2/default-doc ":variables"}
                       [:sequential {:min 1 :max max-pairs-variables} keyword?])
       (schema/nilable :column-variables {:description "the numeric columns of the matrix columns, left to right (on x)"
                                          :cljplot2/default-doc ":variables"}
                       [:sequential {:min 1 :max max-pairs-variables} keyword?])
       (schema/nilable :mapping {:description "extra mapping of every cell (grouping aesthetics such as :color, :group)"}
                       data/mapping-schema)
       (schema/nilable :lower {:description "charts below the diagonal of a square matrix; off-diagonal (every cell of two different variables) in a matrix that isn't square; :none - empty cells"
                                :cljplot2/default-doc "(chart/scatter)"}
                        slot-schema)
       (schema/nilable :upper {:description "charts above the diagonal (a square matrix only); :none - empty cells"
                                :cljplot2/default-doc "(chart/correlation {:glyph :number})"}
                        slot-schema)
       (schema/nilable :diagonal {:description "charts of the diagonal cells - a cell whose row and column variable are the same (in their own magnitude scale); :names - the variable's name (text role :pairs-name); :none - empty cells"
                                   :cljplot2/default-doc "(chart/density)"}
                        [:maybe [:multi {:dispatch #(cond (#{:names :none} %) :keyword (map? %) :chart :else :charts)
                                         :cljplot2/type-doc ":names, :none, a chart or a vector of charts"}
                                 [:keyword [:enum :names :none]]
                                 [:chart pipeline/chart-schema]
                                 [:charts [:sequential pipeline/chart-schema]]]])
       (schema/nilable :variable-names {:description "display names of the variables, {variable text}"
                                :cljplot2/default-doc "the variable's name"}
                       [:map-of keyword? string?])
       (schema/nilable :scales {:description "the colour scale of every cell that colours (one colour bar), {:color spec}; its :domain wins over the matrix's computed one"}
                       [:map {:closed true} (schema/optional :color {:description "the colour scale spec"} (aesthetic/scale-spec-schema :color))])
       (schema/nilable :theme {:description "theme of every cell and of the matrix (a [:plot :margin] or [:multiplot :gap] given wins over the cells' facet-like spacing)"}
                       any?)]
      (into (builder-entries-of [:title]))
      (schema/with-rules [pairs-variables-problem [:variables]] [pairs-upper-problem [:upper]])))

(defn pairs
  "Builds a scatter-plot matrix of numeric variables as an [[arrange]] plot map: cell (row i, column j) plots column variable j on x against row variable i on y.

  Parameters:

  - `rows` (rows or columns): the data.
  - `options` (map):
    - `:variables` - 2 to 20 numeric columns (keywords), in matrix order (required unless both of the next two are given);
    - `:row-variables` / `:column-variables` - 1 to 20 numeric columns each, the rows top to bottom / the columns left to right (each defaults to `:variables`): a matrix with other variables, or another order, on its rows than on its columns - e.g. 3 rows x 2 columns;
    - `:mapping` - extra mapping of every cell (grouping aesthetics such as `:color`, `:group`);
    - `:lower` / `:upper` - charts below / above the diagonal: a chart, a vector of charts or `:none` (empty cells); `nil` or absent - the defaults `(chart/scatter)` and `(chart/correlation {:glyph :number})`;
    - `:diagonal` - charts of the diagonal cells - those whose row and column variable are the same (x - the variable, y their own magnitude scale with its axis hidden), default `(chart/density)`; `:names` - the variable's name in the middle of the cell (text role `:pairs-name`); `:none` - empty cells;
    - `:variable-names` - `{variable text}`, display names (default the variable's name);
    - `:scales` - `{:color spec}`, the colour scale of every cell that colours (a mapped or computed colour), so the matrix has one colour bar; its `:domain` wins over a computed one;
    - `:theme` - every cell's and the matrix's theme (cells have no plot margin and are `[:facet :gap]` apart, as facet panels; a `[:plot :margin]` / `[:multiplot :gap]` in `:theme` wins);
    - `:title` - a title above the matrix.

  All cells of a column share the x domain of its variable, all cells of a row the y domain of its (the variable's
  range widened by 5 %, no nice extension, no chart padding). Tick labels and axis titles (the variable names) only on the outer cells
  - x under the bottom row, y left of the left column - the inner sides keep grid lines (`:axis :grid`). Diagonal
  charts draw in their own magnitude scale; their y axis and grid show the row variable (`:axis-domain`), so the
  top-left cell labels the first variable as the rest of its row. The cells' legends are hidden; one
  column of `:legend-only` plots, sized to the legend, shows the mapped aesthetics (keys of the lower charts, a color
  bar for a continuous color - left out of the diagonal cells) or, without a mapping, the colors the lower / upper
  charts compute (e.g. the r color bar of correlation glyphs);
  the matrix has `:equal-panels` and cells shaded as a checkerboard like facet panels (`:alternate-panel` on odd
  `i + j`, theme `[:panel :alternate-fill]`; `:none` turns it off).

  A square matrix has the same variables, in the same order, on its rows and columns (`:variables` alone): its
  diagonal runs from the top-left corner, `:lower` below it, `:upper` above. Another matrix (`:row-variables` and
  `:column-variables` differ) has no triangles: every cell of two different variables takes `:lower` (default
  `(chart/scatter)`), a cell of one variable (wherever it is) `:diagonal`; it takes no `:upper` (an error keyed
  `:upper`).

  Returns a plot map. Throws `ex-info {:key k}` for a wrong variable count, a missing or non-numeric variable (`k` - `:variables`, `:row-variables` or `:column-variables`, the option naming it); options are validated first ([[pairs-options]], `ex-info {:key k}`, `k` the option; no variables - `:variables`).

  Equivalents: GGally `ggpairs` - the defaults (scatter below, correlation text above - per group when grouped -,
  density on the diagonal); R `corrgram` / `pairs` panel functions - `chart/correlation` glyphs in `:lower` / `:upper`
  (`:shade`, `:pie`, `:ellipse`, ...) with `:diagonal :names`. Other statistics (errors, effect sizes, distances, a
  user function) are `chart/pairwise` cells: every cell shows the statistic of its column variable against its row
  variable, so the mirrored cell of an asymmetric statistic shows the reverse order. A value-coloured `pairwise`
  statistic is computed once over its cells, and every cell colouring by it gets its range as the colour domain (symmetric
  around 0 for a signed statistic), so the cells and the one colour bar agree; pairwise cells share one text size (see
  [[cljplot2.chart.pairwise/matrix-settings]]). The lower and upper charts colour by one statistic at most - a matrix
  has one colour bar (else an error keyed `:upper`). A matrix as a heatmap (one tile per pair) is `chart/tile` over
  [[cljplot2.data.pairwise/matrix-rows]] (or `fastmath.stats/correlation-matrix` rows) instead."
  {:cljplot2/options pairs-options}
  [rows {:keys [mapping variable-names theme title row-variables column-variables] matrix-scales :scales variable-keys :variables
         lower-slot :lower upper-slot :upper diagonal-slot :diagonal :as options}]
  (let [_ (schema/check-options! pairs-options options)
        square (square? options)
        y-variables (vec (or row-variables variable-keys))
        x-variables (vec (or column-variables variable-keys))
        n-rows (count y-variables)
        n-columns (count x-variables)
        last-row (m/long-dec n-rows)
        row-maps (data/rows rows)
        domains (variable-domains row-maps (distinct (concat (map vector y-variables (repeat (if row-variables :row-variables :variables)))
                                                             (map vector x-variables (repeat (if column-variables :column-variables :variables))))))
        name-of (fn [k] (get variable-names k (name k)))
        ;; a cell of two different variables (the first row's), for the legend and the computed colours
        pair-mapping (assoc mapping
                            :x (or (first (remove #{(y-variables 0)} x-variables)) (x-variables 0))
                            :y (y-variables 0))
        ;; pairwise charts share the matrix's colour domain (their values have no fixed range) and one text size
        settings (pairwise/matrix-settings {:lower (if (nil? lower-slot) [(chart/scatter)] (slot-charts lower-slot))
                                            :upper (cond (not square) [] (nil? upper-slot) [(chart/correlation {:glyph :number})] :else (slot-charts upper-slot))}
                                           row-maps {:rows y-variables :columns x-variables} pair-mapping)
        lower (get-in settings [:slots :lower])
        upper (get-in settings [:slots :upper])
        color-scale (merge (when-let [d (:domain settings)] {:domain d}) (:color matrix-scales))
        colors? (fn [charts cell-mapping] (or (some? (:color cell-mapping))
                                              (some #(contains? (aesthetic/computed-aesthetics % cell-mapping) :color) charts)))
        diagonal (if (nil? diagonal-slot) [(chart/density)] diagonal-slot)
        mapped? (seq (aesthetic/mapped-dims mapping))
        ;; aesthetics with continuous values (e.g. a numeric :color) color marks without grouping: left out on the
        ;; diagonal, whose 1d charts group by their aesthetics
        continuous (set (filter (fn [k] (let [vs (keep (data/row-accessor k (mapping k)) row-maps)]
                                         (and (seq vs) (every? number? vs) (not (accessor/categorical? (mapping k))))))
                                (aesthetic/mapped-dims mapping)))
        diagonal-mapping (apply dissoc mapping continuous)
        ;; the legend column: the mapped aesthetics (keys / color bars of the lower charts), else the colors the cell
        ;; charts compute (r color bars)
        legend-charts (cond
                        mapped? (if (seq lower) lower [(chart/scatter)])
                        :else (vec (distinct (filter #(seq (aesthetic/computed-aesthetics % pair-mapping)) (concat lower upper)))))
        legend? (seq legend-charts)
        axis (fn [outer?] (if outer? true :grid))
        ;; panels as close as facet panels: cells without plot margins, the facet gap between them (`theme` wins)
        cell-theme (theme/deep-merge {:plot {:margin 0}} theme)
        matrix-theme (theme/deep-merge {:multiplot {:gap (get-in (theme/deep-merge theme/default-theme theme) [:facet :gap])}}
                                       theme)
        cell (fn [^long i ^long j]
               (let [vx (x-variables j) vy (y-variables i)
                     diag? (= vx vy) bottom? (m/== i last-row) left? (m/zero? j)
                     names? (and diag? (= :names diagonal))
                     charts (cond names? [(chart/text {:x [:panel 0.5] :y [:panel 0.5] :text (name-of vx) :role :pairs-name})]
                                  diag? (slot-charts diagonal)
                                  (or (not square) (m/> i j)) lower
                                  :else upper)]
                 (cond-> {:data row-maps
                          :mapping (if diag? (assoc diagonal-mapping :x vx) (assoc mapping :x vx :y vy))
                          :charts charts
                          ;; exact domains (already padded): no chart padding on top
                          :scales {:x {:domain (domains vx) :nice false :expand [0.0 0.0] :axis (axis bottom?)}
                                   ;; the diagonal draws in its own magnitude scale, its axis and grid show the row variable
                                   :y (cond diag? {:axis (axis left?) :axis-domain (domains vy)}
                                            :else {:domain (domains vy) :nice false :expand [0.0 0.0] :axis (axis left?)})}
                          :labels {:x (when bottom? (name-of vx)) :y (when left? (name-of vy))}
                          :legend false}
                   (m/odd? (m/+ i j)) (assoc :alternate-panel true)
                   ;; names or an empty diagonal: no data on y - a fixed unit domain
                   (and diag? (or names? (empty? charts))) (assoc-in [:scales :y] {:domain [0 1] :expand [0.0 0.0] :axis (axis left?)
                                                                                   :axis-domain (domains vx)})
                   names? (update :mapping dissoc :y)
                   (and (seq color-scale) (colors? charts (if diag? (assoc diagonal-mapping :x vx) (assoc mapping :x vx :y vy))))
                   (assoc-in [:scales :color] color-scale)
                   true (assoc :theme cell-theme))))
        legend-cell (fn [^long i] (cond-> {:legend-only true :theme cell-theme}
                                   (m/zero? i) (assoc :data row-maps :mapping pair-mapping :charts legend-charts)
                                   (and (m/zero? i) (seq color-scale) (colors? legend-charts pair-mapping)) (assoc :scales {:color color-scale})))
        n-cells (if legend? (m/long-inc n-columns) n-columns)
        plots (vec (for [^long i (range n-rows)
                         ^long j (range n-cells)]
                     (if (m/== j n-columns) (legend-cell i) (cell i j))))]
    (arrange plots (cond-> {:columns n-cells :equal-panels true :theme matrix-theme}
                     legend? (assoc :widths (conj (vec (repeat n-columns 1)) :measured))
                     title (assoc :title title)))))
