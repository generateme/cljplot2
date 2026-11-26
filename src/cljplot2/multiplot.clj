(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).

  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]]. Sub-plots keep their own themes.

  Functions: [[arrange]] (plots in a grid, options [[arrange-options]]), [[pairs]] (a scatter-plot matrix, options [[pairs-options]]); [[builder-entries]] - the option entries every builder takes."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.chart.pairwise :as pairwise]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.text :as text]
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
   [(schema/optional :title {:description "The plot's title: a text or a label map." :cljplot2/default-doc "No title." :cljplot2/example "Friends"} any?)
    (schema/optional :scales {:description "Scale specs merged at every depth into the builder's own; the built plot checks them."
                              :cljplot2/default-doc "The builder's scales." :cljplot2/example {:size {:range [2 20]}} :cljplot2/example-context {:mapping {:size :degree}}}
                     any?)
    (schema/optional :theme {:description "A theme merged over the builder's own theme settings and the default theme for the built plot."
                             :cljplot2/default-doc "The default theme." :cljplot2/example {:chart {:network-node {:color :black}}}}
                     any?)]))

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
    - `:widths`, `:heights` - relative column / row weights (default equal); a `:widths` entry `:measured` sizes a column of `:legend-only` plots (or one-column arranges of them) to their legends (e.g. a shared legend column; measured at the height each cell gets, so a legend taller than its cell wraps into further columns and the column widens), a `:heights` entry `:measured` sizes a row of `:legend-only` plots to their legends' height (all measured rows scaled down in proportion when they don't fit; e.g. a stack of legends), an entry may also be `[:fixed px]`;
    - `:row-spans` - `{plot-index rows}` (keyed by plot index, not cell): that plot covers `rows` rows from its own down (e.g. one legend cell beside the whole grid); later plots skip the covered cells, and without `:rows` a span past the last row adds rows, see [[cljplot2.layout.plot-grid/arrange-cells]];
    - `:align` - align the panels of sub-plots in rows and columns (default `true`; `false` - each sub-plot laid out on its own), see [[cljplot2.layout.align]];
    - `:equal-panels` - `true`: after the first layout the columns and rows are resized so every aligned panel has the same width and height (cells with axes get the extra room; fixed / `:measured` columns keep their width) and the space between neighbouring panels is the same at every column and row boundary (the largest needed) - a matrix of panels, e.g. [[pairs]]; the whole multiplot is built twice;
    - `:title` - overall title above the cells (`:labels {:title t}`);
    - `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]` around them; sub-plots keep their own themes.

  Returns a plot map; the grid is generated at build time from the merged theme ([[cljplot2.layout.plot-grid/arrange-grid]], cells `[:plot i]`), where the spec is validated (`ex-info {:key :arrange}`).

  Options are validated first ([[arrange-options]]): an invalid value or an unknown key throws `ex-info {:key k}`, `k` the option (with a spelling suggestion when a known key is close)."
  {:cljplot2/options arrange-options}
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (schema/check-options! arrange-options options)
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :row-spans :align :equal-panels]) :plots plots)}
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

(def ^:private slot-form-meanings
  "The meanings of a pairs slot's forms on its card (both a chart and a sequence of charts)."
  {"chart" "One chart in every such cell, e.g. `(chart/scatter)`." :sequential "Charts layered in every such cell."})

(def ^:private pairs-example-keys
  "The required options of every pairs option example: the variables of the example rows (the cards' placeholder `rows` hold `:a :b :c`)."
  {:variables [:a :b :c]})

(defn- with-pairs-example-keys
  "Gives a pairs options map schema with [[pairs-example-keys]] under the example context of each top-level entry but
  the variables' own (they name the variables themselves); a nested entry (`[:scales :color]`) takes its container's
  context.

  Parameters:

  - `schema-form` (vector): a `[:map props entry ...]` schema form.

  Returns the form with the entries' example contexts merged over the keys."
  [[map-type props & entries]]
  (into [map-type props]
        (map (fn [[k entry-props s :as entry]]
               (if (#{:variables :row-variables :column-variables} k)
                 entry
                 [k (update entry-props :cljplot2/example-context #(merge pairs-example-keys %)) s])))
        entries))

(def pairs-options
  "Schema of the `options` of [[pairs]] (the variables' count and values are checked against the data); sections `:variables` (what the matrix shows), `:cells` (how its cells draw) and `:layout` (the matrix's title, legend column and theme)."
  (-> [:map {:closed true}]
      (into (schema/in-section
             :variables
             [(schema/nilable :variables {:description "The numeric columns, in matrix order on the rows and the columns."
                                          :cljplot2/default-doc "None: required unless `:row-variables` and `:column-variables` are given."
                                          :cljplot2/example [:a :c]}
                              [:sequential {:min 2 :max max-pairs-variables} keyword?])
              (schema/nilable :row-variables {:description "The numeric columns of the matrix rows, top to bottom (on y)."
                                              :cljplot2/default-doc "`:variables`." :cljplot2/example [:c :a]
                                              :cljplot2/example-context {:column-variables [:a :b]}}
                              [:sequential {:min 1 :max max-pairs-variables} keyword?])
              (schema/nilable :column-variables {:description "The numeric columns of the matrix columns, left to right (on x)."
                                                 :cljplot2/default-doc "`:variables`." :cljplot2/example [:c :a]
                                                 :cljplot2/example-context {:row-variables [:a :b]}}
                              [:sequential {:min 1 :max max-pairs-variables} keyword?])
              (schema/nilable :mapping {:description "A mapping added to every cell, for grouping aesthetics such as `:color` or `:group`."
                                        :cljplot2/default-doc "None." :cljplot2/example {:color :g}}
                              data/mapping-schema)
              (schema/nilable :variable-names {:description "The names shown for the variables, `{variable text}`."
                                               :cljplot2/default-doc "The variable's name." :cljplot2/example {:a "alpha"}
                                               :cljplot2/example-context {:diagonal :names}}
                              [:map-of keyword? text/label-text-schema])]))
      (into (schema/in-section
             :cells
             [(schema/nilable :lower {:description "The charts below the diagonal of a square matrix; in a matrix that isn't square, every cell of two different variables."
                                      :cljplot2/default-doc "`(chart/scatter)`." :cljplot2/example '(chart/line)
                                      :cljplot2/value-meanings {:none "Empty cells."}
                                      :cljplot2/form-meanings slot-form-meanings}
                              slot-schema)
              (schema/nilable :upper {:description "The charts above the diagonal, in a square matrix only."
                                      :cljplot2/default-doc "`(chart/correlation {:glyph :number})`." :cljplot2/example :none
                                      :cljplot2/value-meanings {:none "Empty cells."}
                                      :cljplot2/form-meanings slot-form-meanings}
                              slot-schema)
              (schema/nilable :diagonal {:description "The charts of the diagonal cells, whose row and column variable are the same; each cell has its own magnitude scale."
                                         :cljplot2/default-doc "`(chart/density)`." :cljplot2/example :names
                                         :cljplot2/value-meanings {:names "The variable's name, in the text role `:pairs-name`." :none "Empty cells."}
                                         :cljplot2/form-meanings slot-form-meanings}
                              [:maybe [:multi {:dispatch #(cond (#{:names :none} %) :keyword (map? %) :chart :else :charts)
                                               :cljplot2/type-doc ":names, :none, a chart or a vector of charts"}
                                       [:keyword [:enum :names :none]]
                                       [:chart pipeline/chart-schema]
                                       [:charts [:sequential pipeline/chart-schema]]]])
              (schema/nilable :scales {:description "The colour scale of every cell that colours, with one colour bar."}
                              [:map {:closed true} (schema/optional :color {:description "The colour scale spec; its `:domain` wins over the matrix's computed one."
                                                                            :cljplot2/default-doc "The computed range of the coloured statistic."
                                                                            :cljplot2/example {:range [:white :darkred]}
                                                                            :cljplot2/example-context {:upper '(chart/correlation {:glyph :shade})}}
                                                     (aesthetic/scale-spec-schema :color))])]))
      (into (schema/in-section
             :layout
             [(schema/nilable :legend-span {:description "The height of the legend column." :default :row
                                            :cljplot2/example-context {:upper '(chart/correlation {:glyph :shade})}
                                            :cljplot2/value-meanings {:row "Beside the first row, one row high." :matrix "Beside the whole matrix, for a legend taller than a row."}}
                              [:enum :row :matrix])
              (schema/nilable :theme {:description "The theme of every cell and of the matrix; a `[:plot :margin]` or `[:multiplot :gap]` you give replaces the matrix's own tight spacing."
                                      :cljplot2/default-doc "The default theme." :cljplot2/example {:plot {:background :white}}}
                              any?)]))
      (into (schema/in-section :layout (builder-entries-of [:title])))
      (with-pairs-example-keys)
      (schema/with-rules [pairs-variables-problem [:variables]] [pairs-upper-problem [:upper]])))

(def ^{:private true :const true :tag 'double} outer-share-divisor
  "Divisor of the outer track's weight in the first layout of a pairs matrix, see [[outer-weights]]: the outer track
  of `n` tracks weighs `(n - 1) / outer-share-divisor` (at least 1) against 1 for the others, so it has at least
  about a ninth of the room - enough for the axis of a matrix of many small cells."
  8.0)

(defn- outer-weights
  "Gives the relative track weights of the first layout of a pairs matrix along one axis.

  The outer track (the left column, the bottom row) holds the axis: in the first layout, before [[arrange]]'s
  `:equal-panels` gives every panel one size, it takes a larger share, so a matrix of many small cells has room for
  its axis (equal tracks would leave the outer cell narrower than its axis).

  Parameters:

  - `n` (long): the track count.
  - `last-outer` (boolean): the outer track is the last one (rows), else the first (columns).

  Returns a vector of `n` weights: 1 (a long) for every track, the outer one `(n - 1) / `[[outer-share-divisor]] (a double) when that is
  larger - the outer track has at least about a ninth of the room; up to 9 tracks all 1, the plot map as with equal
  weights."
  [^long n last-outer]
  (let [share (m// (m/long-dec n) outer-share-divisor)
        outer (if (m/> share 1.0) share 1)
        inner (vec (repeat (m/long-dec n) 1))]
    (if last-outer (conj inner outer) (into [outer] inner))))

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
    - `:legend-span` - the legend column's height: `:row` (default) beside the first row, one row high; `:matrix` beside the whole matrix (one cell spanning every row, arrange `:row-spans`), so a legend taller than a row fits - e.g. the keys of a matrix of many variables, whose rows are low; a colour bar stretches to the matrix height;
    - `:theme` - every cell's and the matrix's theme (cells have no plot margin and are `[:facet :gap]` apart, as facet panels; a `[:plot :margin]` / `[:multiplot :gap]` in `:theme` wins);
    - `:title` - a title above the matrix.

  All cells of a column share the x domain of its variable, all cells of a row the y domain of its (the variable's
  range widened by 5 %, no nice extension, no chart padding). Tick labels and axis titles (the variable names) only on the outer cells
  - x under the bottom row, y left of the left column - the inner sides keep grid lines (`:axis :grid`). Diagonal
  charts draw in their own magnitude scale; their y axis and grid show the row variable (`:axis-domain`), so the
  top-left cell labels the first variable as the rest of its row. Inner cells have no axis room, so up to 20
  variables with short tick labels (a few characters, e.g. values below 10) fit an image of about 1000 x 800 px (in
  the first layout, before `:equal-panels`, the outer column and row take at least about a ninth of the room); longer
  tick labels (e.g. values in the thousands), a smaller image (about 600 x 500 px fits fewer than 12 variables) or more
  variables leave the outer cells too small for their axes - an error naming the cell. A legend beside the first row
  (`:legend-span :row`) taller than that row wraps into columns side by side and takes width from the matrix: with a
  mapped aesthetic and many variables (about 12 or more) use `:legend-span :matrix`. The cells' legends are hidden; one
  column of `:legend-only` plots, sized to the legend at its cell's height (beside the first row, or the whole matrix - `:legend-span`), shows the mapped aesthetics (keys of the lower charts, a color
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
  [rows {:keys [mapping variable-names theme title row-variables column-variables legend-span] matrix-scales :scales variable-keys :variables
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
        legend-plot (cond-> {:legend-only true :theme cell-theme :data row-maps :mapping pair-mapping :charts legend-charts}
                      (and (seq color-scale) (colors? legend-charts pair-mapping)) (assoc :scales {:color color-scale}))
        x-weights (outer-weights n-columns false)
        matrix-options (fn [columns widths matrix-theme-of]
                         (cond-> {:columns columns :equal-panels true :widths widths :heights (outer-weights n-rows true)
                                  :theme matrix-theme-of}
                           title (assoc :title title)))]
    (cond
      (not legend?)
      (arrange (vec (for [^long i (range n-rows) ^long j (range n-columns)] (cell i j)))
               (matrix-options n-columns x-weights matrix-theme))
      ;; the legend beside the whole matrix: one cell of the extra column spanning every row
      (= :matrix legend-span)
      (arrange (vec (concat (for [^long j (range n-columns)] (cell 0 j))
                            [legend-plot]
                            (for [^long i (range 1 n-rows) ^long j (range n-columns)] (cell i j))))
               (assoc (matrix-options (m/long-inc n-columns) (conj x-weights :measured) matrix-theme)
                      :row-spans {n-columns n-rows}))
      ;; the legend in the first row's cell of an extra column, empty legend-only cells below it
      :else
      (arrange (vec (for [^long i (range n-rows) ^long j (range (m/long-inc n-columns))]
                      (cond (m/< j n-columns) (cell i j)
                            (m/zero? i) legend-plot
                            :else {:legend-only true :theme cell-theme})))
               (matrix-options (m/long-inc n-columns) (conj x-weights :measured) matrix-theme)))))
