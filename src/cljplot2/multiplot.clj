(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).

  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]]. Sub-plots keep their own themes.

  Functions: [[arrange]] (plots in a grid, options [[arrange-options]]), [[pairs]] (a scatter-plot matrix, options [[pairs-options]]); [[builder-entries]] - the option entries every builder takes."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
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
  "A plot map arranging plots in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

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

(def ^{:const true :tag 'long} max-pairs-variables
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
  - `variables` (sequence of keywords): the variables.

  Returns `{variable [lo hi]}` (see [[variable-domain]]); `nil` and non-finite numbers are left out. Throws `ex-info {:key :variables}` for a variable without finite numbers or with a value that is neither a number nor `nil`."
  [rows variables]
  (into {} (map (fn [k]
                  (let [acc (data/row-accessor k k)
                        vs (keep acc rows)
                        finite (filter (fn [v] (and (number? v) (m/valid-double? v))) vs)]
                    (when (or (empty? finite) (some (fn [v] (not (number? v))) vs))
                      (schema/fail! (str "Pairs variable " k " needs numeric values.") {:key :variables :value k}))
                    [k (variable-domain finite)])))
        variables))

(defn- slot-charts
  "The charts of a pairs slot.

  Parameters:

  - `slot` (chart, sequence of charts or `nil`): a `:lower`, `:upper` or `:diagonal` value.

  Returns a vector of charts, empty for `nil`."
  [slot]
  (cond (nil? slot) [] (map? slot) [slot] :else (vec slot)))

(defn- chart-value?
  "Is `v` a single chart value (a map), not a sequence of charts?"
  [v]
  (map? v))

(def ^:private slot-schema
  "Schema of a pairs slot: a chart, a sequence of charts or `nil` (none)."
  [:maybe [:multi {:dispatch chart-value? :cljplot2/type-doc "a chart or a vector of charts"}
           [true pipeline/chart-schema] [false [:sequential pipeline/chart-schema]]]])

(def pairs-options
  "Schema of the `options` of [[pairs]] (the variables' count and values are checked against the data)."
  (-> [:map {:closed true}
       [:variables {:description "the numeric columns, in matrix order"}
        [:sequential {:min 2 :max max-pairs-variables} keyword?]]
       (schema/nilable :mapping {:description "extra mapping of every cell (grouping aesthetics such as :color, :group)"}
                       data/mapping-schema)
       (schema/optional :lower {:description "charts below the diagonal; nil - empty cells"
                                :cljplot2/default-doc "(chart/scatter)"}
                        slot-schema)
       (schema/optional :upper {:description "charts above the diagonal; nil - empty cells"
                                :cljplot2/default-doc "(chart/correlation {:glyph :number})"}
                        slot-schema)
       (schema/optional :diagonal {:description "charts of the diagonal cells (in their own magnitude scale); :names - the variable's name (text role :pairs-name); nil - empty cells"
                                   :cljplot2/default-doc "(chart/density)"}
                        [:maybe [:multi {:dispatch #(cond (= :names %) :names (map? %) :chart :else :charts)
                                         :cljplot2/type-doc ":names, a chart or a vector of charts"}
                                 [:names [:= :names]]
                                 [:chart pipeline/chart-schema]
                                 [:charts [:sequential pipeline/chart-schema]]]])
       (schema/nilable :labels {:description "display names of the variables, {variable text}"
                                :cljplot2/default-doc "the variable's name"}
                       [:map-of keyword? string?])
       (schema/nilable :theme {:description "theme of every cell and of the matrix (a [:plot :margin] or [:multiplot :gap] given wins over the cells' facet-like spacing)"}
                       any?)]
      (into (builder-entries-of [:title]))))

(defn pairs
  "A scatter-plot matrix of numeric variables as an [[arrange]] plot map: cell (row i, column j) plots variable j on x against variable i on y.

  Parameters:

  - `data` (rows or columns): the data.
  - `options` (map):
    - `:variables` - 2 to [[max-pairs-variables]] numeric columns (keywords), in matrix order (required);
    - `:mapping` - extra mapping of every cell (grouping aesthetics such as `:color`, `:group`);
    - `:lower` / `:upper` - charts below / above the diagonal: a chart, a vector of charts, `nil` (an empty cell); defaults `(chart/scatter)` and `(chart/correlation {:glyph :number})`;
    - `:diagonal` - charts of the diagonal cells (x - the variable, y their own magnitude scale with its axis hidden), default `(chart/density)`; `:names` - the variable's name in the middle of the cell (text role `:pairs-name`);
    - `:labels` - `{variable text}`, display names (default the variable's name);
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

  Returns a plot map. Throws `ex-info {:key :variables}` for a wrong variable count, a missing or non-numeric variable; options are validated first ([[pairs-options]], `ex-info {:key k}`, `k` the option).

  Equivalents: GGally `ggpairs` - the defaults (scatter below, correlation text above - per group when grouped -,
  density on the diagonal); R `corrgram` / `pairs` panel functions - `chart/correlation` glyphs in `:lower` / `:upper`
  (`:shade`, `:pie`, `:ellipse`, ...) with `:diagonal :names`. A correlation matrix as a heatmap (one tile per pair)
  is `chart/tile` over `fastmath.stats/correlation-matrix` rows instead."
  {:cljplot2/options pairs-options}
  [data {:keys [variables mapping lower upper diagonal labels theme title]
         :or {lower ::default upper ::default diagonal ::default} :as options}]
  (let [_ (schema/check-options! pairs-options options)
        variables (vec variables)
        n (count variables)
        n- (m/dec n)
        n+ (m/inc n)
        rows (data/rows data)
        domains (variable-domains rows variables)
        name-of (fn [k] (get labels k (name k)))
        lower (if (= ::default lower) [(chart/scatter)] (slot-charts lower))
        upper (if (= ::default upper) [(chart/correlation {:glyph :number})] (slot-charts upper))
        diagonal (if (= ::default diagonal) [(chart/density)] diagonal)
        mapped? (seq (aesthetic/mapped-dims mapping))
        ;; aesthetics with continuous values (e.g. a numeric :color) color marks without grouping: left out on the
        ;; diagonal, whose 1d charts group by their aesthetics
        continuous (set (filter (fn [k] (let [vs (keep (data/row-accessor k (mapping k)) rows)]
                                         (and (seq vs) (every? number? vs) (not (accessor/categorical? (mapping k))))))
                                (aesthetic/mapped-dims mapping)))
        diagonal-mapping (apply dissoc mapping continuous)
        pair-mapping (assoc mapping :x (variables 1) :y (variables 0))
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
               (let [vx (variables j) vy (variables i)
                     diag? (m/== i j) bottom? (m/== i n-) left? (m/zero? j)
                     names? (and diag? (= :names diagonal))
                     charts (cond names? [(chart/text {:x [:panel 0.5] :y [:panel 0.5] :text (name-of vx) :role :pairs-name})]
                                  diag? (slot-charts diagonal)
                                  (m/> i j) lower
                                  :else upper)]
                 (cond-> {:data rows
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
                   true (assoc :theme cell-theme))))
        legend-cell (fn [^long i] (cond-> {:legend-only true :theme cell-theme}
                                   (m/zero? i) (assoc :data rows :mapping pair-mapping :charts legend-charts)))
        plots (vec (for [^long i (range n)
                         ^long j (range (if legend? n+ n))]
                     (if (m/== j n) (legend-cell i) (cell i j))))]
    (arrange plots (cond-> {:columns (if legend? n+ n) :equal-panels true :theme matrix-theme}
                     legend? (assoc :widths (conj (vec (repeat n 1)) :measured))
                     title (assoc :title title)))))
