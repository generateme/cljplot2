(ns cljplot2.data.facet
  "Facets: splitting rows into panels by facet values, and the layout of the panels.

  Mapping keys: `:facet` (wrap: one dimension laid out row by row) or `:facet-col` / `:facet-row` (grid: one column per `:facet-col` value, one row per `:facet-row` value; either alone gives one row or one column).

  Levels (facet values in panel order) come from an explicit `:scales {k {:domain [...]}}` or from the data, sorted, or ordered by a statistic of each level's rows (`:scales {k {:order-by f :descending b}}`, see [[ordered-levels]]). `nil` values go to an NA level placed last (`:facets {:na-panel false}` drops them instead). Values outside an explicit domain, NaN and infinite numbers are dropped (and counted). Strip text is `(str value)` (NA for the NA level) unless the facet scale has a `:formatter` (its formula or runs values kept, see [[cljplot2.data.text/label-text]]).

  Group keys: the level for wrap, `[col-level row-level]` for grid. Charts whose mapping has no facet key, and charts whose own `:data` has no value for any of their facet keys (e.g. annotation rows), get the group key [[all-panels]] and are drawn in every panel.

  Layout options (plot `:facets`, see [[facet-options]]): wrap shape and order, the NA panel, position scale relations between panels ([[scale-relations]]), strips on / off, panel column / row weights.

  Functions:

  - mapping: [[facet-keys]], [[facet-dims]], [[mapping-problem]]
  - levels and rows: [[levels]], [[ordered-levels]], [[label]], [[group-rows]]
  - options: [[scale-spec-schema]], [[facet-options]], [[scale-relation]], [[panel-scales?]], [[strips?]]
  - wrap layout: [[most-squared-shape]], [[wrap-shape]], [[wrap-positions]]

  Panels: [[single-panel]] - the panel key without facets."
  (:require [fastmath.core :as m]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as data]
            [cljplot2.data.text :as text]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def facet-keys
  "Mapping keys of the facet dimensions."
  #{:facet :facet-col :facet-row})
(def all-panels
  "Group key of a chart without facet mapping: its data is drawn in every panel."
  ::all-panels)
(def single-panel
  "Group key of the only panel of an unfaceted plot."
  :panel)

(def ^:private na-level
  "The NA level: rows with a `nil` facet value."
  nil)

(defn facet-dims
  "Gives the facet dimensions a mapping uses.

  Parameters:

  - `mapping` (map): a (merged) mapping; a `nil` accessor counts as absent (a chart opts out of the plot's faceting with `{:facet nil}`).

  Returns a vector of the facet keys with an accessor, in the order `:facet`, `:facet-col`, `:facet-row` - e.g. `[:facet]`, `[:facet-col :facet-row]`, `[]`."
  [mapping]
  (filterv #(some? (get mapping %)) [:facet :facet-col :facet-row]))

(defn mapping-problem
  "Tells why the facet keys of a mapping can't be used together.

  Parameters:

  - `mapping` (map): a (merged) mapping.

  Returns the message for `:facet` combined with a grid key (`:facet-col` / `:facet-row`), else `nil`."
  [mapping]
  (let [ks (facet-dims mapping)]
    (when (and (some #{:facet} ks) (m/> (count ks) 1))
      "mapping can't combine :facet with :facet-col / :facet-row")))

(defn- valid-facet-value?
  "Tests whether a value can be a facet level.

  Parameters:

  - `v`: any value.

  Returns true for anything except NaN and infinite numbers; `nil` is valid - the NA level."
  [v]
  (or (not (number? v)) (data/valid-value? v)))

(defn levels
  "Gives the levels of a facet dimension in panel order.

  Parameters:

  - `k` (keyword): the facet dimension (for the error).
  - `values` (collection): its facet values, `nil` allowed.
  - `spec` (map): the facet scale spec; its `:domain` is taken as it is.
  - `na-panel?` (boolean): does the NA level get a panel?

  Returns a vector: the explicit domain, else the distinct valid values sorted; the NA level (`nil`) appended when `na-panel?` and some value is `nil`.

  Throws `ex-info {:key :facet}` for values that don't compare (see [[cljplot2.data.rows/sorted-levels]])."
  [k values spec na-panel?]
  (let [explicit (:domain spec)
        base (if explicit
               (vec explicit)
               (data/sorted-levels k (filter #(and (some? %) (valid-facet-value? %)) values) :facet))]
    (if (and na-panel? (some nil? values)) (conj base na-level) base)))

(def scale-spec-schema
  "Facet scale spec (plot `:scales` of a facet key): `:domain` - distinct levels in panel order, `:formatter` - strip
  text of a level, `:order-by` - a function of a level's rows (every chart's rows of that level) to a number: levels
  in ascending order of it (see [[cljplot2.data.levels/order-by-rows]]; not with `:domain`), `:descending` - `true`
  reverses that order (needs `:order-by`)."
  (apply schema/with-rules
   [:map {:closed true :cljplot2/type-doc "a facet scale spec"}
    (schema/nilable :domain {:description "the facet levels in panel order (values outside are dropped)"
                             :cljplot2/default-doc "the data's levels, sorted (NA last)"}
                    (schema/with-rules [:sequential {:min 1} any?]
                      [#(when-not (apply distinct? %) "facet domain values should be distinct")]))
    (schema/optional :formatter {:description "strip text of a level, a function of the level"
                                 :cljplot2/default-doc "(str level), \"NA\" for the NA level"}
                     schema/function)
    (schema/nilable :order-by {:description "levels in ascending order of a function of each level's rows (not with :domain)"}
                    schema/function)
    (schema/nilable :descending {:description "reverse the :order-by order (needs :order-by)" :default false} schema/bool)
    ;; facets are always categorical; accepted for symmetry with the other scales
    (schema/optional :type {:description "facets are always categorical (accepted for symmetry with other scales)"}
                     [:= :categorical])]
   levels/order-by-rules))

(def ^:private scale-relations
  "Values of plot `:facets {:scales {k relation}}`: `:same` - one domain for every panel, `:free` - each panel's own
  data domain, `:sliced` - each panel's own centre with the span of the widest panel."
  #{:same :free :sliced})

(def facet-options
  "Plot `:facets`: `:columns` / `:rows` (wrap layout shape), `:order` (`:top-down` default or `:bottom-up`), `:na-panel`
  (default true - `nil` facet values get a panel), `:scales {:x relation :y relation}` (see [[scale-relations]];
  default `:same`), `:strips` (default true; `false` - no strips), `:widths` / `:heights` - relative sizes of the
  panel columns / rows (positive numbers, one per column / row)."
  [:map {:closed true}
   (schema/nilable :columns {:cljplot2/section :shape :description "columns of a wrap layout" :cljplot2/default-doc "the most square shape (from :rows when given)"}
                   schema/pos-int)
   (schema/nilable :rows {:cljplot2/section :shape :description "rows of a wrap layout" :cljplot2/default-doc "the most square shape (from :columns when given)"}
                   schema/pos-int)
   (schema/nilable :order {:cljplot2/section :shape :description "wrap panel order: :top-down (first level at the top) or :bottom-up (lattice's)"
                           :default :top-down}
                   [:enum :top-down :bottom-up])
   (schema/nilable :na-panel {:cljplot2/section :panels :description "give nil facet values a panel of their own (false - drop those rows)" :default true}
                   schema/bool)
   (schema/nilable :scales {:cljplot2/section :panels :description "position scale relation between panels per axis"}
                   [:map {:closed true}
                    (schema/nilable :x {:description "x relation: :same (one domain), :free (each panel's data) or :sliced (each panel's centre, the widest span)"
                                        :default :same}
                                    (into [:enum] (sort scale-relations)))
                    (schema/nilable :y {:description "y relation: :same (one domain), :free (each panel's data) or :sliced (each panel's centre, the widest span)"
                                        :default :same}
                                    (into [:enum] (sort scale-relations)))])
   (schema/nilable :strips {:cljplot2/section :panels :description "draw the facet strips" :default true} schema/bool)
   (schema/nilable :widths {:cljplot2/section :shape :description "relative widths of the panel columns, one per column" :cljplot2/default-doc "equal"}
                   [:sequential {:min 1} schema/pos])
   (schema/nilable :heights {:cljplot2/section :shape :description "relative heights of the panel rows, one per row" :cljplot2/default-doc "equal"}
                   [:sequential {:min 1} schema/pos])])

(defn scale-relation
  "Gives the relation of a position scale between panels.

  Parameters:

  - `facets` (map): the plot's `:facets` options.
  - `k` (keyword): `:x` or `:y`.

  Returns `:same` (default), `:free` or `:sliced` (see [[scale-relations]])."
  [facets k]
  (or (get-in facets [:scales k]) :same))

(defn panel-scales?
  "Tests whether the plot's facet options ask for panel-local position scales.

  Parameters:

  - `facets` (map): the plot's `:facets` options - `:scales` relations, `:widths`, `:heights`.

  Returns a truthy value for a `:free` / `:sliced` relation on `:x` or `:y`, or panel `:widths` / `:heights`; else `nil`."
  [facets]
  (or (some #(not= :same (scale-relation facets %)) [:x :y]) (:widths facets) (:heights facets)))

(defn strips?
  "Tests whether a faceted plot draws strips.

  Parameters:

  - `facets` (map): the plot's `:facets` options - `:strips` (default true).

  Returns false only for `:strips false`."
  [facets]
  (not (false? (:strips facets))))

(defn ordered-levels
  "Orders the levels of a facet dimension by a statistic of their rows.

  Parameters:

  - `dim-levels` (vector): the levels (see [[levels]]).
  - `rows-of` (function): level -> its rows.
  - `spec` (map): the facet scale spec - `:order-by` (a function of a level's rows to a number), `:descending`.

  Returns the levels in ascending (`:descending` - descending) order of `:order-by` (see [[cljplot2.data.levels/order-by-rows]]), the NA level (`nil`) last; `dim-levels` unchanged without `:order-by`."
  [dim-levels rows-of {:keys [order-by descending]}]
  (if order-by
    (cond-> (levels/order-by-rows (remove nil? dim-levels) rows-of order-by descending)
      (some nil? dim-levels) (conj na-level))
    dim-levels))

(defn label
  "Gives the strip text of a facet level.

  Parameters:

  - `spec` (map): the facet scale spec; its `:formatter` (a function of the level) makes the text.
  - `level`: the level; `nil` - the NA level.

  Returns NA for the NA level, else the formatter's result or the level as a string."
  [spec level]
  (cond
    (nil? level) "NA"
    (:formatter spec) (text/label-text ((:formatter spec) level))
    :else (str level)))

(defn group-rows
  "Groups rows by their facet key.

  Parameters:

  - `rows` (sequence): the rows.
  - `dims` (vector): the facet dimensions (see [[facet-dims]]).
  - `accessors` (map): dimension -> row accessor.
  - `dim-levels` (map): dimension -> its levels.

  Returns `{:groups {key rows} :dropped n}`: a key is the level for one dimension, `[col-level row-level]` for two; rows whose value isn't a level are dropped and counted."
  [rows dims accessors dim-levels]
  (let [level-sets (into {} (map (fn [d] [d (set (dim-levels d))]) dims))
        keyed (map (fn [row]
                     (let [vs (mapv #((accessors %) row) dims)]
                       (when (every? true? (map (fn [d v] (contains? (level-sets d) v)) dims vs))
                         [(if (m/one? (count dims)) (first vs) vs) row])))
                   rows)]
    {:groups (reduce (fn [acc [k row]] (update acc k (fnil conj []) row)) {} (remove nil? keyed))
     :dropped (count (filter nil? keyed))}))

(defn most-squared-shape
  "Gives the wrap layout shape closest to a square for a number of panels.

  The shape follows the old cljplot rule: the minimal `2 (x y - n) + (sqrt n - x)^2 + (sqrt n - y)^2`, ties by smaller x then y.

  Parameters:

  - `n` (long): the number of panels.

  Returns `[rows cols]` with `rows * cols >= n` and `rows <= cols`; `[1 1]` for `n` up to 1."
  [^long n]
  (if (m/<= n 1)
    [1 1]
    (let [s (m/sqrt n)
          dn (double n)
          top (m/long-inc (long (m/ceil s)))
          [_ ^long x ^long y] (first (sort (for [^long x (range 1 (m/long-inc n))
                                                 :let [y0 (long (m/ceil (m// dn x)))]
                                                 ^long y (range y0 (m/long-inc (m/long-max y0 top)))
                                                 :let [d1 (m/long-sub (m/long-mult x y) n)]]
                                             [(m/+ (m/* 2.0 d1)
                                                   (m/sq (m/- s x))
                                                   (m/sq (m/- s y)))
                                              x y])))]
      [(m/long-min x y) (m/long-max x y)])))

(defn wrap-shape
  "Gives the shape of a wrap layout.

  Parameters:

  - `n` (long): the number of panels.
  - `facets` (map): the plot's `:facets` options - `:columns`, `:rows` (each optional; one given - the other fits the panels).

  Returns `[rows cols]`; both given - as they are, neither - [[most-squared-shape]].

  Throws `ex-info {:key :facets}` when `rows * cols` is less than `n`."
  [^long n {:keys [columns rows]}]
  (let [shape (cond
                (and rows columns) [(long rows) (long columns)]
                rows [(long rows) (long (m/ceil (m// n (double rows))))]
                columns [(long (m/ceil (m// n (double columns)))) (long columns)]
                :else (most-squared-shape n))
        [^long r ^long c] shape]
    (when (m/< (m/long-mult r c) n)
      (schema/fail! "Facet layout is too small for the panels." {:key :facets :in [:facets] :panels n :rows r :columns c}))
    shape))

(defn wrap-positions
  "Gives the grid position of a panel in a wrap layout.

  Parameters:

  - `i` (long): the panel's index, 0-based.
  - `rows`, `cols` (long): the layout shape.
  - `order` (keyword): `:top-down` (the first panel at the top) or `:bottom-up` (lattice's: at the bottom).

  Returns `[row col]`, 0-based from the top."
  [^long i ^long rows ^long cols order]
  (let [r (m/quot i cols)
        c (m/rem i cols)]
    (case order
      :top-down [r c]
      :bottom-up [(m/long-sub rows 1 r) c])))
