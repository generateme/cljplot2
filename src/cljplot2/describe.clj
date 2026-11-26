(ns cljplot2.describe
  "Option tables as data, read from the malli schemas and their documenting entry properties (see [[cljplot2.schema]]: `:description`, `:default`, `:cljplot2/default-doc`, `:cljplot2/fragment`).

  The book's reference part renders these rows; nothing here validates. An entry with `:cljplot2/entries-fragment g` isn't flattened: its map's entries are the shared group `g`'s table, its row keeps its value as the default.

  A row: `{:key k :path [k ...] :type text :required bool :default v :default-doc text :description text :fragment kw :section kw}`:

  - `:path` - longer than `[k]` for the entries of nested closed maps (each nested map also has its own row).
  - `:section` - the slice of a long table the entry belongs to (`:cljplot2/section`, else its enclosing entry's): a section slices the table of the map holding the entry, so a flattened nested map with sections of its own (plot `:facets`) slices its own table - show a schema's own table with [[top-level-rows]], a nested map's with [[rows-under]].
  - `:default` / `:default-doc` / `:description` / `:fragment` / `:section` - `nil` when absent.
  - `:values` - `[{:value v :meaning m} ...]`, the literal values the entry accepts (enum members, `:=` constants, `true` / `false`, a named type's `:cljplot2/value-set`), `nil` only when documented; `:forms` - `[{:form k :type text :meaning m} ...]`, its other accepted forms (`k` a named type's keyword or a malli type, `text` the short type text - a named type's name, composites spelled from their children's names); `:other-values` - the meaning of the values without their own; meanings from the entry's `:cljplot2/value-meanings` / `:cljplot2/form-meanings`, else the schema's `:cljplot2/value-meanings` (see [[cljplot2.schema]]). Empty for a flattened map's row.
  - `:example` - the entry's own example (`:cljplot2/example`), only when given; [[example]] gives one for every row it can; `:example-context` - options the example needs (`:cljplot2/example-context`), only when given; [[example-options]] - the example's whole options map.
  - `:theme-default` - true on a chart's style row whose `:default` comes from the theme.
  - `:required` - false in a merged map (`:cljplot2/merged` - the theme, render options: users give partial overrides).
  - `:default` is the value used when the option is absent (a style row's comes from the theme), `:default-doc` the text of a computed default; only a style row may have both - the theme value and how the chart applies it (e.g. only to grouped lines).

  Entries rejecting every value ([[cljplot2.schema/forbidden]]) and internal entries (`:cljplot2/internal`) have no row.

  Functions and vars:

  - [[options]] - rows of a chart value's options (style defaults from theme `[:chart kind]`), of a builder or API function var (its `:cljplot2/options` schema) or of a map schema; [[schema-rows]], [[mapping]] - rows of the mapping keys a chart reads.
  - [[theme-rows]] - the theme's rows with their defaults and text roles; [[style-rows]] - the rows of a named theme style (a builder's) with its defaults.
  - [[fragments]] - the shared groups of options entries belong to, [[fragment-rows]] - the rows of one group (a chart may take only some of a group's entries - its own rows are what it accepts), [[fragment-keys]], [[own-rows]].
  - Slicing: [[top-level-rows]], [[rows-under]], [[container-row?]]; [[type-text]] - readable type text of a schema; [[documented-types]] - the types the book explains; [[example]] - an option's example value, [[example-options]] - its whole options map; [[concepts]] - section titles.
  - Listings: [[api-vars]] - the user-facing data functions (`:cljplot2/api` var metadata; this namespace loads every namespace holding them), [[chart-constructors]], [[plot-level-schemas]], [[attribute-values]]."
  (:require [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.grid :as grid]
            [malli.core :as malli]
            [cljplot2.chart]
            [cljplot2.chart.function :as function-chart]
            [cljplot2.chart.text :as text-chart]
            [cljplot2.content.label :as label]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.graphics.cells :as cell-draw]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.field :as field]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.kde :as kde]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.space3d :as space]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as position]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            ;; namespaces of the user-facing functions (`:cljplot2/api`), so [[api-vars]] finds them all
            [cljplot2.data.contour]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.levels]
            [cljplot2.data.series :as series]
            [cljplot2.data.sets]
            [cljplot2.data.shingle]
            [cljplot2.data.tree :as tree]
            [cljplot2.data.vector-field]
            [cljplot2.graphics.font]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def concepts
  "Section titles of the reference part: chart kind, builder or function -> the concept its section is named after (the heading `## title`; the first sentence names the function). Kinds without an entry keep the kind's name (pages not yet rewritten)."
  {:scatter "Scatter plots" :line "Line charts" :segment "Line segments" :polygon "Polygons" :ribbon "Bands"
   :candlestick "Candlestick charts" :smooth "Smoothing" :rule "Reference lines" :rect "Rectangles" :text "Text labels"
   :image "Images" :function "Function curves" :bar "Bar charts" :bullet "Bullet charts" :correlation "Correlation cells"
   :pairwise "Statistic cells" :box "Box plots" :violin "Violin plots" :extent "Error bars" :stripplot "Strip plots"
   :beeswarm "Beeswarm plots" :dotplot "Dot plots" :rug "Rug plots"})

(def fragments
  "Shared groups of options (`:cljplot2/fragment` of their entries): keyword -> `{:title t :doc d :entries [entry ...]}` (the group's map entries, see [[fragment-rows]])."
  {:common {:title "Common options" :doc "the chart's own data and mapping and its legend glyph, taken by every chart"
            :entries rows/common-options}
   :strip {:title "Strip options" :doc "placement, width and grouping of strip charts - one item per level on a categorical axis"
           :entries strip/options}
   :mirror {:title "Mirrored strips" :doc "the value side a strip chart draws each group on" :entries [strip/mirror-option]}
   :ridge {:title "Ridgelines" :doc "overlapping rows of histograms and densities over a categorical axis"
           :entries strip/ridge-options}
   :annotation {:title "Annotation options" :doc "the extents of annotation charts" :entries annotation/options}
   :cell {:title "Cell style" :doc "the cell shape and outline of grid-cell charts" :entries cell-draw/style-entries}
   :corner-radius {:title "Rounded corners" :doc "rounded bar corners"
                   :entries [draw/corner-radius-option draw/corner-radius-end-option]}
   :curve {:title "Curves" :doc "interpolation and sampling of the line between data points" :entries curve/options}
   :field {:title "Fields" :doc "the scalar field of a raster or contour chart: a function or a kernel density estimate"
           :entries field/source-options}
   :hierarchy-labels {:title "Hierarchy labels" :doc "group and leaf labels of the hierarchy charts (treemap, partition, pack)"
                      :entries hierarchy/label-entries}
   :frame-3d {:title "3D frame" :doc "view, proportions, frame and domains of a 3D chart" :entries space/frame-options}
   :shading {:title "3D shading" :doc "ambient share and light direction of the shaded 3D charts" :entries space/shading-options}
   :line-style {:title "Line style" :doc "colour, width and dash of a line" :entries polyline/style-entries}
   :pattern {:title "Fill patterns" :doc "the pattern drawn over a filled mark" :entries draw/pattern-style-entries}
   :builder {:title "Builder options" :doc "title, scales and theme of a built plot (a builder takes the ones it uses)"
             :entries multiplot/builder-entries}
   :graph/graph {:title "Graph" :doc "edge and node rows of a graph: accessors of the ends, weight and ids; directedness"
                 :entries graph/graph-entries}
   :graph/layout {:title "Graph layout" :doc "node positions of a network: force, circle or random, iterations, gravity, random generator"
                  :entries graph/layout-entries}
   :graph/chord-layout {:title "Chord layout" :doc "node order, sector gaps, ring radius and the smallest flow of a chord diagram"
                        :entries graph/chord-layout-entries}
   :graph/arc-layout {:title "Arc layout" :doc "node order of an arc diagram" :entries graph/arc-layout-entries}
   :graph/sankey-layout {:title "Sankey layout" :doc "columns, node width, gaps and relaxation of a sankey diagram"
                         :entries graph/sankey-layout-entries}
   :graph/alluvial-graph {:title "Alluvial graph" :doc "axes, weights and fill level of an alluvial diagram"
                          :entries graph/alluvial-graph-entries}
   :tree/layout {:title "Dendrogram layout" :doc "cluster cut and circular layout of a dendrogram" :entries tree/layout-entries}
   :tree/node-link-layout {:title "Node-link tree layout"
                           :doc "tidy or cluster placement, link shape and circular layout of a node-link tree"
                           :entries tree/node-link-layout-entries}
   :tree/bundle-paths {:title "Edge bundling" :doc "edge ends and bundling strength of hierarchical edge bundling"
                       :entries tree/bundle-paths-entries}
   :hierarchy/sequence-levels {:title "Sequences" :doc "sequence accessor, step separator, end marker and weights of a flow sunburst"
                               :entries hierarchy/sequence-levels-entries}
   :series/acf-rows {:title "Autocorrelation rows" :doc "lags, significance level and kind of the autocorrelation rows"
                     :entries series/acf-rows-entries}
   :series/horizon-rows {:title "Horizon rows" :doc "row keys, band count and height, and resampling of the horizon bands"
                         :entries series/horizon-rows-entries}
   :arrange {:title "Multiplot layout" :doc "sub-plots in a grid: shape, weights, panel alignment"
             :entries plot-grid/arrange-entries}
   :text-style {:title "Text style" :doc "font, size, colour, inset, alignment and background of a text"
                :entries theme/text-style-overrides}
   :theme-style {:title "Named theme styles" :doc "a theme [:chart name] style merged over the chart kind's style (builders name theirs)"
                 :entries theme/theme-style-entries}})

;; types

(def ^:private predicate-texts
  "Predicate symbol -> its type text."
  {'keyword? "keyword" 'fn? "function" 'ifn? "function" 'string? "string" 'boolean? "true or false"
   'number? "number" 'int? "integer" 'integer? "integer" 'map? "map" 'any? "any value" 'some? "any value but nil"
   'vector? "vector" 'sequential? "sequence" 'set? "set" 'double? "number" 'pos-int? "positive integer"
   'nat-int? "non-negative integer"})

(def ^:private builtin-texts
  "Built-in malli type -> its type text."
  {:string "string" :boolean "true or false" :keyword "keyword" :int "integer" :double "number" :any "any value"
   :nil "nil"})

(defn- message-text
  "Gives an error message as type text.

  Parameters:

  - `message` (string): a malli error message.

  Returns `message` without its leading `should be `."
  [message]
  (str/replace-first message #"^should be " ""))

(defn- bounds-text
  "Gives the bounds text of a collection or number schema.

  Parameters:

  - `properties` (map): the schema's properties - `:min`, `:max`.

  Returns ` (min..max)`, ` (at least min)`, ` (at most max)`, or an empty string without `:min` / `:max`."
  [{lo :min hi :max}]
  (cond
    (and lo hi) (str " (" lo ".." hi ")")
    lo (str " (at least " lo ")")
    hi (str " (at most " hi ")")
    :else ""))

(declare type-text)

(defn- composite-text
  "Returns the type text of a built-in composite schema.

  Parameters:

  - `s`: the malli schema.
  - `t` (keyword): its type (`:maybe`, `:enum`, `:or`, `:map` ...).

  Returns the text, children spelled out by [[type-text]]; `nil` for a type that isn't a known composite."
  [s t]
  (let [ch (malli/children s)]
    (case t
      :maybe (str (type-text (first ch)) " or nil")
      :enum (str "one of " (str/join " " (map pr-str ch)))
      := (pr-str (first ch))
      (:> :>= :< :<=) (str (name t) " " (first ch))
      :or (str/join " or " (map type-text ch))
      :multi (str/join " or " (map (fn [[_ _ c]] (type-text c)) ch))
      :and (str/join ", " (keep #(when-not (= :fn (malli/type (malli/schema %))) (type-text %)) ch))
      (:tuple :cat) (str "[" (str/join ", " (map type-text ch)) "]")
      :? (str "optional " (type-text (first ch)))
      :* (str "any number of " (type-text (first ch)))
      :+ (str "one or more " (type-text (first ch)))
      :map (if (seq ch) (str "{" (str/join ", " (map (fn [[k _ c]] (str k " " (type-text c))) ch)) "}") "map")
      (:vector :sequential :set) (str (name t) " of " (type-text (first ch)) (bounds-text (malli/properties s)))
      :map-of (str "map of " (type-text (first ch)) " to " (type-text (second ch)))
      :fn (or (some-> (:error/message (malli/properties s)) message-text) "a valid value")
      nil)))

(defn type-text
  "Returns the readable type text of a schema, for option tables.

  Parameters:

  - `schema`: a malli schema or form.

  Returns a named type's `:cljplot2/type-doc` (or a schema's own, e.g. a large map's) or its error message without `should be`; built-in types and composites spelled out (`one of :a :b`, `[number number]`, `x or nil` ...); else the printed form."
  [schema]
  (let [s (malli/schema schema)
        t (malli/type s)
        tp (malli/type-properties s)]
    (or (:cljplot2/type-doc tp)
        (:cljplot2/type-doc (malli/properties s))
        (when (and (qualified-keyword? t) (string? (:error/message tp))) (message-text (:error/message tp)))
        (composite-text s t)
        (when-let [text (get builtin-texts t)] (str text (bounds-text (malli/properties s))))
        (when (symbol? t) (get predicate-texts t (str/replace (name t) #"\?$" "")))
        (pr-str (malli/form s)))))

(def ^:private boolean-types
  "Schema types of `true` / `false`: malli's `:boolean`, the predicate `boolean?` and [[cljplot2.schema/bool]]."
  #{:boolean 'boolean? :cljplot2.schema/boolean})

(def documented-types
  "The named types the book's Types page explains, across layers: type keyword -> schema - [[cljplot2.schema/named-types]] and the named types of higher layers carrying `:cljplot2/type-name` and `:cljplot2/type-meaning` (random generator, gradient, position, sample step, panel position, functions, span, quantile range, density kernel, bandwidth rule). Option cards link a form of these types to its explanation."
  (merge schema/named-types
         (into {} (map (fn [s] [(malli/type s) s]))
               [rng/schema polyline/fill-gradient-schema annotation/position curve/sample-step-schema text-chart/panel-position-schema
                function-chart/functions-schema function-chart/span-schema function-chart/quantile-range-schema
                kde/kernel-schema kde/bandwidth-rule-schema])))

(def ^:private bound-words
  "Comparator schema type -> the words of its bound in a short type text."
  {:> "above" :>= "at least" :< "below" :<= "at most"})

(defn- plural
  "Gives the plural of a one-word or short noun phrase: its last word's English plural (`dash` -> `dashes`, `deficiency` -> `deficiencies`, `number` -> `numbers`).

  Parameters:

  - `text` (string): a short type text, e.g. a type name.

  A named type whose name doesn't take this rule gives its own `:cljplot2/type-plural` (see [[form-text]])."
  [text]
  (cond
    (re-find #"(s|sh|ch|x)$" text) (str text "es")
    (re-find #"[^aeiou]y$" text) (str (subs text 0 (dec (count text))) "ies")
    :else (str text "s")))

(declare form-text)

(defn- simple-text?
  "Tests whether a short type text is a plain noun phrase - lowercase words, no code, punctuation, alternatives or composite (`pair of ...`) - which takes a plural after `vector of`."
  [text]
  (and (re-matches #"[a-z][a-z\- ]*" text) (not (re-find #" (or|of) " text))))

(defn- item-text
  "Gives the text of the items of a collection form: the plural of a named type's name (its `:cljplot2/type-plural` when given) or of a plain noun phrase, the plurals of an `:or` of such phrases (`positions or panel positions`), else the item's text in parentheses.

  Parameters:

  - `s`: the item schema."
  [s]
  (let [compiled (malli/schema s)
        own-plural (:cljplot2/type-plural (malli/type-properties compiled))
        text (form-text compiled)
        choices (when (= :or (malli/type compiled)) (map form-text (malli/children compiled)))]
    (cond
      own-plural own-plural
      (simple-text? text) (plural text)
      (and choices (every? simple-text? choices)) (str/join " or " (map plural choices))
      :else (str "(" text ")"))))

(defn- bounded-text
  "Gives the short text of an `:and` schema of a base type and comparator bounds: the base's text followed by the bounds (`integer at least 2`, `number above 0 and below 90`); `nil` for any other `:and` (a predicate child would be dropped).

  Parameters:

  - `children`: the `:and` schema's children."
  [children]
  (let [compiled (map malli/schema children)
        bounds (filter #(contains? bound-words (malli/type %)) compiled)
        others (remove #(contains? bound-words (malli/type %)) compiled)]
    (when (and (seq bounds) (m/one? (count others)))
      (str (form-text (first others)) " "
           (str/join " and " (map #(str (bound-words (malli/type %)) " " (first (malli/children %))) bounds))))))

(defn- form-text
  "Gives the short type text of a form on an option card: a named type's `:cljplot2/type-name` (or a schema's own, e.g. a mapping's), composites spelled with their children's short texts (`vector of open fractions`, `pair of non-negative numbers`, `set of (`:x` or `:y`)`, `map of `:x` or `:y` to padding`, `integer at least 2`; an `:and` keeps every part, a predicate by its error message; alternatives joined by `or`), enum values as code; else [[type-text]] (an option card test then asks for a type name).

  Parameters:

  - `s`: a malli schema (the option table's [[type-text]] stays the validation-based text)."
  [s]
  (let [compiled (malli/schema s)
        t (malli/type compiled)
        children (malli/children compiled)]
    (cond
      (:cljplot2/type-name (malli/type-properties compiled)) (:cljplot2/type-name (malli/type-properties compiled))
      (:cljplot2/type-name (malli/properties compiled)) (:cljplot2/type-name (malli/properties compiled))
      (= :maybe t) (form-text (first children))
      (contains? boolean-types t) "boolean"
      (#{:vector :sequential :set} t) (str (if (= :sequential t) "sequence" (name t)) " of " (item-text (first children)))
      (= :tuple t) (if (and (m/== 2 (count children)) (apply = (map form-text children)))
                     (str "pair of " (item-text (first children)))
                     (str "[" (str/join ", " (map form-text children)) "]"))
      (= :map-of t) (str "map of " (form-text (first children)) " to " (form-text (second children)))
      (= :enum t) (str/join " or " (map #(str "`" (pr-str %) "`") children))
      (= :and t) (or (bounded-text children) (str/join ", " (map form-text children)))
      (= :or t) (str/join " or " (map form-text children))
      (= :multi t) (str/join " or " (map (comp form-text last) children))
      (= :fn t) (type-text compiled)
      :else (type-text compiled))))

(defn- form-of
  "Gives the form of a schema that isn't a literal value: its type key and its short type text.

  Parameters:

  - `s`: a malli schema.

  Returns `{:form k :type text}` - `k` the schema's type (a named type's keyword, a malli type), `text` its [[form-text]]."
  [s]
  (let [compiled (malli/schema s)]
    {:form (malli/type compiled) :type (form-text compiled)}))

(defn- alternatives
  "Gives the values and forms an entry schema accepts, `nil` left out.

  The top-level alternatives of `:maybe`, `:or` and `:multi` (each branch) are taken apart: `:enum` and `:=` give literal values, a boolean type `true` and `false`, a named type with `:cljplot2/value-set` / `:cljplot2/other-types` its values (a vector in its order, a set sorted) and the forms of those types; anything else is one form (see [[form-of]]).

  Parameters:

  - `schema`: an entry's malli schema.

  Returns `{:values [[v meanings] ...] :forms [{:form :type :meaning} ...]}` - a form's meaning from its named type's `:cljplot2/form-meanings` (`nil` without); `meanings` the value meanings of the schema the value comes from (`:cljplot2/value-meanings` of an enum's or a named type's properties), or `nil`. Throws `ex-info` for an `:cljplot2/other-types` keyword that isn't in [[documented-types]]."
  [schema]
  (let [s (malli/schema schema)
        t (malli/type s)
        tp (malli/type-properties s)
        children (malli/children s)]
    (cond
      (= :maybe t) (alternatives (first children))
      (= :or t) (apply merge-with into {:values [] :forms []} (map alternatives children))
      (= :multi t) (apply merge-with into {:values [] :forms []} (map (comp alternatives last) children))
      (= :enum t) {:values (mapv (fn [v] [v (:cljplot2/value-meanings (malli/properties s))]) children) :forms []}
      (= := t) {:values [[(first children) nil]] :forms []}
      (contains? boolean-types t) {:values [[true nil] [false nil]] :forms []}
      (or (:cljplot2/value-set tp) (:cljplot2/other-types tp))
      {:values (mapv (fn [v] [v (:cljplot2/value-meanings tp)]) (let [vs (:cljplot2/value-set tp)] (if (sequential? vs) vs (sort-by str vs))))
       :forms (mapv (fn [k]
                      (assoc (form-of (or (get documented-types k)
                                          (throw (ex-info (str "Not a documented type: " k) {:key :cljplot2/other-types :value k}))))
                             :meaning (get (:cljplot2/form-meanings tp) k)))
                    (:cljplot2/other-types tp))}
      :else {:values [] :forms [(form-of s)]})))

(defn- option-forms
  "Gives the row keys describing what an entry accepts: its values with their meanings and its other forms.

  Parameters:

  - `props` (map): the entry's properties - `:cljplot2/value-meanings` (value meanings, winning over the schema's; a `nil` key adds `nil` as a value), `:cljplot2/form-meanings`, `:cljplot2/other-values`.
  - `s`: the entry's malli schema.

  Returns `{:values [{:value v :meaning m} ...] :forms [{:form k :type text :meaning m} ...] :other-values text}` - each value and each form once, in schema order (`nil` first when documented), a value with the first meaning any part of the schema gives it; meanings `nil` when absent."
  [props s]
  (let [{:keys [values forms]} (alternatives s)
        own (:cljplot2/value-meanings props)
        listed (cond->> values (contains? own nil) (cons [nil nil]))]
    {:values (let [entries (map (fn [[v meanings]] {:value v :meaning (get own v (get meanings v))}) listed)
                   by-value (group-by :value entries)]
               (mapv (fn [v] (let [same (by-value v)] (or (first (filter :meaning same)) (first same))))
                     (distinct (map :value entries))))
     :forms (into [] (comp (distinct) (map #(assoc % :meaning (get (:cljplot2/form-meanings props) (:form %) (:meaning %))))) forms)
     :other-values (:cljplot2/other-values props)}))

(defn example
  "Gives the example value of an option row: shown as code next to the option.

  Parameters:

  - `row` (map): an option row (see the namespace doc).

  Returns `[v]` - the example wrapped, so `false` is one too: the row's own `:example` (the entry's `:cljplot2/example`) when it has one, else the first of its values - those with a meaning first - other than its `:default` and `nil`; `nil` when there is none (a row taking no literal value needs its own example)."
  [{:keys [values default] :as row}]
  (if (contains? row :example)
    [(:example row)]
    (some (fn [{:keys [value]}] (when (and (some? value) (not= value default)) [value]))
          (concat (filter :meaning values) (remove :meaning values)))))

(defn example-options
  "Gives the options map of an option row's example: the option at its path, with what it needs to be valid and to act.

  Parameters:

  - `rows` (sequence): the option rows the row belongs to (its containers among them).
  - `row` (map): an option row.

  Returns the example contexts (`:cljplot2/example-context`, e.g. `{:band :bootstrap}`) of the containers above the row and of the row itself, merged, with the map defaults of those containers (e.g. the theme's `:bridge`) and the [[example]] at the row's path; `nil` without an example."
  [rows {:keys [path example-context] :as row}]
  (when-let [[v] (example row)]
    (let [by-path (into {} (map (juxt :path identity)) rows)
          containers (map #(by-path (subvec path 0 %)) (range 1 (count path)))
          context (reduce theme/deep-merge {} (concat (keep :example-context containers) [example-context]))
          base (reduce (fn [m {container-path :path d :default}]
                         (if (map? d) (update-in m container-path #(merge d %)) m))
                       (or context {})
                       (remove nil? containers))]
      (assoc-in base path v))))

;; rows

(defn- entries-map
  "Gives the `:map` schema holding the entries of a schema.

  Parameters:

  - `schema`: a malli schema.

  Returns the map found directly, under `:maybe` or as the first child of `:and` (as [[cljplot2.schema/with-rules]] builds); `nil` when there is none."
  [schema]
  (let [s (malli/schema schema)]
    (case (malli/type s)
      :map s
      (:maybe :and) (entries-map (first (malli/children s)))
      nil)))

(defn- nested-map
  "Gives the closed `:map` of an entry's schema, whose entries get rows of their own - flattened under the entry's row.

  Every such entry is a [[container?]]; not the reverse: a map under validation rules (`:and`, e.g. a scale spec) or a map of entries isn't flattened (it has a table of its own).

  Parameters:

  - `s`: the entry's schema.

  Returns the closed map (also under `:maybe`), or `nil`."
  [s]
  (let [compiled (malli/schema s)]
    (case (malli/type compiled)
      :map (when (:closed (malli/properties compiled)) compiled)
      :maybe (nested-map (first (malli/children compiled)))
      nil)))

(defn- container?
  "Tests whether an entry schema holds entries of its own - a map or a map of entries (also under `:maybe` / `:and`).

  Its row takes no default value (see [[with-defaults]]); its entries' rows do, when [[nested-map]] flattens them.

  Parameters:

  - `s`: the entry's malli schema (a form or a schema).

  Returns true for a `:map` or `:map-of` (also under `:maybe` / `:and`), else false."
  [s]
  (let [compiled (malli/schema s)]
    (case (malli/type compiled)
      (:map :map-of) true
      (:maybe :and) (container? (first (malli/children compiled)))
      false)))

(defn- flattened?
  "Tests whether the entries of an entry are flattened under its row: a [[nested-map]] whose entries aren't documented by a shared group (`:cljplot2/entries-fragment`).

  Parameters:

  - `props` (map): the entry's properties.
  - `s`: the entry's malli schema.

  Returns true when its entries get rows after its own, else false."
  [props s]
  (and (some? (nested-map s)) (not (:cljplot2/entries-fragment props))))

(defn- entry-row
  "Returns the row of one map entry (see the namespace doc).

  Parameters:

  - `[path k props s]`: the entry's key path, key, properties and schema.
  - `merged?` (boolean): the entry belongs to a merged map (theme, render options: every key is there after merging, users give partial overrides - so not required).
  - `section` (keyword or `nil`): the enclosing entry's section, taken when the entry has no `:cljplot2/section` of its own.

  The row's metadata `::container` marks an entry holding entries of its own (see [[with-defaults]]) - not one whose entries a shared group documents (`:cljplot2/entries-fragment`): that row keeps its own value as the default (e.g. a text role's style)."
  [[path k props s] merged? section]
  (let [flat? (flattened? props s)]
    (with-meta (cond-> (into {:key k :path path :type (if flat? "map" (type-text s))
                              :required (and (not merged?) (not (:optional props)))
                              :default (:default props) :default-doc (:cljplot2/default-doc props) :description (:description props)
                              :fragment (:cljplot2/fragment props) :section (or (:cljplot2/section props) section)}
                             (if flat? {:values [] :forms [] :other-values nil} (option-forms props s)))
                 (contains? props :cljplot2/example) (assoc :example (:cljplot2/example props))
                 (:cljplot2/example-context props) (assoc :example-context (:cljplot2/example-context props)))
      {::container (and (container? s) (not (:cljplot2/entries-fragment props)))})))

(defn- map-rows
  "Returns the rows of the entries of a map schema, nested closed maps flattened after their own row.

  Parameters:

  - `m`: the malli `:map` schema.
  - `path` (vector): the key path of `m` in the documented schema.
  - `merged?`, `section`: as in [[entry-row]], of the enclosing entry; a `:cljplot2/merged` map makes its nested maps merged too.

  Forbidden and `:cljplot2/internal` entries have no row."
  [m path merged? section]
  (let [map-merged? (or merged? (true? (:cljplot2/merged (malli/properties m))))]
    (into [] (mapcat (fn [[k props s]]
                       (when-not (or (schema/forbidden? s) (:cljplot2/internal props))
                         (let [p (conj path k)
                               row (entry-row [p k props s] map-merged? section)]
                           (cons row (when (flattened? props s) (map-rows (nested-map s) p map-merged? (:section row))))))))
          (malli/children m))))

(defn schema-rows
  "Returns the option rows of a map schema (see the namespace doc for a row).

  Parameters:

  - `schema`: a map schema (also under `:maybe` or `:and`), or `nil`.

  Returns a vector of rows; empty for `nil` or a schema without entries. See also [[options]]."
  [schema]
  (if-let [m (some-> schema entries-map)] (map-rows m [] false nil) []))

(defn- chart-rows
  "Gives the option rows of a chart value.

  Parameters:

  - `chart` (map): a chart value - `:kind`, `:schema` (`:options`, `:style`).

  Returns its options schema's rows, the top-level style entries with their `:default` from the default theme `[:chart kind]` and `:theme-default true`."
  [{:keys [kind] :as chart}]
  (let [style-keys (set (map :key (schema-rows (get-in chart [:schema :style]))))
        style (get-in theme/default-theme [:chart kind])]
    (mapv (fn [{option-key :key path :path :as row}]
            (cond-> row
              (and (m/one? (count path))
                   (contains? style-keys option-key)
                   (contains? style option-key))
              (assoc :default (get style option-key) :theme-default true)))
          (schema-rows (get-in chart [:schema :options])))))

(defn- with-defaults
  "Fills the defaults of rows from a nested map of values.

  Parameters:

  - `rows` (sequence): option rows.
  - `defaults` (map): nested values, read at each row's `:path`.

  Returns a vector of the rows, each without a `:default` given the value at its path - except a container row (a map or a map of entries, see [[entry-row]]: its entries' rows carry the values)."
  [rows defaults]
  (mapv (fn [{:keys [path] :as row}]
          (let [v (get-in defaults path)]
            (cond-> row (and (nil? (:default row)) (some? v) (not (::container (meta row)))) (assoc :default v))))
        rows))

(defn- text-roles-schema
  "Returns the theme's `:text` as a merged map schema of the roles of [[cljplot2.theme/text-roles]] (sorted), each a text style documented by the shared group `:text-style`, in its role's section - the rows the theme's `map-of` can't give."
  []
  (into [:map {:closed true :cljplot2/merged true}]
        (map (fn [[role {:keys [description section]}]]
               [role {:description description :cljplot2/section section :cljplot2/entries-fragment :text-style}
                theme/text-style-schema]))
        (sort-by key theme/text-roles)))

(defn theme-rows
  "Returns the option rows of the theme with their defaults from [[cljplot2.theme/default-theme]].

  The rows of [[cljplot2.theme/theme-schema]], then one row `[:text role]` per text role of [[cljplot2.theme/text-roles]] (sorted; its default the role's style). The style keys are one shared table: `(fragment-rows :text-style)`.

  Parameters:

  - `k` (keyword, optional): a top-level theme key (e.g. `:legend`) - only the rows under it.

  Throws `ex-info {:key :theme-key}` for a key the theme hasn't (also `nil`)."
  ([]
   (with-defaults (into (schema-rows theme/theme-schema)
                        (map-rows (malli/schema (text-roles-schema)) [:text] false nil))
                  theme/default-theme))
  ([k]
   (let [rows (filterv #(= k (first (:path %))) (theme-rows))]
     (when (empty? rows)
       (schema/fail! (str "Unknown theme key " (pr-str k) ", known: " (str/join " " (keys theme/default-theme)) ".")
                     {:key :theme-key :value k}))
     rows)))

(defn style-rows
  "Returns the option rows of a named theme style (a builder's) or of a chart kind's theme style.

  Parameters:

  - `style-name` (keyword): a key of [[cljplot2.theme/named-styles]], e.g. `:network-node`, or a built-in chart kind (a key of [[cljplot2.chart/builtin-styles]]) - its own theme style, e.g. keys a theme sets that aren't chart options.
  - `style-keys` (sequence of keywords, optional): only the rows of these top-level style keys (a map key with its nested rows) - those the builder's marks show (e.g. a line style's colour, width and dash, not its area fill); default every style key of the kind.

  Returns the style keys of its chart kind, each with its default from the default theme - the named style merged over the kind's (see [[cljplot2.theme/chart-style]]); one table, without the sections slicing the kind's own options table.

  Throws `ex-info {:key :style-name}` for a name that is neither registered nor a chart kind, `{:key :style-keys}` for `style-keys` empty or holding a key that isn't a style key of the kind."
  ([style-name] (style-rows style-name nil))
  ([style-name style-keys]
   (let [kind (or (get theme/named-styles style-name)
                  (when (contains? cljplot2.chart/builtin-styles style-name) style-name))]
     (when-not kind
       (schema/fail! (str "Unknown named style or chart kind " (pr-str style-name) ", known styles: "
                          (str/join " " (sort (keys theme/named-styles))) ".")
                     {:key :style-name :value style-name}))
     (let [rows (mapv #(assoc % :section nil)
                      (with-defaults (schema-rows (get cljplot2.chart/builtin-styles kind))
                                     (theme/chart-style theme/default-theme kind {:theme-style style-name})))
           top-keys (distinct (map (comp first :path) rows))]
       (when (and (some? style-keys) (or (empty? style-keys) (not-every? (set top-keys) style-keys)))
         (schema/fail! (str "Style keys of " (pr-str style-name) " should be some of: "
                            (str/join " " (map pr-str top-keys)) ".")
                       {:key :style-keys :value style-keys}))
       (if style-keys
         (filterv #(contains? (set style-keys) (first (:path %))) rows)
         rows)))))

(defn fragment-rows
  "Returns the option rows of a shared group's entries.

  Parameters:

  - `fragment` (keyword): a key of [[fragments]].

  Returns the rows; empty for an unknown group. See also [[fragment-keys]]."
  [fragment]
  (schema-rows (into [:map] (get-in fragments [fragment :entries]))))

(defn container-row?
  "Tests whether a row is the row of a map holding options of its own (a heading of its entries' rows, no default), not an option.

  Parameters:

  - `row` (map): an option row (see the namespace doc).

  Returns truthy for a container row, `nil` otherwise - also for a row map built by hand (without the row metadata). See also [[top-level-rows]]."
  [row]
  (::container (meta row)))

(defn top-level-rows
  "Returns the rows for the top-level keys (`:path` of one key) - a schema's own table without its nested maps' entries (those have tables, and sections, of their own).

  Parameters:

  - `rows` (sequence): option rows."
  [rows]
  (filterv #(m/one? (count (:path %))) rows))

(defn rows-under
  "Returns the rows nested under a key path, without the row of the path itself.

  Parameters:

  - `rows` (sequence): option rows.
  - `path` (vector): a key path, e.g. `[:groups]` - the entries of the plot's `:groups` map.

  Returns a vector; empty when nothing is nested there."
  [rows path]
  (let [n (count path)]
    (filterv #(and (m/< n (count (:path %))) (= path (subvec (:path %) 0 n))) rows)))

(defn- under-path?
  "Tests whether a key path is a root path or nested under it.

  Parameters:

  - `root` (vector): the root key path.
  - `path` (vector): the key path tested.

  Returns true when `path` starts with `root` (also when equal), else false."
  [root path]
  (and (m/<= (count root) (count path)) (= root (subvec path 0 (count root)))))

(defn own-rows
  "Returns the rows outside the shared groups - a chart's or builder's own table.

  Parameters:

  - `rows` (sequence): option rows.

  The rows of entries with a `:fragment` and the rows nested under them are left out (the groups have tables in the Shared options chapter, see [[fragment-keys]])."
  [rows]
  (let [roots (keep #(when (:fragment %) (:path %)) rows)]
    (filterv (fn [{:keys [path]}] (not-any? #(under-path? % path) roots)) rows)))

(defn fragment-keys
  "Returns the shared groups of option rows with the keys each contributes.

  Parameters:

  - `rows` (sequence): option rows.

  Returns a sequence of `[fragment keys]` pairs (`keys` - a vector) - each group with the keys of its entries `rows` holds (a chart may take only some of a group's entries), groups in the order of their first row; empty without groups."
  [rows]
  (let [tagged (filter :fragment rows)]
    (mapv (fn [fragment] [fragment (into [] (comp (filter #(= fragment (:fragment %))) (map :key)) tagged)])
          (distinct (map :fragment tagged)))))

(defn options
  "Returns the option rows (see the namespace doc) of a chart, builder, function or schema.

  Parameters:

  - `x`: a chart value (style rows take their default from theme `[:chart kind]`), a var with `:cljplot2/options` metadata (a builder or API function) or a map schema.

  See also [[own-rows]], [[fragment-keys]]."
  [x]
  (cond
    (cljplot2.chart/chart-value? x) (chart-rows x)
    (var? x) (schema-rows (:cljplot2/options (meta x)))
    :else (schema-rows x)))

(defn mapping
  "Gives the rows of the mapping keys a chart value reads.

  Parameters:

  - `chart` (map): a chart value.

  Returns the rows of its `:mapping` schema's entries (see the namespace doc for a row); empty without one."
  [chart]
  (schema-rows (get-in chart [:schema :mapping])))

(defn chart-constructors
  "Returns kind -> constructor var of every built-in chart (the public functions of `cljplot2.chart` returning a chart value when called without arguments), sorted by kind."
  []
  (into (sorted-map)
        (for [v (vals (ns-publics 'cljplot2.chart))
              :let [c (try (@v) (catch Exception _ nil))]
              :when (cljplot2.chart/chart-value? c)]
          [(:kind c) v])))

(def attribute-values
  "The built-in values of attribute options, sorted: `:shape` - point shape characters (shape maps are open), `:dash` - dash style names (dash arrays are open), `:pattern` - fill pattern names (pattern maps are open), `:cell` - grid cell types."
  {:shape (vec (sort-by str shape/shapes))
   :dash (vec (sort (keys schema/dash-styles)))
   :pattern (vec (sort (keys schema/pattern-styles)))
   :cell (vec (sort grid/cell-names))})

(def plot-level-schemas
  "Every non-chart schema users write: name -> schema (the aesthetic scale specs one per dimension; margins also as the map form of one margin)."
  (into {:plot pipeline/plot-schema
         :render-options pipeline/options-schema
         :mapping rows/mapping-schema
         :position-scale position/scale-spec-schema
         :facet-scale facet/scale-spec-schema
         :facets facet/facet-options
         :labels plot-grid/labels-schema
         :label label/label-options
         :arrange plot-grid/arrange-schema
         :coord coord/coord-schema
         :polar-coord coord/polar-schema
         :margins pipeline/margins-schema
         :margin-spec pipeline/margin-spec-schema
         :theme theme/theme-schema}
        (map (fn [dim] [(keyword (str (name dim) "-scale")) (aesthetic/scale-spec-schema dim)]))
        aesthetic/dims))

(defn api-vars
  "Returns the user-facing functions - vars with `:cljplot2/api` metadata in the loaded `cljplot2.*` namespaces - sorted by namespace and name."
  []
  (->> (all-ns)
       (filter #(str/starts-with? (name (ns-name %)) "cljplot2."))
       (mapcat (comp vals ns-publics))
       (filter #(:cljplot2/api (meta %)))
       (sort-by (fn [v] [(str (:ns (meta v))) (str (:name (meta v)))]))
       vec))
