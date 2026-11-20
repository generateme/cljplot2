(ns cljplot2.describe
  "Option tables as data, read from the malli schemas and their documenting entry properties (see
  [[cljplot2.schema]]: `:description`, `:default`, `:cljplot2/default-doc`, `:cljplot2/fragment`).

  [[options]] - rows of a chart value's options (style defaults from theme `[:chart kind]`), of a builder or API
  function var (its `:cljplot2/options` schema) or of a map schema; [[mapping]] - rows of the mapping keys a chart
  reads; [[api-vars]] - the user-facing data functions (`:cljplot2/api` var metadata; this namespace loads every
  namespace holding them); [[fragments]] - the shared groups of options entries belong to, [[fragment-rows]] - the
  rows of one group (a chart may take only some of a group's entries - its own rows are what it accepts);
  [[theme-rows]] - the theme's rows with their defaults and text roles. An entry with `:cljplot2/entries-fragment g`
  isn't flattened: its map's entries are the shared group `g`'s table, its row keeps its value as the default.

  A row: `{:key k :path [k ...] :type text :required? bool :default v :default-doc text :description text
  :fragment kw}` - `:path` is longer than `[k]` for the entries of nested closed maps (each nested map also has its
  own row); `:default` / `:default-doc` / `:description` / `:fragment` are `nil` when absent; `:required?` is false in a
  merged map (`:cljplot2/merged` - the theme, render options: users give partial overrides). Entries rejecting every
  value ([[cljplot2.schema/forbidden]]) and internal entries (`:cljplot2/internal`) have no row. `:default` is the value
  used when the option is absent (a style row's comes from the theme), `:default-doc` the text of a computed default;
  only a style row may have both - the theme value and how the chart applies it (e.g. only to grouped lines)."
  (:require [clojure.string :as str]
            [malli.core :as malli]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.cells :as cells]
            [cljplot2.data.field :as field]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.space3d :as space]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.multiplot :as multiplot]
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

(def fragments
  "Shared groups of options (`:cljplot2/fragment` of their entries): keyword -> `{:title t :doc d :entries [entry ...]}`
  (the group's map entries, see [[fragment-rows]])."
  {:common {:title "Data and mapping" :doc "the chart's own data and mapping, taken by every chart"
            :entries rows/common-options}
   :strip {:title "Strip options" :doc "placement, width and grouping of strip charts - one item per level on a categorical axis"
           :entries strip/options}
   :mirror {:title "Mirrored strips" :doc "the value side a strip chart draws each group on" :entries [strip/mirror-option]}
   :ridge {:title "Ridgelines" :doc "overlapping rows of histograms and densities over a categorical axis"
           :entries strip/ridge-options}
   :annotation {:title "Annotation options" :doc "the extents of annotation charts" :entries annotation/options}
   :cell {:title "Cell style" :doc "the cell shape and outline of grid-cell charts" :entries cells/style-entries}
   :corner-radius {:title "Rounded corners" :doc "rounded bar corners"
                   :entries [draw/corner-radius-option draw/corner-radius-end-option]}
   :curve {:title "Curves" :doc "interpolation and sampling of the line between data points" :entries curve/options}
   :field {:title "Fields" :doc "the scalar field of a raster or contour chart: a function or a kernel density estimate"
           :entries field/options}
   :hierarchy-labels {:title "Hierarchy labels" :doc "group and leaf labels of the hierarchy charts (treemap, partition, pack)"
                      :entries hierarchy/label-entries}
   :frame-3d {:title "3D frame" :doc "view, proportions, frame and domains of a 3D chart" :entries space/frame-options}
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
   :text-style {:title "Text style" :doc "font, size, colour, padding, alignment and background of a text"
                :entries theme/text-style-overrides}})

;; types

(def ^:private predicate-texts
  {'keyword? "keyword" 'fn? "function" 'ifn? "function" 'string? "string" 'boolean? "true or false"
   'number? "number" 'int? "integer" 'integer? "integer" 'map? "map" 'any? "any value" 'some? "any value but nil"
   'vector? "vector" 'sequential? "sequence" 'set? "set" 'double? "number" 'pos-int? "positive integer"
   'nat-int? "non-negative integer"})

(def ^:private builtin-texts
  {:string "string" :boolean "true or false" :keyword "keyword" :int "integer" :double "number" :any "any value"
   :nil "nil"})

(defn- message-text
  "An error message as type text: without its leading \"should be \"."
  [message]
  (str/replace-first message #"^should be " ""))

(defn- bounds-text
  "`\" (min..max)\"`-like text of a schema's `:min` / `:max` properties, or \"\"."
  [{:keys [min max]}]
  (cond
    (and min max) (str " (" min ".." max ")")
    min (str " (at least " min ")")
    max (str " (at most " max ")")
    :else ""))

(declare type-text)

(defn- composite-text
  "Type text of a built-in composite schema `s` of type `t`, or `nil` for another type."
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
  "Readable text of `schema`'s type: a named type's `:cljplot2/type-doc` (or a schema's own, e.g. a large map's) or
  error message, built-in types and composites spelled out (`\"one of :a :b\"`, `\"[number number]\"`, `\"x or nil\"` ...)."
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

;; rows

(defn- entries-map
  "The `:map` schema holding the entries of `schema` (under `:maybe` and the first child of `:and`, as
  [[cljplot2.schema/with-rules]] builds), or `nil`."
  [schema]
  (let [s (malli/schema schema)]
    (case (malli/type s)
      :map s
      (:maybe :and) (entries-map (first (malli/children s)))
      nil)))

(defn- nested-map
  "The closed `:map` of an entry's schema `s` (also under `:maybe`), whose entries get rows of their own - flattened
  under the entry's row - or `nil`. Every such entry is a [[container?]]; not the reverse: a map under validation
  rules (`:and`, e.g. a scale spec) or a map of entries isn't flattened (it has a table of its own)."
  [s]
  (let [s (malli/schema s)]
    (case (malli/type s)
      :map (when (:closed (malli/properties s)) s)
      :maybe (nested-map (first (malli/children s)))
      nil)))

(defn- container?
  "Does entry schema `s` hold entries of its own - a map or a map of entries (also under `:maybe` / `:and`)? Its row
  takes no default value (see [[with-defaults]]); its entries' rows do, when [[nested-map]] flattens them."
  [s]
  (let [s (malli/schema s)]
    (case (malli/type s)
      (:map :map-of) true
      (:maybe :and) (container? (first (malli/children s)))
      false)))

(defn- flattened?
  "Are the entries of entry (`props`, schema `s`) flattened under its row: a [[nested-map]] whose entries aren't
  documented by a shared group (`:cljplot2/entries-fragment`)?"
  [props s]
  (and (some? (nested-map s)) (not (:cljplot2/entries-fragment props))))

(defn- entry-row
  "The row of entry `k` (`props`, schema `s`) at `path`; `merged?` - the entry belongs to a merged map (theme, render
  options: every key is there after merging, users give partial overrides - so not required). Metadata `::container?`
  marks an entry holding entries of its own (see [[with-defaults]]) - not one whose entries a shared group documents
  (`:cljplot2/entries-fragment`): its row keeps its own value as the default (e.g. a text role's style)."
  [path k props s merged?]
  (with-meta {:key k :path path :type (if (flattened? props s) "map" (type-text s))
              :required? (and (not merged?) (not (:optional props)))
              :default (:default props) :default-doc (:cljplot2/default-doc props) :description (:description props)
              :fragment (:cljplot2/fragment props)}
    {::container? (and (container? s) (not (:cljplot2/entries-fragment props)))}))

(defn- map-rows
  "Rows of the entries of map schema `m` under `path` (nested closed maps flattened after their own row); forbidden and
  `:cljplot2/internal` entries have none; `merged?` - see [[entry-row]] (a `:cljplot2/merged` map makes its nested
  maps merged too)."
  [m path merged?]
  (let [merged? (or merged? (true? (:cljplot2/merged (malli/properties m))))]
    (into [] (mapcat (fn [[k props s]]
                       (when-not (or (schema/forbidden? s) (:cljplot2/internal props))
                         (let [p (conj path k)]
                           (cons (entry-row p k props s merged?)
                                 (when (flattened? props s) (map-rows (nested-map s) p merged?)))))))
          (malli/children m))))

(defn schema-rows
  "Rows of the entries of map `schema` (see the namespace doc); empty for a schema without entries."
  [schema]
  (if-let [m (some-> schema entries-map)] (map-rows m [] false) []))

(defn- chart-value? [x] (and (map? x) (contains? x :compute)))

(defn- chart-rows
  "Option rows of `chart` (a chart value): style entries take their default from theme `[:chart kind]`."
  [{:keys [kind] :as chart}]
  (let [style-keys (set (map :key (schema-rows (get-in chart [:schema :style]))))
        style (get-in theme/default-theme [:chart kind])]
    (mapv (fn [{:keys [key path] :as row}]
            (cond-> row
              (and (= 1 (count path)) (contains? style-keys key) (contains? style key)) (assoc :default (get style key))))
          (schema-rows (get-in chart [:schema :options])))))

(defn- with-defaults
  "`rows` with `:default` from the nested map `defaults` at each row's `:path` where the row has none - except a
  container row (a map or a map of entries, see [[entry-row]]: its entries' rows carry the values)."
  [rows defaults]
  (mapv (fn [{:keys [path] :as row}]
          (let [v (get-in defaults path)]
            (cond-> row (and (nil? (:default row)) (some? v) (not (::container? (meta row)))) (assoc :default v))))
        rows))

(defn- text-roles-schema
  "The theme's `:text` as a merged map of the roles of [[cljplot2.theme/text-roles]] (sorted), each a text style
  documented by the shared group `:text-style` - rows for the roles the theme's `map-of` can't give."
  []
  (into [:map {:closed true :cljplot2/merged true}]
        (map (fn [role] [role {:description (theme/text-roles role) :cljplot2/entries-fragment :text-style}
                         theme/text-style-schema]))
        (sort (keys theme/text-roles))))

(defn theme-rows
  "Rows of the theme ([[cljplot2.theme/theme-schema]]), then one row `[:text role]` per text role of
  [[cljplot2.theme/text-roles]] (sorted; its default the role's style), with their defaults from
  [[cljplot2.theme/default-theme]]. The style keys are one shared table: `(fragment-rows :text-style)`."
  []
  (with-defaults (into (schema-rows theme/theme-schema) (map-rows (malli/schema (text-roles-schema)) [:text] false))
                 theme/default-theme))

(defn fragment-rows
  "Rows of the entries of shared group `fragment` (a key of [[fragments]]); empty for an unknown group."
  [fragment]
  (schema-rows (into [:map] (get-in fragments [fragment :entries]))))

(defn options
  "Option rows (see the namespace doc) of `x`: a chart value, a var with `:cljplot2/options` metadata (a builder or
  API function) or a map schema."
  [x]
  (cond
    (chart-value? x) (chart-rows x)
    (var? x) (schema-rows (:cljplot2/options (meta x)))
    :else (schema-rows x)))

(defn mapping
  "Rows of the mapping keys chart value `chart` reads (its `:mapping` schema's entries)."
  [chart]
  (schema-rows (get-in chart [:schema :mapping])))

(defn api-vars
  "The user-facing functions - vars with `:cljplot2/api` metadata in the loaded `cljplot2.*` namespaces - sorted by
  namespace and name."
  []
  (->> (all-ns)
       (filter #(str/starts-with? (name (ns-name %)) "cljplot2."))
       (mapcat (comp vals ns-publics))
       (filter #(:cljplot2/api (meta %)))
       (sort-by (fn [v] [(str (:ns (meta v))) (str (:name (meta v)))]))
       vec))
