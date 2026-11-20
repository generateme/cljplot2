(ns cljplot2.describe
  "Option tables as data, read from the malli schemas and their documenting entry properties (see
  [[cljplot2.schema]]: `:description`, `:default`, `:cljplot2/default-doc`, `:cljplot2/fragment`).

  [[options]] - rows of a chart value's options (style defaults from theme `[:chart kind]`), of a builder or API
  function var (its `:cljplot2/options` schema) or of a map schema; [[mapping]] - rows of the mapping keys a chart
  reads; [[api-vars]] - the user-facing data functions (`:cljplot2/api` var metadata; this namespace loads every
  namespace holding them); [[fragments]] - the shared groups of options entries belong to, [[fragment-rows]] - the
  rows of one group (a chart may take only some of a group's entries - its own rows are what it accepts).

  A row: `{:key k :path [k ...] :type text :required? bool :default v :default-doc text :description text
  :fragment kw}` - `:path` is longer than `[k]` for the entries of nested closed maps (each nested map also has its
  own row); `:default` / `:default-doc` / `:description` / `:fragment` are `nil` when absent. `:default` is the value
  used when the option is absent (a style row's comes from the theme), `:default-doc` the text of a computed default;
  only a style row may have both - the theme value and how the chart applies it (e.g. only to grouped lines). Entries rejecting every
  value ([[cljplot2.schema/forbidden]]) aren't options and have no row."
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
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            ;; namespaces of the user-facing functions (`:cljplot2/api`), so [[api-vars]] finds them all
            [cljplot2.data.contour]
            [cljplot2.data.graph]
            [cljplot2.data.levels]
            [cljplot2.data.series]
            [cljplot2.data.sets]
            [cljplot2.data.shingle]
            [cljplot2.data.tree]
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
  "Readable text of `schema`'s type: a named type's `:cljplot2/type-doc` or error message, built-in types and
  composites spelled out (`\"one of :a :b\"`, `\"[number number]\"`, `\"x or nil\"` ...)."
  [schema]
  (let [s (malli/schema schema)
        t (malli/type s)
        tp (malli/type-properties s)]
    (or (:cljplot2/type-doc tp)
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
  "The closed `:map` of an entry's schema `s` (also under `:maybe`), whose entries get rows of their own, or `nil`."
  [s]
  (let [s (malli/schema s)]
    (case (malli/type s)
      :map (when (:closed (malli/properties s)) s)
      :maybe (nested-map (first (malli/children s)))
      nil)))

(defn- entry-row
  "The row of entry `k` (`props`, schema `s`) at `path`."
  [path k props s]
  {:key k :path path :type (if (nested-map s) "map" (type-text s)) :required? (not (:optional props))
   :default (:default props) :default-doc (:cljplot2/default-doc props) :description (:description props)
   :fragment (:cljplot2/fragment props)})

(defn- map-rows
  "Rows of the entries of map schema `m` under `path` (nested closed maps flattened after their own row)."
  [m path]
  (into [] (mapcat (fn [[k props s]]
                     (when-not (schema/forbidden? s)
                       (let [p (conj path k)]
                         (cons (entry-row p k props s)
                               (when-let [nested (nested-map s)] (map-rows nested p)))))))
        (malli/children m)))

(defn schema-rows
  "Rows of the entries of map `schema` (see the namespace doc); empty for a schema without entries."
  [schema]
  (if-let [m (some-> schema entries-map)] (map-rows m []) []))

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
