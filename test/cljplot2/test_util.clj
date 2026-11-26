(ns cljplot2.test-util
  "Helpers shared by the tests."
  (:require [malli.core :as malli]
            [cljplot2.core :as plot]
            [cljplot2.schema :as schema]
            [cljplot2.describe :as describe]
            [cljplot2.theme :as theme]))

(defn schema-entries
  "`[path props schema]` of every map entry anywhere in `schema` - nested maps, `:multi` branches, `:and` / `:or` /
  `:maybe` / collection children."
  [schema]
  (letfn [(walk [s path]
            (let [compiled (malli/schema s)]
              (case (malli/type compiled)
                :map (mapcat (fn [[k props v]] (cons [(conj path k) props v] (walk v (conj path k)))) (malli/children compiled))
                :multi (mapcat (fn [[_ _ v]] (walk v path)) (malli/children compiled))
                (:maybe :and :or :vector :sequential :set :map-of :tuple)
                (mapcat #(when (malli/schema? (try (malli/schema %) (catch Exception _ nil))) (walk % path)) (malli/children compiled))
                nil)))]
    (walk schema [])))

(def migrated
  "Chart kinds whose reference sections show option cards (`nu/option-cards`) and follow the card rules of
  `option-cards-test` - the one switch of the readability rewrite; `cljplot2.describe/concepts` names their sections."
  #{:scatter :line :segment :polygon :ribbon :candlestick :smooth :rule :rect :text :image :function :bar :bullet
    :correlation :pairwise :box :violin :extent :stripplot :beeswarm :dotplot :rug :histogram :density :ecdf :qq :pp
    :tile :bin2d :field :contour :parallel :cloud :surface :treemap :partition :pack :waffle :venn})

(def migrated-builders
  "Builders of `cljplot2.core` whose reference sections show option cards and follow the card rules of
  `option-cards-test` - the builders' switch; `cljplot2.describe/concepts` names their sections by their symbols."
  '#{network chord arc sankey alluvial dendrogram tree clustered-heatmap bundle sequences acf horizon arrange pairs})

(def migrated-functions
  "User-facing data functions (`:cljplot2/api`, qualified symbols) whose reference sections show option cards and
  follow the card rules of `option-cards-test` - the data functions' switch; `cljplot2.describe/concepts` names their
  sections by their qualified symbols. Only functions taking options are listed (every one of them)."
  '#{cljplot2.data.field/domain-color cljplot2.data.graph/adjacency->edges cljplot2.data.graph/alluvial-graph
     cljplot2.data.graph/arc-layout cljplot2.data.graph/chord-layout cljplot2.data.graph/graph cljplot2.data.graph/layout
     cljplot2.data.graph/sankey-layout cljplot2.data.hierarchy/sequence-levels cljplot2.data.levels/bin
     cljplot2.data.levels/order-levels cljplot2.data.levels/rank-rows cljplot2.data.pairwise/matrix-rows
     cljplot2.data.series/acf-rows cljplot2.data.series/horizon-rows cljplot2.data.shingle/shingle
     cljplot2.data.tree/bundle-paths cljplot2.data.tree/from-levels cljplot2.data.tree/from-parents
     cljplot2.data.tree/layout cljplot2.data.tree/node-link-layout cljplot2.data.vector-field/arrows
     cljplot2.data.vector-field/traces})

(def migrated-groups
  "Shared groups of options (keys of `cljplot2.describe/fragments`) whose Shared options sections show option cards and
  follow the card rules of `option-cards-test` - the groups' switch; the groups a data function owns are documented
  by its section instead (`:owner`)."
  #{:annotation :arrange :builder :cell :common :corner-radius :curve :field :frame-3d :hierarchy-labels :line-style
    :mirror :pattern :ridge :shading :strip :text-style :theme-style})

(def migrated-theme-keys
  "Top-level theme keys whose Theme chapter sections show option cards and follow the card
  rules of `option-cards-test` - the theme's switch (`nu/option-cards [:theme] ...`); every key but `:chart` (each
  chart's page documents its style)."
  #{:plot :panel :axis :strip :facet :multiplot :legend :frame-3d :palette :text})

(def migrated-schemas
  "Plot-level schemas (names in `cljplot2.describe/plot-level-schemas`) whose rows the concept chapters show as option
  cards (the mapping: mapping lists) and whose texts read in the card style - the schemas' switch: name -> the key
  paths migrated (`[]` - the whole schema; a path covers the entries below it)."
  {:mapping #{[]} :position-scale #{[]} :polar-coord #{[]} :color-scale #{[]} :shape-scale #{[]} :size-scale #{[]}
   :dash-scale #{[]} :pattern-scale #{[]} :plot #{[]} :facets #{[]} :facet-scale #{[]} :margins #{[]} :margin-spec #{[]}
   :labels #{[]} :label #{[]} :render-options #{[]}})

(defn path-under?
  "Tests whether key `path` is `root` or lies below it (`[]` - every path)."
  [root path]
  (= (seq root) (seq (take (count root) path))))

(defn migrated-path?
  "Tests whether the entry at key `path` of plot-level schema `n` is migrated (see [[migrated-schemas]]): it is at or
  below one of the schema's migrated paths."
  [n path]
  (some #(path-under? % path) (get migrated-schemas n)))

(defn migrated-entry-props
  "The properties of the entries at migrated paths of the migrated schemas ([[migrated-schemas]]): the same entry nested
  in another schema (the plot's `:mapping` and `:scales`, a builder's `:mapping`) reads in the card style too, so the
  writing rules recognise it by its properties."
  []
  (set (for [n (keys migrated-schemas)
             [path props s] (schema-entries (describe/plot-level-schemas n))
             :when (and (migrated-path? n path) (:description props) (not (schema/forbidden? s)))]
         props)))

(declare migrated-function-groups)

(defn card-groups
  "The shared groups whose entries read in the card style: the migrated groups ([[migrated-groups]]) and those the
  migrated data functions own ([[migrated-function-groups]])."
  []
  (into migrated-groups (migrated-function-groups)))

(defn migrated-function-groups
  "The shared groups whose options the migrated data functions take ([[migrated-functions]]): the functions own them,
  so their texts read in the card style - also for the builders forwarding them."
  []
  (set (for [sym migrated-functions
             [group] (describe/fragment-keys (describe/options (resolve sym)))]
         group)))

(defn error-data
  "`ex-data` of the `ExceptionInfo` thrown by calling `f`, `nil` when it doesn't throw one."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn error
  "[[error-data]] without `:errors` (the first error: `:key`, `:in`, `:value` and any extra data)."
  [f]
  (some-> (error-data f) (dissoc :errors)))

(defn drawn-style
  "Style keys a chart value draws with under the default theme (or `theme`, merged): its kind's style, its named
  `:theme-style`, then its options over them (style keys only matter; other options come along)."
  ([chart] (drawn-style chart nil))
  ([{:keys [kind options]} theme]
   (merge (theme/chart-style (theme/plot-theme theme) kind options) options)))

(defn error-ins
  "Set of the `:in` paths of every error thrown by calling `f`."
  [f]
  (set (map :in (:errors (error-data f)))))

;; defaults (reference part: every schema `:default` is the real one)

(defn explicit-defaults
  "Options `options` with every constant `:default` of option `rows` set where absent (`nil` - not set); nested rows
  only inside a given map (an absent map - e.g. no `:arrow` - is no map of defaults)."
  [rows options]
  (reduce (fn [opts {:keys [path default]}]
            (if (and (some? default) (nil? (get-in opts path))
                     (or (= 1 (count path)) (map? (get-in opts (pop path)))))
              (assoc-in opts path default)
              opts))
          (into {} (remove (comp nil? val)) options)
          (remove #(nil? (:default %)) rows)))

(defn pixels
  "ARGB pixels of `img`."
  ^ints [^java.awt.image.BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn same-pixels?
  "Do plots `p` and `q` render to the same pixels with render `options`?"
  [p q options]
  (java.util.Arrays/equals (pixels (plot/render-image p options)) (pixels (plot/render-image q options))))

(defn builder-defaults-hold?
  "Does builder `v` (a var with `:cljplot2/options`) applied to `args` and `options` render the same pixels as with
  every constant `:default` of its options set explicitly?"
  [v args options render-options]
  (let [f @v
        explicit (explicit-defaults (describe/options v) options)]
    (same-pixels? (apply f (concat args [options])) (apply f (concat args [explicit])) render-options)))
