(ns cljplot2.vocabulary-test
  "Option vocabulary (CLAUDE.md, Glossary): one name per concept, one meaning per name, over every option row of
  `cljplot2.describe` - charts, builders, user-facing data functions and the theme."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cljplot2.core]
            [cljplot2.describe :as describe]))

(defn- option-rows
  "Gives `[owner row]` of every chart, builder and API function option row and every theme row."
  []
  (concat (for [[kind constructor] (describe/chart-constructors)
                row (describe/options (constructor))]
            [(str "chart/" (name kind)) row])
          (for [v (concat (describe/api-vars)
                          (for [ns-sym '[cljplot2.graphs cljplot2.series-plots cljplot2.multiplot]
                                v (vals (ns-publics ns-sym))
                                :when (:cljplot2/options (meta v))]
                            v))
                row (describe/options v)]
            [(str (symbol v)) row])
          (for [row (describe/options (:theme describe/plot-level-schemas))]
            ["theme" row])))

(def ^:private banned-keys
  "Synonyms replaced by the glossary name: banned key -> its glossary name."
  {:band-width :width :body-width :width :wick-width :line-width :points :n :count :n :pad :gap
   :sort :order :order-direction :descending :node-sort :order :format :value-format :key :as})

(def ^:private layout-boxes
  "Theme sections whose boxes keep `:border-color` / `:border-width` (marks use `:stroke-*`)."
  #{:panel :legend :strip})

(deftest banned-names-test
  (doseq [[owner {k :key path :path}] (option-rows)]
    (is (not (contains? banned-keys k)) (str owner " " path ": use " (banned-keys k)))
    (is (not (str/ends-with? (name k) "?")) (str owner " " path ": no ? in option names"))
    (when (#{:border-color :border-width} k)
      (is (and (= "theme" owner) (layout-boxes (first path))) (str owner " " path ": marks use :stroke-*")))))

(def ^:private meanings
  "Meaning of a glossary name, checked on top-level option rows: key -> [test of the row's description, what it should say]."
  {:width [#(str/includes? % "fraction") "a band fraction"]
   :line-width [#(str/includes? % "pixels") "a line width in pixels"]
   :stroke-width [#(str/includes? % "pixels") "an outline width in pixels"]
   :alpha [#(re-find #"opacity|alpha" %) "an opacity"]
   :gap [#(re-find #"fraction|pixels" %) "a gap (fraction or pixels)"]})

(deftest one-meaning-per-name-test
  (doseq [[owner {k :key path :path description :description}] (option-rows)
          :let [[meaning? text] (meanings k)]
          :when (and meaning? (= 1 (count path)) (not= "theme" owner))]
    (is (meaning? (str description)) (str owner " " k " should be " text ": " description))))

(deftest fill-is-a-colour-test
  (testing ":fill is a colour (or :color / :none), never a flag or an accessor"
    (doseq [[owner {k :key path :path type-text :type}] (option-rows)
            :when (and (= :fill k) (= 1 (count path)))]
      (is (str/includes? (str type-text) "color") (str owner " :fill is " type-text)))))

(defn- type-family
  "Gives the coarse family of an option row's type text: `:boolean`, `:flag-or-more`, `:accessor`, `:number`, `:enum`, `:function`, `:map`, `:sequential`, `:string`, `:color` or the type's first word."
  [type-text]
  (let [t (str type-text)]
    (cond
      (re-find #"color" t) :color
      (re-find #"^true or false( or nil)?$" t) :boolean
      (re-find #"true or false" t) :flag-or-more
      (re-find #"accessor|a keyword, a function" t) :accessor
      (re-find #"^(a |an )?(non-negative |positive |finite )?(number|integer)|number in|^integer|fraction" t) :number
      (re-find #"^one of" t) :enum
      (re-find #"function" t) :function
      (re-find #"map" t) :map
      (re-find #"sequential|vector|\[" t) :sequential
      (re-find #"string" t) :string
      :else (keyword (first (str/split t #" "))))))

(def ^:private one-concept-several-shapes
  "Names whose rows differ in type family across owners while meaning one concept: name -> why."
  {:align "alignment: sankey's node alignment (an enum), arrange's panel alignment (a flag)"
   :aspect "proportions: bin2d's height / width ratio, 3D charts' box proportions [ax ay az]"
   :bandwidth "a kernel bandwidth: one number or a selector, or one per axis"
   :bins "binning: a count, a method or per-axis counts"
   :binwidth "a bin width: one number or one per axis"
   :cut "bandwidths past the data: one number or one per axis"
   :domain "a value range: [lo hi] of the one axis, or per axis {:x .. :y ..}"
   :gap "a gap: a fraction or pixels (Glossary)"
   :group-labels "label toggles: a flag, or a flag or a function giving the text"
   :labels "label toggles: a flag, or a flag or an accessor / function giving the text"
   :varwidth "proportional widths: a flag, or a flag or a measure"
   :jitter "jitter: stripplot's fraction of the slot, scatter's [dx dy] in data units"
   :label "a label text: a statistic's name, or how intervals are labelled"
   :mapping "a chart's mapping, or a builder's node mapping"
   :method "a method: a keyword, or a keyword or a function"
   :order "an order: an enum or a sequence of ids (Glossary)"
   :scales "plot scales a builder merges"
   :variables "the variables: accessors of wide rows' columns"
   :variable-names "display names of the variables: positional (alluvial) or keyed by variable (pairs)"
   :x "a literal position or the key of a series column"
   :y "a literal position, a computed y or the key of a series column"})

(deftest one-type-family-per-name-test
  (testing "a top-level option name keeps one type family across charts, builders and API functions (or is a documented variant)"
    (doseq [[k rows] (group-by (comp :key second) (remove #(= "theme" (first %)) (option-rows)))
            :let [top (filter #(= 1 (count (:path (second %)))) rows)
                  families (group-by (comp type-family :type second) top)]
            :when (and (seq top) (not (contains? one-concept-several-shapes k)))]
      (is (= 1 (count families))
          (str k " has several type families: " (update-vals families #(mapv first %)))))))
