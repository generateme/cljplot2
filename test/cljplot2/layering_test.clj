(ns cljplot2.layering-test
  "Namespace layers: every `src/` namespace requires only namespaces of its own layer or below, read from the `ns`
  forms. The layers, bottom up, are written down in CLAUDE.md (Architecture, Layers)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private leaf-namespaces
  "Namespaces requiring nothing above themselves (value types and helpers), usable from every layer."
  #{'cljplot2.error 'cljplot2.cache 'cljplot2.data.accessor 'cljplot2.graphics.font 'cljplot2.graphics.shape})

(def ^:private layer-ranks
  "Rank of each layer, bottom up; a namespace may require ranks up to its own."
  {:leaf 0 :schema 1 :data 2 :scale 2 :theme 3 :graphics 4 :content 5 :layout 6 :chart 7
   :pipeline 8 :builders 9 :api 10})

(def ^:private shared-chart-namespaces
  "Chart namespaces other chart namespaces may require."
  #{'cljplot2.chart.common 'cljplot2.chart.legend 'cljplot2.chart.strip 'cljplot2.chart.statistic-cell})

(defn- layer
  "Gives the layer of a namespace symbol."
  [ns-sym]
  (let [n (str ns-sym)
        [_ part] (str/split n #"\.")]
    (cond
      (leaf-namespaces ns-sym) :leaf
      (= n "cljplot2.schema") :schema
      (= n "cljplot2.theme") :theme
      (= n "cljplot2.chart") :chart
      (#{"cljplot2.pipeline" "cljplot2.render"} n) :pipeline
      (#{"cljplot2.multiplot" "cljplot2.graphs" "cljplot2.series-plots"} n) :builders
      (#{"cljplot2.core" "cljplot2.describe"} n) :api
      :else (keyword part))))

(defn- ns-form
  "Reads the first form of a source file."
  [file]
  (with-open [r (java.io.PushbackReader. (io/reader file))]
    (read r)))

(defn- all-required-namespaces
  "Gives every namespace an `ns` form requires."
  [form]
  (for [clause (rest form)
        :when (and (seq? clause) (= :require (first clause)))
        spec (rest clause)]
    (if (sequential? spec) (first spec) spec)))

(defn- required-namespaces
  "Gives the cljplot2 namespaces an `ns` form requires."
  [form]
  (filter #(str/starts-with? (str %) "cljplot2") (all-required-namespaces form)))

(def ^:private source-forms
  "Map of each `src/` namespace to its `ns` form."
  (into {}
        (for [file (file-seq (io/file "src/cljplot2"))
              :when (str/ends-with? (str file) ".clj")
              :let [form (ns-form file)]]
          [(second form) form])))

(def ^:private source-graph
  "Map of each `src/` namespace to the cljplot2 namespaces it requires."
  (into {}
        (for [file (file-seq (io/file "src/cljplot2"))
              :when (str/ends-with? (str file) ".clj")
              :let [form (ns-form file)]]
          [(second form) (required-namespaces form)])))

(deftest every-namespace-has-a-layer-test
  (doseq [ns-sym (keys source-graph)]
    (is (contains? layer-ranks (layer ns-sym)) (str ns-sym " has no layer"))))

(deftest requires-follow-layers-test
  (testing "no namespace requires a higher layer"
    (doseq [[from targets] source-graph
            target targets]
      (is (<= (layer-ranks (layer target)) (layer-ranks (layer from)))
          (str from " (" (layer from) ") requires " target " (" (layer target) ")"))))
  (testing "chart namespaces require only the shared chart namespaces"
    (doseq [[from targets] source-graph
            :when (str/starts-with? (str from) "cljplot2.chart.")
            target targets
            :when (str/starts-with? (str target) "cljplot2.chart")]
      (is (shared-chart-namespaces target) (str from " requires " target)))))

(deftest data-and-scales-never-draw-test
  (testing "data and scale namespaces require neither clojure2d.core nor a graphics namespace other than the leaves"
    (doseq [[ns-sym form] source-forms
            :when (#{:data :scale} (layer ns-sym))
            required (all-required-namespaces form)]
      (is (not (or (= 'clojure2d.core required)
                   (and (= :graphics (layer required)) (not (leaf-namespaces required)))))
          (str ns-sym " requires " required)))))
