(ns cljplot2.palette-override-test
  "Examples set a categorical palette other than the default only on purpose: every theme `:palette {:color ..}` and
  every `(plot/palette ..)` or colour `:range` naming a qualitative palette in the book's notebooks and the demos is listed
  in [[deliberate]] with its reason (continuous gradients - RdBu, viridis ... - aren't categorical palettes)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private deliberate
  "`[file palette]` -> why the example sets that palette."
  {["notebooks/reference/theme.clj" ":tableau-10"] "shows the theme's :palette option"
   ["notebooks/use_cases/colors.clj" ":MetBrewer/Egypt"] "palette demonstration (colours use case)"
   ["notebooks/use_cases/colors.clj" ":RColorBrewer/Set1"] "palette demonstration (colours use case)"
   ["notebooks/use_cases/colors.clj" ":RColorBrewer/Set2"] "palette demonstration (colours use case)"
   ["notebooks/use_cases/part_of_a_whole.clj" ":set2"] "the plot's title names the palette"
   ["notebooks/use_cases/part_of_a_whole.clj" ":MetBrewer/Archambault"] "palette demonstration"
   ["notebooks/use_cases/part_of_a_whole.clj" ":dark2"] "palette demonstration (the title names it)"
   ["notebooks/reference/builders_trees.clj" ":set1"] "heatmap annotations in colours apart from the main palette"
   ["notebooks/reference/builders_trees.clj" ":set2"] "heatmap annotations in colours apart from the main palette"
   ["notebooks/use_cases/correlation.clj" ":set2"] "heatmap annotations in colours apart from the main palette"
   ["notebooks/use_cases/flow.clj" ":category10"] "shows that a palette is a plot scale"
   ["notebooks/galleries/vega.clj" ":category10"] "the Vega original's colours"
   ["notebooks/galleries/vega.clj" ":category20b"] "the Vega original's colours"})

(def ^:private qualitative
  "Names of qualitative (categorical) palettes a `:range` may give."
  #"(?i):(category\d+[bc]?|set[123]|dark2|paired|pastel[12]|accent|tableau[\w-]*|okabe\w*|MetBrewer/[\w-]+|RColorBrewer/(set[123]|dark2|paired|pastel[12]|accent))\b")

(defn- overrides
  "`[file palette]` of every categorical palette override in the `.clj` files under `dirs`."
  [dirs]
  (for [dir dirs
        ^java.io.File f (file-seq (io/file dir))
        :when (str/ends-with? (.getName f) ".clj")
        :let [path (str/replace (.getPath f) "\\" "/")
              text (slurp f)]
        palette (concat (map second (re-seq #":palette \{:color (:[\w/.-]+)" text))
                        (keep (fn [[_ p]] (when (re-matches qualitative p) p))
                              (concat (re-seq #"\(plot/palette (:[\w/.-]+)" text) (re-seq #":range (:[\w/.-]+)" text))))]
    [path palette]))

(deftest palette-override-test
  (let [found (set (overrides ["notebooks" "dev"]))]
    (is (= #{} (set (remove deliberate found))) "an override without a reason: remove it or list why it is set")
    (is (= #{} (set (remove found (keys deliberate)))) "a listed override no longer in the sources")))
