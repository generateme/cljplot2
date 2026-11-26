(ns cljplot2.code-review-r1-test
  "Code review round 1 - horizon band colours (lerp at fractions), the explainers cache, render option defaults from
  the schema, theme colours as clojure2d colours, named chart styles (`:theme-style`) and builder text roles."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.ml.clustering :as cl]
            [cljplot2.cache :as cache]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.schema :as schema]
            [cljplot2.series-plots :as sp]
            [cljplot2.test-util :refer [error-data error drawn-style]]
            [cljplot2.theme :as theme]
            [notebook-utils]))

(defn- horizon-colors [colors n] (#'sp/horizon-colors colors n))

(deftest horizon-band-colors-test
  (testing "n default bands per sign: lightest to darkest, the middle ones lerped at fractions"
    (let [[p1 p2 p3 n1 n2 n3] (horizon-colors nil 3)]
      (is (= (c/color 158 202 225) p1))
      (is (= (c/lerp (c/color 158 202 225) (c/color 8 69 148) 0.5) p2) "the middle band at 0.5, not truncated to 0")
      (is (= (c/color 8 69 148) p3))
      (is (= 3 (count (distinct [n1 n2 n3]))))))
  (testing "boundary: one band - the darkest; two - both ends; many - all distinct"
    (is (= [(c/color 8 69 148) (c/color 165 15 21)] (horizon-colors nil 1)))
    (is (= [(c/color 158 202 225) (c/color 8 69 148) (c/color 252 174 145) (c/color 165 15 21)] (horizon-colors nil 2)))
    (is (= 16 (count (distinct (horizon-colors nil 8))))))
  (testing "boundary: user colours as given; fewer than n - an error keyed :colors"
    (is (= [:a :b :c :d] (horizon-colors {:positive [:a :b] :negative [:c :d]} 2)))
    (is (= :colors (:key (error-data #(horizon-colors {:positive [:a]} 2)))))))

(deftest explainers-cache-test
  (let [explainers @#'schema/explainers]
    (testing "an equal schema (another value) reuses the cached explainer"
      (cache/clear! explainers)
      (is (seq (schema/errors [:map [:a int?]] {:a "x"} [])))
      (is (= 1 (cache/entry-count explainers)))
      (is (seq (schema/errors (vector :map (vector :a int?)) {:a "y"} [])))
      (is (= 1 (cache/entry-count explainers))))
    (testing "boundary: 300 distinct schemas never hold more than the cap; a schema in use survives"
      (cache/clear! explainers)
      (doseq [i (range 300)]
        (schema/errors [:enum i] i [])
        (schema/errors [:enum :kept] :kept []))
      (is (= 256 (cache/entry-count explainers)))
      (let [built (atom 0)]
        (with-redefs [malli.core/explainer (fn [s] (swap! built inc) (constantly nil))]
          (schema/errors [:enum :kept] :kept [])
          (is (zero? @built) "least recently used evicted, not the schema in use"))))
    (testing "boundary: a valid value - no errors, still cached"
      (cache/clear! explainers)
      (is (empty? (schema/errors int? 1 [])))
      (is (= 1 (cache/entry-count explainers))))))

(deftest default-options-test
  (testing "render option defaults are the schema's image defaults"
    (is (= {:width 600 :height 600 :hint :high} pipeline/default-options))
    (is (= pipeline/default-options (:options (plot/build {:charts []})))))
  (testing "boundary: inspection and colour-vision options have no default entry (false / absent behave alike)"
    (is (not-any? #(contains? pipeline/default-options %) [:debug :cvd :until]))))

(deftest readable-color-test
  (let [readable #'notebook-utils/readable-color]
    (testing "an opaque colour prints as [r g b], a translucent one as [r g b a]"
      (is (= [69 117 180] (readable (c/color 69 117 180))))
      (is (= [69 117 180 180] (readable (c/color 69 117 180 180)))))
    (testing "boundary: alpha 0 and 254 keep the alpha; fractional channels round"
      (is (= [0 0 0 0] (readable (c/gray 0 0))))
      (is (= [1 2 3 254] (readable (c/color 1 2 3 254))))
      (is (= [2 2 3] (readable (c/color 1.5 2.4 2.6)))))))

(deftest theme-colours-test
  (testing "colour literals of the default theme are clojure2d colours; named colours stay keywords"
    (is (= (c/color 236 236 243) (get-in theme/default-theme [:plot :background])))
    (is (= (c/set-alpha theme/default-color 180) (get-in theme/default-theme [:chart :scatter :color])))
    (is (= :black (get-in theme/default-theme [:axis :x :line-color])))))

;; named chart styles

(def ^:private xy {:data [{:x 1 :y 2} {:x 2 :y 3}] :mapping {:x :x :y :y}})

(deftest chart-style-test
  (testing "the kind's style, a named style merged over it key by key"
    (is (= (get-in theme/default-theme [:chart :scatter]) (theme/chart-style theme/default-theme :scatter {})))
    (is (= (merge (get-in theme/default-theme [:chart :segment]) (get-in theme/default-theme [:chart :network-edge]))
           (theme/chart-style theme/default-theme :segment {:theme-style :network-edge})))
    (is (= 1 (:line-width (theme/chart-style theme/default-theme :segment {:theme-style :network-edge}))) "the kind's width kept"))
  (testing "boundary: an unknown name - the kind's style; no kind style - the named one alone; nil theme"
    (is (= (get-in theme/default-theme [:chart :line]) (theme/chart-style theme/default-theme :line {:theme-style :nope})))
    (is (= {:size 3} (theme/chart-style {:chart {:n {:size 3}}} :mine {:theme-style :n})))
    (is (nil? (theme/chart-style nil :scatter {:theme-style :network-node})))))

(deftest named-styles-test
  (testing "every registered named style is an entry of the default theme, over a built-in kind"
    (doseq [[n kind] theme/named-styles]
      (is (map? (get-in theme/default-theme [:chart n])) n)
      (is (contains? chart/builtin-styles kind) n)))
  (testing "every registered named style is drawn by a builder"
    (let [edges [{:from "a" :to "b" :value 1} {:from "b" :to "c" :value 2} {:from "a" :to "c" :value 3}]
          levels (tree/from-levels [{:g "x" :l "p"} {:g "x" :l "q"} {:g "y" :l "r"}] {:hierarchy [:g :l]})
          plots [(plot/network edges {:labels true}) (plot/arc edges {}) (plot/sankey edges {})
                 (plot/alluvial [{:a "x" :b "u" :n 1} {:a "y" :b "v" :n 2}] {:variables [:a :b] :value :n})
                 (plot/dendrogram (cl/hclust [[0.0] [1.0] [5.0]] {:method :complete}) {}) (plot/tree levels {})
                 (plot/bundle [{:from "p" :to "r"}] levels {}) (plot/sequences [{:sequence "a-b-end" :n 1}] {:value :n})
                 (plot/acf (map #(Math/sin (* 0.3 (double %))) (range 50)))]]
      (is (= (set (keys theme/named-styles)) (set (keep #(get-in % [:options :theme-style]) (mapcat :charts plots)))))))
  (testing "the clustered heatmap's trees (arranged cells) draw the dendrogram branch style"
    (let [cells (get-in (plot/clustered-heatmap [{:r "a" :c "x" :v 1} {:r "a" :c "y" :v 2} {:r "b" :c "x" :v 3} {:r "b" :c "y" :v 0}
                                                 {:r "c" :c "x" :v 5} {:r "c" :c "y" :v 1}]
                                                {:row :r :col :c :value :v})
                        [:arrange :plots])]
      (is (some #(= :dendrogram-branch (get-in % [:options :theme-style])) (mapcat :charts cells))))))

(deftest theme-style-option-test
  (testing "a chart draws its named style; options win over it; a theme restyles it"
    (let [c (chart/scatter {:theme-style :network-node})]
      (is (= 12 (:size (drawn-style c))))
      (is (= 4 (:size (drawn-style (chart/scatter {:theme-style :network-node :size 4})))))
      (is (= 30 (:size (drawn-style c {:chart {:network-node {:size 30}}}))))
      (is (map? (plot/build (assoc xy :charts [c]))))))
  (testing "a style a theme adds, used by name"
    (is (map? (plot/build (assoc xy :charts [(chart/scatter {:theme-style :mine})] :theme {:chart {:mine {:size 20}}})))))
  (testing "boundary: an unknown name, a bad value, keys of another kind - errors at the option or the theme entry"
    (is (= [:charts 0 :options :theme-style] (:in (error #(plot/build (assoc xy :charts [(chart/scatter {:theme-style :nope})]))))))
    (is (= [:theme :chart :network-node :size]
           (:in (error #(plot/build (assoc xy :charts [(chart/scatter)] :theme {:chart {:network-node {:size -1}}}))))))
    (is (= [:theme :chart :network-node :stroke-color]
           (:in (error #(plot/build (assoc xy :charts [(chart/line {:theme-style :network-node})])))))
        "a scatter style on a line chart: its keys aren't line style keys")
    (is (= [:theme :chart :mine :width]
           (:in (error #(plot/build (assoc xy :charts [(chart/scatter {:theme-style :mine})] :theme {:chart {:mine {:width 2}}})))))))
  (testing "boundary: a chart kind isn't a named style - an error at the option, also for a compatible kind"
    (is (= [:charts 0 :options :theme-style] (:in (error #(plot/build (assoc xy :charts [(chart/scatter {:theme-style :cloud})]))))))
    (is (= [:charts 0 :options :theme-style] (:in (error #(plot/build (assoc xy :charts [(chart/scatter {:theme-style :line})])))))))
  (testing "boundary: a kind style that isn't a map - only the kind's error; a named entry that isn't a map - its error"
    (is (= #{[:theme :chart :scatter]} (cljplot2.test-util/error-ins #(plot/build (assoc xy :charts [(chart/scatter)] :theme {:chart {:scatter 5}})))))
    (is (= #{[:theme :chart :network-node]}
           (cljplot2.test-util/error-ins #(plot/build (assoc xy :charts [(chart/scatter)] :theme {:chart {:network-node 5}}))))))
  (testing "boundary: kinds without a theme style take no :theme-style"
    (is (= :theme-style (:key (error #(plot/build (assoc xy :charts [(chart/text {:theme-style :network-node :text "a" :x 1 :y 1})]))))))
    (is (not-any? #(= :theme-style (:key %)) (describe/options (chart/image))) "image: no such option")
    (is (every? (fn [kind] (some #(= :theme-style (:key %)) (describe/options (@((describe/chart-constructors) kind)))))
                (remove #{:image} (keys chart/builtin-styles)))
        "every other kind with a theme style takes it")))

(deftest chart-theme-errors-test
  (let [errors #'pipeline/chart-theme-errors
        last-errors @#'pipeline/last-chart-theme-errors
        chart-theme (:chart theme/default-theme)]
    (testing "the default chart styles hold; a bad named style - its error, without any plot"
      (is (= [] (errors chart-theme)))
      (is (= [[:theme :chart :tree-node :size]] (map :in (errors (assoc-in chart-theme [:tree-node :size] -1))))))
    (testing "an equal chart theme reuses the last result; another one is checked anew"
      (let [bad (assoc-in chart-theme [:tree-node :size] -1)]
        (errors bad)
        (is (identical? (errors bad) (errors (into {} bad))) "equal value, same result")
        (is (= [] (errors chart-theme)))
        (is (= chart-theme (first @last-errors)))))
    (testing "boundary: a registered named style used on a chart of its kind isn't checked twice"
      (is (= 1 (count (filter #(= [:theme :chart :network-node :size] (:in %))
                              (:errors (error-data #(plot/build (assoc xy :charts [(chart/scatter {:theme-style :network-node})]
                                                                       :theme {:chart {:network-node {:size -1}}})))))))))))

(deftest text-role-test
  (testing "a text chart draws in its :role; default :annotation"
    (is (map? (plot/build (assoc xy :charts [(chart/text {:role :network-label :text "a" :x 1 :y 1})]))))
    (is (map? (plot/build (assoc xy :charts [(chart/text {:role :mine :text "a" :x 1 :y 1})]
                                 :theme {:text {:mine {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :inset 1}}})))))
  (testing "boundary: a role the theme hasn't - an error at the option"
    (is (= [:charts 0 :options :role] (:in (error #(plot/build (assoc xy :charts [(chart/text {:role :nope :text "a" :x 1 :y 1})]))))))))

(deftest style-rows-test
  (testing "a named style's rows: its kind's style keys, defaults merged"
    (let [rows (describe/style-rows :network-edge)]
      (is (= [:color :line-width :dash] (map :key rows)))
      (is (= [(c/gray 120 200) 1 :none] (map :default rows)))))
  (testing "boundary: a line style (sectioned in the line chart's own rows) - one group of rows, no sections"
    (is (every? nil? (map :section (describe/style-rows :tree-link))))
    (is (<= (count (describe/style-rows :tree-link)) 12)))
  (testing "only the given style keys, in the kind's order"
    (is (= [:color :line-width :dash] (map :key (describe/style-rows :tree-link [:dash :color :line-width]))))
    (is (= [:line-width] (map :key (describe/style-rows :tree-link [:line-width]))))
    (is (= [[:bridge] [:bridge :alpha-factor] [:bridge :line-width] [:bridge :dash]] (map :path (describe/style-rows :tree-link [:bridge])))
        "a map key with its nested rows"))
  (testing "boundary: no keys, a key the kind hasn't - an error keyed :style-keys; nil - every key"
    (is (= :style-keys (:key (error-data #(describe/style-rows :tree-link [])))))
    (is (= :style-keys (:key (error-data #(describe/style-rows :tree-link [:color :size])))))
    (is (= (describe/style-rows :tree-link) (describe/style-rows :tree-link nil))))
  (testing "boundary: an unregistered name - an error keyed :style-name"
    (is (= :style-name (:key (error-data #(describe/style-rows :nope)))))
    (is (= :style-name (:key (error-data #(describe/style-rows nil)))))))
