(ns cljplot2.batch-c2-fix-y3-test
  "Batch C2 review fixes (fork y3): clustered-heatmap annotations (`[accessor spec]` pairs, closed specs, strips without
  a plot margin under a user margin, `:annotation-size`, `:legend-span`), legend cells measured at the height they get
  (arrange `:measured` widths of wrapped legends, `:measured` heights), legend cells named as legends in sub-plot
  errors, sankey labels facing each other sharing the gap."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(def ^:private hm
  "Long rows of a 14 x 5 heatmap with row and column annotations."
  (vec (for [r (range 14) c (range 5)]
         {:r (str "r" r) :c (str "c" c) :v (Math/sin (+ r (* 3 c))) :grp (if (< r 7) "a" "b")
          :many (str "level-" r) :cg (str "g" c) :num (double r)})))

(def ^:private base {:row :r :col :c :value :v})

(defn- renders?
  "Tests whether a plot renders at a size."
  [p w h]
  (instance? BufferedImage (plot/render-image p {:width w :height h})))

(defn- built-grid-tracks
  "Gives the `:arrange` of a plot resolved by the grid stage: the size of every `[:row i]` / `[:column j]` track."
  [p w h axis prefix]
  (let [built (plot/build p {:width w :height h :until :grid})]
    (into {} (keep (fn [{:keys [id size]}] (when (and (vector? id) (= prefix (first id))) [(second id) size])))
          (get-in built [:grid axis]))))

(defn- message
  "Gives the message of the exception thrown by calling `f`, `nil` without one."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- thrown
  "Gives the `ExceptionInfo` thrown by calling `f`, `nil` without one."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest annotation-type-test
  (testing "an annotation is [accessor colour-scale-spec]: the spec may be nil, the accessor a function, a list of annotations is a sequence, no annotations nil"
    (doseq [annotations [[[:grp nil] [:num {:range :viridis/magma}]] (list [:grp nil]) [[(fn [r] (:grp r)) nil]] nil]]
      (is (nil? (thrown #(plot/clustered-heatmap hm (assoc base :row-annotations annotations)))) (pr-str annotations))))
  (testing "boundary: one error per bad annotation at its index; a problem inside the pair named with its place, a value that isn't a pair without one"
    (doseq [[option bad needle] [[:row-annotations [:grp] "should be an annotation [accessor color-scale-spec]"]
                                 [:row-annotations :grp "should be an annotation [accessor color-scale-spec]"]
                                 [:row-annotations {} "should be an annotation [accessor color-scale-spec]"]
                                 [:row-annotations nil "should be an annotation [accessor color-scale-spec]"]
                                 [:row-annotations [:grp nil :extra] "should be an annotation [accessor color-scale-spec]"]
                                 [:row-annotations [:grp {:nope 1}] "at [1 :nope]"]
                                 [:row-annotations [42 nil] "at [0]"]
                                 [:row-annotations [:grp :viridis] "at [1]"]
                                 [:col-annotations [:cg {:nope 1}] "at [1 :nope]"]]
            :let [e (thrown #(plot/clustered-heatmap hm (assoc base option [[:grp nil] bad])))]]
      (is (= [option 1] (:in (ex-data e))) (pr-str bad))
      (is (= 1 (count (:errors (ex-data e)))) (pr-str bad))
      (is (str/includes? (str (ex-message e)) needle) (pr-str bad))
      (is (not (str/includes? (str (ex-message e)) ", at []")) (pr-str bad))))
  (testing "boundary: a whole value that isn't a sequence fails at the option"
    (is (= [:row-annotations] (:in (ex-data (thrown #(plot/clustered-heatmap hm (assoc base :row-annotations :grp)))))))))

;; P2: strips keep no plot margin under a user margin

(deftest heatmap-user-margin-test
  (testing "annotations with a user [:plot :margin] build"
    (is (renders? (plot/clustered-heatmap hm (assoc base :row-annotations [[:grp nil]] :theme {:plot {:margin 20}})) 500 400))
    (is (renders? (plot/clustered-heatmap hm (assoc base :col-annotations [[:cg nil]] :theme {:plot {:margin 30}})) 500 400)))
  (testing "a strip's own margin is 0, the user theme otherwise kept"
    (let [p (plot/clustered-heatmap hm (assoc base :row-annotations [[:grp nil]] :theme {:plot {:margin 20} :facet {:gap 3}}))
          strips (filter #(and (get-in % [:labels :x :text]) (= 0 (get-in % [:theme :plot :margin]))) (get-in p [:arrange :plots]))]
      (is (= 1 (count strips)))
      (is (= 3 (get-in (first strips) [:theme :facet :gap]))))))

;; P3 / P4: legends taller than their cell

(deftest heatmap-many-levels-test
  (testing "a 14-level annotation at 500 x 400"
    (is (renders? (plot/clustered-heatmap hm (assoc base :row-annotations [[:many nil]])) 500 400)))
  (testing "two row annotations and a column annotation, large and small image"
    (let [p (plot/clustered-heatmap hm (assoc base :row-annotations [[:grp nil] [:many nil]] :col-annotations [[:cg nil]]))]
      (is (renders? p 900 700))
      (is (renders? p 500 400))))
  (testing "one annotation"
    (is (renders? (plot/clustered-heatmap hm (assoc base :col-annotations [[:cg nil]])) 400 300))))

(defn- m-frac ^double [^double x] (- x (Math/floor x)))

(deftest pairs-legend-row-test
  (testing "pairs :legend-span :row with a colour mapping and 12 variables builds at 1000 x 800"
    (let [vs (mapv #(keyword (str "v" %)) (range 12))
          rows (vec (for [i (range 60)] (into {:g (str "group-" (mod i 3))} (for [v vs] [v (m-frac (* (inc (count (name v))) i 0.618))]))))]
      (is (renders? (plot/pairs rows {:variables vs :mapping {:color :g}}) 1000 800))
      (is (renders? (plot/pairs rows {:variables vs :mapping {:color :g} :legend-span :matrix}) 1000 800)))))

(defn- legend-plot
  "A legend-only plot of `n` colour levels."
  [n]
  {:legend-only true :data (mapv (fn [i] {:a i :b i :g (str "level " i)}) (range n))
   :mapping {:x :a :y :b :color :g} :charts [(chart/scatter)]})

(def ^:private panel-plot {:data [{:a 0 :b 0} {:a 1 :b 1}] :mapping {:x :a :y :b} :charts [(chart/scatter)]})

(deftest measured-width-at-cell-height-test
  (testing "a legend taller than its cell is measured wrapped: the column widens"
    (let [tall (built-grid-tracks (plot/arrange [panel-plot (legend-plot 12)] {:columns 2 :widths [1 :measured]}) 600 600 :columns :column)
          short-cell (built-grid-tracks (plot/arrange [panel-plot (legend-plot 12) panel-plot {:legend-only true}]
                                                      {:columns 2 :widths [1 :measured] :heights [1 3]})
                                        600 600 :columns :column)
          [_ ^long w-tall] (tall 1)
          [_ ^long w-short] (short-cell 1)]
      (is (< w-tall w-short))))
  (testing "zero legends: margins only"
    (let [[_ ^long w] ((built-grid-tracks (plot/arrange [panel-plot {:legend-only true}] {:columns 2 :widths [1 :measured]}) 400 300 :columns :column) 1)]
      (is (= 20 w)))))

(deftest measured-heights-test
  (testing "measured rows get their legends' natural heights, the rest stays below"
    (let [p (plot/arrange [(legend-plot 2) (legend-plot 4)] {:heights [:measured :measured]})
          rows (built-grid-tracks p 400 600 :rows :row)
          [_ ^long h0] (rows 0)
          [_ ^long h1] (rows 1)]
      (is (< h0 h1))
      (is (< (+ h0 h1) 500))
      (is (renders? p 400 600))))
  (testing "measured rows that don't fit are scaled down to the room (and wrap)"
    (let [p (plot/arrange [(legend-plot 20) (legend-plot 20)] {:heights [:measured :measured]})
          rows (built-grid-tracks p 400 300 :rows :row)
          [_ ^long h0] (rows 0)
          [_ ^long h1] (rows 1)]
      (is (<= (+ h0 h1) 300))
      (is (renders? p 400 300))))
  (testing "a flexible legend row keeps its natural height when measured rows overflow"
    (let [p (plot/arrange [(legend-plot 3) (legend-plot 30)] {:heights [1 :measured]})
          rows (built-grid-tracks p 400 300 :rows :row)]
      (is (= :fixed (first (rows 0))))
      (is (renders? p 400 300))))
  (testing "a measured row holding a plot that isn't legend-only is an error"
    (is (= :arrange (:key (error #(plot/build (plot/arrange [panel-plot (legend-plot 2)] {:heights [:measured 1]})))))))
  (testing "one row"
    (is (renders? (plot/arrange [(legend-plot 3)] {:heights [:measured]}) 300 200))))

;; error messages name a legend cell as a legend, not by its mapping

(deftest legend-cell-message-test
  (let [p (plot/arrange [(assoc (legend-plot 3) :labels {:title :none}) panel-plot] {:columns 2 :widths [:measured 1]})
        msg (message #(plot/render-image p {:width 400 :height 25}))]
    (is (some? msg))
    (is (str/includes? msg "(legend)"))
    (is (not (str/includes? msg "(x :a")))))

(deftest annotation-strip-names-no-internal-key-test
  (let [p (plot/clustered-heatmap hm (assoc base :row-annotations [[:grp nil]] :col-annotations [[:cg nil]]))
        sub-plots (tree-seq #(get-in % [:arrange :plots]) #(get-in % [:arrange :plots]) p)]
    (is (not-any? (fn [sp] (some (fn [k] (let [a (get-in sp [:mapping k])] (and (keyword? a) (= "cljplot2.graphs" (namespace a))))) [:x :y]))
                  sub-plots))))

;; D3 / D4 / D10 / D11: annotation option shapes

(deftest annotation-options-test
  (testing "a typo in a spec is an error keyed by the option"
    (is (= :row-annotations (first (:in (error #(plot/clustered-heatmap hm (assoc base :row-annotations [[:grp {:pallete :set1}]])))))))
    (is (= :col-annotations (first (:in (error #(plot/clustered-heatmap hm (assoc base :col-annotations [[:cg {:range 3}]]))))))))
  (testing "the map form is rejected"
    (is (= :row-annotations (:key (error #(plot/clustered-heatmap hm (assoc base :row-annotations {:grp nil})))))))
  (testing "nil and empty: no strips"
    (is (= (plot/clustered-heatmap hm base) (plot/clustered-heatmap hm (assoc base :row-annotations nil))))
    (is (= (plot/clustered-heatmap hm base) (plot/clustered-heatmap hm (assoc base :row-annotations [] :col-annotations [])))))
  (testing "order kept for many annotations"
    (let [ks (mapv #(keyword (str "a" %)) (range 10))
          rows (mapv (fn [r] (into r (map (fn [k] [k (:grp r)])) ks)) hm)
          p (plot/clustered-heatmap rows (assoc base :row-annotations (mapv (fn [k] [k nil]) ks) :row-tree false :col-tree false))
          titles (keep #(get-in % [:labels :x :text]) (get-in p [:arrange :plots]))]
      (is (= (mapv name ks) (vec titles)))))
  (testing "numeric annotation values: a continuous colour (a colour bar)"
    (is (renders? (plot/clustered-heatmap hm (assoc base :row-annotations [[:num nil]])) 500 400)))
  (testing ":annotation-size - pixels, nil the default 16"
    (let [widths (fn [o] (get-in (plot/clustered-heatmap hm (merge base {:row-annotations [[:grp nil]]} o)) [:arrange :widths]))]
      (is (some #{[:fixed 16]} (widths {})))
      (is (some #{[:fixed 16]} (widths {:annotation-size nil})))
      (is (some #{[:fixed 9]} (widths {:annotation-size 9})))
      (is (= :annotation-size (:key (error #(widths {:annotation-size 0}))))))))

;; D12: :legend-span

(deftest heatmap-legend-span-test
  (let [spans (fn [o] (get-in (plot/clustered-heatmap hm (merge base o)) [:arrange :row-spans]))]
    (testing "defaults: :matrix with annotations, :row without"
      (is (some? (spans {:row-annotations [[:grp nil]]})))
      (is (nil? (spans {}))))
    (testing "explicit"
      (is (nil? (spans {:row-annotations [[:grp nil]] :legend-span :row})))
      (is (= {2 2} (spans {:legend-span :matrix})))
      (is (= {2 3} (spans {:col-annotations [[:cg nil]] :legend-span :matrix})))
      (is (nil? (spans {:legend-span :matrix :col-tree false}))))
    (testing "every combination renders"
      (doseq [o [{:legend-span :row :row-annotations [[:grp nil]] :col-annotations [[:cg nil]]}
                 {:legend-span :matrix}
                 {:legend-span :matrix :col-tree false :row-tree false}
                 {:legend-span :row :col-annotations [[:cg nil]] :col-tree false}]]
        (is (renders? (plot/clustered-heatmap hm (merge base o)) 500 400) (pr-str o))))
    (testing "a wrong value"
      (is (= :legend-span (:key (error #(spans {:legend-span :all}))))))))

;; P8: sankey labels facing each other share the gap

(deftest sankey-label-room-test
  (let [room @#'cljplot2.graphs/label-room
        left {:x0 0.0 :x1 0.1 :y0 0.4 :y1 0.6}
        right {:x0 0.9 :x1 1.0 :y0 0.4 :y1 0.6}
        mid-left {:x0 0.3 :x1 0.4 :y0 0.4 :y1 0.6}
        mid-right {:x0 0.6 :x1 0.7 :y0 0.4 :y1 0.6}
        away {:x0 0.9 :x1 1.0 :y0 0.0 :y1 0.1}]
    (testing "facing labels: half the gap each"
      (is (< (Math/abs (- 0.4 (room [left right] left))) 1e-9))
      (is (< (Math/abs (- 0.4 (room [left right] right))) 1e-9)))
    (testing "a node on the same side labels away: the full gap"
      (is (< (Math/abs (- 0.2 (room [left mid-left] left))) 1e-9))
      (is (< (Math/abs (- 0.2 (room [mid-right right] right))) 1e-9)))
    (testing "the nearest node counts"
      (is (< (Math/abs (- 0.2 (room [left mid-left right] left))) 1e-9)))
    (testing "no node in line: up to the frame"
      (is (< (Math/abs (- 0.9 (room [left away] left))) 1e-9))
      (is (< (Math/abs (- 0.9 (room [right] right))) 1e-9))))
  (testing "one long-named link renders"
    (is (renders? (plot/sankey [{:from "A very long source name" :to "Another very long target name" :value 1}]) 200 150))))
