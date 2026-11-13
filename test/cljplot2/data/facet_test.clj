(ns cljplot2.data.facet-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(def ^:private rows
  (vec (for [i (range 12)] {:x i :y (* 2 i) :g (nth [0 2 4 6 8 10] (mod i 6)) :c (["a" "b"] (mod i 2)) :r ([:lo :hi :mid] (mod i 3))})))

(defn- build
  ([p] (build p {}))
  ([p opts] (plot/build p (merge {:width 800 :height 600} opts))))

(defn- facet-plot [extra] (merge {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]} extra))
(defn- items [built] (set (map :id (:items (:grid built)))))
(defn- track-ids [built axis] (mapv :id (get-in built [:grid axis])))

;; 1.x facet dimension

(deftest mapping-forms-test
  (is (= [:facet] (get-in (build (facet-plot {})) [:split :facets :dims])))
  (is (= [:facet-col :facet-row] (get-in (build (facet-plot {:mapping {:x :x :y :y :facet-col :c :facet-row :r}})) [:split :facets :dims])))
  (is (= [:facet-col] (get-in (build (facet-plot {:mapping {:x :x :y :y :facet-col :c}})) [:split :facets :dims])))
  (is (= [:facet-row] (get-in (build (facet-plot {:mapping {:x :x :y :y :facet-row :r}})) [:split :facets :dims])))
  (is (= :mapping (:key (error #(build (facet-plot {:mapping {:x :x :y :y :facet :g :facet-col :c}}))))))
  (testing "charts with different facet mappings"
    (is (= {:key :charts :in [:charts] :value [[:facet] [:facet-col]]}
           (error #(build {:data rows :mapping {:x :x :y :y}
                           :charts [(chart/scatter {:mapping {:facet :g}})
                                    (chart/scatter {:mapping {:facet-col :c}})]}))))))

(deftest levels-test
  (testing "sorted by default"
    (is (= [0 2 10] (facet/levels :facet [10 2 0 2] nil true)))
    (is (= ["a" "b" "c"] (facet/levels :facet ["c" "a" "b"] nil true)))
    (is (= [[1 "a"] [1 "b"] [2 "a"]] (facet/levels :facet [[2 "a"] [1 "b"] [1 "a"]] nil true))))
  (testing "explicit domain orders"
    (is (= [10 0 2] (facet/levels :facet [0 2 10] {:domain [10 0 2]} true)))
    (is (= [10 0 7] (facet/levels :facet [0] {:domain [10 0 7]} true)) "domain values without data kept"))
  (testing "NA level last"
    (is (= [1 2 nil] (facet/levels :facet [2 nil 1] nil true)))
    (is (= [1 2] (facet/levels :facet [2 nil 1] nil false))))
  (testing "incomparable values"
    (is (= {:key :facet :in [:mapping :facet]} (error #(facet/levels :facet [1 "a"] nil true)))))
  (testing "invalid explicit domain"
    (are [d] (= [:scales :facet :domain] (:in (error #(build (facet-plot {:scales {:facet {:domain d}}})))))
      [] [1 1] "ab")))

(deftest panel-order-test
  (let [built (build (facet-plot {}))]
    (is (= [0 2 4 6 8 10] (get-in built [:split :facets :levels :facet])))
    (is (= 0 (get-in (first (filter #(= [:panel 0 0] (:id %)) (:items (:grid built)))) [:content :panel])))
    (is (= 10 (get-in (first (filter #(= [:panel 1 2] (:id %)) (:items (:grid built)))) [:content :panel])))))

(deftest na-panel-test
  (let [data (conj rows {:x 1 :y 1 :g nil})]
    (testing "NA panel last with strip NA"
      (let [built (build (facet-plot {:data data}))]
        (is (= [0 2 4 6 8 10 nil] (get-in built [:split :facets :levels :facet])))
        (is (some #(= "NA" (get-in % [:content :text])) (:items (:grid built))))))
    (testing ":na-panel false drops and counts"
      (let [built (build (facet-plot {:data data :facets {:na-panel false}}))]
        (is (= [0 2 4 6 8 10] (get-in built [:split :facets :levels :facet])))
        (is (= {:facet 1} (get-in built [:split :facets :dropped]))))))
  (testing "values outside an explicit domain dropped and counted"
    (let [built (build (facet-plot {:scales {:facet {:domain [0 2]}}}))]
      (is (= [0 2] (get-in built [:split :facets :levels :facet])))
      (is (= {:facet 8} (get-in built [:split :facets :dropped]))))))

(deftest facet-opt-out-test
  (testing "a chart mapping the plot's facet key to nil opts out: drawn in all panels"
    (let [built (build {:data rows :mapping {:x :x :y :y :facet :g}
                        :charts [(chart/scatter) (chart/scatter {:data [{:x 100 :y 100}] :mapping {:facet nil}})]})]
      (is (= [facet/all-panels] (keys (get-in built [:stats 1 :data])))))))

(deftest all-panels-test
  (let [built (build {:data rows
                      :charts [(chart/scatter {:mapping {:x :x :y :y :facet :g}})
                               (chart/scatter {:data [{:x 100 :y 100}] :mapping {:x :x :y :y}})]})]
    (is (= [facet/all-panels] (keys (get-in built [:stats 1 :data]))))
    (is (= {:kind :numeric :min 0.0 :max 100.0} (get-in built [:extents :x :extent])) "shared extents include it")))

(deftest strip-text-test
  (let [texts (fn [built] (set (keep #(get-in % [:content :text]) (filter #(= :strip (get-in % [:content :kind])) (:items (:grid built))))))]
    (is (= #{"0" "2" "4" "6" "8" "10"} (texts (build (facet-plot {})))))
    (is (= #{"Score: 0" "Score: 2" "Score: 4" "Score: 6" "Score: 8" "Score: 10"}
           (texts (build (facet-plot {:scales {:facet {:formatter #(str "Score: " %)}}})))))
    (is (= #{"[0 \"a\"]" "[1 \"b\"]"}
           (texts (build {:data [{:x 1 :y 1 :k [0 "a"]} {:x 2 :y 2 :k [1 "b"]}] :mapping {:x :x :y :y :facet :k} :charts [(chart/scatter)]}))))))

(deftest no-rows-test
  (let [built (build (facet-plot {:data []}))]
    (is (= [] (get-in built [:split :facets :dims])))
    (is (contains? (items built) :panel) "single panel")
    (is (not-any? #(= :strip (get-in % [:content :kind])) (:items (:grid built))))))

;; 2.x shared, panel-local

(deftest shared-scales-test
  (let [built (build (facet-plot {}))
        dom (get-in built [:scale-specs :x :domain])]
    (is (<= (first dom) 0.0))
    (is (>= (second dom) 11.0) "global domain, not the panel's own")
    (testing "ticks at identical offsets in every panel"
      (let [fx (get-in built [:finalized-scales :x])
            panels (filter #(and (vector? (:id %)) (= :panel (first (:id %)))) (:items (:layout built)))]
        (is (= 6 (count panels)))
        (is (every? #(<= (abs (- (get-in % [:box :w]) (second (:range fx)))) 1) panels) "panels within 1 px of the reference")))))

;; 3.x grid

(deftest wrap-shape-test
  (are [n opts shape] (= shape (facet/wrap-shape n opts))
    1 {} [1 1]
    2 {} [1 2]
    3 {} [1 3]
    4 {} [2 2]
    6 {} [2 3]
    7 {} [2 4]
    12 {} [3 4]
    6 {:columns 1} [6 1]
    6 {:rows 1} [1 6]
    6 {:columns 4} [2 4]
    6 {:columns 3 :rows 2} [2 3])
  (is (= {:key :facets :in [:facets] :panels 6 :rows 2 :columns 2} (error #(facet/wrap-shape 6 {:columns 2 :rows 2}))))
  (are [opts in] (= in (:in (error #(build (facet-plot {:facets opts})))))
    {:columns 0} [:facets :columns]
    {:rows -1} [:facets :rows]
    {:columns 1.5} [:facets :columns]
    {:na-panel 1} [:facets :na-panel]
    {:colums 2} [:facets :colums]))

(deftest wrap-order-test
  (is (= [[0 0] [0 1] [0 2] [1 0]] (mapv #(facet/wrap-positions % 2 3 :top-down) [0 1 2 3])))
  (is (= [[1 0] [1 1] [1 2] [0 0]] (mapv #(facet/wrap-positions % 2 3 :bottom-up) [0 1 2 3])))
  (let [built (build (facet-plot {:facets {:order :bottom-up}}))]
    (is (= 0 (get-in (first (filter #(= [:panel 1 0] (:id %)) (:items (:grid built)))) [:content :panel])) "first value bottom-left"))
  (is (= [:facets :order] (:in (error #(build (facet-plot {:facets {:order :sideways}})))))))

(deftest tracks-test
  (let [wrap (build (facet-plot {}))]
    (is (= [:margin-left :y-title :y-axis [:panel 0] [:gap 0] [:panel 1] [:gap 1] [:panel 2] :x-overhang :margin-right]
           (track-ids wrap :columns)))
    (is (= [:margin-top :title :y-overhang [:strip 0] [:panel 0] [:gap 0] [:strip 1] [:panel 1] :x-axis :x-title :margin-bottom]
           (track-ids wrap :rows))))
  (let [g (build (facet-plot {:mapping {:x :x :y :y :facet-col :c :facet-row :r}}))]
    (is (= [:margin-left :y-title :y-axis [:panel 0] [:gap 0] [:panel 1] :row-strip :x-overhang :margin-right]
           (track-ids g :columns)))
    (is (= [:margin-top :title :y-overhang :col-strip [:panel 0] [:gap 0] [:panel 1] [:gap 1] [:panel 2] :x-axis :x-title :margin-bottom]
           (track-ids g :rows)))))

(deftest axes-test
  (let [built (build (facet-plot {}))
        ids (items built)]
    (is (= #{[:x-axis 0] [:x-axis 1] [:x-axis 2]} (set (filter #(and (vector? %) (= :x-axis (first %))) ids))))
    (is (= #{[:y-axis 0] [:y-axis 1]} (set (filter #(and (vector? %) (= :y-axis (first %))) ids)))))
  (testing "x axis under a column whose bottom cell is empty"
    (let [built (build (facet-plot {:data (filter #(not= 10 (:g %)) rows)}))]
      (is (contains? (items built) [:x-axis 2]))
      (is (not (contains? (items built) [:panel 1 2]))))))

(deftest unfaceted-grid-test
  (is (= [:margin-left :y-title :y-axis :panel :x-overhang :margin-right]
         (track-ids (build {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]}) :columns))))

;; 4.x strips, 5.x shade - drawing

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none} :alternate-fill :none}})

(defn- pixel [canvas x y]
  (let [rgb (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right rgb 16) 0xff) (bit-and (bit-shift-right rgb 8) 0xff) (bit-and rgb 0xff)]))

(defn- box-of [built id] (grid/item-box (:layout built) id))

(defn- ink? [c {:keys [x y w h]}]
  (some (fn [[px py]] (not= [255 255 255] (pixel c px py))) (for [px (range x (+ x w)) py (range y (+ y h))] [px py])))

(deftest strip-drawing-test
  (let [p (facet-plot {:theme plain})
        built (build p)
        c (plot/render p {:width 800 :height 600})
        strip (box-of built [:strip 0 0])
        panel (box-of built [:panel 0 0])]
    (is (ink? c strip) "strip text drawn")
    (is (< (+ (:y strip) (:h strip)) (inc (:y panel))) "strip above its panel"))
  (testing "default strip is a box: translucent white fill, gray border"
    (let [p (facet-plot {:theme (assoc plain :plot {:background :black})})
          built (build p)
          c (plot/render p {:width 800 :height 600})
          {:keys [x y w h]} (box-of built [:strip 0 0])
          [r g b] (pixel c (+ x 3) (+ y (quot h 2)))]
      (is (< 100 r 200) "white alpha 150 over black")
      (is (= r g b))))
  (testing "strip fill"
    (let [p (facet-plot {:theme (assoc plain :strip {:fill :red})})
          built (build p)
          {:keys [x y]} (box-of built [:strip 0 0])]
      (is (= [255 0 0] (pixel (plot/render p {:width 800 :height 600}) (inc x) (inc y))))))
  (testing "grid row strips on the right, rotated"
    (let [built (build (facet-plot {:mapping {:x :x :y :y :facet-col :c :facet-row :r}}))
          rs (box-of built [:row-strip 0])
          panel (box-of built [:panel 0 1])]
      (is (>= (:x rs) (+ (:x panel) (:w panel))))
      (is (> (:h rs) (:w rs)) "rotated strip is taller than wide"))))

(deftest alternate-shade-test
  (let [p (facet-plot {:theme (assoc-in plain [:panel :alternate-fill] [0 0 0 20])})
        built (build p)
        c (plot/render p {:width 800 :height 600})
        center (fn [id] (let [{:keys [x y w h]} (box-of built id)] (pixel c (+ x 3) (+ y 3))))]
    (is (= [255 255 255] (center [:panel 0 0])))
    (is (not= [255 255 255] (center [:panel 0 1])))
    (is (= (center [:panel 0 0]) (center [:panel 1 1])))
    (testing ":none disables"
      (let [c2 (plot/render (facet-plot {:theme plain}) {:width 800 :height 600})
            {:keys [x y]} (box-of built [:panel 0 1])]
        (is (= [255 255 255] (pixel c2 (+ x 3) (+ y 3))))))
    (testing "single panel isn't shaded"
      (is (not (get-in (first (:items (:grid (build {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]})))) [:content :alternate]))))))

(deftest facet-points-test
  (let [p {:data [{:x 1 :y 1 :g "a"} {:x 9 :y 9 :g "b"}] :mapping {:x :x :y :y :facet :g} :theme plain
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}
           :charts [(chart/scatter {:size 9 :color :black})]}
        built (build p)
        c (plot/render p {:width 800 :height 600})
        fx (scale/forward-fn (get-in built [:finalized-scales :x]))
        fy (scale/forward-fn (get-in built [:finalized-scales :y]))
        at (fn [id vx vy] (let [{:keys [x y]} (box-of built id)] (pixel c (long (+ x (fx vx))) (long (+ y (fy vy))))))]
    (is (= [0 0 0] (at [:panel 0 0] 1 1)) "a's point in a's panel")
    (is (= [0 0 0] (at [:panel 0 1] 9 9)) "b's point in b's panel")
    (is (= [255 255 255] (at [:panel 0 0] 9 9)) "b's point not in a's panel")))

(deftest theme-validation-test
  (are [t k] (= k (:key (error #(build (facet-plot {:theme t})))))
    {:facet {:gap -1}} :gap
    {:strip {:border-width -1}} :border-width
    {:panel {:alternate-fill nil}} nil))

;; review fixes

(deftest invalid-facet-values-test
  (let [built (build (facet-plot {:data (conj rows {:x 1 :y 1 :g ##NaN} {:x 1 :y 1 :g ##Inf})}))]
    (is (= [0 2 4 6 8 10] (get-in built [:split :facets :levels :facet])) "no NaN / Inf panel")
    (is (= {:facet 2} (get-in built [:split :facets :dropped])))))

(deftest facet-spec-validation-test
  (are [scales in] (= in (:in (error #(build (facet-plot {:scales scales})))))
    {:facet [0 2]} [:scales :facet]
    {:facet {:domian [0 2]}} [:scales :facet :domian]
    {:facet {:formatter "x"}} [:scales :facet :formatter]
    {:facet-col {:domain ["a"]}} [:scales :facet-col])
  (is (= [:scales] (:in (error #(build (facet-plot {:scales [0 1]})))))))

(defn- exhaustive-shape
  "Old cljplot search over all pairs (reference for the bounded one)."
  [n]
  (if (<= n 1)
    [1 1]
    (let [s (Math/sqrt n)
          [_ x y] (first (sort (for [x (range 1 (inc n)) y (range 1 (inc n))
                                     :let [d1 (- (* x y) n)] :when (>= d1 0)]
                                 [(+ (* 2.0 d1) (Math/pow (- s x) 2) (Math/pow (- s y) 2)) x y])))]
      [(min x y) (max x y)])))

(deftest bounded-shape-test
  (doseq [n (range 0 120)]
    (is (= (exhaustive-shape n) (facet/most-squared-shape n)) (str n)))
  (let [t0 (System/nanoTime)]
    (facet/most-squared-shape 2000)
    (is (< (/ (- (System/nanoTime) t0) 1e6) 500.0) "2000 panels: fast")))
