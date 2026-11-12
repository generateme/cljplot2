(ns cljplot2.chart-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.walk :as walk]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as data]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale]
            [cljplot2.graphics.shape :as shape])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(def ^:private rows [{:a 1 :b 10} {:a 2 :b 20} {:a 3 :b 30}])

(defn- build
  ([plot] (plot/build plot {:width 600 :height 400}))
  ([plot opts] (plot/build plot (merge {:width 600 :height 400} opts))))

(defn- canvas-pos
  "Canvas pixel of value `v` on scale `k`: panel-local scale position plus the :panel box offset."
  [built k v]
  (let [{:keys [x y]} (grid/item-box (:layout built) :panel)]
    (+ (if (= k :x) x y) (scale/forward (get-in built [:finalized-scales k]) v))))

(defn- scatter-plot [extra] (merge {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)]} extra))

;; 1.x data and mapping

(deftest rows-test
  (are [d r] (= r (data/rows d))
    nil []
    [] []
    {} []
    '({:a 1}) [{:a 1}]
    {:a [1 2] :b [3 4]} [{:a 1 :b 3} {:a 2 :b 4}]
    {:a [] :b []} [])
  (are [d] (= :data (:key (error-data #(data/rows d))))
    {:a [1 2] :b [3]}
    {:a 1}
    [{:a 1} 2]
    "rows"
    42))

(deftest data-forms-test
  (is (= (:stats (build (scatter-plot {})))
         (:stats (build (scatter-plot {:data {:a [1 2 3] :b [10 20 30]}})))))
  (is (= :data (:key (error-data #(build (scatter-plot {:data {:a [1 2] :b [1]}})))))))

(deftest mapping-test
  (testing "keyword and function accessors"
    (is (= (:stats (build (scatter-plot {})))
           (:stats (build (scatter-plot {:mapping {:x #(:a %) :y #(:b %)}}))))))
  (testing "missing or invalid accessors"
    (is (= {:key :mapping :value :y} (error-data #(build (scatter-plot {:mapping {:x :a}})))))
    (is (= {:key :mapping :value :x} (error-data #(build (scatter-plot {:mapping {:x "a" :y :b}})))))
    (is (= :mapping (:key (error-data #(build (scatter-plot {:mapping [:a :b]})))))))
  (testing "chart mapping overrides the plot's, chart data replaces the plot's"
    (let [built (build {:data rows :mapping {:x :a :y :b}
                        :charts [(chart/scatter {:mapping {:y :c} :data [{:a 5 :c 7}]})]})]
      (is (= {:x [5] :y [7]} (select-keys (get-in built [:stats 0 :data :panel]) [:x :y])))))
  (testing "chart :data nil counts as absent: the plot's data is used"
    (is (= [1 2 3] (get-in (build (scatter-plot {:charts [(chart/scatter {:data nil})]})) [:stats 0 :data :panel :x])))))

(deftest dropped-test
  (let [data (concat rows [{:a nil :b 1} {:a ##NaN :b 1} {:a 1 :b ##Inf} {:a 2} {:b 3}])
        st (get-in (build (scatter-plot {:data data})) [:stats 0])]
    (is (= 3 (count (get-in st [:data :panel :x]))))
    (is (= {:x 3 :y 2} (:dropped st))))
  (testing "mixed kinds in one dimension"
    (is (= :extents (:key (error-data #(build (scatter-plot {:data (conj rows {:a "x" :b 1})}))))))))

(deftest empty-data-test
  (testing "no rows: no extents, no scales, no error"
    (let [built (build (scatter-plot {:data []}))]
      (is (= {} (:extents built) (:scale-specs built)))
      (is (some? (plot/render (scatter-plot {:data []}) {:width 100 :height 100})))))
  (testing "a mapped dimension without any valid value raises (usually a misspelt mapping)"
    (is (= {:key :mapping :value :x :rows 1} (error-data #(build (scatter-plot {:data [{:a nil :b 1}]})))))
    (is (= {:key :mapping :value :x :rows 3} (error-data #(build (scatter-plot {:mapping {:x :aa :y :b}})))))
    (is (= {:key :mapping :value :y :rows 3} (error-data #(build (scatter-plot {:mapping {:x :a :y :bb}}))))))
  (testing "explicit domain without data"
    (is (= [0.0 10.0] (mapv double (:domain (get-in (build (scatter-plot {:data [] :scales {:x {:domain [0 10]}}}))
                                                    [:finalized-scales :x]))))))
  (testing "scale entry with neither domain nor data"
    (is (= {:key :domain :scale :x} (error-data #(build (scatter-plot {:data [] :scales {:x {}}})))))))

;; 2.x chart contract

(deftest chart-value-test
  (is (= (chart/scatter {:size 3}) (chart/scatter {:size 3})))
  (is (= (build (scatter-plot {})) (build (scatter-plot {}))) "independent builds are =")
  (is (= :options (:key (error-data #(chart/scatter [1])))))
  (is (= :charts (:key (error-data #(build {:charts (list (chart/scatter))})))))
  (is (= :charts (:key (error-data #(build {:charts [{:kind :fake}]}))))))

(deftest stats-are-data-test
  (let [stats (:stats (build (scatter-plot {})))
        fns (atom 0)]
    (walk/postwalk #(do (when (fn? %) (swap! fns inc)) %) stats)
    (is (zero? @fns))
    (is (= [:data :dropped :extents :na :padding] (sort (keys (first stats)))))))

(deftest extents-test
  (let [built (build {:mapping {:x :a :y :b}
                      :charts [(chart/scatter {:data [{:a 0 :b 1} {:a 5 :b 2}]})
                               (chart/scatter {:data [{:a 3 :b 1} {:a 10 :b 2}] :padding {:x [0.2 0.0]}})]})]
    (is (= {:kind :numeric :min 0.0 :max 10.0} (get-in built [:extents :x :extent])))
    (is (= [0.2 0.05] (get-in built [:extents :x :padding])) "largest padding per side"))
  (testing "different kinds across charts"
    (is (= :extents (:key (error-data #(build {:mapping {:x :a :y :b}
                                               :charts [(chart/scatter {:data [{:a 1 :b 1}]})
                                                        (chart/scatter {:data [{:a "x" :b 1}]})]})))))))

;; 3.x domains

(defn- spec-domain [plot k] (get-in (build plot) [:scale-specs k :domain]))

(defn- final-domain [plot k] (get-in (build plot) [:finalized-scales k :domain]))

(deftest data-domain-test
  (testing "data domain in the spec, padding applied after nice (5% each end by default)"
    (is (= [0.0 100.0] (spec-domain (scatter-plot {:data [{:a 0 :b 0} {:a 100 :b 1}]}) :x)))
    (is (= [0.05 0.05] (get-in (build (scatter-plot {:data [{:a 0 :b 0} {:a 100 :b 1}]})) [:scale-specs :x :padding])))
    (is (= [-5.0 105.0] (final-domain (scatter-plot {:data [{:a 0 :b 0} {:a 100 :b 1}]}) :x))))
  (testing "outermost ticks sit inside the padded domain"
    (let [fx (get-in (build (scatter-plot {:data [{:a 0 :b 0} {:a 100 :b 1}]})) [:finalized-scales :x])]
      (is (< (first (:domain fx)) (first (:ticks fx))))
      (is (> (second (:domain fx)) (peek (:ticks fx))))))
  (testing ":expand wins, data only"
    (is (= [0.0 100.0] (final-domain (scatter-plot {:data [{:a 0 :b 0} {:a 100 :b 1}] :scales {:x {:expand [0 0]}}}) :x)))
    (is (= [0 10] (spec-domain (scatter-plot {:scales {:x {:domain [0 10] :expand [0.5 0.5]}}}) :x))))
  (testing "explicit domain wins over data"
    (is (= [-100 100] (spec-domain (scatter-plot {:scales {:x {:domain [-100 100]}}}) :x))))
  (testing "invalid :expand"
    (are [e] (= :expand (:key (error-data #(build (scatter-plot {:scales {:x {:expand e}}})))))
      [0.1] [-0.1 0] "0" nil [0 0 0]))
  (testing "invalid chart padding"
    (is (= :padding (:key (error-data #(build (scatter-plot {:charts [(chart/scatter {:padding {:x [-1 0]}})]}))))))))

(deftest single-value-test
  (are [v domain] (= domain (spec-domain (scatter-plot {:data [{:a v :b 1} {:a v :b 2}] :scales {:x {:expand [0 0]}}}) :x))
    5 [4.5 5.5]
    -5 [-5.5 -4.5]
    0 [-1.0 1.0])
  (testing "single point lands in the panel center"
    (let [built (build (scatter-plot {:data [{:a 5 :b 7}]}))
          {:keys [x y w h]} (grid/item-box (:layout built) :panel)]
      (is (< (abs (- (+ x (/ w 2.0)) (canvas-pos built :x 5))) 1.0))
      (is (< (abs (- (+ y (/ h 2.0)) (canvas-pos built :y 7))) 1.0)))))

(deftest scale-kinds-from-data-test
  (is (= :bands (get-in (build (scatter-plot {:data [{:a 1 :b "u"} {:a 2 :b "v"}]})) [:scale-specs :y :kind])))
  (is (= ["u" "v"] (spec-domain (scatter-plot {:data [{:a 1 :b "u"} {:a 2 :b "v"} {:a 3 :b "u"}]}) :y)))
  (is (= :datetime (get-in (build (scatter-plot {:data [{:a (LocalDate/of 2020 1 1) :b 1} {:a (LocalDate/of 2020 6 1) :b 2}]}))
                           [:scale-specs :x :kind])))
  (testing "numeric data on bands needs a domain"
    (is (= {:key :domain :scale :x} (error-data #(build (scatter-plot {:scales {:x {:kind :bands}}}))))))
  (testing "log scale: padding in log space stays positive; non-positive data fails"
    (let [d (final-domain (scatter-plot {:data [{:a 1 :b 1} {:a 1000 :b 2}] :scales {:x {:kind :log}}}) :x)]
      (is (< 0 (first d) 1)))
    (is (= {:key :domain :scale :x} (error-data #(build (scatter-plot {:data [{:a 0 :b 1} {:a 10 :b 2}] :scales {:x {:kind :log}}})))))))

;; 4.x scatter drawing

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(defn- pixel [canvas x y]
  (let [rgb (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right rgb 16) 0xff) (bit-and (bit-shift-right rgb 8) 0xff) (bit-and rgb 0xff)]))

(defn- ink-box
  [canvas x0 y0 x1 y1]
  (let [pts (for [x (range x0 x1) y (range y0 y1) :when (not= [255 255 255] (pixel canvas x y))] [x y])]
    (when (seq pts)
      [(apply min (map first pts)) (apply min (map second pts)) (apply max (map first pts)) (apply max (map second pts))])))

(defn- one-point
  "Render one point at the panel center with chart options; return [canvas center-x center-y]."
  [options]
  (let [p {:data [{:a 5 :b 5}] :mapping {:x :a :y :b} :charts [(chart/scatter options)] :theme plain
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
        built (build p)
        cx (canvas-pos built :x 5)
        cy (canvas-pos built :y 5)]
    [(plot/render p {:width 600 :height 400}) (long cx) (long cy)]))

(deftest point-position-test
  (let [[c cx cy] (one-point {:size 9 :color :black})
        [x0 y0 x1 y1] (ink-box c (- cx 20) (- cy 20) (+ cx 20) (+ cy 20))]
    (is (<= (abs (- cx (/ (+ x0 x1) 2.0))) 1))
    (is (<= (abs (- cy (/ (+ y0 y1) 2.0))) 1))))

(deftest size-test
  (let [width (fn [size] (let [[c cx cy] (one-point {:size size :color :black})
                               [x0 _ x1 _] (ink-box c (- cx 30) (- cy 30) (+ cx 30) (+ cy 30))]
                           (- x1 x0)))]
    (is (< (width 4) (width 20))))
  (testing "size 0 draws nothing"
    (let [[c cx cy] (one-point {:size 0 :color :black})]
      (is (nil? (ink-box c (- cx 5) (- cy 5) (+ cx 5) (+ cy 5))))))
  (is (= :size (:key (error-data #(build (scatter-plot {:charts [(chart/scatter {:size -1})]})))))))

(deftest shapes-test
  (doseq [s shape/shapes]
    (let [[c cx cy] (one-point {:size 15 :color :black :shape s})]
      (is (some? (ink-box c (- cx 10) (- cy 10) (+ cx 11) (+ cy 11))) (str "shape " s))))
  (testing "filled vs outlined differ at the center"
    (let [center (fn [s] (let [[c cx cy] (one-point {:size 15 :color :black :shape s})] (pixel c cx cy)))]
      (is (= [0 0 0] (center \O) (center \S)))
      (is (= [255 255 255] (center \o) (center \s)))))
  (is (= {:key :shape :value \Q} (error-data #(build (scatter-plot {:charts [(chart/scatter {:shape \Q})]}))))))

(deftest clip-test
  (let [p {:data [{:a 0 :b 0} {:a 10 :b 10}] :mapping {:x :a :y :b} :theme (assoc plain :axis {:x {:line-color :none :tick-color :none}
                                                                                              :y {:line-color :none :tick-color :none}})
           :charts [(chart/scatter {:size 30 :color :black})]
           ;; no ticks: no tick labels next to the panel
           :scales {:x {:expand [0 0] :nice false :ticks []} :y {:expand [0 0] :nice false :ticks []}}}
        built (build p)
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)
        c (plot/render p {:width 600 :height 400})]
    (is (some? (ink-box c x (- (+ y h) 15) (+ x 15) (+ y h))) "corner point drawn inside")
    (is (nil? (ink-box c (- x 15) (- (+ y h) 15) x (+ y h))) "nothing left of the panel")
    (is (nil? (ink-box c x (+ y h) (+ x 15) (+ y h 15))) "nothing below the panel")))

(deftest chart-order-test
  (let [p {:data [{:a 5 :b 5}] :mapping {:x :a :y :b} :theme plain
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}
           :charts [(chart/scatter {:size 20 :color :red}) (chart/scatter {:size 20 :color :blue})]}
        built (build p)
        cx (long (canvas-pos built :x 5))
        cy (long (canvas-pos built :y 5))]
    (is (= [0 0 255] (pixel (plot/render p {:width 600 :height 400}) cx cy)))))

(deftest bands-points-test
  (let [p {:data [{:a 1 :b "u"} {:a 2 :b "v"}] :mapping {:x :a :y :b} :theme plain :charts [(chart/scatter {:size 9 :color :black})]}
        built (build p)
        cy (long (canvas-pos built :y "v"))
        cx (long (canvas-pos built :x 2))]
    (is (= [0 0 0] (pixel (plot/render p {:width 600 :height 400}) cx cy)))))

(deftest performance-test
  (let [data (vec (for [i (range 10000)] {:a (mod (* i 7919) 1000) :b (mod (* i 104729) 997)}))
        p {:data data :mapping {:x :a :y :b} :charts [(chart/scatter)]}
        _ (plot/render p {:width 600 :height 400})
        t0 (System/nanoTime)
        _ (plot/render p {:width 600 :height 400})
        ms (/ (- (System/nanoTime) t0) 1.0e6)]
    (is (< ms 1000.0) (str "10k points took " ms " ms"))))

;; review fixes

(deftest outside-bands-domain-test
  (testing "values outside an explicit bands domain are skipped, not a crash"
    (let [p {:data [{:a 1 :b "a"} {:a 2 :b "c"}] :mapping {:x :a :y :b} :charts [(chart/scatter {:size 9 :color :black})]
             :theme plain :scales {:y {:domain ["a" "b"]}}}
          built (build p)
          c (plot/render p {:width 600 :height 400})
          cx (long (canvas-pos built :x 1))
          cy (long (canvas-pos built :y "a"))]
      (is (= [0 0 0] (pixel c cx cy)) "the in-domain point is drawn")))
  (testing "forward-fn gives NaN for values the scale can't place"
    (let [f (scale/forward-fn (get-in (build {:scales {:y {:domain ["a" "b"]}}}) [:finalized-scales :y]))]
      (is (Double/isNaN (f "zzz")))
      (is (not (Double/isNaN (f "a")))))))

(deftest style-in-stats-test
  (let [st (get-in (build (scatter-plot {:charts [(chart/scatter {:size 7})]})) [:stats 0 :data :panel :style])]
    (is (= 7 (:size st)))
    (is (= \O (:shape st)) "theme default merged")))

(deftest numeric-extents-are-doubles-test
  (is (= {:kind :numeric :min 1.0 :max 3.0} (data/extent :x [1 2 3])))
  (is (= {:kind :numeric :min 1.0 :max 2.5} (data/extent :x [1 2.5]))))

(deftest explicit-domain-inset-test
  (testing "explicit domain with charts gets the charts' padding after nice: ticks inside"
    (let [fx (get-in (build (scatter-plot {:scales {:x {:domain [0 10]}}})) [:finalized-scales :x])]
      (is (= [-0.5 10.5] (:domain fx)))
      (is (< (first (:domain fx)) (first (:ticks fx))))
      (is (> (second (:domain fx)) (peek (:ticks fx))))))
  (testing ":expand applies to explicit domains"
    (is (= [-1.0 11.0] (:domain (get-in (build (scatter-plot {:scales {:x {:domain [0 10] :expand [0.1 0.1]}}})) [:finalized-scales :x])))))
  (testing "explicit domain without charts (axes only) isn't padded"
    (is (= [0.0 10.0] (mapv double (:domain (get-in (build {:scales {:x {:domain [0 10]}}}) [:finalized-scales :x]))))))
  (testing "temporal and bands domains aren't padded"
    (is (nil? (get-in (build (scatter-plot {:data [{:a (LocalDate/of 2020 1 1) :b 1} {:a (LocalDate/of 2020 6 1) :b 2}]}))
                      [:scale-specs :x :padding])))
    (is (nil? (get-in (build (scatter-plot {:data [{:a 1 :b "u"} {:a 2 :b "v"}]})) [:scale-specs :y :padding]))))
  (testing ":padding is internal"
    (is (= {:key :padding :scale :x} (error-data #(build (scatter-plot {:scales {:x {:padding [0 0]}}})))))))

;; step 11: draw order

(deftest draw-order-test
  (let [data [{:x 1 :y 1 :c "a" :o 2} {:x 1 :y 1 :c "b" :o 1} {:x 1 :y 1 :c "c" :o nil}]
        colors {:color {:range [[255 0 0] [0 0 255] [0 255 0]]}}
        top-color (fn [mapping opts]
                    (let [p {:data data :mapping (merge {:x :x :y :y :color :c} mapping) :scales colors
                             :charts [(chart/scatter (merge {:size 20} opts))]}
                          built (build p)]
                      (pixel (plot/render p {:width 600 :height 400}) (canvas-pos built :x 1) (canvas-pos built :y 1))))]
    (is (= [0 255 0] (top-color {} {})) "no :order: data order, last row on top")
    (is (= [255 0 0] (top-color {:order :o} {})) "ascending: highest order on top")
    (is (= [0 0 255] (top-color {:order :o} {:order-direction :descending})) "descending: lowest order on top")
    (testing "nil order values drawn first; validation"
      (is (= [nil 1 2] (map :o (let [b (build {:data data :mapping {:x :x :y :y :order :o :color :o} :scales {:color {:kind :ordinal}}
                                                :charts [(chart/scatter)]})]
                                   (map #(hash-map :o %) (get-in b [:stats 0 :data :panel :color]))))))
      (is (= :order-direction (:key (error-data #(build {:data data :mapping {:x :x :y :y :order :o} :charts [(chart/scatter {:order-direction :up})]})))))
      (is (= :order (:key (error-data #(build {:data [{:x 1 :y 1 :o 1} {:x 2 :y 2 :o "a"}] :mapping {:x :x :y :y :order :o} :charts [(chart/scatter)]}))))))))
