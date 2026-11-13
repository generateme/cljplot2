(ns cljplot2.strips-test
  "Cycle 3a: the strip framework (placement, band geometry, groups) and the box, violin, strip, rug and extent charts."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [wadogo.scale :as s]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.box :as box]
            [cljplot2.chart.extent :as extent]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.strip :as strip]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]))

(def ^:private rows
  (vec (for [i (range 90)] {:v (+ (mod (* i 37) 23) (* 4 (mod i 3))) :lvl (["a" "b" "c"] (mod i 3)) :g (["p" "q"] (mod i 2))})))

(def ^:private options {:width 500 :height 400})

(defn- built [p] (plot/build p options))
(defn- panel-data [b i] (get-in b [:stats i :data facet/single-panel]))
(defn- panel-box [b] (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %))))

(deftest band-geometry-test
  (let [finalized (scale/finalize (scale/scale-spec :y {:domain ["a" "b" "c"]}) [0.0 300.0] 3)
        expected (select-keys (s/data (s/scale :bands {:domain ["a" "b" "c"] :range [0.0 300.0]})) [:bandwidth :step])]
    (is (= expected (scale/band-geometry finalized)))
    (is (pos? (double (:bandwidth expected))))
    (is (nil? (scale/band-geometry (scale/finalize (scale/scale-spec :x {:domain [0 1]}) [0.0 300.0] 3))))))

(defn- placement-of [b chart-options place]
  (let [{:keys [w h]} (panel-box b)]
    (strip/placement {:w w :h h} (:finalized-scales b) chart-options place)))

(deftest placement-test
  (testing "band: centre at the level, cross toward higher :y, span = width x bandwidth"
    (doseq [flip [false true]
            :let [b (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box)] :flip flip})
                  scales (:finalized-scales b)
                  {:keys [point span]} (placement-of b {} :band)
                  fx (scale/forward-fn (:x scales)) fy (scale/forward-fn (:y scales))
                  bw (double (:bandwidth (scale/band-geometry (:y scales))))
                  ^Vec2 c (point 5 "b" 0.0)
                  ^Vec2 up (point 5 "b" 0.5)]]
      (is (== (* 0.8 bw) (span "b")))
      (if flip
        (do (is (== (fy "b") (.x c))) (is (== (fx 5) (.y c)))
            (is (== (+ (fy "b") (* 0.4 bw)) (.x up))) "flipped: vertical value axis, cross to the right")
        (do (is (== (fx 5) (.x c))) (is (== (fy "b") (.y c)))
            (is (== (- (fy "b") (* 0.4 bw)) (.y up)) "cross up = smaller pixel y")))))
  (testing ":center without :y - the panel middle; :edge - inside the low edge"
    (let [b (built {:data rows :mapping {:x :v} :charts [(chart/box)]})
          {:keys [h]} (panel-box b)
          ^Vec2 c ((:point (placement-of b {} :center)) 5 nil 0.0)
          edge (placement-of b {} :edge)]
      (is (== (* 0.5 (double h)) (.y c)))
      (is (== (double h) (.y ^Vec2 ((:point edge) 5 nil -0.5))))
      (is (== (- (double h) 8.0) (.y ^Vec2 ((:point edge) 5 nil 0.5))))
      (is (== 8.0 ((:span edge) nil)))))
  (testing "{:at v}: centred on y = v, thickness pixels; the y extent includes v"
    (let [b (built {:data rows :mapping {:x :v} :charts [(chart/histogram) (chart/box {:place {:at 50} :thickness 10})]})
          fy (scale/forward-fn (get-in b [:finalized-scales :y]))
          {:keys [point span]} (placement-of b {:thickness 10} {:at 50})]
      (is (== (fy 50) (.y ^Vec2 (point 5 nil 0.0))))
      (is (== 10.0 (span nil)))
      (is (>= (double (get-in b [:extents :y :extent :max])) 50.0)))))

(deftest place-errors-test
  (are [p] (= :place (:key (error #(built p))))
    {:data rows :mapping {:x :v} :charts [(chart/box {:place :band})]}
    {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box {:place :center})]}
    {:data rows :mapping {:x :v :y :v} :charts [(chart/box)]})
  (is (= #{[:charts 0 :options :place]} (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/box {:place :middle})]}))))
  (testing "numeric levels work with an explicit :bands kind, sorted"
    (let [b (built {:data (map #(assoc % :n (mod (:v %) 3)) rows) :mapping {:x :v :y :n} :scales {:y {:kind :bands}}
                    :charts [(chart/box)]})]
      (is (= [0 1 2] (get-in b [:finalized-scales :y :domain]))))))

(deftest groups-test
  (let [items (fn [p] (:items (panel-data (built p) 0)))]
    (testing "two groups dodge by default: halves of the cross range, each narrowed by :dodge-gap (default 0.1)"
      (is (= #{[-0.475 -0.025] [0.025 0.475]}
             (set (map :slot (items {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/box)]})))))
      (is (= #{[-0.5 0.0] [0.0 0.5]}
             (set (map :slot (items {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/box {:dodge-gap 0})]})))))
      (is (= #{[-0.25 -0.25] [0.25 0.25]}
             (set (map :slot (items {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/box {:dodge-gap 1})]}))))
          "gap 1 collapses each slot to its centre")
      (is (= #{[:charts 0 :options :dodge-gap]}
             (error-ins #(built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box {:dodge-gap 1.5})]})))))
    (testing "three groups dodged into thirds; overlay - the whole range"
      (let [rows3 (map #(assoc % :k (mod (:v %) 3)) rows)
            p {:data rows3 :mapping {:x :v :y :lvl :color :k} :scales {:color {:kind :ordinal}}}]
        (is (= 3 (count (distinct (map :slot (items (assoc p :charts [(chart/box)])))))))
        (is (= #{[-0.5 0.5]} (set (map :slot (items (assoc p :charts [(chart/box {:position :overlay})]))))))
        (is (= :position (:key (error #(built (assoc p :charts [(chart/violin {:position :mirror})]))))))))
    (testing "mirror: the first group above, the second below"
      (is (= #{[0.0 0.5] [-0.5 0.0]}
             (set (map :slot (items {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/violin {:position :mirror})]}))))))))

(deftest extents-test
  (let [b (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box)]})]
    (is (= :bands (get-in b [:finalized-scales :y :kind])))
    (is (= ["a" "b" "c"] (get-in b [:finalized-scales :y :domain])))))

(deftest box-test
  (let [sample [1 2 3 4 5 6 7 8 9 40 -30]
        st (box/box-stats sample)
        [q1 m q3] (stats/percentiles sample [25.0 50.0 75.0])]
    (is (= [q1 m q3] [(:q1 st) (:median st) (:q3 st)]))
    (is (= (vec (take 2 (stats/adjacent-values sample q1 q3 m))) [(:lav st) (:uav st)]))
    (is (= #{40.0 -30.0} (set (map double (:outliers st)))))
    (is (= 11 (:n st))))
  (testing "outliers widen the x extent unless :outliers false"
    (let [data (conj (vec (for [i (range 20)] {:v i})) {:v 200})
          max-x (fn [opts] (get-in (built {:data data :mapping {:x :v} :charts [(chart/box opts)]}) [:extents :x :extent :max]))]
      (is (== 200 (max-x {})))
      (is (< (double (max-x {:outliers false})) 200))))
  (testing "varwidth factors are sqrt(n / max n)"
    (let [f (box/width-factors [{:n 4} {:n 16}] true)]
      (is (= [0.5 1.0] [(f {:n 4}) (f {:n 16})]))
      (is (= 1.0 ((box/width-factors [{:n 4} {:n 16}] false) {:n 4}))))))

(defn- area [{:keys [xs widths]}]
  (reduce + (map (fn [[x0 x1] [w0 w1]] (* 0.5 (- (double x1) (double x0)) (+ (double w0) (double w1))))
                 (partition 2 1 xs) (partition 2 1 widths))))

(deftest violin-test
  (let [data (concat (for [i (range 30)] {:v (mod (* i 7) 11) :lvl "a"}) (for [i (range 90)] {:v (mod (* i 5) 13) :lvl "b"}))
        items (fn [opts] (sort-by :level (:items (panel-data (built {:data data :mapping {:x :v :y :lvl} :charts [(chart/violin opts)]}) 0))))]
    (testing ":width - each violin's widest point is 1"
      (is (every? #(< (Math/abs (- 1.0 (double (reduce max (:widths %))))) 1e-12) (items {}))))
    (testing ":area - equal areas; :count - areas proportional to the counts (untrimmed: :cut 3 keeps whole densities)"
      (let [[a b] (items {:scale :area :cut 3})] (is (< (Math/abs (- (area a) (area b))) (* 0.02 (area b)))))
      (let [[a b] (items {:scale :count :cut 3})] (is (< (Math/abs (- (/ (area a) (area b)) (/ 30.0 90.0))) 0.02))))
    (testing ":cut 0 - the violin spans the data range"
      (let [[a] (items {})] (is (== 0 (first (:xs a)))) (is (== 10 (peek (:xs a))))))
    (testing "an item with fewer than 2 distinct values has no violin"
      (let [b (built {:data (conj (vec data) {:v 1 :lvl "c"}) :mapping {:x :v :y :lvl} :charts [(chart/violin)]})]
        (is (= 2 (count (:items (panel-data b 0)))))
        (is (= 1 (get-in b [:stats 0 :dropped :group])))))))

(deftest strip-chart-test
  (let [offsets (fn [opts] (mapcat :offsets (:items (panel-data (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/stripplot opts)]}) 0))))]
    (is (= (offsets {:rng (r/rng :mersenne 5)}) (offsets {:rng (r/rng :mersenne 5)})) "same seed, same offsets")
    (is (not= (offsets {}) (offsets {})) "a fresh rng per build")
    (is (every? #(<= -0.3 (double %) 0.3) (offsets {:jitter 0.6 :rng (r/rng :mersenne 1)})) "clamped to +-jitter/2")
    (is (every? zero? (offsets {:jitter 0.0})))
    (is (= #{[:charts 0 :options :rng]} (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/stripplot {:rng 5})]}))))))

(deftest extent-chart-test
  (let [sample (mapv :v rows)
        interval (fn ([method args] (extent/interval sample method {:method-args args :rng (r/rng :mersenne 1)}))
                   ([method args statistic] (extent/interval sample method {:method-args args :rng (r/rng :mersenne 1)
                                                                            :statistic statistic})))]
    (doseq [[k f] extent/extent-methods
            :let [[lo hi c] (f sample)]]
      (is (= {:lo (double lo) :hi (double hi) :center (some-> c double)} (interval k nil)) (str k)))
    (is (= (let [[lo hi c] (stats/percentile-extent sample 10.0 90.0)] {:lo lo :hi hi :center c}) (interval :percentile [10.0 90.0])))
    (testing "every bootstrap interval: reproducible with a seeded rng, ordered, alpha and samples as method-args"
      (doseq [k (keys extent/bootstrap-intervals)
              :let [{:keys [lo hi center] :as iv} (interval k [0.1 200])]]
        (is (<= lo center hi) (str k))
        (is (= iv (interval k [0.1 200])) (str k " same seed, same interval"))
        (is (some? (interval k nil)) (str k " defaults"))))
    (testing ":statistic chooses what is bootstrapped (default mean)"
      (is (== (stats/mean sample) (:center (interval :bootstrap-basic nil))))
      (is (== (stats/median sample) (:center (interval :bootstrap-basic nil stats/median)))))
    (is (= {:lo 0.0 :hi 1.0 :center nil} (interval (fn [_] [0 1]) nil)))
    (is (= :method (:key (error #(interval (fn [_] [1]) nil)))))
    (is (= :method-args (:key (error #(interval :ci [1 2 3 4])))))
    (is (= :method-args (:key (error #(interval :percentile ["a"])))))
    (is (= :method-args (:key (error #(interval :bootstrap-t [0.1 200 3])))))
    (is (= #{[:charts 0 :options :method]} (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/extent {:method :bootstrap-ci})]}))))
    (is (= #{[:charts 0 :options :statistic]} (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/extent {:statistic :median})]}))))))

(deftest strip-theme-test
  (is (= #{[:theme :chart :box :colour]} (error-ins #(plot/build {:theme {:chart {:box {:colour :red}}}}))))
  (is (= #{[:theme :chart :rug :width]} (error-ins #(plot/build {:theme {:chart {:rug {:width 2}}}}))) "rug lines use :line-width"))

(defn- red? [^long rgb]
  (let [r (bit-and (bit-shift-right rgb 16) 0xff) g (bit-and (bit-shift-right rgb 8) 0xff) b (bit-and rgb 0xff)]
    (and (> r 180) (< g 90) (< b 90))))

(deftest render-test
  (testing "a rug on :edge draws inside the panel, along its bottom"
    (let [p {:data rows :mapping {:x :v} :charts [(chart/density) (chart/rug {:color :red :line-width 2})]}
          b (built p)
          {:keys [x y w h]} (panel-box b)
          ^BufferedImage img (plot/render-image p options)
          reds (for [i (range (.getWidth img)) j (range (.getHeight img)) :when (red? (.getRGB img (int i) (int j)))] [i j])]
      (is (seq reds))
      (is (every? (fn [[i j]] (and (<= x i (+ x w)) (<= (- (+ y h) 9) j (+ y h)))) reds))))
  (testing "every strip chart renders, flipped and not, with groups"
    (doseq [c [(chart/box {:mean true}) (chart/violin {:inner :quartiles}) (chart/stripplot {:rng (r/rng :mersenne 1)})
               (chart/rug) (chart/extent {:method :ci})]
            flip [false true]]
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :lvl :color :g} :charts [c] :flip flip} options))
          (str (:kind c) " flip " flip)))))

(deftest band-shading-test
  (let [p {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box {:color :blue})]
           :theme {:plot {:background :white} :panel {:grid {:bands :none :x :none :bands-fill [255 0 0]}}}}
        b (built p)
        {:keys [x y w h]} (panel-box b)
        fy (scale/forward-fn (get-in b [:finalized-scales :y]))
        ^BufferedImage img (plot/render-image p options)
        px (fn [level] (.getRGB img (int (+ (long x) 3)) (int (+ (long y) (long (fy level))))))]
    (testing "every second band (from the second level) is filled, across the panel"
      (is (red? (px "b")))
      (is (not (red? (px "a"))))
      (is (not (red? (px "c")))))
    (testing ":none turns shading off"
      (let [^BufferedImage img (plot/render-image (assoc-in p [:theme :panel :grid :bands-fill] :none) options)]
        (is (not (red? (.getRGB img (int (+ (long x) 3)) (int (+ (long y) (long (fy "b"))))))))))
    (testing "flipped: vertical stripes"
      (let [p (assoc p :flip true)
            b (built p)
            {:keys [x y]} (panel-box b)
            fy (scale/forward-fn (get-in b [:finalized-scales :y]))
            ^BufferedImage img (plot/render-image p options)]
        (is (red? (.getRGB img (int (+ (long x) (long (fy "b")))) (int (+ (long y) 3)))))))
    (is (= #{[:theme :panel :grid :bands-fill]} (error-ins #(plot/build {:theme {:panel {:grid {:bands-fill 5.5}}}}))))))

(deftest review-fixes-test
  (testing "F2: nil options mean the default"
    (is (some? (built {:data rows :mapping {:x :v} :charts [(chart/density {:cut nil})]})))
    (is (some? (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/violin {:cut nil :n nil})]})))
    (is (= (get-in (built {:data (conj rows {:v 500 :lvl "a" :g "p"}) :mapping {:x :v} :charts [(chart/box)]}) [:extents :x])
           (get-in (built {:data (conj rows {:v 500 :lvl "a" :g "p"}) :mapping {:x :v} :charts [(chart/box {:outliers nil})]}) [:extents :x]))
        ":outliers nil keeps outliers")
    (is (true? (:zero-line? (panel-data (built {:data rows :mapping {:x :v :color :g} :charts [(chart/density {:position :mirror :zero-line nil})]}) 0)))))
  (testing "F5: band placement needs a bands :y scale, {:at v} a numeric one"
    (let [b (built {:data rows :mapping {:x :v} :charts [(chart/histogram)]})
          {:keys [w h]} (panel-box b)]
      (is (= :place (:key (error #(strip/placement {:w w :h h} (:finalized-scales b) {} :band))))))
    (is (= :extents (:key (error #(built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box) (chart/rug {:mapping {:y nil} :place {:at 1}})]}))))
        "levels and {:at v} mixed: the extents stage rejects it first")
    (is (= :place (:key (error #(plot/render-image {:data rows :mapping {:x :v} :scales {:y {:domain ["a" "b"]}}
                                                    :charts [(chart/rug {:place {:at 1}})]} options))))
        "{:at v} on an explicit bands domain: placement rejects it"))
  (testing "F1: a flipped violin whose level is outside an explicit domain is skipped, not drawn with NaN"
    (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :lvl} :scales {:y {:domain ["a" "b"]}}
                                                     :flip true :charts [(chart/violin)]} options)))))
