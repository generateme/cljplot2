(ns cljplot2.batch-c2-fix-y2-test
  "Batch C2 review fixes (y2): bars with a numeric colour and an explicit colour domain (every row builds its bar, an
  aggregate outside the domain in the NA colour), the bar's `:color-value`, a binned colour with one bin, end labels
  on a reversed axis, the n-label gap on the item, stat-space helpers in the lower layers."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.common :as common]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as rows]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 400 :height 300})

(def ^:private colored
  "Levels p, q, r - three rows each, `:v` 2, colour `:c` 0-2, 10-12, 20-22 (sums 3, 33, 63)."
  (vec (for [[l b] [["p" 0] ["q" 10] ["r" 20]] i (range 3)] {:l l :v 2.0 :c (+ b i)})))

(defn- stats [p] (get-in (plot/build p size) [:stats 0]))

(defn- bars [p] (vec (mapcat (comp :bars val) (:data (stats p)))))

(defn- rgb-at
  "Gives the opaque colour of plot `p` at pixel `[x y]`."
  [p x y]
  (let [^BufferedImage img (plot/render-image (update p :labels #(merge {:x :none :y :none} %)) size)]
    (c/set-alpha (c/to-color (.getRGB img (int x) (int y))) 255)))

(deftest bar-color-domain-test
  (let [p {:data colored :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]}]
    (testing "an explicit colour domain drops no row: every bar keeps its length"
      (doseq [domain [[0 15] [0 30] [100 200]]]
        (let [s (stats (assoc p :scales {:color {:domain domain}}))]
          (is (= [6.0 6.0 6.0] (mapv :length (mapcat (comp :bars val) (:data s)))))
          (is (= 0 (get-in s [:dropped :color])))
          (is (contains? (:na s) :color) "aggregates outside the domain - the NA key"))))
    (testing "every aggregate inside: no NA key"
      (is (not (contains? (:na (stats (assoc p :scales {:color {:domain [0 100]}}))) :color))))
    (testing "the bar outside the domain is drawn in the NA colour, the one inside through the scale"
      (let [q (assoc p :scales {:color {:domain [0 15] :range [[255 0 0] [0 0 255]]}}
                     :charts [(chart/bar {:stroke-color :none :width 1.0})] :legend false)
            na (c/set-alpha (c/to-color (get-in theme/default-theme [:palette :na])) 255)]
        ;; vertical bars: levels p q r across the panel, bars up from the bottom
        (is (not= na (rgb-at q 80 250)) "p (3) inside [0 15]")
        (is (= na (rgb-at q 220 250)) "q (33) outside")
        (is (= na (rgb-at q 360 250)) "r (63) outside")))
    (testing "binned breaks: a value outside every bin - NA colour, length kept"
      (is (= [6.0 6.0 6.0] (mapv :length (bars (assoc p :scales {:color {:kind :binned :bins [0 10 20]}}))))))
    (testing "NA rows: kept (NA colour) or dropped with :groups {:na false} - counted"
      (let [with-na (conj colored {:l "s" :v 1.0 :c nil})]
        (is (= [6.0 6.0 6.0 1.0] (mapv :length (bars (assoc p :data with-na)))))
        (is (= 0 (get-in (stats (assoc p :data with-na)) [:dropped :color])))
        (is (= 1 (get-in (stats (assoc p :data with-na :groups {:na false})) [:dropped :color])))))
    (testing "a bar whose colour values are all non-finite: not drawn, its rows counted (batch D: invalid isn't NA)"
      (let [p-nan (assoc p :data (conj colored {:l "s" :v 1.0 :c ##NaN}))]
        (is (= [6.0 6.0 6.0] (mapv :length (bars p-nan))))
        (is (= 1 (get-in (stats p-nan) [:dropped :color])))
        (is (not (contains? (:na (stats p-nan)) :color)) "NaN isn't NA")))
    (testing "zero rows, one row"
      (is (instance? BufferedImage (plot/render-image (assoc p :data [] :scales {:color {:domain [0 1]}}) size)))
      (is (= [[6.0 3.0]] (mapv (juxt :length :color-value) (bars (assoc p :data (take 3 colored) :scales {:color {:domain [5 9]}}))))))
    (testing "flip, polar, facets, stack render with a domain"
      (doseq [extra [{:flip true} {:coord {:kind :polar}} {:mapping {:x :l :y :v :color :c :facet :l}}
                     {:charts [(chart/bar {:position :stack})]}]]
        (is (instance? BufferedImage (plot/render-image (merge (assoc p :scales {:color {:domain [0 15]}}) extra) size)))))))

(deftest bar-color-value-test
  (testing "a numeric colour value sits on the bar as :color-value, not in its aesthetics"
    (let [b (first (bars {:data colored :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]}))]
      (is (= 3.0 (:color-value b)))
      (is (not (contains? (:aesthetics b) :color)))))
  (testing "a :value-label function gets it as :color-value"
    (let [seen (atom [])]
      (plot/build {:data colored :mapping {:x :l :y :v :color :c}
                   :charts [(chart/bar {:value-label #(do (swap! seen conj %) "x")})]} size)
      (is (= #{3.0 33.0 63.0} (set (map :color-value @seen))))
      (is (every? #(nil? (get-in % [:group :color])) @seen))))
  (testing "categorical colours: no :color-value, the level in [:group :color]"
    (let [seen (atom [])]
      (plot/build {:data colored :mapping {:x :l :y :v :color :l}
                   :charts [(chart/bar {:value-label #(do (swap! seen conj %) "x")})]} size)
      (is (= #{"p" "q" "r"} (set (map #(get-in % [:group :color]) @seen))))
      (is (not-any? #(contains? % :color-value) @seen)))))

(deftest extent-beside-numeric-color-test
  (let [p {:data colored :mapping {:x :l :y :v :color :c}}]
    (is (= :mapping (:key (error #(plot/build (assoc p :charts [(chart/bar) (chart/extent)]) size)))))
    (is (map? (plot/build (assoc p :charts [(chart/bar) (chart/extent {:mapping {:color nil}})]) size)))))

(deftest binned-one-bin-test
  (let [one [{:l "a" :v 1 :c 5} {:l "b" :v 1 :c 5}]
        colour (fn [spec] (rgb-at {:data one :mapping {:x :l :y :v :color :c} :scales {:color spec} :legend false
                                   :charts [(chart/bar {:stroke-color :none :width 1.0})]}
                                  100 250))]
    (testing "one distinct value: the gradient's middle, as a linear scale"
      (is (= (colour {}) (colour {:kind :binned})))
      (is (= (colour {:range [[255 0 0] [0 0 255]]}) (colour {:kind :binned :range [[255 0 0] [0 0 255]]})
             (c/to-color [128 0 128]))))
    (testing "a one-colour range keeps its colour"
      (is (= (c/to-color [0 128 0]) (colour {:kind :binned :range [[0 128 0]]}))))
    (testing "two bins: the range ends (unchanged)"
      (is (= (c/to-color [255 0 0]) (rgb-at {:data [{:l "a" :v 1 :c 0} {:l "b" :v 1 :c 10}] :mapping {:x :l :y :v :color :c}
                                             :scales {:color {:kind :binned :bins 2 :range [[255 0 0] [0 0 255]]}} :legend false
                                             :charts [(chart/bar {:stroke-color :none :width 1.0})]}
                                            100 250))))))

(deftest value-direction-test
  (testing "range and domain directions"
    (is (= 1.0 (scale/value-direction {:range [0.0 100.0] :domain [0.0 10.0]})))
    (is (= -1.0 (scale/value-direction {:range [100.0 0.0] :domain [0.0 10.0]})) "y pixels grow down")
    (is (= -1.0 (scale/value-direction {:range [0.0 100.0] :domain [10.0 0.0]})) "a reversed axis")
    (is (= 1.0 (scale/value-direction {:range [100.0 0.0] :domain [10.0 0.0]})) "both")
    (is (= 1.0 (scale/value-direction {:range [0.0 6.28] :domain [-5 5]})) "polar radians, long domain")))

(deftest end-labels-on-reversed-axis-test
  (let [pl {:point (fn [at _ _] (v/vec2 (* 10.0 (- 10.0 (double at))) 50.0))}
        place (fn [domain] (first (strip-chart/end-label-placements
                                   pl [{:level "a" :at 5.0 :slot [-0.5 0.5] :side 1.0 :text "n=1"}]
                                   {:x {:kind :linear :range [0.0 100.0] :domain domain}})))]
    (testing "past larger values: on a reversed axis toward smaller pixels"
      (is (= [47.0 [:right :middle]] ((juxt :x :anchor) (place [10.0 0.0]))))))
  (testing "box n labels and bar end labels render reversed, flipped and polar"
    (let [box-rows (concat (for [i (range 20)] {:v (double i) :g (if (even? i) "a" "b")}) [{:v 60.0 :g "a"}])]
      (doseq [p [{:data box-rows :mapping {:x :g :y :v} :charts [(chart/box {:n-label true})] :scales {:y {:reverse true}}}
                 {:data box-rows :mapping {:x :g :y :v} :charts [(chart/box {:n-label true})] :scales {:y {:reverse true}} :flip true}
                 {:data colored :mapping {:x :l :y :v} :charts [(chart/bar {:value-label true})] :scales {:y {:reverse true}}}
                 {:data colored :mapping {:x :l :y :v} :charts [(chart/bar {:value-label true})] :scales {:y {:reverse true}}
                  :coord {:kind :polar}}]]
        (is (instance? BufferedImage (plot/render-image p size)))))))

(deftest n-label-gap-on-item-test
  (testing "one arity each"
    (is (= 1 (count (:arglists (meta #'strip-chart/draw-n-labels!)))))
    (is (= 1 (count (:arglists (meta #'strip-chart/n-label-room))))))
  (testing "n-label-room takes the largest :n-label-gap of the items"
    (let [env {:theme theme/default-theme :hint :high}
          room (fn [items] (strip-chart/n-label-room {:n-label true} {:panel items} env))
          item {:level "a" :n 3}]
      (is (= 5.0 (- (second (:x (room [(assoc item :n-label-gap 5.0)]))) (second (:x (room [item]))))))
      (is (= 7.0 (- (second (:x (room [(assoc item :n-label-gap 5.0) (assoc item :n-label-gap 7.0)]))) (second (:x (room [item]))))))
      (is (nil? (strip-chart/n-label-room {:n-label false} {:panel [item]} env)))
      (is (nil? (room []))))))

(deftest stat-space-helpers-test
  (testing "the transform by the stat space"
    (is (= 1.0 ((:forward (scale/stat-transform-for :scale {:kind :log})) 10.0)))
    (is (= 10.0 ((:forward (scale/stat-transform-for :data {:kind :log})) 10.0)))
    (is (= 10.0 ((:forward (scale/stat-transform-for :scale nil)) 10.0))))
  (testing "values the transform can't place are dropped and counted"
    (let [log (scale/stat-transform-for :scale {:kind :log})]
      (is (= {:values [1.0 2.0] :dropped 3} (rows/transformed-numbers log {:values [10.0 0.0 -1.0 100.0] :dropped 1})))
      (is (= {:values [] :dropped 0} (rows/transformed-numbers log {:values [] :dropped 0})))))
  (testing "chart.common keeps only the schema piece"
    (is (nil? (resolve 'cljplot2.chart.common/stat-space-transform)))
    (is (some? common/stat-space-option))))

(deftest stat-space-dates-test
  (let [dated (for [i (range 10)] {:d (java.time.LocalDate/of 2020 1 (inc i)) :y (double (* i i))})]
    (testing "histogram and density take no dates (in either space)"
      (doseq [ch [(chart/histogram) (chart/density) (chart/histogram {:stat-space :scale}) (chart/density {:stat-space :scale})]]
        (is (= :mapping (:key (error #(plot/build {:data dated :mapping {:x :d} :charts [ch]} size)))))))
    (testing "smooth fits dates in both spaces"
      (doseq [space [:data :scale]]
        (is (map? (plot/build {:data dated :mapping {:x :d :y :y} :charts [(chart/smooth {:stat-space space})]} size)))))))
