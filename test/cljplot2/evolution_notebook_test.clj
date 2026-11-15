(ns cljplot2.evolution-notebook-test
  "Evolution notebook review: ggplot2 loess defaults in smooth (R1), function labels in their given order (L2), padded
  datetime domains (L6)."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.smooth :as smooth]
            [fastmath.core :as m]
            [fastmath.interpolation :as interpolation])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate LocalDateTime LocalTime]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))

;; R1 loess defaults

(def ^:private wave (mapv (fn [x] {:x x :y (+ (m/sin (* 0.3 x)) (* 0.1 (m/cos (* 7.0 x))))}) (range 40)))

(deftest loess-defaults-test
  (testing "smooth's loess: ggplot2's span 0.75 and local quadratic fits, user params over them"
    (is (= {:bandwidth 0.75 :degree 2} smooth/loess-defaults))
    (let [curve (fn [opts] (get-in (build {:data wave :mapping {:x :x :y :y} :charts [(chart/smooth opts)]})
                                   [:stats 0 :data facet/single-panel :curves 0]))
          xs (mapv :x wave) ys (mapv :y wave)
          {:keys [x y]} (curve {:band :none})]
      (is (every? true? (map #(< (m/abs (- (double ((interpolation/loess xs ys {:bandwidth 0.75 :degree 2}) %1)) (double %2))) 1e-12) x y)))
      (is (not= y (:y (curve {:band :none :method-params {:degree 1}}))) ":degree 1 is local linear")
      (is (= y (:y (curve {:band :none :method-params {:bandwidth 0.75}}))) "the degree default stays with a user bandwidth")))
  (testing "loess :degree only 1 or 2; other methods but lm reject it"
    (is (= :method-params (:key (error #(build {:data wave :mapping {:x :x :y :y} :charts [(chart/smooth {:method-params {:degree 0}})]})))))
    (is (= :method-params (:key (error #(build {:data wave :mapping {:x :x :y :y} :charts [(chart/smooth {:method :glm :method-params {:degree 2}})]})))))))

;; L2 function labels in order

(defn- color-domain [p] (get-in (build p) [:scale-specs :color :domain]))

(def ^:private ten
  (mapv (fn [k] [(str "sin " k "x") (fn [x] (m/sin (* k (double x))))]) (range 1 11)))

(deftest function-label-order-test
  (testing "[label f] pairs keep their order (no lexicographic sort: \"sin 10x\" last)"
    (is (= (mapv first ten)
           (color-domain {:charts [(chart/function {:f ten :domain [0 3] :mapping {:color :function}})]}))))
  (testing "a small map keeps its iteration order; a vector its indices"
    (is (= ["z" "a" "m"] (color-domain {:charts [(chart/function {:f (array-map "z" m/sin "a" m/cos "m" m/tan) :domain [0 1]
                                                                   :mapping {:color :function}})]})))
    (is (= [0 1 2] (mapv long (color-domain {:charts [(chart/function {:f [m/sin m/cos m/exp] :domain [0 1] :mapping {:color :function}})]})))))
  (testing "an explicit domain wins"
    (is (= ["sin 2x" "sin 1x"] (color-domain {:scales {:color {:domain ["sin 2x" "sin 1x"]}}
                                              :charts [(chart/function {:f (subvec ten 0 2) :domain [0 1] :mapping {:color :function}})]}))))
  (testing "another chart's (unordered) levels on the same scale: sorted, as before"
    (is (= ["a" "b" "c"] (color-domain {:data [{:x 0 :y 0 :g "a"}]
                                        :charts [(chart/function {:f [["c" m/sin] ["b" m/cos]] :domain [0 1] :mapping {:color :function}})
                                                 (chart/scatter {:mapping {:x :x :y :y :color :g}})]}))))
  (testing "ordered extents merge"
    (is (:ordered? (rows/merge-extents :color [{:kind :categorical :values ["b" "a"] :ordered? true}
                                                {:kind :categorical :values ["c"] :ordered? true}])))
    (is (nil? (:ordered? (rows/merge-extents :color [{:kind :categorical :values ["b"] :ordered? true}
                                                      {:kind :categorical :values ["a"]}])))))
  (testing "pairs render with a legend; a parametric curve"
    (is (instance? BufferedImage (plot/render-image {:charts [(chart/function {:f ten :domain [0 m/PI] :mapping {:color :function}})]} size)))
    (is (instance? BufferedImage (plot/render-image {:aspect :iso :charts [(chart/function {:parametric (fn [t] [(m/sin (* 3.0 t)) (m/sin (* 4.0 t))])
                                                                                            :t [0 m/TWO_PI]})]} size))))
  (testing "a vector mixing pairs and functions is rejected"
    (is (= :f (:key (error #(build {:charts [(chart/function {:f [["a" m/sin] m/cos] :domain [0 1]})]})))))))

;; L6 padded datetime domains

(deftest datetime-padding-test
  (let [domain (fn [rows] (get-in (build {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]}) [:finalized-scales :x :domain]))]
    (testing "dates: 5% of the span each side, beyond nice ends"
      (let [[lo hi] (domain [{:x (LocalDate/of 2020 1 1) :y 1} {:x (LocalDate/of 2020 12 31) :y 2}])]
        (is (neg? (compare lo (LocalDateTime/of 2020 1 1 0 0))))
        (is (pos? (compare hi (LocalDateTime/of 2020 12 31 0 0))))))
    (testing "LocalTime: the niced domain is LocalDateTime (one day), padded past midnight on both sides"
      (let [[lo hi] (domain [{:x LocalTime/MIDNIGHT :y 1} {:x (LocalTime/of 23 59) :y 2}])]
        (is (instance? LocalDateTime lo))
        (is (.isBefore (.toLocalTime ^LocalDateTime lo) (LocalTime/of 23 59)) "the day before, late")
        (is (.isAfter (.toLocalTime ^LocalDateTime lo) (LocalTime/of 22 0)))
        (is (.isBefore (.toLocalTime ^LocalDateTime hi) (LocalTime/of 2 0)) "the next day, early")))
    (testing "a single date: widened, then padded"
      (let [[lo hi] (domain [{:x (LocalDate/of 2020 6 1) :y 1}])]
        (is (neg? (compare lo (LocalDateTime/of 2020 5 31 0 0))))
        (is (pos? (compare hi (LocalDateTime/of 2020 6 2 0 0))))))
    (testing "an explicit date domain is padded too (as numeric explicit domains)"
      (let [[lo] (get-in (build {:data [{:x (LocalDate/of 2020 1 5) :y 1}] :mapping {:x :x :y :y}
                                 :scales {:x {:domain [(LocalDate/of 2020 1 1) (LocalDate/of 2020 1 31)]}}
                                 :charts [(chart/scatter)]})
                         [:finalized-scales :x :domain])]
        (is (neg? (compare lo (LocalDateTime/of 2020 1 1 0 0))))))))

;; M1 segment baseline padding

(deftest segment-baseline-padding-test
  (let [padding (fn [rows opts & [extra]]
                  (get-in (build (merge {:data rows :mapping {:x :x :y :y} :charts [(chart/segment opts)]} extra)) [:stats 0 :padding]))]
    (testing "no padding on the side where the literal end is the extent's end"
      (is (= [0.0 0.05] (:y (padding [{:x 1 :y 3} {:x 2 :y 5}] {:yend 0}))) "stems up from 0")
      (is (= [0.05 0.0] (:y (padding [{:x 1 :y -3} {:x 2 :y -5}] {:yend 0}))) "stems down to 0")
      (is (= [0.05 0.05] (:y (padding [{:x 1 :y -3} {:x 2 :y 5}] {:yend 0}))) "0 inside: both sides")
      (is (= [0.0 0.05] (:x (padding [{:x 1 :y 3} {:x 2 :y 5}] {:xend 0}))) ":xend on x")
      (is (= [0.05 0.05] (:x (padding [{:x 1 :y 3} {:x 2 :y 5}] {:yend 0}))) "the other axis keeps 5%")
      (is (= [0.05 0.05] (:y (padding [{:x 1 :y 3} {:x 2 :y 5}] {}))) "no literal end"))
    (testing "dates and the user's :padding"
      (is (= [0.0 0.05] (:x (padding [{:x (LocalDate/of 2020 1 5) :y 1}] {:xend (LocalDate/of 2020 1 1)}))))
      (is (= [0.1 0.1] (:y (padding [{:x 1 :y 3}] {:yend 0 :padding {:y [0.1 0.1]}})))))
    (testing "the axis starts at the baseline"
      (is (= 0.0 (double (first (get-in (build {:data [{:x 1 :y 3} {:x 2 :y 5}] :mapping {:x :x :y :y} :charts [(chart/segment {:yend 0})]})
                                        [:finalized-scales :y :domain]))))))))

(deftest padding-requests-test
  (let [y-domain (fn [p] (get-in (build p) [:finalized-scales :y :domain]))
        rows [{:x 1 :y 3} {:x 2 :y 5}]]
    (testing "a lollipop - segment {:yend 0} with a scatter - starts at the axis"
      (is (= 0.0 (double (first (y-domain {:data rows :mapping {:x :x :y :y}
                                           :charts [(chart/segment {:yend 0}) (chart/scatter)]}))))))
    (testing "a histogram with points on it starts at 0 too"
      (is (= 0.0 (double (first (y-domain {:data [{:x 1 :y 0.5} {:x 2 :y 1} {:x 2 :y 1.5}] :mapping {:x :x}
                                           :charts [(chart/histogram) (chart/scatter {:mapping {:y :y}})]}))))))
    (testing "the user's :padding is a request: zero wins over another chart's default (axis at the data minimum)"
      (is (= 3.0 (double (first (y-domain {:data rows :mapping {:x :x :y :y}
                                           :charts [(chart/line) (chart/scatter {:padding {:y [0 0.05]}})]}))))))
    (testing "requests compete by size: room below the data over a zero baseline"
      (let [b (build {:data (mapv (fn [v] {:x (double v)}) (range 10)) :mapping {:x :x}
                      :charts [(chart/density) (chart/rug {:place :bottom})]})]
        (is (neg? (double (first (get-in b [:finalized-scales :y :domain])))))))
    (testing "data below the baseline: the baseline isn't the extent's end, the defaults apply"
      (is (neg? (double (first (y-domain {:data (conj rows {:x 3 :y -2}) :mapping {:x :x :y :y}
                                          :charts [(chart/segment {:yend 0}) (chart/scatter)]}))))))
    (testing "charts without requests: unchanged (largest padding)"
      (is (= [0.05 0.05] (get-in (build {:data rows :mapping {:x :x :y :y} :charts [(chart/line) (chart/scatter)]})
                                 [:extents :y :padding]))))))

(deftest padding-requests-reach-test
  (let [y-domain (fn [p] (get-in (build p) [:finalized-scales :y :domain]))]
    (testing "N1 another chart's data below a baseline voids the zero request: default padding below"
      (is (< (double (first (y-domain {:data [{:x 1 :y -5} {:x 2 :y 1} {:x 2 :y 3}] :mapping {:x :x}
                                       :charts [(chart/histogram) (chart/scatter {:mapping {:y :y}})]})))
             -5.0) "histogram + a point at -5")
      (is (< (double (first (y-domain {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "a"}] :mapping {:x :x :y :y}
                                       :charts [(chart/line {:position :stack :mapping {:color :g}})
                                                (chart/scatter {:data [{:x 1 :y -3}]})]})))
             -3.0) "stacked line + a point at -3"))
    (testing "N1 a user request on a chart that doesn't reach the end doesn't cut the other chart's marks"
      (is (neg? (double (first (y-domain {:mapping {:x :x :y :y}
                                          :charts [(chart/scatter {:data [{:x 1 :y 3} {:x 2 :y 5}] :padding {:y [0 0]}})
                                                   (chart/scatter {:data [{:x 1 :y 0} {:x 2 :y 10}]})]})))))
      (is (= 0.0 (double (first (y-domain {:mapping {:x :x :y :y}
                                           :charts [(chart/scatter {:data [{:x 1 :y 0} {:x 2 :y 5}] :padding {:y [0 0]}})
                                                    (chart/scatter {:data [{:x 1 :y 3} {:x 2 :y 10}]})]}))))
          "the requesting chart reaches the low end: its zero holds"))
    (testing "a tie at the end: both charts reach it, the request holds"
      (is (= 0.0 (double (first (y-domain {:data [{:x 1 :y 0} {:x 2 :y 5}] :mapping {:x :x :y :y}
                                           :charts [(chart/segment {:yend 0}) (chart/scatter)]}))))))))
