(ns cljplot2.evolution-cycle3b-test
  "Evolution cycle 3b: groundwork (x keys, `fill-area!`, `:ymin` / `:ymax`), ribbon chart, smooth chart."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.ribbon :as ribbon]
            [cljplot2.chart.smooth :as smooth-chart]
            [clojure2d.core :as c2d]
            [cljplot2.data.rng :as rng]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.smooth :as smooth]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.theme :as theme]
            [fastmath.interpolation :as interpolation]
            [fastmath.interpolation.gp :as gp]
            [fastmath.ml.regression :as regression]
            [fastmath.random :as r]
            [malli.core :as mc])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]
           [java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime ZoneId ZoneOffset ZonedDateTime]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- stats [p] (get-in (build p) [:stats 0]))
(defn- panel-data [p] (get-in (stats p) [:data facet/single-panel]))
(def ^:private white {:plot {:background :white} :panel {:grid {:x :none :y :none}}})
(defn- identity-projection [x y] (Vec2. (double x) (double y)))
(defn- close? [^double tol a b] (every? true? (map #(< (Math/abs (- (double %1) (double %2))) tol) a b)))

(def ^:private noisy
  (let [rng (r/rng :mersenne 7)]
    (mapv (fn [x] {:x x :y (+ (Math/sin (* 0.3 x)) (* 0.2 (r/grandom rng)))}) (range 30))))

;; 1. groundwork

(deftest x-key-test
  (testing "1.1 x keys of numbers and java.time values; nil otherwise"
    (is (= 2.0 (rows/x-key 2)))
    (is (= 86400000.0 (rows/x-key (LocalDate/of 1970 1 2))))
    (is (= 3600000.0 (rows/x-key (LocalTime/of 1 0))))
    (is (nil? (rows/x-key "a")))
    (is (nil? (rows/x-key nil)))))

(deftest key->x-test
  (testing "1.2 key -> value -> key for every type; LocalDate becomes LocalDateTime"
    (let [zdt (ZonedDateTime/of 2020 3 1 10 30 0 0 (ZoneId/of "Europe/Warsaw"))
          odt (OffsetDateTime/of 2020 3 1 10 30 0 0 (ZoneOffset/ofHours 3))]
      (are [like] (let [k (+ 12345.0 (double (rows/x-key like)))] (= k (rows/x-key (rows/key->x k like))))
        1.5 (LocalDate/of 2020 1 1) (LocalDateTime/of 2020 1 1 5 0) (Instant/ofEpochSecond 1000) zdt odt (LocalTime/of 3 0))
      (is (instance? LocalDateTime (rows/key->x 0.0 (LocalDate/of 2020 1 1))))
      (is (= (.getZone zdt) (.getZone ^ZonedDateTime (rows/key->x 0.0 zdt))) "zone kept")
      (is (= (.getOffset odt) (.getOffset ^OffsetDateTime (rows/key->x 0.0 odt))) "offset kept")))
  (testing "1.2 rounded to the millisecond; LocalTime kept within the day"
    (is (= (LocalDateTime/of 1970 1 1 0 0 0 1000000) (rows/key->x 0.6 (LocalDate/of 2020 1 1))))
    (is (= (LocalTime/of 23 59 59 999000000) (rows/key->x 1.0e9 (LocalTime/of 1 0))))
    (is (= LocalTime/MIDNIGHT (rows/key->x -5.0 (LocalTime/of 1 0))))
    (is (= 2.5 (rows/key->x 2.5 1)) "numbers unchanged")))

(deftest groundwork-test
  (testing "1.3 fill-area! is public in polyline"
    (is (fn? polyline/fill-area!)))
  (testing "1.4 :ymin / :ymax are mapping keys"
    (is (mc/validate rows/mapping-schema {:x :a :ymin :lo :ymax :hi}))
    (is (not (mc/validate rows/mapping-schema {:ymin 5}))))
  (testing "4.1 theme entries"
    (is (= 0.3 (get-in theme/default-theme [:chart :ribbon :alpha])))
    (is (= 0.3 (get-in theme/default-theme [:chart :smooth :alpha])))))

;; 2. ribbon

(def ^:private bands-data
  (vec (for [g ["a" "b" "c"] x (range 5)] {:x x :lo (- x) :hi (+ x 10) :g g})))

(defn- ribbon-plot [data opts & [extra]]
  (merge {:data data :mapping {:x :x :ymin :lo :ymax :hi} :charts [(chart/ribbon opts)]} extra))

(deftest ribbon-compute-test
  (testing "2.1 one band per group, ordered by x; y extent over both bounds"
    (let [s (stats (ribbon-plot (reverse bands-data) {} {:mapping {:x :x :ymin :lo :ymax :hi :color :g}}))
          {:keys [bands]} (get-in s [:data facet/single-panel])]
      (is (= 3 (count bands)))
      (is (= [0 1 2 3 4] (:x (first bands))))
      (is (= {:kind :numeric :min -4.0 :max 14.0} (get-in s [:extents facet/single-panel :y])))))
  (testing "2.2 invalid x dropped, invalid bounds break (counted)"
    (let [s (stats (ribbon-plot [{:x 0 :lo 0 :hi 1} {:x nil :lo 0 :hi 1} {:x 2 :lo nil :hi 1} {:x 3 :lo 0 :hi ##NaN}] {}))]
      (is (= {:x 1 :ymin 1 :ymax 1} (select-keys (:dropped s) [:x :ymin :ymax])))
      (is (= [0 2 3] (get-in s [:data facet/single-panel :bands 0 :x])))
      (is (= [0 nil 0] (get-in s [:data facet/single-panel :bands 0 :ymin])))))
  (testing "no valid values for a mapped dimension"
    (is (= :mapping (:key (error #(build (ribbon-plot [{:x 0 :lo "a" :hi 1}] {:interpolator :cubic}
                                                      {:mapping {:x :x :ymin :nope :ymax :hi}})))))))
  (testing "2.6 :fill-gradient-axis needs :fill-gradient; unknown gradient"
    (is (= :fill-gradient-axis (:key (error #(build (ribbon-plot bands-data {:fill-gradient-axis :y}))))))
    (is (= :fill-gradient-axis (:key (error #(build (ribbon-plot bands-data {:fill-gradient [:yellow :purple] :fill-gradient-axis :z}))))))))

(deftest band-runs-test
  (let [xs [0 1 2 3 4]]
    (testing "2.2 a nil bound breaks the band; one-pair runs left out"
      (is (= 2 (count (ribbon/band-runs xs [0 0 nil 0 0] [1 1 1 1 1] identity-projection {}))))
      (is (= 1 (count (ribbon/band-runs xs [0 nil 0 0 0] [1 1 1 1 1] identity-projection {}))) "one point left of the gap")
      (is (= [] (ribbon/band-runs [0] [0] [1] identity-projection {})))
      (is (= [] (ribbon/band-runs [] [] [] identity-projection {}))))
    (testing "NaN bounds (smooth bands) break the band too"
      (is (= 2 (count (ribbon/band-runs xs [0 0 ##NaN 0 0] [1 1 1 1 1] identity-projection {})))))
    (testing "2.3 ymin above ymax: the same runs swapped"
      (let [[[u l]] (ribbon/band-runs xs [1 1 1 1 1] [0 0 0 0 0] identity-projection {})
            [[u2 l2]] (ribbon/band-runs xs [0 0 0 0 0] [1 1 1 1 1] identity-projection {})]
        (is (= [u l] [l2 u2]))))
    (testing "2.5 :interpolator samples both bounds through the data"
      (let [[[u l]] (ribbon/band-runs xs [0 1 0 1 0] [5 6 5 6 5] identity-projection {:interpolator :cubic :along :x})]
        (is (= 5 (count u)) "every pixel of a 4 px span")
        (is (close? 1e-9 [5 6 5 6 5] (map second u)))
        (is (close? 1e-9 [0 1 0 1 0] (map second l)))))
    (testing "2.8 flipped: sampled along pixel y"
      (let [[[u]] (ribbon/band-runs xs [0 0 0 0 0] [1 2 1 2 1] (fn [x y] (Vec2. (double y) (double x)))
                                    {:interpolator :cubic :along :y :sample-step 0.5})]
        (is (= 9 (count u)))
        (is (close? 1e-9 [1 2 1 2 1] (map first (take-nth 2 u))))))
    (testing "repeated x with an interpolator"
      (is (= :interpolator (:key (error #(ribbon/band-runs [0 0 1] [0 0 0] [1 1 1] identity-projection
                                                           {:interpolator :cubic :along :x}))))))))

(defn- red-pixels
  "Pixels of `img` with red above 200, green and blue below 60."
  [^BufferedImage img]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img))
               :let [v (.getRGB img x y)]
               :when (and (> (bit-and (bit-shift-right v 16) 0xff) 200) (< (bit-and (bit-shift-right v 8) 0xff) 60)
                          (< (bit-and v 0xff) 60))]
           1)))

(deftest ribbon-draw-test
  (let [p (fn [opts] (ribbon-plot [{:x 0 :lo 0 :hi 1} {:x 1 :lo 0 :hi 2}] (merge {:fill :none} opts)
                                  {:theme white}))
        img #(plot/render-image (p %) size)]
    (testing "2.4 no edge by default; :stroke-color strokes both bounds"
      (is (zero? (red-pixels (img {:color [255 0 0]}))))
      (is (pos? (red-pixels (img {:color [255 0 0] :stroke-color :color}))))
      (is (pos? (red-pixels (img {:stroke-color [255 0 0] :stroke-width 2 :dash :dashed}))))))
  (testing "2.6 / 2.7 gradient, legend, flip render"
    (is (instance? BufferedImage (plot/render-image (ribbon-plot bands-data {:fill-gradient [:yellow :purple] :fill-gradient-axis :y}
                                                                 {:mapping {:x :x :ymin :lo :ymax :hi :color :g}}) size)))
    (is (instance? BufferedImage (plot/render-image (ribbon-plot bands-data {:interpolator :monotone :stroke-color :color} {:flip true :mapping {:x :x :ymin :lo :ymax :hi :color :g}}) size)))))

;; 3. smooth

(defn- smooth-plot [data opts & [extra]]
  (merge {:data data :mapping {:x :x :y :y} :charts [(chart/smooth opts)]} extra))

(defn- curve-of [p] (get-in (panel-data p) [:curves 0]))

(def ^:private line-data (mapv (fn [x] {:x x :y (+ 1 (* 2 x))}) (range 10)))

(deftest smooth-data-test
  (testing "3.1.1 rows with invalid x or y left out (counted); categorical x throws"
    (let [s (stats (smooth-plot (into line-data [{:x nil :y 1} {:x 3 :y nil} {:x 4 :y ##NaN}]) {:method :lm}))]
      (is (= {:x 1 :y 2 :group 0} (select-keys (:dropped s) [:x :y :group]))))
    (is (= :x (:key (error #(build (smooth-plot [{:x "a" :y 1} {:x "b" :y 2}] {:method :lm})))))))
  (testing "3.1.2 too few distinct x: the group dropped, the others drawn"
    (let [s (stats (smooth-plot (conj (mapv #(assoc % :g "a") line-data) {:x 1 :y 1 :g "b"} {:x 1 :y 2 :g "b"})
                                {:method :lm} {:mapping {:x :x :y :y :color :g}}))]
      (is (= 1 (get-in s [:dropped :group])))
      (is (= 1 (count (get-in s [:data facet/single-panel :curves]))))))
  (testing "3.1.2 required points: lm degree + 2, glm 3, loess by bandwidth, others 2"
    (is (= 3 (smooth/required-points :lm nil)))
    (is (= 5 (smooth/required-points :lm {:degree 3})))
    (is (= 3 (smooth/required-points :glm {:family :poisson})))
    (is (= 3 (smooth/required-points :loess nil)) "default bandwidth 0.75")
    (is (= 7 (smooth/required-points :loess {:bandwidth 0.3})))
    (is (= 4 (smooth/required-points :loess {:bandwidth 0.5})))
    (is (= 2 (smooth/required-points :loess {:bandwidth 1.0})))
    (is (= 2 (smooth/required-points :cubic nil)))
    (is (= 1 (get-in (stats (smooth-plot (take 6 line-data) {:method-params {:bandwidth 0.3}})) [:dropped :group])) "6 points, loess at 0.3")
    (is (= 0 (get-in (stats (smooth-plot (take 7 line-data) {:method-params {:bandwidth 0.3}})) [:dropped :group])))
    (is (= 1 (get-in (stats (smooth-plot (take 2 line-data) {})) [:dropped :group])) "2 points, default 0.75")
    (is (= 0 (get-in (stats (smooth-plot (take 3 line-data) {})) [:dropped :group]))))
  (testing "3.1.2 other fit errors name the group"
    (let [e (error #(build (smooth-plot (mapv (fn [r] (assoc r :g "a")) line-data)
                                        {:method (fn [_ _] (throw (ex-info "boom" {})))} {:mapping {:x :x :y :y :color :g}})))]
      (is (= :method (:key e)))
      (is (= {:color "a"} (:group e)))))
  (testing "empty data"
    (is (= [] (:curves (panel-data (smooth-plot [] {} {:scales {:x {:domain [0 1]} :y {:domain [0 1]}}})))))))

(deftest smooth-methods-test
  (let [xs (mapv :x noisy) ys (mapv :y noisy)]
    (testing "3.2.1 loess (default) equals fastmath loess on the grid"
      (let [{:keys [x y]} (curve-of (smooth-plot noisy {:band :none}))
            f (interpolation/loess xs ys smooth/loess-defaults)]
        (is (close? 1e-12 (map f x) y))))
    (testing "3.2.1 :method-params reach loess"
      (let [{:keys [x y]} (curve-of (smooth-plot noisy {:band :none :method-params {:bandwidth 0.6 :iters 0}}))
            f (interpolation/loess xs ys {:bandwidth 0.6 :degree 2 :iters 0})]
        (is (close? 1e-12 (map f x) y))))
    (testing "3.2.2 lm degree 1 on an exact line; degree 3 equals fastmath"
      (is (close? 1e-9 (map #(+ 1 (* 2 %)) (:x (curve-of (smooth-plot line-data {:method :lm}))))
                  (:y (curve-of (smooth-plot line-data {:method :lm})))))
      (let [{:keys [x y]} (curve-of (smooth-plot noisy {:method :lm :method-params {:degree 3}}))
            model (regression/lm ys (mapv vector xs) {:transformer (regression/polynomial-transformer xs 3)})]
        (is (close? 1e-9 (map #(regression/predict model [%]) x) y))))
    (testing "3.2.3 glm with a family"
      (let [counts (mapv (fn [x] {:x x :y (+ 1 (mod x 4))}) (range 20))
            {:keys [x y]} (curve-of (smooth-plot counts {:method :glm :method-params {:family :poisson}}))
            model (regression/glm (mapv :y counts) (mapv (comp vector :x) counts) {:family :poisson})]
        (is (close? 1e-9 (map #(regression/predict model [%]) x) y))))
    (testing "3.2.4 gp mean"
      (let [{:keys [x y]} (curve-of (smooth-plot noisy {:method :gp :method-params {:noise 0.1}}))
            model (gp/gaussian-process xs ys {:noise 0.1})]
        (is (close? 1e-9 (map #(gp/predict model % false) x) y))))
    (testing "3.2.5 interpolators and functions"
      (is (close? 1e-9 [1 19] ((juxt first peek) (:y (curve-of (smooth-plot line-data {:method :akima}))))))
      (is (close? 1e-12 (repeat 200 7.0) (:y (curve-of (smooth-plot line-data {:method (fn [_ _] (constantly 7.0))})))))
      (is (close? 1e-12 (repeat 200 3.0)
                  (:y (curve-of (smooth-plot line-data {:method (fn [_ _ {:keys [c]}] (constantly c)) :method-params {:c 3.0}}))))
          "params passed to a function")
      (is (= [] (:bands (curve-of (smooth-plot noisy {:method :cubic-smoothing})))) "no own band"))
    (testing "3.2.5 repeated x averaged for interpolation methods"
      (is (= [[1.0 2.0] [2.0 5.0]] (smooth/prepared [2 1 2 1] [4 1 6 3])))
      (is (some? (curve-of (smooth-plot (into line-data line-data) {:method :cubic})))))
    (testing "3.2.6 unknown method; :degree outside lm or not a positive integer"
      (is (= :method (:key (error #(build (smooth-plot noisy {:method :nope}))))))
      (is (= :method-params (:key (error #(build (smooth-plot noisy {:method-params {:degree 3}}))))) "loess degree 1 or 2")
      (is (= :method-params (:key (error #(build (smooth-plot noisy {:method :gp :method-params {:degree 2}}))))))
      (is (= :method-params (:key (error #(build (smooth-plot noisy {:method :lm :method-params {:degree 0}}))))))
      (is (= :method-params (:key (error #(build (smooth-plot noisy {:method :lm :method-params {:degree 1.5}})))))))))

(deftest smooth-bands-test
  (let [xs (mapv :x noisy) ys (mapv :y noisy)]
    (testing "3.3.1 default band per method; :confidence without own errors throws"
      (is (= :confidence (smooth/default-band :loess)))
      (is (= :none (smooth/default-band :akima)))
      (is (= :band (:key (error #(build (smooth-plot noisy {:method :akima :band :confidence})))))))
    (testing "3.3.2 lm confidence band equals fastmath's interval"
      (let [{:keys [x bands]} (curve-of (smooth-plot noisy {:method :lm :level 0.9}))
            model (regression/lm ys (mapv vector xs) {:alpha 0.1})
            ci (map #(:confidence-interval (regression/predict model [%] true)) x)]
        (is (close? 1e-9 (map first ci) (:lo (first bands))))
        (is (close? 1e-9 (map second ci) (:hi (first bands))))))
    (testing "3.3.2 loess band: fit ± t · stderr"
      (let [{:keys [x y bands]} (curve-of (smooth-plot noisy {}))
            f (interpolation/loess xs ys smooth/loess-defaults)
            t (r/icdf (r/distribution :t {:degrees-of-freedom (:df f)}) 0.975)]
        (is (close? 1e-9 (map (fn [x fit] (- (double fit) (* t (double (second (f x true)))))) x y) (:lo (first bands))))))
    (testing "3.3.2 gp band: mean ± z · σ"
      (let [{:keys [x bands]} (curve-of (smooth-plot noisy {:method :gp :method-params {:noise 0.1}}))
            model (gp/gaussian-process xs ys {:noise 0.1})
            z (r/icdf (r/distribution :normal) 0.975)]
        (is (close? 1e-9 (map #(let [[m s] (gp/predict model % true)] (+ (double m) (* z (double s)))) x) (:hi (first bands))))))
    (testing "3.3.3 prediction: lm only, wider than confidence"
      (let [width (fn [band] (let [{:keys [lo hi]} (first (:bands (curve-of (smooth-plot noisy {:method :lm :band band}))))]
                               (- (double (first hi)) (double (first lo)))))]
        (is (> (double (width :prediction)) (double (width :confidence)))))
      (is (= :band (:key (error #(build (smooth-plot noisy {:band :prediction})))))))
    (testing "3.3.4 bootstrap: any method, reproducible with a seeded :rng"
      (let [boot #(:bands (curve-of (smooth-plot noisy {:method %1 :band :bootstrap :bootstrap {:samples 30 :rng (r/rng :mersenne 5) :ci %2}})))]
        (is (= (boot :cubic-smoothing :percentile) (boot :cubic-smoothing :percentile)))
        (is (= 1 (count (boot :lm :basic))))
        (is (not= (boot :lm :percentile) (boot :lm :basic)))))
    (testing "3.3.5 level vectors: nested, widest first; out of (0, 1) throws"
      (is (= [0.95 0.8 0.5] (map :level (:bands (curve-of (smooth-plot noisy {:level [0.5 0.95 0.8]})))))))
    (are [level] (= :level (:key (error #(build (smooth-plot noisy {:level level})))))
      0 1 1.0 -0.5 [] [0.5 1.0] "0.9")
    (testing "3.3.6 loess band undefined outside the data; the line goes on"
      (let [{:keys [y bands]} (curve-of (smooth-plot noisy {:full-range true} {:scales {:x {:domain [-10 40]}}}))]
        (is (every? #(Double/isFinite %) y))
        (is (Double/isNaN (first (:lo (first bands)))))
        (is (some #(Double/isFinite %) (:lo (first bands))))))
    (testing "3.3.7 :band :none"
      (is (= [] (:bands (curve-of (smooth-plot noisy {:band :none}))))))
    (testing "bootstrap options are checked"
      (is (= [:charts 0 :options :bootstrap :samples] (:in (error #(build (smooth-plot noisy {:band :bootstrap :bootstrap {:samples 0}}))))))
      (is (= [:charts 0 :options :bootstrap :ci] (:in (error #(build (smooth-plot noisy {:band :bootstrap :bootstrap {:ci :bca}})))))))))

(deftest smooth-grid-test
  (testing "3.4.1 :n points from the group's min to max"
    (let [{:keys [x]} (curve-of (smooth-plot noisy {}))]
      (is (= 200 (count x)))
      (is (= [0.0 29.0] [(first x) (peek x)])))
    (is (= [0.0 29.0] (:x (curve-of (smooth-plot noisy {:n 2})))))
    (is (= :n (:key (error #(build (smooth-plot noisy {:n 1})))))))
  (testing "3.4.2 :full-range: the plot's x domain, else the union of the groups"
    (let [two (into (mapv #(assoc % :g "a") (take 15 noisy)) (map #(assoc % :g "b") (drop 10 noisy)))
          xs (fn [p] (map (juxt (comp first :x) (comp peek :x)) (:curves (panel-data p))))
          grouped {:mapping {:x :x :y :y :color :g}}]
      (is (= [[0.0 14.0] [10.0 29.0]] (xs (smooth-plot two {:method :lm} grouped))))
      (is (= [[0.0 29.0] [0.0 29.0]] (xs (smooth-plot two {:method :lm :full-range true} grouped))))
      (is (= [[-5.0 35.0] [-5.0 35.0]] (xs (smooth-plot two {:method :lm :full-range true} (assoc grouped :scales {:x {:domain [-5 35]}})))))))
  (testing "3.4.3 temporal x: LocalDateTime grid, LocalDate extents; renders with LocalDate points"
    (let [dated (mapv #(update % :x (fn [x] (.plusDays (LocalDate/of 2020 1 1) x))) noisy)
          p (merge (smooth-plot dated {}) {:charts [(chart/scatter) (chart/smooth)]})
          s (get-in (build p) [:stats 1])]
      (is (every? #(instance? LocalDateTime %) (get-in s [:data facet/single-panel :curves 0 :x])))
      (is (= (LocalDate/of 2020 1 1) (get-in s [:extents facet/single-panel :x :min])))
      (is (instance? BufferedImage (plot/render-image p size))))))

(deftest smooth-extents-draw-test
  (testing "3.5.3 y extent covers the band"
    (let [s (stats (smooth-plot noisy {:method :lm}))
          lo (reduce min (:lo (first (get-in s [:data facet/single-panel :curves 0 :bands]))))]
      (is (= lo (get-in s [:extents facet/single-panel :y :min])))))
  (testing "3.5 grouped, nested, flipped, no band, :fill - render"
    (are [opts extra] (instance? BufferedImage (plot/render-image (smooth-plot (into (mapv #(assoc % :g "a") noisy)
                                                                                     (map #(assoc % :g "b" :y (+ 2 (:y %))) noisy))
                                                                               opts (merge {:mapping {:x :x :y :y :color :g}} extra))
                                                                  size))
      {:level [0.5 0.95]} {}
      {:method :lm} {:flip true}
      {:band :none} {}
      {:fill [200 0 0 60] :dash :dashed :width 2} {})))

;; review fixes

(deftest review-bandwidth-test
  (testing "J1 loess :bandwidth outside (0, 1] or not a number throws instead of hanging"
    (are [bw] (= :method-params (:key (error #(build (smooth-plot noisy {:method-params {:bandwidth bw}})))))
      0 -1 0.0 1.5 :x nil)
    (is (some? (curve-of (smooth-plot noisy {:method-params {:bandwidth 1.0}}))) "1 is allowed")
    (is (= 200 (smooth/required-points :loess {:bandwidth 0.01})))
    (is (= 0 (get-in (stats (smooth-plot noisy {:method :lm :method-params {:bandwidth 0}})) [:dropped :group]))
        ":bandwidth is a loess parameter only")))

(deftest review-rng-test
  (testing "J3 for-build: a copy in the current state, the option not advanced"
    (let [g (r/rng :mersenne 9)
          _ (r/drandom g)
          c (rng/for-build g)]
      (is (not (identical? g c)))
      (is (= (r/drandom c) (r/drandom g)) "the same next number")
      (is (some? (rng/for-build nil)))
      (let [synced (r/synced-rng :mersenne 1)] (is (identical? synced (rng/for-build synced)) "not copyable: itself"))))
  (testing "J3 every build of a plot with a seeded :rng draws the same numbers"
    (let [boot {:method :cubic-smoothing :band :bootstrap :bootstrap {:samples 20 :rng (r/rng :mersenne 3)}}
          p (smooth-plot noisy boot)]
      (is (= (:bands (curve-of p)) (:bands (curve-of p)))))
    (let [strip-data (mapv (fn [i] {:x (double i) :y (if (even? i) "a" "b")}) (range 20))
          strip (fn [c] {:data strip-data :mapping {:x :x :y :y} :charts [c]})
          items #(get-in (stats %) [:data facet/single-panel :items])
          g (r/rng :mersenne 4)
          p (strip (chart/stripplot {:rng g}))]
      (is (= (map :offsets (items p)) (map :offsets (items p))) "stripplot")
      (is (= (r/drandom g) (r/drandom (r/rng :mersenne 4))) "the option untouched")
      (let [e (strip (chart/extent {:method :bootstrap-percentile :rng (r/rng :mersenne 5)}))]
        (is (= (map :lo (items e)) (map :lo (items e))) "extent")))))

(deftest review-levels-fill-test
  (testing "J6 repeated levels drawn once"
    (is (= [0.95 0.5] (map :level (:bands (curve-of (smooth-plot noisy {:level [0.5 0.95 0.5 0.95]})))))))
  (testing "J7 :fill :none: no band computed, no extent from it"
    (let [s (stats (smooth-plot noisy {:method :lm :fill :none}))]
      (is (= [] (get-in s [:data facet/single-panel :curves 0 :bands])))
      (is (= (reduce min (get-in s [:data facet/single-panel :curves 0 :y])) (get-in s [:extents facet/single-panel :y :min]))))
    (is (= [] (:bands (curve-of (smooth-plot noisy {:method :lm} {:theme {:chart {:smooth {:fill :none}}}})))) "theme :fill :none")))

(deftest review-edges-test
  (testing "J8 lm with degree + 1 points has no residual df: dropped; degree + 2 fitted"
    (is (= 1 (get-in (stats (smooth-plot (take 3 line-data) {:method :lm :method-params {:degree 2}})) [:dropped :group])))
    (is (= 1 (get-in (stats (smooth-plot (take 2 line-data) {:method :glm})) [:dropped :group])))
    (let [{:keys [y bands]} (curve-of (smooth-plot (take 4 line-data) {:method :lm :method-params {:degree 2}}))]
      (is (close? 1e-9 [1 7] [(first y) (peek y)]))
      (is (every? #(Double/isFinite %) (:lo (first bands)))))
    (is (instance? BufferedImage (plot/render-image (merge (smooth-plot (take 2 line-data) {:method :lm}) {:charts [(chart/scatter) (chart/smooth {:method :lm})]}) size))
        "everything dropped: the plot still renders"))
  (testing "J4 shared models keep the bands: lm at 0.95 and gp"
    (let [xs (mapv :x noisy) ys (mapv :y noisy)
          {:keys [x bands]} (curve-of (smooth-plot noisy {:method :lm :level [0.95 0.5]}))
          model (regression/lm ys (mapv vector xs))]
      (is (close? 1e-9 (map #(first (:confidence-interval (regression/predict model [%] true))) x) (:lo (first bands)))))))

(defn- legend-image
  "Image of the `legend` glyph of `chart` with `attributes` in a 20 x 20 white box."
  [legend chart attributes]
  (let [canvas (c2d/canvas 20 20)]
    (c2d/with-canvas-> canvas (c2d/set-background :white))
    (c2d/with-canvas [c canvas] (legend chart attributes c {:x 0 :y 0 :w 20 :h 20} {:theme theme/default-theme}))
    (c2d/get-image canvas)))

(defn- near? [a b] (every? (fn [[p q]] (<= (Math/abs (- (long p) (long q))) 2)) (map vector a b)))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(deftest review-legend-test
  (testing "J8 smooth glyph: band square under the line; none without a band"
    (let [img (legend-image smooth-chart/legend (chart/smooth) {:color [255 0 0]})]
      (is (near? [255 179 179] (rgb img 6 6)) "band at alpha 0.3")
      (is (= 255 (first (rgb img 10 10))))
      (is (> 100 (second (rgb img 10 10))) "the line"))
    (is (= [255 255 255] (rgb (legend-image smooth-chart/legend (chart/smooth {:band :none}) {:color [255 0 0]}) 6 6))))
  (testing "J8 ribbon glyph: band square, edge only with :stroke-color"
    (let [img (legend-image ribbon/legend (chart/ribbon) {:color [255 0 0]})]
      (is (near? [255 179 179] (rgb img 10 10)))
      (is (near? [255 179 179] (rgb img 3 10)) "no edge"))
    (is (= [0 0 0] (rgb (legend-image ribbon/legend (chart/ribbon {:stroke-color [0 0 0]}) {:color [255 0 0]}) 3 10)))))
