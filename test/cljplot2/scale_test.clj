(ns cljplot2.scale-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [wadogo.scale :as s]
            [cljplot2.core :as plot]
            [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- build
  ([scales] (build scales {}))
  ([scales extra] (plot/build (merge {:scales scales} extra) {:width 600 :height 400})))

(defn- finalized [scales k] (get-in (build scales) [:finalized-scales k]))
(defn- track-size [built axis id] (:size (first (filter #(= id (:id %)) (get-in built [:layout axis])))))

;; 1.x phase 1

(deftest spec-inference-test
  (are [domain kind] (= kind (:kind (scale/scale-spec :x {:domain domain})))
    [0 1] :linear
    [0.5 2] :linear
    ["a" "b"] :bands
    [:a :b :c] :bands
    ["a" :b] :bands
    [(LocalDate/of 2020 1 1) (LocalDate/of 2021 1 1)] :datetime)
  (is (= :log (:kind (scale/scale-spec :x {:kind :log :domain [1 100]}))))
  (testing "nice defaults to true, explicit false kept"
    (is (true? (:nice (scale/scale-spec :x {:domain [0 1]}))))
    (is (false? (:nice (scale/scale-spec :x {:domain [0 1] :nice false}))))))

(deftest spec-validation-test
  (testing "domain"
    (are [domain] (= {:key :domain :scale :x} (error-data #(scale/scale-spec :x {:domain domain})))
      nil [] "abc" [1 "a"] [5 5] [3] [0 ##NaN] [0 ##Inf] ["a" "a"] [1 (LocalDate/of 2020 1 1)])
    (is (= {:key :domain :scale :x} (error-data #(scale/scale-spec :x {:kind :log :domain [0 10]}))))
    (is (= {:key :domain :scale :x} (error-data #(scale/scale-spec :x {:kind :log :domain [-1 10]}))))
    (is (= {:key :domain :scale :x}
           (error-data #(scale/scale-spec :x {:domain [(LocalDate/of 2020 1 1) (LocalDate/of 2020 1 1)]})))))
  (testing "kind"
    (are [kind] (= :kind (:key (error-data #(scale/scale-spec :x {:kind kind :domain [0 1]}))))
      :ordinal :quantile :unknown))
  (testing "ticks"
    (are [ticks] (= :ticks (:key (error-data #(scale/scale-spec :x {:domain [0 1] :ticks ticks}))))
      0 -1 2.5 "5")
    (are [ticks] (scale/scale-spec :x {:domain [0 1] :ticks ticks})
      1 5 [0 0.5 1] []))
  (testing "spec shape and plot keys"
    (is (= {:key :scales :scale :x} (error-data #(scale/scale-spec :x [0 1]))))
    (is (= :scales (:key (error-data #(build {:z {:domain [0 1]}})))))
    (is (= :scales (:key (error-data #(plot/build {:scales [0 1]})))))
    (is (= {} (:scale-specs (plot/build {}))))
    (is (= {} (:scale-specs (plot/build {:scales {}}))))))

(deftest specs-are-data-test
  (is (= (:scale-specs (build {:x {:domain [0 10]} :y {:domain ["a" "b"]}}))
         (:scale-specs (build {:x {:domain [0 10]} :y {:domain ["a" "b"]}}))))
  (is (= (:finalized-scales (build {:x {:domain [0 10]} :y {:domain ["a" "b"]}}))
         (:finalized-scales (build {:x {:domain [0 10]} :y {:domain ["a" "b"]}})))))

;; 2.x phase 2

(deftest range-test
  (let [built (build {:x {:domain [0 10]} :y {:domain [0 10]}})
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)
        fx (get-in built [:finalized-scales :x])
        fy (get-in built [:finalized-scales :y])]
    (is (= [(double x) (double (+ x w))] (:range fx)))
    (is (= [(double (+ y h)) (double y)] (:range fy)) "y grows upward")
    (is (== x (scale/forward fx 0)))
    (is (== (+ x w) (scale/forward fx 10)))
    (is (== (+ y h) (scale/forward fy 0)))
    (is (== y (scale/forward fy 10))))
  (testing "reversed domain maps its first value to the range start"
    (let [fx (finalized {:x {:domain [10 0]}} :x)]
      (is (== (first (:range fx)) (scale/forward fx 10))))))

(deftest forward-oracle-test
  (let [fx (finalized {:x {:domain [0.13 7.31]}} :x)
        oracle (s/scale :linear {:domain (:domain fx) :range (:range fx)})]
    (doseq [v [0 1.5 3.14159 8]]
      (is (== (oracle v) (scale/forward fx v))))))

(deftest tick-hint-test
  (are [len spacing n] (= n (scale/tick-hint len spacing))
    0.0 80.0 2
    1.0 80.0 2
    159.0 80.0 2
    160.0 80.0 2
    240.0 80.0 3
    800.0 80.0 10
    1.0e6 50.0 20000)
  (testing "plot width drives the x tick count"
    (let [n (fn [w] (count (:ticks (get-in (plot/build {:scales {:x {:domain [0 100] :nice false}}} {:width w :height 300})
                                            [:finalized-scales :x]))))]
      (is (< (n 300) (n 1200)))))
  (testing "explicit ticks win"
    (is (= [0.0 5.0 10.0] (mapv double (:ticks (finalized {:x {:domain [0 10] :ticks [0 5 10]}} :x)))))
    (is (= [] (:ticks (finalized {:x {:domain [0 10] :ticks []}} :x))))))

(deftest nice-test
  (let [fx (finalized {:x {:domain [0.13 7.31]}} :x)]
    (is (== (first (:ticks fx)) (first (:domain fx))))
    (is (== (peek (:ticks fx)) (second (:domain fx)))))
  (is (= [0.13 7.31] (:domain (finalized {:x {:domain [0.13 7.31] :nice false}} :x))))
  (testing "bands are never niced"
    (is (= ["a" "b" "c"] (:domain (finalized {:y {:domain ["a" "b" "c"]}} :y))))))

(deftest tick-labels-test
  (are [domain labels] (= labels (:tick-labels (scale/finalize (scale/scale-spec :x {:domain domain :nice false}) [0 500] 5)))
    [0 1e6] ["0" "200000" "400000" "600000" "800000" "1000000"]
    [0 1] ["0.0" "0.2" "0.4" "0.6" "0.8" "1.0"])
  (testing "-0 prints as 0"
    (is (= ["-1" "0" "1"] (:tick-labels (finalized {:x {:domain [-1 1] :ticks [-1 -0.0 1]}} :x)))))
  (testing "custom formatter wins"
    (is (= ["0%" "50%" "100%"]
           (:tick-labels (finalized {:x {:domain [0 100] :ticks [0 50 100] :formatter #(str (long %) "%")}} :x)))))
  (testing "bands, log, datetime"
    (is (= ["a" "b" "c"] (:tick-labels (finalized {:y {:domain ["a" "b" "c"]}} :y))))
    (is (= ["1" "10" "100" "1000"] (:tick-labels (finalized {:x {:kind :log :domain [1 1000]}} :x))))
    (is (every? string? (:tick-labels (finalized {:x {:domain [(LocalDate/of 2020 1 1) (LocalDate/of 2020 12 31)]}} :x))))))

(deftest provisional-finalization-test
  (testing "first round has no layout: provisional range"
    (is (= [0.0 1.0] (:range (scale/finalize (scale/scale-spec :x {:domain [0 1]}) [0.0 1.0] 10)))))
  (testing "layout converges within the cap"
    (is (<= (get-in (build {:x {:domain [0 1e6]} :y {:domain [0 1e6]}}) [:layout :rounds]) 3))))

(deftest custom-grid-scales-test
  (testing "scales need a :panel item"
    (is (= {:key :scales} (error-data #(plot/build {:scales {:x {:domain [0 1]}}
                                                    :custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                                                                  :items [{:id :p :column 0 :row 0}]}}))))))

;; 3.x axes

(defn- tick-label-size
  [text]
  (measure/call-with-scratch-canvas
   :high (fn [cv] (let [v (measure/natural-size {:kind :label :role :tick-label :text text}
                                                 {:canvas cv :theme theme/default-theme})]
                    [(.x v) (.y v)]))))

(deftest axis-size-test
  (testing "no scales: axis tracks empty"
    (let [built (plot/build {} {:width 600 :height 400})]
      (is (= 0 (track-size built :columns :y-axis)))
      (is (= 0 (track-size built :rows :x-axis)))))
  (testing "only x: only the x axis track"
    (let [built (build {:x {:domain [0 1]}})]
      (is (pos? (track-size built :rows :x-axis)))
      (is (= 0 (track-size built :columns :y-axis)))))
  (testing "oracle: y axis width = tick length + gap + widest label"
    (let [built (build {:y {:domain [0 1e6]}})
          labels (get-in built [:finalized-scales :y :tick-labels])
          widest (apply max (map (comp first tick-label-size) labels))]
      (is (= (long (+ 6 2 widest)) (track-size built :columns :y-axis)))))
  (testing "oracle: x axis height = tick length + gap + label height"
    (let [built (build {:x {:domain [0 1]}})]
      (is (= (long (+ 6 2 (second (tick-label-size "0")))) (track-size built :rows :x-axis)))))
  (testing "longer labels widen the y axis"
    (is (< (track-size (build {:y {:domain [0 1]}}) :columns :y-axis)
           (track-size (build {:y {:domain [0 1e6]}}) :columns :y-axis))))
  (testing "tick length override changes the size by the difference"
    (is (= 10 (- (track-size (build {:y {:domain [0 10]}} {:theme {:axis {:y {:tick-length 16}}}}) :columns :y-axis)
                 (track-size (build {:y {:domain [0 10]}}) :columns :y-axis)))))
  (testing "explicit empty ticks: no labels"
    (is (= 8 (track-size (build {:y {:domain [0 10] :ticks []}}) :columns :y-axis)))))

(deftest axis-validation-test
  (is (= {:key :scale :value :z} (error-data #(plot/build {:custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                                                                         :items [{:id :a :column 0 :row 0 :content {:kind :axis :scale :z}}]}}))))
  (are [t k] (= k (:key (error-data #(plot/build {:theme t}))))
    {:axis {:x {:tick-length -1}}} :tick-length
    {:axis {:y {:label-gap "2"}}} :label-gap
    {:axis {:x {:tick-spacing 0}}} :tick-spacing
    {:axis {:x :none}} :axis
    {:panel {:grid {:x {:width -1}}}} :width
    {:panel {:grid {:y {:dash [0]}}}} :dash
    {:panel {:grid {:y {:dash []}}}} :dash
    {:panel {:grid {:y {:dash nil}}}} nil
    {:panel {:grid {:x [1 2]}}} :grid))

;; 3.x / 4.x drawing

(defn- pixel [canvas x y]
  (let [rgb (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right rgb 16) 0xff) (bit-and (bit-shift-right rgb 8) 0xff) (bit-and rgb 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(defn- render [scales theme] (plot/render {:scales scales :theme theme} {:width 600 :height 400}))

(defn- dark? [[r g b]] (< (+ r g b) 200))

(deftest tick-ink-test
  (let [scales {:x {:domain [0 10] :ticks [0 5 10]} :y {:domain [0 10] :ticks [0 5 10]}}
        built (plot/build {:scales scales :theme plain} {:width 600 :height 400})
        c (render scales plain)
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)
        fx (get-in built [:finalized-scales :x])
        fy (get-in built [:finalized-scales :y])]
    (testing "x ticks below the panel at forward(tick) +/-1 px"
      (doseq [t [0 5 10] :let [px (long (scale/forward fx t))]]
        (is (some #(dark? (pixel c % (+ y h 3))) (range (dec px) (+ px 2))) (str "x tick " t))))
    (testing "y ticks left of the panel"
      (doseq [t [0 5 10] :let [py (long (scale/forward fy t))]]
        (is (some #(dark? (pixel c (- x 3) %)) (range (dec py) (+ py 2))) (str "y tick " t))))
    (testing "no tick ink between ticks"
      (is (not (dark? (pixel c (long (scale/forward fx 2.5)) (+ y h 3))))))
    (testing "axis lines on the panel edges"
      (is (dark? (pixel c (+ x (quot w 3)) (+ y h))))
      (is (dark? (pixel c x (+ y (quot h 3))))))
    (testing ":none colors draw no line or ticks"
      (let [c2 (render scales (assoc plain :axis {:x {:line-color :none :tick-color :none}}))]
        (is (not (dark? (pixel c2 (+ x (quot w 3)) (+ y h)))))
        (is (not (dark? (pixel c2 (long (scale/forward fx 5)) (+ y h 3)))))))))

(deftest grid-lines-test
  (let [scales {:x {:domain [0 10] :ticks [0 5 10]} :y {:domain [0 10] :ticks [0 5 10]}}
        grid-theme {:plot {:background :white} :panel {:grid {:x {:color :red :width 1 :dash :none}
                                                              :y {:color :blue :width 1 :dash :none}}}}
        built (plot/build {:scales scales :theme grid-theme} {:width 600 :height 400})
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)
        px (long (scale/forward (get-in built [:finalized-scales :x]) 5))
        py (long (scale/forward (get-in built [:finalized-scales :y]) 5))
        ;; antialiased 1 px lines at fractional positions: dominance, not pure color
        reddish? (fn [[r g b]] (and (> r (+ g 60)) (> r (+ b 60))))
        bluish? (fn [[r g b]] (and (> b (+ r 60)) (> b (+ g 60))))]
    (testing "vertical lines at x ticks, horizontal at y ticks"
      (let [c (render scales grid-theme)]
        (is (some #(reddish? (pixel c % (+ y (quot h 3)))) (range (dec px) (+ px 2))))
        (is (some #(bluish? (pixel c (+ x (quot w 3)) %)) (range (dec py) (+ py 2))))))
    (testing ":none per axis"
      (let [c (render scales (assoc-in grid-theme [:panel :grid :x] :none))]
        (is (not-any? #(reddish? (pixel c % (+ y (quot h 3)))) (range (dec px) (+ px 2))))
        (is (some #(bluish? (pixel c (+ x (quot w 3)) %)) (range (dec py) (+ py 2))))))
    (testing "default grid lines are on (dashed gray)"
      (let [c (plot/render {:scales scales :theme {:plot {:background :white}}} {:width 600 :height 400})]
        (is (some (fn [yy] (some #(let [[r g b] (pixel c % yy)] (and (= r g b) (< 120 r 250))) (range (dec px) (+ px 2))))
                  (range (+ y 2) (+ y 40))))))))

(deftest bands-test
  (let [built (build {:y {:domain ["a" "b" "c"]}})
        fy (get-in built [:finalized-scales :y])
        {:keys [y h]} (grid/item-box (:layout built) :panel)
        points (map #(scale/forward fy %) ["a" "b" "c"])]
    (is (= ["a" "b" "c"] (:ticks fy)))
    (is (every? #(< y % (+ y h)) points) "band centers inside the panel")
    (is (apply > points) "first band at the bottom (y grows upward)"))
  (testing "single band"
    (let [fy (finalized {:y {:domain ["only"]}} :y)]
      (is (= ["only"] (:tick-labels fy))))))
