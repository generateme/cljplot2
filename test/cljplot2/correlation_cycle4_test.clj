(ns cljplot2.correlation-cycle4-test
  "Correlation cycle 4: size scale `:area`, label `:background`, scatter `:jitter`."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int (m/round (double x))) (int (m/round (double y))))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- count-color [^BufferedImage img color]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (= color (rgb img x y))] 1)))

(def ^:private plain {:plot {:background :white} :panel {:fill :white :alternate-fill :none :grid {:x :none :y :none}}})

(defn- image [p size] (plot/render-image (update p :theme #(merge plain %)) size))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(def ^:private palette (:palette theme/default-theme))

(defn- size-of [spec v]
  ((aesthetic/attribute-fn :size (aesthetic/scale-spec :size spec nil false palette) 1.0) v))

;; 1. area sizes

(deftest area-size-test
  (testing "1.1 linear: area linear in t; default unchanged"
    (is (m/delta-eq 5.0 (size-of {:area true :range [0 10] :domain [0 1]} 0.25)))
    (is (m/delta-eq 2.5 (size-of {:range [0 10] :domain [0 1]} 0.25)))
    (are [v s] (m/delta-eq s (size-of {:area true :range [2 10] :domain [0 1]} v))
      0 2.0
      1 10.0
      0.5 (m/sqrt 52.0)))
  (testing "1.1 :reverse and :area together"
    (is (m/delta-eq 10.0 (size-of {:area true :reverse true :range [0 10] :domain [0 1]} 0)))
    (is (m/delta-eq (m/sqrt 75.0) (size-of {:area true :reverse true :range [0 10] :domain [0 1]} 0.25))))
  (testing "1.1 ordinal levels evenly in area; one level at the middle"
    (is (every? true? (map m/delta-eq [0.0 (m/sqrt 50.0) 10.0]
                           (map #(size-of {:area true :range [0 10] :domain ["a" "b" "c"]} %) ["a" "b" "c"]))))
    (is (= [0.0 5.0 10.0] (mapv #(size-of {:range [0 10] :domain ["a" "b" "c"]} %) ["a" "b" "c"])) "linear unchanged")
    (is (m/delta-eq (m/sqrt 50.0) (size-of {:area true :range [0 10] :domain ["a"]} "a"))))
  (testing "1.1 outside the domain and NA as before"
    (is (nil? (size-of {:area true :range [0 10] :domain [0 1]} 2)))
    (is (= 1.0 (size-of {:area true :range [0 10] :domain [0 1]} nil))))
  (testing "1.2 :area only on linear / ordinal size scales"
    (is (= :area (:key (error #(aesthetic/scale-spec :color {:area true :domain [0 1]} nil false palette)))))
    (is (= :area (:key (error #(aesthetic/scale-spec :size {:area true :kind :identity} {:kind :numeric} false palette)))))
    (are [k spec] (= [:scales k :area] (:in (error #(plot/build {:data [{:x 1 :y 1 :v 1} {:x 2 :y 2 :v 2}]
                                                                  :mapping {:x :x :y :y k :v} :charts [(chart/scatter)]
                                                                  :scales {k spec}} {:width 200 :height 200}))))
      :color {:area true :kind :linear}
      :shape {:area true}
      :color {:area true :kind :binned}
      :size {:area true :kind :identity}
      :size {:area true :domain ["a" "b"] :range {"a" 2 "b" 5}}))
  (testing "F2 :area with a level map :range rejected, like :reverse"
    (is (= :area (:key (error #(aesthetic/scale-spec :size {:area true :domain ["a" "b"] :range {"a" 2 "b" 5}} nil false palette))))))
  (testing "1.3 legend keys follow the area sizes"
    (let [p (fn [area?] {:data [{:x 1 :y 1 :s "a"} {:x 2 :y 2 :s "b"} {:x 3 :y 3 :s "c"}] :mapping {:x :x :y :y :size :s}
                         :charts [(chart/scatter {:color [255 0 0] :stroke-width 0})] :legend-only true
                         :scales {:size (cond-> {:range [0 20]} area? (assoc :area true))}})
          linear (count-color (image (p false) {:width 200 :height 200}) [255 0 0])
          area (count-color (image (p true) {:width 200 :height 200}) [255 0 0])]
      (is (pos? linear))
      (is (> area linear) "the middle key is larger in area"))))

;; 2. label background

(defn- label-image
  "A 200×200 white image with label `content` drawn centred."
  [content]
  (let [canvas (c2d/canvas 200 200)]
    (c2d/with-canvas-> canvas (c2d/set-background :white))
    (c2d/with-canvas [c canvas] (label/draw-at! c content [:center :middle] 100 100 {:theme theme/default-theme}))
    (c2d/get-image canvas)))

(defn- frame-corner
  "Pixel 1.5 px inside the top-left corner of the label frame `[w h]` centred at (100, 100), rotated `deg` clockwise."
  [[w h] deg]
  (let [a (m/radians (double deg))
        x (- 1.5 (* 0.5 (double w))) y (- 1.5 (* 0.5 (double h)))]
    [(+ 100.0 (- (* x (m/cos a)) (* y (m/sin a)))) (+ 100.0 (* x (m/sin a)) (* y (m/cos a)))]))

(deftest background-test
  (let [content {:kind :label :role :annotation :text "Label" :inset 6 :background [255 0 0]}
        frame (let [canvas (c2d/canvas 10 10)]
                (c2d/with-canvas [c canvas] (label/text-frame content c theme/default-theme)))]
    (testing "2.1 the frame filled at its padding corner, rotated with the text"
      (are [deg] (= [255 0 0] (apply rgb (label-image (assoc content :rotation deg)) (frame-corner frame deg)))
        0 90 30 180 270 -30))
    (testing "2.1 alpha blends with what is below"
      (let [[r g b] (apply rgb (label-image (assoc content :background [255 0 0 128])) (frame-corner frame 0))]
        (is (= 255 r))
        (is (< 100 g 160))
        (is (= g b))))
    (testing "2.1 no background, :none: nothing filled"
      (is (zero? (count-color (label-image (dissoc content :background)) [255 0 0])))
      (is (zero? (count-color (label-image (assoc content :background :none)) [255 0 0]))))
    (testing "2.1 empty text draws nothing"
      (is (zero? (count-color (label-image (assoc content :text "")) [255 0 0]))))
    (testing "2.1 schema: a background is a color - text chart options, theme roles"
      (is (= :background (last (:in (error #(stats {:charts [(chart/text {:x 0 :y 0 :text "a" :background :no-such-color})]}))))))
      (is (= [:theme :text :title :background]
             (:in (error #(stats {:data [{:x 0 :y 0}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                  :theme {:text {:title {:background :no-such-color}}}}))))))))

(deftest text-chart-background-test
  (let [p (fn [opts] {:charts [(chart/text (merge {:x 0.5 :y 0.5 :text "M"} opts))]
                      :scales {:x {:domain [0 1]} :y {:domain [0 1]}}})]
    (testing "2.2 text chart :background and :padding: a box around the label"
      (let [small (count-color (image (p {:background [255 0 0]}) {:width 200 :height 200}) [255 0 0])
            padded (count-color (image (p {:background [255 0 0] :inset 8}) {:width 200 :height 200}) [255 0 0])]
        (is (pos? small))
        (is (> padded small) ":padding grows the box"))
      (is (zero? (count-color (image (p {}) {:width 200 :height 200}) [255 0 0]))))
    (testing "2.2 end labels (:select) take the background"
      (is (pos? (count-color (image {:data [{:x 0 :y 0 :g "a"} {:x 1 :y 1 :g "a"}] :mapping {:x :x :y :y}
                                     :charts [(chart/line) (chart/text {:mapping {:x :x :y :y :label :g} :select :last
                                                                        :background [255 0 0]})]}
                                    {:width 300 :height 200})
                             [255 0 0]))))
    (testing "F7 legend key: background kept, the role's padding instead of the chart's"
      (let [key-image (fn [opts]
                        (let [canvas (c2d/canvas 40 40)]
                          (c2d/with-canvas-> canvas (c2d/set-background :white))
                          (c2d/with-canvas [c canvas]
                            (let [ch (chart/text (merge {:x 0 :y 0 :text "a"} opts))]
                              ((:legend ch) ch {} c {:x 0 :y 0 :w 40 :h 40} {:theme theme/default-theme})))
                          (c2d/get-image canvas)))
            n (count-color (key-image {:background [255 0 0]}) [255 0 0])]
        (is (pos? n))
        (is (= n (count-color (key-image {:background [255 0 0] :inset 30}) [255 0 0])) "a large :inset doesn't grow the key")))
    (testing "2.3 other label roles via the theme"
      (let [p (fn [t] {:data [{:x 0 :y 0 :v 1} {:x 1 :y 0 :v 2}] :mapping {:x :x :y :y :color :v}
                       :charts [(chart/tile {:value-label true})] :legend false :theme t})]
        (is (pos? (count-color (image (p {:text {:tile-value {:background [255 0 0]}}}) {:width 200 :height 200}) [255 0 0])))
        (is (zero? (count-color (image (p {}) {:width 200 :height 200}) [255 0 0])))))))

;; 3. scatter jitter

(def ^:private lattice (vec (for [x (range 3) y (range 3) _ (range 10)] {:x x :y y})))

(defn- points [p] (get-in (stats p) [:data :panel]))

(defn- jittered [opts] {:data lattice :mapping {:x :x :y :y} :charts [(chart/scatter opts)]})

(deftest jitter-test
  (testing "3.1 offsets uniform within ±d"
    (let [{:keys [x y]} (points (jittered {:jitter [0.2 0.1] :rng (r/rng :mersenne 1)}))
          dx (map #(- (double %1) (double (:x %2))) x lattice)
          dy (map #(- (double %1) (double (:y %2))) y lattice)]
      (is (= (count lattice) (count x)))
      (is (every? #(<= -0.2 (double %) 0.2) dx))
      (is (every? #(<= -0.1 (double %) 0.1) dy))
      (is (> (apply max dx) 0.1) "spread over the interval, not clustered")
      (is (< (apply min dx) -0.1))))
  (testing "3.1 one :rng - every build the same points; none - builds differ"
    (let [p (jittered {:jitter [0.2 0.2] :rng (r/rng :mersenne 1)})]
      (is (= (points p) (points p))))
    (let [p (jittered {:jitter [0.2 0.2]})]
      (is (not= (:x (points p)) (:x (points p))))))
  (testing "3.1 [0 0] and one zero dimension leave values as they are"
    (is (= (points (jittered {})) (points (jittered {:jitter [0 0] :rng (r/rng :mersenne 1)}))))
    (is (= (mapv :y lattice) (:y (points (jittered {:jitter [0.3 0]}))))))
  (testing "3.1 schema: two non-negative numbers"
    (are [j] (= [:charts 0 :options :jitter] (take 4 (:in (error #(stats (jittered {:jitter j}))))))
      [-0.1 0] [0.1] 0.1))
  (testing "3.1 a positive jitter needs numeric positions; zero is fine"
    (let [p (fn [j] {:data [{:x "a" :y 1} {:x "b" :y 2}] :mapping {:x :x :y :y} :charts [(chart/scatter {:jitter j})]})]
      (is (= :jitter (:key (error #(stats (p [0.2 0]))))))
      (is (= 2 (count (:y (points (p [0 0.2]))))))))
  (testing "3.2 the extents cover the jittered points"
    (let [s (stats (jittered {:jitter [0.4 0.4] :rng (r/rng :mersenne 2)}))
          {:keys [x]} (get-in s [:data :panel])]
      (is (= [(apply min x) (apply max x)] ((juxt :min :max) (get-in s [:extents :panel :x]))))
      (is (< (double (apply min x)) 0.0))))
  (testing "3.3 on a :log axis in decades"
    (let [data (vec (for [v [10 100 1000] _ (range 20)] {:x v :y 1}))
          s (stats {:data data :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                    :charts [(chart/scatter {:jitter [0.5 0] :rng (r/rng :mersenne 3)})]})
          d (map #(- (m/log10 (double %1)) (m/log10 (double (:x %2)))) (get-in s [:data :panel :x]) data)]
      (is (every? #(<= -0.5 (double %) 0.5) d))
      (is (> (apply max d) 0.25) "offsets in decades, not data units")))
  (testing "F1 a non-positive value on :log stays, the scale reports it"
    (are [data] (= :domain (:key (error #(stats {:data data :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                                                  :charts [(chart/scatter {:jitter [0.3 0] :rng (r/rng :mersenne 1)})]}))))
      [{:x 0 :y 1} {:x 10 :y 2}]
      [{:x 10 :y 2} {:x -1 :y 1}])
    (is (re-find #"positive" (try (stats {:data [{:x 0 :y 1} {:x 10 :y 2}] :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                                          :charts [(chart/scatter {:jitter [0.3 0]})]})
                                  "" (catch clojure.lang.ExceptionInfo e (ex-message e))))))
  (testing "F4 categorical positions: the error names chart/stripplot"
    (is (re-find #"stripplot" (try (stats {:data [{:x "a" :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter {:jitter [0.2 0]})]})
                                   "" (catch clojure.lang.ExceptionInfo e (ex-message e))))))
  (testing "F8 flipped and polar plots: x jittered in its own scale's coordinates"
    (let [data (vec (for [v [10 100 1000] _ (range 20)] {:x v :y 1}))
          decades (fn [p] (map #(- (m/log10 (double %1)) (m/log10 (double (:x %2)))) (get-in (stats p) [:data :panel :x]) data))
          p (fn [extra] (merge {:data data :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                                :charts [(chart/scatter {:jitter [0.5 0] :rng (r/rng :mersenne 3)})]} extra))]
      (doseq [extra [{:flip true} {:coord {:kind :polar}}]
              :let [d (decades (p extra))]]
        (is (every? #(<= -0.5 (double %) 0.5) d) (pr-str extra))
        (is (> (apply max d) 0.25) (pr-str extra))
        (is (= (repeat (count data) 1) (get-in (stats (p extra)) [:data :panel :y])) (pr-str extra)))
      (is (instance? BufferedImage (plot/render-image (p {:flip true}) {:width 200 :height 200})))
      (is (instance? BufferedImage (plot/render-image (p {:coord {:kind :polar}}) {:width 200 :height 200})))))
  (testing "3.4 invalid positions dropped and counted, not jittered"
    (let [s (stats {:data (conj lattice {:x nil :y 1} {:x 1 :y ##NaN}) :mapping {:x :x :y :y}
                    :charts [(chart/scatter {:jitter [0.2 0.2] :rng (r/rng :mersenne 1)})]})]
      (is (= (count lattice) (count (get-in s [:data :panel :x]))))
      (is (= {:x 1 :y 1} (:dropped s)))))
  (testing "3.1 empty data with jitter"
    (is (empty? (:x (points {:data [] :mapping {:x :x :y :y} :charts [(chart/scatter {:jitter [0.2 0.2]})]
                             :scales {:x {:domain [0 1]} :y {:domain [0 1]}}}))))))
