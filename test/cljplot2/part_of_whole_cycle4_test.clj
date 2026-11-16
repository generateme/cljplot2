(ns cljplot2.part-of-whole-cycle4-test
  "Part of a whole cycle 4: bar label content (`:value-label` functions) and placement (`:label-position`, outer pie
  labels, collision and fit skips, leaders)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.strip :as strip]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private shares [{:part "alpha" :n 45} {:part "beta" :n 25} {:part "gamma" :n 18} {:part "delta" :n 12}])
(def ^:private size {:width 500 :height 450})

(defn- pie [bar-options & [extra]]
  (merge {:data shares :mapping {:x :n :color :part} :coord {:kind :polar} :scales {:x {:axis false}} :legend nil
          :theme {:plot {:background :white}}
          :charts [(chart/bar (merge {:position :stack :width 1.0} bar-options))]}
         extra))

(defn- pixels [p] (let [^BufferedImage img (plot/render-image p size)] (vec (.getRGB img 0 0 500 450 nil 0 500))))

(defn- placements
  "End-label placements bar draws for plot `p`."
  [p]
  (let [seen (atom [])]
    (with-redefs [strip/draw-label-placements! (fn [_ ps _] (swap! seen into ps))]
      (plot/render-image p size))
    @seen))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- dark? [[r g b]] (and (< (long r) 90) (< (long g) 90) (< (long b) 90)))

(defn- geometry [p] (get-in (plot/build p size) [:finalized-scales :x :coord]))
(defn- panel-origin [p] (let [b (plot/build p size) {:keys [x y]} (grid/item-box (:layout b) :panel)] [(long x) (long y)]))

(deftest content-test
  (testing "1.1 the function gets value, count, share of the level, level, group"
    (let [seen (atom [])]
      (plot/render-image (pie {:value-label (fn [i] (swap! seen conj i) "x")}) size)
      (is (= 4 (count @seen)))
      (is (m/delta-eq 1.0 (reduce + (map :share @seen)) 1e-12))
      (is (= #{"alpha" "beta" "gamma" "delta"} (set (map #(get-in % [:group :color]) @seen))))
      (is (= {:value 45.0 :n 1 :share 0.45} (select-keys (first (filter #(= "alpha" (get-in % [:group :color])) @seen)) [:value :n :share])))))
  (testing "1.1 text drawn; nil / \"\" - no label"
    (let [none (pixels (pie {}))]
      (is (not= none (pixels (pie {:value-label (constantly "label")}))))
      (is (= none (pixels (pie {:value-label (constantly nil)}))))
      (is (= none (pixels (pie {:value-label (constantly "")}))))))
  (testing "1.1 with :n-label both texts join"
    (is (= ["7 n=1"] (map #(get-in % [:content :text])
                          (placements {:data [{:c "a" :v 1}] :mapping {:x :c :y :v}
                                       :charts [(chart/bar {:value-label (constantly "7") :n-label true :label-stack :horizontal})]})))))
  (testing "1.1 shares within a level (dodged groups)"
    (let [seen (atom [])]
      (plot/render-image {:data [{:c "a" :g "p" :v 1} {:c "a" :g "q" :v 3} {:c "b" :g "p" :v 2}] :mapping {:x :c :y :v :color :g}
                          :charts [(chart/bar {:value-label (fn [i] (swap! seen conj i) nil)})]} size)
      (is (= {["a" "p"] 0.25 ["a" "q"] 0.75 ["b" "p"] 1.0}
             (into {} (map (fn [i] [[(:level i) (get-in i [:group :color])] (:share i)])) @seen)))))
  (testing "1.2 docs"
    (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.bar))) ":share"))))

(deftest placement-test
  (testing "2.1 schema; defaults: centred when stacked, past the end otherwise"
    (is (= :label-position (:key (error #(plot/build (pie {:label-position :left}) size)))))
    (is (= :label-leader (:key (error #(plot/build (pie {:label-leader 1}) size)))))
    (is (every? #(zero? (double (get-in % [:label :side]))) (placements (pie {:value-label true}))))
    (is (every? #(= 1.0 (double (get-in % [:label :side])))
                (placements {:data shares :mapping {:x :part :y :n} :charts [(chart/bar {:value-label true})]}))))
  (testing "2.2 :outer - cartesian plots throw"
    (is (= :label-position (:key (error #(plot/build {:data shares :mapping {:x :part :y :n}
                                                      :charts [(chart/bar {:value-label true :label-position :outer})]} size))))))
  (testing "2.2 :outer labels past the outer radius at the slice's middle angle, anchored outward"
    (let [p (pie {:value-label (constantly "WWWW") :label-position :outer})
          {:keys [cx cy radius]} (geometry p)
          [ox oy] (panel-origin p)
          ^BufferedImage img (plot/render-image p size)
          ;; alpha: 0..45% of the circle, middle at 81 degrees (clockwise from 12)
          a (m/radians 81.0)
          ink (for [d (range 8 40) dy (range -8 9)
                    :let [r (+ (double radius) d)]]
                (rgb img (+ ox (m/round (+ (double cx) (* r (m/sin a))))) (+ oy dy (m/round (- (double cy) (* r (m/cos a)))))))]
      (is (some dark? ink))
      (is (= #{true} (set (map #(get-in % [:label :outer?]) (placements p)))))))
  (testing "2.3 room for :outer labels: the circle shrinks; labels at 12, 3, 6, 9 o'clock inside the image"
    (let [even (pie {:value-label (constantly "WWWWWW\nWWWWWW") :label-position :outer}
                    {:data (for [i (range 4)] {:part (str i) :n 1}) :coord {:kind :polar :start -45}})
          plain (pie {} {:data (for [i (range 4)] {:part (str i) :n 1}) :coord {:kind :polar :start -45}})
          centred (pie {:value-label true} {:data (for [i (range 4)] {:part (str i) :n 1}) :coord {:kind :polar :start -45}})
          b (plot/build even size)
          [rx ry] (get-in b [:stats 0 :outer-room])
          {:keys [cx cy radius]} (geometry even)
          {:keys [w h]} (grid/item-box (:layout b) :panel)]
      (is (and (pos? (double rx)) (pos? (double ry))))
      (is (< (double radius) (double (:radius (geometry plain)))))
      (is (= (:radius (geometry plain)) (:radius (geometry centred))) "no :outer labels: radius unchanged")
      (is (nil? (get-in (plot/build centred size) [:stats 0 :outer-room])))
      (is (<= (+ (double radius) (double rx)) (+ (* 0.5 (double w)) 1e-6)))
      (is (<= (+ (double radius) (double ry)) (+ (* 0.5 (double h)) 1e-6)))
      (let [ps (placements even)]
        (is (= 4 (count ps)))
        (is (every? (fn [{:keys [box]}] (and (<= 0 (long (:x box))) (<= (+ (long (:x box)) (long (:w box))) (long w))
                                            (<= 0 (long (:y box))) (<= (+ (long (:y box)) (long (:h box))) (long h))))
                    ps)
            "every label box inside the panel"))))
  (testing "2.4 colliding labels skipped, largest first"
    (let [many (pie {:value-label #(get-in % [:group :color]) :label-position :outer}
                    {:data (map (fn [i] {:part (str "p" i) :n (if (< i 3) (* 30 (- 3 i)) (inc (mod i 3)))}) (range 20))})
          ps (placements many)
          boxes (map :box ps)]
      (is (< (count ps) 20))
      (is (some #(= "p0" (get-in % [:content :text])) ps) "the largest slice labelled")
      (is (not-any? true? (for [[i a] (map-indexed vector boxes) b (drop (inc i) boxes)]
                            (and (< (:x a) (+ (:x b) (:w b))) (< (:x b) (+ (:x a) (:w a)))
                                 (< (:y a) (+ (:y b) (:h b))) (< (:y b) (+ (:y a) (:h a)))))))))
  (testing "2.5 centred labels that don't fit their slice skipped"
    (let [ps (placements (pie {:value-label #(str (get-in % [:group :color]) " slice")}
                              {:data [{:part "big" :n 40} {:part "rest" :n 59} {:part "tiny" :n 1}]}))
          texts (set (map #(get-in % [:content :text]) ps))]
      (is (contains? texts "big slice"))
      (is (not (contains? texts "tiny slice")))))
  (testing "2.5 cartesian: a stacked segment too short for its label"
    (let [ps (placements {:data [{:c "a" :g "p" :v 100} {:c "a" :g "q" :v 0.5}] :mapping {:x :c :y :v :color :g}
                          :charts [(chart/bar {:position :stack :value-label true})]})]
      (is (= ["100"] (map #(get-in % [:content :text]) ps)))))
  (testing "2.6 leaders from the slice's edge to the label"
    (let [p (pie {:value-label (constantly "W") :label-position :outer :label-leader true})
          ps (placements p)
          [ox oy] (panel-origin p)
          ^BufferedImage img (plot/render-image p size)]
      (is (every? :leader ps))
      (is (every? (fn [{[[x0 y0] [x1 y1]] :leader}]
                    (let [mx (+ ox (m/round (* 0.5 (+ (double x0) (double x1))))) my (+ oy (m/round (* 0.5 (+ (double y0) (double y1)))))]
                      ;; a 1-pixel antialiased line: ink in the 3x3 neighbourhood
                      (some (fn [[r g b]] (and (< (long r) 200) (< (long g) 200) (< (long b) 200)))
                            (for [dx [-1 0 1] dy [-1 0 1]] (rgb img (+ mx dx) (+ my dy))))))
                  ps)
          "ink at every leader's middle")
      (is (not-any? :leader (placements (pie {:value-label (constantly "W") :label-position :outer}))) "no leaders by default"))))

(deftest review-fixes-test
  (testing "Y1 :outer labels hide the angle axis by default; the plot's scale brings it back"
    (is (= {:x {:axis false}} (get-in (plot/build (pie {:value-label true :label-position :outer}) size) [:stats 0 :scale-defaults])))
    (is (= {:x {:axis false}} (get-in (plot/build {:data shares :mapping {:x :part :y :n} :coord {:kind :polar}
                                                   :charts [(chart/bar {:value-label true :label-position :outer})]} size)
                                      [:stats 0 :scale-defaults]))
        "circular barplot: the levels on the angle")
    (is (= {:y {:axis false}} (get-in (plot/build (pie {:value-label true :label-position :outer} {:flip true}) size)
                                      [:stats 0 :scale-defaults]))
        "flipped: the angle on :y")
    (is (false? (get-in (plot/build (pie {:value-label true :label-position :outer}) size) [:finalized-scales :x :axis])))
    (is (not (false? (get-in (plot/build (pie {:value-label true :label-position :outer} {:scales {:x {:axis true}}}) size)
                             [:finalized-scales :x :axis]))))
    (is (nil? (get-in (plot/build (pie {:value-label true}) size) [:stats 0 :scale-defaults]))))
  (testing "Y2 end labels of every strip chart skip collisions"
    (let [rows (for [i (range 30) v [1 2 3]] {:c (format "%02d" i) :v v})
          seen (atom [])]
      (with-redefs [strip/draw-label-placements! (fn [_ ps _] (swap! seen into ps))]
        (plot/render-image {:data rows :mapping {:x :v :y :c} :charts [(chart/box {:n-label true})]} {:width 300 :height 120}))
      (is (< 0 (count @seen) 30))
      (is (every? :box @seen))))
  (testing "Y3 leaders lead: longer gap, more room"
    (let [with (pie {:value-label (constantly "W") :label-position :outer :label-leader true})
          without (pie {:value-label (constantly "W") :label-position :outer})]
      (is (every? (fn [{[[x0 y0] [x1 y1]] :leader}] (>= (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))) 14.0))
                  (placements with)))
      (is (> (double (first (get-in (plot/build with size) [:stats 0 :outer-room])))
             (double (first (get-in (plot/build without size) [:stats 0 :outer-room])))))))
  (testing "Y4 every label carries its own bar"
    (is (every? #(get-in % [:label :bar]) (placements (pie {:value-label true}))))
    (let [ps (placements {:data [{:c "a" :g "p" :v 2} {:c "a" :g "q" :v 2}] :mapping {:x :c :y :v :color :g}
                          :charts [(chart/bar {:position :overlay :value-label true})]})]
      (is (= 1 (count ps)) "two equal overlaid bars: one label (the other collides)")))
  (testing "Y5 a centred label wider than its slice's chord near 12 o'clock is skipped"
    (let [p (pie {:value-label #(if (= "top" (get-in % [:group :color])) "WWWWWWWWWW" "x")}
                 {:data [{:part "top" :n 6} {:part "rest" :n 94}] :coord {:kind :polar :start -10.8}})
          texts (map #(get-in % [:content :text]) (placements p))]
      (is (not (some #{"WWWWWWWWWW"} texts)))
      (is (some #{"x"} texts)))))
