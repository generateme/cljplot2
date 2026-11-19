(ns cljplot2.vega-cycle2-test
  "Vega cycle 2 (distributions and composites): dot plots (Wilkinson dot density, fixed bins, wheat plot, shared
  stacks), pre-aggregated boxes (`:stat :identity`), horizon rows and the horizon builder."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.dots :as dots]
            [cljplot2.data.series :as series]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.test-util :refer [error]])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]))

(def ^:private red (unchecked-int 0xffff0000))
(def ^:private blue (unchecked-int 0xff0000ff))

(defn- px [^BufferedImage img x y] (.getRGB img (int x) (int y)))

(defn- count-color
  [^BufferedImage img color]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (== (int color) (px img x y))] 1)))

(defn- color-points
  "Pixels `[x y]` of `img` exactly `color`."
  [^BufferedImage img color]
  (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (== (int color) (px img x y))] [x y]))

(defn- stats
  "Computed data of chart 0 of plot `p` (first panel)."
  [p]
  (val (first (get-in (plot/build p {:width 400 :height 300}) [:stats 0 :data]))))

(defn- counts
  "Stack sizes of every slot of the computed dot plot data `d`."
  [d]
  (vec (for [s (:slots d) st (:stacks s)] (:n st))))

(deftest bins-test
  (testing "the spec example: both methods give 4 bins of 3, 2, 2, 1"
    (let [vs [1 1 1 2 2 3 3 4]]
      (is (= [3 2 2 1] (mapv #(count (:values %)) (dots/dot-density-bins vs 1.0))))
      (is (= [3 2 2 1] (mapv #(count (:values %)) (dots/fixed-bins vs 1.0 1.0))))))
  (testing "dot density: a bin starts at its first value; centre - midpoint of its smallest and largest value"
    (is (= [{:center 1.25 :values [1.0 1.5]} {:center 2.5 :values [2.5]}] (dots/dot-density-bins [1.0 1.5 2.5] 1.0)))
    (is (= [1.0] (mapv :center (dots/dot-density-bins [1.0] 1.0))) "one value")
    (is (= [[1.0 1.0 1.0]] (mapv :values (dots/dot-density-bins [1.0 1.0 1.0] 1.0))) "all equal")
    (is (= 1 (count (dots/dot-density-bins [1.0 2.0 3.0] 10.0))) "binwidth above the range"))
  (testing "fixed bins: from the origin, centred, empty bins left out, values below the origin too"
    (is (= [{:center 0.5 :values [0.2]} {:center 2.5 :values [2.0 2.9]}] (dots/fixed-bins [0.2 2.0 2.9] 1.0 0.0)))
    (is (= [-0.5] (mapv :center (dots/fixed-bins [-0.4] 1.0 0.0)))))
  (testing "default binwidth: range / 30, 1 for one distinct value"
    (is (== 1.0 (dots/default-binwidth [0 30])))
    (is (== 1.0 (dots/default-binwidth [5 5])))))

(defn- dot-plot
  [rows mapping options]
  {:data rows :mapping mapping :charts [(chart/dotplot options)]})

(deftest dotplot-compute-test
  (let [rows (map (fn [v] {:v v}) [1 1 1 2 2 3 3 4])]
    (testing "stacks per bin; the default binwidth gives at most 30 bins"
      (is (= [3 2 2 1] (counts (stats (dot-plot rows {:x :v} {:binwidth 1})))))
      (is (= [3 2 2 1] (counts (stats (dot-plot rows {:x :v} {:binwidth 1 :method :histogram})))))
      (is (>= 30 (count (counts (stats (dot-plot (map (fn [v] {:v (* 0.37 v)}) (range 200)) {:x :v} {})))))))
    (testing ":at :value keeps every dot's value; :center puts them at the bin centre"
      (let [rows [{:v 1.0} {:v 1.4} {:v 1.8}]
            xs (fn [o] (sort (for [s (:slots (stats (dot-plot rows {:x :v} o))) st (:stacks s) d (:dots st)] (:x d))))]
        (is (= [1.0 1.4 1.8] (xs {:binwidth 1 :at :value})))
        (is (= [14 14 14] (map #(Math/round (* 10.0 (double %))) (xs {:binwidth 1}))))))
    (testing "groups share stacks (overlay): one stack per bin, ordered by group, then value"
      (let [rows [{:v 1.0 :g "a"} {:v 1.1 :g "b"} {:v 1.2 :g "a"}]
            d (stats (dot-plot rows {:x :v :color :g} {:binwidth 1}))
            stack (first (:stacks (first (:slots d))))]
        (is (= [3] (counts d)))
        (is (= [0 1 2] (mapv :rank (:dots stack))))
        (is (= ["a" "a" "b"] (mapv #(:color (:aesthetics %)) (:dots stack))))))
    (testing ":position :dodge - each group its own stacks"
      (is (= [2 1] (sort > (counts (stats (dot-plot [{:v 1.0 :g "a"} {:v 1.1 :g "b"} {:v 1.2 :g "a"}]
                                                     {:x :v :color :g} {:binwidth 1 :position :dodge})))))))
    (testing "levels: one set of stacks per level"
      (is (= 2 (count (:slots (stats (dot-plot [{:v 1 :s "p"} {:v 2 :s "q"}] {:x :v :y :s} {:binwidth 1})))))))
    (testing "boundary: a single row; invalid x rows dropped and counted"
      (is (= [1] (counts (stats (dot-plot [{:v 3}] {:x :v} {})))))
      (is (= 1 (get-in (plot/build (dot-plot [{:v 3} {:v nil}] {:x :v} {})) [:stats 0 :dropped :x]))))
    (testing "errors"
      (is (= :binwidth (:key (error #(plot/build (dot-plot rows {:x :v} {:binwidth 0}))))))
      (is (= :binwidth (:key (error #(plot/build (dot-plot rows {:x :v} {:binwidth -1}))))))
      (is (= :origin (:key (error #(plot/build (dot-plot rows {:x :v} {:origin 0}))))))
      (is (= :mapping (:key (error #(plot/build (dot-plot [{:v "a"}] {:x :v} {})))))))))

(deftest dotplot-draw-test
  (testing "the tallest stack fits its slot: 40 dots no taller than the 200 px image's panel"
    (let [img (plot/render-image (dot-plot (concat (repeat 40 {:v 1.0}) [{:v 2.0}]) {:x :v} {:binwidth 1 :color :red})
                                 {:width 300 :height 200})
          ys (map second (color-points img red))]
      (is (pos? (count ys)))
      (is (< (- (long (reduce max ys)) (long (reduce min ys))) 170) "inside the panel (the image minus the x axis)")))
  (testing "fixed bins: neighbouring stacks don't overlap - a gap between them on the bottom row"
    (let [img (plot/render-image (dot-plot (concat (repeat 30 {:v 1.0}) (repeat 30 {:v 2.0}))
                                           {:x :v} {:method :histogram :binwidth 1 :origin 0.5 :color :red})
                                 {:width 300 :height 200})
          pts (color-points img red)
          bottom (reduce max (map second pts))
          row (sort (map first (filter #(= (- (long bottom) 1) (second %)) pts)))
          runs (count (filter (fn [[a b]] (> (- (long b) (long a)) 1)) (partition 2 1 row)))]
      (is (= 1 runs) "two runs of red - one gap")))
  (testing "a centred stack is symmetric about the level's centre line; :up starts at the slot's low edge"
    (let [rows (concat (repeat 8 {:v 1.0}) (repeat 2 {:v 3.0}))
          mean-y (fn [pts] (/ (double (reduce + (map second pts))) (count pts)))
          halves (fn [dir]
                   (let [pts (color-points (plot/render-image (dot-plot rows {:x :v} {:binwidth 1 :color :red :stack-dir dir})
                                                              {:width 300 :height 200})
                                           red)
                         mid (/ (double (reduce + (map first pts))) (count pts))]
                     [(mean-y (filter #(< (double (first %)) mid) pts)) (mean-y (filter #(> (double (first %)) mid) pts))]))
          [tall-c short-c] (halves :center)
          [tall-u short-u] (halves :up)]
      (is (< (Math/abs (- tall-c short-c)) 1.0) "both stacks centred on one line")
      (is (> short-u (+ tall-u 5.0)) "the short stack sits low")))
  (testing "a fixed :size overrides the computed diameter"
    (let [small (count-color (plot/render-image (dot-plot (repeat 3 {:v 1.0}) {:x :v} {:binwidth 1 :color :red :size 4})
                                                {:width 200 :height 200}) red)
          auto (count-color (plot/render-image (dot-plot (repeat 3 {:v 1.0}) {:x :v} {:binwidth 1 :color :red})
                                               {:width 200 :height 200}) red)]
      (is (< small auto))))
  (testing "shape maps and a legend render; flipped (levels on x) too"
    (is (some? (plot/render-image (dot-plot [{:v 1 :g "a"} {:v 2 :g "b"}] {:x :v :color :g}
                                            {:shape {:path "M0 0 L10 0 L5 9 Z"}})
                                  {:width 300 :height 200})))
    (is (some? (plot/render-image (dot-plot [{:v 1 :s "p"} {:v 2 :s "q"}] {:y :v :x :s} {:binwidth 1})
                                  {:width 300 :height 200})))))

(deftest n-label-test
  (testing "the n label sits past the farthest dot edge of its slot: bin centre + half a bin with :at :center"
    (let [d (stats (dot-plot [{:v 1.0} {:v 1.9}] {:x :v} {:binwidth 1 :n-label true}))]
      (is (== 1.95 (:label-x (first (:items d)))) "centre 1.45 + 0.5, past the largest value 1.9"))
    (let [d (stats (dot-plot [{:v 1.0} {:v 1.9}] {:x :v} {:binwidth 1 :at :value :n-label true}))]
      (is (== 2.4 (:label-x (first (:items d)))) ":at :value - the largest value + half a bin")))
  (testing "dodged groups: each item past its own slot's dots"
    (let [d (stats (dot-plot [{:v 1.0 :g "a"} {:v 5.0 :g "b"}] {:x :v :color :g} {:binwidth 1 :position :dodge :n-label true}))]
      (is (= [1.5 5.5] (sort (map :label-x (:items d)))))))
  (testing "renders with n labels"
    (is (some? (plot/render-image (dot-plot [{:v 1.0 :s "p"} {:v 2.0 :s "q"}] {:x :v :y :s} {:n-label true})
                                  {:width 300 :height 200})))))

(deftest overlap-test
  (testing "dot density centres follow their values (ggplot): neighbouring centres may be closer than a bin width"
    (is (= [0.45 1.0] (mapv :center (dots/dot-density-bins [0.0 0.9 1.0] 1.0))))))

(deftest none-color-test
  (testing ":color :none draws no fill (only the outline) instead of failing"
    (let [cv (c2d/canvas 20 20)]
      (c2d/with-canvas [g cv]
        (c2d/set-background g :white)
        (shape/draw! g \O (Vec2. 10 10) {:size 12 :color :none :stroke-color :red :stroke-width 1})
        (shape/draw! g \o (Vec2. 10 10) {:size 6 :color :none}))
      (let [img (c2d/get-image cv)]
        (is (not= red (px img 10 10)) "no fill")
        (is (pos? (count-color img red)) "the outline")))))

(def ^:private penguins
  [{:species "Adelie" :lower 2850 :q1 3350 :median 3700 :q3 4000 :upper 4775 :outliers []}
   {:species "Chinstrap" :lower 2700 :q1 3487.5 :median 3700 :q3 3950 :upper 4800 :outliers [2700 4800]}
   {:species "Gentoo" :lower 3950 :q1 4700 :median 5000 :q3 5500 :upper 6300}])

(def ^:private stat-mapping {:lower :lower :q1 :q1 :median :median :q3 :q3 :upper :upper})

(defn- identity-box
  [rows mapping options]
  {:data rows :mapping (merge stat-mapping mapping) :charts [(chart/box (merge {:stat :identity} options))]})

(deftest identity-box-test
  (testing "statistics read from the rows; outliers optional"
    (let [items (:items (stats (identity-box penguins {:y :species :outliers :outliers} {})))]
      (is (= [3350 3487.5 4700] (mapv :q1 items)))
      (is (= [2850 2700 3950] (mapv :lav items)))
      (is (= [[] [2700 4800] []] (mapv :outliers items)))))
  (testing "the same boxes as the computed stat on values with those statistics"
    (let [values [1.0 2.0 3.0 4.0 5.0]
          tukey (first (:items (stats {:data (map (fn [v] {:v v}) values) :mapping {:x :v} :charts [(chart/box)]})))
          given (first (:items (stats (identity-box [(merge (select-keys tukey [:q1 :median :q3])
                                                            {:lower (:lav tukey) :upper (:uav tukey)})]
                                                    {} {}))))]
      (is (= (select-keys tukey [:q1 :median :q3 :lav :uav]) (select-keys given [:q1 :median :q3 :lav :uav])))))
  (testing "boundary: q1 = q3, a single box without levels, both variants render"
    (is (some? (plot/render-image (identity-box [{:lower 1 :q1 2 :median 2 :q3 2 :upper 3}] {} {}) {:width 300 :height 150})))
    (is (some? (plot/render-image (identity-box penguins {:y :species} {:variant :tufte}) {:width 300 :height 200}))))
  (testing ":n, :mean feed :varwidth, :n-label and :mean"
    (let [rows (map #(assoc %1 :n %2 :mean (:median %1)) penguins [10 20 40])]
      (is (some? (plot/render-image (identity-box rows {:y :species :n :n :mean :mean} {:varwidth true :n-label true :mean true})
                                    {:width 400 :height 250})))
      (is (= [10 20 40] (mapv :n (:items (stats (identity-box rows {:y :species :n :n} {}))))))))
  (testing "errors"
    (is (= :mapping (:key (error #(plot/build (identity-box (conj penguins (first penguins)) {:y :species} {}))))) "duplicate level")
    (is (= :mapping (:key (error #(plot/build (identity-box [{:lower 5 :q1 2 :median 3 :q3 4 :upper 6}] {} {}))))) "unordered")
    (is (= :mapping (:key (error #(plot/build (identity-box [{:lower 1 :q1 ##NaN :median 3 :q3 4 :upper 6}] {} {}))))) "not finite")
    (is (= :mapping (:key (error #(plot/build {:data penguins :mapping {:y :species :q1 :q1} :charts [(chart/box {:stat :identity})]}))))
        "a statistic not mapped")
    (is (= :mapping (:key (error #(plot/build (identity-box [{:lower 1 :q1 2 :median 3 :q3 4 :upper 6 :o ["x"]}] {:outliers :o} {}))))))
    (is (= :varwidth (:key (error #(plot/build (identity-box penguins {:y :species} {:varwidth true}))))))
    (is (= :n-label (:key (error #(plot/build (identity-box penguins {:y :species} {:n-label true}))))))
    (is (= :mean (:key (error #(plot/build (identity-box penguins {:y :species} {:mean true}))))))
    (is (= :stat (:key (error #(plot/build (identity-box penguins {:y :species} {:stat :minmax}))))))))

(deftest horizon-rows-test
  (testing "the spec example: y = [0 10 20], 2 bands, h = 10"
    (let [{:keys [rows band-height]} (series/horizon-rows [{:x 0 :y 0} {:x 1 :y 10} {:x 2 :y 20}] {:bands 2})]
      (is (== 10.0 band-height))
      (is (= [0.0 10.0 10.0] (mapv :y (filter #(= 1 (:band %)) rows))))
      (is (= [0.0 0.0 10.0] (mapv :y (filter #(= 2 (:band %)) rows))))
      (is (every? zero? (map :y (filter #(neg? (long (:band %))) rows))))))
  (testing "crossings of band edges and 0 inserted (exact areas); a crossing exactly at an edge adds no point"
    (let [{:keys [rows]} (series/horizon-rows [{:x 0 :y -10} {:x 4 :y 30}] {:bands 2 :band-height 10})]
      (is (= [0.0 1.0 2.0 3.0 4.0] (mapv :x (filter #(= 1 (:band %)) rows)))))
    (let [{:keys [rows]} (series/horizon-rows [{:x 0 :y 0} {:x 1 :y 10}] {:bands 2 :band-height 10})]
      (is (= [0.0 1.0] (mapv :x (filter #(= 1 (:band %)) rows))))))
  (testing "negatives mirrored: a sine wave - as many positive as negative band rows"
    (let [{:keys [rows]} (series/horizon-rows (map (fn [i] {:x i :y (Math/sin (* 0.3 i))}) (range 60)) {})]
      (is (= (count (filter #(pos? (long (:band %))) rows)) (count (filter #(neg? (long (:band %))) rows))))
      (is (every? #(<= 0.0 (double (:y %))) rows))))
  (testing "values above the top band saturate (band-height given)"
    (is (= [5.0] (mapv :y (filter #(= 1 (:band %)) (:rows (series/horizon-rows [{:x 0 :y 50}] {:bands 1 :band-height 5})))))))
  (testing "series: first-appearance order, one band height"
    (let [{:keys [rows band-height]} (series/horizon-rows [{:x 0 :y 1 :s "b"} {:x 0 :y 4 :s "a"}] {:series :s :bands 2})]
      (is (== 2.0 band-height))
      (is (= ["b" "a"] (distinct (map :series rows))))))
  (testing "boundary: one band, all-zero series (h = 1, empty bands), nil y breaks, unsorted x"
    (is (== 1.0 (:band-height (series/horizon-rows [{:x 0 :y 0} {:x 1 :y 0}] {}))))
    (is (= [nil] (distinct (map :y (filter #(= 1 (:band %)) (:rows (series/horizon-rows [{:x 0 :y nil}] {:bands 1})))))))
    (is (= [0.0 1.0 2.0] (mapv :x (filter #(= 1 (:band %)) (:rows (series/horizon-rows [{:x 2 :y 1} {:x 0 :y 1} {:x 1 :y 1}] {:bands 1})))))))
  (testing "errors"
    (doseq [[opts k] [[{:bands 0} :bands] [{:bands 1.5} :bands] [{:band-height 0} :band-height] [{:band-height ##Inf} :band-height]]]
      (is (= k (:key (error #(series/horizon-rows [{:x 0 :y 1}] opts)))) (pr-str opts)))
    (is (= :x (:key (error #(series/horizon-rows [{:x "a" :y 1}] {})))))
    (is (= :y (:key (error #(series/horizon-rows [{:x 0 :y "a"}] {})))))))

(deftest horizon-builder-test
  (testing "a plot map: y domain [0 h] without an axis, bands in drawing order, a panel per series"
    (let [p (plot/horizon [{:t 0 :v 1 :s "a"} {:t 1 :v -3 :s "a"} {:t 0 :v 2 :s "b"} {:t 1 :v 2 :s "b"}]
                          {:x :t :y :v :series :s :bands 3})]
      (is (= [0.0 1.0] (get-in p [:scales :y :domain])))
      (is (false? (get-in p [:scales :y :axis])))
      (is (= [1 2 3 -1 -2 -3] (get-in p [:scales :color :domain])))
      (is (= :series (get-in p [:mapping :facet])))
      (is (some? (plot/render-image p {:width 400 :height 300})))))
  (testing "the darkest band lies only where |y| is above one band height (x in 2.5..7.5 of 0..10), the bands span
           the whole width (no x padding)"
    (let [img (plot/render-image (plot/horizon [{:x 0 :y 0} {:x 5 :y 10} {:x 10 :y 0}]
                                               {:bands 2 :colors {:positive [[0 0 255] [255 0 0]]}})
                                 {:width 300 :height 120})
          xs-of (fn [c] (map first (color-points img c)))
          all (concat (xs-of blue) (xs-of red))
          a (double (reduce min all)) b (double (reduce max all))
          reds (xs-of red)]
      (is (pos? (count reds)))
      (is (every? #(< (+ a (* 0.22 (- b a))) (double %) (- b (* 0.22 (- b a)))) reds))
      (is (some #(> (double %) (+ a (* 0.3 (- b a)))) reds))
      (is (some #(< (double %) (- b (* 0.3 (- b a)))) reds))))
  (testing "panels in the series' first-appearance order (a nil series - the NA panel, left to facets)"
    (is (= ["zeta" "alpha"] (get-in (plot/horizon [{:x 0 :y 1 :s "zeta"} {:x 0 :y 2 :s "alpha"}] {:series :s})
                                    [:scales :facet :domain])))
    (is (= ["a"] (get-in (plot/horizon [{:x 0 :y 1 :s "a"} {:x 0 :y 2 :s nil}] {:series :s}) [:scales :facet :domain])))
    (is (some? (plot/render-image (plot/horizon [{:x 0 :y 1 :s "a"} {:x 1 :y 2 :s nil}] {:series :s}) {:width 300 :height 200})))
    (is (nil? (get-in (plot/horizon [{:x 0 :y 1}]) [:scales :facet])) "no series - no facet scale"))
  (testing "the band chart has no x or y padding"
    (is (= {:x [0.0 0.0] :y [0.0 0.0]} (get-in (plot/horizon [{:x 0 :y 1}]) [:charts 0 :options :padding]))))
  (testing "the x domain is the data's x range: no nice rounding (data from 1 to 20 stays 1..20)"
    (let [b (plot/build (plot/horizon (map (fn [x] {:x x :y (mod x 3)}) (range 1 21))) {:width 300 :height 100})]
      (is (= [1.0 20.0] (mapv double (get-in b [:finalized-scales :x :domain])))))
    (is (true? (get-in (plot/horizon [{:x 0 :y 1}] {:scales {:x {:nice true}}}) [:scales :x :nice])) "overridable"))
  (testing ":scales merged; :colors validated"
    (is (= "v" (get-in (plot/horizon [{:x 0 :y 1}] {:scales {:color {:title "v"}}}) [:scales :color :title])))
    (is (= :colors (:key (error #(plot/horizon [{:x 0 :y 1}] {:colors {:positive [[0 0 255]]}})))) "fewer than :bands colours")
    (is (= :colors (:key (error #(plot/horizon [{:x 0 :y 1}] {:colors {:up [[0 0 255] [0 0 9]]}})))))))
