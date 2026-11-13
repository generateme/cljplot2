(ns cljplot2.stat-chart-test
  "Step 8: groups within panels, histogram, density, line, ECDF, polylines, suggested axis titles."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.core :as c2d]
            [cljplot2.scale.aesthetic :as aesthetic]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.chart.ecdf :as ecdf]
            [cljplot2.chart.histogram :as histogram]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(defn- build
  ([p] (plot/build p {:width 600 :height 400}))
  ([p opts] (plot/build p (merge {:width 600 :height 400} opts))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(def ^:private rows
  (vec (for [i (range 40)]
         {:v (double (mod (* i 7) 13)) :g (["a" "b"] (mod i 2)) :f (["p" "q"] (quot i 20)) :n (mod i 3)})))

(defn- data-of [built] (get-in built [:stats 0 :data]))
(defn- panel [built] (get (data-of built) :panel))

;; 1.x groups

(deftest groups-test
  (testing "one curve per (panel, group)"
    (let [built (build {:data rows :mapping {:x :v :color :g :facet :f} :charts [(chart/density)]})]
      (is (= {"p" 2 "q" 2} (update-vals (data-of built) (comp count :curves))))
      (is (= [{:color "a"} {:color "b"}] (map :aesthetics (:curves (get (data-of built) "p")))))))
  (testing "numeric aesthetic on a linear scale can't group; ordinal can"
    (is (= {:key :mapping :value :color} (error #(build {:data rows :mapping {:x :v :color :n} :charts [(chart/density)]}))))
    (is (= 3 (count (:curves (panel (build {:data rows :mapping {:x :v :color :n} :scales {:color {:kind :ordinal}}
                                           :charts [(chart/density)]}))))))
    (is (= 3 (count (:curves (panel (build {:data rows :mapping {:x :v :color :n} :scales {:color {:domain [0 1 2]}}
                                           :charts [(chart/density)]})))))
        "explicit 3-value numeric domain is ordinal"))
  (testing ":group splits without styling, any value kind"
    (let [built (build {:data rows :mapping {:x :v :y :v :group :n} :charts [(chart/line)]})]
      (is (= 3 (count (:lines (panel built)))))
      (is (every? #(= {} (:aesthetics %)) (:lines (panel built))))
      (is (nil? (get-in built [:scale-specs :group])))))
  (testing ":group accessor validated; :group has no scale"
    (is (= {:key :group :in [:mapping :group] :value 3} (error #(build {:data rows :mapping {:x :v :y :v :group 3} :charts [(chart/line)]}))))
    (is (= [:scales :group] (:in (error #(build {:data rows :mapping {:x :v :y :v :group :n} :scales {:group {}} :charts [(chart/line)]}))))))
  (testing "NA group and :groups {:na false}"
    (let [data (conj rows {:v 1.0 :g nil} {:v 2.0 :g nil})
          built (build {:data data :mapping {:x :v :color :g} :charts [(chart/density)]})]
      (is (= [{:color "a"} {:color "b"} {:color nil}] (map :aesthetics (:curves (panel built)))) "NA last")
      (is (true? (get-in built [:scale-specs :color :na?])))
      (is (= 2 (get-in (build {:data data :mapping {:x :v :color :g} :groups {:na false} :charts [(chart/density)]})
                       [:stats 0 :dropped :color])))))
  (testing "explicit domain orders groups; group outside the domain gets no style"
    (is (= [["b"] ["a"]] (group/ordered-keys [["a"] ["b"]] [:color] {:color {:domain ["b" "a"]}})))
    (is (= [["a"] ["b"] [nil]] (group/ordered-keys [[nil] ["b"] ["a"]] [:color] {})))
    (let [built (build {:data rows :mapping {:x :v :color :g} :scales {:color {:domain ["a"]}} :charts [(chart/density)]})]
      (is (nil? (group/style {:color :red} {:color "b"} (:finalized-scales built) (:theme built))))
      (is (some? (group/style {:color :red} {:color "a"} (:finalized-scales built) (:theme built))))))
  (testing "all-NA aesthetic: misspelt mapping"
    (is (= {:key :mapping :value :color :rows 40} (error #(build {:data rows :mapping {:x :v :color :nope} :charts [(chart/density)]}))))))

;; 2.x histogram

(deftest breaks-test
  (are [values bins brks] (= brks (histogram/breaks values bins))
    [0.0 10.0] 2 [0.0 5.0 10.0]
    [0.0 10.0] 1 [0.0 10.0]
    [3.0 3.0] :sturges [2.5 3.5]
    [] :sturges nil
    [0.0 10.0] [0 1 4] [0.0 1.0 4.0])
  (is (= 5 (dec (count (histogram/breaks (mapv double (range 1 11)) :sturges)))) "sturges on 10 values")
  (are [bins] (= [:charts 0 :options :bins]
                 (take 4 (:in (error #(build {:data [{:x 0.0} {:x 1.0}] :mapping {:x :x} :charts [(chart/histogram {:bins bins})]})))))
    0 -2 "x" [1] [2 1] [1 1] [0 ##NaN] [0 1.5 1.5])
  (is (= {:key :bins :value :nope}
         (error #(build {:data [{:x 0.0} {:x 1.0}] :mapping {:x :x} :charts [(chart/histogram {:bins :nope})]})))
      "unknown fastmath method, found when estimating"))

(deftest histogram-test
  (let [chem (vec (for [i (range 300)] {:x (double (mod (* i 37) 100)) :s (mod i 3)}))]
    (testing "breaks shared by all panels"
      (let [built (build {:data chem :mapping {:x :x :facet :s} :charts [(chart/histogram)]})
            edges (fn [p] (set (map (juxt :x0 :x1) (:bars (get (data-of built) p)))))]
        (is (= (edges 0) (edges 1) (edges 2)))))
    (testing "measures"
      (let [sum-of (fn [y] (reduce + (map :y1 (:bars (panel (build {:data chem :mapping {:x :x} :charts [(chart/histogram {:y y})]}))))))]
        (is (< (m/abs (- 100.0 (sum-of :percent))) 1e-9))
        (is (= 300.0 (sum-of :count)))
        (is (< (m/abs (- 1.0 (sum-of :pmf))) 1e-9))
        (let [bars (:bars (panel (build {:data chem :mapping {:x :x} :charts [(chart/histogram {:y :density})]})))]
          (is (< (m/abs (- 1.0 (reduce + (map #(* (:y1 %) (- (:x1 %) (:x0 %))) bars)))) 1e-9)))))
    (testing "percent of each group's total when grouped"
      (let [bars (:bars (panel (build {:data rows :mapping {:x :v :color :g} :charts [(chart/histogram)]})))]
        (is (every? #(< (m/abs (- 100.0 %)) 1e-9)
                    (vals (update-vals (group-by :aesthetics bars) #(reduce + (map :y1 %))))))))
    (testing "y from 0, bars on the panel's bottom edge"
      (let [p {:data chem :mapping {:x :x} :charts [(chart/histogram {:color [255 0 0] :stroke-width 0})] :theme plain}
            built (build p)
            {:keys [x y w h]} (grid/item-box (:layout built) :panel)
            c (plot/render p {:width 600 :height 400})]
        (is (= 0.0 (double (first (get-in built [:finalized-scales :y :domain])))))
        (is (= [255 0 0] (pixel c (+ x (quot w 2)) (+ y h -1))) "bottom pixel row is bar")))
    (testing "positions"
      (let [bars-of (fn [position] (:bars (panel (build {:data rows :mapping {:x :v :color :g}
                                                          :charts [(chart/histogram {:position position :bins 4})]}))))
            dodge (bars-of :dodge)
            mirror (bars-of :mirror)
            overlay (bars-of :overlay)
            width #(- (:x1 %) (:x0 %))]
        (is (= (* 2 (width (first dodge))) (width (first overlay))) "dodge: half-width bars")
        (is (every? #(<= (:y1 %) 0.0) (filter #(= {:color "b"} (:aesthetics %)) mirror)) "mirror: second group down")
        (is (every? #(>= (:y1 %) 0.0) (filter #(= {:color "a"} (:aesthetics %)) mirror)))
        (is (neg? (double (first (get-in (build {:data rows :mapping {:x :v :color :g} :charts [(chart/histogram {:position :mirror})]})
                                         [:scale-specs :y :domain])))))
        (is (= {:key :position :groups 3}
               (error #(build {:data rows :mapping {:x :v :color :n} :scales {:color {:kind :ordinal}}
                                    :charts [(chart/histogram {:position :mirror})]}))))
        (is (= {:key :position :groups 1} (error #(build {:data rows :mapping {:x :v} :charts [(chart/histogram {:position :mirror})]}))))
        (is (true? (:translucent? (panel (build {:data rows :mapping {:x :v :color :g} :charts [(chart/histogram)]})))))
        (is (false? (:translucent? (panel (build {:data rows :mapping {:x :v} :charts [(chart/histogram)]})))) "single group opaque")))
    (testing "overlay blends group colours"
      (let [p {:data [{:x 1.0 :g "a"} {:x 1.0 :g "b"}] :mapping {:x :x :color :g} :theme plain
               :scales {:color {:range [[255 0 0] [0 0 255]]}}
               :charts [(chart/histogram {:stroke-width 0 :alpha 0.5 :bins 1})]}
            built (build p)
            {:keys [x y w h]} (grid/item-box (:layout built) :panel)
            [r _ b] (pixel (plot/render p {:width 600 :height 400}) (+ x (quot w 2)) (+ y (quot h 2)))]
        (is (and (> r 0) (> b 0)) "both colours in the overlap")))
    (testing "edge cases"
      (is (= {} (data-of (build {:data [] :mapping {:x :x} :charts [(chart/histogram)]}))))
      (is (= [[4.5 5.5]] (map (juxt :x0 :x1) (:bars (panel (build {:data [{:x 5} {:x 5}] :mapping {:x :x} :charts [(chart/histogram)]}))))))
      (let [built (build {:data [{:x 1} {:x ##NaN} {:x ##Inf} {:x 3} {:x 50}] :mapping {:x :x} :charts [(chart/histogram {:bins [0 10]})]})]
        (is (= 3 (get-in built [:stats 0 :dropped :x])) "NaN, Inf and outside explicit breaks")
        (is (= 100.0 (reduce + (map :y1 (:bars (panel built)))))))
      (is (= {:key :mapping :value :x :rows 2} (error #(build {:data [{:x "a"} {:x "b"}] :mapping {:x :x} :charts [(chart/histogram)]}))))
      (are [opts k] (= k (:key (error #(build {:data rows :mapping {:x :v} :charts [(chart/histogram opts)]}))))
        {:y :freq} :y
        {:position :stack} :position
        {:alpha 1.5} :alpha
        {:stroke-width -1} :stroke-width
        {:color :none} :color
        {:padding {:y [-1 0]}} :y
        {:padding {:z [0 0]}} :z
        {:bins 10 :typo 1} :typo))))

;; 3.x density

(deftest density-test
  (let [built (build {:data rows :mapping {:x :v} :charts [(chart/density)]})
        {:keys [x y]} (first (:curves (panel built)))
        step (- (x 1) (x 0))]
    (testing "curve integrates to 1, spans data +- cut*h, 512 points"
      (is (= 512 (count x)))
      (is (< (m/abs (- 1.0 (* step (reduce + y)))) 0.01))
      (is (< (first x) 0.0))
      (is (> (peek x) 12.0)))
    (testing "y extent from 0"
      (is (= 0.0 (double (first (get-in built [:finalized-scales :y :domain])))))))
  (testing "options"
    (is (= 10 (count (:x (first (:curves (panel (build {:data rows :mapping {:x :v} :charts [(chart/density {:n 10 :cut 0})]}))))))))
    (let [{:keys [x]} (first (:curves (panel (build {:data rows :mapping {:x :v} :charts [(chart/density {:cut 0})]}))))]
      (is (= [0.0 12.0] [(first x) (peek x)]) "cut 0: data range"))
    (let [default (first (:curves (panel (build {:data rows :mapping {:x :v} :charts [(chart/density {:cut 0})]}))))
          custom (first (:curves (panel (build {:data rows :mapping {:x :v} :charts [(chart/density {:cut 0 :bandwidth 0.5 :kernel :epanechnikov})]}))))]
      (is (not= (:y default) (:y custom)) "kernel and bandwidth used"))
    (is (= :density (:key (error #(build {:data rows :mapping {:x :v} :charts [(chart/density {:kernel :no-such-kernel})]})))))
    (are [opts k] (= k (:key (error #(build {:data rows :mapping {:x :v} :charts [(chart/density opts)]}))))
      {:n 1} :n
      {:cut -1} :cut
      {:width -1} :width
      {:dash [0]} :dash
      {:fill nil} :fill))
  (testing "groups with < 2 distinct values: no curve, counted"
    (let [built (build {:data (conj rows {:v 5.0 :g "c"} {:v 5.0 :g "c"}) :mapping {:x :v :color :g} :charts [(chart/density)]})]
      (is (= 2 (count (:curves (panel built)))))
      (is (= 1 (get-in built [:stats 0 :dropped :group])))))
  (testing "no :ref option or theme key any more"
    (is (not (contains? (get-in (build {:data rows :mapping {:x :v} :charts [(chart/density)]}) [:theme :chart :density]) :ref-color)))))

;; 4.x line, polylines

(deftest polyline-runs-test
  (are [xs ys n] (= n (count (polyline/runs xs ys)))
    [] [] 0
    [1.0] [1.0] 1
    [1.0 2.0 3.0] [1.0 ##NaN 3.0] 2
    [##NaN 2.0 3.0] [1.0 2.0 ##NaN] 1
    [1.0 2.0] [##NaN ##NaN] 0
    [1.0 2.0 3.0 4.0] [1.0 2.0 3.0 4.0] 1))

(deftest line-test
  (let [data [{:x 3 :y 30} {:x 1 :y 10} {:x 2 :y 20}]
        line-of (fn [opts d] (first (:lines (panel (build {:data d :mapping {:x :x :y :y} :charts [(chart/line opts)]})))))]
    (testing "sorted by x; :sort false keeps data order"
      (is (= [1 2 3] (:x (line-of {} data))))
      (is (= [3 1 2] (:x (line-of {:sort false} data)))))
    (testing "ties keep data order"
      (is (= [10 11 20] (:y (line-of {} [{:x 2 :y 20} {:x 1 :y 10} {:x 1 :y 11}])))))
    (testing "invalid y breaks the line; invalid x dropped"
      (let [built (build {:data [{:x 1 :y 1} {:x 2 :y ##NaN} {:x 3 :y 3} {:x nil :y 4}] :mapping {:x :x :y :y} :charts [(chart/line)]})]
        (is (= [1 nil 3] (:y (first (:lines (panel built))))))
        (is (= {:x 1 :y 1} (select-keys (get-in built [:stats 0 :dropped]) [:x :y])))))
    (testing "categorical x keeps data order"
      (is (= ["b" "a"] (:x (line-of {} [{:x "b" :y 1} {:x "a" :y 2}])))))
    (testing "padding 5%"
      (is (= [0.05 0.05] (get-in (build {:data data :mapping {:x :x :y :y} :charts [(chart/line)]}) [:stats 0 :padding :x]))))
    (testing "validation"
      (is (= :sort (:key (error #(build {:data data :mapping {:x :x :y :y} :charts [(chart/line {:sort 1})]})))))
      (is (= {:key :y :in [:charts 0 :options :mapping :y] :value nil}
             (error #(build {:data data :mapping {:x :x} :charts [(chart/line)]}))))
      (is (= {:key :y :in [:charts 0 :options :mapping :y] :value nil}
             (error #(build {:data data :mapping {:x :x :y :y} :charts [(chart/line {:mapping {:y nil}})]})))
          "a chart can't unmap a required key"))))

;; 5.x ECDF

(deftest ecdf-test
  (testing "steps from (min, 0) to (max, 1.0)"
    (let [{:keys [x y]} (ecdf/steps [3.0 1.0 2.0 2.0 5.0 4.0])]
      (is (= [1.0 1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0 5.0] x))
      (is (= 0.0 (first y)))
      (is (= 1.0 (peek y)))
      (is (apply <= y))))
  (testing "single value; duplicates"
    (is (= {:x [7.0 7.0] :y [0.0 1.0]} (ecdf/steps [7.0])))
    (is (= {:x [7.0 7.0] :y [0.0 1.0]} (ecdf/steps [7.0 7.0 7.0]))))
  (testing "chart: y extent [0 1], groups"
    (let [built (build {:data rows :mapping {:x :v :color :g} :charts [(chart/ecdf)]})]
      (is (= 2 (count (:steps (panel built)))))
      (is (= {:kind :numeric :min 0.0 :max 1.0} (get-in built [:extents :y :extent]))))))

;; 6.x legends, axis titles

(defn- y-title [built] (some #(when (= :y-title (:id %)) (get-in % [:content :text])) (get-in built [:grid :items])))

(deftest axis-titles-test
  (are [charts labels title] (= title (y-title (build {:data rows :mapping {:x :v} :charts charts :labels labels})))
    [(chart/histogram)] nil "Percent of Total"
    [(chart/histogram {:y :count})] nil "Count"
    [(chart/histogram {:y :pmf})] nil "Probability"
    [(chart/histogram {:y :density})] nil "Density"
    [(chart/density)] nil "Density"
    [(chart/ecdf)] nil "Empirical CDF"
    [(chart/density) (chart/histogram)] nil "Density"
    [(chart/histogram)] {:y "Share"} "Share"
    [(chart/histogram)] {:y nil} nil
    [(chart/histogram)] {:title "T"} "Percent of Total")
  (is (nil? (y-title (build {:data rows :mapping {:x :v :y :v} :charts [(chart/line)]})))))

(deftest legend-glyph-test
  (let [render-key (fn [chart-value]
                     (let [p {:data rows :mapping {:x :v :y :v :color :g} :theme plain
                              :scales {:color {:range [[255 0 0] [0 0 255]]}} :charts [chart-value]}
                           built (build p)
                           box (grid/item-box (:layout built) :legends)
                           c (plot/render p {:width 600 :height 400})]
                       (set (for [px (range (:x box) (+ (:x box) 14)) py (range (:y box) (+ (:y box) (:h box)))] (pixel c px py)))))]
    (is (contains? (render-key (chart/histogram {:stroke-width 0 :position :dodge})) [255 0 0]) "histogram: filled key")
    (let [overlay (render-key (chart/histogram {:stroke-width 0 :alpha 0.6}))]
      (is (not (contains? overlay [255 0 0])) "overlay key translucent like the bars")
      (is (contains? overlay [255 102 102]) "red at alpha 0.6 over white"))
    (is (contains? (render-key (chart/density {:width 3})) [255 0 0]) "density: line key")
    (is (contains? (render-key (chart/line {:width 3})) [255 0 0]))
    (is (contains? (render-key (chart/ecdf {:width 3})) [255 0 0]))))

;; 7.x theme

(deftest theme-validation-test
  (are [t k] (= k (:key (error #(build {:data rows :mapping {:x :v} :theme t
                                              :charts [(chart/histogram) (chart/density) (chart/ecdf)]}))))
    {:chart {:histogram {:alpha 2}}} :alpha
    {:chart {:density {:width -1}}} :width
    {:chart {:ecdf {:dash [-1]}}} :dash))

;; mirror labels and symmetric halves

(deftest mirror-axis-test
  (let [uneven (vec (concat (for [i (range 30)] {:v (double (mod i 5)) :g "a"})
                            (for [i (range 10)] {:v (double (mod i 5)) :g "b"})))
        mirror (fn [opts extra] (build (merge {:data uneven :mapping {:x :v :color :g}
                                                :charts [(chart/histogram (merge {:position :mirror :y :count :bins 5} opts))]}
                                               extra)))
        fy (fn [built] (get-in built [:finalized-scales :y]))]
    (testing "mirror: tick labels are magnitudes, positions stay signed"
      (let [y (fy (mirror {} {}))]
        (is (some neg? (:ticks y)))
        (is (not-any? #(re-find #"-" %) (:tick-labels y)))
        (is (= (count (:ticks y)) (count (:tick-labels y))))))
    (testing "a plot :formatter wins over the absolute format"
      (is (some #(re-find #"-" %) (:tick-labels (fy (mirror {} {:scales {:y {:formatter str}}}))))))
    (testing "other positions keep signed default labels; :tick-format is internal"
      (is (nil? (get-in (build {:data uneven :mapping {:x :v :color :g} :charts [(chart/histogram)]}) [:scale-specs :y :tick-format])))
      (is (= :tick-format (:key (error #(build {:data uneven :mapping {:x :v} :scales {:y {:tick-format :absolute}}
                                                     :charts [(chart/histogram)]}))))))
    (testing "not symmetric by default: halves sized by their own tallest bar"
      (let [ext (get-in (mirror {} {}) [:extents :y :extent])]
        (is (= [-2.0 6.0] [(:min ext) (:max ext)]))))
    (testing ":symmetric ties both halves to the tallest bar"
      (let [built (mirror {:symmetric true} {})
            ext (get-in built [:extents :y :extent])
            [lo hi] (:domain (fy built))]
        (is (= [-6.0 6.0] [(:min ext) (:max ext)]))
        (is (< (m/abs (+ (double lo) (double hi))) 1e-9) "finalized domain symmetric")))
    (testing ":symmetric across facets uses the tallest bar of all panels"
      (let [built (mirror {:symmetric true} {:data (map #(assoc % :f (if (< (:v %) 2) "p" "q")) uneven)
                                              :mapping {:x :v :color :g :facet :f}})]
        (is (= [-6.0 6.0] ((juxt :min :max) (get-in built [:extents :y :extent]))))))
    (testing ":symmetric ignored outside mirror; validated"
      (is (= 0.0 (:min (get-in (build {:data uneven :mapping {:x :v} :charts [(chart/histogram {:symmetric true})]}) [:extents :y :extent]))))
      (is (= :symmetric (:key (error #(mirror {:symmetric 1} {}))))))))

;; review fixes: explicit domains at compute, counting after the split, area fill

(deftest in-domain-test
  (are [k spec v ok] (= ok (aesthetic/in-domain? k spec v))
    :color {} "z" true
    :color {:domain ["a" "b"]} "a" true
    :color {:domain ["a" "b"]} "z" false
    :color {:domain ["a" "b"]} nil true
    :color {:domain [1931.0 1932.0] :kind :ordinal} 1931 true
    :color {:domain [1931 1932 1933]} 1934 false
    :size {:domain [0 10]} 10 true
    :size {:domain [0 10]} 0 true
    :size {:domain [0 10]} 10.001 false
    :size {:domain [0 10]} -1 false
    :size {:domain [0 10]} "x" false
    :color {:domain "bad"} "x" true
    :color {:domain []} "x" true
    :shape {:domain [1 2]} 2 true
    :shape {:domain [1 2]} 3 false))

(deftest explicit-domain-at-compute-test
  (let [data (concat (repeat 10 {:x 1 :g "a"}) (repeat 90 {:x 2 :g "b"}))]
    (testing "stat charts: groups outside the domain dropped, counted, not in extents"
      (let [built (build {:data data :mapping {:x :x :color :g} :scales {:color {:domain ["a"]}} :charts [(chart/histogram {:y :count})]})]
        (is (= 90 (get-in built [:stats 0 :dropped :color])))
        (is (= 10.0 (:max (get-in built [:extents :y :extent]))))
        (is (= [{:color "a"}] (distinct (map :aesthetics (:bars (panel built))))))))
    (testing "scatter: points outside the domain dropped, counted, not in extents"
      (let [built (build {:data (map #(assoc % :y (:x %)) data) :mapping {:x :x :y :y :color :g}
                          :scales {:color {:domain ["a"]}} :charts [(chart/scatter)]})]
        (is (= 90 (get-in built [:stats 0 :dropped :color])))
        (is (= 1.0 (:max (get-in built [:extents :x :extent]))))))
    (testing "linear explicit domain: outside values dropped (boundaries kept)"
      (let [built (build {:data (for [s [0 5 10 11]] {:x s :y s :s s}) :mapping {:x :x :y :y :size :s}
                          :scales {:size {:domain [0 10]}} :charts [(chart/scatter)]})]
        (is (= 1 (get-in built [:stats 0 :dropped :size])))
        (is (= [0 5 10] (:x (panel built))))))
    (testing "everything outside: empty, no error"
      (is (= {:panel []} (update-vals (data-of (build {:data data :mapping {:x :x :color :g} :scales {:color {:domain ["z"]}}
                                               :charts [(chart/density)]}))
                             :curves)
             )))))

(deftest no-valid-x-after-split-test
  (let [data [{:x "bad" :g nil} {:x "worse" :g "a"} {:x "nope" :g "a"}]]
    (doseq [c [(chart/density) (chart/ecdf) (chart/histogram)]]
      (is (= {:key :mapping :value :x :rows 2}
             (error #(build {:data data :mapping {:x :x :color :g} :groups {:na false} :charts [c]})))
          (str (:kind c))))))

(deftest line-area-test
  (let [area-plot (fn [ys] {:data (map-indexed (fn [i y] {:x i :y y}) ys) :mapping {:x :x :y :y} :theme plain
                            :charts [(chart/line {:fill [255 0 0] :color :none})]})
        probe (fn [ys py-of]
                (let [p (area-plot ys)
                      built (build p)
                      {:keys [x y w h]} (grid/item-box (:layout built) :panel)
                      c (plot/render p {:width 600 :height 400})]
                  (pixel c (+ x (quot w 2)) (py-of built y h))))
        fy (fn [built v] (scale/forward (get-in built [:finalized-scales :y]) v))]
    (testing "fill between the line and y = 0"
      (is (= [255 0 0] (probe [-5 5 5 -5] (fn [b y _] (+ y (long (fy b 2.5)))))))
      (is (= [255 255 255] (probe [1 5 5 1] (fn [_ y _] (+ y 1)))) "nothing above the line"))
    (testing "0 below the y range: fill down to the panel's bottom edge"
      (is (= [255 0 0] (probe [10 20 20 10] (fn [_ y h] (+ y h -2))))))
    (testing "0 above the y range: fill up to the top edge"
      (is (= [255 0 0] (probe [-10 -20 -20 -10] (fn [_ y _] (+ y 1))))))
    (testing "default :none; nil rejected"
      (is (= :none (get-in (build (area-plot [1 2])) [:theme :chart :line :fill])))
      (is (= :fill (:key (error #(build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/line {:fill nil})]}))))))))
