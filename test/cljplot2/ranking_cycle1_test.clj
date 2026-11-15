(ns cljplot2.ranking-cycle1-test
  "Ranking cycle 1: the bar chart - stats, positions, mirror, widths, baseline, labels, error-bar slots, strips."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [clojure2d.core :as c2d]
            [cljplot2.chart.bar :as bar]
            [cljplot2.data.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- stats [p i] (get-in (build p) [:stats i]))
(defn- bars
  ([p] (bars p :panel))
  ([p panel] (get-in (stats p 0) [:data panel :bars])))
(defn- ends [p] (mapv (juxt :level :y0 :y1) (bars p)))
(def ^:private white {:plot {:background :white} :panel {:fill :white :grid {:x :none :y :none :bands-fill :none}}})
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- red-run
  "Longest vertical run of pure-red pixels over every column of `img` - a horizontal bar's thickness, a vertical bar's
  height (wherever the axes put the bar)."
  [^BufferedImage img]
  (reduce max 0 (for [x (range (.getWidth img))]
                  (->> (range (.getHeight img)) (map #(= [255 0 0] (rgb img x %))) (partition-by identity) (filter first)
                       (map count) (reduce max 0)))))

(def ^:private cyl (vec (concat (repeat 11 {:cyl "4"}) (repeat 7 {:cyl "6"}) (repeat 14 {:cyl "8"}))))
(def ^:private values [{:g "a" :v 1.0} {:g "a" :v 2.0} {:g "a" :v 6.0} {:g "b" :v 4.0}])
(def ^:private two-groups [{:l "p" :c "x" :v 2} {:l "p" :c "y" :v 3} {:l "q" :c "x" :v 5} {:l "q" :c "y" :v 1}])

(deftest bar-chart
  (testing "1.1 one bar per level, horizontal by default; flip draws vertical bars"
    (let [p {:data [{:l "a" :v 3} {:l "b" :v 5}] :mapping {:x :v :y :l} :charts [(chart/bar)]}]
      (is (= [["a" 0.0 3.0] ["b" 0.0 5.0]] (ends p)))
      (let [img (plot/render-image (assoc p :theme white :charts [(chart/bar {:color [255 0 0] :stroke-color :none})]) size)
            vimg (plot/render-image (assoc p :flip true :theme white :charts [(chart/bar {:color [255 0 0] :stroke-color :none})]) size)]
        (is (< 0 (red-run img) 150) "a horizontal bar is thinner than the panel")
        (is (> (red-run vimg) 150) "the tall vertical bar spans most of the panel height")))))

(deftest bar-stats
  (testing "1.2.1 value stats over :x"
    (are [stat expected] (= expected (mapv :length (bars {:data values :mapping {:x :v :y :g} :charts [(chart/bar {:stat stat})]})))
      :sum [9.0 4.0]
      :mean [3.0 4.0]
      :median [2.0 4.0]
      count [3.0 1.0]))
  (testing "1.2.2 count measures need no :x (mtcars cyl counts)"
    (is (= [11.0 7.0 14.0] (mapv :length (bars {:data cyl :mapping {:y :cyl} :charts [(chart/bar {:stat :count})]}))))
    (is (= [11 7 14] (mapv :n (bars {:data cyl :mapping {:y :cyl} :charts [(chart/bar {:stat :count})]}))))
    (let [pct (mapv :length (bars {:data cyl :mapping {:y :cyl} :charts [(chart/bar {:stat :percent})]}))]
      (is (< (Math/abs (- 100.0 (reduce + pct))) 1e-9)))
    (testing "percent and pmf over each group's total"
      (let [data (concat (map #(assoc % :s "x") cyl) [{:cyl "4" :s "y"} {:cyl "8" :s "y"}])
            bs (bars {:data data :mapping {:y :cyl :color :s} :charts [(chart/bar {:stat :pmf})]})]
        (is (= [0.5 0.5] (mapv :length (filter #(= ["y"] (:key %)) bs))))
        (is (< (Math/abs (- 1.0 (reduce + (map :length (filter #(= ["x"] (:key %)) bs))))) 1e-9))))
    (testing "count rows even with an :x mapping (invalid :x included)"
      (is (= [2.0] (mapv :length (bars {:data [{:l "a" :v 1} {:l "a" :v nil}] :mapping {:x :v :y :l}
                                        :charts [(chart/bar {:stat :count})]}))))))
  (testing "1.2.2 a value stat needs :x; a stat function must return a finite number"
    (is (= :mapping (:key (error #(build {:data cyl :mapping {:y :cyl} :charts [(chart/bar)]})))))
    (is (= :stat (:key (error #(build {:data values :mapping {:x :v :y :g} :charts [(chart/bar {:stat (constantly ##NaN)})]})))))
    (is (= :stat (:key (error #(build {:data values :mapping {:x :v :y :g} :charts [(chart/bar {:stat (constantly "x")})]}))))))
  (testing "1.2.3 shared count measures"
    (is (= [2.0 6.0] (measure/heights :count [2 6] (repeat 1.0))))
    (is (= [25.0 75.0] (measure/heights :percent [2 6] (repeat 1.0))))
    (is (= [0.25 0.75] (measure/heights :pmf [2 6] (repeat 1.0))))
    (is (= [0.125 0.75] (measure/heights :density [2 6] [2.0 1.0])))
    (is (= [0.0 0.0] (measure/heights :percent [0 0] (repeat 1.0))) "zero total")
    (is (= [] (measure/heights :count [] [])))
    (is (= "Count" (get-in (stats {:data cyl :mapping {:y :cyl} :charts [(chart/bar {:stat :count})]} 0) [:axis-titles :x])))
    (testing "R10 value stats over a keyword :x: \"<stat> of <x>\" when a bar aggregates; else none"
      (is (= {:x "sum of v"} (:axis-titles (stats {:data values :mapping {:x :v :y :g} :charts [(chart/bar)]} 0))))
      (is (= {:x "mean of v"} (:axis-titles (stats {:data values :mapping {:x :v :y :g} :charts [(chart/bar {:stat :mean})]} 0))))
      (is (= {} (:axis-titles (stats {:data [{:g "a" :v 1} {:g "b" :v 2}] :mapping {:x :v :y :g} :charts [(chart/bar)]} 0)))
          "one value per bar")
      (is (= {} (:axis-titles (stats {:data values :mapping {:x :v :y :g} :charts [(chart/bar {:stat count})]} 0))) "a stat function")
      (is (= {} (:axis-titles (stats {:data values :mapping {:x #(:v %) :y :g} :charts [(chart/bar)]} 0))) "a function :x"))))

(deftest positions
  (let [p (fn [opts] {:data two-groups :mapping {:x :v :y :l :color :c} :charts [(chart/bar opts)]})
        by (fn [bs] (into {} (map (fn [b] [[(:level b) (first (:key b))] [(:y0 b) (:y1 b)]]) bs)))]
    (testing "1.3.1 dodge is the default with two groups; distinct slots"
      (let [bs (bars (p {}))]
        (is (= {["p" "x"] [0.0 2.0] ["p" "y"] [0.0 3.0] ["q" "x"] [0.0 5.0] ["q" "y"] [0.0 1.0]} (by bs)))
        (is (= 2 (count (distinct (map :slot bs)))))))
    (testing "1.3.1 stack sums per level"
      (is (= {["p" "x"] [0.0 2.0] ["p" "y"] [2.0 5.0] ["q" "x"] [0.0 5.0] ["q" "y"] [5.0 6.0]} (by (bars (p {:position :stack})))))
      (is (= [[-0.5 0.5]] (distinct (map :slot (bars (p {:position :stack})))))))
    (testing "1.3.1 normalize sums to 1 per level"
      (is (= {["p" "x"] [0.0 0.4] ["p" "y"] [0.4 1.0] ["q" "x"] [0.0 0.8333] ["q" "y"] [0.8333 1.0]}
             (update-vals (by (bars (p {:position :normalize}))) (fn [ys] (mapv #(/ (Math/round (* 1e4 (double %))) 1e4) ys)))))
      (is (= "Proportion" (get-in (stats (p {:position :normalize}) 0) [:axis-titles :x]))))
    (testing "1.3.2 a group absent from a level stacks as 0"
      (is (= {["p" "x"] [0.0 2.0] ["q" "y"] [0.0 1.0] ["p" "y"] [2.0 5.0]}
             (by (bars {:data (remove #(= ["q" "x"] [(:l %) (:c %)]) two-groups) :mapping {:x :v :y :l :color :c}
                        :charts [(chart/bar {:position :stack})]})))))
    (testing "1.3.4 overlay groups are translucent, a single group isn't"
      (is (true? (get-in (stats (p {:position :overlay}) 0) [:data :panel :translucent?])))
      (is (false? (get-in (stats {:data two-groups :mapping {:x :v :y :l} :charts [(chart/bar {:position :overlay})]} 0)
                          [:data :panel :translucent?]))))
    (testing "1.3.3 negative values: down from 0, stacked separately; normalize rejects them"
      (let [neg [{:l "p" :c "x" :v -2} {:l "p" :c "y" :v 3} {:l "p" :c "z" :v -1}]
            q (fn [opts] {:data neg :mapping {:x :v :y :l :color :c} :charts [(chart/bar opts)]})]
        (is (= {["p" "x"] [0.0 -2.0] ["p" "y"] [0.0 3.0] ["p" "z"] [0.0 -1.0]} (by (bars (q {})))))
        (is (= {["p" "x"] [-2.0 0.0] ["p" "y"] [0.0 3.0] ["p" "z"] [-3.0 -2.0]} (by (bars (q {:position :stack})))))
        (is (= :position (:key (error #(build (q {:position :normalize}))))))))))

(def ^:private pyramid
  (vec (for [[age f m] [["young" 5 4] ["old" 2 3]] [sex n] [["f" f] ["m" m]] region ["u" "r"]]
         {:age age :sex sex :region region :n (if (= region "u") n (* 2 n))})))

(deftest mirror
  (let [p (fn [mapping opts] {:data pyramid :mapping (merge {:x :n :y :age} mapping) :charts [(chart/bar opts)]})
        by (fn [bs] (into {} (map (fn [b] [[(:level b) (:key b)] [(:y0 b) (:y1 b)]]) bs)))]
    (testing "1.3.5.1 the second level points the other way"
      (is (= {["young" ["f"]] [0.0 15.0] ["young" ["m"]] [-12.0 -0.0] ["old" ["f"]] [0.0 6.0] ["old" ["m"]] [-9.0 -0.0]}
             (by (bars (p {:color :sex} {:mirror :color}))))))
    (testing "1.3.5.2 positions apply per side: two stacks out from 0"
      (is (= {["young" ["f" "r"]] [0.0 10.0] ["young" ["f" "u"]] [10.0 15.0]
              ["young" ["m" "r"]] [-8.0 -0.0] ["young" ["m" "u"]] [-12.0 -8.0]}
             (select-keys (by (bars (p {:color :sex :group :region} {:mirror :color :position :stack})))
                          [["young" ["f" "u"]] ["young" ["f" "r"]] ["young" ["m" "u"]] ["young" ["m" "r"]]]))))
    (testing "1.3.5.3 dodged: both sides share a slot; slots split by the other dimension only"
      (let [bs (bars (p {:color :sex :group :region} {:mirror :color :position :dodge}))
            slot (fn [k] (:slot (first (filter #(= k (:key %)) bs))))]
        (is (= (slot ["f" "u"]) (slot ["m" "u"])))
        (is (not= (slot ["f" "u"]) (slot ["f" "r"])))
        (is (= 2 (count (distinct (map :slot bs)))))))
    (testing "1.3.5.3 the default position follows the slot keys: one per side is overlay (full slot)"
      (is (= [[-0.5 0.5]] (distinct (map :slot (bars (p {:color :sex} {:mirror :color})))))))
    (testing "1.3.5.4 normalize per side; data negatives still throw"
      (let [bs (bars (p {:color :sex :group :region} {:mirror :color :position :normalize}))]
        (is (every? #(<= -1.0 (:y0 %) (:y1 %) 1.0) bs))
        (is (= 1.0 (reduce max (map :y1 (filter #(= "f" (first (:key %))) bs)))))
        (is (= -1.0 (reduce min (map :y0 (filter #(= "m" (first (:key %))) bs))))))
      (is (= :position (:key (error #(build {:data (assoc-in pyramid [0 :n] -1) :mapping {:x :n :y :age :color :sex :group :region}
                                            :charts [(chart/bar {:mirror :color :position :normalize})]}))))))
    (testing "1.3.5.5 magnitudes on the value axis"
      (is (= {:x :absolute} (:tick-format (stats (p {:color :sex} {:mirror :color}) 0))))
      (is (nil? (:tick-format (stats (p {:color :sex} {}) 0)))))
    (testing "1.3.5.6 the mirror dimension must be mapped and have exactly two levels"
      (is (= :mirror (:key (error #(build (p {} {:mirror :color}))))))
      (is (= :mirror (:key (error #(build {:data (conj pyramid {:age "mid" :sex "o" :region "u" :n 1})
                                          :mapping {:x :n :y :age :color :sex} :charts [(chart/bar {:mirror :color})]})))))
      (is (= :mirror (:key (error #(build {:data (filter (fn [r] (= "f" (:sex r))) pyramid)
                                          :mapping {:x :n :y :age :color :sex} :charts [(chart/bar {:mirror :color})]}))))))
    (testing "1.3.5.7 the 0 line on top by default with mirror"
      (is (true? (get-in (stats (p {:color :sex} {:mirror :color}) 0) [:data :panel :zero-line?])))
      (is (false? (get-in (stats (p {:color :sex} {:mirror :color :zero-line false}) 0) [:data :panel :zero-line?]))))))

(deftest geometry
  (testing "1.4.1 :width is the bar's share of the band"
    (let [p (fn [w] {:data [{:l "a" :v 3} {:l "b" :v 5}] :mapping {:x :v :y :l} :theme white
                     :charts [(chart/bar {:width w :color [255 0 0] :stroke-color :none})]})
          wide (red-run (plot/render-image (p 0.8) size))
          narrow (red-run (plot/render-image (p 0.4) size))]
      (is (<= 1.9 (/ (double wide) narrow) 2.1))))
  (testing "1.4.2 :varwidth true - sqrt of the count relative to the largest"
    (let [data (concat (repeat 4 {:l "a" :v 1}) (repeat 1 {:l "b" :v 1}))]
      (is (= [1.0 0.5] (mapv :width-factor (bars {:data data :mapping {:x :v :y :l} :charts [(chart/bar {:varwidth true})]}))))))
  (testing "1.4.2 :varwidth by a column: the item's sum relative to the largest; zero weights; negative weights"
    (let [p (fn [data vw] {:data data :mapping {:x :v :y :l} :charts [(chart/bar {:varwidth vw})]})
          data [{:l "a" :v 1 :w 2} {:l "a" :v 1 :w 2} {:l "b" :v 1 :w 1} {:l "c" :v 1 :w nil}]]
      (is (= [1.0 0.25 0.0] (mapv :width-factor (bars (p data :w)))))
      (is (= [1.0 0.25 0.0] (mapv :width-factor (bars (p data #(:w %))))))
      (is (= [0.0 0.0] (mapv :width-factor (bars (p [{:l "a" :v 1 :w 0} {:l "b" :v 2 :w 0}] :w)))))
      (is (= :varwidth (:key (error #(build (p [{:l "a" :v 1 :w -1}] :w))))))
      (is (nil? (:rows (first (bars (p data :w))))) "rows aren't kept in the stats")))
  (testing "1.4.3 the value extent includes 0; bars start on the axis"
    (let [p (fn [vs] {:data (map-indexed (fn [i v] {:l (str i) :v v}) vs) :mapping {:x :v :y :l} :charts [(chart/bar)]})]
      (are [vs extent padding request] (let [st (stats (p vs) 0)]
                                         (and (= extent (select-keys (get-in st [:extents :panel :x]) [:min :max]))
                                              (= padding (get-in st [:padding :x]))
                                              (= request (get-in st [:padding-explicit :x]))))
        [3 5] {:min 0.0 :max 5.0} [0.0 0.05] [0.0 nil]
        [-3 -5] {:min -5.0 :max 0.0} [0.05 0.0] [nil 0.0]
        [-3 5] {:min -3.0 :max 5.0} [0.05 0.05] nil
        [0 0] {:min 0.0 :max 0.0} [0.0 0.0] [0.0 0.0])
      (testing "the user's :padding wins"
        (is (= [0.1 0.2] (get-in (stats (assoc-in (p [3 5]) [:charts 0 :options :padding] {:x [0.1 0.2]}) 0) [:padding :x]))))))
  (testing "1.5 theme [:chart :bar] is checked; a mapped colour gets a legend"
    (is (= :stroke-width (:key (error #(build {:data values :mapping {:x :v :y :g} :theme {:chart {:bar {:stroke-width -1}}}
                                       :charts [(chart/bar)]})))))
    (is (instance? BufferedImage (plot/render-image {:data two-groups :mapping {:x :v :y :l :color :c}
                                                     :charts [(chart/bar {:position :overlay})]} size)))))

(deftest labels
  (let [p (fn [data opts] {:data data :mapping {:x :v :y :l} :charts [(chart/bar opts)]})
        texts (fn [pl] (mapv :text (keep :label (bars pl))))]
    (testing "1.6.2 value labels: whole numbers as integers, else distinct digits; a format string"
      (is (= ["3" "5"] (texts (p [{:l "a" :v 3} {:l "b" :v 5}] {:value-label true}))))
      (is (= ["1.25" "1.5"] (texts (p [{:l "a" :v 1.25} {:l "b" :v 1.5}] {:value-label true}))))
      (is (= ["3.0" "5.0"] (texts (p [{:l "a" :v 3} {:l "b" :v 5}] {:value-label true :value-format "%.1f"}))))
      (is (= :value-format (:key (error #(build (p [{:l "a" :v 3}] {:value-label true :value-format "%d"})))))))
    (testing "1.6.2 negative and mirrored bars label past their negative end; room on that side"
      (let [pl (p [{:l "a" :v -3} {:l "b" :v 5}] {:value-label true})
            [a b] (keep :label (bars pl))
            [lo hi] (get-in (stats pl 0) [:padding-px :x])]
        (is (= [-3.0 -1.0] [(:at a) (:side a)]))
        (is (= [5.0 1.0] [(:at b) (:side b)]))
        (is (and (pos? lo) (pos? hi))))
      (is (= #{-1.0 1.0} (set (map :side (keep :label (bars {:data pyramid :mapping {:x :n :y :age :color :sex}
                                                                 :charts [(chart/bar {:mirror :color :value-label true})]})))))))
    (testing "R3 a zero-length bar gets no value label (its n label stays)"
      (is (= ["5"] (texts (p [{:l "a" :v 0} {:l "b" :v 5}] {:value-label true}))))
      (is (= ["n=1" "5 n=1"] (texts (p [{:l "a" :v 0} {:l "b" :v 5}] {:value-label true :n-label true})))))
    (testing "R2 value and n label form one label: side by side along horizontal bars, stacked along vertical ones"
      (let [data [{:l "a" :v 3}]
            label (fn [pl] (:text (first (keep :label (bars pl)))))]
        (is (= "3 n=1" (label (p data {:value-label true :n-label true}))))
        (is (= "3\nn=1" (label (assoc (p data {:value-label true :n-label true}) :flip true))))
        (is (= "3\nn=1" (label (p data {:value-label true :n-label true :label-stack :vertical}))))
        (is (= "3 n=1" (label (assoc (p data {:value-label true :n-label true :label-stack :horizontal}) :flip true))))
        (is (= 1 (count (keep :label (bars (p data {:value-label true :n-label true}))))))
        (is (> (second (get-in (stats (p data {:value-label true :n-label true}) 0) [:padding-px :x]))
               (second (get-in (stats (p data {:value-label true}) 0) [:padding-px :x])))
            "room for the joined label")))
    (testing "1.6.1 n labels count the rows of each bar; no labels - no room"
      (is (= ["n=11" "n=7" "n=14"] (texts {:data cyl :mapping {:y :cyl} :charts [(chart/bar {:stat :count :n-label true})]})))
      (is (nil? (:padding-px (stats (p [{:l "a" :v 3}] {}) 0)))))
    (testing "labels render horizontal and flipped"
      (is (instance? BufferedImage (plot/render-image (assoc (p [{:l "a" :v -3} {:l "b" :v 5}] {:value-label true :n-label true})
                                                             :flip true) size))))))

(deftest error-bars
  (testing "1.7 chart/extent with the same mapping sits on the bars' slots (dodged groups too)"
    (let [data (for [l ["p" "q"] c ["x" "y"] v [1 2 4]] {:l l :c c :v v})
          built (build {:data data :mapping {:x :v :y :l :color :c}
                        :charts [(chart/bar {:stat :mean}) (chart/extent {:method :stddev})]})
          bar-slots (into {} (map (juxt (juxt :level :key) :slot) (get-in built [:stats 0 :data :panel :bars])))
          extent-slots (into {} (map (juxt (juxt :level :key) :slot) (get-in built [:stats 1 :data :panel :items])))]
      (is (= 4 (count bar-slots)))
      (is (= bar-slots extent-slots))))
  (testing "R1 with the same :mirror, extents take the bars' slots and side"
    (let [data (for [a ["y" "o"] sx ["f" "m"] v [1 2 4]] {:a a :s sx :v v})
          built (build {:data data :mapping {:x :v :y :a :color :s}
                        :charts [(chart/bar {:stat :mean :mirror :color}) (chart/extent {:method :min-max :mirror :color :n-label true})]})
          bar-of (into {} (map (juxt (juxt :level :key) identity) (get-in built [:stats 0 :data :panel :bars])))
          items (get-in built [:stats 1 :data :panel :items])]
      (is (every? #(= (:slot %) (:slot (bar-of [(:level %) (:key %)]))) items))
      (is (= #{[1.0 4.0] [-4.0 -1.0]} (set (map (juxt :lo :hi) items))))
      (is (every? #(neg? (:center %)) (filter #(= ["m"] (:key %)) items)) "centres negated")
      (is (= {:x :absolute} (get-in built [:stats 1 :tick-format])))
      (let [[lo hi] (get-in built [:stats 1 :padding-px :x])] (is (and (pos? lo) (pos? hi)) "n label room on both sides"))))
  (testing "R1 :mirror with :position :mirror (half slots) is rejected; an unmapped dimension too"
    (is (= :mirror (:key (error #(build {:data two-groups :mapping {:x :v :y :l :color :c}
                                         :charts [(chart/extent {:mirror :color :position :mirror})]})))))
    (is (= :mirror (:key (error #(build {:data two-groups :mapping {:x :v :y :l}
                                         :charts [(chart/extent {:mirror :color})]})))))))

(deftest strips-and-panels
  (testing "1.8 facets: bars per panel; {:facet nil} in every panel"
    (let [data (for [f ["F1" "F2"] l ["a" "b"]] {:f f :l l :v (if (= f "F1") 1 2)})
          built (build {:data data :mapping {:x :v :y :l :facet :f} :charts [(chart/bar)]})]
      (is (= #{[1.0] [2.0]} (set (map (fn [[_ d]] (vec (distinct (map :length (:bars d))))) (get-in built [:stats 0 :data])))))))
  (testing "1.9 without :y: one bar per group, centred"
    (let [bs (bars {:data [{:v 3} {:v 4}] :mapping {:x :v} :charts [(chart/bar)]})]
      (is (= [[nil 0.0 7.0]] (mapv (juxt :level :y0 :y1) bs)))))
  (testing "empty own data: no bars, no error"
    (is (empty? (bars {:data two-groups :mapping {:x :v :y :l} :charts [(chart/bar {:data []})]}))))
  (testing "NA groups are kept as a level of their own"
    (let [bs (bars {:data (conj two-groups {:l "p" :c nil :v 9}) :mapping {:x :v :y :l :color :c}
                    :charts [(chart/bar {:position :stack})]})]
      (is (= 5 (count bs)))
      (is (= [5.0 14.0] ((juxt :y0 :y1) (first (filter #(= [nil] (:key %)) bs)))) "NA stacks last")))
  (testing "strip levels keep their first appearance past 8 levels (box too)"
    (let [ls (mapv #(str "level-" %) (range 12 0 -1))
          data (map-indexed (fn [i l] {:l l :v i}) ls)]
      (is (= ls (mapv :level (bars {:data data :mapping {:x :v :y :l} :charts [(chart/bar)]}))))
      (is (= ls (get-in (build {:data data :mapping {:x :v :y :l} :charts [(chart/box)]}) [:stats 0 :extents :panel :y :values]))))))

(deftest review-2
  (testing "S1 labels of stacked and normalized bars sit centred in their segments; no room for them"
    (let [p (fn [opts] {:data two-groups :mapping {:x :v :y :l :color :c} :charts [(chart/bar (merge {:value-label true} opts))]})
          labels (fn [opts] (into {} (map (fn [b] [[(:level b) (first (:key b))] ((juxt :at :side :text) (:label b))]) (bars (p opts)))))]
      (is (= {["p" "x"] [1.0 0.0 "2"] ["p" "y"] [3.5 0.0 "3"] ["q" "x"] [2.5 0.0 "5"] ["q" "y"] [5.5 0.0 "1"]}
             (labels {:position :stack})))
      (is (every? #(zero? (second %)) (vals (labels {:position :normalize}))))
      (is (= [5.0 1.0 "5"] (get (labels {:position :dodge}) ["q" "x"])) "dodged labels stay past the end")
      (is (= [0.0 0.0] (get-in (stats (p {:position :stack}) 0) [:padding-px :x])) "centred labels need no room")
      (testing "mirrored stacks centre on the negative side"
        (is (every? #(and (neg? (:at %)) (zero? (:side %)))
                    (keep :label (filter :mirrored? (bars {:data pyramid :mapping {:x :n :y :age :color :sex :group :region}
                                                           :charts [(chart/bar {:mirror :color :position :stack :value-label true})]}))))))
      (testing "contrast colour: white text on a dark fill"
        (let [img (plot/render-image {:data [{:l "a" :v 10}] :mapping {:x :v :y :l} :theme white
                                      :charts [(chart/bar {:position :stack :value-label true :color [0 0 0] :stroke-color :none})]}
                                     size)]
          (is (some (fn [c] (every? #(> (long %) 200) c)) (for [x (range 150 300) y (range 100 200)] (rgb img x y))))))))
  (testing "S2 the legend key is translucent exactly when the bars are"
    (let [key-rgb (fn [p]
                    (let [built (build p)
                          canvas (c2d/canvas 20 20)]
                      (c2d/with-canvas-> canvas (c2d/set-background :white))
                      (c2d/with-canvas [c canvas]
                        (bar/legend (first (:charts p)) {:color [255 0 0]} c {:x 0 :y 0 :w 20 :h 20}
                                    {:theme theme/default-theme :chart-stats (get-in built [:stats 0])}))
                      (rgb (c2d/get-image canvas) 10 10)))
          single (filter #(= "x" (:c %)) two-groups)]
      (is (not= [255 0 0] (key-rgb {:data two-groups :mapping {:x :v :y :l :color :c} :charts [(chart/bar {:position :overlay})]})))
      (is (= [255 0 0] (key-rgb {:data (concat single (map #(assoc % :g "h") single)) :mapping {:x :v :y :l :color :c :group :g}
                                 :charts [(chart/bar {:position :overlay :mirror :group})]}))
          "one colour level, mirrored by :group - solid bars, solid key")
      (is (= [255 0 0] (key-rgb {:data two-groups :mapping {:x :v :y :l :color :c} :charts [(chart/bar {:position :stack})]})))))
  (testing "S5 mirror + extent with dodged groups beside the mirror dimension; the :group recipe keeps slots"
    (let [built (build {:data pyramid :mapping {:x :n :y :age :color :sex :group :region}
                        :charts [(chart/bar {:mirror :color :position :dodge})
                                 (chart/extent {:mirror :color})
                                 (chart/extent {:mirror :group :mapping {:color nil :group :sex}})]})
          slots (fn [i k] (into {} (map (juxt (juxt :level :key) :slot) (get-in built [:stats i :data :panel k]))))
          bar-slots (slots 0 :bars)]
      (is (= bar-slots (slots 1 :items)))
      (is (= 2 (count (distinct (vals bar-slots)))))
      (is (= #{[-0.5 0.5]} (set (vals (slots 2 :items)))) "the :group recipe without the other dimension: one slot per side")
      (is (every? #(neg? (:hi %)) (filter :mirrored? (get-in built [:stats 1 :data :panel :items]))))))
  (testing "S5 a vertical stack is narrower along horizontal bars than side by side"
    (let [room (fn [stack] (second (get-in (stats {:data [{:l "a" :v 3}] :mapping {:x :v :y :l}
                                                   :charts [(chart/bar {:value-label true :n-label true :label-stack stack})]} 0)
                                           [:padding-px :x])))]
      (is (< (room :vertical) (room :horizontal)))))
  (testing "S6 strip charts without a negative side don't accept :mirror"
    (is (some? (error #(build {:data two-groups :mapping {:x :v :y :l :color :c} :charts [(chart/box {:mirror :color})]}))))))
