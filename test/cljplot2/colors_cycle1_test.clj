(ns cljplot2.colors-cycle1-test
  "Colors cycle 1: level maps, `:spread`, `:midpoint`, `:reverse`, identity and binned aesthetic scales."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [cljplot2.test-util :refer [error error-data]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.data.group :as group]
            [cljplot2.data.levels :as levels]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- build [p] (plot/build p {:width 600 :height 400}))

(def ^:private iris-like
  [{:x 1 :y 1 :sp "setosa" :v -2 :n 1931}
   {:x 2 :y 2 :sp "versicolor" :v 0 :n 1932}
   {:x 3 :y 3 :sp "virginica" :v 8 :n 1931}])

(defn- plot-of
  ([mapping scales] (plot-of iris-like mapping scales))
  ([data mapping scales] {:data data :mapping (merge {:x :x :y :y} mapping) :scales scales :charts [(chart/scatter)]}))

(defn- spec [built k] (get-in built [:scale-specs k]))
(defn- attr
  "Attribute of value `v` under the built scale of `k` (`nil` NA)."
  [built k v]
  ((aesthetic/attribute-fn k (spec built k) :na-marker) v))
(defn- rgb [col] (mapv #(Math/round (double %)) (take 3 (c/to-color col))))
(defn- key-of [mapping scales] (:key (error #(build (plot-of mapping scales)))))
(defn- legends [built] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in built [:grid :items])))

;; 1. level maps

(deftest level-map-test
  (testing "1.1 colors, shapes, sizes by level; numbers match as doubles"
    (let [b (build (plot-of {:color :sp} {:color {:range {"setosa" :red "versicolor" :grey "virginica" :grey}}}))]
      (is (= (rgb :red) (rgb (attr b :color "setosa"))))
      (is (= (rgb :grey) (rgb (attr b :color "virginica"))))
      (is (= ["setosa" "versicolor" "virginica"] (mapv :label (:entries (first (legends b))))) "legend in domain order")
      (is (= (rgb :red) (rgb (get-in (first (legends b)) [:entries 0 :attributes :color])))))
    (let [b (build (plot-of {:shape :sp} {:shape {:range {"setosa" \O "versicolor" \S "virginica" \A}}}))]
      (is (= [\O \S \A] (mapv #(attr b :shape %) ["setosa" "versicolor" "virginica"]))))
    (let [b (build (plot-of {:size :sp} {:size {:range {"setosa" 2 "versicolor" 9 "virginica" 4}}}))]
      (is (= [2 9 4] (mapv #(attr b :size %) ["setosa" "versicolor" "virginica"]))))
    (let [b (build (plot-of {:color :n} {:color {:type :categorical :range {1931.0 :red 1932 :blue}}}))]
      (is (= (rgb :red) (rgb (attr b :color 1931))) "1931.0 key = 1931 level")))
  (testing "1.2 a missing level throws naming it"
    (let [e (error-data #(build (plot-of {:color :sp} {:color {:range {"setosa" :red "versicolor" :grey}}})))]
      (is (= :range (:key e)))
      (is (str/includes? (:message (first (:errors e))) "virginica"))))
  (testing "1.3 a level map on non-ordinal scales throws"
    (is (= :range (key-of {:color :v} {:color {:range {0 :red}}})) "inferred linear")
    (is (= :range (key-of {:color :v} {:color {:kind :linear :range {0 :red}}})) "explicit linear")
    (is (= :range (key-of {:color :v} {:color {:kind :binned :range {0 :red}}})))
    (is (= :range (key-of {:color :sp} {:color {:kind :identity :range {"setosa" :red}}}))))
  (testing "1.3 an empty map or invalid attributes are rejected"
    (is (= :range (key-of {:color :sp} {:color {:range {}}})))
    (is (= :range (key-of {:color :sp} {:color {:range {"setosa" :no-such-color}}})))
    (is (= :range (key-of {:shape :sp} {:shape {:range {"setosa" :circle}}})))
    (is (= :range (key-of {:size :sp} {:size {:range {"setosa" -1}}}))))
  (testing "1.4 extra keys ignored"
    (let [b (build (plot-of {:color :sp} {:color {:range {"setosa" :red "versicolor" :grey "virginica" :grey "other" :green}}}))]
      (is (= 3 (count (:entries (first (legends b)))))))))

;; 2. spread

(deftest spread-test
  (let [blues (c/palette :blues-9)]
    (testing "2.1 three levels: lightest, middle, darkest"
      (let [b (build (plot-of {:color :sp} {:color {:range :blues-9 :spread true}}))]
        (is (= (mapv rgb (c/resample blues 3)) (mapv #(rgb (attr b :color %)) ["setosa" "versicolor" "virginica"])))
        (is (= (rgb (first blues)) (rgb (attr b :color "setosa"))))
        (is (= (rgb (last blues)) (rgb (attr b :color "virginica"))))))
    (testing "2.1 more levels than colors: all distinct"
      (let [data (for [i (range 12)] {:x i :y i :g (str (char (+ 97 i)))})
            b (build (plot-of data {:color :g} {:color {:range :blues-5 :spread true}}))]
        (is (= 12 (count (distinct (map #(rgb (attr b :color (str (char (+ 97 %))))) (range 12))))))))
    (testing "2.4 one level: the palette average"
      (let [b (build (plot-of [{:x 1 :y 1 :g "a"}] {:color :g} {:color {:range :blues-9 :spread true}}))]
        (is (= (rgb (first (c/resample blues 1))) (rgb (attr b :color "a"))))))
    (testing ":spread false is the default"
      (let [b (build (plot-of {:color :sp} {:color {:range :blues-9 :spread false}}))]
        (is (= (rgb (nth blues 1)) (rgb (attr b :color "versicolor")))))))
  (testing "2.2 / 2.3 misuse"
    (is (= :spread (key-of {:shape :sp} {:shape {:spread true}})))
    (is (= :spread (key-of {:size :sp} {:size {:spread true}})))
    (is (= :spread (key-of {:color :v} {:color {:spread true}})) "inferred linear")
    (is (= :spread (key-of {:color :v} {:color {:kind :binned :spread true}})))
    (is (= :spread (key-of {:color :sp} {:color {:spread true :range {"setosa" :red "versicolor" :red "virginica" :red}}})))))

;; 3. midpoint

(def ^:private t-range "A color range function returning `t` itself, to read positions." (fn [t] t))

(deftest midpoint-test
  (testing "3.1 symmetric around the midpoint"
    (let [b (build (plot-of {:color :v} {:color {:range t-range :midpoint 0}}))]
      (is (= [0.375 0.5 1.0] (mapv #(attr b :color %) [-2 0 8])))))
  (testing "3.1 zero span at the midpoint: 0.5"
    (let [b (build (plot-of [{:x 1 :y 1 :v 3}] {:color :v} {:color {:range t-range :midpoint 3}}))]
      (is (= 0.5 (attr b :color 3)))))
  (testing "3.3 a midpoint outside the domain colours one side"
    (let [b (build (plot-of [{:x 1 :y 1 :v 1} {:x 2 :y 2 :v 5}] {:color :v} {:color {:range t-range :midpoint 0}}))]
      (is (= [0.6 1.0] (mapv #(attr b :color %) [1 5])))))
  (testing "3.2 misuse"
    (is (= :midpoint (key-of {:size :v} {:size {:midpoint 0}})))
    (is (= :midpoint (key-of {:color :sp} {:color {:midpoint 0}})) "ordinal")
    (is (= :midpoint (key-of {:color :sp} {:color {:kind :identity :midpoint 0}})))
    (is (= :midpoint (key-of {:color :v} {:color {:midpoint ##NaN}})))))

;; 4. reverse

(deftest reverse-test
  (testing "4.1 ordinal: assigned colors reversed"
    (let [pal (c/palette :category20)
          b (build (plot-of {:color :sp} {:color {:reverse true}}))]
      (is (= (mapv rgb [(pal 2) (pal 1) (pal 0)]) (mapv #(rgb (attr b :color %)) ["setosa" "versicolor" "virginica"])))))
  (testing "4.1 ordinal spread then reversed"
    (let [b (build (plot-of {:color :sp} {:color {:range :blues-9 :spread true :reverse true}}))]
      (is (= (rgb (last (c/palette :blues-9))) (rgb (attr b :color "setosa"))))))
  (testing "4.1 linear: t -> 1 - t, after the midpoint"
    (let [b (build (plot-of {:color :v} {:color {:range t-range :reverse true}}))]
      (is (= [1.0 0.0] (mapv #(attr b :color %) [-2 8]))))
    (let [b (build (plot-of {:color :v} {:color {:range t-range :midpoint 0 :reverse true}}))]
      (is (= [0.625 0.5 0.0] (mapv #(attr b :color %) [-2 0 8])))))
  (testing "4.1 linear size reversed: biggest for the smallest value"
    (let [b (build (plot-of {:size :v} {:size {:range [1 9] :reverse true}}))]
      (is (= [9.0 1.0] (mapv #(attr b :size %) [-2 8])))))
  (testing "4.1 ordinal shapes reversed"
    (let [b (build (plot-of {:shape :sp} {:shape {:range [\O \S \A] :reverse true}}))]
      (is (= [\A \S \O] (mapv #(attr b :shape %) ["setosa" "versicolor" "virginica"])))))
  (testing "4.2 misuse"
    (is (= :reverse (key-of {:color :sp} {:color {:reverse true :range {"setosa" :red "versicolor" :red "virginica" :red}}})))
    (is (= :reverse (key-of {:color :sp} {:color {:kind :identity :reverse true}}))))
  (testing "4.3 legend entries keep domain order with reversed colors"
    (let [pal (c/palette :category20)
          b (build (plot-of {:color :sp} {:color {:reverse true}}))]
      (is (= ["setosa" "versicolor" "virginica"] (mapv :label (:entries (first (legends b))))))
      (is (= (rgb (pal 2)) (rgb (get-in (first (legends b)) [:entries 0 :attributes :color])))))))

;; 5. identity

(def ^:private colored
  [{:x 1 :y 1 :c :red} {:x 2 :y 2 :c "#00ff00"} {:x 3 :y 3 :c [0 0 255]} {:x 4 :y 4 :c nil}])

(deftest identity-test
  (testing "5.1 values are the attributes"
    (let [b (build (plot-of colored {:color :c} {:color {:kind :identity}}))]
      (is (= :red (attr b :color :red)))
      (is (= [0 0 255] (attr b :color [0 0 255])))
      (is (= 4 (count (get-in b [:stats 0 :data :panel :x]))) "every point kept (NA included)")
      (is (= :na-marker (attr b :color nil)) "5.2 nil is NA")))
  (testing "5.1 shapes and sizes"
    (let [b (build (plot-of [{:x 1 :y 1 :s \O :z 3} {:x 2 :y 2 :s \A :z 0}] {:shape :s :size :z}
                            {:shape {:kind :identity} :size {:kind :identity}}))]
      (is (= \A (attr b :shape \A)))
      (is (= 0.0 (attr b :size 0)) "zero size")))
  (testing "5.2 an invalid value throws naming it"
    (let [e (error #(build (plot-of [{:x 1 :y 1 :c :no-such-color}] {:color :c} {:color {:kind :identity}})))]
      (is (= :mapping (:key e)))
      (is (= :no-such-color (:value e))))
    (is (= :mapping (:key (error #(build (plot-of [{:x 1 :y 1 :z -1}] {:size :z} {:size {:kind :identity}}))))))
    (is (= :mapping (:key (error #(build (plot-of [{:x 1 :y 1 :c :none}] {:color :c} {:color {:kind :identity}})))))))
  (testing "5.3 no level cap"
    (let [data (vec (for [i (range 300)] {:x i :y i :c [(mod i 256) 0 (- 255 (mod i 256))]}))
          p (plot-of data {:color :c} {:color {:kind :identity}})]
      (is (= 300 (count (get-in (build p) [:stats 0 :data :panel :x]))))
      (is (some? (plot/render p {:width 300 :height 200})))))
  (testing "5.4 no legend"
    (is (nil? (legends (build (plot-of colored {:color :c} {:color {:kind :identity}}))))))
  (testing "5.5 groups by distinct value in data order"
    (let [rows [{:x 1 :y 1 :c :red} {:x 2 :y 2 :c :blue} {:x 3 :y 1 :c :red} {:x 4 :y 3 :c :green}]
          split (group/split {:p rows} {:color :c} {:plot-scales {:color {:kind :identity}}})]
      (is (= [[:red] [:blue] [:green]] (mapv :key (get-in split [:panels :p])))))
    (let [b (build {:data colored :mapping {:x :x :y :y :color :c} :scales {:color {:kind :identity}} :charts [(chart/line)]})]
      (is (= [:red "#00ff00" [0 0 255] nil] (mapv #(get-in % [:aesthetics :color]) (get-in b [:stats 0 :data :panel :lines])))
          "line chart: one line per color, data order, NA last")))
  (testing "5.6 contradictions"
    (are [extra k] (= k (key-of {:color :c} {:color (merge {:kind :identity} extra)}))
      {:domain [:red]} :domain
      {:range [:red]} :range
      {:spread true} :spread
      {:midpoint 0} :midpoint
      {:reverse true} :reverse
      {:bins 3} :bins
      {:type :categorical} :type
      {:type :numeric} :type))
  (testing "5.7 never inferred: color keywords are ordinal levels"
    (is (= :ordinal (:kind (spec (build (plot-of [{:x 1 :y 1 :c :red} {:x 2 :y 2 :c :blue}] {:color :c} {})) :color))))))

;; 6. binned

(def ^:private spread-values (vec (for [v [0 5 10 15 20 35 50 51]] {:x v :y v :v v})))

(deftest binned-test
  (testing "6.1 / 6.2 breaks vector: bins [a b), the last [a b]"
    (let [b (build (plot-of (pop spread-values) {:color :v} {:color {:kind :binned :bins [0 10 20 50] :range [:red :green :blue]}}))
          s (spec b :color)]
      (is (= [0.0 50.0] (:domain s)))
      (is (= [0.0 10.0 20.0 50.0] (:breaks s)))
      (is (= (mapv rgb [:red :red :green :green :blue :blue :blue])
             (mapv #(rgb (attr b :color %)) [0 9.999 10 15 20 35 50])))))
  (testing "6.6 values outside the breaks dropped"
    (let [b (build (plot-of spread-values {:color :v} {:color {:kind :binned :bins [0 10 20 50]}}))]
      (is (= 1 (get-in b [:stats 0 :dropped :color])))
      (is (nil? (attr b :color 51)))))
  (testing "6.2 a count gives equal bins over the domain"
    (let [b (build (plot-of [{:x 0 :y 0 :v 0} {:x 1 :y 1 :v 100}] {:color :v} {:color {:kind :binned :bins 4}}))]
      (is (= [0.0 25.0 50.0 75.0 100.0] (:breaks (spec b :color)))))
    (let [b (build (plot-of [{:x 0 :y 0 :v 0} {:x 1 :y 1 :v 100}] {:color :v} {:color {:kind :binned :bins 1}}))]
      (is (= [0.0 100.0] (:breaks (spec b :color))) "one bin")))
  (testing "6.2 explicit domain with a count; with a matching or differing breaks vector"
    (let [b (build (plot-of {:color :v} {:color {:kind :binned :domain [-10 10] :bins 2}}))]
      (is (= [-10.0 0.0 10.0] (:breaks (spec b :color))))
      (is (= (attr b :color 0) (attr b :color 8)) "0 and 8 share the upper bin [0 10]"))
    (is (some? (build (plot-of {:color :v} {:color {:kind :binned :domain [-2 8] :bins [-2 0 8]}}))))
    (is (= :bins (key-of {:color :v} {:color {:kind :binned :domain [-2 9] :bins [-2 0 8]}}))))
  (testing "6.2 a single data value"
    (let [b (build (plot-of [{:x 0 :y 0 :v 3}] {:color :v} {:color {:kind :binned :bins 2}}))]
      (is (= [2.5 3.5] (:breaks (spec b :color))) "one bin of width 1 around it, as levels/breaks")))
  (testing "6.3 default 5 bins"
    (is (= 6 (count (:breaks (spec (build (plot-of {:color :v} {:color {:kind :binned}})) :color))))))
  (testing "6.4 a palette of exactly n colors gives its colors; default range is the theme gradient"
    (let [b (build (plot-of [{:x 0 :y 0 :v 0} {:x 1 :y 1 :v 5}] {:color :v} {:color {:kind :binned :bins 5 :range :blues-5}}))]
      (is (= (mapv rgb (c/palette :blues-5)) (mapv #(rgb (attr b :color %)) [0.5 1.5 2.5 3.5 4.5]))))
    (let [b (build (plot-of {:color :v} {:color {:kind :binned :bins 2}}))
          g (:gradient (:palette theme/default-theme))]
      (is (= (rgb (first g)) (rgb (attr b :color -2))))
      (is (= (rgb (last g)) (rgb (attr b :color 8))))))
  (testing "6.5 midpoint: bins symmetric around it get mirror positions"
    (let [b (build (plot-of [{:x 0 :y 0 :v -2} {:x 1 :y 1 :v 2}] {:color :v}
                            {:color {:kind :binned :bins [-2 -1 0 1 2] :midpoint 0 :range t-range}}))]
      (is (= [0.125 0.375 0.625 0.875] (mapv #(attr b :color %) [-1.5 -0.5 0.5 1.5])))))
  (testing "4.1 binned reversed"
    (let [b (build (plot-of {:color :v} {:color {:kind :binned :bins 3 :range [:red :green :blue] :reverse true}}))]
      (is (= (rgb :blue) (rgb (attr b :color -2))))))
  (testing "6.7 binned aesthetics don't group (a line colors along its length instead, cycle 2)"
    (is (= :mapping (:key (error #(build {:data iris-like :mapping {:x :x :color :v}
                                          :scales {:color {:kind :binned}} :charts [(chart/density)]}))))))
  (testing "6.8 misuse"
    (is (= :kind (key-of {:shape :sp} {:shape {:kind :binned}})))
    (is (= :kind (key-of {:size :v} {:size {:kind :binned}})))
    (is (= :kind (key-of {:color :sp} {:color {:kind :binned}})) "non-numeric data")
    (is (= :bins (key-of {:color :v} {:color {:bins 3}})) "bins without binned")
    (is (= :bins (key-of {:color :v} {:color {:kind :linear :bins 3}})))
    (is (= :type (key-of {:color :v} {:color {:kind :binned :type :categorical}})))
    (is (some? (build (plot-of {:color :v} {:color {:kind :binned :type :numeric}}))) ":type :numeric allowed")
    (is (= :bins (key-of {:color :v} {:color {:kind :binned :bins :no-such-method}})) "unknown method (methods supported since cycle 3)")
    (is (= :bins (key-of {:color :v} {:color {:kind :binned :bins [0 10 5]}})))
    (is (= :bins (key-of {:color :v} {:color {:kind :binned :bins [0]}})))
    (is (= :bins (key-of {:color :v} {:color {:kind :binned :bins 0}})))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- bar-column
  "Pixels down the color bar of plot `p` (white background)."
  [p]
  (let [built (build p)
        item (some #(when (= :legends (:id %)) %) (get-in built [:grid :items]))
        [{:keys [x y h]}] (measure/call-with-scratch-canvas
                           :high #(legend/legend-boxes (:content item) {:canvas % :theme (:theme built)}
                                                       (grid/item-box (:layout built) :legends)))
        c (plot/render p {:width 600 :height 400})]
    (for [py (range y (+ y h))] (pixel c (+ x 4) py))))

(deftest stepped-colorbar-test
  (let [p (merge (plot-of (pop spread-values) {:color :v} {:color {:kind :binned :bins [0 10 20 50] :range [:red :lime :blue]}})
                 {:theme {:plot {:background :white}}})
        [cb] (legends (build p))]
    (testing "6.9 content: binned scale, ticks at inner breaks"
      (is (= :colorbar (:kind cb)))
      (is (= :binned (get-in cb [:scale :kind])))
      (is (= [10.0 20.0] (:ticks cb)))
      (is (= ["10" "20"] (:labels cb))))
    (testing "6.9 bar: three equal blocks, blue on top"
      (let [runs (->> (bar-column p)
                      (map #(cond (= % [0 0 255]) :blue (= % [0 255 0]) :lime (= % [255 0 0]) :red))
                      (partition-by identity)
                      (filter (comp some? first)))
            sizes (map count runs)]
        (is (= [:blue :lime :red] (map first runs)))
        (is (<= (- (apply max sizes) (apply min sizes)) 1) "equal heights"))))
  (testing "6.9 count bins: compact distinct labels; a :formatter wins"
    (let [p (plot-of [{:x 0 :y 0 :v -1.0} {:x 1 :y 1 :v 4.9}] {:color :v} {:color {:kind :binned :bins 6}})]
      (is (= ["-0.0167" "0.967" "1.95" "2.93" "3.92"] (:labels (first (legends (build p)))))))
    (let [p (plot-of {:color :v} {:color {:kind :binned :bins [-2 0 8] :formatter #(str "<" % ">")}})]
      (is (= ["<0.0>"] (:labels (first (legends (build p))))))))
  (testing "one bin: no ticks, still drawn"
    (let [p (plot-of {:color :v} {:color {:kind :binned :bins 1}})]
      (is (= [] (:ticks (first (legends (build p))))))
      (is (some? (plot/render p {:width 600 :height 400})))))
  (testing "4.3 reversed linear color bar: red on top"
    (let [p (merge (plot-of {:color :v} {:color {:range [[255 0 0] [0 0 255]] :reverse true}})
                   {:theme {:plot {:background :white}}})
          bar (filter (fn [[r g b]] (and (< g 40) (> (+ r b) 200))) (bar-column p))]
      (is (> (first (first bar)) 240) "top red")
      (is (> (nth (last bar) 2) 240) "bottom blue"))))

(deftest break-labels-test
  (is (= ["0" "1000" "123456"] (levels/break-labels [0 1000.0 123456])) "whole numbers as integers")
  (is (= ["0" "0.333" "0.667" "1"] (levels/break-labels [0 (/ 1.0 3) (/ 2.0 3) 1])) "3 digits")
  (is (= ["1" "1.0001" "1.0002"] (levels/break-labels [1 1.0001 1.0002])) "more digits to stay distinct")
  (is (= ["2.5" "3.5"] (levels/break-labels [2.5 3.5])))
  (is (= 3 (levels/distinct-digits [7.0] 3)) "one value"))

;; review fixes

(deftest review-fixes-test
  (testing "F1 non-numeric data on a declared linear / binned scale throws :kind"
    (is (= :kind (key-of {:color :sp} {:color {:kind :binned :bins [0 1 2]}})) "breaks vector")
    (is (= :kind (key-of {:color :sp} {:color {:kind :binned :domain [0 2]}})))
    (is (= :kind (key-of {:color :sp} {:color {:domain [0 2]}})) "two-number domain = linear")
    (is (= :kind (key-of {:size :sp} {:size {:kind :linear :domain [0 2]}})))
    (is (= 1 (get-in (build (plot-of {:color :v} {:color {:domain [0 10]}})) [:stats 0 :dropped :color]))
        "numbers outside the domain are still dropped"))
  (testing "F2 treemap follows the aesthetic value rules"
    (let [p (fn [cs scale] {:data [{:g "a" :v 1 :c (cs 0)} {:g "b" :v 2 :c (cs 1)}]
                            :mapping {:value :v :color :c :hierarchy [:g]} :scales {:color scale} :charts [(chart/treemap)]})]
      (is (= 0 (get-in (build (p [[255 0 0] [0 0 255]] {:kind :identity})) [:stats 0 :dropped :color])) "vector colors kept")
      (is (= :mapping (:key (error #(build (p [:red :no-such-color] {:kind :identity}))))))
      (is (= :kind (:key (error #(build (p ["x" "y"] {:kind :binned :bins [0 1 2]}))))))))
  (testing "F3 identity colors of mixed forms (integer, keyword, vector)"
    (let [b (build (plot-of [{:x 1 :y 1 :c 0xff0000} {:x 2 :y 2 :c :blue} {:x 3 :y 3 :c [0 255 0]}] {:color :c} {:color {:kind :identity}}))]
      (is (= 3 (count (get-in b [:stats 0 :data :panel :x]))))
      (is (= 0xff0000 (attr b :color 0xff0000))))
    (let [rows [{:x 1 :y 1 :c 0xff0000} {:x 2 :y 2 :c :blue}]]
      (is (some? (build {:data rows :mapping {:x :x :y :y :color :c} :scales {:color {:kind :identity}} :charts [(chart/line)]})))))
  (testing "F4 level map on numbers hints at :type :categorical"
    (let [e (error-data #(build (plot-of {:color :n} {:color {:range {1931 :red 1932 :blue}}})))]
      (is (= :range (:key e)))
      (is (str/includes? (:message (first (:errors e))) ":type :categorical"))))
  (testing "F5 gradient names"
    (let [g (c/gradient :viridis/viridis)
          b (build (plot-of {:color :v} {:color {:range :viridis/viridis}}))]
      (is (= (rgb (g 0.0)) (rgb (attr b :color -2))))
      (is (= (rgb (g 1.0)) (rgb (attr b :color 8)))))
    (let [b (build (plot-of {:color :v} {:color {:kind :binned :bins 2 :range :viridis/viridis}}))]
      (is (= (rgb ((c/gradient :viridis/viridis) 1.0)) (rgb (attr b :color 8)))))
    (let [b (build (plot-of {:color :sp} {:color {:range :viridis/viridis :spread true}}))]
      (is (= (mapv rgb (c/resample (c/gradient :viridis/viridis) 3)) (mapv #(rgb (attr b :color %)) ["setosa" "versicolor" "virginica"])))
      "ordinal with :spread")
    (let [b (build (plot-of {:color :sp} {:color {:range t-range :spread true}}))]
      (is (some? (attr b :color "setosa")) "a function range with :spread"))
    (is (= :range (key-of {:color :sp} {:color {:range :viridis/viridis}})) "ordinal without :spread")
    (is (= :range (key-of {:color :v} {:color {:range :no-such-gradient}})))
    (is (= :range (key-of {:shape :sp} {:shape {:range :viridis/viridis}}))))
  (testing "F6c shape can't be linear (rule now in option-problems)"
    (is (= :kind (key-of {:shape :sp} {:shape {:kind :linear}}))))
  (testing "F6e huge whole breaks"
    (is (= ["0" "100000000000000000000"] (levels/break-labels [0 1e20]))))
  (testing "F6f midpoint tick on the color bar"
    (let [ticks (fn [scales] (:ticks (first (legends (build (plot-of {:color :v} scales))))))]
      (is (some #(== 0.5 %) (ticks {:color {:midpoint 0.5}})))
      (is (not-any? #(== 0.0 %) (ticks {:color {:midpoint 0.5}})) "a colliding nice tick dropped")
      (is (= (ticks {:color {}}) (ticks {:color {:midpoint 20}})) "outside the domain: no tick")
      (is (= (ticks {:color {}}) (ticks {:color {:midpoint 0}})) "already a tick"))))

(deftest midpoint-label-test
  (testing "notebook cycle: a midpoint tick is labelled compactly and leaves the other labels as they were"
    (let [data [{:x 0 :y 0 :v 52} {:x 1 :y 1 :v 335}]
          labels (fn [scale] (:labels (first (legends (build (plot-of data {:color :v} {:color scale}))))))]
      (is (= ["100" "147" "200" "250" "300"] (labels {:midpoint 146.6875})))
      (is (= (labels {}) ["100" "150" "200" "250" "300"]) "without a midpoint")
      (is (some #{"<146.6875>"} (labels {:midpoint 146.6875 :formatter #(str "<" % ">")})) "a :formatter labels it")
      (is (= ["100" "150" "200" "250" "300"] (labels {:midpoint 150})) "a midpoint on a tick adds nothing"))))
