(ns cljplot2.cycle6-fixes-test
  "Cycle 6 review fixes: strip group reading order, violin legend key, zero lines, `:fill` mapping."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(def ^:private rows
  (vec (for [i (range 80)] {:v (+ (mod (* i 37) 23) (* 3 (mod i 4))) :lvl (["a" "b"] (mod i 2)) :g (["p" "q"] (mod (quot i 2) 2))})))

(def ^:private options {:width 500 :height 400})
(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none :bands-fill :none}}})

(defn- built [p] (plot/build p options))
(defn- items [b] (get-in b [:stats 0 :data facet/single-panel :items]))

(defn- slot-of [b group]
  (:slot (first (filter #(= [group] (:key %)) (items b)))))

;; 1. reading order

(deftest reading-order-test
  (testing "dodge: the first group on top unflipped (high cross end), on the left flipped (low end)"
    (let [unflipped (built {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/box)]})
          flipped (built {:data rows :mapping {:x :v :y :lvl :color :g} :flip true :charts [(chart/box)]})]
      (is (> (double (first (slot-of unflipped "p"))) (double (first (slot-of unflipped "q")))))
      (is (< (double (first (slot-of flipped "p"))) (double (first (slot-of flipped "q")))))))
  (testing "pixels: unflipped p is above q (smaller pixel y), flipped p left of q"
    (doseq [flip [false true]
            :let [b (built {:data rows :mapping {:x :v :y :lvl :color :g} :flip flip :charts [(chart/box)]})
                  {:keys [w h]} (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %)))
                  pl (cljplot2.data.strip/placement {:w w :h h} (:finalized-scales b) {} :band)
                  at (fn [g] (let [^fastmath.vector.Vec2 v ((:point pl) 10 "a" (cljplot2.data.strip/in-slot (slot-of b g) 0.0))]
                               (if flip (.x v) (.y v))))]]
      (is (< (double (at "p")) (double (at "q"))) (str "flip " flip))))
  (testing "mirror: the first group's half on top / on the left"
    (is (= [0.0 0.5] (slot-of (built {:data rows :mapping {:x :v :y :lvl :color :g}
                                      :charts [(chart/violin {:position :mirror})]}) "p")))
    (is (= [-0.5 0.0] (slot-of (built {:data rows :mapping {:x :v :y :lvl :color :g} :flip true
                                       :charts [(chart/violin {:position :mirror})]}) "p"))))
  (testing "three groups: first on top, then down"
    (let [rs (map #(assoc % :g3 (["x" "y" "z"] (mod (:v %) 3))) rows)
          b (built {:data rs :mapping {:x :v :y :lvl :color :g3} :charts [(chart/stripplot {:rng (fastmath.random/rng :mersenne 1)})]})
          lo #(double (first (:slot (first (filter (fn [it] (= [%] (:key it))) (items b))))))]
      (is (> (lo "x") (lo "y") (lo "z")))))
  (testing "ridgeline mirrors unchanged, flipped or not: the first group grows toward the next level"
    (doseq [flip [false true]
            :let [b (built {:data rows :mapping {:x :v :y :lvl :color :g} :flip flip
                            :charts [(chart/density {:position :mirror})]})]]
      (is (= [0.0 0.5] (:slot (first (filter #(= ["p"] (:key %)) (get-in b [:stats 0 :data facet/single-panel :ridges])))))
          (str "flip " flip)))))

;; 2. violin key

(deftest violin-key-test
  (let [p {:data rows :mapping {:x :v :y :lvl :color :g} :theme plain
           :scales {:color {:range [[255 0 0] [0 0 255]]}} :charts [(chart/violin {:stroke-color :none})]}
        b (built p)
        {:keys [x y w h]} (grid/item-box (:layout b) :legends)
        ^BufferedImage img (plot/render-image p options)
        red? (fn [i j] (= 0xff0000 (bit-and (.getRGB img (int i) (int j)) 0xffffff)))
        ;; the red key glyph: red pixels per column of the legend box
        cols (for [i (range x (+ (long x) (long w)))]
               [i (count (filter #(red? i %) (range y (+ (long y) (long h)))))])
        heights (map second (filter #(pos? (second %)) cols))]
    (is (seq heights))
    (is (= (reduce max heights) (nth heights (quot (count heights) 2))) "widest in the middle")
    (is (< (first heights) (reduce max heights)) "pointed at the left end")
    (is (< (last heights) (reduce max heights)) "pointed at the right end")))

;; 3. zero lines

(defn- zero-row-black-share
  "Share of black pixels along the panel row of y = 0 of plot `p`."
  [p]
  (let [b (built p)
        {:keys [x y w]} (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %)))
        zy (long (+ (double y) (Math/floor (scale/forward (get-in b [:finalized-scales :y]) 0.0))))
        ^BufferedImage img (plot/render-image p options)
        xs (range (+ (long x) 2) (- (+ (long x) (long w)) 2))]
    (/ (double (count (filter #(= 0 (bit-and (.getRGB img (int %) (int zy)) 0xffffff)) xs))) (count xs))))

(deftest zero-line-test
  (testing "mirror density: the zero line is drawn over the curves' tails"
    (is (< 0.99 (zero-row-black-share {:data rows :mapping {:x :v :color :g} :theme plain
                                        :charts [(chart/density {:position :mirror :width 3})]}))))
  (testing "mirror histogram: a clean zero line on top"
    (is (< 0.99 (zero-row-black-share {:data rows :mapping {:x :v :color :g} :theme plain
                                        :charts [(chart/histogram {:position :mirror :bins 40})]})))
    (is (true? (get-in (built {:data rows :mapping {:x :v :color :g} :charts [(chart/histogram {:position :mirror})]})
                       [:stats 0 :data facet/single-panel :zero-line?])))
    (is (false? (get-in (built {:data rows :mapping {:x :v :color :g} :charts [(chart/histogram {:position :mirror :zero-line false})]})
                        [:stats 0 :data facet/single-panel :zero-line?])))
    (is (false? (get-in (built {:data rows :mapping {:x :v} :charts [(chart/histogram)]})
                        [:stats 0 :data facet/single-panel :zero-line?]))
        "only mirror histograms"))
  (testing "sub-half-pixel bars aren't drawn; visible ones are"
    (doseq [[n-left drawn?] [[20000 false] [20 true]]
            :let [;; one value in the right bin: its bar is 1 / n-left of the tallest
                  rs (conj (vec (repeat n-left {:v 1})) {:v 100})
                  p {:data rs :mapping {:x :v} :theme plain
                     :charts [(chart/histogram {:bins 2 :color [255 0 0] :stroke-color :none :y :count})]}
                  b (built p)
                  {:keys [x y w h]} (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %)))
                  ^BufferedImage img (plot/render-image p options)
                  reddish (count (for [i (range (+ (long x) (quot (long w) 2) 5) (- (+ (long x) (long w)) 5))
                                       j (range y (+ (long y) (long h)))
                                       :let [rgb (.getRGB img (int i) (int j))]
                                       :when (< (bit-and rgb 0xff) 250)]
                                   1))]]
      (is (= drawn? (pos? reddish)) (str n-left))))
  (testing "theme defaults and schema"
    (is (= :black (get-in (built {:data rows :mapping {:x :v} :charts [(chart/histogram)]}) [:theme :chart :histogram :zero-line-color])))
    (is (= #{[:charts 0 :options :zero-line]}
           (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/histogram {:zero-line 1})]}))))))

;; 4. :fill mapping

(deftest fill-mapping-test
  (is (= #{[:mapping :fill]} (error-ins #(built {:data rows :mapping {:x :v :fill :g} :charts [(chart/density)]}))))
  (is (= #{[:charts 0 :options :mapping :fill]}
         (error-ins #(built {:data rows :mapping {:x :v} :charts [(chart/density {:mapping {:fill :g}})]}))))
  (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :fill nil} :charts [(chart/density)]} options))
      "a nil :fill is not mapped"))

;; 5. categorical declarations

(def ^:private cars
  (vec (for [i (range 40)] {:mpg (+ 10 (mod (* i 7) 25)) :cyl ([4 6 8] (mod i 3)) :year (+ 1970 (mod i 3))})))

(deftest categorical-type-test
  (testing ":type :categorical - bands for :y, ordinal for :color"
    (let [b (built {:data cars :mapping {:x :mpg :y :cyl :color :year}
                    :scales {:y {:type :categorical} :color {:type :categorical}} :charts [(chart/box)]})]
      (is (= :bands (get-in b [:finalized-scales :y :kind])))
      (is (= [4 6 8] (get-in b [:finalized-scales :y :domain])))
      (is (= :ordinal (get-in b [:scale-specs :color :kind])))
      (is (= [1970 1971 1972] (get-in b [:scale-specs :color :domain])))))
  (testing "the mapping marker gives the same plot"
    (let [by-scale (built {:data cars :mapping {:x :mpg :y :cyl} :scales {:y {:type :categorical}} :charts [(chart/box)]})
          by-marker (built {:data cars :mapping {:x :mpg :y (cljplot2.data.levels/categorical :cyl)} :charts [(chart/box)]})]
      (is (= (get-in by-scale [:finalized-scales :y]) (get-in by-marker [:finalized-scales :y])))
      (is (= (items by-scale) (items by-marker)))))
  (testing "markers: function accessors, aesthetics, facets, chart mappings; callable on rows"
    (let [marker (cljplot2.data.levels/categorical #(* 2 (:cyl %)))]
      (is (= 8 (marker {:cyl 4})))
      (is (= [8 12 16] (get-in (built {:data cars :mapping {:x :mpg :y marker} :charts [(chart/box)]})
                               [:finalized-scales :y :domain]))))
    (is (= :ordinal (get-in (built {:data cars :mapping {:x :mpg :y :mpg :color (cljplot2.data.levels/categorical :year)}
                                    :charts [(chart/scatter)]})
                            [:scale-specs :color :kind])))
    (is (= [4 6 8] (get-in (built {:data cars :mapping {:x :mpg :facet (cljplot2.data.levels/categorical :cyl)}
                                   :charts [(chart/histogram)]})
                           [:split :facets :levels :facet])))
    (is (= :bands (get-in (built {:data cars :mapping {:x :mpg}
                                  :charts [(chart/box {:mapping {:y (cljplot2.data.levels/categorical :cyl)}})]})
                          [:finalized-scales :y :kind]))
        "a marker in a chart's own mapping"))
  (testing ":type :numeric - linear aesthetics"
    (is (= :linear (get-in (built {:data (map #(assoc % :c (str (:cyl %))) cars) :mapping {:x :mpg :y :mpg :color :year}
                                   :scales {:color {:type :numeric}} :charts [(chart/scatter)]})
                           [:scale-specs :color :kind]))))
  (testing "contradictions throw {:key :type}"
    (doseq [p [{:scales {:y {:type :categorical :kind :linear}}}
               {:scales {:y {:type :numeric :kind :bands}}}
               {:scales {:color {:type :categorical :kind :linear}}}
               {:scales {:color {:type :numeric :kind :ordinal}}}
               {:scales {:y {:type :numeric}} :mapping {:x :mpg :y (cljplot2.data.levels/categorical :cyl)}}]]
      (is (= :type (:key (cljplot2.test-util/error #(built (merge {:data cars :mapping {:x :mpg :y :cyl :color :year}
                                                                   :charts [(chart/scatter)]} p)))))
          (pr-str p))))
  (testing "a matching :kind is fine; :kind alone still works"
    (is (= :bands (get-in (built {:data cars :mapping {:x :mpg :y :cyl} :scales {:y {:type :categorical :kind :bands}}
                                  :charts [(chart/box)]}) [:finalized-scales :y :kind])))
    (is (= :bands (get-in (built {:data cars :mapping {:x :mpg :y :cyl} :scales {:y {:kind :bands}} :charts [(chart/box)]})
                          [:finalized-scales :y :kind]))))
  (testing "a categorical numeric :y without levels (scatter) explains itself"
    (is (re-find #"declared categorical"
                 (ex-message (try (built {:data cars :mapping {:x :mpg :y :cyl} :scales {:y {:type :categorical}}
                                          :charts [(chart/scatter)]})
                                  nil
                                  (catch clojure.lang.ExceptionInfo e e))))))
  (testing "schema: unknown :type, facet :type :numeric"
    (is (= #{[:scales :y :type]} (error-ins #(built {:data cars :mapping {:x :mpg :y :cyl} :scales {:y {:type :ordinal}}
                                                    :charts [(chart/box)]}))))
    (is (= #{[:scales :facet :type]} (error-ins #(built {:data cars :mapping {:x :mpg :facet :cyl} :scales {:facet {:type :numeric}}
                                                        :charts [(chart/histogram)]}))))))

;; 6. plot margins

(def ^:private xy
  (vec (for [i (range 90)] {:a (+ 1.0 (mod (* i 7) 13)) :b (+ 2.0 (mod (* i 11) 17)) :s (["p" "q" "r"] (mod i 3))})))

(defn- box-of [b pred] (grid/find-content-box (:grid b) (:layout b) pred))
(defn- main-panel [b] (box-of b #(and (= :panel (:kind %)) (nil? (:margin %)))))
(defn- margin-panel [b side] (box-of b #(= side (:margin %))))

(deftest margins-test
  (let [p {:data xy :mapping {:x :a :y :b :color :s} :charts [(chart/scatter)]
           :margins {:top (chart/density) :right {:charts (chart/histogram) :axis true :size 60}}}
        b (built p)
        panel (main-panel b)
        top (margin-panel b :top)
        right (margin-panel b :right)]
    (testing "shared domains and pixel alignment"
      (is (= (get-in b [:finalized-scales :x]) (get-in b [:margin-env :top :scales :x])))
      (is (= (get-in b [:finalized-scales :y]) (get-in b [:margin-env :right :scales :x])))
      (is (= [(:x panel) (:w panel)] [(:x top) (:w top)]))
      (is (= [(:y panel) (:h panel)] [(:y right) (:h right)]))
      (is (< (+ (double (:y top)) (double (:h top))) (double (:y panel))) "top margin above the panel")
      (is (> (double (:x right)) (+ (double (:x panel)) (double (:w panel)))) "right margin right of the panel"))
    (testing "sizes: default 80, :size"
      (is (== 80 (:h top)))
      (is (== 60 (:w right))))
    (testing "axes: one per main dimension; a magnitude axis only with :axis"
      (let [axes (filter #(= :axis (get-in % [:content :kind])) (:items (:grid b)))]
        (is (= #{:x :y} (set (map #(get-in % [:content :scale]) (remove #(get-in % [:content :margin]) axes)))))
        (is (= [:right] (keep #(get-in % [:content :margin]) axes)))
        (is (= :horizontal (get-in b [:margin-env :right :scales :y :direction])))
        (is (= :vertical (get-in b [:margin-env :top :scales :y :direction])))))
    (testing "the legend right of the right margin"
      (let [legends (grid/item-box (:layout b) :legends)]
        (is (> (double (:x legends)) (+ (double (:x right)) (double (:w right)))))))
    (testing "margin charts group by the plot's colour mapping"
      (is (= 3 (count (get-in b [:margin-env :top :stats 0 :data facet/single-panel :curves])))))
    (testing "renders"
      (is (instance? BufferedImage (plot/render-image p options)))))
  (testing "flipped: the top margin shares the horizontal scale (:y)"
    (let [b (built {:data xy :mapping {:x :a :y :b} :flip true :charts [(chart/scatter)] :margins {:top (chart/violin)}})]
      (is (= (get-in b [:finalized-scales :y]) (get-in b [:margin-env :top :scales :x])))
      (is (instance? BufferedImage (plot/render-image {:data xy :mapping {:x :a :y :b} :flip true :charts [(chart/scatter)]
                                                       :margins {:top (chart/violin) :right [(chart/box) (chart/rug)]}}
                                                      options)))))
  (testing "strip margins have no magnitude scale"
    (is (nil? (get-in (built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:right (chart/box)}})
                      [:margin-env :right :scales :y]))))
  (testing "a margin chart's own :mapping wins"
    (let [b (built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:top (chart/density {:mapping {:x :b} :cut 0})}})
          xs (get-in b [:margin-env :top :stats 0 :data facet/single-panel :curves 0 :x])]
      (is (= [2.0 18.0] [(double (reduce min xs)) (double (reduce max xs))]) "the :b range, not :a's")
      (is (instance? BufferedImage (plot/render-image {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)]
                                                       :margins {:top (chart/density {:mapping {:x :b}})}} options)))))
  (testing "margins in arranged sub-plots: main panels aligned"
    (let [b (built (plot/arrange [{:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:top (chart/density)}}
                                  {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)]}]
                                 {:columns 2}))
          [left right] (map #(get-in b [:subplots [:plot %]]) [0 1])]
      (is (== (:y (main-panel left)) (:y (main-panel right))))
      (is (== (:h (main-panel left)) (:h (main-panel right))))))
  (testing "errors"
    (is (= #{[:margins :right :axis]}
           (error-ins #(built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:right {:charts [(chart/box)] :axis true}}}))))
    (is (= #{[:margins]} (error-ins #(built {:data xy :mapping {:x :a :y :b :facet :s} :charts [(chart/scatter)] :margins {:top (chart/density)}}))))
    (is (= #{[:margins]} (error-ins #(built (assoc (plot/arrange [{:data xy :mapping {:x :a} :charts [(chart/density)]}]) :margins {:top (chart/density)})))))
    (is (= :margins (:key (cljplot2.test-util/error #(built {:data xy :mapping {:y :b} :charts [(chart/density {:mapping {:x :b}})]
                                                            :margins {:top (chart/density)}})))))
    (is (seq (error-ins #(built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:top []}}))) "empty vector")
    (is (seq (error-ins #(built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:left (chart/density)}}))) "unknown side")
    (is (= #{[:margins :top :charts 0 :options :bins]}
           (error-ins #(built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:top (chart/histogram {:bins -1})}}))))))
