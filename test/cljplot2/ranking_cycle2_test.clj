(ns cljplot2.ranking-cycle2-test
  "Ranking cycle 2: fill patterns - the schema and named set, tiles, the shared helper and ink, the `:pattern`
  aesthetic and legend, adoption by the filled charts."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.bar :as bar]
            [cljplot2.chart.density :as density]
            [cljplot2.core :as plot]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.pattern :as pattern]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            [malli.core :as mc])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(def ^:private size {:width 300 :height 220})
(defn- build [p] (plot/build p size))
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- reddish? [[r g b]] (and (> (long r) 150) (< (long g) 110) (< (long b) 110)))
(defn- red-count
  "Count of reddish pixels (pattern ink `[255 0 0]`) in `img`."
  [^BufferedImage img]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (reddish? (rgb img x y))] 1)))
(def ^:private white {:plot {:background :white} :panel {:fill :white :grid {:x :none :y :none :bands-fill :none}}})
(def ^:private red [255 0 0])

(defn- filled
  "A canvas `w` x `h` with a white background and the rect `[x y rw rh]` filled with `fill` and pattern `p` (ink
  `ink`)."
  ^BufferedImage [w h [x y rw rh] fill p ink]
  (let [canvas (c2d/canvas w h)]
    (c2d/with-canvas-> canvas (c2d/set-background :white))
    (c2d/with-canvas [c canvas]
      (draw/bar! c (fastmath.vector/vec2 x y) (fastmath.vector/vec2 (+ x rw) (+ y rh)) fill :none 0
                 (draw/pattern-fill {:pattern p :pattern-color ink} fill)))
    (c2d/get-image canvas)))

(deftest pattern-schema
  (testing "1.1.1 spec maps and names"
    (are [v] (schema/pattern? v)
      :solid :hatch :dense-dots
      {:kind :lines :angle 0 :spacing 4 :width 1} {:kind :cross :angle 135 :spacing 8.5 :width 0.5}
      {:kind :dots :spacing 5 :size 1})
    (are [v] (not (schema/pattern? v))
      :none :hachure nil {:kind :lines :angle 30 :spacing 6 :width 1} {:kind :lines :angle 45 :spacing 0 :width 1}
      {:kind :dots :spacing 5 :size -1} {:kind :lines :angle 45 :spacing 6} {:kind :dots :spacing 5 :size 1 :angle 0}
      {:kind :waves :spacing 5}))
  (testing "1.1.1 options are checked at build, misspelt names get a suggestion"
    (is (= :pattern (:key (error #(build {:data [{:l "a" :v 1}] :mapping {:x :v :y :l}
                                          :charts [(chart/bar {:pattern {:kind :lines :angle 30 :spacing 6 :width 1}})]})))))
    (is (re-find #"spelled :hatch" (str (:errors (cljplot2.test-util/error-data
                                                  #(build {:data [{:l "a" :v 1}] :mapping {:x :v :y :l}
                                                           :charts [(chart/bar {:pattern :hach})]})))))))
  (testing "1.1.3 :solid and nil have no spec"
    (is (nil? (pattern/spec-of :solid)))
    (is (nil? (pattern/spec-of nil)))
    (is (= {:kind :dots :spacing 6.0 :size 2.0} (pattern/spec-of :dots)))))

(defn- ink-runs
  "Count of runs of non-white pixels along `points` of `img`."
  [^BufferedImage img points]
  (->> points (map (fn [[x y]] (not= [255 255 255] (rgb img x y)))) (partition-by identity) (filter first) count))

(deftest named-set-and-tiles
  (testing "1.1.2 every named pattern shows at least two marks in a 14 px key (along a row, column or diagonal)"
    (doseq [p (remove #{:solid} (keys schema/pattern-styles))
            :let [img (filled 14 14 [0 0 14 14] :white p :black)
                  ks (range 14)]]
      (is (>= (max (ink-runs img (map vector ks (repeat 7))) (ink-runs img (map vector (repeat 7) ks))
                   (ink-runs img (map vector ks ks)) (ink-runs img (map vector ks (reverse ks))))
              2)
          (str p))))
  (testing "1.2 periods: whole pixels, spacing or spacing·√2, at least 2"
    (is (= 6 (pattern/period (pattern/spec-of :horizontal))))
    (is (= 8 (pattern/period (pattern/spec-of :hatch))))
    (is (= 4 (pattern/period (pattern/spec-of :dense-dots))))
    (is (= 2 (pattern/period {:kind :lines :angle 0 :spacing 0.5 :width 1})) "tiny spacing clamps to 2"))
  (testing "1.2 tiles: transparent background, ink marks"
    (let [^BufferedImage t (pattern/tile (pattern/spec-of :vertical) :black)
          alphas (for [x (range 6) y (range 6)] (bit-and (bit-shift-right (.getRGB t x y) 24) 0xff))]
      (is (= [6 6] [(.getWidth t) (.getHeight t)]))
      (is (some zero? alphas))
      (is (some #(= 255 %) alphas))))
  (testing "1.2 seamless: a hatched 60 px row has evenly spaced marks"
    (doseq [p [:hatch :back-hatch :vertical :cross]
            :let [img (filled 60 60 [0 0 60 60] :white p :black)
                  starts (->> (range 60) (map (fn [x] [x (not= [255 255 255] (rgb img x 30))])) (partition-by second)
                              (filter (comp second first)) (map ffirst))
                  gaps (set (map - (rest starts) starts))]]
      (is (>= (count starts) 6) (str p))
      (is (<= (- (apply max gaps) (apply min gaps)) 1) (str p " gaps " gaps)))))

(deftest helper-and-ink
  (testing "1.3 :solid draws nothing over the fill; :hatch draws ink inside the shape only"
    (let [solid (filled 40 40 [10 10 20 20] [200 200 200] :solid red)
          hatched (filled 40 40 [10 10 20 20] [200 200 200] :hatch red)]
      (is (zero? (red-count solid)))
      (is (pos? (red-count hatched)))
      (is (every? #(= [255 255 255] %) (for [x (range 40) y (range 40) :when (or (< x 10) (>= x 30) (< y 10) (>= y 30))]
                                         (rgb hatched x y))))))
  (testing "1.4 black ink on a white fill: only greys inside"
    (let [img (filled 40 40 [0 0 40 40] :white :cross :black)]
      (is (every? (fn [[r g b]] (= r g b)) (for [x (range 40) y (range 40)] (rgb img x y))))))
  (testing "1.4 / 3.4 ink: :darker of the fill, never more opaque than the fill"
    (is (= 255.0 (clojure2d.color/alpha (draw/pattern-ink :darker [100 150 200]))))
    (is (= 128.0 (clojure2d.color/alpha (draw/pattern-ink :darker [100 150 200 128]))))
    (is (= 128.0 (clojure2d.color/alpha (draw/pattern-ink :black [100 150 200 128]))))
    (is (= 60.0 (clojure2d.color/alpha (draw/pattern-ink [0 0 0 60] [100 150 200]))) "a translucent ink stays as given"))
  (testing "pattern-fill: nil without a pattern or a fill"
    (is (nil? (draw/pattern-fill {:pattern :solid} :red)))
    (is (nil? (draw/pattern-fill {:pattern :hatch} :none)))
    (is (nil? (draw/pattern-fill {:pattern :hatch} nil)))
    (is (= :hatch (:pattern (draw/pattern-fill {:pattern :hatch} :red))))))

(def ^:private groups [{:g "a" :v 1} {:g "b" :v 2} {:g "c" :v 3}])

(defn- legends [built] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in built [:grid :items])))

(deftest pattern-aesthetic
  (testing "2.1 the theme range in group order; an explicit range replaces it"
    (let [b (build {:data groups :mapping {:x :v :y :g :pattern :g} :charts [(chart/bar)]})]
      (is (= [:hatch :back-hatch :horizontal] (mapv #(get-in % [:attributes :pattern]) (:entries (first (legends b)))))))
    (let [b (build {:data groups :mapping {:x :v :y :g :pattern :g} :scales {:pattern {:range [:dots :cross]}}
                    :charts [(chart/bar)]})]
      (is (= [:dots :cross :dots] (mapv #(get-in % [:attributes :pattern]) (:entries (first (legends b))))) "cycled")))
  (testing "2.1 a bad range is rejected; identity scales take patterns as data"
    (is (some? (error #(build {:data groups :mapping {:x :v :y :g :pattern :g} :scales {:pattern {:range [:hachure]}}
                               :charts [(chart/bar)]}))))
    (is (= :mapping (:key (error #(build {:data groups :mapping {:x :v :y :g :pattern :g} :scales {:pattern {:kind :identity}}
                                          :charts [(chart/bar)]}))))
        "identity: values must be patterns"))
  (testing "2.2 numeric data is a set of levels (always ordinal, as :dash)"
    (is (= [1.0 2.0 3.0] (mapv double (get-in (build {:data groups :mapping {:x :v :y :g :pattern :v} :charts [(chart/bar)]})
                                              [:scale-specs :pattern :domain]))))
    (is (= :kind (:key (error #(build {:data groups :mapping {:x :v :y :g :pattern :v} :scales {:pattern {:kind :linear}}
                                          :charts [(chart/bar)]}))))
        "a linear pattern scale is rejected"))
  (testing "2.3 :pattern and :color on one variable merge into one legend"
    (let [ls (legends (build {:data groups :mapping {:x :v :y :g :pattern :g :color :g} :charts [(chart/bar)]}))]
      (is (= 1 (count ls)))
      (is (= #{:color :pattern} (set (keys (:attributes (first (:entries (first ls))))))))))
  (testing "2.4 :pattern groups (stacking)"
    (let [bs (get-in (build {:data (map #(assoc % :l "x") groups) :mapping {:x :v :y :l :pattern :g}
                             :charts [(chart/bar {:position :stack})]})
                     [:stats 0 :data :panel :bars])]
      (is (= [[0.0 1.0] [1.0 3.0] [3.0 6.0]] (mapv (juxt :y0 :y1) bs))))))

(defn- ink
  "Reddish (ink) pixels of plot `p` rendered on white."
  [p]
  (red-count (plot/render-image (assoc p :theme white :legend false) size)))

(def ^:private samples (vec (for [g ["a" "b"] i (range 60)] {:g g :v (+ (* 0.1 i) (if (= g "a") 0 2)) :i i :y (Math/sin (* 0.2 i))})))
(def ^:private prices
  (vec (for [i (range 6)] {:day (.plusDays (LocalDate/of 2024 1 1) i) :open (+ 10 i) :close (+ 12 i) :high (+ 14 i) :low (+ 9 i)})))

(deftest chart-adoption
  (testing "3.2 hatch draws ink inside each chart's fills, :solid none"
    (are [chart-fn p] (and (pos? (ink (assoc p :charts [(chart-fn {:pattern :hatch})])))
                           (zero? (ink (assoc p :charts [(chart-fn {:pattern :solid})]))))
      #(chart/bar (merge {:color :white :pattern-color red} %)) {:data groups :mapping {:x :v :y :g}}
      #(chart/histogram (merge {:color :white :pattern-color red} %)) {:data samples :mapping {:x :v}}
      #(chart/histogram (merge {:color :white :pattern-color red} %)) {:data samples :mapping {:x :v :y :g}}
      #(chart/density (merge {:color [250 250 250] :fill :color :alpha 1.0 :pattern-color red} %)) {:data samples :mapping {:x :v}}
      #(chart/density (merge {:color [250 250 250] :alpha 1.0 :pattern-color red :position :stack} %)) {:data samples :mapping {:x :v :group :g}}
      #(chart/density (merge {:color [250 250 250] :fill :color :alpha 1.0 :pattern-color red} %)) {:data samples :mapping {:x :v :y :g}}
      #(chart/line (merge {:color [250 250 250] :fill :color :alpha 1.0 :pattern-color red} %)) {:data samples :mapping {:x :i :y :y}}
      #(chart/line (merge {:color [250 250 250] :alpha 1.0 :pattern-color red :position :stack} %)) {:data samples :mapping {:x :i :y :v :group :g}}
      #(chart/line (merge {:color [250 250 250] :fill :color :alpha 1.0 :pattern-color red :fill-gradient [:white :white]} %)) {:data samples :mapping {:x :i :y :y}}
      #(chart/ribbon (merge {:color [250 250 250] :alpha 1.0 :pattern-color red} %)) {:data samples :mapping {:x :i :ymin :y :ymax :v}}
      #(chart/smooth (merge {:color [250 250 250] :alpha 1.0 :pattern-color red :method :lm} %)) {:data samples :mapping {:x :i :y :y}}
      #(chart/box (merge {:color :white :pattern-color red} %)) {:data samples :mapping {:x :v :y :g}}
      #(chart/violin (merge {:color :white :pattern-color red} %)) {:data samples :mapping {:x :v :y :g}}
      #(chart/rect (merge {:fill :white :pattern-color red :x [0 1] :y [0 1]} %)) {:data groups :mapping {:x :v :y :v}}))
  (testing "3.2.4 treemap leaves: the pattern changes the drawn image"
    (let [img (fn [p] (plot/render-image {:data groups :mapping {:value :v :hierarchy [:g]} :legend false
                                          :charts [(chart/treemap {:pattern p :pattern-color :black})]} size))
          ^BufferedImage a (img :hatch) ^BufferedImage b (img :solid)]
      (is (pos? (count (for [x (range 300) y (range 220) :when (not= (rgb a x y) (rgb b x y))] 1))))))
  (testing "3.2.5 candlestick bodies: up and down patterns"
    (let [p {:data prices :mapping {:x :day :open :open :high :high :low :low :close :close}}
          c (fn [o] (assoc p :charts [(chart/candlestick (merge {:up-color :white :down-color :white :pattern-color red} o))]))]
      (is (pos? (ink (c {:up-pattern :hatch}))) "every candle is up")
      (is (zero? (ink (c {:down-pattern :hatch}))))
      (is (zero? (ink (c {}))))))
  (testing "3.1 a mapped pattern wins over the literal one"
    (is (zero? (ink {:data (map #(assoc % :p :solid) groups) :mapping {:x :v :y :g :pattern :p} :scales {:pattern {:kind :identity}}
                     :charts [(chart/bar {:color :white :pattern-color red :pattern :hatch})]}))))
  (testing "3.1 theme defaults are checked"
    (is (= :pattern (:key (error #(build {:data groups :mapping {:x :v :y :g} :theme {:chart {:box {:pattern :hachure}}}
                                          :charts [(chart/bar)]})))))
    (is (= :solid (get-in theme/default-theme [:chart :violin :pattern])))
    (is (= :darker (get-in theme/default-theme [:chart :ribbon :pattern-color])))))

(defn- key-ink
  "Reddish pixels of a 20 px legend key drawn by `legend-fn`."
  [legend-fn chart attributes]
  (let [canvas (c2d/canvas 20 20)]
    (c2d/with-canvas-> canvas (c2d/set-background :white))
    (c2d/with-canvas [c canvas]
      (legend-fn chart attributes c {:x 0 :y 0 :w 20 :h 20} {:theme theme/default-theme :chart-stats {}}))
    (red-count (c2d/get-image canvas))))

(deftest legend-glyphs
  (testing "3.3 key glyphs show the entry's pattern"
    (is (pos? (key-ink bar/legend (chart/bar {:pattern-color red}) {:color [250 250 250] :pattern :hatch})))
    (is (zero? (key-ink bar/legend (chart/bar {:pattern-color red}) {:color [250 250 250]})))
    (is (pos? (key-ink density/legend (chart/density {:fill :color :alpha 1.0 :pattern-color red}) {:color [250 250 250] :pattern :cross})))))

(deftest review-fixes
  (testing "T1 :pattern-color :none - no pattern, no error"
    (is (nil? (draw/pattern-fill {:pattern :hatch :pattern-color :none} :red)))
    (is (zero? (ink {:data groups :mapping {:x :v :y :g} :charts [(chart/bar {:color :white :pattern :hatch :pattern-color :none})]}))))
  (testing "T2 one pattern fill per pattern, ink and fill in a draw call"
    (let [cache (draw/pattern-fill-cache)
          a (cache {:pattern :hatch :color :blue} [255 255 255])
          b (cache {:pattern :hatch :color :red} [255 255 255])]
      (is (identical? a b) "other style keys don't matter")
      (is (not (identical? a (cache {:pattern :dots} [255 255 255]))))
      (is (not (identical? a (cache {:pattern :hatch} [0 0 0]))))
      (is (nil? (cache {:pattern :solid} [0 0 0])))
      (is (instance? java.awt.image.BufferedImage (:tile a)))))
  (testing "T3 angles as doubles"
    (is (schema/pattern? {:kind :lines :angle 45.0 :spacing 6 :width 1}))
    (is (not (schema/pattern? {:kind :lines :angle 45.5 :spacing 6 :width 1})))
    (is (= 8 (pattern/period {:kind :cross :angle 135.0 :spacing 6 :width 1})))
    (is (pos? (ink {:data groups :mapping {:x :v :y :g}
                    :charts [(chart/bar {:color :white :pattern-color red :pattern {:kind :lines :angle 90.0 :spacing 5 :width 1}})]}))))
  (testing "T5 treemap: leaves patterned, group boxes not"
    (let [data (for [g ["p" "q"] l ["a" "b"]] {:g g :l l :v 1})
          render (fn [p] (plot/render-image {:data data :mapping {:value :v :hierarchy [:g :l]} :legend false :theme white
                                             :charts [(chart/treemap {:pattern p :pattern-color red :gap 12})]}
                                            size))]
      (is (pos? (red-count (render :hatch))))
      (is (zero? (red-count (render :solid))))
      (testing "the group padding (just inside a group box) stays free of ink"
        (let [img (render :hatch)]
          (is (not-any? reddish? (for [x (range 300)] (rgb img x 9)))
              "a row inside the top padding band (panel starts at y 0 in a legend-free plot without title)")))))
  (testing "T7 default position: dodge only when a level holds two or more groups"
    (let [slots (fn [p] (set (map :slot (get-in (build p) [:stats 0 :data :panel (if (= :bar (get-in p [:charts 0 :kind])) :bars :items)]))))]
      (is (= #{[-0.5 0.5]} (slots {:data groups :mapping {:x :v :y :g :color :g} :charts [(chart/bar)]}))
          "one group per level - full band")
      (is (not (get-in (build {:data groups :mapping {:x :v :y :g :color :g} :charts [(chart/bar)]}) [:stats 0 :data :panel :translucent]))
          "not translucent")
      (is (= 2 (count (slots {:data (concat groups (map #(assoc % :g "a" :c "z") groups)) :mapping {:x :v :y :g :color :c}
                              :charts [(chart/bar)]})))
          "a level with two groups dodges")
      (is (= #{[-0.5 0.5]} (slots {:data samples :mapping {:x :v :y :g :color :g} :charts [(chart/box)]})) "box too")
      (is (= #{[-0.5 0.5]} (slots {:data samples :mapping {:x :v :y :g :color :g} :charts [(chart/extent)]})) "and extent")
      (is (= 2 (count (slots {:data samples :mapping {:x :v :color :g} :charts [(chart/box)]})))
          "without levels the panel is the level: groups dodge"))))
