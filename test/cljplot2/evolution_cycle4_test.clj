(ns cljplot2.evolution-cycle4-test
  "Evolution cycle 4: facet context recipe, temporal annotation positions, text `:select`, segment, candlestick,
  rank helper."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.curve :as curve]
            [malli.core :as mc])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate LocalDateTime LocalTime]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- stats [p i] (get-in (build p) [:stats i]))
(def ^:private white {:plot {:background :white} :panel {:grid {:x :none :y :none}}})
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- pixels-of
  "Count of pixels of `img` within 3 of `color` per channel."
  [^BufferedImage img color]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img))
               :when (every? (fn [[a b]] (<= (Math/abs (- (long a) (long b))) 3)) (map vector (rgb img x y) color))]
           1)))

(def ^:private day0 (LocalDate/of 2024 1 1))
(def ^:private series
  (vec (for [s ["a" "b" "c"] i (range 10)] {:d (.plusDays day0 i) :i i :v (+ i (case s "a" 0 "b" 5 "c" 10)) :s s})))

;; 1. facet context

(deftest facet-context-test
  (testing "1.1 a {:facet nil} line is drawn in every panel, the faceted one per panel"
    (let [p {:data series :mapping {:x :i :y :v :color :s :facet :s}
             :charts [(chart/line {:mapping {:facet nil :color nil :group :s} :color [200 200 200]}) (chart/line)]}
          b (build p)]
      (is (= [facet/all-panels] (keys (get-in b [:stats 0 :data]))))
      (is (= 3 (count (get-in b [:stats 0 :data facet/all-panels :lines]))) "every group, ungrouped by colour")
      (is (= 3 (count (keys (get-in b [:stats 1 :data])))) "one panel per facet level")
      (is (instance? BufferedImage (plot/render-image p size))))))

;; 2. end labels

(deftest temporal-annotation-test
  (testing "2.1.1 text and rule at java.time positions; extents include them"
    (let [p {:data series :mapping {:x :d :y :v}
             :charts [(chart/line)
                      (chart/text {:x (LocalDate/of 2024 2 1) :y 5 :text "later"})
                      (chart/rule {:x (LocalDate/of 2023 12 25)})]}
          b (build p)]
      (is (= (LocalDate/of 2024 2 1) (get-in b [:stats 1 :extents facet/all-panels :x :max])))
      (is (= (LocalDate/of 2023 12 25) (get-in b [:stats 2 :extents facet/all-panels :x :min])))
      (is (instance? BufferedImage (plot/render-image p size)))))
  (testing "rect between dates"
    (is (instance? BufferedImage (plot/render-image {:data series :mapping {:x :d :y :v}
                                                     :charts [(chart/rect {:x [(.plusDays day0 2) (.plusDays day0 4)]}) (chart/line)]}
                                                    size)))))

(defn- text-plot [opts & [extra]]
  (merge {:data series :mapping {:x :d :y :v :color :s}
          :charts [(chart/line) (chart/text (merge {:mapping {:x :d :y :v :label :s :color :s}} opts))]}
         extra))

(defn- labels
  ([p] (labels p 1))
  ([p i] (mapcat :labels (mapcat :groups (vals (get-in (build p) [:stats i :data]))))))

(deftest select-test
  (testing "2.2.1 one label per group at its last / first x (dates)"
    (is (= #{["a" (.plusDays day0 9)] ["b" (.plusDays day0 9)] ["c" (.plusDays day0 9)]}
           (set (map (juxt :text :x) (labels (text-plot {:select :last}))))))
    (is (= #{(.plusDays day0 0)} (set (map :x (labels (text-plot {:select :first})))))))
  (testing "2.2.1 per label text within a group (no aesthetic)"
    (is (= 3 (count (labels {:data series :charts [(chart/text {:mapping {:x :i :y :v :label :s} :select :last})]} 0)))))
  (testing "2.2.1 rows out of x order; ties keep the later row; categorical x - data order"
    (let [rows [{:x 3 :y 1 :t "p"} {:x 9 :y 2 :t "p"} {:x 1 :y 3 :t "p"} {:x 9 :y 4 :t "p"}]
          one (fn [rows sel] (labels {:data rows :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select sel})]} 0))]
      (is (= [{:x 9 :y 4 :text "p"}] (one rows :last)))
      (is (= [{:x 1 :y 3 :text "p"}] (one rows :first)))
      (is (= [{:x "b" :y 2 :text "p"}] (one [{:x "a" :y 1 :t "p"} {:x "b" :y 2 :t "p"}] :last)))
      (is (= [] (one [] :last)) "no rows")))
  (testing "2.2.2 :select needs data mode"
    (is (= :select (:key (error #(build {:charts [(chart/text {:x 1 :y 1 :text "a" :select :last})]})))))
    (is (= :select (:key (error #(build (text-plot {:select :middle})))))))
  (testing "2.2.3 flip-aware defaults; explicit options win"
    (let [opts (fn [p] (get-in (build p) [:stats 1 :data] ) )
          placed (fn [p] (select-keys (:options (first (vals (opts p)))) [:anchor :offset]))]
      (is (= {:anchor [:left :middle] :offset [5 0]} (placed (text-plot {:select :last}))))
      (is (= {:anchor [:right :middle] :offset [-5 0]} (placed (text-plot {:select :first}))))
      (is (= {:anchor [:center :bottom] :offset [0 5]} (placed (text-plot {:select :last} {:flip true}))))
      (is (= {:anchor [:center :top] :offset [0 -5]} (placed (text-plot {:select :first} {:flip true}))))
      (is (= {:anchor [:right :top] :offset [5 0]} (placed (text-plot {:select :last :anchor [:right :top]}))))))
  (testing "2.2.4 room past the data: high side for :last, low for :first; label inside the image"
    (let [[lo hi] (:x (:padding-px (stats (text-plot {:select :last}) 1)))]
      (is (zero? (double lo)))
      (is (> (double hi) 5.0)))
    (let [[lo hi] (:x (:padding-px (stats (text-plot {:select :first}) 1)))]
      (is (> (double lo) 5.0))
      (is (zero? (double hi))))
    (is (nil? (:padding-px (stats (text-plot {}) 1))) "no room without :select")
    (let [p {:data (mapv #(assoc % :s "a-very-long-label") (take 10 series)) :mapping {:x :d :y :v} :theme white
             :charts [(chart/line) (chart/text {:mapping {:x :d :y :v :label :s} :select :last :color [255 0 0] :size 14})]}
          img (plot/render-image p size)
          xs (for [x (range 400) y (range 300) :let [[r g b] (rgb img x y)] :when (and (> r 200) (< g 60) (< b 60))] x)]
      (is (seq xs))
      (is (< (reduce max xs) 399) "the label ends inside the image")))
  (testing "2.2.4 room on a datetime axis widens its domain"
    (let [domain (fn [p] (get-in (build p) [:finalized-scales :x :domain]))
          with (domain (text-plot {:select :last}))
          without (domain (text-plot {}))]
      (is (instance? LocalDateTime (second with)))
      (is (pos? (compare (second with) (second without))) "the label room on top of the padding")
      (is (pos? (compare (second without) (LocalDateTime/of 2024 1 10 0 0))) "datetime domains padded (L6)"))))

;; 3. segment

(def ^:private stems [{:x 1 :y 3} {:x 2 :y 5} {:x 3 :y nil} {:x 4 :y -2}])

(deftest segment-test
  (testing "3.1 ends: literal, mapped, or the start"
    (let [s (stats {:data stems :mapping {:x :x :y :y} :charts [(chart/segment {:yend 0})]} 0)
          seg (get-in s [:data facet/single-panel :segments 0])]
      (is (= [1 2 4] (:x seg)) "3.3 a row without y dropped")
      (is (= [1 2 4] (:xend seg)) "xend - the start")
      (is (= [0 0 0] (:yend seg)))
      (is (= 1 (get-in s [:dropped :y])))
      (is (= {:kind :numeric :min -2.0 :max 5.0} (get-in s [:extents facet/single-panel :y])) "3.6 y over y and yend"))
    (let [rows [{:x 0 :y 0 :x2 5 :y2 nil} {:x 1 :y 1 :x2 2 :y2 3}]
          s (stats {:data rows :mapping {:x :x :y :y :xend :x2 :yend :y2} :charts [(chart/segment)]} 0)]
      (is (= [2] (get-in s [:data facet/single-panel :segments 0 :xend])))
      (is (= {:x 0 :y 0 :xend 0 :yend 1} (select-keys (:dropped s) [:x :y :xend :yend]))))
    (is (= [9 9] (get-in (stats {:data [{:x 0 :y 0 :x2 5} {:x 1 :y 1 :x2 2}] :mapping {:x :x :y :y :xend :x2}
                                 :charts [(chart/segment {:xend 9})]} 0)
                         [:data facet/single-panel :segments 0 :xend]))
        "3.2 a literal end wins"))
  (testing "3.2 mapping keys"
    (is (mc/validate rows/mapping-schema {:xend :a :yend :b :open :o :high :h :low :l :close :c})))
  (testing "3.4 / 3.5 ordinal groups; numeric colour per segment"
    (let [rows (mapv (fn [i] {:x i :y i :g (if (even? i) "e" "o")}) (range 6))]
      (is (= 2 (count (get-in (stats {:data rows :mapping {:x :x :y :y :color :g} :charts [(chart/segment {:yend 0})]} 0)
                              [:data facet/single-panel :segments]))))
      (let [s (stats {:data rows :mapping {:x :x :y :y :color :y} :charts [(chart/segment {:yend 0})]} 0)]
        (is (= [0 1 2 3 4 5] (get-in s [:data facet/single-panel :segments 0 :colors]))))
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :x :y :y :color :y}
                                                       :charts [(chart/segment {:yend 0 :line-width 4})]} size)))))
  (testing "3.7 flipped and temporal stems render; no valid values"
    (is (instance? BufferedImage (plot/render-image {:data stems :flip true :mapping {:x :x :y :y} :charts [(chart/segment {:yend 0})]} size)))
    (is (instance? BufferedImage (plot/render-image {:data series :mapping {:x :d :y :v} :charts [(chart/segment {:yend 0})]} size)))
    (is (= :mapping (:key (error #(build {:data stems :mapping {:x :x :y :nope} :charts [(chart/segment)]})))))))

;; 4. candlestick

(def ^:private ohlc
  [{:d (.plusDays day0 0) :o 10 :h 12 :l 9 :c 11}
   {:d (.plusDays day0 1) :o 11 :h 11.5 :l 8 :c 9}
   {:d (.plusDays day0 2) :o 9 :h 10 :l 8.5 :c 9}
   {:d (.plusDays day0 3) :o nil :h 10 :l 8 :c 9}])

(def ^:private ohlc-mapping {:x :d :open :o :high :h :low :l :close :c})

(defn- candle-plot [opts & [extra]]
  (merge {:data ohlc :mapping ohlc-mapping :theme white :charts [(chart/candlestick opts)]} extra))

(deftest candlestick-test
  (testing "4.1 rows with an invalid value dropped; aesthetics rejected"
    (let [s (stats (candle-plot {}) 0)]
      (is (= 3 (count (get-in s [:data facet/single-panel :candles]))))
      (is (= {:x 0 :open 1 :high 0 :low 0 :close 0 :ohlc 0} (:dropped s)))
      (is (= {:kind :numeric :min 8.0 :max 12.0} (get-in s [:extents facet/single-panel :y])) "4.5 y over the values"))
    (is (= :mapping (:key (error #(build (candle-plot {:mapping {:color :d}}))))) "own aesthetic")
    (is (= :mapping (:key (error #(build (candle-plot {:mapping {:group :d}})))))))
  (testing "4.2 up green, down red bodies"
    (let [img (plot/render-image (candle-plot {}) size)]
      (is (pos? (pixels-of img [26 152 80])))
      (is (pos? (pixels-of img [215 48 39])))))
  (testing "4.3 body width: a fraction of the x step"
    (let [count-up (fn [w] (pixels-of (plot/render-image (candle-plot {:width w}) size) [26 152 80]))]
      (is (< (* 1.5 (double (count-up 0.3))) (double (count-up 0.9))))))
  (testing "4.3 a single candle: 10 px; bands x"
    (is (instance? BufferedImage (plot/render-image (candle-plot {} {:data [(assoc (first ohlc) :d 1)]}) size)))
    (is (instance? BufferedImage (plot/render-image (candle-plot {} {:data (mapv #(update % :d str) (take 3 ohlc))}) size))))
  (testing "4.4 open = close: a tick in the up colour"
    (is (pos? (pixels-of (plot/render-image (candle-plot {} {:data [(nth ohlc 2) (assoc (nth ohlc 2) :d (.plusDays day0 5))]}) size)
                         [26 152 80]))))
  (testing "4.5 flipped; 4.6 OHLC; outline"
    (is (instance? BufferedImage (plot/render-image (candle-plot {} {:flip true}) size)))
    (let [candle (plot/render-image (candle-plot {}) size)
          bars (plot/render-image (candle-plot {:style :ohlc}) size)]
      (is (< (* 3 (pixels-of bars [26 152 80])) (pixels-of candle [26 152 80])) "no filled bodies"))
    (is (pos? (pixels-of (plot/render-image (candle-plot {:stroke-color [0 0 0]}) size) [0 0 0])))
    (is (= :style (:key (error #(build (candle-plot {:style :bars}))))))
    (is (= :width (:key (error #(build (candle-plot {:width 1.5}))))))))

;; 5. rank helper

(deftest rank-rows-test
  (testing "5.1 / 5.2 1-based, highest first, ties :min; strategies"
    (let [rows (mapv (fn [v] {:x 1 :v v}) [5 9 9 1])]
      (is (= [3 1 1 4] (map :rank (levels/rank-rows rows :x :v))))
      (is (= [3 1 2 4] (map :rank (levels/rank-rows rows :x :v {:ties :first}))))
      (is (= [3.0 1.5 1.5 4.0] (map :rank (levels/rank-rows rows :x :v {:ties :average}))))
      (is (= [2 3 3 1] (map :rank (levels/rank-rows rows :x :v {:descending false}))))
      (is (= [3 1 1 4] (map :place (levels/rank-rows rows :x :v {:as :place}))))
      (is (= :ties (:key (error #(levels/rank-rows rows :x :v {:ties :nope})))))))
  (testing "5.1 ranks within each x; 5.3 invalid values unranked; columns"
    (is (= [1 2 2 1 nil nil] (map :rank (levels/rank-rows [{:x 1 :v 3} {:x 1 :v 1} {:x 2 :v 1} {:x 2 :v 4} {:x 2 :v nil} {:x 3 :v ##NaN}] :x :v))))
    (is (= [2 1] (map :rank (levels/rank-rows {:x [1 1] :v [1 2]} :x :v))))
    (is (= [] (levels/rank-rows [] :x :v))))
  (testing "5.4 a bump chart renders, rank 1 on top"
    (let [ranked (levels/rank-rows series :i :v)
          p {:data ranked :mapping {:x :i :y :rank :color :s} :scales {:y {:domain [3 1]}}
             :charts [(chart/line {:interpolator :monotone}) (chart/scatter)
                      (chart/text {:mapping {:x :i :y :rank :label :s :color :s} :select :last})]}
          b (build p)]
      (is (apply > (map double (get-in b [:finalized-scales :y :domain]))) "reversed")
      (is (instance? BufferedImage (plot/render-image p size))))))

;; review fixes

(deftest review-drawn-point-test
  (testing "K1 drawn points: clamped, rounded to 1e-6 px"
    (is (= [155.0 1.0e6] (curve/drawn-point [154.99999999999997 1.0e300])))
    (is (= [155.0 -1.0e6] (curve/drawn-point [155.00000000000003 ##-Inf])))
    (is (= [0.5 0.123457] (curve/drawn-point [0.5 0.1234567]))))
  (testing "K1 a thick monotone line's flat run draws as a straight line (no dots at the joins)"
    (let [rows [{:x 0 :y 5} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 3} {:x 4 :y 1}]
          img (fn [opts] (plot/render-image {:data rows :mapping {:x :x :y :y} :theme white :labels {:x :none :y :none}
                                             :scales {:x {:domain [0 4]} :y {:domain [0 6]}}
                                             :charts [(chart/line (merge {:line-width 3 :color [0 0 255]} opts))]} size))
          curved (img {:interpolator :monotone})
          straight (img {})
          panel-x (fn [v] (+ 29 (* v 90)))
          column (fn [^BufferedImage im x] (mapv #(rgb im x %) (range 300)))]
      (is (every? #(= (column curved %) (column straight %)) (range (panel-x 1.2) (panel-x 2.8)))))))

(deftest review-temporal-domain-test
  (testing "K2 a single date widens one day either side"
    (let [p {:data [{:d day0 :v 1} {:d day0 :v 2}] :mapping {:x :d :y :v} :charts [(chart/scatter)]}]
      (is (= [(LocalDate/of 2023 12 31) (LocalDate/of 2024 1 2)] (get-in (build p) [:scale-specs :x :domain])))
      (is (instance? BufferedImage (plot/render-image p size)))))
  (testing "K2 a single LocalTime: one hour either side, within the day"
    (let [domain (fn [t] (get-in (build {:data [{:t t :v 1}] :mapping {:x :t :y :v} :charts [(chart/scatter)]}) [:scale-specs :x :domain]))]
      (is (= [(LocalTime/of 11 0) (LocalTime/of 13 0)] (domain (LocalTime/of 12 0))))
      (is (= [(LocalTime/of 22 30) (LocalTime/of 23 59 59)] (domain (LocalTime/of 23 30))))
      (is (= [LocalTime/MIDNIGHT (LocalTime/of 1 0)] (domain LocalTime/MIDNIGHT)))))
  (testing "K2 a single candle on a date; a text-only :select plot"
    (is (instance? BufferedImage (plot/render-image (candle-plot {} {:data (take 1 ohlc)}) size)))
    (is (instance? BufferedImage (plot/render-image {:data series :charts [(chart/text {:mapping {:x :d :y :v :label :s} :select :last})]} size)))))

(deftest review-rank-test
  (testing "K3 x shared by key: 1 and 1.0 one x; dates; invalid x unranked"
    (is (= [2 1] (map :rank (levels/rank-rows [{:x 1 :v 5} {:x 1.0 :v 9}] :x :v))))
    (is (= [nil nil 1] (map :rank (levels/rank-rows [{:x nil :v 1} {:x ##NaN :v 2} {:x "a" :v 3}] :x :v))))
    (is (= [2 1] (map :rank (levels/rank-rows [{:x day0 :v 1} {:x (LocalDate/of 2024 1 1) :v 2}] :x :v))))))

(deftest review-room-test
  (testing "K4 the room follows :rotation: a long label at 90 degrees needs its height"
    (let [room (fn [rotation] (second (:x (:padding-px (stats {:data [{:x 1 :y 1 :t "a-long-label"} {:x 2 :y 2 :t "a-long-label"}]
                                                               :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select :last
                                                                                     :rotation rotation})]} 0)))))]
      (is (< (* 2.0 (double (room 90))) (double (room 0)))))))

(deftest review-candlestick-test
  (testing "K6 inconsistent rows dropped, counted as :ohlc"
    (let [s (stats (candle-plot {} {:data (conj (vec (take 3 ohlc)) {:d (.plusDays day0 5) :o 10 :h 9 :l 11 :c 10}
                                                {:d (.plusDays day0 6) :o 13 :h 12 :l 9 :c 10})}) 0)]
      (is (= 3 (count (get-in s [:data facet/single-panel :candles]))))
      (is (= 2 (get-in s [:dropped :ohlc])))))
  (testing "K7 aesthetics inherited from the plot mapping are ignored; no glyph in their keys"
    (let [p {:data (mapv #(assoc % :g "p") (take 3 ohlc)) :mapping (assoc ohlc-mapping :y :c :color :g)
             :charts [(chart/candlestick) (chart/line)]}]
      (is (= 3 (count (get-in (build p) [:stats 0 :data facet/single-panel :candles]))))
      (is (instance? BufferedImage (plot/render-image p size))))))

(deftest review-segment-test
  (testing "K8 / K9 per-segment colours: one stroke setup per run of a colour, colours as the scale gives them"
    (let [rows (mapv (fn [i] {:x i :y (inc i) :v (double i)}) (range 6))
          img (plot/render-image {:data rows :mapping {:x :x :y :y :color :v} :theme white
                                  :scales {:color {:kind :linear :range [[255 0 0] [0 0 255]]}}
                                  :charts [(chart/segment {:yend 0 :line-width 4})]} size)]
      (is (pos? (pixels-of img [255 0 0])) "lowest value red")
      (is (pos? (pixels-of img [0 0 255])) "highest value blue"))))

(deftest review-single-body-test
  (testing "K9 a single candle body is 10 px wide"
    (let [img (plot/render-image (candle-plot {} {:data [(assoc (first ohlc) :d 1)]}) size)
          row (for [x (range 400) :let [[r g b] (rgb img x 150)] :when (= [r g b] [26 152 80])] x)]
      (is (<= 9 (count row) 11)))))

(deftest candlestick-ends-test
  (testing "the x extent reaches half the smallest gap beyond the data (outer bodies fit on a date axis)"
    (let [x-extent (fn [rows] (get-in (stats (candle-plot {} {:data rows}) 0) [:extents facet/single-panel :x]))]
      (is (= {:kind :temporal :min (.minusDays day0 1) :max (.plusDays day0 3)} (x-extent (take 3 ohlc))) "LocalDate: whole days")
      (is (= {:kind :numeric :min 0.0 :max 6.0} (x-extent (map-indexed #(assoc %2 :d (+ 1 (* 2 %1))) (take 3 ohlc)))) "numbers: gap 2")
      (is (= {:kind :temporal :min day0 :max day0} (x-extent (take 1 ohlc))) "one x: unchanged")
      (is (= {:kind :categorical :values ["a" "b"]} (x-extent (map #(assoc %1 :d %2) (take 2 ohlc) ["a" "b"]))) "levels: unchanged")))
  (testing "a candlestick and a line on LocalDate x share one axis"
    (is (instance? BufferedImage (plot/render-image (merge (candle-plot {}) {:charts [(chart/candlestick) (chart/line {:mapping {:y :c}})]}) size)))))
