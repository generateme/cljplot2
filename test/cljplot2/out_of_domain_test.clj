(ns cljplot2.out-of-domain-test
  "Values outside an aesthetic scale's explicit domain (spec A of the domains round): the scale option `:out-of-domain`
  - `:drop`, `:as-na`, `:clamp`, `nil` each chart's own behaviour - through the two scale gates
  (`aesthetic/kept-value?`, `aesthetic/attribute-fn`), bar, the charts computing their colour, hierarchies, extents,
  partial domains, the colour bar's markers and the legends' `outside` entry; the position scale's typed wadogo keys.
  Pixel checks render without legends (`:legend false`) unless they test a legend, so a legend's marker never stands
  in for a mark."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as position]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error error-data pixels]])
  (:import [java.time LocalDate]))

(def ^:private na-color (get-in theme/default-theme [:palette :na]))

(def ^:private palette (:palette theme/default-theme))

(defn- finalized-scale
  "The finalized colour (or `k`) scale of plot scale spec `s` over a numeric extent `[lo hi]`, an extent map, or `nil`."
  ([s extent] (finalized-scale :color s extent))
  ([k s extent]
   (aesthetic/scale-spec k s (if (sequential? extent) {:kind :numeric :min (first extent) :max (second extent)} extent) false palette)))

(defn- same-color? [a b] (and a b (= (c/to-color a) (c/to-color b))))

(defn- built [p] (plot/build p {:width 300 :height 200}))

(defn- image [p] (pixels (plot/render-image p {:width 300 :height 200})))

(defn- same-image? [p q] (java.util.Arrays/equals ^ints (image p) ^ints (image q)))

(defn- color-pixels
  "The number of pixels of plot `p` (the whole image, legends included) rendered in exactly colour `col`."
  [p col]
  (let [packed (c/pack col)]
    (count (filter #(== packed (bit-or (long %) 0xff000000)) (image p)))))

(deftest schema-test
  (testing "every value accepted on linear and binned scales; :drop and :as-na on ordinal and identity scales and every dimension; nil - the default"
    (doseq [v [nil :drop :as-na :clamp]]
      (is (some? (built {:data [{:x 1 :y 1 :c 1}] :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)]
                         :scales {:color {:domain [0 2] :out-of-domain v}}})))
      (is (some? (built {:data [{:x 1 :y 1 :c 1}] :mapping {:x :x :y :y :size :c} :charts [(chart/scatter)]
                         :scales {:size {:kind :binned :bins 2 :domain [0 2] :out-of-domain v}}}))))
    (doseq [v [:drop :as-na]
            [k chart-value] [[:shape (chart/scatter)] [:dash (chart/line)] [:pattern (chart/bar)]]]
      (is (some? (built {:data [{:x "1" :y 1 :c "a"}] :mapping {:x :x :y :y k :c} :charts [chart-value]
                         :scales {k {:domain ["a"] :out-of-domain v}}})) [k v]))
    (doseq [v [:drop :as-na]]
      (is (some? (built {:data [{:x 1 :y 1 :c :red}] :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)]
                         :scales {:color {:kind :identity :out-of-domain v}}})))))
  (testing "boundary: :clamp needs a linear or binned scale - an ordinal (levels have no ends) or identity scale rejects it"
    (is (= :out-of-domain (:key (error #(built {:data [{:x 1 :y 1 :c "a"}] :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)]
                                                :scales {:color {:domain ["a" "b"] :out-of-domain :clamp}}})))))
    (is (= :out-of-domain (:key (error #(built {:data [{:x 1 :y 1 :c "a"}] :mapping {:x :x :y :y :shape :c} :charts [(chart/scatter)]
                                                :scales {:shape {:out-of-domain :clamp}}})))))
    (is (= :out-of-domain (:key (error #(built {:data [{:x 1 :y 1 :c :red}] :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)]
                                                :scales {:color {:kind :identity :out-of-domain :clamp}}}))))))
  (testing "boundary: another value is rejected"
    (is (= :out-of-domain (:key (error #(built {:data [{:x 1 :y 1 :c 1}] :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)]
                                                :scales {:color {:domain [0 2] :out-of-domain :squish}}})))))))

(deftest gates-test
  (testing "kept-value?: a value outside the domain is kept for :as-na and :clamp, not for :drop or nil; nil is never kept (NA is the callers')"
    (doseq [[v kept] [[nil false] [:drop false] [:as-na true] [:clamp true]]]
      (is (= kept (boolean (aesthetic/kept-value? :color {:domain [0 10] :out-of-domain v} 11))) v)
      (is (aesthetic/kept-value? :color {:domain [0 10] :out-of-domain v} 10)))
    (is (not (aesthetic/kept-value? :color {:domain [0 10] :out-of-domain :as-na} nil))))
  (testing "linear: inside - the option changes nothing; outside - nil (nil, :drop), NA (:as-na), the nearer end's colour (:clamp); nil - always NA; a value that isn't finite - nil under every option"
    (let [plain (aesthetic/attribute-fn :color (finalized-scale {:domain [0 10]} [0 10]) na-color)]
      (doseq [option [nil :drop :as-na :clamp]
              :let [f (aesthetic/attribute-fn :color (finalized-scale (cond-> {:domain [0 10]} option (assoc :out-of-domain option)) [0 10]) na-color)]]
        (is (same-color? (plain 5) (f 5)))
        (is (same-color? na-color (f nil)))
        (is (nil? (f ##NaN)) option)
        (is (nil? (f ##Inf)) option)
        (is (nil? (f ##-Inf)) option)
        (case option
          (nil :drop) (do (is (nil? (f -1)) option) (is (nil? (f 11)) option))
          :as-na (do (is (same-color? na-color (f -1))) (is (same-color? na-color (f 11))))
          :clamp (do (is (same-color? (f 0) (f -1))) (is (same-color? (f 10) (f 11)))
                     (is (same-color? (f 10) (f 1e9)) "far beyond")
                     (is (not (same-color? (f 0) (f 10))) "the two ends differ"))))))
  (testing "boundary: a one-value domain clamps everything to its colour"
    (let [f (aesthetic/attribute-fn :color (finalized-scale {:domain [3 3] :out-of-domain :clamp} [0 9]) na-color)]
      (is (same-color? (f 3) (f -1)))
      (is (same-color? (f 3) (f 9)))))
  (testing "linear :clamp on a reversed scale takes the reversed end's colour"
    (let [f (aesthetic/attribute-fn :color (finalized-scale {:domain [0 10] :reverse true :out-of-domain :clamp} [0 10]) na-color)]
      (is (same-color? (f 0) (f -5)))
      (is (same-color? (f 10) (f 15)))))
  (testing "linear size :clamp: the end sizes"
    (let [f (aesthetic/attribute-fn :size (finalized-scale :size {:domain [0 10] :range [1 5] :out-of-domain :clamp} [0 10]) 2.0)]
      (is (= 1.0 (f -3)))
      (is (= 5.0 (f 30)))))
  (testing "binned: :clamp - below the first break the first bin, above the last the last bin; :as-na - NA; a break end is inside; NaN - nil"
    (let [s (finalized-scale {:kind :binned :bins [0 2 4 6] :out-of-domain :clamp} [0 6])
          f (aesthetic/attribute-fn :color s na-color)
          g (aesthetic/attribute-fn :color (assoc s :out-of-domain :as-na) na-color)]
      (is (same-color? (f 1) (f -1)))
      (is (same-color? (f 5) (f 7)))
      (is (same-color? (f 5) (f 6)) "the last break is in the last bin")
      (is (same-color? na-color (g -1)))
      (is (same-color? na-color (g 7)))
      (is (nil? (f ##NaN)))
      (is (nil? (g ##Inf)))
      (is (nil? (f "x")) "not a number - no attribute")))
  (testing "binned with a bin count over an explicit domain: the domain's ends are the outer breaks"
    (let [f (aesthetic/attribute-fn :color (finalized-scale {:kind :binned :bins 2 :domain [2 6] :out-of-domain :clamp} [0 9]) na-color)]
      (is (same-color? (f 3) (f 0)))
      (is (same-color? (f 5) (f 9)))))
  (testing "linear over dates: :clamp at the dates' ends"
    (let [lo (LocalDate/parse "2020-01-01") hi (LocalDate/parse "2020-12-31")
          f (aesthetic/attribute-fn :color (finalized-scale {:domain [lo hi] :out-of-domain :clamp} {:kind :temporal :min lo :max hi}) na-color)]
      (is (same-color? (f lo) (f (LocalDate/parse "2019-06-01"))))
      (is (same-color? (f hi) (f (LocalDate/parse "2021-06-01"))))))
  (testing "ordinal: a level outside the domain - nil, or NA with :as-na"
    (let [s (finalized-scale :shape {:domain ["a" "b"]} {:kind :categorical :values ["a" "b" "c"]})]
      (is (nil? ((aesthetic/attribute-fn :shape s :na-shape) "c")))
      (is (= :na-shape ((aesthetic/attribute-fn :shape (assoc s :out-of-domain :as-na) :na-shape) "c")))
      (is (= :na-shape ((aesthetic/attribute-fn :shape (assoc s :out-of-domain :as-na) :na-shape) nil))))))

(def ^:private rows
  "Ten rows, colour value 0 .. 9."
  (for [i (range 10)] {:x i :y (mod i 3) :c i}))

(deftest outside-values-test
  (testing "the scale reports which ends the extent passes, only for :as-na and :clamp"
    (is (= #{:below :above} (:outside (finalized-scale {:domain [2 6] :out-of-domain :clamp} [0 9]))))
    (is (= #{:below} (:outside (finalized-scale {:domain [2 9] :out-of-domain :as-na} [0 9]))))
    (is (= #{:above} (:outside (finalized-scale {:domain [0 6] :out-of-domain :clamp} [0 9]))))
    (is (nil? (:outside (finalized-scale {:domain [0 9] :out-of-domain :clamp} [0 9]))) "an extent at the ends is inside")
    (is (nil? (:outside (finalized-scale {:domain [2 6] :out-of-domain :drop} [0 9]))))
    (is (nil? (:outside (finalized-scale {:domain [2 6]} [0 9]))))
    (is (nil? (:outside (finalized-scale {:domain [2 6] :out-of-domain :clamp} nil))) "no data")
    (is (= #{:levels} (:outside (finalized-scale :shape {:domain ["a"] :out-of-domain :as-na} {:kind :categorical :values ["a" "b"]}))))
    (is (= #{:below :above} (:outside (finalized-scale {:kind :binned :bins [2 4 6] :out-of-domain :clamp} [0 9]))) "a binned scale's breaks are its domain"))
  (testing "built plots: dates past both ends; an NA level isn't outside"
    (let [day #(LocalDate/of 2020 1 (inc %))
          b (built {:data (for [i (range 10)] {:x i :y i :d (day i)}) :mapping {:x :x :y :y :color :d} :charts [(chart/scatter)]
                    :scales {:color {:domain [(day 2) (day 6)] :out-of-domain :clamp}}})]
      (is (= #{:below :above} (get-in b [:scale-specs :color :outside]))))
    (let [b (built {:data [{:x 1 :y 1 :s "a"} {:x 2 :y 2 :s nil}] :mapping {:x :x :y :y :shape :s} :charts [(chart/scatter)]
                    :scales {:shape {:domain ["a"] :out-of-domain :as-na}}})]
      (is (nil? (get-in b [:scale-specs :shape :outside])))
      (is (not-any? #(= "outside" (:label %)) (aesthetic/legend-entries :shape (get-in b [:scale-specs :shape]))))))
  (testing "boundary: partial domains - the open end from the data; data wholly beyond the given end throws"
    (let [s (finalized-scale {:domain [2 nil] :out-of-domain :clamp} [0 9])]
      (is (= [2 9] (mapv long (:domain s))))
      (is (= #{:below} (:outside s))))
    (let [s (finalized-scale {:domain [nil 6] :out-of-domain :as-na} [0 9])]
      (is (= [0 6] (mapv long (:domain s))))
      (is (= #{:above} (:outside s))))
    (is (= :domain (:key (error-data #(finalized-scale {:domain [20 nil] :out-of-domain :clamp} [0 9])))))
    (let [b (built {:data rows :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)] :scales {:color {:domain [nil 6] :out-of-domain :clamp}}})]
      (is (= [0 6] (mapv long (get-in b [:scale-specs :color :domain]))) "through a build")
      (is (< 8.5 (double (second (get-in b [:finalized-scales :x :domain])))) "the x extent holds the clamped rows"))))

(defn- scatter-plot [option]
  {:data rows :mapping {:x :x :y :y :color :c} :charts [(chart/scatter)] :legend false
   :scales {:color (cond-> {:domain [2 6]} option (assoc :out-of-domain option))}})

(deftest row-charts-test
  (testing "scatter: nil and :drop drop the rows outside, counted; :as-na and :clamp keep them, uncounted - and the x extent holds them"
    (doseq [[option points dropped] [[nil 5 5] [:drop 5 5] [:as-na 10 0] [:clamp 10 0]]
            :let [b (built (scatter-plot option))]]
      (is (= points (count (:x (first (vals (get-in b [:stats 0 :data])))))) option)
      (is (= dropped (get-in b [:stats 0 :dropped :color] 0)) option)
      (is (= (boolean (#{:as-na :clamp} option)) (< 6.5 (double (second (get-in b [:finalized-scales :x :domain])))))
          (str option ": the x domain holds the kept rows (x up to 9) or only those inside (x up to 6)"))))
  (testing "scatter marks: :as-na draws the NA colour, :clamp doesn't; nil draws neither"
    (is (pos? (color-pixels (scatter-plot :as-na) na-color)))
    (is (zero? (color-pixels (scatter-plot :clamp) na-color)))
    (is (zero? (color-pixels (scatter-plot nil) na-color))))
  (testing "line, segment, tile and cloud: rows outside dropped and counted by default, kept and drawn NA under :as-na"
    (doseq [[label p] [["line" {:data rows :mapping {:x :x :y :y :color :c} :charts [(chart/line {:line-width 4})]}]
                       ["segment" {:data rows :mapping {:x :x :y :y :xend (comp inc :x) :yend :y :color :c} :charts [(chart/segment {:line-width 4})]}]
                       ["tile" {:data rows :mapping {:x :x :y :y :color :c} :charts [(chart/tile)]}]
                       ["cloud" {:data (map #(assoc % :z (:x %)) rows) :mapping {:x :x :y :y :z :z :color :c} :charts [(chart/cloud {:size 8})]}]]
            :let [with-option (fn [option] (assoc p :legend false :scales {:color (cond-> {:domain [2 6]} option (assoc :out-of-domain option))}))]]
      (is (pos? (get-in (built (with-option nil)) [:stats 0 :dropped :color] 0)) label)
      (is (zero? (get-in (built (with-option :as-na)) [:stats 0 :dropped :color] 0)) label)
      (is (zero? (color-pixels (with-option nil) na-color)) label)
      (is (pos? (color-pixels (with-option :as-na) na-color)) label))))

(def ^:private bar-rows
  "Three rows per bar, the bar's colour value (their mean) 0 .. 9."
  (for [i (range 10) _ (range 3)] {:x (str i) :y 1 :c i}))

(defn- bar-plot [option]
  {:data bar-rows :mapping {:x :x :y :y :color :c} :charts [(chart/bar {:color-stat :mean})] :legend false
   :scales {:color (cond-> {:domain [2 6]} option (assoc :out-of-domain option))}})

(deftest bar-test
  (let [bars (fn [b] (count (mapcat :bars (vals (get-in b [:stats 0 :data])))))]
    (testing "nil (bar's default): every bar kept, the bars outside in the NA colour, uncounted, :na holds :color"
      (let [b (built (bar-plot nil))]
        (is (= 10 (bars b)))
        (is (zero? (get-in b [:stats 0 :dropped :color] 0)))
        (is (contains? (get-in b [:stats 0 :na]) :color))
        (is (pos? (color-pixels (bar-plot nil) na-color)))))
    (testing ":drop drops the bars whose colour value lies outside, counting their rows (three per bar)"
      (let [b (built (bar-plot :drop))]
        (is (= 5 (bars b)))
        (is (= 15 (get-in b [:stats 0 :dropped :color])))
        (is (not (contains? (get-in b [:stats 0 :na]) :color)))
        (is (zero? (color-pixels (bar-plot :drop) na-color)))))
    (testing ":clamp keeps them in the end colours, :as-na in the NA colour; neither counts nor marks them NA"
      (doseq [option [:clamp :as-na]
              :let [b (built (bar-plot option))]]
        (is (= 10 (bars b)) option)
        (is (zero? (get-in b [:stats 0 :dropped :color] 0)) option)
        (is (not (contains? (get-in b [:stats 0 :na]) :color)) option))
      (is (zero? (color-pixels (bar-plot :clamp) na-color)))
      (is (pos? (color-pixels (bar-plot :as-na) na-color))))))

(def ^:private bin-rows
  "Points of a 2D histogram on the diagonal: counts 1 .. 4 in four cells."
  (for [[x n] [[0.5 1] [1.5 2] [2.5 3] [3.5 4]] _ (range n)] {:x x :y x}))

(defn- with-color-option
  "Plot `p` without legends, its colour scale's `:out-of-domain` set to `option` (`nil` - left out)."
  [p option]
  (cond-> (assoc p :legend false) option (assoc-in [:scales :color :out-of-domain] option)))

(deftest computed-color-test
  (testing "charts computing their colour: nil and :drop skip the marks outside (same image), uncounted; :clamp paints them (the image changes, no NA); :as-na paints NA"
    (doseq [[label p check-na] [["bin2d" {:data bin-rows :mapping {:x :x :y :y} :charts [(chart/bin2d {:bins 4})] :scales {:color {:domain [2 3]}}} true]
                                ["field" {:charts [(chart/field {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]}})] :scales {:color {:domain [0.5 1.5]}}} true]
                                ["surface" {:charts [(chart/surface {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]} :shade false})]
                                            :scales {:color {:domain [0.5 1.5]}}} true]
                                ["contour" {:charts [(chart/contour {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]}})] :scales {:color {:domain [0.5 1.5]}}} false]
                                ["correlation cell (r = 1)" {:data rows :mapping {:x :x :y :c} :charts [(chart/correlation {:glyph :square})]
                                                            :scales {:color {:domain [-0.5 0.5]}}} true]
                                ["pairwise cell (r = 1)" {:data rows :mapping {:x :x :y :c} :charts [(chart/pairwise {:glyph :shade})]
                                                          :scales {:color {:domain [-0.5 0.5]}}} true]]]
      (is (same-image? (with-color-option p nil) (with-color-option p :drop)) label)
      (is (not (same-image? (with-color-option p nil) (with-color-option p :clamp))) label)
      (is (nil? (get-in (built (with-color-option p :drop)) [:stats 0 :dropped :color])) label)
      (is (zero? (color-pixels (with-color-option p :clamp) na-color)) label)
      (when check-na
        (is (pos? (color-pixels (with-color-option p :as-na) na-color)) label))))
  (testing "a value that isn't finite (a field's +Inf; the finite half lies inside the domain) is never drawn, whatever the option"
    (let [p {:charts [(chart/field {:f (fn [[x _]] (if (< x 0.5) ##Inf x)) :domain {:x [0 1] :y [0 1]}})] :scales {:color {:domain [0.4 1.1]}}}]
      (is (zero? (color-pixels (with-color-option p :as-na) na-color))))))

(deftest hierarchy-by-name-test
  (testing "partition coloured by name keeps nodes outside the domain in the NA colour, also under :drop (a named cell can't vanish)"
    (let [p {:data [{:g "a" :n "x" :v 1} {:g "a" :n "y" :v 2} {:g "b" :n "z" :v 3}] :mapping {:value :v :hierarchy [:g :n]}
             :charts [(chart/partition {:color-by :name})] :legend false :scales {:color {:domain ["a" "x"]}}}]
      (is (same-image? p (assoc-in p [:scales :color :out-of-domain] :drop)))
      (is (pos? (color-pixels (assoc-in p [:scales :color :out-of-domain] :drop) na-color))))))

(deftest group-na-attribute-test
  (testing "a group drawn as NA takes the chart's own attribute where its style has none: a line's ordinal size (its line width)"
    (let [line-plot (fn [option] {:data (for [g ["a" "b"] x (range 3)] {:x x :y (if (= g "a") x (- 3 x)) :g g}) :mapping {:x :x :y :y :size :g}
                                  :charts [(chart/line)] :legend false :scales {:size (cond-> {:domain ["a"]} option (assoc :out-of-domain option))}})]
      (is (zero? (get-in (built (line-plot :as-na)) [:stats 0 :dropped :size] 0)))
      (is (not (same-image? (line-plot nil) (line-plot :as-na))) "the group outside is drawn")))
  (testing "a tile cell sized as NA fills its cell"
    (let [tile-plot (fn [option] {:data rows :mapping {:x :x :y :y :color :c :size :c} :charts [(chart/tile)] :legend false
                                  :scales {:size (cond-> {:domain [2 6]} option (assoc :out-of-domain option))}})]
      (is (not (same-image? (tile-plot nil) (tile-plot :as-na)))))))

(defn- marker-plot
  "Invisible points (size 0) coloured 0 .. 9 under a colour domain and `:out-of-domain` - only the legend shows colours."
  [domain option]
  {:data rows :mapping {:x :x :y :y :color :c} :charts [(chart/scatter {:size 0})] :scales {:color {:domain domain :out-of-domain option}}})

(deftest colorbar-markers-test
  (let [markers (fn [p] (#'legend/colorbar-markers (:scale (last (legend/legends (built p))))))
        end-colors (fn [domain] (let [f (aesthetic/attribute-fn :color (finalized-scale {:domain domain} [0 9]) na-color)]
                                  [(f (first domain)) (f (second domain))]))
        ;; exact end-colour pixels: the bar itself hardly samples exactly an end, a triangle is a block of it
        end-pixels (fn [domain option] (let [[lo-color hi-color] (end-colors domain)]
                                         [(color-pixels (marker-plot domain option) lo-color) (color-pixels (marker-plot domain option) hi-color)]))
        triangle? (fn [with without] (< (+ without 30) with))]
    (testing ":clamp - a triangle at each end with values beyond it"
      (is (= [true true] (mapv boolean ((juxt :top :bottom) (markers (marker-plot [2 6] :clamp))))))
      (is (= [true false] (mapv boolean ((juxt :top :bottom) (markers (marker-plot [0 6] :clamp))))))
      (is (= [false true] (mapv boolean ((juxt :top :bottom) (markers (marker-plot [2 9] :clamp))))))
      (is (= [false false] (mapv boolean ((juxt :top :bottom) (markers (marker-plot [0 9] :clamp))))) "no values beyond"))
    (testing "the triangles are drawn: blocks of the end colours, absent without values beyond"
      (let [[lo-clamp hi-clamp] (end-pixels [2 6] :clamp)
            [lo-drop hi-drop] (end-pixels [2 6] :drop)]
        (is (triangle? lo-clamp lo-drop))
        (is (triangle? hi-clamp hi-drop)))
      (let [[lo-clamp hi-clamp] (end-pixels [0 6] :clamp)
            [lo-drop hi-drop] (end-pixels [0 6] :drop)]
        (is (not (triangle? lo-clamp lo-drop)) "nothing below 0")
        (is (triangle? hi-clamp hi-drop))))
    (testing ":as-na - the NA swatch when values lie outside; nil and :drop - nothing"
      (is (:swatch (markers (marker-plot [2 6] :as-na))))
      (is (not (:swatch (markers (marker-plot [0 9] :as-na)))))
      (is (not (:swatch (markers (marker-plot [2 6] :drop)))))
      (is (not (:swatch (markers (marker-plot [2 6] nil))))))
    (testing "the swatch is drawn: a block of the NA colour only with values outside"
      (is (< 100 (color-pixels (marker-plot [2 6] :as-na) na-color)))
      (is (zero? (color-pixels (marker-plot [0 9] :as-na) na-color))))))

(deftest legend-outside-entry-test
  (testing ":as-na adds an `outside` entry (the NA attribute) to ordinal and size legends with values outside, before an NA entry"
    (let [s (finalized-scale :shape {:domain ["a"] :out-of-domain :as-na} {:kind :categorical :values ["a" "b"]})]
      (is (= [{:label "a" :value "a"} {:label "outside" :value nil}] (aesthetic/legend-entries :shape s)))
      (is (= [{:label "outside" :value nil} {:label "NA" :value nil}] (take-last 2 (aesthetic/legend-entries :shape (assoc s :na-level true))))))
    (is (= "outside" (:label (last (aesthetic/legend-entries :size (finalized-scale :size {:domain [2 6] :out-of-domain :as-na} [0 9]))))))
    (testing "boundary: no entry without values outside, or for :clamp"
      (is (not-any? #(= "outside" (:label %)) (aesthetic/legend-entries :size (finalized-scale :size {:domain [0 9] :out-of-domain :as-na} [0 9]))))
      (is (not-any? #(= "outside" (:label %)) (aesthetic/legend-entries :size (finalized-scale :size {:domain [2 6] :out-of-domain :clamp} [0 9]))))))
  (testing "a merged legend (colour and shape of one variable, both :as-na) has one `outside` entry"
    (let [b (built {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "b"}] :mapping {:x :x :y :y :color :g :shape :g} :charts [(chart/scatter)]
                    :scales {:color {:domain ["a"] :out-of-domain :as-na} :shape {:domain ["a"] :out-of-domain :as-na}}})
          contents (legend/legends b)]
      (is (= 1 (count contents)) "one merged legend")
      (is (= 1 (count (filter #(= "outside" (str (:label %))) (mapcat :entries contents))))))))

(deftest position-keys-test
  (testing "the position :clamp clamps to the finalized domain (nice and padded), not the given one"
    (let [b (built {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:domain [2 6] :clamp true}}})
          xs (get-in b [:finalized-scales :x])
          fx (position/forward-fn xs)
          [lo] (:domain xs)]
      (is (< (double lo) 2.0) "padded below the given end")
      (is (== (fx lo) (fx 0)))
      (is (< (fx lo) (fx 2)))))
  (testing "a nil wadogo key means wadogo's default"
    (is (same-image? {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:clamp nil :round nil}}}
                     {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]})))
  (testing "boundary: an empty :align, a base of 1 or less, an exponent of 0 are rejected"
    (is (= :align (:key (error #(built {:data rows :mapping {:x (comp str :x) :y :y} :charts [(chart/scatter)] :scales {:x {:align []}}})))))
    (is (= :base (:key (error #(built {:data rows :mapping {:x (comp inc :x) :y :y} :charts [(chart/scatter)] :scales {:x {:kind :log :base 1}}})))))
    (is (= :exponent (:key (error #(built {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:kind :pow :exponent 0}}})))))))
