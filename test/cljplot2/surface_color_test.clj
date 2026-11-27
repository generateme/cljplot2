(ns cljplot2.surface-color-test
  "A surface coloured by a mapped `:color` column (spec C, vault notes 07): `field/lattice-values`' second value
  (`extra-of`), the colour source order (fixed `:color` > a mapped `:color` of gridded data > the computed tone), the
  rules for sources that can't take a mapping, kept and dropped nodes, extents, the quad's colour (mean or most common
  corner, NA corners, `:out-of-domain`, dates), the legend's key glyph and `:fu`'s colour value. Pixel checks draw
  without shading and without legends (plot `:legend false` - no colour bar either), so each quad shows its exact
  colour."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.chart.surface :as surface]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error error-ins pixels]])
  (:import [java.time LocalDate]))

(def ^:private na-color (get-in theme/default-theme [:palette :na]))

(def ^:private palette-colors (get-in theme/default-theme [:palette :color]))

(defn- built [p] (plot/build p {:width 300 :height 300}))

(defn- stats [p] (first (:stats (built p))))

(defn- color-pixels
  "The number of pixels of plot `p` rendered in exactly colour `col`."
  [p col]
  (let [packed (c/pack col)]
    (count (filter #(== packed %) (pixels (plot/render-image p {:width 300 :height 300}))))))

(defn- lattice
  "Rows of a 3 x 3 lattice (x, y in 0..2), z = x y, `:w` = x + y, `:k` - `a` for x 0, else `b`."
  []
  (vec (for [x (range 3) y (range 3)] {:x x :y y :z (* x y) :w (+ x y) :k (if (zero? x) "a" "b")})))

(defn- quad-plot
  "A plot of one quad (a 2 x 2 lattice) whose corners take colour values `[c00 c10 c11 c01]` (at x, y), drawn flat."
  ([corners] (quad-plot corners nil))
  ([[c00 c10 c11 c01] color-scale]
   (cond-> {:data [{:x 0 :y 0 :z 0 :c c00} {:x 1 :y 0 :z 0 :c c10} {:x 1 :y 1 :z 0 :c c11} {:x 0 :y 1 :z 0 :c c01}]
            :mapping {:x :x :y :y :z :z :color :c}
            :charts [(chart/surface {:shade false})]
            :legend false}
     color-scale (assoc :scales {:color color-scale}))))

(deftest lattice-values-extra-test
  (let [rows [{:x 0 :y 0 :v 1 :e "a"} {:x 1 :y 0 :v 2 :e 3} {:x 0 :y 1 :v 3 :e nil} {:x 1 :y 1 :v 4 :e ##NaN}]
        {:keys [vss extras]} (field/lattice-values rows :x :y :v :z :e)]
    (testing "the second value per lattice point, as in its row"
      (is (= [[1.0 3.0] [2.0 4.0]] vss))
      (is (= ["a" nil] (first extras)))
      (is (= 3 (first (second extras))))
      (is (Double/isNaN (second (second extras)))))
    (testing "without a second accessor, no :extras"
      (is (not (contains? (field/lattice-values rows :x :y :v :z) :extras))))))

(deftest color-source-test
  (testing "computed aesthetics: a mapped colour of gridded data and a fixed colour compute none"
    (is (= {} (surface/computed-aesthetics {} {:x :x :y :y :z :z :color :w})))
    (is (= {} (surface/computed-aesthetics {:color :red} {:x :x :y :y :z :z :color :w})))
    (is (= {:color "z"} (surface/computed-aesthetics {} {:x :x :y :y :z :z})))
    (is (= {:color nil} (surface/computed-aesthetics {:f identity} {:color :w}))))
  (testing "mapped-color?"
    (is (true? (surface/mapped-color? {} {:color :w})))
    (is (false? (surface/mapped-color? {} {:color nil})))
    (is (false? (surface/mapped-color? {:color :red} {:color :w})))
    (is (false? (surface/mapped-color? {:density true} {:color :w}))))
  (testing "a fixed-colour surface draws no key into another chart's colour legend"
    (let [b (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :k}
                    :charts [(chart/surface {:color :red :domain {:x [0 2] :y [0 2] :z [0 4]}})
                             (chart/cloud {:domain {:x [0 2] :y [0 2] :z [0 4]}})]})]
      (is (= [1] (map :chart (:charts (first (legend/legends b))))))))
  (testing "a fixed colour wins over a :color inherited from the plot: no colour scale"
    (let [b (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :w} :charts [(chart/surface {:color :red})]})]
      (is (empty? (:scale-specs b)))
      (is (not (contains? (:dropped (first (:stats b))) :color)))))
  (testing "a cloud and a surface mapping :color share one scale"
    (let [b (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :w}
                    :charts [(chart/surface {:domain {:x [0 2] :y [0 2] :z [0 4]}})
                             (chart/cloud {:domain {:x [0 2] :y [0 2] :z [0 4]} :data [{:x 1 :y 1 :z 1 :w 10}]})]})]
      (is (= [0.0 10.0] (mapv double (get-in b [:scale-specs :color :domain]))))))
  (testing "a computed surface colour beside a cloud mapping :color is still an error"
    (is (= #{[:charts 1 :options :mapping :color]}
           (error-ins #(built {:data (lattice) :mapping {:x :x :y :y :z :z}
                               :charts [(chart/surface {:domain {:x [0 2] :y [0 2] :z [0 4]}})
                                        (chart/cloud {:domain {:x [0 2] :y [0 2] :z [0 4]} :mapping {:color :w}})]}))))))

(deftest color-rules-test
  (let [f-opts {:f (fn [[x y]] (* x y)) :domain {:x [0 1] :y [0 1]}}]
    (testing "the chart's own mapping: a fixed and a mapped colour together"
      (is (= #{[:charts 0 :options :mapping :color]}
             (error-ins #(built {:data (lattice) :mapping {:x :x :y :y :z :z}
                                 :charts [(chart/surface {:color :red :mapping {:color :w}})]})))))
    (testing "the chart's own mapping: :f, :fu, :density take no mapped colour"
      (doseq [opts [f-opts {:fu (fn [[u v]] [u v 0]) :u [0 1] :v [0 1]} {:density true}]]
        (is (= #{[:charts 0 :options :mapping :color]}
               (error-ins #(built {:data (lattice) :mapping {:x :x :y :y}
                                   :charts [(chart/surface (assoc opts :mapping {:color :w}))]})))
            (str (keys opts)))))
    (testing "an inherited :color on a function, parametric or density surface without a fixed colour"
      (doseq [opts [f-opts {:fu (fn [[u v]] [u v 0]) :u [0 1] :v [0 1]} {:density true}]]
        (is (= {:key :mapping :in [:mapping :color] :value :w}
               (select-keys (error #(built {:data (lattice) :mapping {:x :x :y :y :color :w} :charts [(chart/surface opts)]}))
                            [:key :in :value]))
            (str (keys opts)))))
    (testing "a mapped colour without a valid value is a mapping error"
      (doseq [v [nil ##NaN]]
        (is (= {:key :mapping :in [:mapping :color]}
               (select-keys (error #(built {:data (mapv (fn [r] (assoc r :w v)) (lattice)) :mapping {:x :x :y :y :z :z :color :w}
                                            :charts [(chart/surface)]}))
                            [:key :in])))))
    (testing "an inherited :color set to nil, or a fixed colour, builds"
      (is (built {:data (lattice) :mapping {:color :w} :charts [(chart/surface (assoc f-opts :mapping {:color nil}))]}))
      (is (built {:data (lattice) :mapping {:color :w} :charts [(chart/surface (assoc f-opts :color :red))]})))))

(deftest kept-nodes-test
  (testing "the mesh's colours follow its nodes, row by row (y), x within a row"
    (is (= [0 10 20 1 11 21]
           (:colors (:mesh (val (first (:data (stats {:data (vec (for [y (range 2) x (range 3)] {:x x :y y :z 0 :w (+ (* 10 x) y)}))
                                                      :mapping {:x :x :y :y :z :z :color :w} :charts [(chart/surface)]})))))))))
  (testing "numeric colour: linear scale over the column, no drops"
    (let [b (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :w} :charts [(chart/surface)]})]
      (is (= :linear (get-in b [:scale-specs :color :kind])))
      (is (= [0.0 4.0] (mapv double (get-in b [:scale-specs :color :domain]))))
      (is (= 0 (get-in (first (:stats b)) [:dropped :color])))
      (is (= #{} (:na (first (:stats b)))))))
  (testing "categorical colour: ordinal levels"
    (is (= ["a" "b"] (get-in (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :k} :charts [(chart/surface)]})
                             [:scale-specs :color :domain]))))
  (testing "a binned scale over the column: quantile breaks from the column's values"
    (is (= [0.0 2.0 4.0] (mapv double (get-in (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :w}
                                                      :scales {:color {:kind :binned :bins {:quantiles 2}}} :charts [(chart/surface)]})
                                              [:scale-specs :color :breaks])))))
  (testing "a single-level column: an ordinal scale of one level"
    (is (= ["q"] (get-in (built {:data (mapv #(assoc % :k "q") (lattice)) :mapping {:x :x :y :y :z :z :color :k} :charts [(chart/surface)]})
                         [:scale-specs :color :domain]))))
  (testing "facets: drops add up over the panels, extents merge"
    (let [rows (vec (for [f ["p" "q"] r (lattice)] (cond-> (assoc r :f f) (= f "q") (update :w + 10))))]
      (is (= 3 (get-in (stats {:data rows :mapping {:x :x :y :y :z :z :color :w :facet :f} :scales {:color {:domain [0 12]}}
                               :charts [(chart/surface)]})
                       [:dropped :color])))
      (is (= [0.0 14.0] (mapv double (get-in (built {:data rows :mapping {:x :x :y :y :z :z :color :w :facet :f} :charts [(chart/surface)]})
                                             [:scale-specs :color :domain]))))))
  (testing "values outside an explicit domain (default :out-of-domain) and invalid values drop their nodes, counted"
    (let [rows (mapv #(cond-> % (= [2 2] [(:x %) (:y %)]) (assoc :w ##NaN)) (lattice))
          st (stats {:data rows :mapping {:x :x :y :y :z :z :color :w} :scales {:color {:domain [0 2]}}
                     :charts [(chart/surface)]})]
      ;; w = 3 at (1 2), (2 1); NaN at (2 2)
      (is (= 3 (get-in st [:dropped :color])))
      (is (= 3 (count (filter nil? (:nodes (:mesh (val (first (:data st))))))))))
    (testing "under :as-na and :clamp the nodes stay"
      (doseq [o [:as-na :clamp]]
        (is (= 0 (get-in (stats {:data (lattice) :mapping {:x :x :y :y :z :z :color :w}
                                 :scales {:color {:domain [0 2] :out-of-domain o}} :charts [(chart/surface)]})
                         [:dropped :color]))))))
  (testing "a nil colour value is NA: its node stays, :na holds :color"
    (let [rows (mapv #(cond-> % (zero? (:x %)) (assoc :w nil)) (lattice))
          st (stats {:data rows :mapping {:x :x :y :y :z :z :color :w} :charts [(chart/surface)]})]
      (is (= 0 (get-in st [:dropped :color])))
      (is (= #{:color} (:na st)))
      (is (not-any? nil? (:nodes (:mesh (val (first (:data st)))))))))
  (testing "with the NA level off, a nil colour value's node is left out, counted"
    (let [st (stats {:data (mapv #(cond-> % (zero? (:x %)) (assoc :w nil)) (lattice)) :groups {:na false}
                     :mapping {:x :x :y :y :z :z :color :w} :charts [(chart/surface)]})]
      (is (= 3 (get-in st [:dropped :color])))
      (is (= #{} (:na st)))))
  (testing "nodes outside a position domain take no part in the colour extent"
    (is (= [1.0 4.0] (mapv double (get-in (built {:data (lattice) :mapping {:x :x :y :y :z :z :color :w}
                                                  :charts [(chart/surface {:domain {:x [0.5 2]}})]})
                                          [:scale-specs :color :domain]))))))

(defn- attribute [scale-spec v] ((aesthetic/attribute-fn :color scale-spec na-color) v))

(deftest quad-color-test
  (testing "numbers: the corners' mean"
    (let [p (quad-plot [0 0 0 4])
          spec (get-in (built p) [:scale-specs :color])]
      (is (pos? (color-pixels p (attribute spec 1.0))))
      (is (zero? (color-pixels p (attribute spec 0.0))))))
  (testing "a binned scale takes the mean too"
    (let [p (quad-plot [0 0 0 4] {:kind :binned :bins [0 0.5 1.5 4]})
          spec (get-in (built p) [:scale-specs :color])]
      (is (pos? (color-pixels p (attribute spec 1.0))))
      (is (zero? (color-pixels p (attribute spec 0.0))) "not the most common corner")))
  (testing "a mean on a bin edge takes the upper bin"
    (let [p (quad-plot [0 0 2 2] {:kind :binned :bins [0 1 4]})
          spec (get-in (built p) [:scale-specs :color])]
      (is (pos? (color-pixels p (attribute spec 2.0))))
      (is (zero? (color-pixels p (attribute spec 0.5))))))
  (testing "dates on a linear scale: the mean date"
    (let [d (LocalDate/of 2026 1 1)
          p (quad-plot [d d d (.plusDays d 4)])
          spec (get-in (built p) [:scale-specs :color])]
      (is (= :linear (:kind spec)))
      (is (pos? (color-pixels p (attribute spec (.plusDays d 1)))))
      (is (zero? (color-pixels p (attribute spec d))))))
  (testing "levels: the most common corner"
    (is (pos? (color-pixels (quad-plot ["a" "b" "b" "b"]) (second palette-colors))))
    (is (zero? (color-pixels (quad-plot ["a" "b" "b" "b"]) (first palette-colors)))))
  (testing "a tie goes to the first corner (x 0, y 0)"
    (is (pos? (color-pixels (quad-plot ["b" "a" "a" "b"]) (second palette-colors))))
    (is (zero? (color-pixels (quad-plot ["b" "a" "a" "b"]) (first palette-colors))))
    (is (pos? (color-pixels (quad-plot ["a" "b" "b" "a"]) (first palette-colors))))
    (is (pos? (color-pixels (quad-plot ["b" "a" "b" "a"]) (second palette-colors))) "first, not last")
    (is (zero? (color-pixels (quad-plot ["b" "a" "b" "a"]) (first palette-colors)))))
  (testing "numbers on an ordinal scale: the most common corner, not the mean"
    (let [p (quad-plot [3 1 1 1] {:kind :ordinal})]
      (is (pos? (color-pixels p (first palette-colors))))))
  (testing "a nil corner makes the quad NA (numbers and levels)"
    (is (pos? (color-pixels (quad-plot [0 1 2 nil]) na-color)))
    (is (pos? (color-pixels (quad-plot ["a" "a" "a" nil]) na-color))))
  (testing "an identity scale draws the most common colour itself"
    (is (pos? (color-pixels (quad-plot [:red :red :blue :red] {:kind :identity}) :red))))
  (testing ":out-of-domain on the corners' values"
    (let [domain {:domain [0 2]}
          end (attribute (get-in (built (quad-plot [3 3 3 3] (assoc domain :out-of-domain :clamp))) [:scale-specs :color]) 2.0)]
      (is (= 4 (get-in (stats (quad-plot [3 3 3 3] domain)) [:dropped :color])) ":drop - every node left out")
      (is (= 4 (get-in (stats (quad-plot [3 3 3 3] (assoc domain :out-of-domain :drop))) [:dropped :color])) "explicit :drop")
      (is (zero? (color-pixels (quad-plot [3 3 3 3] (assoc domain :out-of-domain :drop)) end)))
      (is (pos? (color-pixels (quad-plot [3 3 3 3] (assoc domain :out-of-domain :as-na)) na-color)))
      (is (pos? (color-pixels (quad-plot [3 3 3 3] (assoc domain :out-of-domain :clamp)) end))))
    (testing ":as-na - a corner outside makes the quad NA, though the mean lies inside"
      (is (pos? (color-pixels (quad-plot [3 3 -1 -1] {:domain [0 2] :out-of-domain :as-na}) na-color)))
      (is (pos? (color-pixels (quad-plot [1 1 1 3] {:domain [0 2] :out-of-domain :as-na}) na-color))))
    (testing ":clamp - corners clamped before the mean"
      (let [p (quad-plot [100 0 0 0] {:domain [0 2] :out-of-domain :clamp})
            spec (get-in (built p) [:scale-specs :color])]
        (is (pos? (color-pixels p (attribute spec 0.5))))
        (is (zero? (color-pixels p (attribute spec 2.0))))))))

(deftest legend-glyph-test
  (testing "a categorical surface colour draws its legend keys"
    (let [p (fn [legend?] {:data (lattice) :mapping {:x :x :y :y :z :z :color :k}
                           :charts [(chart/surface {:style :wireframe :legend legend?})]})]
      (is (> (color-pixels (p nil) (first palette-colors)) (color-pixels (p false) (first palette-colors))))
      (is (= ["a" "b"] (map :label (:entries (first (filter #(= :legend (:kind %)) (legend/legends (built (p nil))))))))))))

(deftest parametric-color-value-test
  (testing ":fu's 4th value is the computed colour value"
    (is (= [0.0 1.0] (mapv double (get-in (built {:charts [(chart/surface {:fu (fn [[u v]] [u v 0 (* u v)]) :u [0 1] :v [0 1] :n 4})]})
                                          [:scale-specs :color :domain]))))))
