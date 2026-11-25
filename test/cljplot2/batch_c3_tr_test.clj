(ns cljplot2.batch-c3-tr-test
  "Batch C3, text repel and text along a path: text `:overlapping-labels :repel` (`:repel-distance`, `:label-leader`,
  `measure/repelled-placements`) and `:along-path` (`:path-position`, `cljplot2.graphics.text-path`)."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.text-path :as text-path]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 300 :height 200})

(defn- pixels
  "Gives a hash of the pixels of a rendered plot (equal images - equal hashes; short failure reports)."
  [p]
  (let [^BufferedImage img (plot/render-image p size)
        w (.getWidth img) h (.getHeight img)]
    (hash (vec (.getRGB img 0 0 w h nil 0 w)))))

(defn- text-plot
  "Gives a plot of labels at rows `rows` (`:x :y :label`) with text options `options`, merged with `extra`."
  ([rows options] (text-plot rows options nil))
  ([rows options extra]
   (merge {:data rows
           :charts [(chart/text (merge {:mapping {:x :x :y :y :label :label}} options))]
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
          extra)))

(def ^:private one [{:x 5 :y 5 :label "label"}])
(def ^:private two [{:x 5 :y 5 :label "label"} {:x 5.2 :y 5 :label "other"}])
(def ^:private crowd (vec (for [i (range 8)] {:x 5 :y 5 :label (str "label " i)})))

(defn- repel-shifts
  "Gives the shifts of [[cljplot2.graphics.measure/repelled-placements]] (`nil` - left out)."
  [boxes points area]
  (mapv :shift (measure/repelled-placements boxes points area)))

(defn- overlapping-pairs
  "Gives the pairs of boxes, each moved by its shift, that overlap by more than 1 px (`nil` shifts left out)."
  [boxes shifts]
  (let [moved (vec (keep (fn [[{:keys [^double x ^double y] :as b} s]]
                           (when-let [[^double dx ^double dy] s] (assoc b :x (+ x dx) :y (+ y dy))))
                         (map vector boxes shifts)))]
    (for [i (range (count moved)) j (range (inc i) (count moved))
          :let [a (moved i) b (moved j)
                over (fn [k l] (- (min (+ (k a) (l a)) (+ (k b) (l b))) (max (k a) (k b))))]
          :when (and (> (over :x :w) 1.0) (> (over :y :h) 1.0))]
      [i j])))

(deftest repelled-placements-test
  (let [area {:w 200.0 :h 100.0 :distance 60.0}
        box {:x 90.0 :y 45.0 :w 30.0 :h 10.0}]
    (testing "zero, one and many boxes"
      (is (= [] (repel-shifts [] [] area)))
      (is (= [[0.0 0.0]] (repel-shifts [box] [[90.0 50.0]] area)))
      (let [boxes (vec (repeat 6 box))
            shifts (repel-shifts boxes (vec (repeat 6 [90.0 50.0])) area)]
        (is (= [0.0 0.0] (first shifts)))
        (is (every? some? shifts))
        (is (empty? (overlapping-pairs boxes shifts)) "moved boxes never overlap")
        (is (= shifts (repel-shifts boxes (vec (repeat 6 [90.0 50.0])) area)) "deterministic")))
    (testing "a box stays inside the panel"
      (let [[[^double dx ^double dy]] (repel-shifts [{:x 190.0 :y -5.0 :w 30.0 :h 10.0}] [[190.0 0.0]] area)]
        (is (<= (+ 190.0 dx 30.0) 200.0))
        (is (>= (+ -5.0 dy) 0.0))))
    (testing "a box larger than the panel stays unmoved; no free position within the distance - nil"
      (is (= [[0.0 0.0]] (repel-shifts [{:x 0.0 :y 0.0 :w 300.0 :h 10.0}] [[0.0 0.0]] area)))
      (is (= [[0.0 0.0] nil] (repel-shifts [box box] [[0.0 0.0] [0.0 0.0]] (assoc area :distance 2.0)))))
    (testing "other labels' points are obstacles, the own one isn't"
      (is (= [[0.0 0.0]] (repel-shifts [box] [[100.0 50.0]] area)))
      (let [shifts-with (fn [p] (repel-shifts [box box] [p [105.0 50.0]] area))
            [_ [^double dx ^double dy]] (shifts-with [105.0 38.0])]
        (is (not= (shifts-with [0.0 0.0]) (shifts-with [105.0 38.0])) "a moved box avoids another label's point")
        (is (not (measure/boxes-overlap? {:x (+ 90.0 dx) :y (+ 45.0 dy) :w 30.0 :h 10.0} {:x 104.0 :y 37.0 :w 2.0 :h 2.0})))))
    (testing "a box overlapping only another label's point stays"
      (is (= [[0.0 0.0] [0.0 0.0]] (repel-shifts [{:x 0.0 :y 0.0 :w 10.0 :h 10.0} box] [[100.0 50.0] [10.0 10.0]] area))))
    (testing "empty boxes never overlap"
      (is (= [[0.0 0.0] [0.0 0.0]] (repel-shifts [{:x 100.0 :y 50.0 :w 0.0 :h 0.0} box] [[100.0 50.0] [100.0 50.0]] area))))))

(deftest repelled-boxes-test
  (let [area {:w 200.0 :h 100.0 :distance 60.0}]
    (testing "boxed (large inset) and unboxed sizes mixed never overlap once moved"
      (let [boxes [{:x 80.0 :y 35.0 :w 60.0 :h 30.0} {:x 90.0 :y 45.0 :w 20.0 :h 8.0}
                   {:x 85.0 :y 40.0 :w 50.0 :h 24.0} {:x 95.0 :y 48.0 :w 18.0 :h 8.0}]
            shifts (repel-shifts boxes (vec (repeat 4 [100.0 50.0])) area)]
        (is (every? some? shifts))
        (is (empty? (overlapping-pairs boxes shifts)))
        (doseq [[{:keys [x y w h]} [dx dy]] (map vector boxes shifts)]
          (is (and (>= (+ x dx) 0.0) (>= (+ y dy) 0.0) (<= (+ x dx w) 200.0) (<= (+ y dy h) 100.0)) "whole box inside"))))
    (testing "a box touching the panel edge stays"
      (is (= [[0.0 0.0]] (repel-shifts [{:x 170.0 :y 90.0 :w 30.0 :h 10.0}] [[185.0 95.0]] area)))
      (is (= [[0.0 0.0]] (repel-shifts [{:x 0.0 :y 0.0 :w 30.0 :h 10.0}] [[15.0 5.0]] area))))))

(deftest leader-test
  (let [box {:x 90.0 :y 45.0 :w 40.0 :h 20.0}
        area {:w 300.0 :h 200.0 :distance 60.0 :leaders true}
        result (measure/repelled-placements [box box] [[80.0 55.0] [80.0 55.0]] area)]
    (testing "the unmoved label has no leader; the moved one a leader from its point to its box edge"
      (is (= {:shift [0.0 0.0] :leader nil} (result 0)))
      (let [{[^double dx ^double dy] :shift [[^double x0 ^double y0] [^double x1 ^double y1] :as leader] :leader} (result 1)
            moved {:x (+ 90.0 dx) :y (+ 45.0 dy) :w 40.0 :h 20.0}]
        (is (= [80.0 55.0] [x0 y0]))
        (is (some? leader))
        (is (or (m/== x1 (double (:x moved))) (m/== x1 (+ (double (:x moved)) 40.0))
                (m/== y1 (double (:y moved))) (m/== y1 (+ (double (:y moved)) 20.0))) "ends on the box edge")
        (is (nil? (measure/box-crosses-path? moved [[x0 y0] [x1 y1]])) "never through the box")))
    (testing "without :leaders no leader"
      (is (every? nil? (map :leader (measure/repelled-placements [box box] [[80.0 55.0] [80.0 55.0]] (dissoc area :leaders))))))))

(deftest repel-chart-test
  (testing "one label and labels apart stay where :keep draws them"
    (is (= (pixels (text-plot one {})) (pixels (text-plot one {:overlapping-labels :repel :label-leader true}))))
    (let [apart [{:x 1 :y 1 :label "a"} {:x 8 :y 8 :label "b"}]]
      (is (= (pixels (text-plot apart {})) (pixels (text-plot apart {:overlapping-labels :repel}))))))
  (testing "zero labels"
    (is (map? (plot/build (text-plot [] {:overlapping-labels :repel}) size))))
  (testing "an overlapping label moves (drawn, unlike :drop)"
    (let [repelled (pixels (text-plot two {:overlapping-labels :repel}))]
      (is (not= repelled (pixels (text-plot two {}))))
      (is (not= repelled (pixels (text-plot two {:overlapping-labels :drop}))))))
  (testing "all labels at one point; leaders drawn only with :label-leader"
    (let [without (pixels (text-plot crowd {:overlapping-labels :repel}))]
      (is (= without (pixels (text-plot crowd {:overlapping-labels :repel :label-leader false}))))
      (is (not= without (pixels (text-plot crowd {:overlapping-labels :repel :label-leader true}))))))
  (testing "a label past the panel edge moves inside; one wider than the panel stays where :keep draws it"
    (let [edge [{:x 9.9 :y 5 :label "edge label"}]]
      (is (not= (pixels (text-plot edge {})) (pixels (text-plot edge {:overlapping-labels :repel})))))
    (let [wide [{:x 5 :y 5 :label (apply str (repeat 80 "w"))}]]
      (is (= (pixels (text-plot wide {})) (pixels (text-plot wide {:overlapping-labels :repel}))))))
  (testing ":repel-distance limits the move"
    (is (= (pixels (text-plot two {:overlapping-labels :drop}))
           (pixels (text-plot two {:overlapping-labels :repel :repel-distance 1})))))
  (testing "rotated labels, facets, flipped and polar plots"
    (is (not= (pixels (text-plot two {:rotation 30})) (pixels (text-plot two {:rotation 30 :overlapping-labels :repel}))))
    (let [faceted [{:x 5 :y 5 :label "label" :f "a"} {:x 5 :y 5 :label "label" :f "b"}]]
      (is (= (pixels (text-plot faceted {} {:mapping {:facet :f}}))
             (pixels (text-plot faceted {:overlapping-labels :repel} {:mapping {:facet :f}})))))
    (is (not= (pixels (text-plot two {} {:flip true})) (pixels (text-plot two {:overlapping-labels :repel} {:flip true}))))
    (is (map? (plot/build (text-plot crowd {:overlapping-labels :repel :label-leader true} {:coord {:kind :polar}}) size)))
    (is (some? (plot/render-image (text-plot crowd {:overlapping-labels :repel :label-leader true} {:coord {:kind :polar}}) size))))
  (testing "labels in boxes: background and a large inset"
    (let [boxed {:background [255 255 255 220] :inset 8}]
      (is (not= (pixels (text-plot two boxed)) (pixels (text-plot two (assoc boxed :overlapping-labels :repel)))))
      (is (not= (pixels (text-plot crowd (assoc boxed :overlapping-labels :repel)))
                (pixels (text-plot crowd (assoc boxed :overlapping-labels :repel :inset 2)))))
      (is (not= (pixels (text-plot two (assoc boxed :rotation 30)))
                (pixels (text-plot two (assoc boxed :rotation 30 :overlapping-labels :repel :label-leader true)))))
      (testing "a box that fits only without its inset is moved; one wider than the panel with it stays"
        (let [edge [{:x 9.5 :y 5 :label "edge"}]]
          (is (not= (pixels (text-plot edge boxed)) (pixels (text-plot edge (assoc boxed :overlapping-labels :repel))))))
        (let [wide [{:x 5 :y 5 :label (apply str (repeat 30 "w"))}]]
          (is (= (pixels (text-plot wide (assoc boxed :inset 60)))
                 (pixels (text-plot wide (assoc boxed :overlapping-labels :repel :inset 60)))))))))
  (testing "invalid values"
    (is (= :repel-distance (:key (error #(plot/build (text-plot one {:overlapping-labels :repel :repel-distance 0}) size)))))
    (is (= :repel-distance (:key (error #(plot/build (text-plot one {:overlapping-labels :repel :repel-distance "far"}) size)))))
    (is (= :label-leader (:key (error #(plot/build (text-plot one {:label-leader "yes"}) size)))))
    (is (some? (error #(plot/build (text-plot one {:overlapping-labels :repel :anchor-frame :text}) size))))))

(defn- close?
  "Tests whether two vectors of `[x y angle]` are equal within 1e-9."
  [a b]
  (and (= (count a) (count b))
       (every? true? (map (fn [p q] (every? true? (map #(m/delta-eq (double %1) (double %2) 1.0e-9) p q))) a b))))

(deftest glyph-placements-test
  (let [line [[0.0 0.0] [100.0 0.0]]
        advances [0.0 10.0 20.0]]
    (testing "centred at the position, each glyph at its start"
      (is (close? [[40.0 0.0 0.0] [50.0 0.0 0.0]] (text-path/glyph-placements line advances {})))
      (is (close? [[20.0 0.0 0.0] [30.0 0.0 0.0]] (text-path/glyph-placements line advances {:position 0.2 :anchor [:left :middle]})))
      (is (close? [[30.0 0.0 0.0] [40.0 0.0 0.0]] (text-path/glyph-placements line advances {:position 0.4 :anchor [:right :middle] :offset [10 0]}))))
    (testing "on the baseline line, below the path as the text reads"
      (is (close? [[40.0 5.0 0.0] [50.0 5.0 0.0]] (text-path/glyph-placements line advances {:baseline 5.0}))))
    (testing "moved back onto the path at its ends"
      (is (close? [[0.0 0.0 0.0] [10.0 0.0 0.0]] (text-path/glyph-placements line advances {:position 0.0})))
      (is (close? [[80.0 0.0 0.0] [90.0 0.0 0.0]] (text-path/glyph-placements line advances {:position 1.0}))))
    (testing "a path ending left of its start is reversed (read upright), the baseline on the reading side"
      (is (close? [[40.0 0.0 0.0] [50.0 0.0 0.0]] (text-path/glyph-placements (rseq line) advances {})))
      (is (close? [[40.0 5.0 0.0] [50.0 5.0 0.0]] (text-path/glyph-placements (rseq line) advances {:baseline 5.0})))
      (testing "decided by the whole path: a closed path turning back is laid as given"
        (is (close? [[0.0 0.0 0.0] [10.0 0.0 0.0]]
                    (text-path/glyph-placements [[0 0] [50 0] [0 0]] advances {:position 0.0})))))
    (testing "an exactly vertical path reads bottom to top whatever its direction"
      (let [up (text-path/glyph-placements [[0 100] [0 0]] advances {})]
        (is (close? up (text-path/glyph-placements [[0 0] [0 100]] advances {})))
        (is (m/== (- m/HALF_PI) (double (get-in up [0 2]))))))
    (testing "glyphs follow the turns of the path, spaced by arc length along it"
      (let [[[x0 y0 a0] [x1 y1 a1]] (text-path/glyph-placements [[0 0] [10 0] [10 10]] advances {})]
        (is (close? [[0.0 0.0 0.0] [10.0 0.0 m/HALF_PI]] [[x0 y0 a0] [x1 y1 a1]]))))
    (testing "too short a path, too few points, no glyphs - nil; repeated points merged"
      (is (nil? (text-path/glyph-placements [[0 0] [15 0]] advances {})))
      (is (nil? (text-path/glyph-placements [[0 0]] advances {})))
      (is (nil? (text-path/glyph-placements [[0 0] [0 0]] advances {})))
      (is (nil? (text-path/glyph-placements line [0.0] {})))
      (is (close? [[40.0 0.0 0.0] [50.0 0.0 0.0]] (text-path/glyph-placements [[0 0] [0 0] [100 0] [100 0]] advances {}))))))

(def ^:private series (vec (for [g ["up" "down"] t (range 11)] {:t t :v (if (= g "up") t (- 10 t)) :g g})))

(defn- path-plot
  "Gives a line plot of `series` with a text chart of options `options` over it, merged with `extra`."
  ([options] (path-plot options nil))
  ([options extra]
   (merge {:data series :mapping {:x :t :y :v :color :g}
           :charts [(chart/line) (chart/text (merge {:mapping {:x :t :y :v :color :g :label :g}} options))]}
          extra)))

(deftest along-path-test
  (let [plain (pixels (merge (path-plot {}) {:charts [(chart/line)] :legend false}))]
    (testing "data: one text per group along its rows; differs from labels at points"
      (let [along (pixels (path-plot {:along-path true}))]
        (is (not= plain along))
        (is (not= along (pixels (path-plot {}))))
        (is (not= along (pixels (path-plot {:along-path true :path-position 0.2}))))
        (is (not= along (pixels (path-plot {:along-path true :anchor [:center :bottom] :offset [0 3]}))))))
    (testing "a text longer than its path isn't drawn"
      (is (= plain (pixels (path-plot {:along-path true :mapping {:x :t :y :v :color :g :label (constantly (apply str (repeat 80 "w")))}}
                                      {:legend false})))))
    (testing "literal: positions with one text form one path; one position draws nothing"
      (let [base {:data series :mapping {:x :t :y :v :color :g} :charts [(chart/line)]}]
        (is (not= (pixels base)
                  (pixels (update base :charts conj (chart/text {:x [0 5 10] :y [0 8 0] :text "a path" :along-path true})))))
        (is (= (pixels base) (pixels (update base :charts conj (chart/text {:x 5 :y 5 :text "dot" :along-path true})))))))
    (testing "flipped, polar and faceted plots"
      (is (not= (pixels (path-plot {} {:flip true})) (pixels (path-plot {:along-path true} {:flip true}))))
      (is (some? (plot/render-image (path-plot {:along-path true} {:coord {:kind :polar}}) size)))
      (is (some? (plot/render-image (path-plot {:along-path true} {:mapping {:x :t :y :v :color :g :facet :g}}) size))))
    (testing "zero labels and an empty text"
      (is (map? (plot/build (path-plot {:along-path true :mapping {:x :t :y :v :label (constantly "")}}) size)))
      (is (= plain (pixels (path-plot {:along-path true :mapping {:x :t :y :v :label (constantly "")}} {:legend false})))))
    (testing "invalid values and conflicts"
      (is (= :path-position (:key (error #(plot/build (path-plot {:along-path true :path-position 1.5}) size)))))
      (is (= :along-path (:key (error #(plot/build (path-plot {:along-path "yes"}) size)))))
      (doseq [conflict [{:select :last} {:overlapping-labels :drop} {:overlapping-labels :repel}
                        {:anchor-frame :text} {:rotation 30} {:rotation :radial}]]
        (is (some? (error #(plot/build (path-plot (merge {:along-path true} conflict)) size))) (str conflict)))
      (is (map? (plot/build (path-plot {:along-path true :rotation 0 :background :none}) size))))))
