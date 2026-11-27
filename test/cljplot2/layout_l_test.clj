(ns cljplot2.layout-l-test
  "Batch L (round 08) - layout: alignment rounds that alternate keep their least misaligned round, track corrections
  that change nothing rebuild nothing, 3D frame margins per side with a space error for a box without room, default
  axis titles dropped when the plot's fixed parts don't fit."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.content.space3d :as space3d]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.grid :as grid]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error]]))

(defn- panel-sizes
  "Panel `[w h]` of every cell sub-plot of a built arrange."
  [b]
  (keep (fn [cell] (when-let [{:keys [w h]} (grid/find-content-box (:grid cell) (:layout cell) #(and (= :panel (:kind %)) (nil? (:margin %))))]
                     [w h]))
        (vals (:subplots b))))

(defn- spread
  "The largest difference of panel widths and of panel heights."
  [sizes]
  (let [ws (map first sizes) hs (map second sizes)]
    [(- (apply max ws) (apply min ws)) (- (apply max hs) (apply min hs))]))

(def ^:private cells
  "Four scatter cells whose y ranges differ (tick labels of different widths), titled by their columns."
  (vec (for [i (range 4)] {:data [{:x 0 :y (* i 1000)} {:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})))

(defn- arranged [w h] (plot/build (plot/arrange cells {:rows 2 :equal-panels true}) {:width w :height h}))

(deftest alignment-rounds-test
  (testing "alternating rounds (an end tick label at a pixel threshold) end with equal panels"
    (is (= [0 0] (spread (panel-sizes (arranged 400 300)))) "was 1 px")
    (doseq [w [312 354 396 438]]
      (is (= 0 (first (spread (panel-sizes (arranged w 300))))) (str w))))
  (testing "the 19 px case at height 220 (an alignment of 0 or 18 px) stays small; heights 216-221 at most 8 px (P12)"
    (doseq [h (range 216 222)]
      (is (<= (apply max (spread (panel-sizes (arranged 500 h)))) 8) (str h))))
  (testing "no round rebuilt only to be dropped: arrange and padded cell builds counted"
    (let [arranges (atom 0)
          padded (atom 0)
          build pipeline/build]
      (with-redefs [pipeline/build (fn [p & more]
                                     (when (:arrange p) (swap! arranges inc))
                                     (when (:alignment-padding p) (swap! padded inc))
                                     (apply build p more))]
        (arranged 500 220))
      (is (<= @arranges 3) "was 7 (identical equal-panel and gap rebuilds)")
      (is (<= @padded 16) "was 47")))
  (testing "boundaries: one cell, one row, an iso cell, already aligned cells"
    (is (= 1 (count (panel-sizes (plot/build (plot/arrange [(first cells)] {}) {:width 300 :height 200})))))
    (is (= [0 0] (spread (panel-sizes (plot/build (plot/arrange (vec (repeat 3 (first cells))) {:columns 3 :equal-panels true}) {:width 600 :height 200})))))
    (is (some? (plot/render-image (plot/arrange [(second cells) (assoc (first cells) :aspect :iso)] {:columns 2}) {:width 700 :height 250})))))

(deftest unchanged-corrections-test
  (let [corrected #'pipeline/corrected-tracks
        tracks [[:fixed 100] [:fixed 100]]]
    (testing "a spread inside one track: no correction changes a track - nil, nothing rebuilt"
      (is (nil? (corrected tracks {0 [100 101] 1 [100]} 1.0))))
    (testing "equal panels - nil; unequal tracks - corrected"
      (is (nil? (corrected tracks {0 [100] 1 [100]} 1.0)))
      (is (= [[:fixed 102] [:fixed 98]] (corrected tracks {0 [98] 1 [102]} 1.0))))))

(def ^:private long-title (apply str (repeat 30 "w")))

(defn- margins
  "The frame margins of a unit 3D box with `titles`, measured with the default theme."
  [titles & [view]]
  (measure/call-with-scratch-canvas :high
                                    (fn [c] (#'space3d/frame-margins (cond-> {:axes true :domains {:x [0 1] :y [0 1] :z [0 1]} :titles titles}
                                                                       view (assoc :view view))
                                                                     [400 400] {:canvas c :theme theme/default-theme}))))

(deftest frame-margins-test
  (let [plain (margins {})]
    (testing "titles along their edges keep one thickness on the left, right and bottom; the top takes none"
      (is (= {:top 12.0 :right 35.0 :bottom 35.0 :left 35.0} plain))
      (is (= {:top 12.0 :right 57.0 :bottom 57.0 :left 57.0} (margins {:x "x"})))
      (is (= {:top 12.0 :right 57.0 :bottom 57.0 :left 57.0} (margins {:x "x" :y "y" :z "z"}))))
    (testing "turned x / y titles: left and right take the turned width, the bottom its height"
      (is (= {:top 12.0 :right 57.0 :bottom 260.0 :left 57.0} (margins {:x {:text long-title :rotation 90}})))
      (is (= {:top 12.0 :right 260.0 :bottom 57.0 :left 260.0} (margins {:y {:text long-title :rotation 0}})))
      (is (= {:top 12.0 :right 57.0 :bottom 260.0 :left 57.0} (margins {:y {:text long-title :rotation 90}}))))
    (testing "a turned z title takes only the left"
      (let [m (margins {:z {:text long-title :rotation 45}})]
        (is (= 35.0 (:right m) (:bottom m)))
        (is (< 200.0 (:left m) 230.0))))
    (testing "a turned z title only on the left, by its extent across that edge"
      (let [vertical (margins {:z {:text long-title :rotation 90}})
            horizontal (margins {:z {:text long-title :rotation 0}})]
        (is (= (select-keys plain [:right :bottom]) (select-keys vertical [:right :bottom]) (select-keys horizontal [:right :bottom])))
        (is (< (:left vertical) 100) "a vertical title is narrow across a vertical edge")
        (is (> (:left horizontal) 200))))
    (testing "a turned x title: left / right its width, bottom its height"
      (let [m (margins {:x {:text long-title :rotation 0}})]
        (is (> (:left m) 200))
        (is (= (:left m) (:right m)))
        (is (< (:bottom m) 100))))
    (testing "a 45 degree title takes both extents' share; the top never takes a title"
      (let [m (margins {:y {:text long-title :rotation 45}})]
        (is (< (:left plain) (:left m)))
        (is (= (:top plain) (:top m)))))
    (testing "azimuth independent"
      (is (= (margins {:z {:text long-title :rotation 0}} {:azimuth 30}) (margins {:z {:text long-title :rotation 0}} {:azimuth 200}))))))

(def ^:private cube (for [x [0 1] y [0 1] z [0 1]] {:x x :y y :z z}))

(deftest three-d-space-test
  (let [p (fn [labels] {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)] :labels labels})
        space (fn [q size] (select-keys (error #(plot/build q size)) [:key :axis :required :available :cause]))]
    (testing "a box without room is a space error at build, in the plot's pixels"
      (is (= {:key :width :axis :columns :required 338 :available 250 :cause :track-space}
             (space (p {:z {:text long-title :rotation 0}}) {:width 250 :height 250})))
      (is (map? (plot/build (p {:z {:text long-title :rotation 0}}) {:width 338 :height 250})) "the required size fits")
      (is (= {:key :height :axis :rows :required 293 :available 250 :cause :track-space}
             (space (p {:x {:text long-title :rotation 90}}) {:width 250 :height 250}))))
    (testing "the message names the 3D frame and reads as the plot's space errors"
      (is (= "Invalid input: Not enough space: the 3D frame (axes, tick labels, titles) needs 338 px of width, 250 px given"
             (try (plot/build (p {:z {:text long-title :rotation 0}}) {:width 250 :height 250}) "" (catch Exception e (ex-message e))))))
    (testing "not caused by titles: a tiny 3D plot"
      (is (= {:key :width :axis :columns :required 135 :available 90 :cause :track-space} (space (p {}) {:width 90 :height 90}))))
    (testing "facets: the first panel too small"
      (is (= :width (:key (error #(plot/build (assoc (p {:z {:text long-title :rotation 0}})
                                                      :data (map (fn [r] (assoc r :f (if (zero? (:x r)) "a" "b"))) cube)
                                                      :mapping {:x :x :y :y :z :z :facet :f})
                                               {:width 400 :height 250}))))))
    (testing "an arrange cell is named like other sub-plot space errors"
      (let [e (error #(plot/build (plot/arrange [(p {:z {:text long-title :rotation 0}}) {:data [{:a 1 :b 1}] :mapping {:x :a :y :b} :charts [(chart/scatter)]}]
                                                {:columns 2 :equal-panels true})
                                  {:width 600 :height 300}))]
        (is (= [:plot 0] (:subplot e)))
        (is (= :track-space (:cause e)))))
    (testing "a vertical z title renders in the same panel (the U2 empty panel)"
      (is (some? (plot/render-image (p {:z {:text long-title :rotation 90}}) {:width 250 :height 250}))))
    (testing "the box-space check itself: no layers - a bare frame with room; panel pixels as longs"
      (is (nil? (measure/call-with-scratch-canvas :high #(space3d/box-space-problem [] [400 400] {:canvas % :theme theme/default-theme})))))))

(def ^:private scatter-plot
  "A scatter plot whose axes are titled by their columns."
  {:data [{:a 1 :b 2} {:a 2 :b 3}] :mapping {:x :a :y :b} :charts [(chart/scatter)]})

(defn- items [p o] (set (map :id (get-in (plot/build p o) [:grid :items]))))

(defn- titles
  "The axis title items a built plot keeps."
  [p o]
  (set (filter #{:x-title :y-title} (items p o))))

(deftest default-titles-give-way-test
  (testing "a height shortfall drops the x default title only; the plot's :labels stay as given"
    (let [ids (items scatter-plot {:width 100 :height 60})]
      (is (not (contains? ids :x-title)))
      (is (contains? ids :y-title)))
    (is (nil? (:labels (plot/build scatter-plot {:width 100 :height 60})))))
  (testing "a width shortfall drops the y default title only"
    (let [ids (items scatter-plot {:width 70 :height 300})]
      (is (contains? ids :x-title))
      (is (not (contains? ids :y-title)))))
  (testing "both dropped when one isn't enough"
    (let [ids (items scatter-plot {:width 70 :height 60})]
      (is (not-any? ids [:x-title :y-title]))))
  (testing "user titles and suggestions never give way"
    (is (= :height (:key (error #(plot/build (assoc scatter-plot :labels {:x "a"}) {:width 100 :height 60})))))
    (is (= :height (:key (error #(plot/build {:data [{:a 1} {:a 2}] :mapping {:x :a} :charts [(chart/histogram)] :labels {:x "a"}}
                                             {:width 100 :height 60}))))
        "the histogram's suggested y title and a user x title"))
  (testing "a plot too small even without titles throws the original error"
    (let [e (error #(plot/build scatter-plot {:width 30 :height 30}))]
      (is (= :width (:key e)))
      (is (= 85 (:required e)) "the requirement with titles")))
  (testing "titles that fit stay"
    (is (every? (items scatter-plot {:width 400 :height 300}) [:x-title :y-title])))
  (testing "facets and arrange sub-plots give way too"
    (is (= #{:y-title} (titles (assoc scatter-plot :data [{:a 1 :b 2 :f "p"} {:a 2 :b 3 :f "q"}] :mapping {:x :a :y :b :facet :f})
                               {:width 160 :height 70})))
    (let [b (plot/build (plot/arrange [scatter-plot scatter-plot] {:columns 2}) {:width 220 :height 80})]
      (is (= [#{:y-title} #{:y-title}] (mapv (fn [k] (set (filter #{:x-title :y-title} (map :id (get-in b [:subplots k :grid :items])))))
                                            (sort (keys (:subplots b))))))))
  (testing "flip: the title under the panel (the y title) gives way for height, the left one (x) for width"
    (let [p (assoc scatter-plot :flip true)]
      (is (= #{:y-title} (titles p {:width 100 :height 60})) "the x title, now on the left, stays")
      (is (= #{:x-title} (titles p {:width 70 :height 300})))))
  (testing "a suggestion stays while the default gives way"
    (let [hist {:data [{:a 1} {:a 2} {:a 2}] :mapping {:x :a} :charts [(chart/histogram)]}]
      (is (= #{:y-title} (titles hist {:width 100 :height 60})) "the default x title gives way, Count stays")
      (is (= :width (:key (error #(plot/build hist {:width 70 :height 300})))) "Count isn't dropped for width")))
  (testing "one default title: y mapped by a function"
    (is (= #{} (titles (assoc scatter-plot :mapping {:x :a :y (fn [r] (:b r))}) {:width 100 :height 60}))))
  (testing "a label map without :text restyles the default and gives way with it"
    (is (= #{:y-title} (titles (assoc scatter-plot :labels {:x {:color "red"}}) {:width 100 :height 60})))
    (is (= {:x {:color "red"}} (:labels (plot/build (assoc scatter-plot :labels {:x {:color "red"}}) {:width 100 :height 60}))) "labels as given"))
  (testing "a plot without :labels gets none added"
    (is (not (contains? (plot/build scatter-plot {:width 100 :height 60}) :labels)))))
