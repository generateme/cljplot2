(ns cljplot2.batch-c3-fix-z3-test
  "Batch C3 review fixes (fork z3): plot margin and invalid-value band overhangs - a margin magnitude axis's start
  label kept a gap from the panel axis's labels (single panel and facets), every facet panel axis's start and end
  overhangs in its column's / row's inner tracks, one start overhang per axis (no double reservation with bands),
  margin and band axes at the panels carrying the main axes."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def ^:private opts {:width 700 :height 400})

(def ^:private pts
  "Points over x [0 0.2], y [0 1] in two facets (`:f`) and four (`:g`)."
  (vec (for [f [1 2] i (range 3) g [0 1]] {:x (* 0.1 i) :y (* 0.5 i) :f f :g (+ g (* 2 (dec f)))})))

(defn- built [p & [o]] (plot/build p (merge opts o)))
(defn- ids-of [tracks] (mapv :id tracks))
(defn- index-of [ids id] (.indexOf ^java.util.List ids id))
(defn- track [b axis id] (some #(when (= id (:id %)) %) (get-in b [:layout axis])))
(defn- size [b axis id] (:size (track b axis id)))
(defn- grid-items [b pred] (filter pred (:items (:grid b))))
(defn- items-of [b id-pred] (grid-items b #(id-pred (:id %))))
(defn- id-head? [k] (fn [id] (and (vector? id) (= k (first id)))))
(defn- content-is [b m] (grid-items b #(= m (select-keys (:content %) (keys m)))))

(defn- scatter
  "A scatter of `pts` with plot keys `extra`."
  [extra]
  (merge {:data pts :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:expand [0 0]}}} extra))

;; P8 - facets: the panel axis's end label keeps clear of the right margin's magnitude axis

(deftest facet-right-margin-test
  (let [b (built (scatter {:mapping {:x :x :y :y :facet :f}
                           :margins {:right {:charts (chart/histogram {:bins 3}) :axis true :size 30}}}))
        cols (ids-of (get-in b [:grid :columns]))]
    (testing "every panel column: panel | inner (x end overhang) | gap | margin axis start | margin | overhang"
      (doseq [c [0 1]]
        (is (< (index-of cols [:panel c]) (index-of cols [:right-margin-inner c]) (index-of cols [:right-margin-gap c])
               (index-of cols [:right-margin-axis-start c]) (index-of cols [:right-margin c])
               (index-of cols [:right-margin-overhang c])))
        (is (pos? (long (size b :columns [:right-margin-inner c]))) "the x end label \"0.2\" sticks out")
        (is (pos? (long (size b :columns [:right-margin-axis-start c]))) "the margin axis's \"0\" sticks out")))
    (testing "an end overhang item per axis, in its column's inner track; none in :x-overhang"
      (is (= #{[:right-margin-inner 0] [:right-margin-inner 1]}
             (set (map :column (content-is b {:kind :axis-overhang :scale :x :end :end})))))
      (is (zero? (long (size b :columns :x-overhang)))))
    (testing "the x end label box and the margin axis's start label box are a gap apart"
      (let [inner (track b :columns [:right-margin-inner 0])
            start (track b :columns [:right-margin-axis-start 0])]
        (is (= 5 (- (long (:offset start)) (+ (long (:offset inner)) (long (:size inner)))))))))
  (testing "single panel: the x end overhang stays in :x-overhang, the margin start label in its own track"
    (let [b (built (scatter {:margins {:right {:charts (chart/histogram {:bins 3}) :axis true :size 30}}}))
          cols (ids-of (get-in b [:grid :columns]))]
      (is (< (index-of cols :x-overhang) (index-of cols :right-margin-gap) (index-of cols :right-margin-axis-start)
             (index-of cols :right-margin)))
      (is (not-any? #{:right-margin-inner} cols)))))

(deftest facet-free-and-top-margin-test
  (testing "free x: an end overhang per panel in its column's inner track and its own axis row"
    (let [b (built (scatter {:mapping {:x :x :y :y :facet :g} :facets {:scales {:x :free} :columns 2}
                             :margins {:right {:charts (chart/histogram {:bins 3}) :axis true :size 30}}})
                   {:height 500})]
      (is (= #{[[:right-margin-inner 0] [:x-axis 0]] [[:right-margin-inner 1] [:x-axis 0]]
               [[:right-margin-inner 0] [:x-axis 1]] [[:right-margin-inner 1] [:x-axis 1]]}
             (set (map (juxt :column :row) (content-is b {:kind :axis-overhang :scale :x :end :end})))))))
  (testing "top margins: every panel row's y end overhang in its [:top-margin-inner r], between margin and panel"
    (let [b (built (scatter {:mapping {:x :x :y :y :facet :g} :facets {:columns 2}
                             :margins {:top {:charts (chart/density) :axis true :size 40}}})
                   {:height 500})
          rows (ids-of (get-in b [:grid :rows]))]
      (doseq [r [0 1]]
        (is (< (index-of rows [:top-margin r]) (index-of rows [:top-margin-axis-start r]) (index-of rows [:top-margin-gap r])
               (index-of rows [:top-margin-inner r]) (index-of rows [:panel r]))))
      (is (= #{[:top-margin-inner 0] [:top-margin-inner 1]}
             (set (map :row (remove (comp :margin :content) (content-is b {:kind :axis-overhang :scale :y :end :end})))))))))

;; pre-existing: a left margin's magnitude label "0.0" ran into the x axis's first label "-4"

(deftest left-margin-start-label-test
  (let [p (fn [extra] (merge {:data (vec (for [i (range 40)] {:x (- (mod (* i 7) 9) 4.0) :y (mod (* i 3) 7)}))
                              :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:domain [-4 4] :expand [0 0]}}}
                             extra))]
    (testing "single panel: margin | margin axis start | gap | inner (x start) | panel, the labels a gap apart"
      (let [b (built (p {:margins {:left {:charts (chart/density) :axis true}}}))
            cols (ids-of (get-in b [:grid :columns]))
            start (track b :columns :left-margin-axis-start)
            inner (track b :columns :left-margin-inner)]
        (is (< (index-of cols :left-margin) (index-of cols :left-margin-axis-start) (index-of cols :left-margin-gap)
               (index-of cols :left-margin-inner) (index-of cols :panel)))
        (is (pos? (long (:size start))) "half of \"0.0\"")
        (is (pos? (long (:size inner))) "half of \"-4\"")
        (is (= 5 (- (long (:offset inner)) (+ (long (:offset start)) (long (:size start))))))
        (is (= [:left-margin-axis-start :x-axis]
               ((juxt :column :row) (first (content-is b {:kind :axis-overhang :margin :left :end :start})))))))
    (testing "no axis: no start track (margin grids without axes unchanged)"
      (doseq [side [:top :right :bottom :left]]
        (let [b (built (p {:margins {side (chart/density)}}))
              ids (into (ids-of (get-in b [:grid :columns])) (ids-of (get-in b [:grid :rows])))]
          (is (not-any? #{(keyword (str (name side) "-margin-axis-start"))} ids)))))
    (testing "bottom margin: the margin axis's start label row between the gap and the margin"
      (let [b (built (p {:margins {:bottom {:charts (chart/histogram {:bins 4}) :axis true}}}))
            rows (ids-of (get-in b [:grid :rows]))]
        (is (< (index-of rows :panel) (index-of rows :bottom-margin-inner) (index-of rows :bottom-margin-gap)
               (index-of rows :bottom-margin-axis-start) (index-of rows :bottom-margin)))
        (is (pos? (long (size b :rows :bottom-margin-axis-start))))))
    (testing "facets: an x start overhang per panel column in its [:left-margin-inner c]"
      (let [b (built (p {:data (vec (for [f [0 1] i (range 20)] {:x (- (mod (* i 7) 9) 4.0) :y (mod (* i 3) 7) :f f}))
                         :mapping {:x :x :y :y :facet :f}
                         :margins {:left {:charts (chart/density) :axis true :size 40}}}))]
        (is (= #{[:left-margin-inner 0] [:left-margin-inner 1]}
               (set (map :column (content-is b {:kind :axis-overhang :scale :x :end :start})))))
        (is (pos? (long (size b :columns [:left-margin-inner 1]))) "the interior column's \"-4\" is reserved")))
    (testing "renders, also flipped"
      (doseq [flip [false true]]
        (is (some? (plot/render-image (p {:flip flip :margins {:left {:charts (chart/density) :axis true}
                                                               :bottom {:charts (chart/density) :axis true}}})
                                      opts)))))))

;; D13 - one answer to which panel carries an axis

(deftest axis-panels-test
  (testing "3 panels in 2 columns: the margins' and the bands' axes stand at the panels drawing the main axes"
    (let [data (vec (concat (for [f ["a" "b" "c"] i (range 12)] {:x (* 1.0 (mod (* i 5) 11)) :y (* 1.0 i) :f f})
                            [{:x nil :y 1.0 :f "a"}]))
          b (built {:data data :mapping {:x :x :y :y :facet :f} :facets {:columns 2}
                    :charts [(chart/histogram {:invalid :show :bins 4})]
                    :margins {:right {:charts (chart/histogram {:bins 3}) :axis true :size 30}
                              :top {:charts (chart/density) :axis true :size 30}}}
                   {:height 500})
          key-of (into {} (map (fn [{[_ r c] :id :as item}] [[r c] (get-in item [:content :panel])]))
                       (items-of b (id-head? :panel)))
          main-axes (fn [k] (set (map (comp :panel :content) (remove (comp :margin :content) (content-is b {:kind :axis :scale k})))))
          ;; margin axis items are `[id row col]` (one magnitude scale per side, no panel key)
          margin-axes (fn [id] (set (map (fn [{[_ r c] :id}] (key-of [r c])) (items-of b (id-head? id)))))]
      (is (= #{"b" "c"} (main-axes :x)) "the bottom panel of each column - b in the shorter column")
      (is (= #{"a" "c"} (main-axes :y)) "the first panel of each row")
      (is (= (main-axes :x) (margin-axes :right-margin-axis)))
      (is (= (main-axes :y) (margin-axes :top-margin-axis)))
      (is (= (main-axes :x) (set (map (comp :panel :content) (content-is b {:kind :invalid-band-axis}))))))))

;; D15 - one start overhang per axis beside a band

(deftest facet-band-start-test
  (let [data (vec (concat (for [f [0 1] i (range 30)] {:x (* 1000000.0 (- (mod (* i 7) 21) 10)) :y (* 1.0 i) :f f})
                          [{:x nil :y 1.0 :f 0} {:x nil :y 2.0 :f 1}]))
        ;; a scatter maps :y (a left margin needs it), the histogram doesn't
        p {:data data :mapping {:x :x :y :y :facet :f} :scales {:x {:expand [0 0]}}
           :charts [(chart/scatter) (chart/histogram {:invalid :show :bins 5 :mapping {:y nil}})]}]
    (testing "horizontal band: the x start overhang only in [:invalid-band-inner c], not in the y-axis column"
      (let [b (built p)
            starts (content-is b {:kind :axis-overhang :scale :x :end :start})]
        (is (= #{[:invalid-band-inner 0] [:invalid-band-inner 1]} (set (map :column starts))))
        (is (empty? (items-of b (id-head? :invalid-band-start))))
        (is (< (long (size b :columns :y-axis)) (long (size b :columns [:invalid-band-inner 0])))
            "the y-axis column is as wide as its own labels, narrower than the wide first x label's overhang")))
    (testing "band and left margin: the margin's inner track takes the start overhang, the band's holds nothing"
      (let [b (built (assoc p :margins {:left (chart/density)}))]
        (is (= #{[:left-margin-inner 0] [:left-margin-inner 1]}
               (set (map :column (content-is b {:kind :axis-overhang :scale :x :end :start})))))
        (is (zero? (long (size b :columns [:invalid-band-inner 0]))))))
    (testing "vertical band (flipped): the vertical start overhang per row in [:invalid-band-inner r]"
      (let [b (built (assoc p :flip true :facets {:columns 1}) {:height 500})]
        (is (= #{[:invalid-band-inner 0] [:invalid-band-inner 1]}
               (set (map :row (content-is b {:kind :axis-overhang :scale :x :end :start})))))))
    (testing "no bands, no margins: the outer overhang items as before"
      (let [b (built (scatter {:mapping {:x :x :y :y :facet :f}}))]
        (is (= [:y-axis :x-overhang]
               (map #(:column (first (content-is b {:kind :axis-overhang :scale :x :end %}))) [:start :end])))
        (is (= 1 (count (content-is b {:kind :axis-overhang :scale :x :end :start}))))))))
