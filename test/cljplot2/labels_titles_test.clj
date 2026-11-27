(ns cljplot2.labels-titles-test
  "Titles of every mapped dimension (spec B, vault notes 07): plot `:labels` keys `:z` and the aesthetics, `:none`
  hiding a title and `nil` keeping the default (labels and scale titles), the rules for labels nothing draws, the
  legend title order (`:labels` > scale `:title` > computed > accessor name), label maps on legend and 3D frame
  titles, and 3D frame titles from `:labels` (the charts' `:axis-titles` removed, no outer titles on 3D plots)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.content.space3d :as space-draw]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.test-util :refer [error-data error-ins pixels]]))

(def ^:private rows (vec (for [i (range 24)] {:x (mod i 6) :y (mod i 4) :z i :g (if (even? i) "a" "b") :s (inc i)})))

(defn- built [p] (plot/build p {:width 400 :height 300}))

(defn- item-ids [b] (set (map :id (get-in b [:grid :items]))))

(defn- item-text [b id] (some #(when (= id (:id %)) (get-in % [:content :text])) (get-in b [:grid :items])))

(defn- legend-titles [p] (mapv :title (legend/legends (built p))))

(defn- image [p] (vec (pixels (plot/render-image p {:width 400 :height 300}))))

(defn- frame-titles [p] (get-in (built p) [:stats 0 :data :panel :frame :titles]))

(def ^:private scatter-plot {:data rows :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]})

(def ^:private cloud-plot {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})

(deftest label-keys-test
  (testing "the keys of :labels, in item order"
    (is (= [:title :x :y :z :color :shape :size :dash :pattern] plot-grid/label-keys)))
  (testing "every aesthetic key validates on a plot mapping it"
    (is (= ["kind" "n"] (legend-titles {:data rows :mapping {:x :x :y :y :shape :g :size :s} :charts [(chart/scatter)] :labels {:shape "kind" :size "n"}})))
    (is (built {:data rows :mapping {:x :x :y :y :dash :g} :charts [(chart/line)] :labels {:dash "series"}}))
    (is (built {:data rows :mapping {:x :g :y :z :pattern :g} :charts [(chart/bar)] :labels {:pattern "group"}})))
  (testing "label contents leave out nil and :none, and have no item for :z or legend titles"
    (is (= [:title] (map first (plot-grid/label-contents {:title "T" :x :none :y nil}))))
    (is (= #{:panel :x-axis :x-axis-start :x-axis-end :y-axis :y-axis-start :y-axis-end :title :legends}
           (item-ids (built (assoc scatter-plot :labels {:title "T" :color "C" :z nil :x :none :y :none :shape nil})))
           (disj (item-ids (built (assoc scatter-plot :labels {:title "T" :x :none :y "Y"}))) :y-title)))
    (is (contains? (item-ids (built (assoc scatter-plot :labels {:y "Y"}))) :y-title)))
  (testing "an empty :labels map changes nothing"
    (is (= (image scatter-plot) (image (assoc scatter-plot :labels {}))))))

(deftest none-and-nil-test
  (let [hist {:data rows :mapping {:x :x} :charts [(chart/histogram {:y :count})]}]
    (testing "a suggested axis title: nil keeps it, :none hides it, a text replaces it"
      (is (= "Count" (item-text (built hist) :y-title)))
      (is (= "Count" (item-text (built (assoc hist :labels {:y nil})) :y-title)))
      (is (nil? (item-text (built (assoc hist :labels {:y :none})) :y-title)))
      (is (= "n" (item-text (built (assoc hist :labels {:y "n"})) :y-title))))
    (testing ":none never prints as a text"
      (is (not-any? #(= :none (get-in % [:content :text])) (get-in (built (assoc hist :labels {:title :none :x :none :y :none})) [:grid :items])))
      (is (not (contains? (item-ids (built (assoc hist :labels {:title :none}))) :title))))
    (testing "a custom grid with :title :none has no title row"
      (let [grid {:columns [{:id :c :size [:flexible]}] :rows [{:id :r :size [:flexible]}] :items []}]
        (is (= (:grid (built {:custom-grid grid})) (:grid (built {:custom-grid grid :labels {:title :none}}))))))
    (testing ":none in the labels doesn't change the estimated panel size; a text does"
      (let [theme (:theme (built hist))
            est (fn [l] (plot-grid/estimated-panel-size {:options {:width 400 :height 300} :theme theme :labels l} [] {} {:columns 1 :rows 1} false false))]
        (is (= (est nil) (est {:title :none :x :none})))
        (is (not= (est nil) (est {:title "none" :x "none"})))))
    (testing ":none in an arranged sub-plot hides its suggested title"
      (is (not= (image {:arrange {:plots [(assoc hist :labels {:y :none})]}}) (image {:arrange {:plots [hist]}}))))))

(deftest label-rules-test
  (let [message (fn [p] (:message (first (:errors (error-data #(built p))))))]
    (testing "a legend title for an aesthetic nothing maps or computes"
      (is (= #{[:labels :shape]} (error-ins #(built (assoc scatter-plot :labels {:shape "S"})))))
      (is (str/includes? (message (assoc scatter-plot :labels {:shape "S"})) "legend title needs")))
    (testing ":z without a 3D chart"
      (is (= #{[:labels :z]} (error-ins #(built (assoc scatter-plot :labels {:z "Z"})))))
      (is (str/includes? (message (assoc scatter-plot :labels {:z "Z"})) ":z titles a 3D frame"))))
  (testing ":none or nil for a title nothing draws hides nothing and passes"
    (is (built (assoc scatter-plot :labels {:shape :none :z :none :size nil}))))
  (testing "formulas of the new label keys are checked"
    (let [bad "$\\undefinedcommand$"]
      (is (= #{[:labels :color]} (error-ins #(built (assoc scatter-plot :labels {:color bad})))))
      (is (= #{[:labels :z]} (error-ins #(built (assoc cloud-plot :labels {:z bad})))))))
  (testing "a mapped aesthetic under :legend false, and a computed one, take a title"
    (is (built (assoc scatter-plot :legend false :labels {:color "C"})))
    (is (= ["n"] (legend-titles {:data rows :mapping {:x :x :y :y} :charts [(chart/bin2d)] :labels {:color "n"}}))))
  (testing "a legend-only plot takes legend titles; an arrangement only a title"
    (is (= ["C"] (legend-titles (assoc scatter-plot :legend-only true :labels {:color "C"}))))
    (is (= [nil] (legend-titles (assoc scatter-plot :legend-only true :labels {:color :none}))))
    (is (= #{[:labels :x]} (error-ins #(built {:arrange {:plots [scatter-plot]} :labels {:x "x"}}))))))

(deftest legend-title-order-test
  (testing ":labels > scale :title > computed title > accessor name"
    (is (= ["L"] (legend-titles (assoc scatter-plot :scales {:color {:title "S"}} :labels {:color "L"}))))
    (is (= ["S"] (legend-titles (assoc scatter-plot :scales {:color {:title "S"}}))))
    (is (= ["g"] (legend-titles scatter-plot)))
    (is (= ["L"] (legend-titles {:data rows :mapping {:x :x :y :y} :charts [(chart/bin2d)] :scales {:color {:title nil}} :labels {:color "L"}})))
    (is (= ["S"] (legend-titles {:data rows :mapping {:x :x :y :y} :charts [(chart/bin2d)] :scales {:color {:title "S"}}}))))
  (testing "nil passes to the next level, :none hides"
    (is (= ["S"] (legend-titles (assoc scatter-plot :scales {:color {:title "S"}} :labels {:color nil}))))
    (is (= [nil] (legend-titles (assoc scatter-plot :scales {:color {:title "S"}} :labels {:color :none}))))
    (is (= ["g"] (legend-titles (assoc scatter-plot :scales {:color {:title nil}}))))
    (is (= [nil] (legend-titles (assoc scatter-plot :scales {:color {:title :none}})))))
  (testing "a colour bar takes its title from :labels"
    (is (= ["Z"] (legend-titles {:data rows :mapping {:x :x :y :y :color :z} :charts [(chart/scatter)] :labels {:color "Z"}}))))
  (testing "merged legends take the first member's title; none only when every titled member hides it"
    (let [merged {:data rows :mapping {:x :x :y :y :color :g :shape :g} :charts [(chart/scatter)]}]
      (is (= ["S"] (legend-titles (assoc merged :labels {:shape "S"}))))
      (is (= ["S"] (legend-titles (assoc merged :labels {:color :none :shape "S"}))))
      (is (= ["C"] (legend-titles (assoc merged :labels {:color "C" :shape :none}))))
      (is (= [nil] (legend-titles (assoc merged :labels {:color :none :shape :none}))))))
  (testing "provisional legends (the panel size estimate) take the same titles"
    (let [prov (fn [p] (let [b (built p)] (mapv :title (legend/provisional-legends b (get-in b [:split :charts])))))]
      (is (= ["LONG"] (prov (assoc scatter-plot :labels {:color "LONG"}))))
      (is (= [nil] (prov (assoc scatter-plot :labels {:color :none}))))
      (is (= ["g"] (prov scatter-plot))))))

(deftest legend-title-label-map-test
  (let [p (fn [title] (assoc scatter-plot :labels {:color title}))]
    (testing "a label map is the legend's title"
      (is (= [{:text "C" :role :title}] (legend-titles (p {:text "C" :role :title})))))
    (testing "it draws in its own role, differently from the text alone; a map of the text alone draws as the text"
      (is (not= (image (p "C")) (image (p {:text "C" :role :title}))))
      (is (= (image (p "C")) (image (p {:text "C"})))))
    (testing "a colour bar draws a label map title"
      (let [bar (fn [t] {:data rows :mapping {:x :x :y :y :color :z} :charts [(chart/scatter)] :labels {:color t}})]
        (is (not= (image (bar "Z")) (image (bar {:text "Z" :size 20}))))
        (is (= (image (bar "Z")) (image (bar {:text "Z"}))))))))

(deftest frame-titles-test
  (testing "the column names by default; a label replaces its axis's, :none removes it"
    (is (= {:x "x" :y "y" :z "z"} (frame-titles cloud-plot)))
    (is (= {:x "east" :y "y"} (frame-titles (assoc cloud-plot :labels {:x "east" :z :none}))))
    (is (= {:x "x" :y "y" :z "z"} (frame-titles (assoc cloud-plot :labels {:x nil})))))
  (testing "a chart's suggestion below the labels"
    (let [binned {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud {:type :bars :bins [3 2]})]}]
      (is (= "Count" (:z (frame-titles binned))))
      (is (= "n" (:z (frame-titles (assoc binned :labels {:z "n"})))))))
  (testing "the first 3D chart's suggestion titles the scene"
    (let [d {:x [0 5] :y [0 3] :z [0 30]}
          scene-titles (fn [charts] (:titles (space-draw/scene-frame (map #(get-in % [:data :panel])
                                                                           (:stats (built {:data rows :mapping {:x :x :y :y :z :z} :charts charts}))))))]
      (is (= "z" (:z (scene-titles [(chart/cloud {:domain d}) (chart/surface {:density true :domain d})]))))
      (is (= "Density" (:z (scene-titles [(chart/surface {:density true :domain d}) (chart/cloud {:domain d})]))))))
  (testing "facets share the settled titles"
    (let [b (built (assoc cloud-plot :mapping {:x :x :y :y :z :z :facet :g} :labels {:x "east"}))]
      (is (every? #(= "east" (get-in % [:frame :titles :x])) (vals (get-in b [:stats 0 :data]))))))
  (testing ":axis-titles is no chart option any more"
    (is (= #{[:charts 0 :options :axis-titles]} (error-ins #(built {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:axis-titles {:x "a"}})]}))))))

(deftest no-outer-titles-on-3d-test
  (testing "a 3D plot's :labels :x / :y title the frame, not the outer grid"
    (let [b (built (assoc cloud-plot :labels {:x "a" :y "b" :title "T"}))]
      (is (= #{:panel :title} (item-ids b)))
      (is (= "a" (get-in b [:stats 0 :data :panel :frame :titles :x]))))))

(deftest frame-title-label-map-test
  (testing "a label map titles the frame in its own style; a map of the text alone draws as the text"
    (is (= {:text "a" :size 24} (:x (frame-titles (assoc cloud-plot :labels {:x {:text "a" :size 24}})))))
    (is (not= (image (assoc cloud-plot :labels {:x "a"})) (image (assoc cloud-plot :labels {:x {:text "a" :size 24}}))))
    (is (= (image (assoc cloud-plot :labels {:x "abc"})) (image (assoc cloud-plot :labels {:x {:text "abc"}})))))
  (testing "a map's :rotation turns the title"
    (is (not= (image (assoc cloud-plot :labels {:x {:text "abc"}})) (image (assoc cloud-plot :labels {:x {:text "abc" :rotation 0}})))))
  (let [b (built cloud-plot)
        computed (get-in b [:stats 0 :data :panel])
        frame (assoc (:frame computed) :domains (:domains computed))
        margin (fn [t] (measure/call-with-scratch-canvas :high #(space-draw/frame-margin (assoc frame :titles {:z t}) [300 300] {:canvas % :theme (:theme b)})))]
    (testing "the frame margin grows with a larger title"
      (is (< (margin "a") (margin {:text "a" :size 40}))))
    (testing "a title turned across its edge takes its turned length, not its text height"
      (is (< (margin "a long z title") (margin {:text "a long z title" :rotation 90}))))))
