(ns cljplot2.batch-c2-bl-test
  "Batch C2, builder labels: text `:overlapping-labels` and a mapped `:x-span` (fit to a room, down to a text
  role's `:min-size`), sankey / network / alluvial labels using them, box n labels past the outlier marks."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.core :as plot]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 300 :height 200})

(defn- pixels
  "Gives the pixels of a rendered plot."
  [p]
  (let [^BufferedImage img (plot/render-image p size)
        w (.getWidth img) h (.getHeight img)]
    (vec (.getRGB img 0 0 w h nil 0 w))))

(defn- text-plot
  "Gives a plot of labels at rows `rows` (`:x :y :label`, optional `:room`) with text options `options`."
  ([rows options] (text-plot rows options nil))
  ([rows options extra]
   (merge {:data rows
           :charts [(chart/text (merge {:mapping (cond-> {:x :x :y :y :label :label}
                                                   (some :room rows) (assoc :x-span :room))}
                                       options))]
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
          extra)))

(def ^:private one [{:x 5 :y 5 :label "label"}])
(def ^:private two [{:x 5 :y 5 :label "label"} {:x 5.2 :y 5 :label "other"}])

(deftest overlapping-labels-test
  (testing ":keep (default) draws every label, :drop the first of overlapping ones"
    (is (= (pixels (text-plot two {})) (pixels (text-plot two {:overlapping-labels :keep}))))
    (is (not= (pixels (text-plot one {})) (pixels (text-plot two {}))))
    (is (= (pixels (text-plot one {})) (pixels (text-plot two {:overlapping-labels :drop})))))
  (testing "zero and one label"
    (is (map? (plot/build (text-plot [] {:overlapping-labels :drop}) size)))
    (is (= (pixels (text-plot one {})) (pixels (text-plot one {:overlapping-labels :drop})))))
  (testing "labels apart are all kept"
    (let [apart [{:x 1 :y 1 :label "a"} {:x 8 :y 8 :label "b"}]]
      (is (= (pixels (text-plot apart {})) (pixels (text-plot apart {:overlapping-labels :drop}))))))
  (testing "an empty text never overlaps"
    (is (= (pixels (text-plot one {}))
           (pixels (text-plot [{:x 5 :y 5 :label ""} {:x 5 :y 5 :label "label"}] {:overlapping-labels :drop})))))
  (testing "per panel: facets keep a label in each"
    (let [faceted [{:x 5 :y 5 :label "label" :f "a"} {:x 5 :y 5 :label "label" :f "b"}]]
      (is (= (pixels (text-plot faceted {} {:mapping {:facet :f}}))
             (pixels (text-plot faceted {:overlapping-labels :drop} {:mapping {:facet :f}}))))))
  (testing "flipped and polar plots"
    (is (= (pixels (text-plot one {} {:flip true})) (pixels (text-plot two {:overlapping-labels :drop} {:flip true}))))
    (is (map? (plot/build (text-plot two {:overlapping-labels :drop} {:coord {:kind :polar}}) size))))
  (testing "invalid values"
    (is (= :overlapping-labels (:key (error #(plot/build (text-plot one {:overlapping-labels :hide}) size)))))
    (is (some? (error #(plot/build (text-plot one {:overlapping-labels :drop :anchor-frame :text}) size))))
    (is (map? (plot/build (text-plot one {:overlapping-labels :keep :anchor-frame :text}) size)))))

(def ^:private long-label [{:x 1 :y 5 :label "a rather long label text"}])

(deftest x-span-test
  (testing "a label longer than its room shrinks down to its role's :min-size"
    (let [roomy (pixels (text-plot (mapv #(assoc % :room 9.0) long-label) {}))
          tight-rows (mapv #(assoc % :room 1.0) long-label)
          min-theme {:theme {:text {:annotation {:min-size 5}}}}
          plain-theme {:theme {:text {:plain (dissoc (get-in theme/default-theme [:text :annotation]) :min-size)}}}]
      (is (= (pixels (text-plot long-label {})) roomy) "room enough - unchanged")
      (is (= (pixels (text-plot long-label {:role :plain} plain-theme)) (pixels (text-plot tight-rows {:role :plain} plain-theme)))
          "a role without :min-size doesn't shrink")
      (is (not= (pixels (text-plot long-label {})) (pixels (text-plot tight-rows {})))
          "the :annotation role shrinks by default (:min-size 6)")
      (is (not= (pixels (text-plot long-label {} min-theme)) (pixels (text-plot tight-rows {} min-theme))))
      (is (= (pixels (text-plot tight-rows {} {:theme {:text {:annotation {:min-size 5}}}}))
             (pixels (text-plot long-label {:size 5} min-theme)))
          "a tiny room - the least size")))
  (testing "nil, non-finite rooms and polar plots aren't fitted"
    (let [min-theme {:theme {:text {:annotation {:min-size 5}}}}]
      (doseq [room [nil ##NaN ##Inf]]
        (is (= (pixels (text-plot long-label {} min-theme))
               (pixels (text-plot (mapv #(assoc % :room room) long-label) {:mapping {:x :x :y :y :label :label :x-span :room}} min-theme)))))
      (is (= (pixels (text-plot long-label {} (assoc min-theme :coord {:kind :polar})))
             (pixels (text-plot (mapv #(assoc % :room 1.0) long-label) {} (assoc min-theme :coord {:kind :polar})))))))
  (testing "flipped plots aren't fitted (the text doesn't run along x)"
    (is (= (pixels (text-plot long-label {} {:flip true}))
           (pixels (text-plot (mapv #(assoc % :room 1.0) long-label) {} {:flip true})))))
  (testing "a wrong :min-size type"
    (is (some? (error #(plot/build (text-plot long-label {} {:theme {:text {:annotation {:min-size "big"}}}}) size))))))

(def ^:private flows
  [{:from "a long source name" :to "h" :value 3} {:from "b" :to "h" :value 1} {:from "h" :to "x" :value 2}
   {:from "h" :to "y" :value 2} {:from "b" :to "y" :value 2}])

(defn- text-charts
  "Gives the text charts of a plot."
  [p]
  (filter #(= :text (:kind %)) (:charts p)))

(deftest builder-labels-test
  (testing "sankey: rooms, larger nodes first, :drop"
    (let [texts (text-charts (plot/sankey flows))
          rows (mapcat #(get-in % [:options :data]) texts)]
      (is (= 2 (count texts)))
      (is (every? #(= :drop (get-in % [:options :overlapping-labels])) texts))
      (is (every? #(= :room (get-in % [:options :mapping :x-span])) texts))
      (is (every? #(< 0.0 (:room %) 1.0) rows))
      (is (= "h" (:label (first (get-in (first texts) [:options :data])))) "the largest node first")
      (is (map? (plot/build (plot/sankey flows) size)))
      (is (empty? (text-charts (plot/sankey flows {:labels false}))))))
  (testing "sankey: one flow"
    (is (map? (plot/build (plot/sankey [{:from "a" :to "b" :value 1}]) size))))
  (testing "network: labels of larger nodes first"
    (let [[t] (text-charts (plot/network flows {:labels true :mapping {:size :degree} :rng (r/rng :mersenne 1)}))]
      (is (= :drop (get-in t [:options :overlapping-labels])))
      (is (= "h" (:id (first (get-in t [:options :data])))))))
  (testing "alluvial: larger strata first"
    (let [[t] (text-charts (plot/alluvial [{:a "x" :b "p"} {:a "x" :b "q"} {:a "y" :b "q"}] {:variables [:a :b]}))]
      (is (= :drop (get-in t [:options :overlapping-labels])))
      (is (= "x" (:label (first (get-in t [:options :data]))))))))

(deftest radial-min-size-test
  (testing "radial leaf names shrink down to the role's :min-size"
    (let [many (vec (for [i (range 300)] {:from (str "n" i) :to (str "n" (mod (inc i) 300)) :value 1}))
          sizes (fn [theme] (set (map #(get-in % [:options :size]) (text-charts (plot/chord many {:gap-angle 0.2 :theme theme})))))]
      (is (= #{5.0} (sizes nil)))
      (is (= #{7.0} (sizes {:text {:radial-label {:min-size 7}}})))
      (is (= #{9.0} (sizes {:text {:radial-label {:min-size 12}}})) "never above the role's size"))))

(deftest n-label-gap-test
  (testing "end labels keep their :gap past the value"
    (let [room (fn [labels] (strip-chart/end-label-room labels {:theme theme/default-theme :hint :high}))]
      (is (= 5.0 (- (second (:x (room [{:text "n=1" :side 1.0 :gap 5.0}])))
                    (second (:x (room [{:text "n=1" :side 1.0}]))))))
      (is (= 0.0 (first (:x (room [{:text "n=1" :side 1.0 :gap 5.0}])))) "only on its side")
      (is (nil? (room [])))))
  (testing "a box n label past its top outlier's mark moves; without outliers above it doesn't"
    (let [rows (concat (for [v (range 20)] {:v (double v)}) [{:v 60.0}])
          box (fn [options] (pixels {:data rows :mapping {:x :v} :charts [(chart/box (merge {:n-label true} options))]}))]
      (is (map? (plot/build {:data rows :mapping {:x :v} :charts [(chart/box {:n-label true})]} size)))
      (is (not= (box {}) (box {:outliers false})))
      (is (map? (plot/build {:data rows :mapping {:x :v} :charts [(chart/box {:n-label true :variant :tufte})] :flip true} size))))))
