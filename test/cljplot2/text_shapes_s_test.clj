(ns cljplot2.text-shapes-s-test
  "Batch S (round 08) - text shapes: a string is short for `{:text s}` wherever a point shape is accepted (chart
  options, scale ranges, identity scales, theme palettes); a character stays a named shape."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.test-util :refer [error error-data same-pixels?]]))

(def ^:private size {:width 200 :height 160})

(def ^:private rows
  "Three points in two groups."
  [{:x 1 :y 1 :g "a" :s "E"} {:x 2 :y 2 :g "b" :s "J"} {:x 3 :y 1 :g "a" :s "E"}])

(defn- scatter
  "A plot of `rows` drawn by a scatter chart of `options`, `extra` merged into the plot."
  [options & [extra]]
  (merge {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter (merge {:size 14 :color :black} options))]} extra))

(deftest string-shape-test
  (testing "a non-empty string is a shape; an empty one isn't, with its own message"
    (doseq [s ["E" "AB" "🐄" " " "$x$"]]
      (is (shape/shape? s) (pr-str s)))
    (is (= "a string shape should be non-empty" (shape/shape-problem ""))))
  (testing "a character stays a named shape: the triangle \\A, not the letter; an unknown character is rejected"
    (is (shape/shape? \A))
    (is (not (shape/shape? \E)))
    (is (str/includes? (shape/shape-problem \E) "a non-empty string"))
    (is (not (same-pixels? (scatter {:shape \A}) (scatter {:shape "A"}) size))))
  (testing "a string draws as its text shape map, pixel for pixel"
    (doseq [s ["E" "AB" "🐄" "$x$"]]
      (is (same-pixels? (scatter {:shape s}) (scatter {:shape {:text s}}) size) (pr-str s)))
    (is (not (same-pixels? (scatter {:shape "$x$"}) (scatter {:shape "x"}) size)) "dollars drawn literally, not a formula"))
  (testing "a string of spaces draws nothing; the chart still renders"
    (is (same-pixels? (scatter {:shape " "}) (scatter {:color :none}) size)))
  (testing "an empty string is an option error at its path"
    (is (= {:key :shape :in [:charts 0 :options :shape]}
           (select-keys (error #(plot/build (scatter {:shape ""}))) [:key :in])))
    (is (= "a string shape should be non-empty" (-> (error-data #(plot/build (scatter {:shape ""}))) :errors first :message)))))

(deftest string-shape-sites-test
  (let [mapped (fn [extra] (merge {:data rows :mapping {:x :x :y :y :shape :g} :charts [(chart/scatter {:size 14})]} extra))]
    (testing "scale ranges: a vector and a level map take strings, as text maps"
      (is (same-pixels? (mapped {:scales {:shape {:range ["E" "J"]}}})
                        (mapped {:scales {:shape {:range [{:text "E"} {:text "J"}]}}}) size))
      (is (same-pixels? (mapped {:scales {:shape {:range {"a" "E" "b" "J"}}}})
                        (mapped {:scales {:shape {:range {"a" {:text "E"} "b" {:text "J"}}}}}) size))
      (is (= [:scales :shape :range 1] (:in (error #(plot/build (mapped {:scales {:shape {:range ["E" ""]}}}))))) "an empty string in a range"))
    (testing "the theme's shape palette takes strings"
      (is (same-pixels? (mapped {:theme {:palette {:shape ["E" "J"]}}})
                        (mapped {:theme {:palette {:shape [{:text "E"} {:text "J"}]}}}) size)))
    (testing "an identity scale draws a string column; an empty string there is a mapping error"
      (let [p {:data rows :mapping {:x :x :y :y :shape :s} :scales {:shape {:kind :identity}} :charts [(chart/scatter {:size 14})]}]
        (is (same-pixels? p (assoc p :data (map #(update % :s (fn [s] {:text s})) rows)) size))
        (is (= {:key :mapping :in [:mapping :shape]} (select-keys (error #(plot/render-image (assoc-in p [:data 0 :s] "") size)) [:key :in])))))
    (testing "a merged legend shows the letters as keys (shape and colour by one column)"
      (let [p (assoc (mapped {:scales {:shape {:range {"a" "E" "b" "J"}}}}) :mapping {:x :x :y :y :shape :g :color :g})
            legends (->> (get-in (plot/build p size) [:grid :items])
                         (filter #(= :legends (get-in % [:content :kind])))
                         (mapcat #(get-in % [:content :legends])))]
        (is (= 1 (count legends)) "one merged legend")
        (is (= [{:chart 0 :dims [:color :shape]}] (:charts (first legends))))
        (is (= ["E" "J"] (map #(get-in % [:attributes :shape]) (:entries (first legends)))))
        (is (same-pixels? p (assoc-in p [:scales :shape :range] {"a" {:text "E"} "b" {:text "J"}}) size) "keys drawn as text maps")
        (is (not (same-pixels? p (assoc-in p [:scales :shape :range] {"a" \O "b" \s}) size)) "keys are letters, not other shapes")))
))

(deftest string-shape-charts-test
  (let [data (concat [{:x 2.5 :y 40 :z 1 :g "a"} {:x 2 :y 2 :z 2 :g "b"} {:x 2.2 :y 1.2 :z 2 :g "b"}]
                     (for [i (range 8)] {:x i :y (+ 1 (* 0.1 i)) :z i :g "a"}))
        ;; a string draws as its text map and unlike the chart's default shape
        same (fn [mk mapping]
               (let [p (fn [s] {:data data :mapping mapping :charts [(mk (cond-> {:size 14} s (assoc :shape s)))]})]
                 (and (same-pixels? (p "E") (p {:text "E"}) size) (not (same-pixels? (p "E") (p nil) size)))))]
    (doseq [[k mk mapping] [[:stripplot #(chart/stripplot (assoc % :jitter 0)) {:x :g :y :y}]
                            [:beeswarm chart/beeswarm {:x :g :y :y}] [:dotplot chart/dotplot {:x :y}]
                            [:box chart/box {:x :g :y :y}] [:extent chart/extent {:x :g :y :y}]
                            [:cloud chart/cloud {:x :x :y :y :z :z}]
                            [:qq #(chart/qq (assoc % :x-distribution [:normal])) {:y :y}]
                            [:pp #(chart/pp (assoc % :x-distribution [:normal])) {:y :y}]]]
      (is (same mk mapping) (str k)))
    (testing "a theme chart style takes a string shape"
      (let [p (fn [s] {:data data :mapping {:x :x :y :y} :theme {:chart {:scatter {:shape s :size 14}}} :charts [(chart/scatter)]})]
        (is (same-pixels? (p "E") (p {:text "E"}) size))))
    (testing "a builder's node shapes take strings"
      (let [star [{:from "hub" :to "a"} {:from "hub" :to "b"} {:from "hub" :to "c"} {:from "a" :to "b"}]
            net (fn [rng] (plot/network star {:rng (r/rng :mersenne 1) :mapping {:shape :id} :scales {:shape {:range rng}}}))
            s {:width 300 :height 300}]
        (is (same-pixels? (net ["E" "J" "U" "H"]) (net (mapv #(hash-map :text %) ["E" "J" "U" "H"])) s))
        (is (not (same-pixels? (net ["E" "J" "U" "H"]) (net [\O \s \A \V]) s)))))))
