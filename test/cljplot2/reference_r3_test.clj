(ns cljplot2.reference-r3-test
  "Reference part R3: `plot/save`, spelling suggestions for transposed keys, table sections (`:cljplot2/section`) and
  the notebook options tables (`notebook-utils/options-table`)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [malli.core :as malli]
            [notebook-utils :as nu]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.schema-metadata-test :refer [non-chart-schemas]]
            [cljplot2.test-util :refer [error-data]])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private small-plot {:data [{:x 1 :y 1} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})

(defn- temp-path
  "A path `name` in a fresh temporary directory (deleted on exit)."
  [name]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "cljplot2-save" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    (str dir "/" name)))

(deftest save-test
  (let [image (plot/render-image small-plot {:width 120 :height 80})]
    (testing "an image: written in the extension's format, the image returned (composes with ->)"
      (doseq [ext ["png" "jpg" "bmp"]]
        (let [path (temp-path (str "plot." ext))]
          (is (identical? image (plot/save image path)) ext)
          (let [read (ImageIO/read (io/file path))]
            (is (= [120 80] [(.getWidth read) (.getHeight read)]) ext)))))
    (testing "a canvas of render: written, the canvas returned"
      (let [canvas (plot/render small-plot {:width 100 :height 60})
            path (temp-path "canvas.png")]
        (is (identical? canvas (plot/save canvas path)))
        (is (= 100 (.getWidth (ImageIO/read (io/file path)))))))
    (testing "boundary: missing parent directories are created"
      (let [path (str (temp-path "a") "/b/c/plot.png")]
        (plot/save image path)
        (is (.exists (io/file path)))))
    (testing "boundary: an extension without an image writer - nothing written, the image returned"
      (let [path (temp-path "plot.nope")]
        (is (identical? image (plot/save image path)))
        (is (not (.exists (io/file path))))))))

(deftest transposed-key-test
  (let [s [:map {:closed true} [:size :int] [:color :int] [:id :int]]
        message #(:message (first (:errors (error-data (fn [] (schema/check-options! s %))))))]
    (testing "two adjacent characters swapped: suggested (malli's similarity misses short keys)"
      (is (= "unknown key, should be spelled :size" (message {:szie 1})))
      (is (= "unknown key, should be spelled :size" (message {:isze 1})) "at the start")
      (is (= "unknown key, should be spelled :size" (message {:siez 1})) "at the end"))
    (testing "boundary: two letters; two swaps or a non-adjacent swap - no suggestion"
      (is (= "unknown key, should be spelled :id" (message {:di 1})))
      (is (= "disallowed key" (message {:isez 1})))
      (is (= "disallowed key" (message {:zise 1}))))
    (testing "a chart option in a plot: suggested too"
      (is (re-find #"should be spelled :size"
                   (pr-str (:errors (error-data #(plot/build (assoc small-plot :charts [(chart/scatter {:szie 4})]))))))))))

(defn- maps-in
  "Every `:map` schema anywhere in `schema` (nested maps, `:multi` branches, `:and` / `:or` / `:maybe` / collection
  children)."
  [schema]
  (let [s (malli/schema schema)]
    (case (malli/type s)
      :map (cons s (mapcat (fn [[_ _ v]] (maps-in v)) (malli/children s)))
      :multi (mapcat (fn [[_ _ v]] (maps-in v)) (malli/children s))
      (:maybe :and :or :vector :sequential :set :map-of :tuple)
      (mapcat #(when (malli/schema? (try (malli/schema %) (catch Exception _ nil))) (maps-in %)) (malli/children s))
      nil)))

(defn- mixed-sections
  "Keys of the user entries (not forbidden, not internal) of map `m` when some have a section and some don't."
  [m]
  (let [entries (remove (fn [[_ props v]] (or (schema/forbidden? v) (:cljplot2/internal props))) (malli/children m))
        sectioned (filter (fn [[_ props]] (:cljplot2/section props)) entries)]
    (when (< 0 (count sectioned) (count entries))
      (mapv first entries))))

(deftest sections-test
  (testing "in every map of the plot-level schemas and shared groups, every user entry has a section or none has"
    (doseq [[n s] (concat non-chart-schemas
                          (for [[f {:keys [entries]}] describe/fragments] [f (into [:map] entries)]))
            m (maps-in s)]
      (is (nil? (mixed-sections m)) n)))
  (testing "plot-level tables longer than 12 options are sliced (the theme per top-level key, its own slicing)"
    (doseq [[n rows] (concat (for [[n s] (dissoc non-chart-schemas :theme)] [n (describe/options s)])
                             (for [k (distinct (map (comp first :path) (describe/theme-rows)))]
                               [[:theme k] (describe/theme-rows k)]))
            :let [options (remove #(:cljplot2.describe/container? (meta %)) rows)]
            :when (< 12 (count rows))]
      (is (every? :section options) n)))
  (testing "boundary: a map mixing sectioned and plain entries is found"
    (is (= [:a :b] (mixed-sections (malli/schema [:map [:a {:cljplot2/section :s} :int] [:b :int]]))))
    (is (nil? (mixed-sections (malli/schema [:map [:a :int] [:b :int]]))))))

(deftest options-table-test
  (let [rows (describe/options cljplot2.pipeline/options-schema)
        table #(-> % meta :kindly/kind)]
    (testing "a table of all rows or of one section, its code hidden"
      (is (= :kind/table (table (nu/options-table rows))))
      (is (= 3 (count (:row-vectors (nu/options-table rows :image)))) "width, height, hint")
      (is (:kindly/hide-code (meta (nu/options-table rows)))))
    (testing "a Group column only when the rows mix groups"
      (is (= 4 (count (:column-names (nu/options-table (describe/fragment-rows :curve))))) "one group")
      (is (= 5 (count (:column-names (nu/options-table (describe/options (chart/line)))))) "own and group rows"))
    (testing "boundary: no rows, an unknown section - errors, never an empty table"
      (is (= :rows (:key (error-data #(nu/options-table [])))))
      (is (= :section (:key (error-data #(nu/options-table rows :no-such-section))))))))
