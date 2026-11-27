(ns cljplot2.reference-r3-test
  "Reference part R3: `plot/save`, spelling suggestions for transposed keys, table sections (`:cljplot2/section`),
  the notebook utilities (options and data tables, dataset column order), label content roles, the attribute values
  registry. Colours and dates: [[cljplot2.reference-r3-colors-test]]; tick label collisions:
  [[cljplot2.reference-r3-labels-test]]."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [malli.core :as malli]
            [notebook-utils :as nu]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.graphics.shape]
            [cljplot2.test-util :refer [error-data]])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private small-plot {:data [{:x 1 :y 1} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)] :labels {:x :none :y :none}})

(defn- delete-tree!
  "Delete file or directory `f` with everything under it."
  [^java.io.File f]
  (doseq [child (reverse (file-seq f))] (io/delete-file child true)))

(def ^:dynamic ^:private *temp-dir* nil)

(defn- temp-path
  "Path `name` under the test's temporary directory."
  [name]
  (str *temp-dir* "/" name))

(defn- with-temp-dir
  "Run `f` with [[*temp-dir*]] a fresh temporary directory, deleted afterwards (also when `f` throws)."
  [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "cljplot2-save"
                                                            (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try (binding [*temp-dir* dir] (f))
         (finally (delete-tree! dir)))))

(defn- save-cases
  "The `plot/save` cases, files under [[*temp-dir*]]."
  []
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
      (let [path (temp-path "a/b/c/plot.png")]
        (plot/save image path)
        (is (.exists (io/file path)))))
    (testing "boundary: an extension without an image writer - nothing written, the image returned"
      (let [path (temp-path "plot.nope")
            out (with-out-str (is (identical? image (plot/save image path))))]
        (is (not (.exists (io/file path))))
        (is (re-find #"can't save an image" out) "clojure2d's message on *out*, as documented")))))

(deftest save-test
  (with-temp-dir save-cases))

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
    (doseq [[n s] (concat describe/plot-level-schemas
                          (for [[f {:keys [entries]}] describe/fragments] [f (into [:map] entries)]))
            m (maps-in s)]
      (is (nil? (mixed-sections m)) n)))
  (testing "plot-level tables longer than 12 options are sliced (the theme per top-level key, its own slicing)"
    (doseq [[n rows] (concat (for [[n s] (dissoc describe/plot-level-schemas :theme)] [n (describe/options s)])
                             (for [k (distinct (map (comp first :path) (describe/theme-rows)))]
                               [[:theme k] (describe/theme-rows k)]))
            :let [options (remove describe/container-row? rows)]
            :when (< 12 (count rows))]
      (is (every? :section options) n)))
  (testing "boundary: a map mixing sectioned and plain entries is found"
    (is (= [:a :b] (mixed-sections (malli/schema [:map [:a {:cljplot2/section :s} :int] [:b :int]]))))
    (is (nil? (mixed-sections (malli/schema [:map [:a :int] [:b :int]]))))))

(deftest option-cards-fn-test
  (let [rows (describe/options cljplot2.pipeline/options-schema)
        card-count (fn [cards] (count (filter vector? (rest cards))))]
    (testing "a card per row, or per row of one section, its code hidden"
      (is (= (count rows) (card-count (nu/option-cards [:options] rows))))
      (is (= 3 (card-count (nu/option-cards [:options] rows :image))) "width, height, hint")
      (is (:kindly/hide-code (meta (nu/option-cards [:options] rows)))))
    (testing "boundary: no rows, an unknown section - errors, never an empty card group"
      (is (= :rows (:key (error-data #(nu/option-cards [:options] [])))))
      (is (= :section (:key (error-data #(nu/option-cards [:options] rows :no-such-section))))))))

(defn- label-grid
  "A one-cell custom grid holding label `content`."
  [content]
  {:custom-grid {:columns [{:size [:flexible 1]}] :rows [{:size [:flexible 1]}]
                 :items [{:id :note :column 0 :row 0 :content (merge {:kind :label :text "note"} content)}]}})

(deftest label-content-role-test
  (testing "label content needs a theme role: missing - a validation error at the role, before drawing"
    (let [data (error-data #(plot/build (label-grid {}) {:width 200 :height 100}))]
      (is (= :role (:key data)))
      (is (re-find #"missing required key" (pr-str (:errors data))))))
  (testing "a known role builds; an unknown one fails keyed by the role"
    (is (instance? BufferedImage (plot/render-image (label-grid {:role :axis-title}) {:width 200 :height 100})))
    (is (= :role (:key (error-data #(plot/build (label-grid {:role :no-such-role}) {:width 200 :height 100}))))))
  (testing "boundary: plot :labels need no role (the layout gives title / axis-title)"
    (is (instance? BufferedImage (plot/render-image (assoc small-plot :labels {:title {:text "t"} :x {:text "x"}})
                                                    {:width 200 :height 150})))))

(deftest data-table-test
  (let [table (fn [data] (nth (nu/data-table data) 2))]
    (testing "a table in a box of the data-table class (compact style), the code hidden"
      (let [box (nu/data-table [{:a 1}])]
        (is (= :kind/hiccup (:kindly/kind (meta box))))
        (is (:kindly/hide-code (meta box)))
        (is (= [:div {:class "data-table"}] (subvec box 0 2)))
        (is (= :kind/table (:kindly/kind (meta (nth box 2)))))))
    (testing "rows: a column per key (first appearance), cells as text"
      (let [t (table [{:a 1 :b "x"} {:a 2 :c (java.time.LocalDate/of 2024 1 2)}])]
        (is (= ["a" "b" "c"] (:column-names t)))
        (is (= [["1" "x" ""] ["2" "" "2024-01-02"]] (:row-vectors t)) "a missing value - an empty cell")))
    (testing "columns become rows"
      (is (= [["0" "1"] ["1" "2"]] (:row-vectors (table {:t [0 1] :v [1 2]})))))
    (testing "boundary: at most 100 rows, longer data cut with a last row of dots; exactly 100 and one row uncut"
      (let [cut (:row-vectors (table (for [i (range 101)] {:i i :j (- i)})))]
        (is (= 101 (count cut)))
        (is (= ["99" "-99"] (nth cut 99)))
        (is (= ["…" "…"] (peek cut)) "a mark per column"))
      (is (= 101 (count (:row-vectors (table (for [i (range 5000)] {:i i}))))) "long data: 100 rows and the mark")
      (is (= ["99"] (peek (:row-vectors (table (for [i (range 100)] {:i i}))))) "exactly 100 - no mark")
      (is (= [["0"]] (:row-vectors (table [{:i 0}]))))
      (is (= 101 (count (:row-vectors (table {:t (range 300) :v (range 300)})))) "columns cut alike"))
    (testing "boundary: empty, not data, unequal columns - errors keyed :data"
      (doseq [bad [[] nil {} [:a :b] {:t [0 1] :v [1]} "rows"]]
        (is (= :data (:key (error-data #(table bad)))) (pr-str bad))))))

(deftest csv-order-test
  (testing "rows keep the header's column order, also past 8 columns (hash maps would scramble it)"
    (let [header (mapv #(str "c" %) (range 12))
          text (str (clojure.string/join "," header) "\n" (clojure.string/join "," (range 12)) "\n")
          rows (nu/parse-csv text)]
      (is (= (map keyword header) (keys (first rows))))
      (is (= ["c0" "c1"] (take 2 (:column-names (nth (nu/data-table rows) 2)))))))
  (testing "boundary: one column; a dataset of the book (mtcars - model first)"
    (is (= [:a] (keys (first (nu/parse-csv "a\n1\n")))))
    (is (= :model (first (keys (first (nu/dataset :mtcars))))))))

(deftest json-order-test
  (testing "rows of a JSON dataset keep the file's key order, also past 8 keys"
    (is (= [:name :miles-per-gallon :cylinders :displacement :horsepower :weight-in-lbs :acceleration :year :origin]
           (keys (first (nu/dataset :cars))))))
  (testing "boundary: a row missing some keys keeps the order of those it has"
    (is (every? #(apply < (map (into {} (map-indexed (fn [i k] [k i]) (keys (first (nu/dataset :movies))))) (filter (set (keys (first (nu/dataset :movies)))) (keys %))))
                (take 50 (nu/dataset :movies))))))

(deftest attribute-values-test
  (testing "every built-in shape, dash style, pattern and cell type, sorted"
    (is (= (set cljplot2.graphics.shape/shapes) (set (:shape describe/attribute-values))))
    (is (= (sort (keys schema/dash-styles)) (:dash describe/attribute-values)))
    (is (some #{:hatch} (:pattern describe/attribute-values)))
    (is (some #{:pointy-hex} (:cell describe/attribute-values)))))
