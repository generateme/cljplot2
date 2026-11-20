(ns cljplot2.reference-r3-test
  "Reference part R3: `plot/save`, spelling suggestions for transposed keys, table sections (`:cljplot2/section`) and
  the notebook options tables (`notebook-utils/options-table`)."
  (:require [clojure.java.io :as io]
            [clojure2d.color]
            [clojure.test :refer [deftest testing is]]
            [malli.core :as malli]
            [notebook-utils :as nu]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.content.legend :as legend]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.test-util :refer [error-data same-pixels?]]
            [cljplot2.theme])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private small-plot {:data [{:x 1 :y 1} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})

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

;; text role colours (:contrast)

(def ^:private fill-label-plots
  "Role -> a plot drawing labels of that role over fills."
  (let [budget [{:a "x" :b "p" :v 4} {:a "x" :b "q" :v 3} {:a "y" :b "r" :v 2}]]
    {:treemap-leaf {:data budget :mapping {:hierarchy [:a :b] :value :v} :charts [(chart/treemap)]}
     :treemap-group {:data budget :mapping {:hierarchy [:a :b] :value :v} :charts [(chart/treemap)]}
     :partition-leaf {:data budget :mapping {:hierarchy [:a :b] :value :v} :charts [(chart/partition)]}
     :pack-leaf {:data budget :mapping {:hierarchy [:a :b] :value :v} :charts [(chart/pack)]}
     :venn-count {:data [{:s "a" :i 1} {:s "a" :i 2} {:s "b" :i 2} {:s "b" :i 3}] :mapping {:set :s :item :i}
                  :charts [(chart/venn)]}
     :tile-value {:data [{:x "a" :y "u" :v 1} {:x "b" :y "u" :v 9}] :mapping {:x :x :y :y :color :v}
                  :charts [(chart/tile {:value-label true})]}
     :bar-value {:data [{:k "a" :v 3} {:k "b" :v 5}] :mapping {:x :k :y :v}
                 :charts [(chart/bar {:value-label true :label-position :center})]}}))

(defn- with-role-color [p role color] (assoc-in p [:theme :text role :color] color))

(deftest role-color-test
  (let [o {:width 240 :height 180}]
    (doseq [[role p] fill-label-plots]
      (testing (str role ": the default is :contrast; an explicit colour wins over it")
        (is (= :contrast (get-in cljplot2.theme/default-theme [:text role :color])))
        (is (same-pixels? p (with-role-color p role :contrast) o))
        (is (not (same-pixels? p (with-role-color p role [255 0 255]) o)))))
    (testing "bar labels past the bar's end: :contrast by the plot background - black on the default one, white on black"
      (let [p {:data [{:k "a" :v 3}] :mapping {:x :k :y :v} :charts [(chart/bar {:value-label true})]}]
        (is (same-pixels? p (with-role-color p :bar-value :black) o))
        (is (same-pixels? (assoc-in p [:theme :plot :background] :black)
                          (-> p (assoc-in [:theme :plot :background] :black) (with-role-color :bar-value :white)) o))))
    (testing "the :annotation role styles strip n labels and text charts, not bar values"
      (let [bars (:bar-value fill-label-plots)
            box {:data [{:k "a" :v 1} {:k "a" :v 2}] :mapping {:x :k :y :v} :charts [(chart/box {:n-label true})]}]
        (is (same-pixels? bars (with-role-color bars :annotation [255 0 255]) o))
        (is (not (same-pixels? box (with-role-color box :annotation [255 0 255]) o)))))
    (testing "boundary: :contrast on a role with no fill behind - by the plot background (:none - white)"
      (let [p (assoc small-plot :labels {:title "T"})]
        (is (same-pixels? (with-role-color p :title :contrast) (with-role-color p :title :black) o))
        (is (same-pixels? (-> p (assoc-in [:theme :plot :background] :black) (with-role-color :title :contrast))
                          (-> p (assoc-in [:theme :plot :background] :black) (with-role-color :title :white)) o))
        (is (= :white (draw/plot-backdrop {:plot {:background :none}})))))
    (testing "the text colour type: a colour or :contrast; anything else fails at the role"
      (is (= :color (peek (:in (first (:errors (error-data #(plot/build (with-role-color small-plot :title :contrastt)))))))))
      (is (nil? (error-data #(plot/build (with-role-color small-plot :title [10 20 30]))))))
    (testing "resolution: a colour stays; :contrast is white on dark, black on light"
      (is (= [10 20 30] (draw/resolved-text-color [10 20 30] :black)))
      (is (= :white (draw/resolved-text-color :contrast :black)))
      (is (= :black (draw/resolved-text-color :contrast :white))))))

;; colour search (cljplot2.core over clojure2d.color)

(deftest colour-functions-test
  (testing "the same results as clojure2d.color, every arity"
    (is (= (clojure2d.color/find-palette) (plot/find-palette)))
    (is (= (clojure2d.color/find-palette #"tableau") (plot/find-palette #"tableau")))
    (is (= (clojure2d.color/find-gradient "viridis" #"ma") (plot/find-gradient "viridis" #"ma")))
    (is (= (clojure2d.color/palette :tableau-10) (plot/palette :tableau-10)))
    (is (= (clojure2d.color/palette :tableau-10 3) (plot/palette :tableau-10 3)))
    (is (= ((clojure2d.color/gradient :viridis/magma) 0.3) ((plot/gradient :viridis/magma) 0.3))))
  (testing "boundary: no match - empty; one colour of a palette"
    (is (empty? (plot/find-palette #"^no-such-palette$")))
    (is (= 1 (count (plot/palette :tableau-10 1))))))

;; colour transforms

(defn- color-of
  "Colour (rounded channels) of `v` on a linear colour scale over `domain` with `transform` (spec through the
  library)."
  [domain transform v]
  (let [spec (aesthetic/scale-spec :color {:transform transform :domain domain} nil false
                                   (:palette cljplot2.theme/default-theme))]
    (mapv #(Math/round (double %)) ((aesthetic/attribute-fn :color spec :grey) v))))

(deftest transform-test
  (testing "named transforms: :sq and :exp place values by square and exponential"
    (is (= (color-of [0 10] :sq 5) (color-of [0 100] nil 25)))
    (is (= (color-of [0 2] :exp 1) (color-of [1 (Math/exp 2)] nil (Math/exp 1)))))
  (testing "a function: as the named one it equals"
    (is (= (color-of [0 10] :sq 5) (color-of [0 10] #(* % %) 5))))
  (testing "a function's numeric inverse (the colour bar): increasing and decreasing"
    (let [inc-inv (:inverse (aesthetic/value-transform {:transform #(* % %) :domain [0 10]}))
          dec-inv (:inverse (aesthetic/value-transform {:transform #(- %) :domain [0 10]}))]
      (is (< (Math/abs (- 3.0 (double (inc-inv 9.0)))) 1e-9))
      (is (< (Math/abs (- 4.0 (double (dec-inv -4.0)))) 1e-9))
      (is (< (Math/abs (- 10.0 (double (inc-inv 1e9)))) 1e-9) "beyond the range: the nearest end")))
  (testing "boundary: a domain the transform can't take, non-finite ends - errors at :transform"
    (doseq [[domain t] [[[-1 3] :sq] [[-1 3] :sqrt] [[0 3] :log] [[1 3000] :exp] [[0 1] #(/ 1.0 %)]]]
      (is (= :transform (:key (error-data #(plot/build {:data [{:x 1 :y (first domain)} {:x 2 :y (second domain)}]
                                                         :mapping {:x :x :y :y :color :y} :charts [(chart/scatter)]
                                                         :scales {:color {:transform t}}}))))
          [domain t])))
  (testing "a colour bar renders under every transform"
    (doseq [t [:sqrt :log :sq :exp #(Math/cbrt %)]]
      (is (instance? BufferedImage (plot/render-image {:data (for [i (range 1 11)] {:x i :y i}) :mapping {:x :x :y :y :color :y}
                                                       :charts [(chart/scatter)] :scales {:color {:transform t}}}
                                                      {:width 300 :height 200}))))))

;; dates on colour

(def ^:private dated
  (for [i (range 12)] {:x i :y i :d (.plusMonths (java.time.LocalDate/of 2024 1 1) i)}))

(deftest temporal-color-test
  (let [p {:data dated :mapping {:x :x :y :y :color :d} :charts [(chart/scatter)]}
        spec #(get-in (plot/build % {:width 300 :height 200}) [:scale-specs :color])]
    (testing "dates mapped to :color: a linear scale over epoch milliseconds, a date sample for the labels"
      (is (= :linear (:kind (spec p))))
      (is (= [(rows/x-key (:d (first dated))) (rows/x-key (:d (last dated)))] (:domain (spec p))))
      (is (instance? java.time.LocalDate (:temporal-sample (spec p)))))
    (testing "the colour bar ticks at dates (labels of a datetime scale)"
      (let [legend (first (filter #(= :colorbar (:kind %)) (legend/legends (plot/build p {:width 300 :height 200 :until :scale-specs}))))]
        (is (seq (:labels legend)))
        (is (every? #(re-find #"[A-Za-z]{3}|\d{4}" %) (:labels legend)))))
    (testing "an explicit domain of two dates: rows outside it aren't drawn"
      (let [d (fn [m] (java.time.LocalDate/of 2024 m 1))
            s (spec (assoc p :scales {:color {:domain [(d 3) (d 6)]}}))]
        (is (= [(rows/x-key (d 3)) (rows/x-key (d 6))] (:domain s)))
        (is (nil? ((aesthetic/attribute-fn :color s :grey) (d 9))))
        (is (some? ((aesthetic/attribute-fn :color s :grey) (d 4))))))
    (testing "boundary: :midpoint, :transform and :binned need numbers"
      (is (= :midpoint (:key (error-data #(spec (assoc p :scales {:color {:midpoint 3}}))))))
      (is (= :transform (:key (error-data #(spec (assoc p :scales {:color {:transform :sqrt}}))))))
      (is (= :kind (:key (error-data #(spec (assoc p :scales {:color {:kind :binned}})))))))
    (testing "boundary: dates on :size stay levels (ordinal); dates declared categorical stay levels on :color"
      (is (= :ordinal (get-in (plot/build (assoc p :mapping {:x :x :y :y :size :d}) {:width 300 :height 200 :until :scale-specs})
                              [:scale-specs :size :kind])))
      (is (= :ordinal (:kind (spec (assoc p :scales {:color {:type :categorical}}))))))))

;; data tables

(deftest data-table-test
  (let [table (fn [data] (nu/data-table data))]
    (testing "rows: a column per key (first appearance), cells as text; the code hidden"
      (let [t (table [{:a 1 :b "x"} {:a 2 :c (java.time.LocalDate/of 2024 1 2)}])]
        (is (= ["a" "b" "c"] (:column-names t)))
        (is (= [["1" "x" "nil"] ["2" "nil" "2024-01-02"]] (:row-vectors t)))
        (is (:kindly/hide-code (meta t)))))
    (testing "columns become rows"
      (is (= [["0" "1"] ["1" "2"]] (:row-vectors (table {:t [0 1] :v [1 2]})))))
    (testing "boundary: at most 20 rows; exactly one row"
      (is (= 20 (count (:row-vectors (table (for [i (range 50)] {:i i}))))))
      (is (= 1 (count (:row-vectors (table [{:i 0}]))))))
    (testing "boundary: empty, not data, unequal columns - errors keyed :data"
      (doseq [bad [[] nil {} [:a :b] {:t [0 1] :v [1]} "rows"]]
        (is (= :data (:key (error-data #(table bad)))) (pr-str bad))))))
