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
            [cljplot2.content.axis :as axis]
            [cljplot2.data.tree :as tree]
            [cljplot2.scale.position :as position]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [fastmath.vector]
            [cljplot2.graphics.shape]
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
        (is (= [["1" "x" ""] ["2" "" "2024-01-02"]] (:row-vectors t)) "a missing value - an empty cell")
        (is (:kindly/hide-code (meta t)))))
    (testing "columns become rows"
      (is (= [["0" "1"] ["1" "2"]] (:row-vectors (table {:t [0 1] :v [1 2]})))))
    (testing "boundary: at most 20 rows; exactly one row"
      (is (= 20 (count (:row-vectors (table (for [i (range 50)] {:i i}))))))
      (is (= 1 (count (:row-vectors (table [{:i 0}]))))))
    (testing "boundary: empty, not data, unequal columns - errors keyed :data"
      (doseq [bad [[] nil {} [:a :b] {:t [0 1] :v [1]} "rows"]]
        (is (= :data (:key (error-data #(table bad)))) (pr-str bad))))))

;; CSV column order

(deftest csv-order-test
  (testing "rows keep the header's column order, also past 8 columns (hash maps would scramble it)"
    (let [header (mapv #(str "c" %) (range 12))
          text (str (clojure.string/join "," header) "\n" (clojure.string/join "," (range 12)) "\n")
          rows (nu/parse-csv text)]
      (is (= (map keyword header) (keys (first rows))))
      (is (= ["c0" "c1"] (take 2 (:column-names (nu/data-table rows)))))))
  (testing "boundary: one column; a dataset of the book (mtcars - model first)"
    (is (= [:a] (keys (first (nu/parse-csv "a\n1\n")))))
    (is (= :model (first (keys (first (nu/dataset :mtcars))))))))

;; colliding labels

(deftest kept-label-indices-test
  (testing "the first label, then each not overlapping the last kept one"
    (is (= [0 2 4] (measure/kept-label-indices [0 5 10 15 20] [8 8 8 8 8])))
    (is (= [0 1 2] (measure/kept-label-indices [0 10 20] [8 8 8]))))
  (testing "boundary: no labels; one; touching or 1 px overlap kept, more dropped; reversed positions"
    (is (= [] (measure/kept-label-indices [] [])))
    (is (= [0] (measure/kept-label-indices [5] [100])))
    (is (= [0 1] (measure/kept-label-indices [0 10] [10 10])) "touching")
    (is (= [0 1] (measure/kept-label-indices [0 9] [10 10])) "1 px")
    (is (= [0] (measure/kept-label-indices [0 8.5] [10 10])) "1.5 px")
    (is (= [0 2] (measure/kept-label-indices [100 95 80] [8 8 8])) "pixels decreasing (a vertical axis)")))

(deftest axis-label-collisions-test
  (let [sizes (fn [n w] (vec (repeat n (fastmath.vector/vec2 w 10))))]
    (testing "continuous axes drop labels colliding with the last drawn one; bands axes keep every s-th label"
      (let [finalized (get-in (plot/build {:data [{:x 0 :y 0} {:x 4 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                           :scales {:x {:ticks [0 1 2 3 4] :expand [0 0] :overlapping-labels :drop}}}
                                          {:width 300 :height 200})
                              [:finalized-scales :x])
            n (count (:ticks finalized))
            step (/ (Math/abs (- (double (second (:range finalized))) (double (first (:range finalized))))) (dec n))]
        (is (= (vec (range n)) (axis/drawn-label-indices :x finalized (sizes n (* 0.5 step)) 2 nil nil)) "room for all")
        (is (= (vec (range 0 n 2)) (axis/drawn-label-indices :x finalized (sizes n (* 1.5 step)) 2 nil nil)) "every other fits")))
    (testing "a symlog axis crowded near 0: fewer labels drawn than ticks, none overlapping"
      (let [built (plot/build {:data (for [i (range -40 41)] {:x (* i i i) :y i}) :mapping {:x :x :y :y}
                               :charts [(chart/scatter)] :scales {:x {:kind :symlog :ticks 15}}}
                              {:width 300 :height 200})]
        (is (instance? BufferedImage (plot/render-image (dissoc built :subplots) {:width 300 :height 200})))))))

;; dates as colour bar ticks

(deftest date-ticks-test
  (let [p {:data dated :mapping {:x :x :y :y :color :d} :charts [(chart/scatter)]}
        d (fn [m] (java.time.LocalDate/of 2024 m 1))
        colorbar (fn [plot] (first (filter #(= :colorbar (:kind %))
                                           (legend/legends (plot/build plot {:width 300 :height 200 :until :scale-specs})))))]
    (testing "explicit date ticks: those inside the domain, placed by their epoch milliseconds"
      (let [cb (colorbar (assoc p :scales {:color {:ticks [(d 3) (d 6) (d 12) (java.time.LocalDate/of 2030 1 1)]}}))]
        (is (= (mapv #(rows/x-key (d %)) [3 6 12]) (:ticks cb)))))
    (testing "the formatter gets values of the data's type (LocalDate), computed ticks too"
      (is (every? #{"java.time.LocalDate"}
                  (:labels (colorbar (assoc p :scales {:color {:formatter #(.getName (class %))}}))))))
    (testing "boundary: numbers on a date scale, dates on a numeric one - errors at :ticks"
      (is (= :ticks (:key (error-data #(plot/build (assoc p :scales {:color {:ticks [1 2]}}))))))
      (is (= :ticks (:key (error-data #(plot/build (assoc p :mapping {:x :x :y :y :color :y}
                                                          :scales {:color {:ticks [(d 3)]}})))))))))

;; legend texts with :contrast

(deftest legend-contrast-test
  (testing "legend titles and labels resolve :contrast over the legend backdrop - no failure, black on the default"
    (doseq [p [{:data dated :mapping {:x :x :y :y :color :y} :charts [(chart/scatter)]}
               {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "b"}] :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]}]
            :let [o {:width 300 :height 200}
                  with (fn [p color] (-> p (assoc-in [:theme :text :legend-label :color] color)
                                         (assoc-in [:theme :text :legend-title :color] color)))]]
      (is (same-pixels? (with p :contrast) (with p :black) o))
      (is (same-pixels? (-> p (with :contrast) (assoc-in [:theme :legend :fill] :black))
                        (-> p (with :white) (assoc-in [:theme :legend :fill] :black)) o)))))

(deftest throwing-transform-test
  (testing "a transform function throwing at a domain end: the keyed error, not the raw exception"
    (is (= :transform (:key (error-data #(plot/build {:data [{:x 1 :y 0} {:x 2 :y 3}] :mapping {:x :x :y :y :color :y}
                                                       :charts [(chart/scatter)]
                                                       :scales {:color {:transform (fn [v] (if (zero? (double v)) (throw (ex-info "no" {})) v))}}})))))))

(deftest attribute-values-test
  (testing "every built-in shape, dash style, pattern and cell type, sorted"
    (is (= (set cljplot2.graphics.shape/shapes) (set (:shape describe/attribute-values))))
    (is (= (sort (keys schema/dash-styles)) (:dash describe/attribute-values)))
    (is (some #{:hatch} (:pattern describe/attribute-values)))
    (is (some #{:pointy-hex} (:cell describe/attribute-values)))))

;; :overlapping-labels

(defn- crowded-axis-drawn
  "`[tick-count drawn-indices]` of a crowded continuous x axis of explicit `:ticks` 0..40, position scale `spec` merged
  in."
  [spec]
  (let [finalized (get-in (plot/build {:data [{:x 0 :y 0} {:x 40 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                       :scales {:x (merge {:ticks (vec (range 0 41)) :expand [0 0]} spec)}}
                                      {:width 300 :height 200})
                              [:finalized-scales :x])
        sizes (vec (repeat (count (:ticks finalized)) (fastmath.vector/vec2 30 10)))]
    [(count sizes) (axis/drawn-label-indices :x finalized sizes 2 nil nil)]))

(deftest overlapping-labels-test
  (testing ":drop - colliding labels dropped; :keep - every label drawn (the default for explicit ticks: see
            explicit-ticks-keep-test)"
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :drop})]
      (is (< (count drawn) n)))
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :keep})]
      (is (= (vec (range n)) drawn))))
  (testing "the finalized scale carries :keep only; an :axis-domain display scale carries it too"
    (is (= :keep (:overlapping-labels (position/finalize {:kind :linear :domain [0 1] :overlapping-labels :keep} [0 100] 5))))
    (is (nil? (:overlapping-labels (position/finalize {:kind :linear :domain [0 1] :overlapping-labels :drop} [0 100] 5))))
    (is (= :keep (:overlapping-labels (position/display-scale
                                       (position/finalize {:kind :linear :domain [0 1] :axis-domain [0 10] :overlapping-labels :keep}
                                                          [0 100] 5))))))
  (testing "builders naming data items on an axis keep every name: arc, dendrogram leaves, alluvial axes"
    (is (= :keep (get-in (plot/arc [{:from "a" :to "b"}] {}) [:scales :x :overlapping-labels])))
    (is (= :keep (get-in (plot/dendrogram (tree/from-parents [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1}] {}) {})
                         [:scales :x :overlapping-labels])))
    (is (= :keep (get-in (plot/alluvial [{:a "p" :b "q"} {:a "r" :b "q"}] {:axes [:a :b]}) [:scales :x :overlapping-labels]))))
  (testing "boundary: a builder's :scales override it; other values rejected"
    (is (= :drop (get-in (plot/arc [{:from "a" :to "b"}] {:scales {:x {:overlapping-labels :drop}}})
                         [:scales :x :overlapping-labels])))
    (is (= :overlapping-labels (:key (error-data #(plot/build (assoc small-plot :scales {:x {:overlapping-labels :all}}))))))))

;; label boxes (polar labels)

(deftest kept-box-indices-test
  (let [box (fn [x y] {:x x :y y :w 10 :h 10})]
    (testing "a box is kept when it overlaps no kept box; 1 px overlap tolerated"
      (is (= [0 2] (measure/kept-box-indices [(box 0 0) (box 5 0) (box 20 0)])))
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 9 0)])) "1 px")
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 5 20)])) "apart across"))
    (testing "boundary: no boxes; one; the last checked against the first (a circle of labels)"
      (is (= [] (measure/kept-box-indices [])))
      (is (= [0] (measure/kept-box-indices [(box 0 0)])))
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 30 0) (box 3 2)]))))))

(deftest polar-label-collisions-test
  (let [placements #'cljplot2.content.panel/polar-label-placements
        polar-x (fn [spec] (get-in (plot/build {:data (for [i (range 60)] {:x i :y 1}) :mapping {:x :x :y :y}
                                                :charts [(chart/scatter)] :coord {:kind :polar}
                                                :scales {:x (merge {:ticks 30} spec)}}
                                               {:width 200 :height 200})
                                   [:finalized-scales :x]))
        ;; every label at one point: all collide
        same-place (fn [_] [50.0 50.0 [:center :middle]])
        count-placed (fn [finalized] (measure/call-with-scratch-canvas
                                      :high #(count (placements % finalized same-place {:theme cljplot2.theme/default-theme}))))]
    (testing "colliding polar labels: only the first kept"
      (is (= 1 (count-placed (polar-x {})))))
    (testing ":overlapping-labels :keep and explicit ticks keep every label"
      (let [kept (polar-x {:overlapping-labels :keep})]
        (is (= (count (:ticks kept)) (count-placed kept))))
      (let [explicit (polar-x {:ticks [0 10 20 30]})]
        (is (= (count (:ticks explicit)) (count-placed explicit))))
      (is (= 1 (count-placed (polar-x {:ticks [0 10 20 30] :overlapping-labels :drop})))))))

(deftest explicit-ticks-keep-test
  (testing "explicit :ticks keep their labels unless :drop is asked; computed ticks drop"
    (is (position/keep-labels? {:ticks [1 2]}))
    (is (not (position/keep-labels? {:ticks [1 2] :overlapping-labels :drop})))
    (is (position/keep-labels? {:ticks 5 :overlapping-labels :keep}))
    (is (not (position/keep-labels? {:ticks 5})))
    (is (not (position/keep-labels? {}))))
  (testing "a finalized scale of explicit ticks keeps; its crowded axis draws every label"
    (let [[n drawn] (crowded-axis-drawn {})]
      (is (= (vec (range n)) drawn) "the crowded helper gives explicit :ticks"))
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :drop})]
      (is (< (count drawn) n)))))

;; other java.time types on colour

(deftest temporal-types-color-test
  (doseq [[label values] {"LocalTime" (for [h (range 8 20)] (java.time.LocalTime/of h 30))
                          "Instant" (for [d (range 12)] (.plusSeconds (java.time.Instant/parse "2024-01-01T00:00:00Z") (* d 86400 30)))
                          "ZonedDateTime" (for [d (range 12)] (.plusDays (java.time.ZonedDateTime/of 2024 1 1 0 0 0 0 (java.time.ZoneId/of "Europe/Warsaw")) (* 30 d)))
                          "LocalDateTime" (for [d (range 12)] (.plusHours (java.time.LocalDateTime/of 2024 1 1 0 0) (* 7 d)))}
          :let [rows (map-indexed (fn [i v] {:x i :y i :t v}) values)
                p {:data rows :mapping {:x :x :y :y :color :t} :charts [(chart/scatter)]}
                built (plot/build p {:width 300 :height 200 :until :scale-specs})
                colorbar (first (filter #(= :colorbar (:kind %)) (legend/legends built)))
                with-formatter (first (filter #(= :colorbar (:kind %))
                                              (legend/legends (plot/build (assoc p :scales {:color {:formatter #(.getName (class %))}})
                                                                          {:width 300 :height 200 :until :scale-specs}))))]]
    (testing (str label ": a linear colour scale with tick labels; the formatter gets the data's type; it renders")
      (is (= :linear (get-in built [:scale-specs :color :kind])))
      (is (<= 2 (count (:labels colorbar))))
      (is (every? #{(.getName (class (first values)))} (:labels with-formatter)))
      (is (instance? BufferedImage (plot/render-image p {:width 300 :height 200}))))))

;; JSON column order

(deftest json-order-test
  (testing "rows of a JSON dataset keep the file's key order, also past 8 keys"
    (is (= [:name :miles-per-gallon :cylinders :displacement :horsepower :weight-in-lbs :acceleration :year :origin]
           (keys (first (nu/dataset :cars))))))
  (testing "boundary: a row missing some keys keeps the order of those it has"
    (is (every? #(apply < (map (into {} (map-indexed (fn [i k] [k i]) (keys (first (nu/dataset :movies))))) (filter (set (keys (first (nu/dataset :movies)))) (keys %))))
                (take 50 (nu/dataset :movies))))))

(deftest local-date-time-conversions-test
  (let [zone (java.time.ZoneId/of "Europe/Warsaw")
        values [(java.time.LocalDate/of 2024 2 29) (java.time.LocalDateTime/of 2024 2 29 13 45)
                (java.time.LocalTime/of 23 59 59) (java.time.Instant/parse "2024-02-29T13:45:00Z")
                (java.time.ZonedDateTime/of 2024 2 29 13 45 0 0 zone)
                (java.time.OffsetDateTime/of 2024 2 29 13 45 0 0 (java.time.ZoneOffset/ofHours -5))]]
    (testing "every java.time type round-trips through a local date-time"
      (doseq [v values]
        (is (= v (rows/local-date-time->like (rows/->local-date-time v) v)) (str (class v)))))
    (testing "how each reads: a date at its start, a time on 1970-01-01, an instant in UTC, zoned in its zone"
      (is (= (java.time.LocalDateTime/of 2024 2 29 0 0) (rows/->local-date-time (values 0))))
      (is (= (java.time.LocalDateTime/of 1970 1 1 23 59 59) (rows/->local-date-time (values 2))))
      (is (= (java.time.LocalDateTime/of 2024 2 29 13 45) (rows/->local-date-time (values 3))))
      (is (= (java.time.LocalDateTime/of 2024 2 29 13 45) (rows/->local-date-time (values 4)))))
    (testing "boundary: a date-time back to a date drops the time; to a time drops the date"
      (is (= (java.time.LocalDate/of 2024 2 29) (rows/local-date-time->like (java.time.LocalDateTime/of 2024 2 29 18 0) (values 0))))
      (is (= (java.time.LocalTime/of 18 0) (rows/local-date-time->like (java.time.LocalDateTime/of 2024 2 29 18 0) (values 2)))))))

;; 3D frame labels

(deftest frame-label-collisions-test
  (let [draw-axis! #'cljplot2.graphics.space3d/draw-axis!
        boxes (fn [length]
                (measure/call-with-scratch-canvas
                 :high
                 (fn [c]
                   (draw-axis! c :x {:axis :x :from [0 0 0] :to [1 0 0]}
                               {:corner (fn [[x _ _]] (fastmath.vector/vec2 (+ 10.0 (* (double length) (double x))) 100.0))
                                :centre (fastmath.vector/vec2 (+ 10.0 (* 0.5 (double length))) 0.0)}
                               {:domains {:x [0 1e9 ]} :levels {} :titles {}}
                               [] {:theme cljplot2.theme/default-theme :canvas c}))))
        overlap? (fn [[ax0 ay0 ax1 ay1] [bx0 by0 bx1 by1]]
                   (and (< ax0 bx1) (< bx0 ax1) (< ay0 by1) (< by0 ay1)))
        pairwise-free? (fn [bs] (every? true? (for [[i a] (map-indexed vector bs) [j b] (map-indexed vector bs) :when (< i j)]
                                                (not (overlap? a b)))))]
    (testing "a short edge with wide labels: the labels drawn don't overlap each other"
      (let [short (boxes 120)]
        (is (seq short))
        (is (pairwise-free? short))))
    (testing "a long edge keeps more of them, still without overlaps"
      (let [long-boxes (boxes 900)]
        (is (pairwise-free? long-boxes))
        (is (< (count (boxes 120)) (count long-boxes)))))))

;; Year and YearMonth

(deftest year-values-test
  (let [years (for [y (range 2000 2012)] (java.time.Year/of y))
        months (for [m (range 1 13)] (java.time.YearMonth/of 2024 m))
        o {:width 300 :height 200}]
    (testing "positions and colours read a Year / YearMonth as its period's start - a datetime x, a date gradient"
      (doseq [vs [years months]
              :let [rows (map-indexed (fn [i v] {:x i :y i :t v}) vs)]]
        (is (= :datetime (get-in (plot/build {:data rows :mapping {:x :t :y :y} :charts [(chart/scatter)]} o) [:scale-specs :x :kind])))
        (is (= :linear (get-in (plot/build {:data rows :mapping {:x :x :y :y :color :t} :charts [(chart/scatter)]} o) [:scale-specs :color :kind])))
        (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :t :y :y :color :t} :charts [(chart/line) (chart/scatter)]} o)))))
    (testing "period starts and the conversions"
      (is (= (java.time.LocalDate/of 2024 1 1) (rows/period-start (java.time.Year/of 2024))))
      (is (= (java.time.LocalDate/of 2024 3 1) (rows/period-start (java.time.YearMonth/of 2024 3))))
      (is (= (rows/x-key (java.time.LocalDate/of 2024 3 1)) (rows/x-key (java.time.YearMonth/of 2024 3))))
      (is (= (java.time.Year/of 2024) (rows/local-date-time->like (rows/->local-date-time (java.time.Year/of 2024)) (java.time.Year/of 1))))
      (is (= (java.time.YearMonth/of 2024 3) (rows/local-date-time->like (java.time.LocalDateTime/of 2024 3 17 12 0) (java.time.YearMonth/of 1 1)))))
    (testing "boundary: levels stay Years - facet panels, hierarchy levels; other values pass through"
      (let [rows (map-indexed (fn [i v] {:x i :y i :t v}) (take 3 years))]
        (is (= (vec (take 3 years)) (get-in (plot/build {:data rows :mapping {:x :x :y :y :facet :t} :charts [(chart/scatter)]} o)
                                            [:split :facets :levels :facet])))
        (is (instance? BufferedImage (plot/render-image {:data (map #(assoc % :v 1) rows) :mapping {:hierarchy [:t] :value :v}
                                                         :charts [(chart/treemap)]} o)))
        (is (= "x" (rows/period-start "x")))
        (is (nil? (rows/period-start nil)))))
    (testing "an explicit domain of Years on a continuous x scale"
      (let [rows (map-indexed (fn [i v] {:x i :y i :t v}) years)]
        (is (= [(java.time.LocalDate/of 1999 1 1) (java.time.LocalDate/of 2013 1 1)]
               (get-in (plot/build {:data rows :mapping {:x :t :y :y} :charts [(chart/scatter)]
                                    :scales {:x {:domain [(java.time.Year/of 1999) (java.time.Year/of 2013)]}}} o)
                       [:scale-specs :x :domain])))))))

(deftest explicit-bands-ticks-keep-test
  (testing "explicit :ticks on a bands scale keep every label (no every-s-th thinning) unless :drop"
    (let [levels-data (for [i (range 40)] {:k (str "level number " i) :v i})
          finalized (fn [spec] (get-in (plot/build {:data levels-data :mapping {:x :k :y :v} :charts [(chart/bar)] :scales {:x spec}}
                                                   {:width 300 :height 200})
                                       [:finalized-scales :x]))
          drawn (fn [f] (count (axis/drawn-label-indices :x f (vec (repeat (count (:ticks f)) (fastmath.vector/vec2 60 10))) 2 nil nil)))
          ticks (mapv #(str "level number " %) (range 40))]
      (is (= 40 (drawn (finalized {:ticks ticks}))))
      (is (< (drawn (finalized {:ticks ticks :overlapping-labels :drop})) 40))
      (is (< (drawn (finalized {})) 40) "computed (every band) ticks thin"))))

(deftest date-colour-charts-test
  (testing "charts colouring along their marks take date colour values: line gradients, segments, polygons"
    (let [dates (for [m (range 12)] (.plusMonths (java.time.LocalDate/of 2024 1 1) m))
          rows (map-indexed (fn [i d] {:x i :y (mod (* i 5) 7) :xend (inc i) :yend (mod (* i 3) 7) :d d}) dates)
          o {:width 300 :height 200}]
      (doseq [c [(chart/line) (chart/segment) (chart/polygon)]]
        (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :x :y :y :xend :xend :yend :yend :color :d}
                                                          :charts [c]} o))
            (str (:kind c)))))))
