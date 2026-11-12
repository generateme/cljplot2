(ns cljplot2.aesthetic-test
  "Step 7: aesthetics (color, shape, size), their scales, legends, bands grid lines."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [cljplot2.aesthetic :as aesthetic]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- build
  ([p] (plot/build p {:width 600 :height 400}))
  ([p opts] (plot/build p (merge {:width 600 :height 400} opts))))

(def ^:private rows
  [{:x 1 :y 1 :year 1932 :g "b" :s 10}
   {:x 2 :y 2 :year 1931 :g "a" :s 20}
   {:x 3 :y 3 :year 1932 :g "b" :s 30}
   {:x 4 :y 4 :year 1931 :g "c" :s 40}])

(defn- plot-of
  ([mapping] (plot-of mapping {}))
  ([mapping extra] (merge {:data rows :mapping (merge {:x :x :y :y} mapping) :charts [(chart/scatter)]} extra)))

(defn- panel-data [built] (get-in built [:stats 0 :data :panel]))
(defn- spec [built k] (get-in built [:scale-specs k]))

(defn- legends [built]
  (some->> (:items (:grid built)) (filter #(= :legends (:id %))) first :content :items (mapv :content)))

(defn- rgb [col] (vec (take 3 (map long (c/to-color col)))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(defn- measured
  "Natural size of `content` with `theme`, measured on a scratch canvas."
  ^fastmath.vector.Vec2 [content theme]
  (measure/call-with-scratch-canvas :high #(measure/natural-size content {:canvas % :theme theme})))

;; 1.x mapping

(deftest mapping-test
  (testing "mapped aesthetic: per-point vectors aligned with x / y"
    (let [d (panel-data (build (plot-of {:color :year :size :s :shape :g})))]
      (is (= [1932 1931 1932 1931] (:color d)))
      (is (= ["b" "a" "b" "c"] (:shape d)))
      (is (= (count (:x d)) (count (:color d)) (count (:size d)) (count (:shape d))))))
  (testing "constant in the mapping raises"
    (is (= {:key :mapping :value :color} (error-data #(build (plot-of {:color [255 0 0]})))))
    (is (= {:key :mapping :value :size} (error-data #(build (plot-of {:size 5}))))))
  (testing "nil accessor = not mapped"
    (let [built (build (plot-of {:color nil}))]
      (is (not (contains? (panel-data built) :color)))
      (is (nil? (spec built :color)))
      (is (nil? (legends built)))))
  (testing "function accessor"
    (is (= [1933 1932 1933 1932] (:color (panel-data (build (plot-of {:color #(inc (:year %))}))))))))

(deftest na-test
  (let [data (conj rows {:x 5 :y 5 :year nil :g nil :s 50})]
    (testing "nil -> NA level, listed last"
      (let [built (build (plot-of {:color :g} {:data data}))]
        (is (= ["b" "a" "b" "c" nil] (:color (panel-data built))))
        (is (true? (:na? (spec built :color))))
        (is (= ["a" "b" "c" "NA"] (mapv :label (:entries (first (legends built))))))
        (is (= {:color (get-in theme/default-theme [:palette :na])} (:attributes (last (:entries (first (legends built))))))
            "NA entry: NA color only")))
    (testing "NA point drawn with the NA color"
      (let [p (plot-of {:color :g} {:data [{:x 0 :y 0 :g nil} {:x 1 :y 1 :g "a"}]
                                    :theme plain :charts [(chart/scatter {:size 20})]})
            built (build p)
            {:keys [x y]} (grid/item-box (:layout built) :panel)
            fx (get-in built [:finalized-scales :x])
            fy (get-in built [:finalized-scales :y])
            c (plot/render p {:width 600 :height 400})
            px (+ (long x) (long ((requiring-resolve 'cljplot2.scale/forward) fx 0)))
            py (+ (long y) (long ((requiring-resolve 'cljplot2.scale/forward) fy 0)))]
        (is (= (rgb (get-in theme/default-theme [:palette :na])) (pixel c px py)))))
    (testing ":groups {:na false} drops and counts"
      (let [built (build (plot-of {:color :g} {:data data :groups {:na false}}))]
        (is (= {:x 0 :y 0 :color 1} (get-in built [:stats 0 :dropped])))
        (is (false? (:na? (spec built :color))))
        (is (= ["a" "b" "c"] (mapv :label (:entries (first (legends built))))))))
    (testing "NaN / infinite dropped and counted (NA on)"
      (let [built (build (plot-of {:size :s} {:data (conj rows {:x 6 :y 6 :s ##NaN} {:x 7 :y 7 :s ##Inf})}))]
        (is (= 2 (get-in built [:stats 0 :dropped :size])))
        (is (= [10 20 30 40] (:size (panel-data built))))))
    (testing "all values NA -> misspelt mapping error"
      (is (= {:key :mapping :value :color :rows 4} (error-data #(build (plot-of {:color :yaer}))))))
    (testing "empty data: no error, no scale, no legend"
      (let [built (build (plot-of {:color :g} {:data []}))]
        (is (nil? (spec built :color)))
        (is (nil? (legends built)))))
    (testing "invalid :groups"
      (are [g] (= :groups (:key (error-data #(build (plot-of {:color :g} {:groups g})))))
        {:na 1} [:na] true))))

;; 2.x scales

(def ^:private category20 (vec (c/palette :category20)))

(deftest ordinal-test
  (testing "numbers forced ordinal: sorted levels, category20 colors"
    (let [s (spec (build (plot-of {:color :year} {:scales {:color {:kind :ordinal}}})) :color)
          f (aesthetic/attribute-fn :color s :na)]
      (is (= [1931 1932] (:domain s)))
      (is (= "#1f77b4" (c/format-hex (f 1931))))
      (is (= "#aec7e8" (c/format-hex (f 1932))))
      (is (= :na (f 1999)) "unknown value -> NA")
      (is (= :na (f nil)))))
  (testing "strings sorted; explicit domain keeps its order"
    (is (= ["a" "b" "c"] (:domain (spec (build (plot-of {:color :g})) :color))))
    (is (= ["c" "b" "a"] (:domain (spec (build (plot-of {:color :g} {:scales {:color {:domain ["c" "b" "a"]}}})) :color)))))
  (testing "values outside an explicit domain -> NA level"
    (let [s (spec (build (plot-of {:color :g} {:scales {:color {:domain ["a" "b"]}}})) :color)]
      (is (true? (:na? s)))
      (is (= :na ((aesthetic/attribute-fn :color s :na) "c")))))
  (testing "one level; more levels than colors cycle"
    (let [one (aesthetic/scale-spec :color {:domain ["x"]} nil false (:palette theme/default-theme))
          many (aesthetic/scale-spec :color {:kind :ordinal :domain (vec (range 25))} nil false (:palette theme/default-theme))
          f (aesthetic/attribute-fn :color many nil)]
      (is (= (category20 0) ((aesthetic/attribute-fn :color one nil) "x")))
      (is (= (f 0) (f 20)))
      (is (= (f 4) (f 24)))))
  (testing "shape: always ordinal, cycled shape sequence"
    (let [s (spec (build (plot-of {:shape :s})) :shape)
          f (aesthetic/attribute-fn :shape s nil)]
      (is (= :ordinal (:kind s)))
      (is (= [\O \s \A \x] (mapv f [10 20 30 40])))))
  (testing "ordinal size: evenly spread, midpoint for one level"
    (let [f (aesthetic/attribute-fn :size (spec (build (plot-of {:size :g})) :size) nil)]
      (is (= [2.0 6.0 10.0] (mapv f ["a" "b" "c"]))))
    (is (= 6.0 ((aesthetic/attribute-fn :size (aesthetic/scale-spec :size {:domain ["x"]} nil false (:palette theme/default-theme)) nil) "x")))))

(deftest linear-test
  (testing "size: domain ends to [min max], clamped, midpoint for zero span"
    (let [s (spec (build (plot-of {:size :s})) :size)
          f (aesthetic/attribute-fn :size s nil)]
      (is (= :linear (:kind s)))
      (is (= [10.0 40.0] (:domain s)) "data domain, no nice or padding")
      (is (= [2.0 10.0] [(f 10) (f 40)]))
      (is (= [2.0 10.0] [(f -100) (f 1000)]) "clamped")
      (is (nil? (f "x")) "non-number -> NA"))
    (let [f (aesthetic/attribute-fn :size (spec (build (plot-of {:size :s} {:data [{:x 1 :y 1 :s 7}]})) :size) nil)]
      (is (= 6.0 (f 7)))))
  (testing "color: gradient (k2 reversed) ends and midpoint"
    (let [s (spec (build (plot-of {:color :s})) :color)
          f (aesthetic/attribute-fn :color s nil)
          g (c/gradient (get-in theme/default-theme [:palette :gradient]))]
      (is (= :linear (:kind s)))
      (is (= (g 0.0) (f 10)))
      (is (= (g 1.0) (f 40)))
      (is (= (g 0.5) (f 25)))))
  (testing "explicit range"
    (is (= (c/to-color :red) (c/to-color ((aesthetic/attribute-fn :color (spec (build (plot-of {:color :g} {:scales {:color {:range [:red :blue]}}})) :color) nil) "a"))))
    (is (= [1.0 3.0] (mapv (aesthetic/attribute-fn :size (spec (build (plot-of {:size :s} {:scales {:size {:range [1 3]}}})) :size) nil) [10 40])))
    (is (= 0.25 ((aesthetic/attribute-fn :color (spec (build (plot-of {:color :s} {:scales {:color {:range (fn [t] (* 0.5 t))}}})) :color) nil) 25)))))

(deftest merged-domain-test
  (let [built (build {:mapping {:x :x :y :y :color :g}
                      :charts [(chart/scatter {:data [{:x 1 :y 1 :g "b"} {:x 2 :y 2 :g "a"}]})
                               (chart/scatter {:data [{:x 1 :y 1 :g "c"} {:x 2 :y 2 :g "b"}]})]})]
    (is (= ["a" "b" "c"] (:domain (spec built :color))))))

(deftest scale-validation-test
  (are [scales k] (= k (:key (error-data #(build (plot-of {:color :g :shape :g :size :s} {:scales scales})))))
    {:shape {:range [\O \?]}} :shape
    {:shape {:range []}} :shape
    {:size {:range [:a 3]}} :size
    {:size {:range [5 1]}} :size
    {:size {:range [-1 3]}} :size
    {:color {:range []}} :range
    {:color {:range :no-such-palette}} :range
    {:shape {:kind :linear}} :kind
    {:color {:kind :nominal}} :kind
    {:color {:kind :linear}} :kind
    {:color {:zoom 2}} :scales
    {:color {:formatter "x"}} :formatter
    {:color {:title 3}} :title
    {:color {:legend :yes}} :legend
    {:size {:domain [1 1]}} :domain
    {:size {:domain [0 ##NaN]}} :domain
    {:color {:domain []}} :domain
    {:color {:domain ["a" "a"]}} :domain
    {:color {:domain ["a" nil]}} :domain
    {:color [1 2]} :scales)
  (testing "aesthetic scale without a mapping"
    (is (= {:key :scales :scale :color} (error-data #(build (plot-of {} {:scales {:color {:domain ["a"]}}}))))))
  (testing "incomparable levels"
    (is (= :domain (:key (error-data #(build (plot-of {:color :g} {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g :b}]}))))))))

(deftest finalized-test
  (let [built (build (plot-of {:color :g :size :s}))]
    (is (= (spec built :color) (get-in built [:finalized-scales :color])))
    (is (= (spec built :size) (get-in built [:finalized-scales :size])))
    (is (not (contains? (get-in built [:finalized-scales :size]) :ticks)) "no pixel finalization")))

;; 3.x legends

(deftest legend-merge-test
  (are [mapping n] (= n (count (legends (build (plot-of mapping)))))
    {:color :g :shape :g} 1
    {:color :g :shape :year} 2
    {:color :g :shape :g :size :g} 1
    {:color #(:g %) :shape #(:g %)} 2)
  (let [f #(:g %)]
    (is (= 1 (count (legends (build (plot-of {:color f :shape f})))))))
  (testing "merged entries combine attributes"
    (let [[{:keys [entries title]}] (legends (build (plot-of {:color :g :shape :g})))]
      (is (= "g" title))
      (is (= #{:color :shape} (set (keys (:attributes (first entries))))))
      (is (= \O (get-in (first entries) [:attributes :shape])))))
  (testing "same accessor, different labels -> separate legends"
    (is (= 2 (count (legends (build (plot-of {:color :g :shape :g} {:scales {:shape {:domain ["c" "b" "a"]}}}))))))))

(deftest legend-title-labels-test
  (are [scales title] (= title (:title (first (legends (build (plot-of {:color :g} {:scales scales}))))))
    {} "g"
    {:color {:title "Group"}} "Group"
    {:color {:title nil}} nil)
  (is (nil? (:title (first (legends (build (plot-of {:color #(:g %)})))))))
  (is (= ["<a>" "<b>" "<c>"] (mapv :label (:entries (first (legends (build (plot-of {:color :g} {:scales {:color {:formatter #(str "<" % ">")}}})))))))))

(deftest legend-charts-test
  (testing "every chart mapping the dimension draws its glyph (overlay), opted-out charts don't"
    (let [built (build {:data rows :mapping {:x :x :y :y :color :g}
                        :charts [(chart/scatter {:shape \s}) (chart/scatter {:mapping {:color nil}}) (chart/scatter {:shape \x})]})]
      (is (= [0 2] (:charts (first (legends built)))))))
  (testing "overlay renders both glyphs into a key"
    (let [p {:data rows :mapping {:x :x :y :y :color :g} :theme plain
             :charts [(chart/scatter {:shape \S :size 14 :color :black}) (chart/scatter {:shape \O :size 4})]}
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {lx :x ly :y} (grid/find-item-box (:layout built) [:legend 0])
          title-h (long (.y (measured {:kind :label :role :legend-title :text "g"} (:theme built))))
          key-cx (+ (long lx) 7)
          key-cy (+ (long ly) title-h 7)]
      (is (= (rgb (category20 0)) (pixel c key-cx key-cy)) "small mapped dot on top")
      (is (not= [255 255 255] (pixel c (- key-cx 5) (- key-cy 5))) "square under it"))))

(deftest legend-kinds-test
  (testing "linear color: no legend, no legend track"
    (let [built (build (plot-of {:color :s}))]
      (is (nil? (legends built)))
      (is (not-any? #(= :legend (:id %)) (get-in built [:grid :columns])))))
  (testing "linear size: entries at domain ticks, sizes increasing"
    (let [[{:keys [entries]}] (legends (build (plot-of {:size :s} {:scales {:size {:domain [0 100]}}})))]
      (is (= ["0" "20" "40" "60" "80" "100"] (mapv :label entries)))
      (is (apply < (map #(get-in % [:attributes :size]) entries)))))
  (testing "single-value size domain: one entry"
    (is (= 1 (count (:entries (first (legends (build (plot-of {:size :s} {:data [{:x 1 :y 1 :s 5}]}))))))))))

(deftest legend-switch-test
  (is (nil? (legends (build (plot-of {:color :g} {:legend false})))))
  (is (nil? (legends (build (plot-of {:color :g} {:scales {:color {:legend false}}})))))
  (is (= 1 (count (legends (build (plot-of {:color :g :shape :year} {:scales {:shape {:legend false}}}))))))
  (is (= :legend (:key (error-data #(build (plot-of {:color :g} {:legend :no})))))))

(deftest legend-geometry-test
  (let [size-of (fn [mapping extra]
                  (let [built (build (plot-of mapping extra))]
                    (measured (first (legends built)) (:theme built))))
        label-w (fn [built text] (.x (measured {:kind :label :role :legend-label :text text} (:theme built))))
        built (build (plot-of {:color :g} {:scales {:color {:title nil :formatter #(str % "-long-label")}}}))
        {:keys [gap key-size]} (get-in built [:theme :legend])]
    (is (>= (.x ^fastmath.vector.Vec2 (size-of {:color :g} {:scales {:color {:title nil :formatter #(str % "-long-label")}}}))
            (+ (double key-size) (double gap) (label-w built "a-long-label"))))
    (let [h (fn [n] (.y ^fastmath.vector.Vec2 (size-of {:color :g} {:scales {:color {:kind :ordinal :domain (vec (range n))}}
                                                                    :data (map #(assoc %1 :g %2) rows (cycle (range n)))})))]
      (is (= (- (h 3) (h 2)) (- (h 4) (h 3))) "height linear in entry count"))))

(deftest legend-placement-test
  (testing "single panel: legend right of the panel, top-aligned"
    (let [built (build (plot-of {:color :g}))
          panel (grid/item-box (:layout built) :panel)
          lg (grid/find-item-box (:layout built) [:legend 0])]
      (is (>= (:x lg) (+ (:x panel) (:w panel))))
      (is (= (:y lg) (:y panel)))))
  (testing "facets: right of every panel and strip; stacked legends"
    (let [built (build (plot-of {:color :g :shape :year} {:mapping {:x :x :y :y :color :g :shape :year :facet :g}}))
          right (reduce max (keep (fn [{:keys [id box]}]
                                    (when (and (vector? id) (#{:panel :strip} (first id))) (+ (:x box) (:w box))))
                                  (get-in built [:layout :items])))
          l0 (grid/find-item-box (:layout built) [:legend 0])
          l1 (grid/find-item-box (:layout built) [:legend 1])
          top-strip (grid/item-box (:layout built) [:strip 0 0])]
      (is (>= (:x l0) right))
      (is (= (:y l0) (:y top-strip)))
      (is (= (:y l1) (+ (:y l0) (:h l0) (get-in built [:theme :legend :gap]))))))
  (testing "nothing mapped: no legend tracks"
    (is (not-any? #(#{:legend :legend-gap} (:id %)) (get-in (build (plot-of {})) [:grid :columns]))))
  (testing "legend ink right of the panel"
    (let [p (plot-of {:color :g} {:theme plain})
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {:keys [x y w h]} (grid/find-item-box (:layout built) [:legend 0])]
      (is (some (fn [[px py]] (not= [255 255 255] (pixel c px py)))
                (for [px (range x (+ x w)) py (range y (+ y h))] [px py]))))))

(deftest legend-theme-test
  (testing ":fill fills the legend box"
    (let [p (plot-of {:color :g} {:theme (assoc plain :legend {:fill [255 0 0]})})
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {:keys [x y w h]} (grid/find-item-box (:layout built) [:legend 0])]
      (is (= [255 0 0] (pixel c (+ x w -1) (+ y h -1))))))
  (testing "validated at build time"
    (are [t k] (= k (:key (error-data #(build (plot-of {} {:theme t})))))
      {:legend {:gap -1}} :gap
      {:legend {:key-size "x"}} :key-size
      {:legend {:border-width -2}} :border-width
      {:palette {:color :no-such-palette}} :palette
      {:palette {:gradient []}} :palette
      {:palette {:shape [\?]}} :shape
      {:palette {:size [3 1]}} :size
      {:palette {:na :not-a-color}} :na
      {:panel {:grid {:bands {:width -1}}}} :width)))

;; 4.x style

(deftest style-override-test
  (testing "mapped color keeps the chart's fixed shape"
    (let [built (build (plot-of {:color :g} {:charts [(chart/scatter {:shape \s})]}))
          d (panel-data built)]
      (is (= \s (get-in d [:style :shape])))
      (is (not (contains? d :shape)))
      (is (= #{:color} (set (keys (:attributes (first (:entries (first (legends built))))))))))))

;; 5.x bands

(deftest bands-grid-test
  (let [varieties (mapv #(str "v" %) (range 10))
        data (for [v varieties i (range 2)] {:yield (+ 20 i (count v)) :variety v})
        p {:data data :mapping {:x :yield :y :variety} :charts [(chart/scatter)]
           :theme (assoc-in plain [:panel :grid :bands] {:color [255 0 0] :width 1 :dash :none})}
        built (build p)
        c (plot/render p {:width 600 :height 400})
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)]
    (testing "a tick (grid line) at every band"
      (is (= (sort varieties) (sort (get-in built [:finalized-scales :y :ticks])))))
    (testing "bands lines use [:panel :grid :bands]"
      (is (= 10 (count (filter #(= [255 0 0] (pixel c (+ x 2) %)) (range y (+ y h)))))))
    (testing "numeric x keeps its own grid style (none here)"
      (is (not-any? #(= [255 0 0] (pixel c % (+ y 1))) (range x (+ x w)))))))
