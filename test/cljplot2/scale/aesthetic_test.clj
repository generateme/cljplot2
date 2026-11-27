(ns cljplot2.scale.aesthetic-test
  "Step 7: aesthetics (color, shape, size), their scales, legends, bands grid lines."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.content.axis :as axis]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.content.legend :as legend]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

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

(defn- legends-item [built] (first (filter #(= :legends (:id %)) (:items (:grid built)))))

(defn- legends [built] (some-> (legends-item built) :content :legends))

(defn- rgb [col] (vec (take 3 (map long (c/to-color col)))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(defn- measured
  "Natural size of `content` with `theme`, measured on a scratch canvas (optional item `box`)."
  (^fastmath.vector.Vec2 [content theme] (measured content theme nil))
  (^fastmath.vector.Vec2 [content theme box]
   (measure/call-with-scratch-canvas :high #(measure/natural-size content {:canvas % :theme theme :box box}))))

(defn- legend-boxes
  "Canvas boxes of the plot's legends (final layout)."
  [built]
  (let [box (grid/item-box (:layout built) :legends)]
    (measure/call-with-scratch-canvas :high #(legend/legend-boxes (:content (legends-item built)) {:canvas % :theme (:theme built)} box))))

;; 1.x mapping

(deftest mapping-test
  (testing "mapped aesthetic: per-point vectors aligned with x / y"
    (let [d (panel-data (build (plot-of {:color :year :size :s :shape :g})))]
      (is (= [1932 1931 1932 1931] (:color d)))
      (is (= ["b" "a" "b" "c"] (:shape d)))
      (is (= (count (:x d)) (count (:color d)) (count (:size d)) (count (:shape d))))))
  (testing "constant in the mapping raises"
    (is (= {:key :color :in [:mapping :color] :value [255 0 0]} (error #(build (plot-of {:color [255 0 0]})))))
    (is (= {:key :size :in [:mapping :size] :value 5} (error #(build (plot-of {:size 5}))))))
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
        (is (true? (:na-level (spec built :color))))
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
            px (+ (long x) (long (scale/forward fx 0)))
            py (+ (long y) (long (scale/forward fy 0)))]
        (is (= (rgb (get-in theme/default-theme [:palette :na])) (pixel c px py)))))
    (testing ":groups {:na false} drops and counts"
      (let [built (build (plot-of {:color :g} {:data data :groups {:na false}}))]
        (is (= {:x 0 :y 0 :color 1} (get-in built [:stats 0 :dropped])))
        (is (false? (:na-level (spec built :color))))
        (is (= ["a" "b" "c"] (mapv :label (:entries (first (legends built))))))))
    (testing "NaN / infinite dropped and counted (NA on)"
      (let [built (build (plot-of {:size :s} {:data (conj rows {:x 6 :y 6 :s ##NaN} {:x 7 :y 7 :s ##Inf})}))]
        (is (= 2 (get-in built [:stats 0 :dropped :size])))
        (is (= [10 20 30 40] (:size (panel-data built))))))
    (testing "all values NA -> misspelt mapping error"
      (is (= {:key :mapping :in [:mapping :color] :rows 4} (error #(build (plot-of {:color :yaer}))))))
    (testing "empty data: no error, no scale, no legend"
      (let [built (build (plot-of {:color :g} {:data []}))]
        (is (nil? (spec built :color)))
        (is (nil? (legends built)))))
    (testing "invalid :groups"
      (are [g in] (= in (:in (error #(build (plot-of {:color :g} {:groups g})))))
        {:na 1} [:groups :na]
        [:na] [:groups]
        true [:groups]
        {:na true :drop 1} [:groups :drop]))))

;; 2.x scales

(def ^:private default-colors
  "The default theme's categorical palette."
  (get-in theme/default-theme [:palette :color]))

(deftest ordinal-test
  (testing "numbers forced ordinal: sorted levels, the default palette's colors"
    (let [s (spec (build (plot-of {:color :year} {:scales {:color {:kind :ordinal}}})) :color)
          f (aesthetic/attribute-fn :color s :na)]
      (is (= [1931 1932] (:domain s)))
      (is (= (c/format-hex (default-colors 0)) (c/format-hex (f 1931))))
      (is (= (c/format-hex (default-colors 1)) (c/format-hex (f 1932))))
      (is (nil? (f 1999)) "value outside the domain -> nil (skipped)")
      (is (= :na (f nil)) "NA")))
  (testing "numbers match as doubles: long data, double domain"
    (let [built (build (plot-of {:color :year} {:scales {:color {:kind :ordinal :domain [1931.0 1932.0]}}}))
          f (aesthetic/attribute-fn :color (spec built :color) :na)]
      (is (false? (:na-level (spec built :color))))
      (is (= (default-colors 0) (f 1931)))
      (is (= (default-colors 1) (f 1932.0))))
    (is (= :domain (:key (error #(build (plot-of {:color :year} {:scales {:color {:kind :ordinal :domain [1931 1931.0]}}})))))
        "1931 and 1931.0 are one level")
    (is (= [1931 1932] (:domain (spec (build (plot-of {:color :year} {:data (conj rows {:x 9 :y 9 :year 1931.0}) :scales {:color {:kind :ordinal}}})) :color)))))
  (testing "explicit numeric domain: two values linear, one or three+ levels"
    (are [domain kind] (= kind (:kind (spec (build (plot-of {:color :year} {:scales {:color {:domain domain}}})) :color)))
      [1900 2000] :linear
      [1931] :ordinal
      [1931 1932 1933] :ordinal
      '(1900 2000) :linear))
  (testing "strings sorted; explicit domain keeps its order"
    (is (= ["a" "b" "c"] (:domain (spec (build (plot-of {:color :g})) :color))))
    (is (= ["c" "b" "a"] (:domain (spec (build (plot-of {:color :g} {:scales {:color {:domain ["c" "b" "a"]}}})) :color)))))
  (testing "values outside an explicit domain: skipped, no NA level"
    (let [p (plot-of {:color :g} {:scales {:color {:domain ["a" "b"]}} :theme plain
                                  :data [{:x 0 :y 0 :g "c"} {:x 1 :y 1 :g "a"}] :charts [(chart/scatter {:size 20})]})
          built (build p)
          s (spec built :color)
          {:keys [x y]} (grid/item-box (:layout built) :panel)
          c (plot/render p {:width 600 :height 400})]
      (is (false? (:na-level s)))
      (is (nil? ((aesthetic/attribute-fn :color s :na) "c")))
      (is (= ["a" "b"] (mapv :label (:entries (first (legends built))))))
      (is (= 1 (get-in built [:stats 0 :dropped :color])) "point outside the domain dropped and counted")
      (is (= {:kind :numeric :min 1.0 :max 1.0} (get-in built [:extents :x :extent])) "extents only cover kept points")
      (is (some? c))))
  (testing "one level; more levels than colors cycle"
    (let [one (aesthetic/scale-spec :color {:domain ["x"]} nil false (:palette theme/default-theme))
          many (aesthetic/scale-spec :color {:kind :ordinal :domain (vec (range 25))} nil false (:palette theme/default-theme))
          f (aesthetic/attribute-fn :color many nil)]
      (is (= (default-colors 0) ((aesthetic/attribute-fn :color one nil) "x")))
      (is (= (f 0) (f (count default-colors))))
      (is (= (f 4) (f (+ 4 (count default-colors)))))))
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
  (testing "size: domain ends to [min max], outside skipped, midpoint for zero span"
    (let [s (spec (build (plot-of {:size :s})) :size)
          f (aesthetic/attribute-fn :size s nil)]
      (is (= :linear (:kind s)))
      (is (= [10.0 40.0] (:domain s)) "data domain, no nice or padding")
      (is (= [2.0 10.0] [(f 10) (f 40)]))
      (is (= [nil nil] [(f 9.999) (f 40.001)]) "outside [lo hi] -> nil (skipped)")
      (is (nil? (f "x")) "non-number -> nil")
      (is (= :na ((aesthetic/attribute-fn :size s :na) nil)) "NA"))
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
    (is (= [2.0 6.0 10.0] (mapv (aesthetic/attribute-fn :size (spec (build (plot-of {:size :s} {:scales {:size {:domain '(0 100)}}})) :size) nil) [0 50 100]))
        "linear :domain as any sequence")
    (is (= 0.25 ((aesthetic/attribute-fn :color (spec (build (plot-of {:color :s} {:scales {:color {:range (fn [t] (* 0.5 t))}}})) :color) nil) 25)))))

(deftest merged-domain-test
  (let [built (build {:mapping {:x :x :y :y :color :g}
                      :charts [(chart/scatter {:data [{:x 1 :y 1 :g "b"} {:x 2 :y 2 :g "a"}]})
                               (chart/scatter {:data [{:x 1 :y 1 :g "c"} {:x 2 :y 2 :g "b"}]})]})]
    (is (= ["a" "b" "c"] (:domain (spec built :color))))))

(deftest scale-validation-test
  (are [scales k] (= k (:key (error #(build (plot-of {:color :g :shape :g :size :s} {:scales scales})))))
    {:shape {:range [\O \?]}} :range
    {:shape {:range []}} :range
    {:size {:range [:a 3]}} :range
    {:size {:range [5 1]}} :range
    {:size {:range [-1 3]}} :range
    {:color {:range []}} :range
    {:color {:range :no-such-palette}} :range
    {:shape {:kind :linear}} :kind
    {:color {:kind :nominal}} :kind
    {:color {:kind :linear}} :kind
    {:color {:zoom 2}} :zoom
    {:color {:range (fn [t] t)}} :range
    {:color {:formatter "x"}} :formatter
    {:color {:title 3}} :title
    {:color {:legend :yes}} :legend
    {:size {:domain [1 1]}} :domain
    {:size {:domain [2 1]}} :domain
    {:size {:domain [0 ##NaN]}} :domain
    {:color {:domain []}} :domain
    {:color {:domain ["a" "a"]}} :domain
    {:color {:domain ["a" nil]}} :domain
    {:color [1 2]} :color)
  (testing "aesthetic scale without a mapping"
    (is (= {:key :color :in [:scales :color] :value {:domain ["a"]}}
           (error #(build (plot-of {} {:scales {:color {:domain ["a"]}}}))))))
  (testing "incomparable levels"
    (is (= :domain (:key (error #(build (plot-of {:color :g} {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g :b}]}))))))))

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
    {:color {:title nil}} "g"
    {:color {:title :none}} nil)
  (is (nil? (:title (first (legends (build (plot-of {:color #(:g %)})))))))
  (is (= ["<a>" "<b>" "<c>"] (mapv :label (:entries (first (legends (build (plot-of {:color :g} {:scales {:color {:formatter #(str "<" % ">")}}})))))))))

(deftest legend-charts-test
  (testing "every chart mapping the dimension draws its glyph (overlay), opted-out charts don't"
    (let [built (build {:data rows :mapping {:x :x :y :y :color :g}
                        :charts [(chart/scatter {:shape \s}) (chart/scatter {:mapping {:color nil}}) (chart/scatter {:shape \x})]})]
      (is (= [{:chart 0 :dims [:color]} {:chart 2 :dims [:color]}] (:charts (first (legends built)))))))
  (testing "merged legend: each chart gets only the dimensions it maps"
    (let [built (build {:data rows :mapping {:x :x :y :y}
                        :charts [(chart/scatter {:shape \s :mapping {:color :g}}) (chart/scatter {:mapping {:shape :g}})]})]
      (is (= [{:chart 0 :dims [:color]} {:chart 1 :dims [:shape]}] (:charts (first (legends built)))))))
  (testing "overlay renders both glyphs into a key"
    (let [p {:data rows :mapping {:x :x :y :y :color :g} :theme plain
             :charts [(chart/scatter {:shape \S :size 14 :color :black}) (chart/scatter {:shape \O :size 4})]}
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {lx :x ly :y} (first (legend-boxes built))
          title-h (long (.y (measured {:kind :label :role :legend-title :text "g"} (:theme built))))
          key-cx (+ (long lx) 7)
          key-cy (+ (long ly) title-h 7)]
      (is (= (rgb (default-colors 0)) (pixel c key-cx key-cy)) "small mapped dot on top")
      (is (not= [255 255 255] (pixel c (- key-cx 5) (- key-cy 5))) "square under it"))))

(deftest legend-kinds-test
  (testing "linear color: a color bar (step 11)"
    (let [built (build (plot-of {:color :s}))]
      (is (= [:colorbar] (map :kind (legends built))))
      (is (some #(= :legend (:id %)) (get-in built [:grid :columns])))))
  (testing "linear size: entries at domain ticks, sizes increasing"
    (let [[{:keys [entries]}] (legends (build (plot-of {:size :s} {:scales {:size {:domain [0 100]}}})))]
      (is (= ["0" "20" "40" "60" "80" "100"] (mapv :label entries)))
      (is (apply < (map #(get-in % [:attributes :size]) entries)))))
  (testing "single-value size domain: one entry, integer label"
    (is (= ["5"] (mapv :label (:entries (first (legends (build (plot-of {:size :s} {:data [{:x 1 :y 1 :s 5}]}))))))))
    (is (= ["5.5"] (mapv :label (:entries (first (legends (build (plot-of {:size :s} {:data [{:x 1 :y 1 :s 5.5}]})))))))))
  (testing "more than max-levels distinct numbers"
    (let [many (mapv (fn [i] {:x i :y i :v i}) (range 101))
          exact (mapv (fn [i] {:x i :y i :v i}) (range 100))]
      (is (= {:key :domain :in [:scales :shape :domain]} (error #(build (plot-of {:shape :v} {:data many}))))
          "ordinal: explicit domain needed")
      (is (= 100 (count (:domain (spec (build (plot-of {:shape :v} {:data exact})) :shape)))) "100 levels allowed")
      (is (= [0 1] (:domain (spec (build (plot-of {:shape :v} {:data many :scales {:shape {:domain [0 1]}}})) :shape)))
          "explicit domain: rest skipped")
      (is (= :linear (:kind (spec (build (plot-of {:size :v} {:data many})) :size))) "linear needs no levels")
      (is (nil? (get-in (build (plot-of {:size :v} {:data many})) [:extents :size :extent :values])) "values not kept")))
  (testing "merged extents capped across charts"
    (let [half (fn [from] (chart/scatter {:data (mapv (fn [i] {:x i :y i :v i}) (range from (+ from 60)))}))]
      (is (= {:key :domain :in [:scales :shape :domain]}
             (error #(build {:mapping {:x :x :y :y :shape :v} :charts [(half 0) (half 60)]})))))))

(deftest legend-switch-test
  (is (nil? (legends (build (plot-of {:color :g} {:legend false})))))
  (is (nil? (legends (build (plot-of {:color :g} {:scales {:color {:legend false}}})))))
  (is (= 1 (count (legends (build (plot-of {:color :g :shape :year} {:scales {:shape {:legend false}}}))))))
  (is (= :legend (:key (error #(build (plot-of {:color :g} {:legend :no})))))))

(deftest legend-geometry-test
  (let [size-of (fn [mapping extra]
                  (let [built (build (plot-of mapping extra))]
                    (measured (first (legends built)) (:theme built))))
        label-w (fn [built text] (.x (measured {:kind :label :role :legend-label :text text} (:theme built))))
        built (build (plot-of {:color :g} {:scales {:color {:title :none :formatter #(str % "-long-label")}}}))
        {:keys [gap key-size]} (get-in built [:theme :legend])]
    (is (>= (.x ^fastmath.vector.Vec2 (size-of {:color :g} {:scales {:color {:title :none :formatter #(str % "-long-label")}}}))
            (+ (double key-size) (double gap) (label-w built "a-long-label"))))
    (let [h (fn [n] (.y ^fastmath.vector.Vec2 (size-of {:color :g} {:scales {:color {:kind :ordinal :domain (vec (range n))}}
                                                                    :data (map #(assoc %1 :g %2) rows (cycle (range n)))})))]
      (is (= (- (h 3) (h 2)) (- (h 4) (h 3))) "height linear in entry count"))))

(deftest legend-placement-test
  (testing "single panel: legend right of the panel and the x overhang, top-aligned"
    (let [built (build (plot-of {:color :g}))
          panel (grid/item-box (:layout built) :panel)
          lg (first (legend-boxes built))
          cols (mapv :id (get-in built [:grid :columns]))]
      (is (>= (:x lg) (+ (:x panel) (:w panel))))
      (is (= (double (:y lg)) (double (:y panel))))
      (is (= [:x-overhang :legend-gap :legend :margin-right] (subvec cols (- (count cols) 4))))))
  (testing "facets: right of every panel and strip; stacked legends"
    (let [built (build (plot-of {:color :g :shape :year} {:mapping {:x :x :y :y :color :g :shape :year :facet :g}}))
          right (reduce max (keep (fn [{:keys [id box]}]
                                    (when (and (vector? id) (#{:panel :strip} (first id))) (+ (:x box) (:w box))))
                                  (get-in built [:layout :items])))
          [l0 l1] (legend-boxes built)
          top-strip (grid/item-box (:layout built) [:strip 0 0])]
      (is (>= (:x l0) right))
      (is (= (double (:y l0)) (double (:y top-strip))))
      (is (= (double (:y l1)) (+ (double (:y l0)) (double (:h l0)) (double (get-in built [:theme :legend :gap])))))))
  (testing "nothing mapped: no legend tracks"
    (is (not-any? #(#{:legend :legend-gap} (:id %)) (get-in (build (plot-of {})) [:grid :columns]))))
  (testing "legend ink right of the panel"
    (let [p (plot-of {:color :g} {:theme plain})
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {:keys [x y w h]} (first (legend-boxes built))]
      (is (some (fn [[px py]] (not= [255 255 255] (pixel c px py)))
                (for [px (range x (+ x w)) py (range y (+ y h))] [px py]))))))

(deftest legend-theme-test
  (testing ":fill fills the legend box"
    (let [p (plot-of {:color :g} {:theme (assoc plain :legend {:fill [255 0 0]})})
          built (build p)
          c (plot/render p {:width 600 :height 400})
          {:keys [x y w h]} (first (legend-boxes built))]
      (is (= [255 0 0] (pixel c (+ x w -1) (+ y h -1))))))
  (testing "validated at build time"
    (are [t k] (= k (:key (error #(build (plot-of {} {:theme t})))))
      {:legend {:gap -1}} :gap
      {:legend {:key-size "x"}} :key-size
      {:legend {:border-width -2}} :border-width
      {:palette {:color :no-such-palette}} :color
      {:palette {:gradient []}} :gradient
      {:palette {:shape [\?]}} :shape
      {:palette {:size [3 1]}} :size
      {:palette {:na :not-a-color}} :na
      {:panel {:grid {:bands {:line-width -1}}}} :line-width)))

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
           :theme (-> plain
                      (assoc-in [:panel :grid :y] {:color [0 0 255] :line-width 1 :dash :none})
                      (assoc-in [:panel :grid :bands] {:color [255 0 0] :line-width 1 :dash :none}))}
        built (build p)
        c (plot/render p {:width 600 :height 400})
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)]
    (testing "a tick (grid line) at every band"
      (is (= (sort varieties) (sort (get-in built [:finalized-scales :y :ticks])))))
    (testing "bands lines use [:panel :grid :bands]"
      (is (= 10 (count (filter #(= [255 0 0] (pixel c (+ x 2) %)) (range y (+ y h)))))))
    (testing "numeric x keeps its own grid style (none here)"
      (is (not-any? #(= [255 0 0] (pixel c % (+ y 1))) (range x (+ x w)))))))

;; review fixes

(deftest legend-wrap-test
  (let [data (fn [n] (mapv (fn [i] {:x i :y i :g (format "level-%02d" i)}) (range n)))]
    (testing "too many entries for the panel height: wraps into columns, no error"
      (let [built (build (plot-of {:color :g} {:data (data 60)}) {:width 600 :height 300})
            [lg] (legend-boxes built)
            item (grid/item-box (:layout built) :legends)
            entry-h (.y (measured {:kind :label :role :legend-label :text "level-00"} (:theme built)))]
        (is (<= (:h lg) (:h item)) "legend fits the available height")
        (is (> (:w lg) (* 2.0 (.x (measured {:kind :label :role :legend-label :text "level-00"} (:theme built)))))
            "more than one column")
        (is (some? (plot/render (plot-of {:color :g} {:data (data 60)}) {:width 600 :height 300})))
        (is (< entry-h (:h item)))))
    (testing "entries that fit: one column (width independent of count)"
      (let [w (fn [n] (:w (first (legend-boxes (build (plot-of {:color :g} {:data (data n)}))))))]
        (is (= (w 3) (w 5)))))
    (testing "one entry taller than the space: one row per column, still no error"
      (is (some? (build (plot-of {:color :g} {:data (data 3) :labels {:x :none :y :none}}) {:width 600 :height 60}))))
    (testing "stacked legends overflowing start a new stack column"
      (let [built (build (plot-of {:color :g :shape :year} {:data (mapv #(assoc % :year (mod (:x %) 2)) (data 15))
                                                            :scales {:shape {:kind :ordinal}}})
                         {:width 600 :height 260})
            [l0 l1] (legend-boxes built)]
        (is (> (:x l1) (:x l0)))
        (is (= (double (:y l1)) (double (:y l0))))))
    (testing "unbounded measurement (no box): single column"
      (let [content {:kind :legend :title "t" :charts []
                     :entries (mapv (fn [i] {:label (str i) :attributes {}}) (range 50))}
            theme (:theme (build {}))]
        (is (= (.x (measured content theme)) (.x (measured content theme {:x 0 :y 0 :w 100 :h 1e6}))))
        (is (< (.x (measured content theme)) (.x (measured content theme {:x 0 :y 0 :w 100 :h 100}))))))))

(deftest legend-key-size-test
  (let [theme (:theme (build {}))
        content (fn [sizes] {:kind :legend :title nil :charts []
                             :entries (mapv (fn [s] {:label "a" :attributes {:size s}}) sizes)})
        h (fn [sizes] (.y (measured (content sizes) theme)))
        label-h (.y (measured {:kind :label :role :legend-label :text "a"} theme))
        key-size (double (get-in theme [:legend :key-size]))
        row (max key-size label-h)]
    (is (= (* 2 row) (h [1 2])) "small glyphs: key-size rows")
    (is (= (+ row 40.0) (h [1 40])) "a large glyph grows only its own row")
    (is (= (+ key-size 5.0 (.x (measured {:kind :label :role :legend-label :text "a"} theme)))
           (.x (measured (content [1]) theme))))
    (is (= (+ 40.0 5.0 (.x (measured {:kind :label :role :legend-label :text "a"} theme)))
           (.x (measured (content [1 40]) theme))) "key column as wide as the largest glyph")))

(deftest bands-grid-none-test
  (let [p {:data [{:x 1 :y "a"} {:x 2 :y "b"}] :mapping {:x :x :y :y} :charts [(chart/scatter {:size 0})]
           :theme {:plot {:background :white} :panel {:grid {:x :none :y :none :bands {:color [255 0 0] :dash :none}}}}}
        built (build p)
        c (plot/render p {:width 600 :height 400})
        {:keys [x y h]} (grid/item-box (:layout built) :panel)]
    (is (not-any? #(= [255 0 0] (pixel c (+ x 2) %)) (range y (+ y h))) ":none on :y wins over :bands")))

(deftest band-label-thinning-test
  (let [domain (mapv #(format "category-%02d" %) (range 60))
        step-of (fn [height]
                  (let [built (build {:scales {:y {:domain domain}}} {:width 400 :height height})
                        fy (get-in built [:finalized-scales :y])
                        sizes (measure/call-with-scratch-canvas
                               :high (fn [cv] (mapv #(measure/natural-size {:kind :label :role :tick-label :text %}
                                                                           {:canvas cv :theme (:theme built)})
                                                    (:tick-labels fy))))]
                    [(count (:ticks fy)) (axis/label-step :y fy sizes 2)]))]
    (testing "every band keeps its tick"
      (is (= 60 (first (step-of 300)))))
    (testing "crowded labels thinned, roomy ones not"
      (is (> (long (second (step-of 300))) 1))
      (is (= 1 (second (step-of 3000)))))
    (testing "continuous and short axes untouched"
      (is (= 1 (axis/label-step :x {:kind :linear} [] 2)))
      (is (= 1 (axis/label-step :y {:kind :bands :ticks ["a"]} [(v/vec2 10 10)] 2))))))

(deftest mapped-draw-timing-test
  (let [data (vec (for [i (range 10000)] {:a (mod (* i 7919) 1000) :b (mod (* i 104729) 997)
                                          :g (str "g" (mod i 7)) :s (mod i 50)}))
        p {:data data :mapping {:x :a :y :b :color :g :shape :g :size :s} :charts [(chart/scatter)]}
        _ (plot/render p {:width 600 :height 400})
        t0 (System/nanoTime)
        _ (plot/render p {:width 600 :height 400})
        ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (< ms 1500.0) (str "10k mapped points took " ms " ms"))))

(deftest label-step-threshold-test
  (let [fy (scale/finalize (scale/scale-spec :y {:domain ["a" "b" "c" "d"]}) [400.0 0.0] 10)
        spacing (Math/abs (- (scale/forward fy "a") (scale/forward fy "b")))
        sizes (fn [block] (vec (repeat 4 (v/vec2 30 (+ block 4.0)))))]
    (is (= 1 (axis/label-step :y fy (sizes spacing) 2)) "text blocks touching: no thinning")
    (is (= 1 (axis/label-step :y fy (sizes (+ spacing 1.0)) 2)) "1 px overlap tolerated (rounding)")
    (is (= 2 (axis/label-step :y fy (sizes (+ spacing 1.5)) 2)) "more overlap: every 2nd label")
    (is (= 4 (axis/label-step :y fy (sizes 10000.0) 2)) "nothing fits: first label only")))
