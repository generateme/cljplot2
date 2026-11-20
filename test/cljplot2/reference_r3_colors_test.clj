(ns cljplot2.reference-r3-colors-test
  "Reference part R3 - colours and dates: text role colours (`:contrast`), colour search functions, colour
  transforms, `java.time` values (and `Year` / `YearMonth`) on colour and position scales, date colour bar ticks,
  legend texts."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.content.legend :as legend]
            [cljplot2.data.levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme]
            [cljplot2.test-util :refer [error-data same-pixels?]])
  (:import [java.awt.image BufferedImage]))

(def ^:private small-plot {:data [{:x 1 :y 1} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})

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

(deftest throwing-transform-test
  (testing "a transform function throwing at a domain end: the keyed error, not the raw exception"
    (is (= :transform (:key (error-data #(plot/build {:data [{:x 1 :y 0} {:x 2 :y 3}] :mapping {:x :x :y :y :color :y}
                                                       :charts [(chart/scatter)]
                                                       :scales {:color {:transform (fn [v] (if (zero? (double v)) (throw (ex-info "no" {})) v))}}})))))))

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

(deftest year-values-test
  (let [years (for [y (range 2000 2012)] (java.time.Year/of y))
        months (for [m (range 1 13)] (java.time.YearMonth/of 2024 m))
        o {:width 300 :height 200}]
    (testing "continuous scales place a Year / YearMonth at its period's start - a datetime x, a date gradient"
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

(deftest date-colour-charts-test
  (testing "charts colouring along their marks take date colour values: line gradients, segments, polygons"
    (let [dates (for [m (range 12)] (.plusMonths (java.time.LocalDate/of 2024 1 1) m))
          rows (map-indexed (fn [i d] {:x i :y (mod (* i 5) 7) :xend (inc i) :yend (mod (* i 3) 7) :d d}) dates)
          o {:width 300 :height 200}]
      (doseq [c [(chart/line) (chart/segment) (chart/polygon)]]
        (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :x :y :y :xend :xend :yend :yend :color :d}
                                                          :charts [c]} o))
            (str (:kind c)))))))

(deftest marked-date-levels-test
  (let [rows (for [y (range 2000 2006) i (range 3)] {:year (java.time.Year/of y) :d (java.time.LocalDate/of y 3 1) :v (+ i (mod y 7))})
        o {:width 400 :height 250}
        build (fn [mapping chart] (plot/build {:data rows :mapping mapping :charts [chart]} o))]
    (testing "dates marked categorical are strip levels: one band per date, in time order, labelled as their text"
      (let [b (build {:x (cljplot2.data.levels/categorical :year) :y :v} (chart/bar {:stat :sum}))]
        (is (= :bands (get-in b [:scale-specs :x :kind])))
        (is (= (mapv #(java.time.Year/of %) (range 2000 2006)) (get-in b [:scale-specs :x :domain])))
        (is (= (mapv str (range 2000 2006)) (get-in b [:finalized-scales :x :tick-labels]))))
      (is (= :bands (get-in (build {:y (cljplot2.data.levels/categorical :d) :x :v} (chart/box)) [:scale-specs :y :kind])) "on y, LocalDate")
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x (cljplot2.data.levels/categorical :d) :y :v}
                                                       :charts [(chart/box)]} o))))
    (testing "boundary: unmarked dates aren't levels - the error names dates and the marker"
      (let [message (:message (first (:errors (error-data #(build {:x :year :y :v} (chart/bar {:stat :sum}))))))]
        (is (re-find #"numbers or dates declared categorical" message))))))

(deftest explicit-year-color-domain-test
  (testing "Year data with or without an explicit colour :domain / :ticks of Years: the formatter gets Years (the data's type)"
    (let [rows (for [y (range 2000 2010)] {:x y :y y :t (java.time.Year/of y)})
          labels (fn [scale] (:labels (first (filter #(= :colorbar (:kind %))
                                                      (legend/legends (plot/build {:data rows :mapping {:x :x :y :y :color :t}
                                                                                   :charts [(chart/scatter)]
                                                                                   :scales {:color (merge {:formatter #(.getName (class %))} scale)}}
                                                                                  {:width 300 :height 200 :until :scale-specs}))))))]
      (is (every? #{"java.time.Year"} (labels {})))
      (is (every? #{"java.time.Year"} (labels {:domain [(java.time.Year/of 2000) (java.time.Year/of 2009)]})))
      (is (every? #{"java.time.Year"} (labels {:ticks [(java.time.Year/of 2002) (java.time.Year/of 2005)]}))))))

(deftest year-tick-labels-test
  (testing "a datetime x over Years labels whole years"
    (let [rows (for [y (range 2000 2012)] {:t (java.time.Year/of y) :y y})
          labels (get-in (plot/build {:data rows :mapping {:x :t :y :y} :charts [(chart/scatter)]} {:width 500 :height 200})
                         [:finalized-scales :x :tick-labels])]
      (is (seq labels))
      (is (every? #(re-matches #"\d{4}" %) labels) (pr-str labels)))))

(deftest categorical-years-test
  (let [rows (for [y (range 2000 2004)] {:year (java.time.Year/of y) :v (mod y 5)})
        o {:width 300 :height 200}
        x-labels #(get-in (plot/build (merge {:data rows :charts [(chart/bar {:stat :sum})]} %) o) [:finalized-scales :x :tick-labels])
        color-domain #(get-in (plot/build (merge {:data rows :charts [(chart/scatter)]} %) o) [:scale-specs :color :domain])]
    (testing "a categorical marker and :type :categorical give the same Year levels (positions and colours)"
      (is (= ["2000" "2001" "2002" "2003"]
             (x-labels {:mapping {:x (cljplot2.data.levels/categorical :year) :y :v}})
             (x-labels {:mapping {:x :year :y :v} :scales {:x {:type :categorical}}})))
      (is (= (mapv #(java.time.Year/of %) (range 2000 2004))
             (color-domain {:mapping {:x :v :y :v :color (cljplot2.data.levels/categorical :year)}})
             (color-domain {:mapping {:x :v :y :v :color :year} :scales {:color {:type :categorical}}}))))
    (testing "boundary: undeclared Years are dates - a datetime x"
      (is (= :datetime (get-in (plot/build {:data rows :mapping {:x :year :y :v} :charts [(chart/scatter)]} o) [:scale-specs :x :kind]))))))
