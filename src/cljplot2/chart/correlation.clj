(ns cljplot2.chart.correlation
  "Correlation: the correlation coefficient r of the panel's `:x` / `:y` drawn as one glyph filling the panel (a
  correlogram cell; cartesian plots only) - with [[cljplot2.multiplot/pairs]] a correlogram is a matrix of these.

  Mapping: numeric `:x`, `:y` (rows without finite values are dropped and counted). `:method` - `:pearson` (default),
  `:spearman`, `:kendall` (fastmath); fewer than 2 points or a constant variable give no r (`NA`).

  `:glyph`:

  * `:number` (default) - r to 2 decimals (text role `:correlation`); with a grouping mapping (`:color`, `:group`,
    other ordinal aesthetics) \"Corr: r\" of all the panel's points and a line per group, in its style
  * `:shade` - the whole panel in r's color
  * `:square`, `:circle` - area proportional to |r|
  * `:ellipse` - the bivariate normal contour of r: axes along the diagonals, minor / major ratio
    √((1−|r|)/(1+|r|)), rising for r > 0
  * `:pie` - a disk filled |r|·360° from the top, clockwise for r > 0, counter-clockwise for r < 0, outlined

  Color: r through the plot's color scale (a computed color - domain [-1 1], diverging `:RColorBrewer/RdBu`, legend
  hidden by default: the glyph carries the value; `:scales {:color {...}}` changes any of them); option `:color` - one fixed color; grouped glyphs other
  than `:number` show the panel's r in the theme color. The `:number` text takes the `:correlation` text role's
  color; grouped lines their group's color. The chart adds
  no x / y extents: it fills whatever panel it is in (e.g. a `pairs` cell with the other charts' domains).

  Options (style defaults from theme `[:chart :correlation]`): `:color`, `:stroke-color` (glyph outline: a color,
  `:none` or `:darker`), `:stroke-width`, `:method`, `:glyph`."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def glyphs [:number :shade :square :circle :ellipse :pie])

(def ^:private style-entries
  [(schema/optional :color schema/color)
   (schema/optional :stroke-color draw/stroke-color)
   (schema/optional :stroke-width schema/non-neg)])

(def schemas
  "Schemas of the correlation chart: `:style` (theme `[:chart :correlation]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :method [:enum :pearson :spearman :kendall])
                      (schema/optional :glyph (into [:enum] glyphs))))
   :mapping [:maybe [:map [:x schema/accessor] [:y schema/accessor]]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :correlation]) (select-keys options [:color :stroke-color :stroke-width])))

(defn- grouped? [mapping] (boolean (seq (group/group-dims mapping))))

(defn computed-aesthetics
  "The correlation chart's computed color - r, legend hidden - unless a fixed `:color`, the `:number` glyph or a
  grouping mapping colors it."
  [options mapping]
  (if (or (:color options) (= :number (get options :glyph :number)) (grouped? mapping))
    {}
    {:color {:title "r" :legend false :range :RColorBrewer/RdBu}}))

(defn r
  "Correlation coefficient of `xs`, `ys` (doubles) by `method`; `##NaN` for fewer than 2 points or no spread."
  ^double [method xs ys]
  (if (< (count xs) 2)
    ##NaN
    (let [v (double (case method
                      :spearman (stats/spearman-correlation xs ys)
                      :kendall (stats/kendall-correlation xs ys)
                      (stats/pearson-correlation xs ys)))]
      (if (m/valid-double? v) v ##NaN))))

(defn- finite? [v] (and (number? v) (m/valid-double? (double v))))

(defn- pairs-of
  "`[xs ys]` (doubles) of `rows` with finite x and y."
  [rows x-of y-of]
  (let [ps (filterv (fn [[x y]] (and (finite? x) (finite? y))) (map (juxt x-of y-of) rows))]
    [(mapv #(double (first %)) ps) (mapv #(double (second %)) ps)]))

(defn compute
  "Per panel: `{:r r :groups [{:aesthetics a :r r} ..] :glyph g :style st :by-r? b}`; no x / y extents, color extent
  [-1 1] when colored by r, the groups' aesthetic extents when grouped; `:dropped` (`:x`, `:y`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [method (get options :method :pearson)
        glyph (get options :glyph :number)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        grouped (grouped? mapping)
        split (when grouped (group/split groups mapping env))
        by-r? (contains? (computed-aesthetics options mapping) :color)
        rows (mapcat val groups)
        panel-r (fn [rows] (let [[xs ys] (pairs-of rows x-of y-of)] (r method xs ys)))]
    {:data (into {} (map (fn [[panel rows]]
                           [panel {:r (panel-r rows)
                                   :groups (when grouped
                                             (mapv (fn [{:keys [aesthetics rows]}] {:aesthetics aesthetics :r (panel-r rows)})
                                                   (get-in split [:panels panel])))
                                   :glyph glyph :by-r? by-r? :style (style options theme)}]))
                 groups)
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {}
                                       by-r? (assoc :color {:kind :numeric :min -1.0 :max 1.0})
                                       grouped (merge (get (group/aesthetic-extents (:panels split) plot-scales) panel)))]))
                    (keys groups))
     :na (if grouped (group/na-dims (:panels split)) #{})
     :dropped {:x (count (remove (comp finite? x-of) rows)) :y (count (remove (comp finite? y-of) rows))}}))

;; drawing

(defn- number-text [^double v] (if (m/nan? v) "NA" (format "%.2f" v)))

(defn- glyph-color
  "Fill of a glyph for `r`: through the color scale when colored by r, else the style's color; `nil` - skip."
  [r {:keys [by-r? style]} scales env]
  (if by-r?
    (when-not (m/nan? (double r))
      ((aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na])) r))
    (:color style)))

(defn- outline!
  [canvas shape-fn fill {:keys [stroke-color stroke-width]}]
  (let [stroke (draw/outline-color stroke-color fill)]
    (when (and (not (theme/none? stroke)) (pos? (double stroke-width)))
      (c2d/set-stroke-custom canvas {:size (double stroke-width)})
      (c2d/set-color canvas stroke)
      (shape-fn true))))

(defn- ellipse-points
  "Pixel outline of the r ellipse centred at `(cx, cy)` with semi-major axis `a`."
  [^double cx ^double cy ^double a ^double r]
  (let [b (* a (m/sqrt (/ (- 1.0 (m/abs r)) (+ 1.0 (m/abs r)))))
        ;; major axis along the rising diagonal for r > 0 (pixel y grows downward)
        angle (if (neg? r) (* 0.25 m/PI) (* -0.25 m/PI))
        ca (m/cos angle) sa (m/sin angle)]
    (mapv (fn [^long i] (let [t (* m/TWO_PI (/ (double i) 72.0))
                              u (* a (m/cos t)) v (* b (m/sin t))]
                          [(+ cx (- (* u ca) (* v sa))) (+ cy (+ (* u sa) (* v ca)))]))
          (range 72))))

(defn- draw-number!
  [canvas {:keys [r groups]} {:keys [w h]} scales env]
  (let [lines (if (seq groups)
                (into [{:text (str "Corr: " (number-text r))}]
                      (keep (fn [{:keys [aesthetics r]}]
                              (when-let [st (group/style {:color :black} aesthetics scales (:theme env))]
                                {:text (str (or (some-> (first (vals aesthetics)) str) "NA") ": " (number-text r)) :color (:color st)}))
                            groups))
                [{:text (number-text r)}])
        line-h (* 1.3 (double (get-in env [:theme :text :correlation :size])))
        top (- (* 0.5 (double h)) (* 0.5 line-h (dec (count lines))))]
    (doseq [[i {:keys [text color]}] (map-indexed vector lines)]
      (label/draw-at! canvas (cond-> {:kind :label :role :correlation :text text} color (assoc :color color))
                      [:center :middle] (* 0.5 (double w)) (+ top (* line-h (double i))) env))))

(defn draw
  "Draw one panel's glyph (see the namespace doc)."
  [_chart {:keys [r glyph style] :as computed} canvas {:keys [w h] :as box} scales env]
  (let [r (double r)
        w (double w) h (double h)
        cx (* 0.5 w) cy (* 0.5 h)
        side (* 0.9 (m/min w h))
        fill (glyph-color r computed scales env)]
    (case glyph
      :number (draw-number! canvas computed box scales env)
      :shade (when fill (c2d/set-color canvas fill) (c2d/rect canvas 0 0 w h))
      (:square :circle) (when fill
                          (let [s (* side (m/sqrt (m/abs r)))
                                shape! (if (= glyph :square)
                                         #(c2d/rect canvas (- cx (* 0.5 s)) (- cy (* 0.5 s)) s s %)
                                         #(c2d/ellipse canvas cx cy s s %))]
                            (when (pos? s)
                              (c2d/set-color canvas fill) (shape! false) (outline! canvas shape! fill style))))
      :ellipse (when fill
                 (let [pts (ellipse-points cx cy (* 0.5 side) r)
                       shape! #(c2d/path canvas pts true %)]
                   (c2d/set-color canvas fill) (shape! false) (outline! canvas shape! fill style)))
      :pie (when fill
             (let [extent (* m/TWO_PI (m/abs r))
                   ;; from the top (clojure2d angles run with y downward); clockwise on screen for r > 0
                   start (* 0.5 m/PI)
                   sector! #(c2d/arc canvas cx cy side side start (if (neg? r) (- extent) extent) :pie %)]
               (c2d/set-color canvas fill)
               (when (pos? extent) (sector! false))
               (outline! canvas #(c2d/ellipse canvas cx cy side side %) fill style))))))

(defn legend
  "Key glyph of a grouped chart: a filled square in the key's color."
  [_chart attributes canvas box _env]
  (draw/bar-key! canvas box {:color (:color attributes) :stroke-width 0} false))
