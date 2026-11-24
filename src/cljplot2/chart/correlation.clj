(ns cljplot2.chart.correlation
  "Correlation: the correlation coefficient r of the panel's `:x` / `:y` drawn as one glyph filling the panel (a
  correlogram cell; cartesian plots only) - with [[cljplot2.multiplot/pairs]] a correlogram is a matrix of these.

  Mapping: numeric `:x`, `:y` (rows without finite values are dropped and counted). `:method` - `:pearson` (default),
  `:spearman`, `:kendall` (fastmath); fewer than 2 points or a constant variable give no r (`NA`).

  `:glyph`:

  * `:number` (default) - `<method>: r` to 2 decimals (`Corr`, `Spearman`, `Kendall`; text role
    `:correlation`, shrunk to fit a narrow panel); with a grouping mapping (`:group`, ordinal aesthetics such as a categorical `:color`) a line per
    group under it, in the group's style
  * `:shade` - the whole panel in r's color
  * `:square`, `:circle` - area proportional to |r|
  * `:ellipse` - the bivariate normal contour of r: axes along the diagonals, minor / major ratio
    √((1−|r|)/(1+|r|)), rising for r > 0
  * `:pie` - a disk filled |r|·360° from the top, clockwise for r > 0, counter-clockwise for r < 0, outlined

  Color: r through the plot's color scale (a computed color - domain [-1 1], diverging `:RColorBrewer/RdBu`, with a
  color bar; `:scales {:color {...}}` changes them); option `:color` - one fixed color (the `:number` text's too);
  with a mapped aesthetic (the plot's color then belongs to the mapping - e.g. a continuous `:color` of the other
  charts, which doesn't group) glyphs other than `:number` show the panel's r in the theme color. The `:number` text
  takes the `:correlation` text role's color unless `:color` is given; group lines their group's color. A missing r
  (fewer than 2 points, a constant variable) shows `NA`: shape glyphs draw a bare `NA`, `:number` shows `NA` as the value. The chart adds
  no x / y extents: it fills whatever panel it is in (e.g. a `pairs` cell with the other charts' domains).

  Options (style defaults from theme `[:chart :correlation]`): `:color`, `:stroke-color` (glyph outline: a color,
  `:none` or `:darker`), `:stroke-width`, `:method`, `:glyph`, `:glyph-size` (default 0.9 - the glyph's side as a
  fraction of the panel's shorter side).

  Functions: [[method-labels]] - the coefficient labels; [[computed-aesthetics]] - the computed color; [[compute]], [[draw]], [[legend]] - the chart contract. r is [[cljplot2.data.pairwise/statistic-value]] of the paired samples (shared cell parts: [[cljplot2.chart.statistic-cell]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.statistic-cell :as cell]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private glyphs
  "The glyphs r is drawn as (option `:glyph`)."
  [:number :shade :square :circle :ellipse :pie])

(def ^:private style-entries
  "Style entries of the correlation chart (theme `[:chart :correlation]`)."
  [(schema/optional :color {:description "one fixed glyph colour, the :number text's too"
                             :cljplot2/default-doc (str "grouped shape glyphs: the theme's; ungrouped: r on a diverging scale; :number: the "
                                                        ":correlation text role's colour, group lines mapped or black")}
                    schema/color)
   (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)])

(def ^{:private true :const true :tag 'double} default-glyph-size
  "Glyph side as a fraction of the panel's shorter side, unless `:glyph-size` says otherwise."
  0.9)

(def schemas
  "Schemas of the correlation chart: `:style` (theme `[:chart :correlation]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :method {:description "correlation coefficient: :pearson, :spearman or :kendall"
                                                :default :pearson}
                                        [:enum :pearson :spearman :kendall])
                      (schema/optional :glyph {:description (str "how r is drawn: :number (text, a line per group), :shade (the panel in r's colour), "
                                                                 ":square / :circle (area ~ |r|), :ellipse (normal contour) or :pie (a disk filled |r|)")
                                               :default :number}
                                       (into [:enum] glyphs))
                      (schema/optional :glyph-size {:description "glyph side (:square, :circle, :ellipse, :pie) as a fraction of the panel's shorter side"
                                                    :default default-glyph-size}
                                       schema/fraction)))
   :mapping [:maybe [:map
                     [:x {:description "first variable (numbers)"} schema/accessor]
                     [:y {:description "second variable (numbers)"} schema/accessor]
                     (schema/nilable :color {:description "categorical groups, each a line of the :number glyph in its colour"} schema/accessor)
                     (schema/nilable :group {:description "groups without a visual aesthetic, each a line of the :number glyph"} schema/accessor)]]})

(defn computed-aesthetics
  "Gives the correlation chart's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color {:title r :range range}}` - r on the diverging palette [[cljplot2.chart.statistic-cell/diverging-range]], with its colour bar; `{}` when a fixed `:color`, the `:number` glyph or a mapped aesthetic (the plot's colour then belongs to the mapping) colours the glyphs (see [[cljplot2.chart.statistic-cell/colored-by-value?]])."
  [options mapping]
  (if (cell/colored-by-value? options mapping)
    {:color {:title "r" :range cell/diverging-range}}
    {}))

(def method-labels
  "Label of the coefficient of each `:method`."
  {:pearson "Corr" :spearman "Spearman" :kendall "Kendall"})

(defn compute
  "Computes the correlation chart's stats: r of every panel, and of its groups when grouped.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:method`, `:glyph`, `:glyph-size`, the style).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged; `:x`, `:y`, grouping aesthetics).
  - `env` (map): `:theme`, `:plot-scales` and grouping settings.

  Returns per panel `{:r r :groups [{:aesthetics a :value r} ..] :glyph g :glyph-size s :method m :by-r? b :style st :text-color c}`; no x / y extents, color extent [-1 1] when colored by r, the groups' aesthetic extents when grouped; `:na`, `:dropped` (`:x`, `:y`).

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite `:x` (or `:y`).

  See also [[cljplot2.data.pairwise/statistic-value]] (r is its `:pearson`, `:spearman` or `:kendall`)."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [method (data/option options :method :pearson)
        glyph (data/option options :glyph :number)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        _ (data/check-mapped! (mapcat val groups) [[:x x-of] [:y y-of]] (fn [_ v] (schema/finite? v)))
        {:keys [grouped split]} (cell/grouping groups mapping env)
        by-r? (cell/colored-by-value? options mapping)
        st (theme/resolved-style theme chart)
        rows-r (fn [rows-of] (let [[xs ys] (pairwise/pair-samples rows-of x-of y-of)] (pairwise/statistic-value method xs ys)))]
    {:data (into {} (map (fn [[panel panel-rows]]
                           [panel {:r (rows-r panel-rows)
                                   :groups (when grouped
                                             (mapv (fn [{:keys [aesthetics] group-rows :rows}] {:aesthetics aesthetics :value (rows-r group-rows)})
                                                   (get-in split [:panels panel])))
                                   :glyph glyph
                                   :glyph-size (data/option options :glyph-size default-glyph-size)
                                   :method method
                                   :by-r? by-r?
                                   :style st
                                   :text-color (:color options)}]))
                 groups)
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {}
                                       by-r? (assoc :color {:kind :numeric :min -1.0 :max 1.0})
                                       grouped (merge (get (group/aesthetic-extents (:panels split) plot-scales) panel)))]))
                    (keys groups))
     :na (if grouped (group/na-dims (:panels split)) #{})
     :dropped (cell/dropped (mapcat val groups) mapping)}))

;; drawing

(defn- number-text
  "Gives the text of a coefficient.

  Parameters:

  - `v` (double): the coefficient.

  Returns the coefficient with 2 decimals; `NA` for `##NaN`."
  [^double v]
  (if (m/nan? v) "NA" (format "%.2f" v)))

(defn- outline!
  "Draws a glyph's outline.

  Parameters:

  - `canvas`: the panel's canvas.
  - `shape-fn` (function): draws the glyph's shape, outlined when given `true`.
  - `fill`: the glyph's fill (`:darker` darkens it, see [[cljplot2.graphics.draw/outline-color]]).
  - `style` (map): `:stroke-color`, `:stroke-width`.

  Nothing for a `:none` stroke or a zero width."
  [canvas shape-fn fill {:keys [stroke-color stroke-width]}]
  (draw/outline! canvas (draw/outline-color stroke-color fill) stroke-width (fn [_] (shape-fn true))))

(def ^{:private true :const true :tag 'long} ellipse-segments
  "Segments of an ellipse glyph's outline."
  72)

(defn- ellipse-points
  "Gives the pixel outline of an ellipse glyph.

  The bivariate normal contour of r: axes along the diagonals (the major one rising for r > 0), minor / major ratio `√((1−|r|)/(1+|r|))`.

  Parameters:

  - `cx`, `cy` (doubles): the centre pixel.
  - `a` (double): the semi-major axis in pixels.
  - `coefficient` (double): r.

  Returns a vector of [[ellipse-segments]] pixel points `[x y]` around the ellipse."
  [^double cx ^double cy ^double a ^double coefficient]
  (let [b (m/* a (m/sqrt (m// (m/- 1.0 (m/abs coefficient)) (m/inc (m/abs coefficient)))))
        ;; major axis along the rising diagonal for r > 0 (pixel y grows downward)
        angle (if (m/neg? coefficient) m/QUARTER_PI m/-QUARTER_PI)
        ca (m/cos angle)
        sa (m/sin angle)]
    (mapv (fn [^long i] (let [t (m/* m/TWO_PI (m// i (double ellipse-segments)))
                             u (m/* a (m/cos t))
                             w (m/* b (m/sin t))]
                         [(m/+ cx (m/- (m/* u ca) (m/* w sa))) (m/+ cy (m/+ (m/* u sa) (m/* w ca)))]))
          (range ellipse-segments))))

(defn- draw-text-lines!
  "Draws text lines centred in the panel in the `:correlation` text role, shrunk to fit (see [[cljplot2.content.label/draw-fitted-lines!]]).

  Parameters:

  - `canvas`: the panel's canvas.
  - `lines` (vector): `[{:text :color} ..]` (`:color` optional - the text role's).
  - `template` (string): the widest text a line can hold.
  - `box` (map): the panel box, `:w` and `:h`.
  - `env` (map): the drawing environment."
  [canvas lines template box env]
  (label/draw-fitted-lines! canvas {:lines lines :template template :role :correlation} box env))

(defn- draw-number!
  "Draws the `:number` glyph: `<method label>: r`, with a line per group.

  Parameters:

  - `canvas`: the panel's canvas.
  - `computed` (map): the panel's stats - `:r`, `:groups`, `:method`, `:text-color` (the `:color` option; else the text role's colour).
  - `box` (map): the panel box, `:w` and `:h`.
  - `scales` (map): the panel's scales (the groups' styles).
  - `env` (map): the drawing environment.

  Each group's line in its group's colour; a group whose aesthetics fall outside their scales gets no line."
  [canvas {:keys [method text-color] panel-r :r panel-groups :groups} {:keys [w h]} scales env]
  (let [lines (cell/value-lines {:text (str (method-labels method) ": " (number-text panel-r)) :color text-color}
                                panel-groups number-text
                                (draw/text-color (:theme env) :correlation (draw/plot-backdrop (:theme env))) scales env)]
    (draw-text-lines! canvas lines (str (method-labels method) ": -0.00") {:w w :h h} env)))

(defn draw
  "Draws one panel's glyph (see the namespace doc).

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats (see [[compute]]).
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box, `:w` and `:h` in pixels.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.

  A missing r draws `NA` - a bare `NA` for a shape glyph, `NA` as the value of `:number`; a shape glyph without a fill (r outside the colour scale) draws nothing."
  [_chart {:keys [glyph ^double glyph-size style] ^double coefficient :r :as computed} canvas {:keys [^double w ^double h] :as box} scales env]
  (let [cx (m/* 0.5 w)
        cy (m/* 0.5 h)
        side (m/* glyph-size (m/min w h))
        fill (cell/value-fill coefficient (:by-r? computed) (:color style) scales env)]
    (if (and (m/nan? coefficient) (not= glyph :number))
      (draw-text-lines! canvas [{:text "NA"}] "NA" box env)
      (case glyph
        :number (draw-number! canvas computed box scales env)
        :shade (when fill (c2d/set-color canvas fill) (c2d/rect canvas 0 0 w h))
        (:square :circle) (when fill
                            (let [s (m/* side (m/sqrt (m/abs coefficient)))
                                  shape! (if (= glyph :square)
                                           #(c2d/rect canvas (m/- cx (m/* 0.5 s)) (m/- cy (m/* 0.5 s)) s s %)
                                           #(c2d/ellipse canvas cx cy s s %))]
                              (when (m/pos? s)
                                (c2d/set-color canvas fill) (shape! false) (outline! canvas shape! fill style))))
        :ellipse (when fill
                   (let [pts (ellipse-points cx cy (m/* 0.5 side) coefficient)
                         shape! #(c2d/path canvas pts true %)]
                     (c2d/set-color canvas fill) (shape! false) (outline! canvas shape! fill style)))
        :pie (when fill
               (let [extent (m/* m/TWO_PI (m/abs coefficient))
                     ;; from the top (clojure2d angles run with y downward); clockwise on screen for r > 0
                     sector! #(c2d/arc canvas cx cy side side m/HALF_PI (if (m/neg? coefficient) (m/- extent) extent) :pie %)]
                 (c2d/set-color canvas fill)
                 (when (m/pos? extent) (sector! false))
                 (outline! canvas #(c2d/ellipse canvas cx cy side side %) fill style)))))))

(def legend
  "Draws the key glyph of a grouped chart: a filled square in the key's colour (see [[cljplot2.chart.statistic-cell/legend]])."
  cell/legend)
