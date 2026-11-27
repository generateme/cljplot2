(ns cljplot2.chart.line
  "Line: points `(x, y)` joined per panel and group.

  Options (style defaults from theme `[:chart :line]`): `:order` (`:x` - default, points ordered by numeric or temporal
  x, ties in data order; `:data` keeps data order), `:color`, `:line-width`, `:dash`, `:fill` (area chart: the area
  between the line and y = 0 - or the nearer panel edge when 0 is outside the y range - default `:none`),
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side; stacked `[0 0.05]` for y).
  `:fill :color` - the area in the line (group) colour faded by `:alpha` (0..1, theme default 0.6): its own alpha
  times `:alpha` (a colour with alpha 128 fills at half `:alpha`; see [[cljplot2.graphics.polyline/area-color]]). `:fill-gradient` (a
  clojure2d gradient: preset, colours or function of `t`) colours areas along `:fill-gradient-axis` - `:x` (default,
  over the x scale's range) or `:y` (a linear gradient over the y scale's range); it wins over `:fill`.
  `:position` (see [[cljplot2.data.position]]): `:overlay` (default), `:stack`, `:normalize` (y title
  \"Proportion\"), streamgraphs `:silhouette` / `:wiggle` - groups aligned on the union of their x values (numeric
  or temporal; linear between a group's points, 0 outside its range; NA y left out and interpolated across; repeated
  x in a group - `ex-info {:key :mapping}`), stacked, every band filled (`:fill`, default the group colour faded by `:alpha`)
  and its top stroked; `:interpolator` smooths the bands; `:curve`, `:gaps` and a line coloured along its length
  can't be stacked.
  Curves (see [[cljplot2.graphics.curve]]): `:interpolator` (a fastmath 1D method or a function) with
  `:interpolator-params` - each run fitted along x in pixels and evaluated every `:sample-step` (pixels, default 1,
  or `{:count n}`); `:curve :parametric` / `:bezier` - smooth paths in data order (connected scatter); the area follows
  the curve.
  A row with an invalid x is dropped (counted); an invalid y breaks the line there (counted); `:gaps` - `:break`
  (default, empty space) or `:bridge` (a straight segment over the gap in the theme `:bridge` style - the line alpha
  multiplied by `:alpha-factor`, optional `:line-width` / `:dash`; the area stays empty).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`.
  A `:color` with a linear or binned scale doesn't group: it colors each line along its length - every segment in
  the color of the mean of its two end values (the theme NA color when one is NA), round caps, the dash running on
  along the line. A point whose color value the scale doesn't keep (invalid, outside its explicit domain under
  `:out-of-domain` `nil` / `:drop`, NA with `:groups {:na false}`) breaks the line as an invalid y does (counted in
  `:dropped :color`; `:gaps :bridge` bridges it); under `:as-na` / `:clamp` its segments take the NA / end color. With an `:interpolator` the sampled curve takes the color value interpolated linearly between
  its data points, in steps of a few pixels (`:curve` can't color along the length - `ex-info {:key :curve}`); a bridge takes the color of
  its two end values.
  A mapped `:size` is the line width. With a linear size scale (it doesn't group) each line takes the value of its first
  drawn point - in drawing order, with a valid y and, coloured along its length, a kept color; NA takes the style
  `:line-width`. A line whose value the scale doesn't keep (invalid, outside the explicit domain under `:out-of-domain`
  `nil` / `:drop`, NA with `:groups {:na false}`) is left out, its points counted in `:dropped :size` (see
  [[cljplot2.data.group/kept-marks]]). With an ordinal size scale a line takes its group's level. The legend shows
  lines of those widths.
  `:stroke-alpha-factor` (a fraction) multiplies the stroke colour's opacity of unstacked lines (many overlapping lines; the
  legend key stays opaque).
  Arrows (see [[cljplot2.graphics.arrow]]): `:arrow {:end :start :size :shape}`, `:start-gap` / `:end-gap` (pixels) -
  at the ends of every run of a plain stroked line (no fill, fill gradient, colour along the length or stacking -
  `ex-info {:key :arrow}`; no bridges between the runs).
  `:trail true` (Vega-Lite trail): a linear `:size` sets the width at every point - a filled shape, the width linear
  between points, round ends and joins (see [[cljplot2.graphics.polyline/trail-shape]]); a point whose size the scale
  doesn't keep - NA, invalid, outside its explicit domain under `:out-of-domain` `nil` / `:drop` - breaks the trail
  (counted in `:dropped :size`); without a linear size the group's width (`:line-width` or an ordinal size level). Size scale and
  legend unchanged, one colour per group; polar trails follow the circle with the plot's `:arcs`, else chords. A trail can't take `:fill`,
  `:fill-gradient`, a stacked `:position`, `:arrow` / `:start-gap` / `:end-gap`, `:dash`, `:interpolator`, `:curve`,
  `:gaps :bridge` or a colour along its length - `ex-info {:key :trail}`.

  Functions: [[positions]] and [[schemas]] - the chart's positions and schemas; [[compute]] - the lines of every
  panel (stacked by [[stacked-lines]]); [[stacked-band-points]] - stacked bands in pixels; [[draw]] - one panel's
  lines; [[legend]] - the key glyph."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.common :as common]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.graphics.arrow :as arrow]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the line style: the line style, area `:fill` and `:alpha`, `:bridge`, the area pattern."
  (into (conj polyline/style-entries
              (schema/optional :fill {:cljplot2/section :style :description "Fills the area between the line and y = 0." :cljplot2/value-meanings {:color "The line colour, faded by `:alpha`." :none "No area."}}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:cljplot2/section :style :description "How opaque the area is with `:fill :color` and in stacked areas; it multiplies the line colour's alpha." :cljplot2/example 0.3 :cljplot2/example-context {:fill :color}} schema/fraction)
              (schema/optional :bridge {:cljplot2/section :path :description "The style of the segments that bridge gaps with `:gaps :bridge`." :cljplot2/example-context {:gaps :bridge}}
                               polyline/bridge-schema))
        draw/pattern-style-entries))

(def ^:private positions
  "Line positions: `:overlay` (default), stacked `:stack` / `:normalize` and the stream positions (see
  [[cljplot2.data.position]])."
  (into #{:overlay :stack :normalize} position/stream-positions))

(def ^:private stacked-positions
  "The positions stacking the lines' areas: all but `:overlay`."
  (disj positions :overlay))

(defn- trail-option-problem
  "Gives the option a trail (`:trail true`) can't take, from the options alone.

  Parameters:

  - `options` (map): the chart's options.

  Returns the first of `:fill` (an area), `:fill-gradient`, `:position` (stacked), `:arrow` (arrows or end gaps), `:dash`, `:interpolator`, `:curve`, `:gaps` (`:bridge`) the options hold; `nil` without a trail or a problem. A colour along the length is checked at compute (see [[check-trail!]])."
  [options]
  (when (data/option options :trail false)
    (cond
      (not (theme/none? (data/option options :fill :none))) :fill
      (:fill-gradient options) :fill-gradient
      (stacked-positions (data/option options :position :overlay)) :position
      (some options [:arrow :start-gap :end-gap]) :arrow
      (some? (:dash options)) :dash
      (:interpolator options) :interpolator
      (:curve options) :curve
      (= :bridge (:gaps options)) :gaps)))

(defn- end-marks-problem
  "Gives the option that keeps a line from taking arrows or end gaps.

  Parameters:

  - `options` (map): the chart options.

  Returns `:fill` (an area fill other than `:none`), `:fill-gradient` or `:position` (stacked) - the first the options hold; `nil` when none does. A theme fill, a colour along the length and stacked data are checked at draw."
  [options]
  (cond
    (some-> (:fill options) theme/none? not) :fill
    (:fill-gradient options) :fill-gradient
    (stacked-positions (data/option options :position :overlay)) :position))

(def ^:private option-rules
  "Rules (see [[cljplot2.schema/with-rules]]) of the options: `:fill-gradient-axis` needs `:fill-gradient` ([[cljplot2.chart.common/fill-gradient-rule]]); stacked lines take no `:curve` or `:gaps`; a trail takes no option of [[trail-option-problem]]; `:arrow`, `:start-gap` and `:end-gap` need a plain stroked line ([[end-marks-problem]])."
  (into [common/fill-gradient-rule
         [#(when (and (stacked-positions (data/option % :position :overlay)) (:curve %)) "stacked lines can't take :curve (use :interpolator)") [:curve]]
         [#(when (and (stacked-positions (data/option % :position :overlay)) (:gaps %)) "stacked lines interpolate across missing values; :gaps doesn't apply") [:gaps]]
         [#(when-let [problem (trail-option-problem %)] (str "a trail (:trail true) is a filled shape of varying width: it can't take " problem)) [:trail]]]
        (for [k [:arrow :start-gap :end-gap]]
          [(fn [options] (when (some? (get options k))
                           (when-let [problem (end-marks-problem options)]
                             (str "arrows and end gaps need a plain stroked line: no " problem))))
           [k]])))

(def schemas
  "Schemas of the line chart: `:style` (theme `[:chart :line]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (apply schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (into curve/options)
                 (conj (schema/nilable :order {:cljplot2/section :path :description "The order the points are joined in." :cljplot2/value-meanings {:x "By x (numbers or dates), ties in data order." :data "In data order."} :default :x}
                                       [:enum :x :data])
                       (schema/optional :gaps {:cljplot2/section :path :description "How the line handles an invalid y." :cljplot2/example :bridge :cljplot2/value-meanings {:break "Leaves empty space." :bridge "Joins its neighbours with a straight segment in the `:bridge` style."}
                                               :cljplot2/default-doc "`:break`; stacked lines reject `:gaps` (they interpolate across missing values)."}
                                        [:enum :break :bridge])
                       (schema/optional :position {:cljplot2/section :path :description "How groups combine." :cljplot2/value-meanings {:overlay "Each line on its own." :stack "Stacked on top of each other." :normalize "Stacked as shares: at each x the groups sum to 1." :silhouette "A streamgraph centred on y = 0." :wiggle "A streamgraph whose layers slope as little as possible."} :cljplot2/example :stack
                                                   :default :overlay}
                                        (into [:enum] (sort positions)))
                       (schema/optional :fill-gradient {:cljplot2/section :style :description "A gradient that colours the areas instead of `:fill`." :cljplot2/default-doc "No gradient; `:fill` colours the areas." :cljplot2/example :mpl/viridis}
                                        polyline/fill-gradient-schema)
                       (schema/optional :fill-gradient-axis {:cljplot2/section :style :description "The axis the gradient runs along, across its scale's range; it needs `:fill-gradient`." :cljplot2/value-meanings {:x "Colours change with x." :y "Colours change with y."}
                                                             :cljplot2/default-doc "`:x`." :cljplot2/example :y :cljplot2/example-context {:fill-gradient :mpl/viridis}}
                                        [:enum :x :y])
                       (schema/optional :padding {:cljplot2/section :path :description "Room around the data on x and y, as fractions of the data span." :cljplot2/example {:x [0 0]}
                                                  :cljplot2/default-doc "`0.05` on every side; y `[0 0.05]` with `:stack` or `:normalize` and no negative values."}
                                        data/position-padding)
                       (schema/optional :stroke-alpha-factor {:cljplot2/section :style :description "Multiplies the stroke opacity of unstacked lines; legend keys stay opaque." :cljplot2/default-doc "No fading." :cljplot2/example 0.5}
                                        schema/fraction)
                       (schema/optional :arrow {:cljplot2/section :ends :description "Arrowheads at the ends of each unbroken piece of a plain stroked line (no fill, gradient, colour along the line or stacking)."} arrow/schema)
                       (schema/optional :start-gap {:cljplot2/section :ends :description "Shortens each unbroken piece of the line at its start, in pixels." :cljplot2/default-doc "No gap." :cljplot2/example 4} schema/non-neg)
                       (schema/optional :end-gap {:cljplot2/section :ends :description "Shortens each unbroken piece of the line at its end, in pixels." :cljplot2/default-doc "No gap." :cljplot2/example 4} schema/non-neg)
                       (schema/nilable :trail {:cljplot2/section :style :description "Whether the line becomes a filled trail whose width follows a numeric mapped `:size`." :cljplot2/value-meanings {true "The width changes at every point." false "A plain stroked line."} :cljplot2/example-context {:mapping {:size :y}}
                                               :default false}
                                       schema/bool)))
             option-rules)
   :mapping [:map
             [:x {:description "The x of each point."} schema/accessor]
             [:y {:description "The y of each point."} schema/accessor]
             (schema/nilable :color {:description "Colours the line: per group (ordinal), or along the line (linear or binned)."} schema/accessor)
             (schema/nilable :size {:description "Sets the line width: per group (a linear size takes the value of the line's first drawn point), or per point with `:trail`."} schema/accessor)
             (schema/nilable :dash {:description "Splits the rows into groups, each line with its own dash style."} schema/accessor)
             (schema/nilable :pattern {:description "Splits the rows into groups, each area with its own fill pattern."} schema/accessor)
             (schema/nilable :group {:description "Splits the rows into groups that look the same, each its own line."} schema/accessor)]})

(defn- aligned-heights
  "Gives a group's heights at the union of the groups' x keys.

  Parameters:

  - `ks` (vector): the group's x keys, strictly increasing.
  - `vs` (vector): its values at `ks`.
  - `union` (vector): the union keys, increasing.

  Returns a vector of heights at `union`: linear between the group's points, 0 outside its range (one walk over both)."
  [ks vs union]
  (let [n (count ks)
        lo (double (first ks))
        hi (double (peek ks))]
    (loop [i 0
           us (seq union)
           acc []]
      (if-not us
        acc
        (let [u (double (first us))]
          (cond
            (or (m/< u lo) (m/> u hi)) (recur i (next us) (conj acc 0.0))
            (and (m/< i (m/long-sub n 2)) (m/> u (double (ks (m/long-inc i))))) (recur (m/long-inc i) us acc)
            (m/== n 1) (recur i (next us) (conj acc (double (vs 0))))
            :else (let [k0 (double (ks i))
                        k1 (double (ks (m/long-inc i)))
                        v0 (double (vs i))
                        v1 (double (vs (m/long-inc i)))
                        f (if (m/== k0 k1) 0.0 (m// (m/- u k0) (m/- k1 k0)))]
                    (recur i (next us) (conj acc (m/+ v0 (m/* f (m/- v1 v0))))))))))))

(defn- stacked-lines
  "Stacks the lines of one panel.

  Every group is put on the union of the groups' x (keys of [[cljplot2.data.rows/x-key]]), interpolated linearly inside its own range and 0 outside; NA y points are left out (interpolated across); points are in x order whatever `:order` says.

  Parameters:

  - `lines` (sequence): the panel's lines `{:x [..] :y [..] :aesthetics}` (optional `:size-value`).
  - `position` (keyword): a stacked position (see [[cljplot2.data.position/stack-values]]).

  Returns a vector of lines `{:x union :y upper :y0 lower :heights aligned :aesthetics}` (`:heights` - the group's aligned values, for smoothing at drawing); groups without points are left out.

  Throws `ex-info {:key :mapping :in [:mapping :x]}` (see [[cljplot2.data.rows/fail-mapping!]]) for a non-numeric, non-temporal x or a repeated x in a group."
  [lines position]
  (let [keyed-lines (mapv (fn [{:keys [x y aesthetics] :as line}]
                            (let [keyed (->> (map vector x y)
                                             (filter #(some? (second %)))
                                             (map (fn [[vx vy]] [(or (data/x-key vx)
                                                                     (data/fail-mapping! "Stacked lines need numeric or temporal (java.time) x."
                                                                                         :x {:value vx}))
                                                                 vx vy]))
                                             (sort-by first))
                                  ks (mapv first keyed)
                                  points (mapv (fn [[_ vx vy]] [vx vy]) keyed)]
                              (when-not (or (m/< (count ks) 2) (apply m/< ks))
                                (data/fail-mapping! "Stacked lines need distinct x values within a group." :x {:group aesthetics}))
                              (merge {:ks ks :vs (mapv #(double (second %)) points) :xs (mapv first points) :aesthetics aesthetics}
                                     (select-keys line [:size-value]))))
                          lines)
        stacked-groups (filterv #(seq (:ks %)) keyed-lines)
        by-key (into (sorted-map) (mapcat (fn [{:keys [ks xs]}] (map vector ks xs)) stacked-groups))
        union (vec (keys by-key))
        heights (mapv (fn [{:keys [ks vs]}] (aligned-heights ks vs union)) stacked-groups)
        ys (position/stack-values position heights)
        xs (vec (vals by-key))]
    (mapv (fn [{:keys [aesthetics] :as g} hs y01]
            (merge {:x xs :y (mapv second y01) :y0 (mapv first y01) :heights hs :aesthetics aesthetics}
                   (select-keys g [:size-value])))
          stacked-groups heights ys)))

(defn- check-stacking!
  "Checks that stacked lines aren't coloured along their length (the options alone are checked by the schema, see [[option-rules]]).

  Parameters:

  - `position` (keyword): the chart's position.
  - `gradient?` (boolean): is the line coloured along its length?

  Returns `nil`; throws `ex-info {:key :position}` for a stacked line coloured along its length."
  [position gradient?]
  (when (and (stacked-positions position) gradient?)
    (schema/fail! "A line colored along its length (linear or binned :color) can't be stacked."
                  {:key :position :value position})))

(defn- check-polar!
  "Checks the options of polar lines.

  Parameters:

  - `options` (map): the chart's options.
  - `coord` (map): the plot's `:coord`.

  Returns `nil`; under a polar coord throws `ex-info {:key :coord :value k}` for an option polar lines don't support: `:interpolator`, `:curve`, a stacked or stream `:position`, an area (`:fill`, `:fill-gradient`)."
  [options coord]
  (when (coord/polar? coord)
    (let [problem (cond
                    (:interpolator options) :interpolator
                    (:curve options) :curve
                    (not= :overlay (data/option options :position :overlay)) :position
                    (:fill-gradient options) :fill-gradient
                    (not (theme/none? (data/option options :fill :none))) :fill)]
      (when problem
        (schema/fail! (str "Polar lines are straight and unfilled: " problem " isn't supported with :coord :polar.")
                      {:key :coord :value problem})))))

(defn- check-trail!
  "Checks that a trail (`:trail true`) isn't coloured along its length (its other options are checked by the schema, see [[trail-option-problem]]).

  Parameters:

  - `options` (map): the chart's options.
  - `gradient?` (boolean): is the line coloured along its length?

  Returns `nil`; throws `ex-info {:key :trail}` for a trail coloured along its length."
  [options gradient?]
  (when (and gradient? (data/option options :trail false))
    (schema/fail! "A trail (:trail true) is a filled shape of varying width: it can't take :color."
                  {:key :trail :value :color})))

(defn compute
  "Computes the lines of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales`, `:coord` (and the grouping keys of [[cljplot2.data.group/split]]).

  Returns `{:data :extents :padding :padding-explicit :axis-titles :na :dropped}`: per panel `{:lines [{:x [..] :y [..] :aesthetics}] :style st}` - `y` `nil` where the line breaks (an invalid y, or a point whose colour or trail size isn't kept; such a line without a drawn point is left out); a line coloured along its length also has `:colors` (every point's colour value), a trail with a linear size `:sizes` (every point's size value), a line sized by a linear size `:size-value` (the value of its first drawn point in drawing order; lines whose value isn't kept left out); stacked positions - the lines of [[stacked-lines]]; `st` the resolved style. Padding 5% each side, stacked y `[0 0.05]` without negative values; `:axis-titles` - `Proportion` for `:normalize`.

  Throws `ex-info` for options the lines can't take with the plot or the data (see [[check-polar!]], [[check-trail!]], [[check-stacking!]]; `{:key :fill-gradient}` on a line coloured along its length), `{:key :mapping}` when `:x` or `:y` has no valid value in non-empty data, and the errors of [[stacked-lines]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (check-polar! options (:coord env))
  (let [st (theme/resolved-style theme chart)
        position (data/option options :position :overlay)
        trail? (data/option options :trail false)
        ;; no row is dropped for a linear / binned colour or size: a point not kept breaks the line, a line's size judges the line
        continuous (group/continuous-color groups mapping (assoc env :row-filter :none))
        gradient? (some? continuous)
        _ (check-trail! options gradient?)
        _ (check-stacking! position gradient?)
        _ (when (and gradient? (:fill-gradient options))
            (schema/fail! "A line colored along its length can't take :fill-gradient." {:key :fill-gradient :value (:fill-gradient options)}))
        color-of (:color-of continuous)
        [colored-groups colored-mapping] (if continuous [(:groups continuous) (:mapping continuous)] [groups mapping])
        sized (group/continuous :size colored-groups colored-mapping (assoc env :row-filter :none))
        size-of (:value-of sized)
        sizes? (and trail? (some? sized))
        {split-panels :panels dropped :dropped} (if sized
                                                  (group/split (:groups sized) (:mapping sized) env)
                                                  (group/split colored-groups colored-mapping env))
        x-of (data/row-accessor :x (:x colored-mapping))
        y-of (data/row-accessor :y (:y colored-mapping))
        x-order? (= :x (data/option options :order :x))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals split-panels))
        _ (data/check-mapped! rows [[:x x-of] [:y y-of]])
        na-level (:na-level env)
        color-kept? (fn [c] (group/kept-value? :color na-level plot-scales c))
        size-kept? (fn [s] (group/kept-value? :size false plot-scales s))
        ;; each point judged once: [x y colour size drawn] in drawing order; drawn - a valid y whose colour (and trail
        ;; size, NA never kept) the scale keeps; a valid y not drawn breaks the line
        group-points (fn [g] (let [points (->> (:rows g)
                                                (map (fn [r] [(x-of r) (y-of r) (when color-of (color-of r)) (when sized (size-of r))]))
                                                (filter #(data/valid-value? (first %))))
                                   ordered (if (and x-order? (data/sortable? (map first points))) (sort-by first points) points)]
                               (mapv (fn [[_ y c s :as p]]
                                       (conj p (and (data/valid-value? y) (or (not gradient?) (color-kept? c)) (or (not sizes?) (size-kept? s)))))
                                     ordered)))
        pointed-panels (update-vals split-panels (fn [gs] (mapv #(assoc % ::points (group-points %)) gs)))
        valid-y? (fn [p] (data/valid-value? (second p)))
        ;; a line sized by one value: judged by its first drawn point's size
        size-value (fn [g] (some-> (first (filter peek (::points g))) (nth 3)))
        [panels ^long size-dropped] (if (and sized (not sizes?))
                                      (group/kept-marks :size env size-value #(count (filter valid-y? (::points %))) pointed-panels)
                                      [pointed-panels 0])
        ;; a line broken at every point draws nothing: left out (its points counted)
        drawn-line? (fn [g] (or (not (or gradient? sizes?)) (some peek (::points g))))
        broken (reduce m/long-add 0 (for [g (mapcat val panels) p (::points g) :when (and (valid-y? p) (not (peek p)))] 1))
        line-data (update-vals panels
                               (fn [gs]
                                 {:lines (into [] (comp (filter drawn-line?)
                                                        (map (fn [g] (let [pts (::points g)]
                                                                       (cond-> {:x (mapv first pts)
                                                                                :y (mapv #(when (peek %) (second %)) pts)
                                                                                :aesthetics (:aesthetics g)}
                                                                         gradient? (assoc :colors (mapv #(nth % 2) pts))
                                                                         sizes? (assoc :sizes (mapv #(nth % 3) pts))
                                                                         (and sized (not sizes?)) (assoc :size-value (size-value g)))))))
                                               gs)
                                  :style st}))
        ;; the statistics read drawn points only
        drawn-values (fn [^long i] (update-vals panels (fn [gs] (for [g gs :when (drawn-line? g) p (::points g) :when (peek p)] (nth p i)))))
        data (if (stacked-positions position)
               (update-vals line-data #(update % :lines stacked-lines position))
               line-data)
        negative? (some m/neg? (mapcat (fn [{:keys [lines]}] (mapcat :y0 lines)) (vals data)))
        stacked-from-zero? (and (#{:stack :normalize} position) (not negative?))
        color-stats (when gradient?
                      (group/continuous-color-stats continuous (drawn-values 2) plot-scales))
        size-stats (when sized
                     (group/continuous-stats :size sized (if sizes?
                                                           (drawn-values 3)
                                                           (update-vals data #(map :size-value (:lines %))))
                                             plot-scales))]
    (into (data/padding-stats {:x data/axis-padding :y (if stacked-from-zero? [0.0 0.05] data/axis-padding)}
                              (when stacked-from-zero? {:y [0.0 nil]})
                              options)
          {:data data
           :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                                (:extents color-stats)
                                (:extents size-stats)
                                (into {} (keep (fn [[panel {:keys [lines]}]]
                                                 (let [xs (mapcat :x lines) ys (remove nil? (concat (mapcat :y lines) (mapcat :y0 lines)))]
                                                   (when (seq ys)
                                                     [panel {:x (data/extent :x xs) :y (data/extent :y ys)}]))))
                                      data))
           :axis-titles (when (= :normalize position) {:y "Proportion"})
           :na (cond-> (group/na-dims panels) (:na color-stats) (conj :color) (:na size-stats) (conj :size))
           :dropped (cond-> (assoc dropped :x (data/invalid-count x-of rows) :y (data/invalid-count y-of rows))
                      gradient? (assoc :color broken)
                      sized (assoc :size (if sizes? broken size-dropped)))})))

(defn- group-style
  "Gives the style a line is drawn with.

  Parameters:

  - `style` (map): the chart's resolved style.
  - `line` (map): a computed line or band - `:aesthetics`, optional `:size-value`.
  - `scales` (map): the finalized scales.
  - `theme` (map): the merged theme.

  Returns `style` with the group's aesthetics applied (see [[cljplot2.data.group/style]]) and the width from an ordinal `:size` level or from `:size-value` under the linear size scale (NA - the style width); `nil` for a value the scale doesn't draw (outside its domain without `:as-na` / `:clamp`)."
  [style line scales theme]
  (when-let [aesthetic-st (group/style style (:aesthetics line) scales theme)]
    (let [st (cond-> aesthetic-st (:size aesthetic-st) (-> (assoc :line-width (:size aesthetic-st)) (dissoc :size)))]
      (if (contains? line :size-value)
        (when-let [w (if-let [sc (:size scales)]
                       ((aesthetic/attribute-fn :size sc (:line-width st)) (:size-value line))
                       (:line-width st))]
          (assoc st :line-width w))
        st))))

(defn- value-colors
  "Gives the colours of colour values.

  Parameters:

  - `values` (sequence): the colour values; `nil` - NA.
  - `scale` (map or `nil`): the finalized colour scale; `nil` - every value NA.
  - `theme` (map): the merged theme - `[:palette :na]`.

  Returns a vector of colours: a value's colour under the scale, the NA colour for `nil` (`nil` for a value the scale doesn't draw - outside its domain without `:as-na` / `:clamp`)."
  [values scale theme]
  (let [na (get-in theme [:palette :na])
        color-of (if scale (aesthetic/attribute-fn :color scale na) (constantly na))]
    (mapv #(if (nil? %) na (color-of %)) values)))

(defn- segment-colors
  "Gives the colours of the segments of a line coloured along its length.

  Parameters:

  - `colors` (vector): the points' colour values; `nil` - NA.
  - `scale` (map or `nil`): the finalized colour scale.
  - `theme` (map): the merged theme.

  Returns a vector of colours, segment `i` - the colour of the mean of values `i` and `i + 1`, the NA colour when either is NA (see [[value-colors]])."
  [colors scale theme]
  (value-colors (map (fn [a b] (when (and (some? a) (some? b)) (m/* 0.5 (m/+ (double a) (double b))))) colors (rest colors))
                scale theme))

(defn- lerp-values
  "Interpolates values linearly at target positions.

  Parameters:

  - `anchors` (vector): strictly monotone positions of the values.
  - `values` (vector): the values at `anchors`; `nil` - NA.
  - `targets` (sequence): positions to interpolate at, running in the direction of `anchors`.

  Returns a vector of values at `targets` (one walk over both): `nil` where an enclosing value is `nil`; targets outside take the nearest segment."
  [anchors values targets]
  (let [reversed? (m/> (double (first anchors)) (double (peek anchors)))
        [ps vs ordered-targets] (if reversed? [(vec (rseq anchors)) (vec (rseq values)) (reverse targets)] [anchors values targets])
        last-segment (m/long-sub (count ps) 2)
        lerped (loop [i 0
                      remaining (seq ordered-targets)
                      acc []]
                 (if-not remaining
                   acc
                   (let [t (double (first remaining))]
                     (if (and (m/< i last-segment) (m/> t (double (ps (m/long-inc i)))))
                       (recur (m/long-inc i) remaining acc)
                       (let [a (vs i)
                             b (vs (m/long-inc i))
                             p0 (double (ps i))
                             p1 (double (ps (m/long-inc i)))
                             f (if (m/== p0 p1) 0.0 (m// (m/- t p0) (m/- p1 p0)))]
                         (recur i (next remaining) (conj acc (when (and (some? a) (some? b))
                                                               (m/+ (double a) (m/* f (m/- (double b) (double a))))))))))))]
    (if reversed? (vec (rseq lerped)) lerped)))

(defn- curve-options
  "Gives the curve options of a line.

  Parameters:

  - `options` (map): the chart's options - `:interpolator`, `:interpolator-params`, `:curve`, `:sample-step`.
  - `flipped?` (boolean): does x run along the pixel y axis?

  Returns the curve options with `:along` - `:y` when flipped, else `:x` (see [[cljplot2.graphics.curve]])."
  [options flipped?]
  (assoc (select-keys options [:interpolator :interpolator-params :curve :sample-step]) :along (if flipped? :y :x)))

(defn- drawn-runs
  "Gives the drawn point runs of a curved line.

  Parameters:

  - `index-runs` (sequence): runs of point indices.
  - `px`, `py` (vectors): the points' pixel coordinates.
  - `opts` (map): the curve options (see [[curve-options]]).
  - `aesthetics` (map): the line's group, named in errors.

  Returns a vector of pixel runs, each the curve through its points as drawn (see [[cljplot2.graphics.curve/curve-points]]).

  Throws `ex-info {:key :interpolator}` naming the group for a curve error of the interpolator."
  [index-runs px py opts aesthetics]
  (try (mapv (fn [run] (mapv curve/drawn-point (curve/curve-points (mapv (fn [i] [(px i) (py i)]) run) opts))) index-runs)
       (catch clojure.lang.ExceptionInfo e
         (if (= :interpolator (:key (ex-data e)))
           (let [message (:message (first (:errors (ex-data e))))]
             (schema/fail! (str message (when (seq aesthetics) (str " Group: " (pr-str aesthetics) ".")))
                           (cond-> (dissoc (ex-data e) :errors) (seq aesthetics) (assoc :group aesthetics))))
           (throw e)))))

(def ^{:private true :const true :tag 'double} color-chunk
  "Pixels of a sampled gradient line drawn in one colour (colours change in steps this long, so the segments of a step join into one path)."
  4.0)

(defn- chunk-colors
  "Gives the colour of every segment of a curved run of a line coloured along its length.

  Parameters:

  - `points` (vector): the run's sampled pixel points.
  - `anchors`, `values` (vectors): the run's data points' pixel positions along `a` and their colour values (see [[lerp-values]]).
  - `a` (long): the pixel axis index of x - 0 or 1.
  - `scale` (map or `nil`): the finalized colour scale.
  - `theme` (map): the merged theme.

  Returns a vector of segment colours: the colour value interpolated at the middle of the segment's [[color-chunk]] (chunks by length along the curve)."
  [points anchors values a scale theme]
  (let [lengths (polyline/cumulative-lengths points)
        segment-count (m/long-dec (count points))
        chunk-of (mapv #(long (m/quot (double (lengths %)) color-chunk)) (range segment-count))
        chunks (partition-by chunk-of (range segment-count))
        middles (map (fn [segments] (let [i (long (nth segments (m/long-quot (count segments) 2)))]
                                      (m/* 0.5 (m/+ (double (nth (points i) a)) (double (nth (points (m/long-inc i)) a))))))
                     chunks)
        colors (value-colors (lerp-values anchors values middles) scale theme)]
    (vec (mapcat (fn [segments color] (repeat (count segments) color)) chunks colors))))

(declare draw-gradient-bridges!)

(defn- draw-gradient!
  "Strokes a line coloured along its length.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `px`, `py` (vectors): the data points' pixel coordinates.
  - `colors` (vector): the points' colour values (numbers; `nil` - NA).
  - `index-runs`, `point-runs`: the runs as point indices and as drawn pixel points.
  - `curved?` (boolean): are the runs curves? Then segments take colour values interpolated along `along` (`:x` / `:y`) in [[color-chunk]] steps (see [[chunk-colors]]); else segment colours (see [[segment-colors]]).
  - `st` (map): the line's style.
  - `gaps` (keyword): `:bridge` - bridges in the colour of their end values (see [[draw-gradient-bridges!]]).
  - `scales` (map): the finalized scales - `:color`.
  - `theme` (map): the merged theme."
  [canvas px py colors index-runs point-runs curved? along st gaps scales theme]
  (let [a (if (= :y along) 1 0)]
    (if curved?
      (doseq [[run points] (map vector index-runs point-runs)
              :when (m/> (count points) 1)]
        (polyline/draw-segments! canvas (mapv first points) (mapv second points)
                                 (chunk-colors points (mapv #(if (m/zero? a) (px %) (py %)) run) (mapv colors run) a
                                               (:color scales) theme)
                                 st))
      (polyline/draw-segments! canvas px py (segment-colors colors (:color scales) theme) st))
    (draw-gradient-bridges! canvas px py colors index-runs st gaps scales theme)))

(defn- draw-arc-gradient!
  "Strokes a line coloured along its length in a polar plot with `:arcs`.

  Each data segment between consecutive points of `index-runs` (range points `range-points`) is densified along the circle of `geometry` and stroked in the segment's colour (see [[segment-colors]]); bridges as [[draw-gradient!]] (pixel points `px` / `py`).

  Parameters: `canvas`, the polar `geometry`, `range-points` (vector), `px`, `py`, `colors`, `index-runs`, `st`, `gaps`, `scales`, `theme` - as [[draw-gradient!]]."
  [canvas geometry range-points px py colors index-runs st gaps scales theme]
  (let [segment-color (segment-colors colors (:color scales) theme)]
    (doseq [run index-runs
            [i j] (partition 2 1 run)
            :let [points (mapv curve/drawn-point (coord/line-path geometry [(range-points i) (range-points j)] true))]]
      (polyline/draw-segments! canvas (mapv first points) (mapv second points)
                               (vec (repeat (m/long-dec (count points)) (segment-color i))) st)))
  (draw-gradient-bridges! canvas px py colors index-runs st gaps scales theme))

(defn- draw-gradient-bridges!
  "Draws the bridges between the runs of a line coloured along its length.

  Parameters: `canvas`; `px`, `py` - the points' pixel coordinates; `colors` - their colour values; `index-runs`; `st` - the line style (`:bridge`); `gaps` - nothing drawn unless `:bridge`; `scales`; `theme`.

  A bridge is straight, in the colour of its end values (see [[segment-colors]]), in the bridge style."
  [canvas px py colors index-runs st gaps scales theme]
  (when (= :bridge gaps)
    (doseq [[r0 r1] (map vector index-runs (rest index-runs))
            :let [color (first (segment-colors [(colors (peek r0)) (colors (first r1))] (:color scales) theme))]]
      (polyline/stroke-runs! canvas [[[(px (peek r0)) (py (peek r0))] [(px (first r1)) (py (first r1))]]]
                             (polyline/bridge-style (assoc st :color color) (:bridge st))))))

(defn stacked-band-points
  "Gives the drawn pixel bands of the stacked lines of a panel.

  Parameters:

  - `lines` (sequence): the stacked lines (see [[stacked-lines]], one shared x).
  - `position` (keyword): the stacked position.
  - `scales` (map): the finalized scales.
  - `opts` (map): curve options - `:interpolator` (with its parameters, `:sample-step`, `:along`).

  Returns a vector of `[upper lower]` pixel runs per line. Without an interpolator - the data boundaries; with one the groups' heights are sampled on one pixel grid (see [[cljplot2.graphics.curve/sample-columns]], signs kept) and stacked there again by `position` - `:wiggle` on its baseline interpolated from the data (its offsets accumulate per index) - so adjacent bands share their boundaries and no band turns inside out."
  [lines position scales opts]
  (let [projection (scale/projection scales)
        pixel (fn [vx vy] (let [^Vec2 p (projection vx vy)] (curve/drawn-point [(.x p) (.y p)])))]
    (if-not (:interpolator opts)
      (mapv (fn [{:keys [x y y0]}] [(mapv pixel x y) (mapv pixel x y0)]) lines)
      (let [flipped? (scale/flipped? scales)
            x-fwd (scale/forward-fn (:x scales))
            y-fwd (scale/forward-fn (:y scales))
            x-pixels (mapv x-fwd (:x (first lines)))
            [grid hs] (curve/sample-columns x-pixels (mapv :heights lines) opts true)
            stacks (if (= :wiggle position)
                     (let [[_ [base]] (curve/sample-columns x-pixels [(:y0 (first lines))] opts false)]
                       (mapv (fn [ys] (mapv (fn [[^double y0 ^double y1] ^double b] [(m/+ y0 b) (m/+ y1 b)]) ys base))
                             (position/stack-values :stack hs)))
                     (position/stack-values position hs))
            point (fn [t v] (let [q (double (y-fwd v))] (curve/drawn-point (if flipped? [q t] [t q]))))]
        (mapv (fn [ys] [(mapv (fn [t [_ y1]] (point t y1)) grid ys) (mapv (fn [t [y0 _]] (point t y0)) grid ys)]) stacks)))))

(defn- draw-stacked!
  "Draws the stacked lines of a panel (see [[stacked-band-points]]): every band filled - `gradient` along `axis`, else `:fill` or the group colour faded by `:alpha` - then every upper boundary stroked.

  Parameters: `canvas`; `lines` - the stacked lines; `style` - the chart's resolved style; `position`; `opts` - curve options; `gradient` (function or `nil`) and `axis` - the fill gradient; `scales`; `theme`.

  Groups outside an aesthetic domain are stacked but not drawn."
  [canvas lines style position opts gradient axis scales theme]
  (let [bands (vec (keep (fn [[line [upper lower]]]
                           (when-let [st (group-style style line scales theme)]
                             [upper lower st]))
                         (map vector lines (stacked-band-points lines position scales opts))))]
    ;; all areas first, then the boundaries (as density): no band covers a stroke
    (doseq [[upper lower st] bands
            :let [fill (polyline/area-color st true)]]
      (polyline/fill-area! canvas upper lower fill gradient axis scales
                           (draw/pattern-fill st (if gradient (gradient 0.5) fill))))
    (doseq [[upper _ st] bands]
      (polyline/stroke-runs! canvas [upper] st))))

(defn- arc-trail-run
  "Gives the points of a trail run in a polar plot with `:arcs`.

  Parameters:

  - `geometry` (map): the polar geometry.
  - `range-points` (vector): the points' range points.
  - `widths` (vector): the points' widths in pixels.
  - `run` (sequence): the run's point indices.

  Returns `[xs ys widths]` (vectors): each segment between consecutive range points densified along the circle (see [[cljplot2.scale.coord/line-path]]), the width linear along its points."
  [geometry range-points widths run]
  (let [start (coord/line-path geometry [(range-points (first run))] false)
        points (reduce (fn [acc [i j]]
                         (let [seg (rest (coord/line-path geometry [(range-points i) (range-points j)] true))
                               n (double (count seg))
                               w0 (double (widths i))
                               w1 (double (widths j))]
                           (into acc (map-indexed (fn [^long k [x y]] [x y (m/+ w0 (m/* (m/- w1 w0) (m// (m/inc k) n)))]) seg))))
                       [(conj (first start) (widths (first run)))]
                       (partition 2 1 run))]
    [(mapv first points) (mapv second points) (mapv #(nth % 2) points)]))

(defn- draw-trail!
  "Fills the trail of a line (see [[cljplot2.graphics.polyline/trail-shape]]), one shape per run.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `trail` (map): `:line` (`:sizes` - the points' size values), `:px` / `:py` (pixel coordinates), `:index-runs`, `:arcs` (`{:geometry g :range-points rp}` in a polar plot with `:arcs` - segments follow the circle, see [[arc-trail-run]]; else `nil`).
  - `st` (map): the line's style - `:color`, `:line-width`.
  - `scales` (map): the finalized scales - `:size`.

  The width at every point is its size value under the size scale, else `:line-width`. Nothing is drawn for a `:none` colour."
  [canvas {:keys [line px py index-runs arcs]} st scales]
  (let [width (double (:line-width st))
        width-of (if-let [sc (and (:sizes line) (:size scales))]
                   (let [f (aesthetic/attribute-fn :size sc width)] #(double (or (f %) width)))
                   (constantly width))
        widths (if (:sizes line) (mapv width-of (:sizes line)) (vec (repeat (count px) width)))]
    (when-not (theme/none? (:color st))
      (c2d/set-color canvas (:color st))
      (doseq [run index-runs
              :let [[xs ys ws] (if arcs
                                 (arc-trail-run (:geometry arcs) (:range-points arcs) widths run)
                                 [(mapv px run) (mapv py run) (mapv widths run)])]]
        (c2d/shape canvas (polyline/trail-shape xs ys ws) false)))))

(defn- base-run
  "Gives the base points of a run on an area baseline.

  Parameters:

  - `run` (sequence): pixel points `[x y]`.
  - `area` (map): `:baseline` (pixels), `:horizontal` - the baseline runs horizontally.

  Returns a vector of the points moved onto the baseline."
  [run {:keys [baseline horizontal]}]
  (mapv (fn [[x y]] (if horizontal [x baseline] [baseline y])) run))

(defn draw
  "Draws the lines of one panel, the area fill first.

  Parameters:

  - `chart` (map): the chart value - `:options` (curves, `:gaps`, `:fill-gradient`, `:stroke-alpha-factor`, `:trail`, arrows).
  - `computed` (map): `:lines`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel's `:w`, `:h`.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  `nil` y values and values the scales can't place break a line (bridged with `:gaps :bridge`). The fill baseline is y = 0 clamped into the panel along the y scale's direction (see [[cljplot2.graphics.curve/area-baseline]]); stacked lines fill their bands; `:fill-gradient` colours areas along `:fill-gradient-axis`.

  Throws `ex-info {:key :curve}` for `:curve` on a line coloured along its length, `{:key :arrow}` for arrows or end gaps on a line that isn't plainly stroked."
  [{:keys [options]} {:keys [lines style]} canvas {:keys [w h]} scales env]
  (when (seq lines)
    (let [projection (scale/projection scales)
          geometry (scale/coord-geometry scales)
          y-scale (:y scales)
          y-horizontal? (scale/horizontal? :y y-scale)
          area {:baseline (curve/area-baseline y-scale w h) :horizontal (not y-horizontal?)}
          opts (curve-options options (scale/flipped? scales))
          curved? (curve/curved? opts)
          gaps (data/option options :gaps :break)
          gradient (some-> (:fill-gradient options) polyline/gradient-fn)
          axis (data/option options :fill-gradient-axis :x)]
      (when (and curved? (:curve opts) (some :colors lines))
        (schema/fail! "A line colored along its length can't take :curve (use :interpolator)."
                      {:key :curve :value (:curve opts)}))
      (when (and (some options [:arrow :start-gap :end-gap])
                 (or (some :y0 lines) (some :colors lines) gradient (not (theme/none? (data/option style :fill :none)))))
        (schema/fail! "Arrows and end gaps need a plain stroked line (no fill, gradient, colour along the length or stacking)."
                      {:key :arrow}))
      (if (some :y0 lines)
        (draw-stacked! canvas lines style (:position options) opts gradient axis scales (:theme env))
        (doseq [{:keys [x y aesthetics colors] :as line} lines
                :let [;; colour values as numbers for interpolation (dates - epoch milliseconds, as the colour scale)
                      color-keys (some->> colors (mapv #(some-> % data/x-key)))
                      group-st (group-style style line scales (:theme env))
                      line-st (if (and group-st (:stroke-alpha-factor options) (not (theme/none? (:color group-st))))
                                (update group-st :color draw/faded (:stroke-alpha-factor options))
                                group-st)]
                :when line-st]
          (let [points (mapv (fn [vx vy] (if (nil? vy) (Vec2. ##NaN ##NaN) (projection vx vy))) x y)
                px (mapv #(.x ^Vec2 %) points)
                py (mapv #(.y ^Vec2 %) points)
                index-runs (polyline/finite-runs px py)
                ;; polar with :arcs: the range points, joined along the circle
                range-points (when (:arcs geometry)
                               (let [rp (scale/range-projection scales)]
                                 (mapv (fn [vx vy] (if (nil? vy) [##NaN ##NaN] (rp vx vy))) x y)))
                point-runs (cond
                             curved? (drawn-runs index-runs px py opts aesthetics)
                             range-points (mapv (fn [run] (mapv curve/drawn-point (coord/line-path geometry (mapv range-points run) true)))
                                                index-runs)
                             :else (mapv (fn [run] (mapv (fn [i] (curve/drawn-point [(px i) (py i)])) run)) index-runs))
                st (assoc line-st :fill (polyline/area-color group-st false))]
            (cond
              (:trail options)
              (draw-trail! canvas {:line line :px px :py py :index-runs index-runs
                                   :arcs (when range-points {:geometry geometry :range-points range-points})}
                           st scales)
              color-keys
              (do (polyline/fill-runs-to-baseline! canvas point-runs (:baseline area) (:horizontal area) (:fill st)
                                                   (draw/pattern-fill st (:fill st)))
                  (if range-points
                    (draw-arc-gradient! canvas geometry range-points px py color-keys index-runs st gaps scales (:theme env))
                    (draw-gradient! canvas px py color-keys index-runs point-runs curved? (:along opts) st gaps scales (:theme env))))
              gradient
              (do (doseq [run point-runs :when (m/> (count run) 1)]
                    (polyline/fill-area! canvas run (base-run run area) nil gradient axis scales
                                         (draw/pattern-fill st (gradient 0.5))))
                  (curve/draw-runs! canvas point-runs (assoc st :fill :none) nil gaps))
              (some options [:arrow :start-gap :end-gap])
              (doseq [run point-runs :when (m/> (count run) 1)]
                (arrow/draw-run! canvas run st [(:arrow options) (:start-gap options) (:end-gap options)]))
              :else
              (curve/draw-runs! canvas point-runs st area gaps))))))))

(defn legend
  "Draws the chart's key glyph: a horizontal line through the middle of the key's box; with an area (`:fill`, a stacked position or `:fill-gradient` - its middle colour) a filled square outlined in the line style (see [[cljplot2.graphics.polyline/key-glyph!]]).

  Parameters:

  - `chart` (map): the chart value - `:options` `:fill-gradient`, `:position`.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`, `:pattern`, `:size` (the line width).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (cond-> (theme/key-style env chart attributes [:color :dash :pattern])
             (:size attributes) (assoc :line-width (:size attributes)))
        area (if-let [g (:fill-gradient options)]
               ((polyline/gradient-fn g) 0.5)
               (polyline/area-color st (contains? stacked-positions (:position options))))]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
