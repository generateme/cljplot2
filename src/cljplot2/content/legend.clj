(ns cljplot2.content.legend
  "Legends and colour bars, derived from the aesthetic scales (see [[cljplot2.scale.aesthetic]]) and drawn as grid content.

  [[legends]]: one legend per aesthetic dimension with entries (ordinal scales, linear size) and a color bar for a
  linear or binned `:color` scale (`:legend false` on the plot or the scale hides them; identity scales have none). Dimensions mapped by every chart with the same
  accessor (equal keyword or the same function) and with equal entry labels merge into one legend whose entries
  combine their attributes. Title: the scale's `:title` when given (`nil` - none), else the accessor's name when it
  is a keyword.

  Content `{:kind :legend :title t :entries [{:label l :attributes {dim attribute}}] :charts [{:chart i :dims [dim]}]}`:
  title (`:legend-title` role), then one row per entry - a key box, gap, label (`:legend-label` role). A key box is
  at least theme `[:legend :key-size]` square and grows to the entry's mapped `:size`; a row is as tall as its key
  and label. Entries that don't fit the available height (the item box from the previous layout round) wrap into
  further columns. Each chart `i` (index into the plot's `:charts`) draws its glyph into every key with its `:legend`
  function, in order (overlay), getting only the attributes of the dimensions `dims` it maps. Theme `:legend`
  `:fill`, `:border-color`, `:border-width` box it (a `:none` border takes no space).

  Content `{:kind :colorbar :title t :scale spec :ticks [v ...] :labels [s ...]}`: title, then a vertical gradient bar
  (top = domain maximum, width theme `[:legend :bar-width]`, default `:key-size`) with tick marks and labels
  (`:legend-label`); a binned scale's bar is one equal block per bin, ticked at the inner breaks. Its length is theme `[:legend :bar-length]`: `:stretch` (default) - the height left in its stack
  column (at least 3 x `:key-size`, else it starts a new column); a number - that length, clamped to the height
  available.

  Content `{:kind :legends :legends [legend ...]}`: legends (`:legend` or `:colorbar`) stacked top-down with theme `[:legend :gap]` between them;
  a legend that doesn't fit below the previous one starts a new stack column to the right. [[legend-boxes]] gives
  where each legend is drawn.

  Functions: [[legends]], [[legend-charts]], [[provisional-legends]], [[legend-boxes]]; schemas [[legend-schema]],
  [[colorbar-schema]], [[legends-schema]]; methods of [[cljplot2.graphics.measure/validate-content]],
  [[cljplot2.graphics.measure/natural-size]] and [[cljplot2.graphics.draw/draw-content]] for `:legend`, `:colorbar`
  and `:legends`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [wadogo.scale :as s]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as position]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.java Array]
           [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; derivation

(defn- dim-legend
  "Gives the legend candidate of an aesthetic dimension.

  Parameters:

  - `k` (keyword): the aesthetic.
  - `scale-specs` (map): the plot's scale specs.
  - `mappings` (sequence of maps): the charts' mappings.

  Returns `{:dim :spec :entries :accessors}` - the scale spec, its legend entries (a vector, indexed) and the set of
  accessors charts map `k` with; `nil` without a scale or without entries."
  [k scale-specs mappings]
  (when-let [spec (get scale-specs k)]
    (when-let [entries (seq (aesthetic/legend-entries k spec))]
      {:dim k :spec spec :entries (vec entries) :accessors (set (keep #(get % k) mappings))})))

(defn- merge-key
  "Gives the key legend candidates merge by: candidates with equal keys share a legend.

  Parameters:

  - `candidate` (map): a legend candidate - `:dim`, `:entries` (`:label` read), `:accessors` (the charts' accessors of the dimension).

  Returns `[accessor labels]` when every chart maps the dimension with one accessor, else `[::own dim]` (a legend of its own)."
  [{:keys [dim entries accessors]}]
  (if (m/one? (count accessors)) [(first accessors) (mapv :label entries)] [::own dim]))

(defn- members-title
  "Gives the title of a legend of merged candidates.

  Parameters:

  - `members` (sequence): the merged candidates, `:spec` and `:accessors` read.

  Returns the first member scale's `:title` when one is given (a given `nil` - no title); else the name of the first member's single keyword accessor; `nil` otherwise."
  [members]
  (if-let [titled (first (filter #(contains? (:spec %) :title) members))]
    (get-in titled [:spec :title])
    (let [accessors (:accessors (first members))
          first-accessor (first accessors)]
      (when (and (m/one? (count accessors)) (keyword? first-accessor)) (name first-accessor)))))

(defn- legend-content
  "Gives the `:legend` content of merged candidates.

  Parameters:

  - `members` (sequence of maps): candidates (see [[dim-legend]]) with equal entry labels.
  - `theme` (map): the merged theme (the NA colour).
  - `charts`, `mappings` (vectors): the legend charts and their mappings (see [[legend-charts]]).

  Returns `{:kind :legend :title :entries :charts}` - one entry per entry label with the attributes of every member's
  dimension, and the charts whose glyphs the keys draw (mapping or computing a member's dimension, with a `:legend`
  function)."
  [members theme charts mappings]
  (let [dims (mapv :dim members)
        na {:color (get-in theme [:palette :na])}
        ;; read once per entry
        attribute-fns (mapv (fn [{:keys [dim spec]}] (aesthetic/attribute-fn dim spec (na dim))) members)]
    {:kind :legend
     :title (members-title members)
     :entries (mapv (fn [i]
                      {:label (:label (get-in (first members) [:entries i]))
                       :attributes (into {} (keep (fn [[{:keys [dim entries]} f]]
                                                    (when-some [a (f (:value (entries i)))] [dim a])))
                                         (map vector members attribute-fns))})
                    (range (count (:entries (first members)))))
     ;; glyphs of the charts mapping a dimension or computing it (a categorical computed color, e.g. partition names)
     :charts (vec (keep-indexed (fn [i [chart mapping]]
                                  (let [computed (aesthetic/computed-aesthetics chart mapping)
                                        mapped (filterv #(or (some? (get mapping %)) (contains? computed %)) dims)]
                                    (when (and (:legend chart) (seq mapped)) {:chart i :dims mapped})))
                                (map vector charts mappings)))}))

(defn- with-midpoint-tick
  "Gives colour bar ticks with a linear scale's midpoint added.

  Ticks closer to the midpoint than half the tick step are dropped (their labels would collide); the other labels stay
  as they were.

  Parameters:

  - `ticks` (vector): `[value label]` pairs, ascending.
  - `midpoint`: the scale's `:midpoint`, `nil` - none.
  - `midpoint-label` (string): its label.
  - `lo`, `hi` (numbers): the domain.

  Returns the pairs with `[midpoint midpoint-label]` in order when the midpoint lies in `[lo hi]` and isn't a tick,
  else `ticks` as given."
  [ticks midpoint midpoint-label lo hi]
  (if (and (some? midpoint) (m/<= (double lo) (double midpoint) (double hi)) (not-any? #(m/== (double (first %)) (double midpoint)) ticks))
    (let [mid (double midpoint)
          step (if (m/> (count ticks) 1) (m/- (double (first (second ticks))) (double (first (first ticks)))) (m/- (double hi) (double lo)))]
      (vec (sort-by first (conj (filterv #(m/>= (m/abs (m/- (double (first %)) mid)) (m/* 0.5 step)) ticks)
                                [mid midpoint-label]))))
    ticks))

(defn- date-colorbar-pairs
  "Gives the ticks of the colour bar of a linear colour scale over dates.

  An explicit `:ticks` vector of dates gives those inside the domain, else nice ticks of a wadogo `:datetime` scale
  (at least two inside, as an axis; a count from `:ticks`) over the domain as local date-times (see
  [[cljplot2.data.rows/->local-date-time]]). Ticks are in the data's type (`LocalDate` data - dates: sub-day ticks of
  a short domain collapse to their day; zoned values in their zone - a tick in a daylight-saving fold takes the
  earlier offset), labelled by `:formatter` (a function of such a value) or as wadogo labels them.

  Parameters:

  - `spec` (map): the scale spec - `:temporal-sample`, `:formatter`, `:ticks`.
  - `lo`, `hi` (doubles): the domain in epoch milliseconds.
  - `tick-count` (long): the tick count without a scale `:ticks` count (theme `[:legend :bar-ticks]`).

  Returns a vector of `[key label]`, `key` - the tick's epoch milliseconds."
  [{:keys [temporal-sample formatter ticks]} ^double lo ^double hi tick-count]
  (let [as-data #(data/local-date-time->like % temporal-sample)
        local-at #(data/->local-date-time (data/key->x % temporal-sample))
        scale-of (fn [n] (s/scale :datetime {:domain [(local-at lo) (local-at (if (m/== lo hi) (m/inc hi) hi))] :ticks n}))
        in-domain #(m/<= lo (double (data/x-key (as-data %))) hi)
        {ws :scale times :ticks} (cond
                                   (sequential? ticks) {:scale (scale-of tick-count)
                                                        :ticks (map data/->local-date-time (aesthetic/explicit-ticks ticks lo hi))}
                                   (m/== lo hi) {:scale (scale-of tick-count) :ticks [(local-at lo)]}
                                   :else (position/ticks-inside scale-of (or ticks tick-count) in-domain))
        ;; read by the keys and the labels
        values (into [] (comp (map as-data) (distinct)) times)
        labels (if formatter
                 (map #(str (formatter %)) values)
                 (position/tick-labels ws {:kind :datetime} (mapv data/->local-date-time values)))]
    (mapv vector (map data/x-key values) labels)))

(defn- colorbar-content
  "Gives the `:colorbar` content of a linear or binned colour scale.

  Ticks: linear - nice ticks of the domain (at least two; scale `:ticks` a count of them; whole numbers with
  `:integer-ticks`, see [[cljplot2.scale.aesthetic/integer-ticks]] - a log bar may keep one), labelled as axis ticks,
  and the `:midpoint` (see [[with-midpoint-tick]]; labelled by `:formatter` or [[cljplot2.data.levels/break-labels]])
  - or an explicit `:ticks` vector exactly as given (see [[cljplot2.scale.aesthetic/explicit-ticks]], no added
  midpoint); binned - the inner breaks, labelled by `:formatter` or [[cljplot2.data.levels/break-labels]]; a linear
  scale over dates - see [[date-colorbar-pairs]]. A one-value domain has one tick.

  Parameters:

  - `spec` (map): the colour scale spec.
  - `accessors` (set): the accessors charts map `:color` with (the title rule of [[members-title]]).
  - `tick-count` (long): the tick count without a scale `:ticks` count (theme `[:legend :bar-ticks]`).

  Returns `{:kind :colorbar :title :scale :ticks :labels}`."
  [spec accessors tick-count]
  (let [[^double lo ^double hi] (:domain spec)
        formatter (:formatter spec)
        ;; a log transform ticks at powers of ten, as a log axis
        scale-of (fn [n] (s/scale (if (= :log (:transform spec)) :log :linear)
                                  (cond-> {:domain (if (m/== lo hi) [lo (m/inc lo)] [lo hi]) :ticks n}
                                    formatter (assoc :formatter formatter))))
        explicit (:ticks spec)
        in-domain #(m/<= lo (double %) hi)
        ;; at least two ticks inside the domain, as an axis (see position/ticks-inside), unless given
        {ws :scale nice :ticks} (cond
                                  (:temporal-sample spec) nil
                                  (sequential? explicit) {:scale (scale-of tick-count)
                                                          :ticks (aesthetic/explicit-ticks explicit lo hi)}
                                  (m/== lo hi) {:scale (scale-of tick-count)}
                                  :else (cond-> (position/ticks-inside scale-of (or explicit tick-count) in-domain)
                                          (:integer-ticks spec)
                                          (update :ticks aesthetic/integer-ticks lo hi (or explicit tick-count))))
        axis-labels (fn [ts] (position/tick-labels ws {:kind :linear :formatter formatter} ts))
        ;; read by the ticks and the labels
        pairs (cond
                (:temporal-sample spec) (date-colorbar-pairs spec lo hi tick-count)
                (= :binned (:kind spec))
                (let [breaks (:breaks spec)
                      inner (subvec breaks 1 (m/long-dec (count breaks)))
                      labels (if formatter
                               (axis-labels inner)
                               (let [ls (levels/break-labels breaks)] (subvec ls 1 (m/long-dec (count ls)))))]
                  (mapv vector inner labels))
                (sequential? explicit) (mapv vector nice (axis-labels nice))
                (m/== lo hi) (mapv vector [lo] (axis-labels [lo]))
                :else (let [mid (:midpoint spec)]
                        (with-midpoint-tick (mapv vector nice (axis-labels nice)) mid
                          (when (some? mid) (if formatter (str (formatter mid)) (first (levels/break-labels [mid]))))
                          lo hi)))]
    {:kind :colorbar
     :title (members-title [{:spec spec :accessors accessors}])
     :scale spec
     :ticks (mapv first pairs)
     :labels (mapv second pairs)}))

(defn legend-charts
  "Gives the charts whose glyphs the legends draw, in key index order: the plot's charts, then its margin charts
  (`:top`, then `:right`; see `cljplot2.pipeline`).

  Parameters:

  - `plot` (map): a plot built up to its stats - `:charts`, `:split`, `:stats`, `:margin-stats`.

  Returns `{:charts [..] :mappings [..] :stats [..]}`, vectors indexed by a legend's `:chart`."
  [{:keys [charts split stats margin-stats]}]
  (let [sides (keep margin-stats [:top :right])]
    {:charts (into (vec charts) (mapcat :charts sides))
     :mappings (into (mapv :mapping (:charts split)) (mapcat :mappings sides))
     :stats (into (vec stats) (mapcat :stats sides))}))

(defn legends
  "Gives the legend contents of a plot.

  Keys draw the glyphs of [[legend-charts]] (margin charts included).

  Parameters:

  - `plot` (map): a plot built up to `:scale-specs` - `:scale-specs`, `:theme`, `:legend`.

  Returns a vector of contents in aesthetic order (`:color`, `:shape`, `:size`, `:dash`, `:pattern`), a colour bar
  last; empty when the plot's `:legend` is `false` or nothing with a legend is mapped."
  [{:keys [scale-specs theme legend] :as plot}]
  (if (false? legend)
    []
    (let [{:keys [charts mappings]} (legend-charts plot)
          groups (reduce (fn [acc candidate]
                           (let [k (merge-key candidate)
                                 i (first (keep-indexed (fn [i [gk]] (when (= gk k) i)) acc))]
                             (if i (update-in acc [i 1] conj candidate) (conj acc [k [candidate]]))))
                         [] (keep #(dim-legend % scale-specs mappings) aesthetic/dims))]
      (cond->> (mapv (fn [[_ members]] (legend-content members theme charts mappings)) groups)
        (let [spec (:color scale-specs)] (and (#{:linear :binned} (:kind spec)) (not (false? (:legend spec)))))
        (into [(colorbar-content (:color scale-specs) (set (keep :color mappings)) (get-in theme [:legend :bar-ticks]))])))))

;; provisional legends (before stats)

(def ^{:private true :const true :tag 'long} provisional-values
  "Most distinct mapped values a provisional legend lists."
  50)

(def ^:private provisional-tick-label
  "Stand-in tick label of a provisional color bar (its ticks are unknown before stats)."
  "00")

(defn provisional-legends
  "Gives the legend contents a plot is expected to get, before its stats.

  Used for estimating the panel size (see [[cljplot2.layout.plot-grid/estimated-panel-size]]): one per aesthetic
  mapped by a chart or computed by one (see [[cljplot2.scale.aesthetic/computed-aesthetics]]) - a color bar with a
  stand-in tick label for a linear or binned color (a computed one too), else a legend of the distinct mapped values
  (at most [[provisional-values]]); titles as [[legends]] gives them (the scale's `:title`, the computed title, an
  accessor keyword's name).

  Parameters:

  - `plot` (map): `:charts`, `:scales`, `:legend`.
  - `chart-inputs` (sequence): `[{:mapping :groups}]` of the split stage, in chart order.

  Returns a vector of contents; none for an identity scale, a scale with `:legend false`, or plot `:legend false`."
  [{:keys [charts scales legend]} chart-inputs]
  (if (false? legend)
    []
    (vec (for [k aesthetic/dims
               :let [own-spec (get scales k {})
                     computed (some (fn [[chart {:keys [mapping]}]]
                                      (let [aesthetics (aesthetic/computed-aesthetics chart mapping)] (when (contains? aesthetics k) aesthetics)))
                                    (map vector charts chart-inputs))
                     mapped (filter #(some? (get-in % [:mapping k])) chart-inputs)
                     accessors (set (map #(get-in % [:mapping k]) mapped))
                     values (into [] (comp (mapcat (fn [{:keys [mapping groups]}]
                                                     (map (data/row-accessor k (mapping k)) (mapcat val groups))))
                                           (remove nil?) (distinct) (take provisional-values))
                                  mapped)
                     continuous? (or (some? computed) (aesthetic/continuous-values? k values))
                     kind (when (or computed (seq mapped)) (aesthetic/inferred-kind k own-spec continuous?))
                     suggested (when computed (aesthetic/computed-scale-attributes (get computed k)))
                     spec (merge suggested own-spec)
                     legend-title (cond
                                    (contains? spec :title) (:title spec)
                                    (and (m/one? (count accessors)) (keyword? (first accessors))) (name (first accessors)))]
               :when (and kind (not= :identity kind) (not (false? (:legend spec))))]
           (if (and (= k :color) (#{:linear :binned} kind))
             {:kind :colorbar :title legend-title :scale {:kind :linear :domain [0.0 1.0]} :ticks [0.0]
              :labels [provisional-tick-label]}
             {:kind :legend :title legend-title :charts []
              :entries (mapv (fn [value] {:label (str value) :attributes {}})
                             (if (#{:linear :binned} kind) [provisional-tick-label] values))})))))

;; content

(def legend-schema
  "Schema of `{:kind :legend}` content: `:title`, `:entries` `[{:label :attributes}]`, `:charts` `[{:chart index :dims [dim]}]`."
  [:map
   (schema/nilable :title string?)
   [:entries [:vector [:map [:label string?] [:attributes :map]]]]
   [:charts [:vector [:map [:chart nat-int?] [:dims vector?]]]]])

(def colorbar-schema
  "Schema of `{:kind :colorbar}` content: `:title`, a linear or binned `:scale`, `:ticks` and one label per tick in `:labels`."
  (schema/with-rules
   [:map
    (schema/nilable :title string?)
    [:scale [:map [:kind [:enum :linear :binned]]]]
    [:ticks vector?]
    [:labels vector?]]
   [#(when-not (m/== (count (:ticks %)) (count (:labels %))) "should have one label per tick") [:labels]]))

(def legends-schema
  "Schema of `{:kind :legends}` content: `:legends`, a vector of [[legend-schema]] and [[colorbar-schema]] contents."
  [:map [:legends [:vector [:multi {:dispatch #(if (= :colorbar (:kind %)) :colorbar :legend)}
                            [:colorbar colorbar-schema]
                            [:legend legend-schema]]]]])

(defmethod measure/validate-content :legend [content] (schema/explain! [[legend-schema content [:content]]]))
(defmethod measure/validate-content :colorbar [content] (schema/explain! [[colorbar-schema content [:content]]]))
(defmethod measure/validate-content :legends [content] (schema/explain! [[legends-schema content [:content]]]))

(defn- title-label
  "Gives the label content of a legend title.

  Parameters:

  - `text`: the title text.

  Returns `{:kind :label :role :legend-title :text text}`."
  [text]
  {:kind :label :role :legend-title :text text})

(defn- entry-label
  "Gives the label content of a legend entry label or a colour bar tick label.

  Parameters:

  - `text`: the label text.

  Returns `{:kind :label :role :legend-label :text text}`."
  [text]
  {:kind :label :role :legend-label :text text})

(defn- legend-backdrop
  "Gives what legend texts are drawn over, for `:contrast` role colours.

  Parameters:

  - `theme` (map): the merged theme.

  Returns theme `[:legend :fill]`; the plot backdrop (see [[cljplot2.graphics.draw/plot-backdrop]]) when the fill is `:none`."
  [theme]
  (let [fill (get-in theme [:legend :fill])]
    (if (theme/none? fill) (draw/plot-backdrop theme) fill)))

(defn- legend-text
  "Gives a legend text's label content with its role's colour resolved.

  Parameters:

  - `content` (map): a label content, `:role` read.
  - `theme` (map): the merged theme.

  Returns `content` with `:color` - the role's colour over the legend backdrop (see [[legend-backdrop]])."
  [content theme]
  (assoc content :color (draw/text-color theme (:role content) (legend-backdrop theme))))

(defn- glyph-extent
  "Gives the key box side an entry needs for its glyph.

  Parameters:

  - `attributes` (map): the entry's attributes, `:size` read.

  Returns the mapped `:size` in pixels; 0 when no size is mapped."
  ^double [attributes]
  (let [size (:size attributes)] (if (number? size) (double size) 0.0)))

(defn- wrap-columns
  "Splits legend rows greedily into columns.

  Parameters:

  - `rows` (sequence of maps): `{:entry i :key-w :label-w :h}`, in order.
  - `max-h` (double): the most height a column may take.

  Returns a vector of columns (vectors of rows); each column holds at least one row, so a row taller than `max-h`
  gets a column of its own. Empty for no rows."
  [rows ^double max-h]
  (reduce (fn [cols {:keys [^double h] :as row}]
            (let [col (peek cols)
                  col-h (v/sum (map :h col))]
              (if (or (nil? col) (and (seq col) (m/> (m/+ col-h h) max-h)))
                (conj cols [row])
                (conj (pop cols) (conj col row)))))
          [] rows))

(defn- legend-geometry
  "Gives the layout of a legend, relative to its top-left corner.

  Parameters:

  - `content` (map): the legend content - `:title`, `:entries`.
  - `env` (map): `:theme`, `:canvas` (measuring).
  - `max-h` (double): the most height the legend may take (entries wrap into columns; `##Inf` - one column).

  Returns `{:w :h :border :title-h :columns :labels}`: `:columns` - `[{:x :key-w :rows [{:entry i :y :h}]}]`,
  `:labels` - the entry labels' natural sizes (a vector, indexed by entry)."
  [{:keys [title entries]} env ^double max-h]
  (let [{:keys [^double gap ^double key-size border-width border-color]} (get-in env [:theme :legend])
        border (if (theme/none? border-color) 0.0 (double (m/round (double border-width))))
        ^Vec2 title-size (if title (measure/natural-size (title-label title) env) (v/vec2 0.0 0.0))
        labels (mapv #(measure/natural-size (entry-label (:label %)) env) entries)
        rows (map-indexed (fn [i {:keys [attributes]}]
                            (let [key-w (m/max key-size (glyph-extent attributes))
                                  ^Vec2 label-size (labels i)]
                              {:entry i :key-w key-w :label-w (.x label-size) :h (m/max key-w (.y label-size))}))
                          entries)
        [placed ^double width] (reduce (fn [[acc ^double x] col]
                                         (let [key-w (v/mx (map :key-w col))
                                               label-w (v/mx (map :label-w col))
                                               ys (reductions m/+ 0.0 (map :h col))]
                                           [(conj acc {:x x :key-w key-w
                                                       :rows (mapv (fn [r y] {:entry (:entry r) :y y :h (:h r)}) col ys)})
                                            (m/+ x key-w gap label-w gap)]))
                                       [[] 0.0] (wrap-columns rows (m/- max-h (m/* 2.0 border) (.y title-size))))
        entries-w (m/max 0.0 (m/- width gap))
        entries-h (double (reduce m/max 0.0 (map (fn [column] (let [{:keys [^double y ^double h]} (peek (:rows column))] (m/+ y h))) placed)))]
    {:border border :title-h (.y title-size) :columns placed :labels labels
     :w (m/+ (m/* 2.0 border) (m/max (.x title-size) entries-w))
     :h (m/+ (m/* 2.0 border) (.y title-size) entries-h)}))

(defn- colorbar-geometry
  "Gives the layout of a colour bar with a bar length, relative to its top-left corner.

  Parameters:

  - `content` (map): the colour bar content - `:title`, `:labels`.
  - `env` (map): `:theme`, `:canvas` (measuring).
  - `length` (double): the bar length in pixels.

  Returns `{:w :h :border :title-h :bar-w :bar-len :labels :label-h}`: `:labels` - the tick labels' natural sizes
  (a vector), `:label-h` - the room above and below the bar for the end labels, together."
  [{:keys [title labels]} env ^double length]
  (let [{:keys [^double gap key-size bar-width ^double bar-tick-length border-width border-color]} (get-in env [:theme :legend])
        bar-w (double (or bar-width key-size))
        border (if (theme/none? border-color) 0.0 (double (m/round (double border-width))))
        ^Vec2 title-size (if title (measure/natural-size (title-label title) env) (v/vec2 0.0 0.0))
        sizes (mapv #(measure/natural-size (entry-label %) env) labels)
        ^Vec2 largest (measure/max-size sizes)
        label-h (.y largest)
        label-w (.x largest)]
    {:border border :title-h (.y title-size) :bar-w bar-w :bar-len length :labels sizes :label-h label-h
     :w (m/+ (m/* 2.0 border) (m/max (.x title-size) (m/+ bar-w bar-tick-length gap label-w)))
     :h (m/+ (m/* 2.0 border) (.y title-size) label-h length)}))

(def ^{:private true :const true :tag 'double} min-bar-keys
  "Shortest stretched colour bar, in key sizes (theme `[:legend :key-size]`)."
  3.0)

(defn- colorbar-length
  "Gives the bar length of a colour bar with some height available.

  For theme `[:legend :bar-length] :stretch`: the room minus the bar's fixed parts (title, label room, border), at
  least [[min-bar-keys]] x `:key-size` (also for unbounded room); a number is used up to that stretched length (never
  longer than the room allows).

  Parameters:

  - `content` (map): the colour bar content.
  - `env` (map): `:theme`, `:canvas` (measuring).
  - `room` (double): the height available, `##Inf` - unbounded.

  Returns the length in pixels."
  ^double [content env ^double room]
  (let [{:keys [bar-length ^double key-size]} (get-in env [:theme :legend])
        minimum (m/* min-bar-keys key-size)
        fixed (double (:h (colorbar-geometry content env 0.0)))
        stretched (if (m/valid-double? room) (m/max minimum (m/- room fixed)) ##Inf)]
    (if (number? bar-length)
      (m/min (double bar-length) stretched)
      (if (m/valid-double? stretched) stretched minimum))))

(defn- stretching?
  "Tests whether a legend content is a colour bar stretching over the height left.

  Parameters:

  - `content` (map): a legend or colour bar content.
  - `env` (map): `:theme`, `[:legend :bar-length]` read.

  Returns true for a colour bar whose theme bar length isn't a number (`:stretch`), else false."
  [content env]
  (and (= :colorbar (:kind content)) (not (number? (get-in env [:theme :legend :bar-length])))))

(defn- content-geometry
  "Gives the geometry of a legend or colour bar.

  Parameters:

  - `content` (map): a legend or colour bar content.
  - `env` (map): `:theme`, `:canvas` (for measuring).
  - `room` (double): the height available in pixels.

  Returns the layout of [[legend-geometry]] or [[colorbar-geometry]] (a colour bar's length fitted to `room`, see [[colorbar-length]])."
  [content env ^double room]
  (if (= :colorbar (:kind content))
    (colorbar-geometry content env (colorbar-length content env room))
    (legend-geometry content env room)))

(defn- available-height
  "Gives the height a legend may take.

  Parameters:

  - `env` (map): `:box` - the legends' item box from the previous layout round, absent before the first one.

  Returns the box height; `##Inf` before the first round."
  ^double [env]
  (if-let [box (:box env)] (double (:h box)) ##Inf))

(defn- stack-geometry
  "Gives the placement of legends stacked top-down, new stack columns to the right.

  A legend that doesn't fit below the previous ones starts a new column; a stretching colour bar does when less than
  its minimum length is left.

  Parameters:

  - `stacked` (sequence of maps): `:legend` and `:colorbar` contents.
  - `env` (map): `:theme`, `:canvas` (measuring).
  - `max-h` (double): the height available.

  Returns `{:w :h :placed [{:x :y :geometry}]}` relative to the stack's top-left corner; zero size for no legends."
  [stacked env ^double max-h]
  (let [{:keys [^double gap ^double key-size]} (get-in env [:theme :legend])
        minimum (m/* min-bar-keys key-size)
        {:keys [placed ^double col-x ^double col-w ^double h]}
        (reduce (fn [{:keys [placed ^double col-x ^double col-w ^double y ^double h] :as acc} legend]
                  (let [stretch? (stretching? legend env)
                        ;; a stretching color bar too short below the previous legends starts a new column
                        new-col? (and (m/pos? y)
                                      (if stretch?
                                        (m/< (m/- max-h y (double (:h (colorbar-geometry legend env 0.0)))) minimum)
                                        (m/> (m/+ y (double (:h (content-geometry legend env max-h)))) max-h)))
                        top (if new-col? 0.0 y)
                        g (content-geometry legend env (m/- max-h top))
                        lw (double (:w g)) lh (double (:h g))
                        x (if new-col? (m/+ col-x col-w gap) col-x)]
                    (assoc acc :placed (conj placed {:x x :y top :geometry g})
                           :col-x x :col-w (m/max (if new-col? 0.0 col-w) lw) :y (m/+ top lh gap) :h (m/max h (m/+ top lh)))))
                {:placed [] :col-x 0.0 :col-w 0.0 :y 0.0 :h 0.0}
                stacked)]
    {:w (m/+ col-x col-w) :h h :placed placed}))

(defmethod measure/natural-size :legend
  [content env]
  (let [{:keys [w h]} (legend-geometry content env (available-height env))]
    (v/vec2 w h)))

(defmethod measure/natural-size :colorbar
  [content env]
  (let [{:keys [w h]} (colorbar-geometry content env (colorbar-length content env (available-height env)))]
    (v/vec2 w h)))

(defmethod measure/natural-size :legends
  [content env]
  (let [{:keys [w h]} (stack-geometry (:legends content) env (available-height env))]
    (v/vec2 w h)))

(defn legend-boxes
  "Gives the boxes the legends of `:legends` content are drawn in.

  Parameters:

  - `content` (map): the `:legends` content.
  - `env` (map): `:theme` and a `:canvas` in graphics context for measuring.
  - `box` (map): the item box - `:x`, `:y`, `:h`.

  Returns a vector of `{:x :y :w :h}`, in the order of the legends."
  [content env {:keys [^double x ^double y ^double h]}]
  (mapv (fn [{px :x py :y g :geometry}]
          {:x (m/+ x (double px)) :y (m/+ y (double py)) :w (:w g) :h (:h g)})
        (:placed (stack-geometry (:legends content) env h))))

(defn- draw-key!
  "Overlays the glyphs of a legend's charts in one key box.

  Each glyph is isolated, clipped to the box (whole pixels covering it, plus a pixel around for a glyph's outline,
  within any outer clip) and given the entry's attributes of the dimensions its chart maps; the glyph's `env` carries
  the chart's computed stats (`:chart-stats`).

  Parameters:

  - `c`: the canvas in graphics context.
  - `attributes` (map): the entry's attributes by dimension.
  - `box` (map): the key box `{:x :y :w :h}`.
  - `chart-keys` (sequence): `[{:chart i :dims [dim]}]` (see [[legend-content]]).
  - `env` (map): `:legend-charts`, `:legend-stats` (indexed by `:chart`) and the draw environment.

  Returns `nil`."
  [c attributes {:keys [^double x ^double y ^double w ^double h] :as box} chart-keys env]
  (let [x0 (m/long-dec (long (m/floor x))) y0 (m/long-dec (long (m/floor y)))
        x1 (m/long-inc (long (m/ceil (m/+ x w)))) y1 (m/long-inc (long (m/ceil (m/+ y h))))]
    (doseq [{i :chart dims :dims} chart-keys
            :let [chart (get-in env [:legend-charts i])]]
      (c2d/with-isolated-canvas [clipped c]
        (c2d/add-clip clipped x0 y0 (m/long-sub x1 x0) (m/long-sub y1 y0))
        ((:legend chart) chart (select-keys attributes dims) clipped box (assoc env :chart-stats (get-in env [:legend-stats i])))))))

(defn- draw-label!
  "Draws a legend text on an isolated copy of the canvas.

  Parameters:

  - `c`: the canvas in graphics context.
  - `label-content` (map): a label content (its colour resolved by [[legend-text]]).
  - `box` (map): `{:x :y :w :h}` in pixels.
  - `env` (map): `:theme` and drawing settings.

  Returns `nil`."
  [c label-content box env]
  (c2d/with-isolated-canvas [isolated c]
    (draw/draw-content (legend-text label-content (:theme env)) isolated box env))
  nil)

(defn- draw-legend-box!
  "Fills the legend box and draws its border inside.

  Parameters:

  - `c`: the canvas in graphics context.
  - `box` (map): `{:x :y :w :h}`, rounded to whole pixels.
  - `env` (map): `:theme`, `[:legend]` `:fill`, `:border-color`, `:border-width` read; a `:none` fill or border draws nothing.

  Returns `nil`."
  [c {:keys [x y w h]} env]
  (let [{:keys [fill border-color border-width]} (get-in env [:theme :legend])
        rounded {:x (m/round (double x)) :y (m/round (double y)) :w (m/round (double w)) :h (m/round (double h))}]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c (:x rounded) (:y rounded) (:w rounded) (:h rounded)))
    (draw/inner-border! c rounded border-color border-width)
    nil))

(defn- draw-legend!
  "Draws a legend: its box, title, then each column's keys (see [[draw-key!]]) and labels.

  Parameters:

  - `content` (map): the legend content - `:title`, `:entries`, `:charts`.
  - `geometry` (map): its layout (see [[legend-geometry]]).
  - `c`: the canvas in graphics context.
  - `origin` (`Vec2`): the legend's top-left corner.
  - `env` (map): the draw environment.

  Returns `nil`."
  [{:keys [title entries charts]} geometry c ^Vec2 origin env]
  (let [x (.x origin)
        y (.y origin)
        {:keys [^double w ^double h ^double border ^double title-h columns labels]} geometry
        gap (double (get-in env [:theme :legend :gap]))
        x0 (m/+ x border)
        y0 (m/+ y border)]
    (draw-legend-box! c {:x x :y y :w w :h h} env)
    (when title
      (draw-label! c (title-label title) {:x x0 :y y0 :w (m/- w (m/* 2.0 border)) :h title-h} env))
    (doseq [{cx :x ^double key-w :key-w rows :rows} columns
            {i :entry ry :y rh :h} rows
            :let [kx (m/+ x0 (double cx))
                  ky (m/+ y0 title-h (double ry))
                  entry (entries i)]]
      (draw-key! c (:attributes entry) {:x kx :y ky :w key-w :h rh} charts env)
      (draw-label! c (entry-label (:label entry))
                   {:x (m/+ kx key-w gap) :y ky :w (.x ^Vec2 (labels i)) :h rh} env))))

(defn- bar-value-fn
  "Gives the value drawn at each position along a colour bar.

  Parameters:

  - `spec` (map): the colour scale spec - `:kind`, `:domain`, `:breaks`, `:transform`.

  Returns a function of a position `t` (0 - bottom, 1 - top) giving the value: linear - proportional over the domain
  (in the scale's `:transform` coordinates, see [[cljplot2.scale.aesthetic/value-transform]]); binned - every bin an
  equal block, proportional within it (`t` outside `[0 1]` stays in the end bins).

  See also [[bar-position-fn]]."
  [{:keys [kind domain breaks] :as spec}]
  (let [[start end] domain
        {:keys [forward inverse]} (aesthetic/value-transform spec)
        lo (double (forward (double start))) hi (double (forward (double end)))]
    (if (= :binned kind)
      (let [n (m/long-dec (count breaks))]
        (fn [^double t]
          (let [x (m/* t n)
                i (long (m/constrain (m/floor x) 0 (m/long-dec n)))]
            (m/lerp (double (breaks i)) (double (breaks (m/long-inc i))) (m/- x i)))))
      (fn [^double t] (inverse (m/lerp lo hi t))))))

(defn- bar-position-fn
  "Gives the position along a colour bar of each value: the inverse of [[bar-value-fn]].

  Parameters:

  - `spec` (map): the colour scale spec - `:kind`, `:domain`, `:breaks`, `:transform`.

  Returns a function of a value within the domain giving its position (0 - bottom, 1 - top); a one-value linear
  domain places every value at 0.5."
  [{:keys [kind domain breaks] :as spec}]
  (let [[start end] domain
        {:keys [forward]} (aesthetic/value-transform spec)
        lo (double (forward (double start))) hi (double (forward (double end)))]
    (if (= :binned kind)
      (let [n (m/long-dec (count breaks))
            arr (double-array breaks)]
        (fn [value]
          (let [v (double value)
                i (long (m/constrain (levels/bin-index arr v) 0 (m/long-dec n)))]
            (m// (m/+ i (m/norm v (Array/aget arr i) (Array/aget arr (m/long-inc i)))) n))))
      (fn [value] (if (m/== lo hi) 0.5 (m/norm (double (forward (double value))) lo hi))))))

(defn- draw-colorbar!
  "Draws a colour bar: its box, title, the bar, a mark per tick and the tick labels.

  Every label of an explicit `:ticks` vector is drawn, else from the lowest each label whose text block (padding
  excluded, as axes measure) doesn't overlap the last drawn one (see
  [[cljplot2.graphics.measure/kept-label-indices]]); texts in their roles' colours over the legend backdrop (see
  [[legend-text]]). Ticks are theme `[:legend :bar-tick-length]` long and `:bar-tick-width` wide (0 - none).

  Parameters:

  - `content` (map): the colour bar content - `:title`, `:scale`, `:ticks`, `:labels`.
  - `geometry` (map): its layout (see [[colorbar-geometry]]).
  - `c`: the canvas in graphics context.
  - `origin` (`Vec2`): the colour bar's top-left corner.
  - `env` (map): the draw environment.

  Returns `nil`."
  [{:keys [title scale ticks labels]} geometry c ^Vec2 origin env]
  (let [x (.x origin)
        y (.y origin)
        {:keys [^double gap ^double bar-tick-length ^double bar-tick-width]} (get-in env [:theme :legend])
        {:keys [^double w ^double h ^double border ^double title-h ^double bar-w ^double bar-len ^double label-h]
         sizes :labels} geometry
        color-of (aesthetic/attribute-fn :color scale (get-in env [:theme :palette :na]))
        [^double lo ^double hi] (:domain scale)
        value-at (bar-value-fn scale)
        position-of (bar-position-fn scale)
        x0 (m/+ x border)
        bar-top (m/+ y border title-h (m/* 0.5 label-h))
        bar-x (m/round x0)
        bar-w-px (m/round bar-w)
        bar-right (m/+ bar-x bar-w-px)
        len (m/round bar-len)
        tick-y (fn ^double [t] (m/+ bar-top (if (m/== lo hi) (m/* 0.5 bar-len) (m/* (m/- 1.0 (double (position-of t))) bar-len))))
        label-color (draw/text-color (:theme env) :legend-label (legend-backdrop (:theme env)))
        inset (m/* 2.0 (double (get-in env [:theme :text :legend-label :padding])))
        kept (if (sequential? (:ticks scale))
               (set (range (count ticks)))
               (set (measure/kept-label-indices (mapv tick-y ticks) (mapv #(m/- (.y ^Vec2 %) inset) sizes))))]
    (draw-legend-box! c {:x x :y y :w w :h h} env)
    (when title
      (draw-label! c (title-label title) {:x x0 :y (m/+ y border) :w (m/- w (m/* 2.0 border)) :h title-h} env))
    ;; the bar: one pixel row per value, top = domain maximum
    (dotimes [i len]
      (let [t (if (m/== lo hi) 0.5 (m/- 1.0 (m// (m/+ (double i) 0.5) (double len))))]
        (c2d/set-color c (color-of (value-at t)))
        (c2d/rect c bar-x (m/long-add (m/round bar-top) i) bar-w-px 1)))
    (c2d/set-stroke-custom c {:size bar-tick-width :cap :butt})
    (doseq [[i t text ^Vec2 size] (map vector (range) ticks labels sizes)
            :let [ty (draw/pixel-center (tick-y t))]]
      (c2d/set-color c label-color)
      (when (m/pos? bar-tick-width)
        (c2d/line c bar-right ty (m/+ bar-right bar-tick-length) ty))
      (when (contains? kept i)
        (c2d/with-isolated-canvas [isolated c]
          (draw/draw-content (assoc (entry-label text) :color label-color) isolated
                             {:x (m/+ bar-right bar-tick-length gap) :y (m/- ty (m/* 0.5 (.y size))) :w (.x size) :h (.y size)}
                             env))))))

(defn- draw-content-at!
  "Draws a legend or colour bar at a corner.

  Parameters:

  - `content` (map): a legend or colour bar content.
  - `geometry` (map): its layout (see [[content-geometry]]).
  - `c`: the canvas in graphics context.
  - `origin` (`Vec2`): the top-left corner in pixels.
  - `env` (map): `:theme` and drawing settings.

  Returns `nil`."
  [content geometry c origin env]
  (if (= :colorbar (:kind content))
    (draw-colorbar! content geometry c origin env)
    (draw-legend! content geometry c origin env)))

(defmethod draw/draw-content :colorbar
  [content c {:keys [x y ^double h]} env]
  (let [measure-env (assoc env :canvas c)]
    (draw-colorbar! content (colorbar-geometry content measure-env (colorbar-length content measure-env h))
                    c (v/vec2 x y) measure-env)))

(defmethod draw/draw-content :legend
  [content c {:keys [x y ^double h]} env]
  (let [measure-env (assoc env :canvas c)]
    (draw-legend! content (legend-geometry content measure-env h) c (v/vec2 x y) measure-env)))

(defmethod draw/draw-content :legends
  [content c {:keys [^double x ^double y ^double h]} env]
  (let [measure-env (assoc env :canvas c)
        stacked (:legends content)]
    (doseq [[legend {px :x py :y g :geometry}] (map vector stacked (:placed (stack-geometry stacked measure-env h)))]
      (draw-content-at! legend g c (v/vec2 (m/+ x (double px)) (m/+ y (double py))) measure-env))))
