(ns cljplot2.layout.plot-grid
  "Grid generated from plot keys (see [[cljplot2.layout.grid]] for the grid shape).

  Single panel:

  * columns `:margin-left | :y-title | :y-axis | :panel | :x-overhang | :margin-right`
  * rows `:margin-top | :title | :y-overhang | :panel | :x-axis | :x-title | :margin-bottom`

  Margins are fixed (theme `[:plot :margin]`), the panel is flexible, every other track is measured.
  Axis tracks are reserved for axes and hold no items yet.

  Plot `:labels` - `{:title t :x t :y t}`, `t` a string or a map of label options (`:text`, style overrides,
  `:align`, `:rotation`) merged into the label content; a missing or `nil` label adds no item.
  `:title` uses the `:title` text role, `:x` and `:y` the `:axis-title` role; `:y` is rotated 270 by default.
  The label `:kind` can't be overridden.

  An axis item (`:x-axis` below the panel, `:y-axis` left of it) is added for each scale in `:scale-specs` (not for
  one with `:axis false`), with
  overhang items reserving room for its end tick labels: x end in `:x-overhang`, y end in `:y-overhang`, both
  start ends in the corner cell (`:y-axis` column, `:x-axis` row). Track and item names are by side (`x` - under
  the panel, `y` - left of it): with `:flip true` the `:y` scale's axis and `:labels :y` title go under the panel,
  the `:x` scale's axis and `:labels :x` title left of it.
  Item order: `:panel`, axes and their overhangs (x, then y), `:title`, `:x-title`, `:y-title` (absent ones skipped).

  Faceted plots use [[facet-grid]].

  Legends ([[cljplot2.content.legend/legends]]) add columns `:legend-gap` (fixed, theme `[:legend :gap]`) and `:legend`
  (measured) after `:x-overhang`, and an item `:legends` spanning the panel rows (from the top strip) with
  `{:kind :legends}` content: the legends stacked top-down, wrapping into further columns when too tall."
  (:require [fastmath.core :as m]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.content.label :as label]
            [cljplot2.content.legend :as legend]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private plot-labels
  "Plot label key -> grid item id, content defaults, placement and description, in item order."
  [[:title {:id :title :defaults {:role :title} :column {:from :y-title :to :x-overhang} :row :title
            :description "plot title above the panels (role :title)"}]
   [:x {:id :x-title :defaults {:role :axis-title} :column :panel :row :x-title
        :description "x axis title (role :axis-title)"}]
   [:y {:id :y-title :defaults {:role :axis-title :rotation 270} :column :y-title :row :panel
        :description "y axis title (role :axis-title, rotated 270)"}]])

(def labels-schema
  "Plot `:labels`: `{:title :x :y}`, each a text or a map of [[cljplot2.content.label/label-options]]; `nil` - no label."
  (into [:map {:closed true}]
        (map (fn [[k {:keys [description]}]]
               (schema/nilable k {:description description :cljplot2/default-doc "none; axis titles from the charts or the mapped column names"}
                               [:multi {:dispatch map?} [true label/label-options] [false any?]])))
        plot-labels))

(defn- label-items
  "Grid items of the plot `labels` (fixed order: title, x, y); a `nil` label adds no item.
  `spans` (label key -> `{:column :row}`) overrides the single-panel placement."
  [labels spans]
  (vec (keep (fn [[k {:keys [id defaults column row]}]]
               (when-some [label (get labels k)]
                 {:id id :column (get-in spans [k :column] column) :row (get-in spans [k :row] row)
                  :content (assoc (merge defaults (if (map? label) label {:text label})) :kind :label)}))
             plot-labels)))

(defn- axis-shown?
  "Does position scale `k` have a spec in `scale-specs` whose axis is drawn (see
  [[cljplot2.scale.position/axis-drawn?]])?"
  [scale-specs k]
  (and (contains? scale-specs k) (scale/axis-drawn? (get scale-specs k))))

(defn- axis-items
  "Items of the axis of scale `k` drawn on `side`: `:bottom` (horizontal scale; axis under the panel, end overhang
  in the `:x-overhang` column) or `:left` (vertical scale; axis left of the panel, end overhang in the `:y-overhang`
  row); both start overhangs in the corner cell (`:y-axis` column, `:x-axis` row)."
  [side k]
  (case side
    :bottom [{:id :x-axis :column :panel :row :x-axis :content {:kind :axis :scale k}}
             {:id :x-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale k :end :start}}
             {:id :x-axis-end :column :x-overhang :row :x-axis :content {:kind :axis-overhang :scale k :end :end}}]
    :left [{:id :y-axis :column :y-axis :row :panel :content {:kind :axis :scale k}}
           {:id :y-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale k :end :start}}
           {:id :y-axis-end :column :y-axis :row :y-overhang :content {:kind :axis-overhang :scale k :end :end}}]))

(defn- side-scales
  "`[[side k] ...]` - the scale key drawn on each axis side (`:bottom` horizontal, `:left` vertical) of a plot with
  `flip?` (`:flip` swaps them)."
  [flip?]
  (let [horizontal (scale/horizontal-key flip?)]
    [[:bottom horizontal] [:left (if (= horizontal :x) :y :x)]]))

(defn- side-labels
  "Plot `labels` keyed by axis side slot: `:x` - the title under the panel, `:y` - left of it. A flipped plot's
  `:x` title goes left (the `:x` scale is vertical) and its `:y` title under the panel."
  [labels flip?]
  (if (and flip? (map? labels))
    (set/rename-keys labels {:x :y :y :x})
    labels))

(defn- legend-tracks
  "Columns added for `legends` (none when empty)."
  [legends theme]
  (if (seq legends)
    [{:id :legend-gap :size [:fixed (get-in theme [:legend :gap])]} {:id :legend :size [:measured]}]
    []))

(defn- legend-items
  "The `:legends` item over rows `row-span` (none when `legends` is empty)."
  [legends row-span]
  (if (seq legends)
    [{:id :legends :column :legend :row row-span :content {:kind :legends :legends legends}}]
    []))

(def ^:private margin-sides {:margin-left :left :margin-right :right :margin-top :top :margin-bottom :bottom})

(defn- margin-track-fn
  "Fixed margin track of `id` (`:margin-left` ...): theme `[:plot :margin]` plus the plot's internal
  `:alignment-padding` for that side (set by multiplot panel alignment, see [[cljplot2.multiplot]])."
  [{:keys [theme alignment-padding]}]
  (let [margin (double (get-in theme [:plot :margin]))]
    (fn [id] {:id id :size [:fixed (+ margin (double (get alignment-padding (margin-sides id) 0)))]})))

(defn- plot-margin-tracks
  "Tracks of the plot margin of `side` (`:top` - rows above the panel, `:right` - columns right of it): the margin
  panel's fixed `size`, a gap (theme `[:facet :gap]`) next to the main panel and, with `:axis`, a measured track on
  the far side holding the room the magnitude axis's end tick label sticks out; none without that margin."
  [margins side theme]
  (if-let [{:keys [size axis]} (get margins side)]
    (let [gap {:id (keyword (str (name side) "-margin-gap")) :size [:fixed (get-in theme [:facet :gap])]}
          track {:id (keyword (str (name side) "-margin")) :size [:fixed size]}
          overhang (when axis {:id (keyword (str (name side) "-margin-overhang")) :size [:measured]})]
      (vec (remove nil? (if (= side :top) [overhang track gap] [gap track overhang]))))
    []))

(defn- plot-margin-items
  "Items of the plot `margins`: a margin panel `{:kind :panel :margin side}` beside the main panel and, with `:axis`,
  its magnitude axis `{:kind :axis :scale :y :margin side}` - the top margin's in the y-axis column, the right
  margin's in the x-axis row - with an overhang item for its end tick label."
  [margins]
  (vec (mapcat (fn [[side {:keys [axis]}]]
                 (let [panel-id (keyword (str (name side) "-margin"))
                       [column row axis-column axis-row] (if (= side :top)
                                                           [:panel panel-id :y-axis panel-id]
                                                           [panel-id :panel panel-id :x-axis])]
                   (cond-> [{:id panel-id :column column :row row :content {:kind :panel :margin side}}]
                     axis (conj {:id (keyword (str (name side) "-margin-axis")) :column axis-column :row axis-row
                                 :content {:kind :axis :scale :y :margin side}}
                                ;; room for the end tick label: above the top margin, right of the right margin
                                (let [overhang-id (keyword (str (name side) "-margin-overhang"))]
                                  {:id (keyword (str (name side) "-margin-axis-end"))
                                   :column (if (= side :top) :y-axis overhang-id)
                                   :row (if (= side :top) overhang-id :x-axis)
                                   :content {:kind :axis-overhang :scale :y :end :end :margin side}})))))
               margins)))

(defn single-panel-grid
  "Grid of a single-panel plot: its `:labels`, axes of its `:scale-specs`, its plot margins (`:margin-stats`, see
  [[cljplot2.pipeline]]: a `:top-margin` row above the panel, a `:right-margin` column right of it - before the
  legend - each next to a gap) and the margin of its (merged) `:theme`. `:alternate-panel true` shades the panel as an
  odd facet panel."
  [{:keys [labels theme scale-specs flip margin-stats] :as plot}]
  (let [margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        legends (legend/legends plot)]
    {:columns (-> [(margin-track :margin-left) (measured :y-title) (measured :y-axis) {:id :panel :size [:flexible]}
                   (measured :x-overhang)]
                  (into (plot-margin-tracks margin-stats :right theme))
                  (into (legend-tracks legends theme))
                  (conj (margin-track :margin-right)))
     :rows (-> [(margin-track :margin-top) (measured :title)]
               (into (plot-margin-tracks margin-stats :top theme))
               (into [(measured :y-overhang) {:id :panel :size [:flexible]}
                      (measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]))
     :items (-> [{:id :panel :column :panel :row :panel :content (cond-> {:kind :panel} (:alternate-panel plot) (assoc :alternate true))}]
                (into (mapcat (fn [[side k]] (when (axis-shown? scale-specs k) (axis-items side k)))) (side-scales flip))
                (into (label-items (side-labels labels flip) {}))
                (into (legend-items legends :panel))
                (into (plot-margin-items margin-stats)))}))

;; facets

(defn- strip-content
  "Strip content for facet `level` with `text`; when every one of `levels` is a shingle interval, also
  `:interval {:lo :hi :min :max}` - the level within the range of all levels (a shaded bar, see
  [[cljplot2.content.strip]])."
  [text level levels]
  (cond-> {:kind :strip :text text}
    (and (seq levels) (every? shingle/interval? levels))
    (assoc :interval {:lo (:lo level) :hi (:hi level)
                      :min (reduce min (map :lo levels)) :max (reduce max (map :hi levels))})))

(defn facet-panels
  "Panels `{:key :label :row :col}` and `[rows cols]` of the facets in `split-facets`."
  [{:keys [dims levels]} scales {:keys [order] :as facets}]
  (if (= dims [:facet])
    (let [ls (:facet levels)
          [rows cols] (facet/wrap-shape (count ls) facets)]
      {:rows rows :cols cols :wrap? true :levels ls
       :panels (vec (map-indexed (fn [i level]
                                   (let [[r c] (facet/wrap-positions i rows cols (or order :top-down))]
                                     {:key level :label (facet/label (:facet scales) level) :row r :col c}))
                                 ls))})
    (let [col-levels (get levels :facet-col [::none])
          row-levels (get levels :facet-row [::none])
          key-of (fn [cl rl] (cond (= 2 (count dims)) [cl rl] (some #{:facet-col} dims) cl :else rl))]
      {:rows (count row-levels) :cols (count col-levels) :wrap? false
       :col-levels col-levels :row-levels row-levels
       :col-labels (when (contains? levels :facet-col) (mapv #(facet/label (:facet-col scales) %) col-levels))
       :row-labels (when (contains? levels :facet-row) (mapv #(facet/label (:facet-row scales) %) row-levels))
       :panels (vec (for [[r rl] (map-indexed vector row-levels) [c cl] (map-indexed vector col-levels)]
                      {:key (key-of cl rl) :row r :col c}))})))

(defn panel-layout
  "`{:columns c :rows r}` of the plot's panels: the facet layout of `split-facets` (see the split stage) with plot
  `facets` options (as [[facet-grid]] lays it out, strips not formatted), `{:columns 1 :rows 1}` without facets.
  Throws as [[cljplot2.data.facet/wrap-shape]] for a wrap layout too small for its panels."
  [{:keys [dims levels]} facets]
  (cond
    (empty? dims) {:columns 1 :rows 1}
    (= dims [:facet]) (let [[rows cols] (facet/wrap-shape (count (:facet levels)) facets)] {:columns cols :rows rows})
    :else {:columns (count (get levels :facet-col [nil])) :rows (count (get levels :facet-row [nil]))}))

(def ^:private estimate-tick-label
  "Stand-in tick label of an axis whose ticks are unknown (before stats)."
  "00")

(defn- size-height ^double [size] (if size (.y ^fastmath.vector.Vec2 size) 0.0))
(defn- size-width ^double [size] (if size (.x ^fastmath.vector.Vec2 size) 0.0))

(defn- axis-room
  "Pixels an axis `k` takes across itself for tick labels `across` pixels deep (theme tick length and label gap)."
  ^double [theme k ^double across]
  (+ (double (get-in theme [:axis k :tick-length])) (double (get-in theme [:axis k :label-gap])) across))

(defn estimated-panel-size
  "Expected `[w h]` (pixels, at least 1) of one panel of `plot` before its stats are known: the plot's `:width` /
  `:height` less the room this grid gives the plot margins, the title and axis titles (`:labels`), the axes (stand-in
  two-digit tick labels, no end overhangs), plot `margins` (`{side {:size px}}`), the `legends` (contents, e.g.
  [[cljplot2.content.legend/provisional-legends]]) and the facet strips and gaps of `panel-layout` (`{:columns
  :rows}`; `facets?` - strips present, `grid?` - facet-grid strips (one row, one column), else wrap strips over
  every row), split among the panels; a wrap layout with free / sliced scales (plot `:facets`) also loses the room of
  an axis per panel row / column. Measured on a scratch canvas; an estimate - real tick labels and wrapped legends
  differ."
  [{:keys [options theme labels flip facets]} legends margins {:keys [columns rows]} facets? grid?]
  (measure/call-with-scratch-canvas
   (:hint options)
   (fn [canvas]
     (let [env {:canvas canvas :theme theme}
           size (fn [content] (measure/natural-size content env))
           text-size (fn [role l] (when (some? l)
                                    (size (merge {:kind :label :role role} (if (map? l) (dissoc l :rotation) {:text (str l)})))))
           {:keys [x y title]} (side-labels labels flip)
           margin (double (get-in theme [:plot :margin]))
           tick (size {:kind :label :role :tick-label :text estimate-tick-label})
           tick-w (size-width tick) tick-h (size-height tick)
           gap (double (get-in theme [:facet :gap]))
           strip (if facets? (size-height (size {:kind :strip :text "Xg"})) 0.0)
           margin-room (fn ^double [side] (if-let [{:keys [size]} (get margins side)] (+ (double size) gap) 0.0))
           legend-room (if (seq legends)
                         (+ (double (get-in theme [:legend :gap])) (size-width (size {:kind :legends :legends legends})))
                         0.0)
           columns (long columns) rows (long rows)
           horizontal (scale/horizontal-key flip)
           free? (fn [k] (and (not grid?) (not= :same (facet/scale-relation facets k))))
           inner-y-axes (if (free? (if (= horizontal :x) :y :x)) (* (dec columns) (axis-room theme :y tick-w)) 0.0)
           inner-x-axes (if (free? horizontal) (* (dec rows) (axis-room theme :x tick-h)) 0.0)
           w (- (double (:width options)) (* 2.0 margin) (size-height (text-size :axis-title y))
                (axis-room theme :y tick-w) (double (margin-room :right)) legend-room
                (* gap (dec columns)) (if grid? strip 0.0) inner-y-axes)
           h (- (double (:height options)) (* 2.0 margin) (size-height (text-size :title title))
                (double (margin-room :top)) (axis-room theme :x tick-h)
                (size-height (text-size :axis-title x)) (* gap (dec rows)) (if grid? strip (* strip rows)) inner-x-axes)]
       [(m/max 1.0 (/ w columns)) (m/max 1.0 (/ h rows))]))))

(defn- panel-weights
  "Weights of the `n` panel columns (`k` `:widths`) or rows (`:heights`) from plot `facets`: `nil` (equal) when not
  given; a count other than `n` throws `ex-info {:key k}`."
  [facets k ^long n]
  (when-let [ws (get facets k)]
    (when (not= n (count ws))
      (schema/fail! (str "Facet " k " needs one weight per panel " (if (= k :widths) "column" "row") " (" n "), got "
                         (count ws) ".")
                    {:key k :in [:facets k] :value ws}))
    (vec ws)))

(defn- panel-track
  "Flexible track `[:panel i]`, weighted by `(weights i)` when `weights` is given."
  [weights i]
  {:id [:panel i] :size (if weights [:flexible (weights i)] [:flexible])})

(defn- facet-axis-items
  "Axis items of a facet layout for the horizontal scale `h-key` (under the panels) and the vertical `v-key` (left of
  them), each with the `:panel` key whose scales it draws (see [[cljplot2.scale.position/panel-scales-of]]):
  `free-h?` / `free-v?` (wrap layout with a free / sliced relation) - an axis at every panel, in its own `[:x-axis r]`
  row / `[:y-axis c]` column; otherwise an axis per panel column (`:x-axis` row) / row (`:y-axis` column) drawn with
  that column's bottom / that row's first panel. An axis with another panel's axis right of it (horizontal) gets
  `:next-panel-gap` (labels crossing the next axis's labels are skipped, see [[cljplot2.content.axis]]); with
  panel-local scales (`local-h?` / `local-v?` - neighbouring axes may differ) also `:next-panel` - the next panel's key,
  so its own labels are compared - and vertical axes get both towards the axis below unless `stacked?` (strips or
  axis rows separate the panel rows). Overhang items reserve room for end labels as in the single-panel grid; the
  horizontal start overhang sits in the first y-axis column (`[:y-axis 0]` with `free-v?`)."
  [panels cols gap {:keys [h-key v-key h-shown? v-shown? free-h? free-v? local-h? local-v? stacked?]}]
  ;; panels are passed around as maps (`nil` - no panel): a panel's `:key` may itself be `nil` (the NA facet level)
  (let [at (into {} (map (juxt (juxt :row :col) identity)) panels)
        h-axis (fn [content next-panel]
                 (cond-> content
                   next-panel (assoc :next-panel-gap gap)
                   (and local-h? next-panel) (assoc :next-panel (:key next-panel))))
        v-axis (fn [content next-panel]
                 (cond-> content
                   (and local-v? (not stacked?) next-panel) (assoc :next-panel-gap gap :next-panel (:key next-panel))))
        bottom-of (fn [c] (last (sort-by :row (filter #(= c (:col %)) panels))))
        first-of (fn [r] (first (sort-by :col (filter #(= r (:row %)) panels))))
        rows (sort (distinct (map :row panels)))
        last-row (peek (vec rows))
        next-row (fn [r] (second (drop-while #(not= r %) rows)))
        with-panel (fn [item panel] (cond-> item panel (assoc-in [:content :panel] (:key panel))))
        [x-start x-end] (rest (axis-items :bottom h-key))
        x-start (cond-> x-start free-v? (assoc :column [:y-axis 0]))
        [y-start y-end] (rest (axis-items :left v-key))]
    (concat
     (when h-shown?
       (if free-h?
         (concat
          (for [{:keys [row col] :as panel} panels]
            (with-panel {:id [:x-axis row col] :column [:panel col] :row [:x-axis row]
                         :content (h-axis {:kind :axis :scale h-key} (at [row (inc (long col))]))}
              panel))
          (mapcat (fn [r]
                    (let [in-row (sort-by :col (filter #(= r (:row %)) panels))]
                      [(-> (assoc x-start :id [:x-axis-start r] :row [:x-axis r]) (with-panel (first in-row)))
                       (-> (assoc x-end :id [:x-axis-end r] :row [:x-axis r]) (with-panel (last in-row)))]))
                  rows))
         (concat
          (for [c (range cols)]
            (with-panel {:id [:x-axis c] :column [:panel c] :row :x-axis
                         :content (h-axis {:kind :axis :scale h-key}
                                          (when (< (long c) (dec (long cols))) (bottom-of (inc (long c)))))}
              (bottom-of c)))
          [(with-panel x-start (bottom-of 0)) (with-panel x-end (bottom-of (dec (long cols))))])))
     (when v-shown?
       (concat
        (if free-v?
          (for [{:keys [row col] :as panel} panels]
            (with-panel {:id [:y-axis row col] :column [:y-axis col] :row [:panel row]
                         :content (v-axis {:kind :axis :scale v-key} (at [(inc (long row)) col]))}
              panel))
          (for [r rows]
            (with-panel {:id [:y-axis r] :column :y-axis :row [:panel r]
                         :content (v-axis {:kind :axis :scale v-key} (some-> (next-row r) first-of))}
              (first-of r))))
        [(with-panel y-start (at [last-row 0])) (with-panel y-end (at [(first rows) 0]))])))))

(defn facet-grid
  "Grid of a faceted plot (see [[cljplot2.data.facet]]): panels `[:panel r c]` in a `rows` x `cols` layout with gaps
  (theme `[:facet :gap]`), strips (wrap: above each panel; grid: column strips above the top row, row strips on
  the right; none with `:facets {:strips false}`), an x axis under every panel column and a y axis beside every panel
  row, overhangs at the outer ends; a wrap layout with a free / sliced relation (`:facets {:scales ..}`) has an axis
  at every panel instead (see [[facet-axis-items]]). Panel columns / rows are weighted by `:facets` `:widths` /
  `:heights`. Panels with odd `r + c` are shaded (`:alternate`). Legends as in the single-panel grid."
  [{:keys [labels theme scale-specs scales facets split flip] :as plot}]
  (let [legends (legend/legends plot)
        [[_ h-key] [_ v-key]] (side-scales flip)
        gap (get-in theme [:facet :gap])
        margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        strips? (facet/strips? facets)
        {:keys [rows cols wrap? panels col-labels row-labels levels col-levels row-levels]}
        (cond-> (facet-panels (:facets split) scales facets)
          (not strips?) (assoc :col-labels nil :row-labels nil))
        rows (long rows) cols (long cols)
        widths (panel-weights facets :widths cols)
        heights (panel-weights facets :heights rows)
        free-h? (and wrap? (not= :same (facet/scale-relation facets h-key)))
        free-v? (and wrap? (not= :same (facet/scale-relation facets v-key)))
        wrap-strips? (and wrap? strips?)
        last-col [:panel (dec cols)]
        last-row [:panel (dec rows)]
        gap-track (fn [prefix i] {:id [prefix i] :size [:fixed gap]})
        column-tracks (vec (butlast (mapcat (fn [c] (cond-> []
                                                      free-v? (conj (measured [:y-axis c]))
                                                      true (conj (panel-track widths c) (gap-track :gap c))))
                                            (range cols))))
        row-tracks (vec (butlast (mapcat (fn [r] (cond-> []
                                                   wrap-strips? (conj (measured [:strip r]))
                                                   true (conj (panel-track heights r))
                                                   free-h? (conj (measured [:x-axis r]))
                                                   true (conj (gap-track :gap r))))
                                         (range rows))))]
    {:columns (-> [(margin-track :margin-left) (measured :y-title) (measured :y-axis)]
                  (into column-tracks)
                  (cond-> row-labels (conj (measured :row-strip)))
                  (conj (measured :x-overhang))
                  (into (legend-tracks legends theme))
                  (conj (margin-track :margin-right)))
     :rows (-> [(margin-track :margin-top) (measured :title) (measured :y-overhang)]
               (cond-> col-labels (conj (measured :col-strip)))
               (into row-tracks)
               (into [(measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]))
     :items (-> (mapv (fn [{:keys [key row col]}]
                        {:id [:panel row col] :column [:panel col] :row [:panel row]
                         :content {:kind :panel :panel key :alternate (odd? (+ (long row) (long col)))}})
                      panels)
                (into (facet-axis-items panels cols gap {:h-key h-key :v-key v-key :free-h? free-h? :free-v? free-v?
                                                         :local-h? (or (not= :same (facet/scale-relation facets h-key)) (some? widths))
                                                         :local-v? (or (not= :same (facet/scale-relation facets v-key)) (some? heights))
                                                         :stacked? (or wrap-strips? free-h?)
                                                         :h-shown? (axis-shown? scale-specs h-key)
                                                         :v-shown? (axis-shown? scale-specs v-key)}))
                (cond-> wrap-strips? (into (map (fn [{:keys [key label row col]}]
                                                  {:id [:strip row col] :column [:panel col] :row [:strip row]
                                                   :content (strip-content label key levels)})
                                                panels)))
                (cond-> col-labels (into (map-indexed (fn [c text] {:id [:col-strip c] :column [:panel c] :row :col-strip
                                                                    :content (strip-content text (col-levels c) col-levels)})
                                                      col-labels)))
                (cond-> row-labels (into (map-indexed (fn [r text] {:id [:row-strip r] :column :row-strip :row [:panel r]
                                                                    :content (assoc (strip-content text (row-levels r) row-levels)
                                                                                    :rotation 90)})
                                                      row-labels)))
                (into (label-items (side-labels labels flip) {:x {:column {:from [:panel 0] :to last-col}}
                                           :y {:row {:from [:panel 0] :to last-row}}}))
                (into (legend-items legends {:from (cond col-labels :col-strip wrap-strips? [:strip 0] :else [:panel 0])
                                                   :to last-row})))}))

(defn titled-custom-grid
  "`custom-grid` under a title row: rows `:margin-top` (theme `[:plot :margin]`) | `:title` (measured) | `:body`
  (flexible, holding `custom-grid` nested). Only `:title` of `labels` is used (the plot schema allows no other)."
  [custom-grid labels theme]
  (if (nil? (:title labels))
    custom-grid
    {:columns [{:id :body :size [:flexible]}]
     :rows [{:id :margin-top :size [:fixed (get-in theme [:plot :margin])]} {:id :title :size [:measured]}
            {:id :body :size [:flexible]}]
     :items (into [{:id :body :column :body :row :body :content (assoc custom-grid :kind :grid)}]
                  (label-items {:title (:title labels)} {:title {:column :body :row :title}}))}))

(defn- arrange-shape
  "`[rows cols]` for `n` plots and optional `columns` / `rows` (default one column)."
  [^long n columns rows]
  (cond
    (and rows columns) [(long rows) (long columns)]
    columns [(long (Math/ceil (/ (double n) (double columns)))) (long columns)]
    rows [(long rows) (long (Math/ceil (/ (double n) (double rows))))]
    :else [n 1]))

(defn- arrange-problem
  "Why the `:arrange` spec (valid entries) can't be laid out, or `nil`: too few cells, or `:widths` / `:heights` not
  one per column / row."
  [{:keys [plots columns rows widths heights]}]
  (let [n (count plots)
        [^long r ^long c] (arrange-shape n columns rows)]
    (cond
      (< (* r c) n) (str "layout " r "x" c " is too small for " n " plots")
      (and widths (not= c (count widths))) (str ":widths should have one weight per column (" c ")")
      (some (fn [j] (and (= :measured (nth widths j))
                         (not-every? :legend-only (keep (fn [i] (get (vec plots) (+ (long j) (* c (long i))))) (range r)))))
            (range (count widths)))
      ":widths :measured sizes a column of :legend-only plots only"
      (and heights (not= r (count heights))) (str ":heights should have one weight per row (" r ")"))))

(def arrange-schema
  "Plot `:arrange`, see [[cljplot2.multiplot/arrange]]; the plots are validated when built as sub-plots."
  (schema/with-rules
   [:map {:closed true :cljplot2/type-doc "an arrange map {:plots :columns :rows :widths :heights :align :equal-panels}"}
    [:plots {:description "the sub-plots, filling the grid row by row"} [:sequential {:min 1} :map]]
    (schema/nilable :columns {:description "grid columns" :cljplot2/default-doc "one column (from :rows when given)"} schema/pos-int)
    (schema/nilable :rows {:description "grid rows" :cljplot2/default-doc "from the plot count and :columns"} schema/pos-int)
    (schema/nilable :widths {:description "relative column weights; :measured sizes a column of legend-only plots, [:fixed px] a fixed width"
                             :cljplot2/default-doc "equal"}
                    [:sequential [:or schema/pos [:= :measured] [:tuple [:= :fixed] schema/non-neg]]])
    (schema/nilable :heights {:description "relative row weights; [:fixed px] a fixed height" :cljplot2/default-doc "equal"}
                    [:sequential [:or schema/pos [:tuple [:= :fixed] schema/non-neg]]])
    (schema/nilable :align {:description "align the sub-plots' panels in rows and columns" :default true} schema/bool)
    (schema/nilable :equal-panels {:description "resize columns and rows so every aligned panel has the same size (builds twice)"
                                   :default false}
                    schema/bool)]
   [arrange-problem]))

(defn- arrange-weights
  [ws ^long n]
  (if (nil? ws) (vec (repeat n 1)) (vec ws)))

(defn- weighted-tracks
  "Tracks `[prefix i]` - flexible, weighted by `ws`; an entry `[:fixed px]` is a fixed track - fixed gaps
  `[gap-prefix i]` between them."
  [prefix gap-prefix ws gap]
  (vec (butlast (mapcat (fn [i w] [{:id [prefix i] :size (if (vector? w) w [:flexible w])} {:id [gap-prefix i] :size [:fixed gap]}])
                        (range) ws))))

(defn legend-only-grid
  "Grid of a plot with `:legend-only true`: only its legends (`legends` - contents, see
  [[cljplot2.content.legend/legends]]) in a measured column between the plot margins, under an optional title row
  (`labels` - only `:title`); no panel, axes or axis titles."
  [legends labels theme]
  (let [margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})]
    {:columns [(margin-track :margin-left) {:id :legend :size [:measured]} {:id :legend-rest :size [:flexible]}
               (margin-track :margin-right)]
     :rows [(margin-track :margin-top) {:id :title :size [:measured]} {:id :legend-row :size [:flexible]}
            (margin-track :margin-bottom)]
     :items (-> (legend-items legends :legend-row)
                (into (label-items (select-keys labels [:title]) {:title {:column :legend :row :title}})))}))

(defn arrange-grid
  "Grid of the plot `:arrange` spec `{:plots [p ...] :columns :rows :widths :heights}` in `theme`: cells `[:plot i]`
  filled row by row (columns `[:column c]`, rows `[:row r]`) with `[:multiplot :gap]` between them, `[:plot :margin]`
  around, and an optional title row (`labels` - only `:title`) above the cells, inside the margins.
  `arrange` is valid by [[arrange-schema]], its `:measured` widths resolved to `[:fixed px]` (see the pipeline's
  grid stage)."
  [{:keys [plots columns rows widths heights]} labels theme]
  (let [plots (vec plots)
        [^long r ^long c] (arrange-shape (count plots) columns rows)
        gap (get-in theme [:multiplot :gap])
        margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})]
    {:columns (-> [(margin-track :margin-left)]
                  (into (weighted-tracks :column :column-gap (arrange-weights widths c) gap))
                  (conj (margin-track :margin-right)))
     :rows (-> [(margin-track :margin-top) {:id :title :size [:measured]}]
               (into (weighted-tracks :row :row-gap (arrange-weights heights r) gap))
               (conj (margin-track :margin-bottom)))
     :items (-> (vec (map-indexed (fn [i p]
                                    {:id [:plot i] :column [:column (rem (long i) c)] :row [:row (quot (long i) c)]
                                     :content {:kind :plot :plot p}})
                                  plots))
                (into (label-items (select-keys labels [:title])
                                   {:title {:column {:from [:column 0] :to [:column (dec c)]} :row :title}})))}))

(def ^:private iso-spacers #{:iso-before :iso-after})

(defn- with-iso-spacers
  "`grid` with spacer tracks `:iso-before` / `:iso-after` (fixed 0) inside its outer margin tracks, both axes."
  [grid]
  (let [insert (fn [tracks] (-> [(first tracks) {:id :iso-before :size [:fixed 0]}]
                                (into (butlast (rest tracks)))
                                (conj {:id :iso-after :size [:fixed 0]} (peek tracks))))]
    (-> grid (update :columns insert) (update :rows insert))))

(defn iso-grid
  "Generated `grid` (with iso spacers) with `column-total` / `row-total` whole pixels of spacer per axis, split
  between the two sides (`:iso-before` gets the floor of half); panels stay flexible and take the rest, the panel
  block centred."
  [grid column-total row-total]
  (let [resize (fn [tracks total]
                 (let [total (long total)
                       before (quot total 2)]
                   (mapv (fn [t] (case (:id t)
                                   :iso-before (assoc t :size [:fixed before])
                                   :iso-after (assoc t :size [:fixed (- total before)])
                                   t))
                         tracks)))]
    (-> grid (update :columns resize column-total) (update :rows resize row-total))))

(defn plot-grid
  "The generated grid: [[facet-grid]] when the plot is faceted (`[:split :facets :dims]` non-empty), else
  [[single-panel-grid]]; plots with `:aspect :iso` get spacer tracks around the panel block (see [[iso-grid]])."
  [plot]
  (cond-> (if (seq (get-in plot [:split :facets :dims]))
            (facet-grid plot)
            (single-panel-grid plot))
    (= :iso (:aspect plot)) with-iso-spacers))
