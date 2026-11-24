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

  Faceted plots use.

  Legends ([[cljplot2.content.legend/legends]]) add columns `:legend-gap` (fixed, theme `[:legend :gap]`) and `:legend`
  (measured) after `:x-overhang`, and an item `:legends` spanning the panel rows (from the top strip) with
  `{:kind :legends}` content: the legends stacked top-down, wrapping into further columns when too tall.

  Functions: [[plot-grid]] (with [[panel-layout]], [[facet-panels]], [[estimated-panel-size]]), grids [[iso-grid]], [[titled-custom-grid]], [[legend-only-grid]], arrangements [[arrange-grid]], [[arrange-entries]]; schemas [[arrange-schema]], [[labels-schema]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.label :as label]
            [cljplot2.content.legend :as legend]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [clojure.set :as set])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private plot-labels
  "Plot label key -> grid item id, content defaults, placement and description, in item order."
  [[:title {:id :title :defaults {:role :title} :column {:from :y-title :to :x-overhang} :row :title
            :description "plot title above the panels (role :title)"}]
   [:x {:id :x-title :defaults {:role :axis-title} :column :panel :row :x-title
        :description "x axis title (role :axis-title)"
        :default-doc "the first chart's suggested title (histogram, density, ecdf, qq, pp ...), else none"}]
   [:y {:id :y-title :defaults {:role :axis-title :rotation 270} :column :y-title :row :panel
        :description "y axis title (role :axis-title, rotated 270)"
        :default-doc "the first chart's suggested title (histogram, density, ecdf, qq, pp ...), else none"}]])

(def labels-schema
  "Plot `:labels`: `{:title :x :y}`, each a text or a map of [[cljplot2.content.label/label-options]]; `nil` - no label."
  (into [:map {:closed true}]
        (map (fn [[k {:keys [description default-doc]}]]
               (schema/nilable k (cond-> {:description description} default-doc (assoc :cljplot2/default-doc default-doc))
                               [:multi {:dispatch map?} [true label/label-options] [false any?]])))
        plot-labels))

(defn- label-items
  "Gives the grid items of plot labels, in the fixed order title, x, y.

  Parameters:

  - `labels` (map): the plot's `:labels` (see the namespace doc); a `nil` label adds no item.
  - `spans` (map): label key -> `{:column :row}` overriding the single-panel placement.

  Returns a sequence of items `{:id :column :row :content}`, content kind `:label`."
  [labels spans]
  (keep (fn [[k {:keys [id defaults column row]}]]
          (when-some [label (get labels k)]
            {:id id :column (get-in spans [k :column] column) :row (get-in spans [k :row] row)
             :content (assoc (merge defaults (if (map? label) label {:text label})) :kind :label)}))
        plot-labels))

(defn- axis-shown?
  "Tests whether a position scale's axis is drawn.

  Parameters:

  - `scale-specs` (map): the plot's scale specs.
  - `k` (keyword): the position scale, `:x` or `:y`.

  Returns true when `scale-specs` has a spec for `k` whose axis is drawn (see [[cljplot2.scale.position/axis-drawn?]]), else false."
  [scale-specs k]
  (and (contains? scale-specs k) (scale/axis-drawn? (get scale-specs k))))

(defn- axis-items
  "Gives the grid items of a scale's axis on a panel side.

  The axis sits beside the panel; its end overhang (room for the last tick label) takes the `:x-overhang` column or the `:y-overhang` row, both start overhangs the corner cell (`:y-axis` column, `:x-axis` row).

  Parameters:

  - `side` (keyword): `:bottom` (a horizontal scale, the axis under the panel) or `:left` (a vertical scale, the axis left of it).
  - `k` (keyword): the scale drawn there.

  Returns a vector of the axis item and its two overhang items."
  [side k]
  (case side
    :bottom [{:id :x-axis :column :panel :row :x-axis :content {:kind :axis :scale k}}
             {:id :x-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale k :end :start}}
             {:id :x-axis-end :column :x-overhang :row :x-axis :content {:kind :axis-overhang :scale k :end :end}}]
    :left [{:id :y-axis :column :y-axis :row :panel :content {:kind :axis :scale k}}
           {:id :y-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale k :end :start}}
           {:id :y-axis-end :column :y-axis :row :y-overhang :content {:kind :axis-overhang :scale k :end :end}}]))

(defn- side-scales
  "Gives the scale drawn on each axis side of a plot.

  Parameters:

  - `flip?`: the plot's `:flip` (truthy swaps the scales).

  Returns `[[:bottom k] [:left k]]` - the horizontal scale under the panel, the vertical one left of it."
  [flip?]
  (let [horizontal (scale/horizontal-key flip?)]
    [[:bottom horizontal] [:left (if (= horizontal :x) :y :x)]]))

(defn- side-labels
  "Gives the plot labels keyed by axis side slot.

  Slot `:x` is the title under the panel, `:y` the title left of it; a flipped plot draws its `:x` scale vertically, so its labels swap slots.

  Parameters:

  - `labels` (map or other): the plot's `:labels`.
  - `flip?`: the plot's `:flip`.

  Returns `labels` with `:x` and `:y` swapped when flipped (a map), else as given."
  [labels flip?]
  (if (and flip? (map? labels))
    (set/rename-keys labels {:x :y :y :x})
    labels))

(defn- legend-tracks
  "Gives the columns added for the legends.

  Parameters:

  - `legends` (sequence): the plot's legend contents.
  - `theme` (map): the merged theme, `[:legend :gap]` read.

  Returns a fixed gap column and a measured legend column; empty without legends."
  [legends theme]
  (if (seq legends)
    [{:id :legend-gap :size [:fixed (get-in theme [:legend :gap])]} {:id :legend :size [:measured]}]
    []))

(defn- legend-items
  "Gives the legends item.

  Parameters:

  - `legends` (sequence): the plot's legend contents.
  - `row-span`: the row (or `[from to]` span) the legend column item covers.

  Returns a vector of one `:legends` item; empty without legends."
  [legends row-span]
  (if (seq legends)
    [{:id :legends :column :legend :row row-span :content {:kind :legends :legends legends}}]
    []))

(def ^:private margin-sides
  "Margin track id -> the side its alignment padding is keyed by."
  {:margin-left :left :margin-right :right :margin-top :top :margin-bottom :bottom})

(defn- margin-track-fn
  "Gives a function of a margin track id (`:margin-left` ...) to its fixed track.

  Parameters:

  - `plot` (map): `:theme` (`[:plot :margin]`) and the internal `:alignment-padding` (set by multiplot panel alignment, see [[cljplot2.multiplot]]).

  Returns `(fn [id] track)`: the track's size is the theme margin plus that side's alignment padding."
  [{:keys [theme alignment-padding]}]
  (let [margin (double (get-in theme [:plot :margin]))]
    (fn [id] {:id id :size [:fixed (m/+ margin (double (get alignment-padding (margin-sides id) 0)))]})))

(defn- plot-margin-tracks
  "Gives the tracks of a plot margin on one side.

  A margin panel has a fixed size and a gap (theme `[:facet :gap]`) next to the main panel; with an axis, a measured track on its far side holds the room the magnitude axis's end tick label sticks out.

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `side` (keyword): `:top` (rows above the panel) or `:right` (columns right of it).
  - `theme` (map): the merged theme.

  Returns the tracks in order from the main panel outwards (`:top` - from the top down); empty without that margin."
  [margins side theme]
  (if-let [{:keys [size axis]} (get margins side)]
    (let [gap {:id (keyword (str (name side) "-margin-gap")) :size [:fixed (get-in theme [:facet :gap])]}
          track {:id (keyword (str (name side) "-margin")) :size [:fixed size]}
          overhang (when axis {:id (keyword (str (name side) "-margin-overhang")) :size [:measured]})]
      (remove nil? (if (= side :top) [overhang track gap] [gap track overhang])))
    []))

(defn- plot-margin-items
  "Gives the grid items of the plot margins.

  Each margin gets a panel `{:kind :panel :margin side}` beside the main panel and, with `:axis`, its magnitude axis `{:kind :axis :scale :y :margin side}` (the top margin's in the y-axis column, the right margin's in the x-axis row) plus an overhang item for its end tick label.

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.

  Returns a lazy sequence of items; empty without margins."
  [margins]
  (mapcat (fn [[side {:keys [axis]}]]
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
          margins))

(defn- single-panel-grid
  "Gives the grid of a single-panel plot.

  The grid holds the labels, the axes of the drawn position scales, the legends, the plot margins (a `:top-margin` row above the panel, a `:right-margin` column right of it before the legend, each next to a gap) and the theme margin.

  Parameters:

  - `plot` (map): the plot built up to its scale specs - `:labels`, `:theme` (merged), `:scale-specs`, `:flip`, `:margin-stats` (see [[cljplot2.pipeline]]), `:alternate-panel` (true shades the panel as an odd facet panel).

  Returns the grid map `{:columns :rows :items}` (see [[cljplot2.layout.grid]])."
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
  "Gives the strip content of a facet level.

  Parameters:

  - `text`: the strip text.
  - `level`: the facet level.
  - `levels` (sequence): every level of the facet dimension.

  Returns `{:kind :strip :text text}`; when every level is a shingle interval also `:interval {:lo :hi :min :max}` - the level within the range of all levels as doubles (a shaded bar, see [[cljplot2.content.strip]])."
  [text level levels]
  (cond-> {:kind :strip :text text}
    (and (seq levels) (every? shingle/interval? levels))
    (assoc :interval {:lo (:lo level) :hi (:hi level)
                      :min (v/mn (map :lo levels)) :max (v/mx (map :hi levels))})))

(defn facet-panels
  "Gives the panels of a facet split and their layout shape.

  Parameters:

  - `split-facets` (map): `{:dims :levels}` of the split stage.
  - `scales` (map): the plot's scale specs (facet labels).
  - `facets` (map): the plot's `:facets` options (`:order`, wrap shape).

  Returns `{:rows :cols :wrap :panels}`: panels `{:key :label :row :col}` (a wrap layout) or `{:key :row :col}` with `:col-levels`, `:row-levels`, `:col-labels`, `:row-labels` (a grid layout)."
  [{:keys [dims levels]} scales {:keys [order] :as facets}]
  (if (= dims [:facet])
    (let [ls (:facet levels)
          [rows cols] (facet/wrap-shape (count ls) facets)]
      {:rows rows :cols cols :wrap true :levels ls
       :panels (vec (map-indexed (fn [i level]
                                   (let [[r c] (facet/wrap-positions i rows cols (or order :top-down))]
                                     {:key level :label (facet/label (:facet scales) level) :row r :col c}))
                                 ls))})
    (let [col-levels (get levels :facet-col [::none])
          row-levels (get levels :facet-row [::none])
          key-of (fn [cl rl] (cond (= 2 (count dims)) [cl rl] (some #{:facet-col} dims) cl :else rl))]
      {:rows (count row-levels) :cols (count col-levels) :wrap false
       :col-levels col-levels :row-levels row-levels
       :col-labels (when (contains? levels :facet-col) (mapv #(facet/label (:facet-col scales) %) col-levels))
       :row-labels (when (contains? levels :facet-row) (mapv #(facet/label (:facet-row scales) %) row-levels))
       :panels (vec (for [[r rl] (map-indexed vector row-levels) [c cl] (map-indexed vector col-levels)]
                      {:key (key-of cl rl) :row r :col c}))})))

(defn panel-layout
  "Gives the numbers of panel columns and rows of a plot.

  Parameters:

  - `split-facets` (map): the split stage's facets, `:dims` and `:levels`.
  - `facets` (map): the plot's `:facets` options.

  Returns `{:columns c :rows r}` as [[facet-grid]] lays the panels out (strips not formatted); `{:columns 1 :rows 1}` without facets.

  Throws as [[cljplot2.data.facet/wrap-shape]] for a wrap layout too small for its panels."
  [{:keys [dims levels]} facets]
  (cond
    (empty? dims) {:columns 1 :rows 1}
    (= dims [:facet]) (let [[rows cols] (facet/wrap-shape (count (:facet levels)) facets)] {:columns cols :rows rows})
    :else {:columns (count (get levels :facet-col [nil])) :rows (count (get levels :facet-row [nil]))}))

(def ^:private estimate-tick-label
  "Stand-in tick label of an axis whose ticks are unknown (before stats)."
  "00")

(defn- size-height
  "Gives the height of a natural size.

  Parameters:

  - `size` (`Vec2` or `nil`): a measured natural size.

  Returns its y; 0 for `nil` (nothing measured)."
  ^double [size]
  (if size (.y ^Vec2 size) 0.0))

(defn- size-width
  "Gives the width of a natural size.

  Parameters:

  - `size` (`Vec2` or `nil`): a measured natural size.

  Returns its x; 0 for `nil` (nothing measured)."
  ^double [size]
  (if size (.x ^Vec2 size) 0.0))

(defn- axis-room
  "Gives the pixels an axis takes across itself.

  Parameters:

  - `theme` (map): the merged theme, `[:axis k]` `:tick-length` and `:label-gap` read.
  - `k` (keyword): the axis's scale, `:x` or `:y`.
  - `across` (double): the depth of its tick labels in pixels.

  Returns the tick length, the label gap and `across` summed."
  ^double [theme k ^double across]
  (m/+ (double (get-in theme [:axis k :tick-length])) (double (get-in theme [:axis k :label-gap])) across))

(defn estimated-panel-size
  "Gives the expected size of one panel of a plot before its stats are known.

  The plot's `:width` / `:height` lose the room the grid gives the plot margins, the title and axis titles, the axes (stand-in two-digit tick labels, no end overhangs), the plot margin panels, the legends and the facet strips and gaps, and the rest is split among the panels; a wrap layout with free or sliced scales also loses an axis per panel row or column. It is an estimate: real tick labels and wrapped legends differ.

  Parameters:

  - `plot` (map): `:options` (`:width`, `:height`, `:hint`), `:theme` (merged), `:labels`, `:flip`, `:facets`.
  - `legends` (sequence): legend contents, e.g. [[cljplot2.content.legend/provisional-legends]].
  - `margins` (map): plot margins `{side {:size px}}`.
  - `panel-layout` (map): `{:columns :rows}` (see [[panel-layout]]).
  - `facets?`: truthy when strips are present.
  - `grid?`: truthy for facet-grid strips (one row, one column), else wrap strips over every row.

  Returns `[w h]` in pixels, each at least 1. Measures text on a scratch canvas."
  [{:keys [options theme labels flip facets]} legends margins {^long columns :columns ^long rows :rows} facets? grid?]
  (measure/call-with-scratch-canvas
   (:hint options)
   (fn [canvas]
     (let [env {:canvas canvas :theme theme}
           natural (fn [content] (measure/natural-size content env))
           text-size (fn [role l] (when (some? l)
                                    (natural (merge {:kind :label :role role} (if (map? l) (dissoc l :rotation) {:text (str l)})))))
           {:keys [x y title]} (side-labels labels flip)
           margin (double (get-in theme [:plot :margin]))
           tick-w (size-width (natural (axis/tick-label theme :y estimate-tick-label)))
           tick-h (size-height (natural (axis/tick-label theme :x estimate-tick-label)))
           gap (double (get-in theme [:facet :gap]))
           strip (if facets? (size-height (natural {:kind :strip :text "Xg"})) 0.0)
           margin-room (fn ^double [side] (if-let [{:keys [size]} (get margins side)] (m/+ (double size) gap) 0.0))
           legend-room (if (seq legends)
                         (m/+ (double (get-in theme [:legend :gap])) (size-width (natural {:kind :legends :legends legends})))
                         0.0)
           horizontal (scale/horizontal-key flip)
           free? (fn [k] (and (not grid?) (not= :same (facet/scale-relation facets k))))
           inner-y-axes (if (free? (if (= horizontal :x) :y :x)) (m/* (m/long-dec columns) (axis-room theme :y tick-w)) 0.0)
           inner-x-axes (if (free? horizontal) (m/* (m/long-dec rows) (axis-room theme :x tick-h)) 0.0)
           w (m/- (double (:width options)) (m/* 2.0 margin) (size-height (text-size :axis-title y))
                  (axis-room theme :y tick-w) (double (margin-room :right)) legend-room
                  (m/* gap (m/long-dec columns)) (if grid? strip 0.0) inner-y-axes)
           h (m/- (double (:height options)) (m/* 2.0 margin) (size-height (text-size :title title))
                  (double (margin-room :top)) (axis-room theme :x tick-h)
                  (size-height (text-size :axis-title x)) (m/* gap (m/long-dec rows)) (if grid? strip (m/* strip rows)) inner-x-axes)]
       [(m/max 1.0 (m// w columns)) (m/max 1.0 (m// h rows))]))))

(defn- panel-weights
  "Gives the weights of the panel columns or rows of a facet layout.

  Parameters:

  - `facets` (map): the plot's `:facets` options.
  - `k` (keyword): `:widths` (columns) or `:heights` (rows).
  - `n` (long): the number of panel columns or rows.

  Returns the weights as a vector; `nil` (equal panels) when not given.

  Throws `ex-info {:key k}` for a count other than `n`."
  [facets k ^long n]
  (when-let [ws (get facets k)]
    (when (m/not== n (count ws))
      (schema/fail! (str "Facet " k " needs one weight per panel " (if (= k :widths) "column" "row") " (" n "), got "
                         (count ws) ".")
                    {:key k :in [:facets k] :value ws}))
    (vec ws)))

(defn- panel-track
  "Gives the flexible track of a panel column or row.

  Parameters:

  - `weights` (vector or `nil`): the panel weights (see [[panel-weights]]).
  - `i` (long): the column or row index.

  Returns the track `{:id [:panel i] :size [:flexible w]}`, unweighted when `weights` is `nil`."
  [weights i]
  {:id [:panel i] :size (if weights [:flexible (weights i)] [:flexible])})

(defn- facet-axis-items
  "Gives the axis items of a facet layout.

  The horizontal scale's axes go under the panels, the vertical's left of them, each with the `:panel` key whose scales it draws (see [[cljplot2.scale.position/panel-scales-of]]). A free or sliced relation in a wrap layout puts an axis at every panel, in its own `[:x-axis r]` row or `[:y-axis c]` column; otherwise each panel column gets an axis in the `:x-axis` row drawn with its bottom panel, and each panel row an axis in the `:y-axis` column drawn with its first panel. An axis with another panel's axis next to it gets `:next-panel-gap` (labels crossing the next axis's labels are skipped, see [[cljplot2.content.axis]]); with panel-local scales also `:next-panel` - the next panel's key - and vertical axes get both towards the axis below unless the panel rows are stacked apart. Overhang items reserve room for end labels as in the single-panel grid; the horizontal start overhang sits in the first y-axis column.

  Parameters:

  - `panels` (sequence): panel maps `{:row :col :key}`.
  - `cols` (long): the number of panel columns.
  - `gap`: the facet gap in pixels.
  - `settings` (map): `:h-key`, `:v-key` - the horizontal and vertical scales; `:h-shown`, `:v-shown` - their axes drawn; `:free-h`, `:free-v` - an axis at every panel; `:local-h`, `:local-v` - panel-local scales (neighbouring axes may differ); `:stacked` - strips or axis rows separate the panel rows.

  Returns a sequence of grid items."
  [panels cols gap {:keys [h-key v-key h-shown v-shown free-h free-v local-h local-v stacked]}]
  ;; panels are passed around as maps (`nil` - no panel): a panel's `:key` may itself be `nil` (the NA facet level)
  (let [at (into {} (map (juxt (juxt :row :col) identity)) panels)
        h-axis (fn [content next-panel]
                 (cond-> content
                   next-panel (assoc :next-panel-gap gap)
                   (and local-h next-panel) (assoc :next-panel (:key next-panel))))
        v-axis (fn [content next-panel]
                 (cond-> content
                   (and local-v (not stacked) next-panel) (assoc :next-panel-gap gap :next-panel (:key next-panel))))
        bottom-of (fn [c] (last (sort-by :row (filter #(= c (:col %)) panels))))
        first-of (fn [r] (first (sort-by :col (filter #(= r (:row %)) panels))))
        rows (sort (distinct (map :row panels)))
        last-row (last rows)
        next-row (fn [r] (second (drop-while #(not= r %) rows)))
        with-panel (fn [item panel] (cond-> item panel (assoc-in [:content :panel] (:key panel))))
        [bottom-start x-end] (rest (axis-items :bottom h-key))
        x-start (cond-> bottom-start free-v (assoc :column [:y-axis 0]))
        [y-start y-end] (rest (axis-items :left v-key))]
    (concat
     (when h-shown
       (if free-h
         (concat
          (for [{:keys [row col] :as panel} panels]
            (with-panel {:id [:x-axis row col] :column [:panel col] :row [:x-axis row]
                         :content (h-axis {:kind :axis :scale h-key} (at [row (m/long-inc (long col))]))}
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
                                          (when (m/< (long c) (m/long-dec (long cols))) (bottom-of (m/long-inc (long c)))))}
              (bottom-of c)))
          [(with-panel x-start (bottom-of 0)) (with-panel x-end (bottom-of (m/long-dec (long cols))))])))
     (when v-shown
       (concat
        (if free-v
          (for [{:keys [row col] :as panel} panels]
            (with-panel {:id [:y-axis row col] :column [:y-axis col] :row [:panel row]
                         :content (v-axis {:kind :axis :scale v-key} (at [(m/long-inc (long row)) col]))}
              panel))
          (for [r rows]
            (with-panel {:id [:y-axis r] :column :y-axis :row [:panel r]
                         :content (v-axis {:kind :axis :scale v-key} (some-> (next-row r) first-of))}
              (first-of r))))
        [(with-panel y-start (at [last-row 0])) (with-panel y-end (at [(first rows) 0]))])))))

(defn- facet-grid
  "Gives the grid of a faceted plot (see [[cljplot2.data.facet]]).

  Panels `[:panel r c]` are laid out in rows and columns with gaps (theme `[:facet :gap]`), weighted by `:facets` `:widths` / `:heights`; strips go above each panel (wrap) or as column strips above the top row and row strips on the right (grid), none with `:facets {:strips false}`. An x axis goes under every panel column and a y axis beside every panel row, overhangs at the outer ends; a wrap layout with a free or sliced relation has an axis at every panel instead (see [[facet-axis-items]]). Panels with odd `r + c` are shaded (`:alternate`). Legends as in the single-panel grid.

  Parameters:

  - `plot` (map): the plot built up to its scale specs - `:labels`, `:theme` (merged), `:scale-specs`, `:scales`, `:facets`, `:split`, `:flip`.

  Returns the grid map `{:columns :rows :items}` (see [[cljplot2.layout.grid]]).

  Throws `ex-info {:key :widths}` / `{:key :heights}` for a weight count other than the panel columns / rows."
  [{:keys [labels theme scale-specs scales facets split flip] :as plot}]
  (let [legends (legend/legends plot)
        [[_ h-key] [_ v-key]] (side-scales flip)
        gap (get-in theme [:facet :gap])
        margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        strips? (facet/strips? facets)
        {^long rows :rows ^long cols :cols :keys [wrap panels col-labels row-labels levels col-levels row-levels]}
        (cond-> (facet-panels (:facets split) scales facets)
          (not strips?) (assoc :col-labels nil :row-labels nil))
        widths (panel-weights facets :widths cols)
        heights (panel-weights facets :heights rows)
        free-h (and wrap (not= :same (facet/scale-relation facets h-key)))
        free-v (and wrap (not= :same (facet/scale-relation facets v-key)))
        wrap-strips? (and wrap strips?)
        last-col [:panel (m/long-dec cols)]
        last-row [:panel (m/long-dec rows)]
        gap-track (fn [prefix i] {:id [prefix i] :size [:fixed gap]})
        column-tracks (butlast (mapcat (fn [c] (cond-> []
                                                 free-v (conj (measured [:y-axis c]))
                                                 true (conj (panel-track widths c) (gap-track :gap c))))
                                       (range cols)))
        row-tracks (butlast (mapcat (fn [r] (cond-> []
                                              wrap-strips? (conj (measured [:strip r]))
                                              true (conj (panel-track heights r))
                                              free-h (conj (measured [:x-axis r]))
                                              true (conj (gap-track :gap r))))
                                    (range rows)))]
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
     :items (-> (mapv (fn [{panel-key :key :keys [row col]}]
                        {:id [:panel row col] :column [:panel col] :row [:panel row]
                         :content {:kind :panel :panel panel-key :alternate (m/odd? (m/long-add (long row) (long col)))}})
                      panels)
                (into (facet-axis-items panels cols gap {:h-key h-key :v-key v-key :free-h free-h :free-v free-v
                                                         :local-h (or (not= :same (facet/scale-relation facets h-key)) (some? widths))
                                                         :local-v (or (not= :same (facet/scale-relation facets v-key)) (some? heights))
                                                         :stacked (or wrap-strips? free-h)
                                                         :h-shown (axis-shown? scale-specs h-key)
                                                         :v-shown (axis-shown? scale-specs v-key)}))
                (cond-> wrap-strips? (into (map (fn [{panel-key :key :keys [label row col]}]
                                                  {:id [:strip row col] :column [:panel col] :row [:strip row]
                                                   :content (strip-content label panel-key levels)})
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
  "Gives a custom grid under a title row.

  Parameters:

  - `custom-grid` (map): the plot's own grid.
  - `labels` (map): the plot labels, only `:title` used (the plot schema allows no other).
  - `theme` (map): the merged theme, `[:plot :margin]` read.

  Returns `custom-grid` itself without a title; else a grid of rows `:margin-top` (the theme margin), `:title` (measured) and `:body` (flexible, holding `custom-grid` nested)."
  [custom-grid labels theme]
  (if (nil? (:title labels))
    custom-grid
    {:columns [{:id :body :size [:flexible]}]
     :rows [{:id :margin-top :size [:fixed (get-in theme [:plot :margin])]} {:id :title :size [:measured]}
            {:id :body :size [:flexible]}]
     :items (into [{:id :body :column :body :row :body :content (assoc custom-grid :kind :grid)}]
                  (label-items {:title (:title labels)} {:title {:column :body :row :title}}))}))

(defn- arrange-shape
  "Gives the numbers of rows and columns of an arranged layout.

  Parameters:

  - `n` (long): the number of plots.
  - `columns`, `rows` (longs or `nil`): the requested columns and rows.

  Returns `[rows cols]` (longs): both given - as given; one given - the other from the plot count, rounded up; none - one column."
  [^long n columns rows]
  (cond
    (and rows columns) [(long rows) (long columns)]
    columns [(long (m/ceil (m// (double n) (double columns)))) (long columns)]
    rows [(long rows) (long (m/ceil (m// (double n) (double rows))))]
    :else [n 1]))

(defn- arrange-problem
  "Gives why an `:arrange` spec (valid entries) can't be laid out, or `nil`.

  Parameters:

  - `arrange` (map): `:plots`, `:columns`, `:rows`, `:widths`, `:heights`.

  Returns a message: too few cells, `:widths` / `:heights` not one per column / row, or a `:measured` width over a column with a plot that isn't `:legend-only`."
  [{:keys [plots columns rows widths heights]}]
  (let [cells (vec plots)
        n (count cells)
        [^long r ^long c] (arrange-shape n columns rows)]
    (cond
      (m/< (m/* r c) n) (str "layout " r "x" c " is too small for " n " plots")
      (and widths (m/not== c (count widths))) (str ":widths should have one weight per column (" c ")")
      (some (fn [j] (and (= :measured (nth widths j))
                         (not-every? :legend-only (keep (fn [i] (get cells (m/long-add (long j) (m/* c (long i))))) (range r)))))
            (range (count widths)))
      ":widths :measured sizes a column of :legend-only plots only"
      (and heights (m/not== r (count heights))) (str ":heights should have one weight per row (" r ")"))))

(def arrange-entries
  "Map entries of a multiplot layout (plot `:arrange`): a shared group (`:arrange`), so other multiplot options can
  reuse them."
  (schema/in-fragment
   :arrange
   [[:plots {:description "the sub-plots, filling the grid row by row"} [:sequential {:min 1} :map]]
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
                    schema/bool)]))

(def arrange-schema
  "Plot `:arrange`, see [[cljplot2.multiplot/arrange]]; the plots are validated when built as sub-plots."
  (schema/with-rules
   (into [:map {:closed true :cljplot2/type-doc "an arrange map {:plots :columns :rows :widths :heights :align :equal-panels}"}]
         arrange-entries)
   [arrange-problem]))

(defn- arrange-weights
  "Gives the weights of arranged columns or rows.

  Parameters:

  - `ws` (sequence or `nil`): the given weights.
  - `n` (long): the number of columns or rows.

  Returns `ws`; `n` equal weights (a lazy sequence of 1s) when `nil`."
  [ws ^long n]
  (or ws (repeat n 1)))

(defn- weighted-tracks
  "Gives weighted tracks with fixed gaps between them.

  Parameters:

  - `prefix` (keyword): the track id prefix - tracks are `[prefix i]`.
  - `gap-prefix` (keyword): the gap id prefix - gaps are `[gap-prefix i]`.
  - `ws` (sequence): the weights; an entry `[:fixed px]` makes a fixed track.
  - `gap`: the gap in pixels.

  Returns a lazy sequence of tracks and gaps, a gap between each two tracks; empty for no weights."
  [prefix gap-prefix ws gap]
  (butlast (mapcat (fn [i w] [{:id [prefix i] :size (if (vector? w) w [:flexible w])} {:id [gap-prefix i] :size [:fixed gap]}])
                   (range) ws)))

(defn legend-only-grid
  "Gives the grid of a plot with `:legend-only true`.

  The grid holds only the legends in a measured column between the plot margins, under an optional title row; no panel, axes or axis titles.

  Parameters:

  - `legends` (sequence): legend contents (see [[cljplot2.content.legend/legends]]).
  - `labels` (map): the plot labels, only `:title` used.
  - `theme` (map): the merged theme, `[:plot :margin]` read.

  Returns the grid map `{:columns :rows :items}`."
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
  "Gives the grid of an arranged plot.

  Cells `[:plot i]` are filled row by row (columns `[:column c]`, rows `[:row r]`) with theme `[:multiplot :gap]` between them, theme `[:plot :margin]` around, and an optional title row above the cells, inside the margins.

  Parameters:

  - `arrange` (map): the plot's `:arrange` spec `{:plots [p ...] :columns :rows :widths :heights}`, valid by [[arrange-schema]], its `:measured` widths resolved to `[:fixed px]` (see the pipeline's grid stage).
  - `labels` (map): the plot labels, only `:title` used.
  - `theme` (map): the merged theme.

  Returns the grid map `{:columns :rows :items}`."
  [{:keys [plots columns rows widths heights]} labels theme]
  (let [[^long r ^long c] (arrange-shape (count plots) columns rows)
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
                                    {:id [:plot i] :column [:column (m/long-rem (long i) c)] :row [:row (m/long-quot (long i) c)]
                                     :content {:kind :plot :plot p}})
                                  plots))
                (into (label-items (select-keys labels [:title])
                                   {:title {:column {:from [:column 0] :to [:column (m/long-dec c)]} :row :title}})))}))

(defn- with-iso-spacers
  "Gives a grid with spacer tracks inside its outer margin tracks.

  Parameters:

  - `grid` (map): a generated grid whose first and last track on each axis are margins.

  Returns the grid with fixed 0 tracks `:iso-before` and `:iso-after` on both axes (see [[iso-grid]])."
  [grid]
  (let [insert (fn [tracks] (-> [(first tracks) {:id :iso-before :size [:fixed 0]}]
                                (into (butlast (rest tracks)))
                                (conj {:id :iso-after :size [:fixed 0]} (peek tracks))))]
    (-> grid (update :columns insert) (update :rows insert))))

(defn iso-grid
  "Gives a grid with iso spacers sized.

  The spacer total of each axis is split between its two sides (`:iso-before` gets the floor of half); panels stay flexible and take the rest, so the panel block is centred.

  Parameters:

  - `grid` (map): a generated grid with iso spacers (see [[with-iso-spacers]]).
  - `column-total`, `row-total` (longs): whole pixels of spacer per axis.

  Returns the grid with its spacer tracks fixed."
  [grid column-total row-total]
  (let [resize (fn [tracks ^long total]
                 (let [before (m/long-quot total 2)]
                   (mapv (fn [t] (case (:id t)
                                   :iso-before (assoc t :size [:fixed before])
                                   :iso-after (assoc t :size [:fixed (m/long-sub total before)])
                                   t))
                         tracks)))]
    (-> grid (update :columns resize column-total) (update :rows resize row-total))))

(defn plot-grid
  "Gives the generated grid of a plot.

  Parameters:

  - `plot` (map): the plot built up to its scale specs.

  Returns [[facet-grid]] when the plot is faceted (`[:split :facets :dims]` non-empty), else [[single-panel-grid]]; a plot with `:aspect :iso` gets spacer tracks around the panel block (see [[iso-grid]])."
  [plot]
  (cond-> (if (seq (get-in plot [:split :facets :dims]))
            (facet-grid plot)
            (single-panel-grid plot))
    (= :iso (:aspect plot)) with-iso-spacers))
