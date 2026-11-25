(ns cljplot2.layout.plot-grid
  "Grid generated from plot keys (see [[cljplot2.layout.grid]] for the grid shape).

  Single panel:

  * columns `:margin-left | :y-title | :y-axis | :panel | :x-overhang | :margin-right`
  * rows `:margin-top | :title | :y-overhang | :panel | :x-axis | :x-title | :margin-bottom`

  Margins are fixed (theme `[:plot :margin]`), the panel is flexible, every other track is measured.
  Axis tracks are reserved for axes and hold no items yet.

  Plot `:margins` (single panel) add a fixed track and a gap beside the panel per side: `:top-margin` rows above
  `:y-overhang`, `:bottom-margin` rows between the panel and `:x-axis`, `:left-margin` columns between `:y-axis` and
  the panel, `:right-margin` columns after `:x-overhang` - the main axes stay on the outer side. A left or bottom
  margin also adds a measured inner track next to the panel (`:left-margin-inner`, `:bottom-margin-inner` - the
  counterpart of `:x-overhang` / `:y-overhang` beside the right / top margins) holding the main axis's start
  overhang, so a margin's magnitude axis labels never touch the main axis labels. A margin's magnitude axis takes
  the `:y-axis` column (top, bottom; the `:left-margin-inner` column with a left margin, next to its own panel) or
  the `:x-axis` row (right, left; the `:bottom-margin-inner` row with a bottom margin), its start overhang (the tick
  label next to the main panel) a measured `-axis-start` track between the margin and its gap, its end overhang a
  measured track on the margin's far side - the main axis's and the margin axis's labels stay a gap apart.

  A faceted plot repeats these tracks at every panel column (`:left`, `:right` - ids `[:left-margin c]` ...) and
  panel row (`:top`, `:bottom` - `[:top-margin r]` ...), so every panel has its own margins and the margins of a
  panel row or column line up; right and top margins there also add a measured inner track next to the panel
  (`[:right-margin-inner c]`, `[:top-margin-inner r]` - the `:x-overhang` / `:y-overhang` of the panel column / row)
  and every panel axis's start and end overhangs go in its column's / row's inner tracks (see `facet-axis-items`); a
  margin panel `{:kind :panel :margin side :panel k}` draws panel `k`'s margin data with that panel's scale (see
  `cljplot2.pipeline` margin stats); magnitude axes stand where the panels' axes of the other dimension are (every
  panel with free scales, else the first panel of a row / the bottom one of a column, see `axis-panels`).

  Invalid-value bands (charts' `:invalid :show`, stats `:invalid-bands`, see [[cljplot2.content.invalid-band]] and
  [[invalid-bands]]) add, per panel column / row of the generated grids, a measured band track outside the axis line
  next to the axis (a horizontal dimension's band right of the `:y-axis` column, a vertical one's above the `:x-axis`
  row), a gap and a measured inner track holding the panel axis's start overhang ([[invalid-band-tracks]]; a left /
  bottom plot margin's inner track takes it instead); the band's axis (its label) goes in the main axis's track. A
  custom grid has no bands.

  `:arrange` cells fill the grid row by row; a plot with a row span (`:row-spans`) covers that many rows and later
  plots skip the covered cells ([[arrange-cells]] - the one place giving a plot's cell); a `:measured` column holds
  legend cells only ([[legend-cell?]]: `:legend-only` plots or one-column arranges of them), measured at the height
  its cells get (a legend taller than its cell wraps into further columns); a `:measured` row holds `:legend-only`
  plots only, as tall as their legends (scaled down in proportion when the measured rows don't fit).

  Plot `:labels` - `{:title t :x t :y t}`, `t` a text (a string, a formula or runs value, see [[cljplot2.data.text]]) or a map of label options (`:text`, style overrides,
  `:align`, `:rotation`) merged into the label content; a missing or `nil` label adds no item.
  `:title` uses the `:title` text role, `:x` and `:y` the `:axis-title` role; `:y` is rotated 270 by default.
  The label `:kind` can't be overridden. An axis title longer than its panels (the width under them, the height
  beside them - one title spanning all facet panels) is fitted to them: wrapped at word boundaries (each line into at
  most 3, the title track grows), then shrunk down to its text role's `:min-size` (`:axis-title` 8 points in the
  default theme), then cut with an ellipsis (label content `:fit true`, see [[cljplot2.content.label/fitted-label]];
  always on - a clipped title is a defect); a title that fits is drawn as given. A title rotated across its own
  track (a y title with `:rotation 0`, an x title with `:rotation 90`) isn't fitted.

  An axis item (`:x-axis` below the panel, `:y-axis` left of it) is added for each scale in `:scale-specs` (not for
  one with `:axis false`), with
  overhang items reserving room for its end tick labels: x end in `:x-overhang`, y end in `:y-overhang`, both
  start ends in the corner cell (`:y-axis` column, `:x-axis` row). Track and item names are by side (`x` - under
  the panel, `y` - left of it): with `:flip true` the `:y` scale's axis and `:labels :y` title go under the panel,
  the `:x` scale's axis and `:labels :x` title left of it.
  Item order: `:panel`, axes and their overhangs (x, then y), `:title`, `:x-title`, `:y-title` (absent ones skipped).

  Faceted plots lay their panels out in rows and columns with strips and per-column / per-row axes (see `facet-grid`).

  Legends ([[cljplot2.content.legend/legends]]) add columns `:legend-gap` (fixed, theme `[:legend :gap]`) and `:legend`
  (measured) after `:x-overhang`, and an item `:legends` spanning the panel rows (from the top strip) with
  `{:kind :legends}` content: the legends stacked top-down, wrapping into further columns when too tall.

  Functions: [[plot-grid]] (with [[panel-layout]], [[facet-panels]], [[estimated-panel-size]], [[invalid-bands]]), grids [[iso-grid]], [[titled-custom-grid]], [[legend-only-grid]], arrangements [[arrange-grid]], [[arrange-cells]], [[legend-cell?]], [[arrange-entries]]; schemas [[arrange-schema]], [[labels-schema]]; plot label content [[label-contents]]."
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
   [:x {:id :x-title :defaults {:role :axis-title :fit true} :column :panel :row :x-title
        :description "x axis title (role :axis-title; too long - wrapped, shrunk, then cut to the panels' width)"
        :default-doc "the first chart's suggested title (histogram, density, ecdf, qq, pp ...), else none"}]
   [:y {:id :y-title :defaults {:role :axis-title :rotation 270 :fit true} :column :y-title :row :panel
        :description "y axis title (role :axis-title, rotated 270; too long - wrapped, shrunk, then cut to the panels' height)"
        :default-doc "the first chart's suggested title (histogram, density, ecdf, qq, pp ...), else none"}]])

(def labels-schema
  "Plot `:labels`: `{:title :x :y}`, each a text or a map of [[cljplot2.content.label/label-options]]; `nil` - no label."
  (into [:map {:closed true}]
        (map (fn [[k {:keys [description default-doc]}]]
               (schema/nilable k (cond-> {:description description} default-doc (assoc :cljplot2/default-doc default-doc))
                               [:multi {:dispatch label/label-options?} [true label/label-options] [false any?]])))
        plot-labels))

(defn label-contents
  "Gives the label content of each plot label.

  Parameters:

  - `labels` (map): the plot's `:labels` (see the namespace doc); a `nil` label has no content.

  Returns a sequence of `[k content]` in the order title, x, y: content kind `:label` with the label key's role and
  defaults under the label's own map (a text alone becomes its `:text`)."
  [labels]
  (keep (fn [[k {:keys [defaults]}]]
          (when-some [label (get labels k)]
            [k (assoc (merge defaults (if (label/label-options? label) label {:text label})) :kind :label)]))
        plot-labels))

(defn- label-items
  "Gives the grid items of plot labels, in the fixed order title, x, y.

  Parameters:

  - `labels` (map): the plot's `:labels` (see the namespace doc); a `nil` label adds no item.
  - `spans` (map): label key -> `{:column :row}` overriding the single-panel placement.

  Returns a sequence of items `{:id :column :row :content}`, content kind `:label`."
  [labels spans]
  (let [contents (into {} (label-contents labels))]
    (keep (fn [[k {:keys [id column row]}]]
            (when-some [content (contents k)]
              {:id id :column (get-in spans [k :column] column) :row (get-in spans [k :row] row) :content content}))
          plot-labels)))

(defn- axis-shown?
  "Tests whether a position scale's axis is drawn.

  Parameters:

  - `scale-specs` (map): the plot's scale specs.
  - `k` (keyword): the position scale, `:x` or `:y`.

  Returns true when `scale-specs` has a spec for `k` whose axis is drawn (see [[cljplot2.scale.position/axis-drawn?]]), else false."
  [scale-specs k]
  (and (contains? scale-specs k) (scale/axis-drawn? (get scale-specs k))))

(defn- margin-track-id
  "Gives the id of a plot margin track.

  Parameters:

  - `side` (keyword): the margin's side.
  - `part` (string): `\"\"` - the margin panel's track, else a suffix (`\"-gap\"`, `\"-inner\"`, `\"-overhang\"`, `\"-axis\"`, `\"-axis-end\"`).

  Returns a keyword, e.g. `:top-margin`, `:left-margin-gap`."
  [side part]
  (keyword (str (name side) "-margin" part)))

(defn- axis-items
  "Gives the grid items of a scale's axis on a panel side.

  The axis sits beside the panel; its end overhang (room for the last tick label) takes the `:x-overhang` column or the `:y-overhang` row, both start overhangs the corner cell (`:y-axis` column, `:x-axis` row) - with a left plot margin the horizontal start overhang takes the `:left-margin-inner` column instead, with a bottom one the vertical start overhang the `:bottom-margin-inner` row (between the margin and the panel, see [[plot-margin-tracks]]); without that margin, an invalid-value band of the horizontal (vertical) dimension puts it in its `:invalid-band-inner` column (row, see [[invalid-band-tracks]]).

  Parameters:

  - `side` (keyword): `:bottom` (a horizontal scale, the axis under the panel) or `:left` (a vertical scale, the axis left of it).
  - `k` (keyword): the scale drawn there.
  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `bands` (map, optional): the plot's invalid-value bands by direction (see [[invalid-bands]]).

  Returns a vector of the axis item and its two overhang items."
  ([side k margins] (axis-items side k margins nil))
  ([side k margins bands]
   (let [start-column (cond (:left margins) (margin-track-id :left "-inner") (:horizontal bands) :invalid-band-inner :else :y-axis)
         start-row (cond (:bottom margins) (margin-track-id :bottom "-inner") (:vertical bands) :invalid-band-inner :else :x-axis)]
     (case side
       :bottom [{:id :x-axis :column :panel :row :x-axis :content {:kind :axis :scale k}}
                {:id :x-axis-start :column start-column :row :x-axis :content {:kind :axis-overhang :scale k :end :start}}
                {:id :x-axis-end :column :x-overhang :row :x-axis :content {:kind :axis-overhang :scale k :end :end}}]
       :left [{:id :y-axis :column :y-axis :row :panel :content {:kind :axis :scale k}}
              {:id :y-axis-start :column :y-axis :row start-row :content {:kind :axis-overhang :scale k :end :start}}
              {:id :y-axis-end :column :y-axis :row :y-overhang :content {:kind :axis-overhang :scale k :end :end}}]))))

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

(def ^:private theme-margin-sides
  "Theme margin track id (`[:plot :margin]`, not a plot `:margins` side) -> the side its alignment padding is keyed by."
  {:margin-left :left :margin-right :right :margin-top :top :margin-bottom :bottom})

(defn- margin-track-fn
  "Gives a function of a margin track id (`:margin-left` ...) to its fixed track.

  Parameters:

  - `plot` (map): `:theme` (`[:plot :margin]`) and the internal `:alignment-padding` (set by multiplot panel alignment, see [[cljplot2.multiplot]]).

  Returns `(fn [id] track)`: the track's size is the theme margin plus that side's alignment padding."
  [{:keys [theme alignment-padding]}]
  (let [margin (double (get-in theme [:plot :margin]))]
    (fn [id] {:id id :size [:fixed (m/+ margin (double (get alignment-padding (theme-margin-sides id) 0)))]})))

(defn- margin-axis-places
  "Gives the grid places of a plot margin's magnitude axis and of its two overhang items.

  The axis lies along the margin panel's track: the top and bottom margins' axes take the `:y-axis` column - the
  `:left-margin-inner` column with a left margin, so the axis stays next to its own margin panel instead of beyond
  the left margin; the right and left margins' take the `:x-axis` row - the `:bottom-margin-inner` row with a bottom
  margin. The start overhang (the tick label next to the main panel) goes in the margin's `-axis-start` track between
  the margin and its gap, the end overhang in its `-overhang` track on the margin's far side, both in the axis's
  column or row (see [[plot-margin-tracks]]). A facet panel's margin takes the tracks of its panel column or row
  (`[id i]`, see [[panel-margin-tracks]]) and, where that dimension's panel axes stand at every panel (free axes), the
  panel's own `[:y-axis c]` column or `[:x-axis r]` row.

  Parameters:

  - `side` (keyword): the margin's side.
  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `position` (map, optional): the facet panel whose margin it is - `:row`, `:col`, and `:free-h`, `:free-v` (an
    axis at every panel for the horizontal / vertical scale); without it a single panel's margin.

  Returns `{:axis place :start place :end place}`, each place `{:column :row}` (track ids)."
  ([side margins] (margin-axis-places side margins nil))
  ([side margins {:keys [row col free-h free-v] :as position}]
   (let [indexed (fn [id i] (if position [id i] id))
         across (#{:top :bottom} side)
         along (fn [part] (indexed (margin-track-id side part) (if across row col)))
         cross (if across
                 (cond (:left margins) (indexed (margin-track-id :left "-inner") col) free-v [:y-axis col] :else :y-axis)
                 (cond (:bottom margins) (indexed (margin-track-id :bottom "-inner") row) free-h [:x-axis row] :else :x-axis))
         place (fn [part] (if across {:column cross :row (along part)} {:column (along part) :row cross}))]
     {:axis (place "") :start (place "-axis-start") :end (place "-overhang")})))

(defn- plot-margin-tracks
  "Gives the tracks of a plot margin on one side.

  A margin panel has a fixed size and a gap (theme `[:facet :gap]`) next to the main panel; with an axis, a measured `-axis-start` track between the margin and its gap holds the room the magnitude axis's start tick label (next to the main panel) sticks out, and a measured track on its far side the room its end tick label sticks out. A left or bottom margin also has a measured inner track between its gap and the main panel (`:left-margin-inner`, `:bottom-margin-inner`) - as `:x-overhang` / `:y-overhang` for the right and top margins: it holds the main axis's start overhang and the top / bottom (right / left) margins' magnitude axes (see [[margin-axis-places]]), so those labels don't run into the margin's.

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `side` (keyword): `:top` (rows above the panel), `:bottom` (rows below it), `:right` (columns right of it) or `:left` (columns left of it).
  - `theme` (map): the merged theme.

  Returns the tracks in grid order (top down, left to right); empty without that margin."
  [margins side theme]
  (if-let [{:keys [size axis]} (get margins side)]
    (let [measured (fn [part] {:id (margin-track-id side part) :size [:measured]})
          gap {:id (margin-track-id side "-gap") :size [:fixed (get-in theme [:facet :gap])]}
          track {:id (margin-track-id side "") :size [:fixed size]}
          overhang (when axis (measured "-overhang"))
          start (when axis (measured "-axis-start"))
          inner (measured "-inner")]
      (remove nil? (case side
                     :top [overhang track start gap]
                     :left [overhang track start gap inner]
                     :right [gap start track overhang]
                     :bottom [inner gap start track overhang])))
    []))

(defn- margin-axis-items
  "Gives the grid items of a plot margin's magnitude axis: the axis `{:kind :axis :scale :y :margin side}` and the
  overhang items of its start and end tick labels.

  Parameters:

  - `side` (keyword): the margin's side.
  - `places` (map): the items' places, see [[margin-axis-places]].
  - `id-of` (function): a part (`\"-axis\"`, `\"-axis-start\"`, `\"-axis-end\"`) -> the item's id.

  Returns a vector of the three items."
  [side places id-of]
  [(merge {:id (id-of "-axis") :content {:kind :axis :scale :y :margin side}} (:axis places))
   (merge {:id (id-of "-axis-start") :content {:kind :axis-overhang :scale :y :end :start :margin side}} (:start places))
   (merge {:id (id-of "-axis-end") :content {:kind :axis-overhang :scale :y :end :end :margin side}} (:end places))])

(defn- plot-margin-items
  "Gives the grid items of the plot margins.

  Each margin gets a panel `{:kind :panel :margin side}` beside the main panel and, with `:axis`, its magnitude axis `{:kind :axis :scale :y :margin side}` (the top and bottom margins' in the y-axis column or next to the panel past a left margin, the right and left margins' in the x-axis row or next to the panel past a bottom margin) plus overhang items for its start and end tick labels (see [[margin-axis-places]]).

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.

  Returns a lazy sequence of items; empty without margins."
  [margins]
  (mapcat (fn [[side {:keys [axis]}]]
            (let [panel-id (margin-track-id side "")
                  places (margin-axis-places side margins)
                  [column row] (if (#{:top :bottom} side) [:panel panel-id] [panel-id :panel])]
              (cond-> [{:id panel-id :column column :row row :content {:kind :panel :margin side}}]
                axis (into (margin-axis-items side places (fn [part] (margin-track-id side part)))))))
          margins))

(defn invalid-bands
  "Gives the invalid-value bands of a plot by the direction they lie across.

  A chart showing its invalid values gives stats `:invalid-bands {dim {:label text :data ...}}` (see
  [[cljplot2.content.invalid-band]]); of several charts with a band of one dimension the first one's label is used.

  Parameters:

  - `stats` (sequence): the charts' computed stats.
  - `flip?`: the plot's `:flip`.

  Returns `{direction {:dim k :label text}}` - `:horizontal` for a band of the horizontal dimension (a column left of
  the panels), `:vertical` for one of the vertical dimension (a row under them); empty without such charts."
  [stats flip?]
  (let [horizontal (scale/horizontal-key flip?)]
    (reduce (fn [acc [dim {:keys [label]}]]
              (let [direction (if (= dim horizontal) :horizontal :vertical)]
                (cond-> acc (not (contains? acc direction)) (assoc direction {:dim dim :label label}))))
            {}
            (mapcat :invalid-bands stats))))

(defn- invalid-band-tracks
  "Gives the tracks of an invalid-value band beside a panel column or row.

  The band (measured: its content's natural thickness) lies outside the axis line, next to the axis - a horizontal
  dimension's band right of the `:y-axis` column, a vertical one's above the `:x-axis` row - then a gap (theme
  `[:facet :gap]`) and a measured inner track next to the panel holding the panel axis's start overhang (its first
  tick label), so it doesn't run into the band's label.

  Parameters:

  - `bands` (map): the plot's bands by direction (see [[invalid-bands]]).
  - `direction` (keyword): `:horizontal` (columns) or `:vertical` (rows).
  - `theme` (map): the merged theme.
  - `i` (long or `nil`): the facet panel column or row; `nil` - a single panel.

  Returns the tracks in grid order - `:invalid-band`, `:invalid-band-gap`, `:invalid-band-inner` (`[id i]` with
  `i`); empty without a band in that direction."
  [bands direction theme i]
  (if (get bands direction)
    (let [id (fn [base] (if (some? i) [base i] base))
          band {:id (id :invalid-band) :size [:measured]}
          gap {:id (id :invalid-band-gap) :size [:fixed (get-in theme [:facet :gap])]}
          inner {:id (id :invalid-band-inner) :size [:measured]}]
      (if (= :horizontal direction) [band gap inner] [inner gap band]))
    []))

(defn- invalid-band-content
  "Gives the content of an invalid-value band or of its axis.

  Parameters:

  - `kind` (keyword): `:invalid-band` or `:invalid-band-axis`.
  - `band` (map): the band - `:dim`, `:label`.
  - `direction` (keyword): `:horizontal` or `:vertical`.
  - `panel` (map or `nil`): the facet panel `{:key}`; `nil` - a single panel.

  Returns the content map (see [[cljplot2.content.invalid-band]])."
  [kind {:keys [dim label]} direction panel]
  (cond-> {:kind kind :dim dim :direction direction :label label}
    (some? panel) (assoc :panel (:key panel))))

(defn- invalid-band-items
  "Gives the grid items of the invalid-value bands of a single-panel plot.

  Each band gets its panel in its track beside the panel and, when the dimension's axis is drawn, its axis in the
  main axis's track (`:x-axis` row under a horizontal band, `:y-axis` column beside a vertical one).

  Parameters:

  - `bands` (map): the plot's bands by direction (see [[invalid-bands]]).
  - `scale-specs` (map): the plot's scale specs.

  Returns a lazy sequence of items; empty without bands."
  [bands scale-specs]
  (mapcat (fn [[direction {:keys [dim] :as band}]]
            (let [horizontal? (= :horizontal direction)]
              (cond-> [{:id [:invalid-band dim] :column (if horizontal? :invalid-band :panel) :row (if horizontal? :panel :invalid-band)
                        :content (invalid-band-content :invalid-band band direction nil)}]
                (axis-shown? scale-specs dim)
                (conj {:id [:invalid-band-axis dim] :column (if horizontal? :invalid-band :y-axis) :row (if horizontal? :x-axis :invalid-band)
                       :content (invalid-band-content :invalid-band-axis band direction nil)}))))
          bands))

(defn- axis-panels
  "Gives the facet panels drawing the shared axes of a facet layout.

  The horizontal scale's axis of a panel column is drawn with its bottom panel, the vertical scale's axis of a panel
  row with its first panel (where an axis stands at every panel, see [[axis-panel?]]).

  Parameters:

  - `panels` (sequence): panel maps `{:row :col :key}`.

  Returns `{:bottom {col panel} :first {row panel}}`."
  [panels]
  {:bottom (update-vals (group-by :col panels) #(apply max-key :row %))
   :first (update-vals (group-by :row panels) #(apply min-key :col %))})

(defn- axis-panel?
  "Tests whether a facet panel draws an axis of the horizontal or the vertical scale.

  Parameters:

  - `carriers` (map): the panels drawing the shared axes, see [[axis-panels]].
  - `horizontal`: truthy - the horizontal scale's axis, else the vertical scale's.
  - `free`: truthy - that scale has an axis at every panel (a free or sliced relation in a wrap layout).
  - `panel` (map): the panel `{:row :col :key}`.

  Returns truthy when `free` or the panel is its column's bottom panel (horizontal) / its row's first panel
  (vertical), else `nil` or false."
  [carriers horizontal free {:keys [row col] :as panel}]
  (or free (= panel (if horizontal (get-in carriers [:bottom col]) (get-in carriers [:first row])))))

(defn- facet-invalid-band-items
  "Gives the grid items of the invalid-value bands of a faceted plot.

  Every panel gets its band in the band track of its panel column (horizontal) or row (vertical), shaded as its panel
  (`:alternate`). Where the panels' axis of the band's dimension is drawn (see [[axis-panel?]]) the band gets its
  axis; the panel axis's start overhang goes in the `:invalid-band-inner` track (see [[facet-axis-items]]).

  Parameters:

  - `panels` (sequence): panel maps `{:row :col :key}`.
  - `bands` (map): the plot's bands by direction (see [[invalid-bands]]).
  - `scale-specs` (map): the plot's scale specs.
  - `axes` (map): the panels' axis settings - `:carriers` (see [[axis-panels]]), `:free-h`, `:free-v` (an axis at
    every panel for the horizontal / vertical scale).

  Returns a lazy sequence of items; empty without bands."
  [panels bands scale-specs {:keys [carriers free-h free-v]}]
  (for [{:keys [row col] :as panel} panels
        [direction {:keys [dim] :as band}] bands
        :let [horizontal (= :horizontal direction)
              axis-here (and (axis-shown? scale-specs dim) (axis-panel? carriers horizontal (if horizontal free-h free-v) panel))]
        item (cond-> [{:id [:invalid-band dim row col]
                       :column (if horizontal [:invalid-band col] [:panel col]) :row (if horizontal [:panel row] [:invalid-band row])
                       :content (assoc (invalid-band-content :invalid-band band direction panel)
                                       :alternate (m/odd? (m/long-add (long row) (long col))))}]
               axis-here (conj {:id [:invalid-band-axis dim row col]
                                :column (if horizontal [:invalid-band col] (if free-v [:y-axis col] :y-axis))
                                :row (if horizontal (if free-h [:x-axis row] :x-axis) [:invalid-band row])
                                :content (invalid-band-content :invalid-band-axis band direction panel)}))]
    item))

(defn- single-panel-grid
  "Gives the grid of a single-panel plot.

  The grid holds the labels, the axes of the drawn position scales, the legends, the plot margins (a `:top-margin` row above the panel, a `:bottom-margin` row below it before the x axis, a `:left-margin` column left of it after the y axis, a `:right-margin` column right of it before the legend, each next to a gap), the charts' invalid-value bands (a column right of the `:y-axis` column, a row above the `:x-axis` row, see [[invalid-band-tracks]]) and the theme margin.

  Parameters:

  - `plot` (map): the plot built up to its scale specs - `:labels`, `:theme` (merged), `:scale-specs`, `:flip`, `:margin-stats` (see [[cljplot2.pipeline]]), `:stats` (invalid-value bands, see [[invalid-bands]]), `:alternate-panel` (true shades the panel as an odd facet panel).

  Returns the grid map `{:columns :rows :items}` (see [[cljplot2.layout.grid]])."
  [{:keys [labels theme scale-specs flip margin-stats] :as plot}]
  (let [margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        legends (legend/legends plot)
        bands (invalid-bands (:stats plot) flip)]
    {:columns (-> [(margin-track :margin-left) (measured :y-title) (measured :y-axis)]
                  (into (invalid-band-tracks bands :horizontal theme nil))
                  (into (plot-margin-tracks margin-stats :left theme))
                  (into [{:id :panel :size [:flexible]} (measured :x-overhang)])
                  (into (plot-margin-tracks margin-stats :right theme))
                  (into (legend-tracks legends theme))
                  (conj (margin-track :margin-right)))
     :rows (-> [(margin-track :margin-top) (measured :title)]
               (into (plot-margin-tracks margin-stats :top theme))
               (into [(measured :y-overhang) {:id :panel :size [:flexible]}])
               (into (plot-margin-tracks margin-stats :bottom theme))
               (into (invalid-band-tracks bands :vertical theme nil))
               (into [(measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]))
     :items (-> [{:id :panel :column :panel :row :panel :content (cond-> {:kind :panel} (:alternate-panel plot) (assoc :alternate true))}]
                (into (mapcat (fn [[side k]] (when (axis-shown? scale-specs k) (axis-items side k margin-stats bands)))) (side-scales flip))
                (into (label-items (side-labels labels flip) {}))
                (into (legend-items legends :panel))
                (into (plot-margin-items margin-stats))
                (into (invalid-band-items bands scale-specs)))}))

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

  The plot's `:width` / `:height` lose the room the grid gives the plot margins, the title and axis titles, the axes (stand-in two-digit tick labels, no end overhangs), the plot margin panels (one per panel column / row), the legends and the facet strips and gaps, and the rest is split among the panels; a wrap layout with free or sliced scales also loses an axis per panel row or column. It is an estimate: real tick labels and wrapped legends differ, and invalid-value band tracks (charts' `:invalid :show`, known only from the stats) aren't counted.

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
                                    (natural (merge {:kind :label :role role} (if (label/label-options? l) (dissoc l :rotation) {:text l})))))
           {:keys [x y title]} (side-labels labels flip)
           margin (double (get-in theme [:plot :margin]))
           tick-w (size-width (natural (axis/tick-label theme :y estimate-tick-label)))
           tick-h (size-height (natural (axis/tick-label theme :x estimate-tick-label)))
           gap (double (get-in theme [:facet :gap]))
           strip (if facets? (size-height (natural {:kind :strip :text "Xg"})) 0.0)
           ;; a faceted plot's margins repeat at every panel column (left, right) / row (top, bottom)
           margin-room (fn ^double [side] (if-let [{:keys [size]} (get margins side)]
                                            (m/* (m/+ (double size) gap) (if (#{:top :bottom} side) rows columns))
                                            0.0))
           legend-room (if (seq legends)
                         (m/+ (double (get-in theme [:legend :gap])) (size-width (natural {:kind :legends :legends legends})))
                         0.0)
           horizontal (scale/horizontal-key flip)
           free? (fn [k] (and (not grid?) (not= :same (facet/scale-relation facets k))))
           inner-y-axes (if (free? (if (= horizontal :x) :y :x)) (m/* (m/long-dec columns) (axis-room theme :y tick-w)) 0.0)
           inner-x-axes (if (free? horizontal) (m/* (m/long-dec rows) (axis-room theme :x tick-h)) 0.0)
           w (m/- (double (:width options)) (m/* 2.0 margin) (size-height (text-size :axis-title y))
                  (axis-room theme :y tick-w) (double (margin-room :right)) (double (margin-room :left)) legend-room
                  (m/* gap (m/long-dec columns)) (if grid? strip 0.0) inner-y-axes)
           h (m/- (double (:height options)) (m/* 2.0 margin) (size-height (text-size :title title))
                  (double (margin-room :top)) (double (margin-room :bottom)) (axis-room theme :x tick-h)
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

  The horizontal scale's axes go under the panels, the vertical's left of them, each with the `:panel` key whose scales it draws (see [[cljplot2.scale.position/panel-scales-of]]). A free or sliced relation in a wrap layout puts an axis at every panel, in its own `[:x-axis r]` row or `[:y-axis c]` column; otherwise each panel column gets an axis in the `:x-axis` row drawn with its bottom panel, and each panel row an axis in the `:y-axis` column drawn with its first panel (see [[axis-panels]]). An axis with another panel's axis next to it gets `:next-panel-gap` (labels crossing the next axis's labels are skipped, see [[cljplot2.content.axis]]); with panel-local scales also `:next-panel` - the next panel's key - and vertical axes get both towards the axis below unless the panel rows are stacked apart.

  Overhang items reserve room for end labels as in the single-panel grid. Where the panel columns (rows) have inner tracks next to the panels - left and right (bottom and top) plot margins, an invalid-value band of the horizontal (vertical) dimension - every axis puts its start and end overhangs in its own column's (row's) inner tracks, `[base c]` (`[base r]`), so its end labels keep clear of the margins' and bands' labels; otherwise the horizontal start overhang sits in the first y-axis column, the end one in `:x-overhang`, the vertical start overhang in the `:x-axis` row and the end one in `:y-overhang`, at the outer panels.

  Parameters:

  - `panels` (sequence): panel maps `{:row :col :key}`.
  - `cols` (long): the number of panel columns.
  - `gap`: the facet gap in pixels.
  - `settings` (map): `:h-key`, `:v-key` - the horizontal and vertical scales; `:h-shown`, `:v-shown` - their axes drawn; `:carriers` - the panels drawing the shared axes (see [[axis-panels]]); `:free-h`, `:free-v` - an axis at every panel; `:local-h`, `:local-v` - panel-local scales (neighbouring axes may differ); `:stacked` - strips, axis rows or plot margins separate the panel rows; `:h-gap` - pixels between neighbouring horizontal axes (default `gap`; plot margins beside the panels add theirs); `:x-start-base`, `:x-end-base`, `:y-start-base`, `:y-end-base` - the base id of the panel columns' (rows') inner tracks taking the horizontal (vertical) start and end overhangs (e.g. `:left-margin-inner`; `nil` - none).

  Returns a sequence of grid items."
  [panels cols gap {:keys [h-key v-key h-shown v-shown carriers free-h free-v local-h local-v stacked h-gap
                           x-start-base x-end-base y-start-base y-end-base]}]
  ;; panels are passed around as maps (`nil` - no panel): a panel's `:key` may itself be `nil` (the NA facet level)
  (let [at (into {} (map (juxt (juxt :row :col) identity)) panels)
        h-axis (fn [content next-panel]
                 (cond-> content
                   next-panel (assoc :next-panel-gap (or h-gap gap))
                   (and local-h next-panel) (assoc :next-panel (:key next-panel))))
        v-axis (fn [content next-panel]
                 (cond-> content
                   (and local-v (not stacked) next-panel) (assoc :next-panel-gap gap :next-panel (:key next-panel))))
        bottom-of (:bottom carriers)
        first-of (:first carriers)
        rows (sort (distinct (map :row panels)))
        last-row (last rows)
        next-row (fn [r] (second (drop-while #(not= r %) rows)))
        with-panel (fn [item panel] (cond-> item panel (assoc-in [:content :panel] (:key panel))))
        [bottom-start x-end] (rest (axis-items :bottom h-key nil))
        x-start (cond-> bottom-start free-v (assoc :column [:y-axis 0]))
        [y-start y-end] (rest (axis-items :left v-key nil))
        ;; an overhang item per axis panel in its column's (row's) inner track
        inner-items (fn [item base drawing-panels place]
                      (for [{:keys [row col] :as panel} drawing-panels]
                        (with-panel (merge item {:id [(:id item) row col]} (place base panel)) panel)))
        h-place (fn [base {:keys [row col]}] {:column [base col] :row (if free-h [:x-axis row] :x-axis)})
        v-place (fn [base {:keys [row col]}] {:column (if free-v [:y-axis col] :y-axis) :row [base row]})]
    (concat
     (when h-shown
       (let [in-rows (if free-h
                       (map (fn [r] (sort-by :col (filter #(= r (:row %)) panels))) rows)
                       [(keep bottom-of (range cols))])]
         (concat
          (if free-h
            (for [{:keys [row col] :as panel} panels]
              (with-panel {:id [:x-axis row col] :column [:panel col] :row [:x-axis row]
                           :content (h-axis {:kind :axis :scale h-key} (at [row (m/long-inc (long col))]))}
                panel))
            (for [c (range cols)]
              (with-panel {:id [:x-axis c] :column [:panel c] :row :x-axis
                           :content (h-axis {:kind :axis :scale h-key}
                                            (when (m/< (long c) (m/long-dec (long cols))) (bottom-of (m/long-inc (long c)))))}
                (bottom-of c))))
          (mapcat (fn [in-row]
                    (let [r (:row (first in-row))
                          outer (fn [item panel] (-> (cond-> item free-h (assoc :id [(:id item) r] :row [:x-axis r]))
                                                     (with-panel panel)))]
                      (concat (if x-start-base (inner-items x-start x-start-base in-row h-place) [(outer x-start (first in-row))])
                              (if x-end-base (inner-items x-end x-end-base in-row h-place) [(outer x-end (last in-row))]))))
                  in-rows))))
     (when v-shown
       (let [v-panels (if free-v panels (keep first-of rows))]
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
          (if y-start-base (inner-items y-start y-start-base v-panels v-place) [(with-panel y-start (at [last-row 0]))])
          (if y-end-base (inner-items y-end y-end-base v-panels v-place) [(with-panel y-end (at [(first rows) 0]))])))))))

(defn- panel-margin-tracks
  "Gives the tracks of a facet panel column's or row's plot margin on one side.

  A right or top margin also gets a measured inner track next to the panel, `[:right-margin-inner i]` /
  `[:top-margin-inner i]` (the panel column's / row's `:x-overhang` / `:y-overhang`), holding its panel axes' end
  overhangs (see [[facet-axis-items]]).

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `side` (keyword): the margin's side (see [[plot-margin-tracks]]).
  - `theme` (map): the merged theme.
  - `i` (long): the panel column (`:left`, `:right`) or row (`:top`, `:bottom`).

  Returns the tracks of [[plot-margin-tracks]] (and the inner track) with ids `[id i]` (e.g. `[:top-margin 0]`);
  empty without that margin."
  [margins side theme i]
  (let [tracks (plot-margin-tracks margins side theme)
        inner {:id (margin-track-id side "-inner") :size [:measured]}]
    (map #(update % :id vector i)
         (cond
           (empty? tracks) tracks
           (= :right side) (cons inner tracks)
           (= :top side) (concat tracks [inner])
           :else tracks))))

(defn- margins-room
  "Gives the fixed pixels plot margins put between neighbouring facet panels.

  Parameters:

  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `sides` (sequence): the margin sides between the panels.
  - `gap` (double): the facet gap (each margin's gap to its panel).

  Returns the margins' sizes and gaps summed, a double (their measured inner and overhang tracks not counted)."
  ^double [margins sides ^double gap]
  (reduce m/+ 0.0 (keep (fn [side] (when-let [{:keys [size]} (get margins side)] (m/+ (double size) gap))) sides)))

(defn- facet-margin-items
  "Gives the grid items of the plot margins of a faceted plot.

  Every panel gets a margin panel per side, `{:kind :panel :margin side :panel k}` (shaded as its panel), in the margin track of its panel row (`:top`, `:bottom`) or column (`:right`, `:left`) - so the margins of a panel row or column line up. A margin with `:axis` draws its magnitude axis where the panels' axes of the other dimension are (see [[axis-panel?]]): the top and bottom margins' at every panel with free vertical axes, else at the first panel of each row; the right and left margins' at every panel with free horizontal axes, else at the bottom panel of each column - each with overhang items for its start and end tick labels, placed as a single panel's in the tracks of the panel's column and row (see [[margin-axis-places]]).

  Parameters:

  - `panels` (sequence): panel maps `{:row :col :key}`.
  - `margins` (map): the plot's margin stats, side -> `{:size :axis}`.
  - `axes` (map): the panels' axis settings - `:carriers` (see [[axis-panels]]), `:free-h`, `:free-v` (an axis at
    every panel for the horizontal / vertical scale).

  Returns a lazy sequence of items; empty without margins."
  [panels margins {:keys [carriers free-h free-v]}]
  (for [{panel-key :key :keys [row col] :as panel} panels
        [side {:keys [axis]}] margins
        :let [across (#{:top :bottom} side)
              track [(margin-track-id side "") (if across row col)]
              ;; the margin's axis stands where the panels' axes of the other dimension are
              axis-here (and axis (if across (axis-panel? carriers false free-v panel) (axis-panel? carriers true free-h panel)))]
        item (cond-> [{:id [(margin-track-id side "") row col]
                       :column (if across [:panel col] track) :row (if across track [:panel row])
                       :content {:kind :panel :margin side :panel panel-key
                                 :alternate (m/odd? (m/long-add (long row) (long col)))}}]
               axis-here (into (margin-axis-items side (margin-axis-places side margins {:row row :col col :free-h free-h :free-v free-v})
                                                  (fn [part] [(margin-track-id side part) row col]))))]
    item))

(defn- facet-grid
  "Gives the grid of a faceted plot (see [[cljplot2.data.facet]]).

  Panels `[:panel r c]` are laid out in rows and columns with gaps (theme `[:facet :gap]`), weighted by `:facets` `:widths` / `:heights`; strips go above each panel (wrap) or as column strips above the top row and row strips on the right (grid), none with `:facets {:strips false}`. An x axis goes under every panel column and a y axis beside every panel row, overhangs at the outer ends; a wrap layout with a free or sliced relation has an axis at every panel instead (see [[facet-axis-items]]). Panels with odd `r + c` are shaded (`:alternate`). Legends as in the single-panel grid.

  Plot `:margins` add their tracks (see [[plot-margin-tracks]]) around every panel column (`:left`, `:right` - `[:left-margin c]` ...) and panel row (`:top`, `:bottom` - `[:top-margin r]` ...): a top margin between the panel's wrap strip and the panel, a bottom one between the panel and its free x-axis row, a left one between the panel's free y-axis column and the panel; the main axes stay outside the margins as in the single-panel grid, every panel axis's start and end overhangs go in the margins' inner tracks of its column / row (see [[facet-axis-items]], [[panel-margin-tracks]]). Margin items: see [[facet-margin-items]].

  Charts' invalid-value bands add their tracks (see [[invalid-band-tracks]]) to every panel column (a horizontal band, `[:invalid-band c]` after the column's free y-axis column) or row (a vertical one, `[:invalid-band r]` before the row's free x-axis row) - one band per panel, see [[facet-invalid-band-items]]; neighbouring horizontal axes are then at least the theme band size and a gap further apart.

  Parameters:

  - `plot` (map): the plot built up to its scale specs - `:labels`, `:theme` (merged), `:scale-specs`, `:scales`, `:facets`, `:split`, `:flip`, `:margin-stats` (see [[cljplot2.pipeline]]), `:stats` (invalid-value bands, see [[invalid-bands]]).

  Returns the grid map `{:columns :rows :items}` (see [[cljplot2.layout.grid]]).

  Throws `ex-info {:key :widths}` / `{:key :heights}` for a weight count other than the panel columns / rows."
  [{:keys [labels theme scale-specs scales facets split flip margin-stats] :as plot}]
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
        bands (invalid-bands (:stats plot) flip)
        axes {:carriers (axis-panels panels) :free-h free-h :free-v free-v}
        column-tracks (butlast (mapcat (fn [c] (cond-> []
                                                 free-v (conj (measured [:y-axis c]))
                                                 true (into (invalid-band-tracks bands :horizontal theme c))
                                                 true (into (panel-margin-tracks margin-stats :left theme c))
                                                 true (conj (panel-track widths c))
                                                 true (into (panel-margin-tracks margin-stats :right theme c))
                                                 true (conj (gap-track :gap c))))
                                       (range cols)))
        row-tracks (butlast (mapcat (fn [r] (cond-> []
                                              wrap-strips? (conj (measured [:strip r]))
                                              true (into (panel-margin-tracks margin-stats :top theme r))
                                              true (conj (panel-track heights r))
                                              true (into (panel-margin-tracks margin-stats :bottom theme r))
                                              true (into (invalid-band-tracks bands :vertical theme r))
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
                (into (facet-axis-items panels cols gap {:h-key h-key :v-key v-key :carriers (:carriers axes) :free-h free-h :free-v free-v
                                                         :local-h (or (not= :same (facet/scale-relation facets h-key)) (some? widths))
                                                         :local-v (or (not= :same (facet/scale-relation facets v-key)) (some? heights))
                                                         :stacked (or wrap-strips? free-h (:top margin-stats) (:bottom margin-stats) (:vertical bands))
                                                         ;; nil - the plain facet gap (nothing between the panels)
                                                         :h-gap (when (or (:left margin-stats) (:right margin-stats) (:horizontal bands))
                                                                  (m/+ (m/+ (double gap) (margins-room margin-stats [:left :right] gap))
                                                                     ;; an invalid-value band: at least its theme size and gap
                                                                     (if (:horizontal bands) (m/+ (double gap) (double (get-in theme [:panel :invalid-band-size]))) 0.0)))
                                                         ;; inner tracks next to the panels, as the single panel's (see `axis-items`)
                                                         :x-start-base (cond (:left margin-stats) (margin-track-id :left "-inner")
                                                                             (:horizontal bands) :invalid-band-inner)
                                                         :x-end-base (when (:right margin-stats) (margin-track-id :right "-inner"))
                                                         :y-start-base (cond (:bottom margin-stats) (margin-track-id :bottom "-inner")
                                                                             (:vertical bands) :invalid-band-inner)
                                                         :y-end-base (when (:top margin-stats) (margin-track-id :top "-inner"))
                                                         :h-shown (axis-shown? scale-specs h-key)
                                                         :v-shown (axis-shown? scale-specs v-key)}))
                (into (facet-margin-items panels margin-stats axes))
                (into (facet-invalid-band-items panels bands scale-specs axes))
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

(defn arrange-cells
  "Gives the grid cell of every plot of an arrangement.

  Plots fill the cells row by row, left to right; a plot with a row span (`:row-spans {index rows}`) also covers the cells below its own, which later plots skip (as an HTML table's `rowspan`). Without row spans plot `k` is in row `k / columns`, column `k mod columns`.

  Parameters:

  - `arrange` (map): `:plots`, `:columns`, `:rows`, optional `:row-spans` - plot index -> the rows it spans (at least 1).

  Returns `{:rows r :columns c :cells [{:row i :column j :span n} ...]}` - the shape and a cell per plot in plot order. The columns come from [[arrange-shape]] over the number of cells the plots cover; the rows are the given `:rows`, else the rows the placed plots reach (a row span starting in the last row adds rows). With `:rows` given, a plot that doesn't fit gets `:row` at or past `r` (or a span past it)."
  [{:keys [plots columns rows row-spans]}]
  (let [n (count plots)
        span-of (fn ^long [^long k] (long (get row-spans k 1)))
        covered (reduce m/long-add 0 (map span-of (range n)))
        [^long r ^long c] (arrange-shape covered columns rows)]
    (loop [k 0
           slot 0
           occupied #{}
           cells []]
      (cond
        (m/>= k n) {:rows (if rows r (reduce (fn [^long acc {:keys [^long row ^long span]}] (m/max acc (m/long-add row span))) r cells))
                    :columns c :cells cells}
        (contains? occupied slot) (recur k (m/long-inc slot) occupied cells)
        :else (let [i (m/long-quot slot c)
                    j (m/long-rem slot c)
                    span (span-of k)]
                (recur (m/long-inc k) (m/long-inc slot)
                       (into occupied (map (fn [^long t] (m/long-add slot (m/* t c)))) (range 1 span))
                       (conj cells {:row i :column j :span span})))))))

(defn legend-cell?
  "Tests whether an arranged plot draws only legends: a `:legend-only` plot, or a one-column `:arrange` of such plots (a stack of legends, e.g. the legend column of [[cljplot2.graphs/clustered-heatmap]] with annotations).

  Parameters:

  - `p` (map): an arranged plot.

  Returns truthy for a legend cell, else `nil` / `false`."
  [p]
  (or (:legend-only p)
      (when-let [{nested :plots nested-columns :columns} (:arrange p)]
        (and (contains? #{nil 1} nested-columns) (every? legend-cell? nested)))))

(defn- arrange-problem
  "Gives why an `:arrange` spec (valid entries) can't be laid out, or `nil`.

  Parameters:

  - `arrange` (map): `:plots`, `:columns`, `:rows`, `:widths`, `:heights`, `:row-spans`.

  Returns a message: too few cells, a row span past the last row or of a plot that doesn't exist, `:widths` / `:heights` not one per column / row, a `:measured` width over a column with a plot that isn't a legend cell (see [[legend-cell?]]), or a `:measured` height over a row with a plot that isn't `:legend-only`."
  [{:keys [plots widths heights row-spans] :as arrange}]
  (let [cells-of (vec plots)
        n (count cells-of)
        {:keys [^long rows ^long columns cells]} (arrange-cells arrange)
        last-cell (peek cells)]
    (cond
      (some (fn [[k _]] (m/>= (long k) n)) row-spans) (str ":row-spans names a plot index past the last plot (" (m/long-dec n) ")")
      (and last-cell (m/>= (long (:row last-cell)) rows)) (str "layout " rows "x" columns " is too small for " n " plots")
      (some (fn [{:keys [^long row ^long span]}] (m/> (m/long-add row span) rows)) cells)
      (str "a :row-spans entry reaches past the last row (" rows " rows)")
      (and widths (m/not== columns (count widths))) (str ":widths should have one weight per column (" columns ")")
      (some (fn [j] (and (= :measured (nth widths j))
                         (not-every? legend-cell? (keep-indexed (fn [k cell] (when (m/== (long j) (long (:column cell))) (cells-of k))) cells))))
            (range (count widths)))
      ":widths :measured sizes a column of legend cells only (:legend-only plots or one-column arranges of them)"
      (and heights (m/not== rows (count heights))) (str ":heights should have one weight per row (" rows ")")
      (some (fn [i] (and (= :measured (nth heights i))
                         (not-every? :legend-only (keep-indexed (fn [k cell] (when (m/== (long i) (long (:row cell))) (cells-of k))) cells))))
            (range (count heights)))
      ":heights :measured sizes a row of :legend-only plots only")))

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
    (schema/nilable :heights {:description "relative row weights; :measured sizes a row of legend-only plots to their legends (scaled down to fit), [:fixed px] a fixed height"
                              :cljplot2/default-doc "equal"}
                    [:sequential [:or schema/pos [:= :measured] [:tuple [:= :fixed] schema/non-neg]]])
    (schema/nilable :row-spans {:description "plot index (not cell) -> the rows that plot spans from its own row down (later plots skip the covered cells; rows are added when not given)"
                                :cljplot2/default-doc "every plot one row"}
                    [:map-of [:int {:min 0}] schema/pos-int])
    (schema/nilable :align {:description "align the sub-plots' panels in rows and columns" :default true} schema/bool)
    (schema/nilable :equal-panels {:description "resize columns and rows so every aligned panel has the same size (builds twice)"
                                   :default false}
                    schema/bool)]))

(def arrange-schema
  "Plot `:arrange`, see [[cljplot2.multiplot/arrange]]; the plots are validated when built as sub-plots."
  (schema/with-rules
   (into [:map {:closed true :cljplot2/type-doc "an arrange map {:plots :columns :rows :widths :heights :row-spans :align :equal-panels}"}]
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

  Cells `[:plot i]` are filled row by row (columns `[:column c]`, rows `[:row r]`; a plot with a row span covers that many rows, see [[arrange-cells]]) with theme `[:multiplot :gap]` between them, theme `[:plot :margin]` around, and an optional title row above the cells, inside the margins.

  Parameters:

  - `arrange` (map): the plot's `:arrange` spec `{:plots [p ...] :columns :rows :widths :heights :row-spans}`, valid by [[arrange-schema]], its `:measured` widths and heights resolved to `[:fixed px]` (see the pipeline's grid stage).
  - `labels` (map): the plot labels, only `:title` used.
  - `theme` (map): the merged theme.

  Returns the grid map `{:columns :rows :items}`."
  [{:keys [plots widths heights] :as arrange} labels theme]
  (let [{^long r :rows ^long c :columns cells :cells} (arrange-cells arrange)
        gap (get-in theme [:multiplot :gap])
        margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})]
    {:columns (-> [(margin-track :margin-left)]
                  (into (weighted-tracks :column :column-gap (arrange-weights widths c) gap))
                  (conj (margin-track :margin-right)))
     :rows (-> [(margin-track :margin-top) {:id :title :size [:measured]}]
               (into (weighted-tracks :row :row-gap (arrange-weights heights r) gap))
               (conj (margin-track :margin-bottom)))
     :items (-> (vec (map (fn [i p {:keys [^long row column ^long span]}]
                                    {:id [:plot i] :column [:column column]
                                     :row (if (m/> span 1) {:from [:row row] :to [:row (m/long-dec (m/long-add row span))]} [:row row])
                                     :content {:kind :plot :plot p}})
                                  (range) plots cells))
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
