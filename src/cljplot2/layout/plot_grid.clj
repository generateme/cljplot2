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

  An axis item (`:x-axis` below the panel, `:y-axis` left of it) is added for each scale in `:scale-specs`, with
  overhang items reserving room for its end tick labels: x end in `:x-overhang`, y end in `:y-overhang`, both
  start ends in the corner cell (`:y-axis` column, `:x-axis` row). Track and item names are by side (`x` - under
  the panel, `y` - left of it): with `:flip true` the `:y` scale's axis and `:labels :y` title go under the panel,
  the `:x` scale's axis and `:labels :x` title left of it.
  Item order: `:panel`, axes and their overhangs (x, then y), `:title`, `:x-title`, `:y-title` (absent ones skipped).

  Faceted plots use [[facet-grid]].

  Legends ([[cljplot2.content.legend/legends]]) add columns `:legend-gap` (fixed, theme `[:legend :gap]`) and `:legend`
  (measured) after `:x-overhang`, and an item `:legends` spanning the panel rows (from the top strip) with
  `{:kind :legends}` content: the legends stacked top-down, wrapping into further columns when too tall."
  (:require [cljplot2.data.facet :as facet]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.content.label :as label]
            [cljplot2.content.legend :as legend]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private plot-labels
  "Plot label key -> grid item id, content defaults and placement, in item order."
  [[:title {:id :title :defaults {:role :title} :column {:from :y-title :to :x-overhang} :row :title}]
   [:x {:id :x-title :defaults {:role :axis-title} :column :panel :row :x-title}]
   [:y {:id :y-title :defaults {:role :axis-title :rotation 270} :column :y-title :row :panel}]])

(def Labels
  "Plot `:labels`: `{:title :x :y}`, each a text or a map of [[cljplot2.content.label/Options]]; `nil` - no label."
  (into [:map {:closed true}]
        (map (fn [[k]] (schema/nilable k [:multi {:dispatch map?} [true label/Options] [false any?]])))
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

(defn single-panel-grid
  "Grid of a single-panel plot: its `:labels`, axes of its `:scale-specs` and the margin of its (merged) `:theme`."
  [{:keys [labels theme scale-specs flip] :as plot}]
  (let [margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        legends (legend/legends plot)]
    {:columns (-> [(margin-track :margin-left) (measured :y-title) (measured :y-axis) {:id :panel :size [:flexible]}
                   (measured :x-overhang)]
                  (into (legend-tracks legends theme))
                  (conj (margin-track :margin-right)))
     :rows [(margin-track :margin-top) (measured :title) (measured :y-overhang) {:id :panel :size [:flexible]}
            (measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]
     :items (-> [{:id :panel :column :panel :row :panel :content {:kind :panel}}]
                (into (mapcat (fn [[side k]] (when (contains? scale-specs k) (axis-items side k)))) (side-scales flip))
                (into (label-items (side-labels labels flip) {}))
                (into (legend-items legends :panel)))}))

;; facets

(defn- interleave-gaps
  "Tracks `[t0 gap t1 gap ...]` for `n` flexible tracks `[prefix i]` with fixed gaps `[gap-prefix i]`."
  [n prefix gap-prefix gap]
  (vec (butlast (mapcat (fn [i] [{:id [prefix i] :size [:flexible]} {:id [gap-prefix i] :size [:fixed gap]}]) (range n)))))

(defn- strip-content
  "Strip content for facet `level` with `text`; when every one of `levels` is a shingle interval, also
  `:interval {:lo :hi :min :max}` - the level within the range of all levels (a shaded bar, see
  [[cljplot2.content.strip]])."
  [text level levels]
  (cond-> {:kind :strip :text text}
    (and (seq levels) (every? shingle/interval? levels))
    (assoc :interval {:lo (:lo level) :hi (:hi level)
                      :min (reduce min (map :lo levels)) :max (reduce max (map :hi levels))})))

(defn- facet-panels
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

(defn facet-grid
  "Grid of a faceted plot (see [[cljplot2.data.facet]]): panels `[:panel r c]` in a `rows` x `cols` layout with gaps
  (theme `[:facet :gap]`), strips (wrap: above each panel; grid: column strips above the top row, row strips on
  the right), an x axis under every panel column and a y axis beside every panel row, overhangs at the outer ends.
  Panels with odd `r + c` are shaded (`:alternate`). Legends as in the single-panel grid."
  [{:keys [labels theme scale-specs scales facets split flip] :as plot}]
  (let [legends (legend/legends plot)
        [[_ h-key] [_ v-key]] (side-scales flip)
        gap (get-in theme [:facet :gap])
        margin-track (margin-track-fn plot)
        measured (fn [id] {:id id :size [:measured]})
        {:keys [rows cols wrap? panels col-labels row-labels levels col-levels row-levels]}
        (facet-panels (:facets split) scales facets)
        rows (long rows) cols (long cols)
        last-col [:panel (dec cols)]
        last-row [:panel (dec rows)]
        row-tracks (vec (butlast (mapcat (fn [r] (cond-> [] wrap? (conj (measured [:strip r]))
                                                     true (conj {:id [:panel r] :size [:flexible]} {:id [:gap r] :size [:fixed gap]})))
                                         (range rows))))]
    {:columns (-> [(margin-track :margin-left) (measured :y-title) (measured :y-axis)]
                  (into (interleave-gaps cols :panel :gap gap))
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
                (cond-> (contains? scale-specs h-key)
                  (into (concat (for [c (range cols)]
                                  {:id [:x-axis c] :column [:panel c] :row :x-axis
                                   :content (cond-> {:kind :axis :scale h-key}
                                              (< (long c) (dec cols)) (assoc :next-panel-gap gap))})
                                (rest (axis-items :bottom h-key)))))
                (cond-> (contains? scale-specs v-key)
                  (into (concat (for [r (range rows)]
                                  {:id [:y-axis r] :column :y-axis :row [:panel r] :content {:kind :axis :scale v-key}})
                                (rest (axis-items :left v-key)))))
                (cond-> wrap? (into (map (fn [{:keys [key label row col]}]
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
                (into (legend-items legends {:from (cond col-labels :col-strip wrap? [:strip 0] :else [:panel 0])
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
      (and heights (not= r (count heights))) (str ":heights should have one weight per row (" r ")"))))

(def Arrange
  "Plot `:arrange`, see [[cljplot2.multiplot/arrange]]; the plots are validated when built as sub-plots."
  (schema/with-rules
   [:map {:closed true}
    [:plots [:sequential {:min 1} :map]]
    (schema/nilable :columns schema/pos-int)
    (schema/nilable :rows schema/pos-int)
    (schema/nilable :widths [:sequential schema/pos])
    (schema/nilable :heights [:sequential schema/pos])
    (schema/nilable :align schema/bool)]
   [arrange-problem]))

(defn- arrange-weights
  [ws ^long n]
  (if (nil? ws) (vec (repeat n 1)) (vec ws)))

(defn- weighted-tracks
  "Flexible tracks `[prefix i]` weighted by `ws`, fixed gaps `[gap-prefix i]` between them."
  [prefix gap-prefix ws gap]
  (vec (butlast (mapcat (fn [i w] [{:id [prefix i] :size [:flexible w]} {:id [gap-prefix i] :size [:fixed gap]}])
                        (range) ws))))

(defn arrange-grid
  "Grid of the plot `:arrange` spec `{:plots [p ...] :columns :rows :widths :heights}` in `theme`: cells `[:plot i]`
  filled row by row (columns `[:column c]`, rows `[:row r]`) with `[:multiplot :gap]` between them, `[:plot :margin]`
  around, and an optional title row (`labels` - only `:title`) above the cells, inside the margins.
  `arrange` is valid by [[Arrange]]."
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
