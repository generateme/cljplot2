(ns cljplot2.plot-grid
  "Grid generated from plot keys (see [[cljplot2.grid]] for the grid shape).

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
  start ends in the corner cell (`:y-axis` column, `:x-axis` row).
  Item order: `:panel`, axes and their overhangs (x, then y), `:title`, `:x-title`, `:y-title` (absent ones skipped).")

(def ^:private plot-labels
  "Plot label key -> grid item id, content defaults and placement, in item order."
  [[:title {:id :title :defaults {:role :title} :column {:from :y-title :to :x-overhang} :row :title}]
   [:x {:id :x-title :defaults {:role :axis-title} :column :panel :row :x-title}]
   [:y {:id :y-title :defaults {:role :axis-title :rotation 270} :column :y-title :row :panel}]])

(def ^:private label-keys (set (map first plot-labels)))

(defn- label-items
  "Grid items of the plot `labels` (fixed order: title, x, y); a `nil` label adds no item."
  [labels]
  (when-not (or (nil? labels) (map? labels))
    (throw (ex-info "Plot :labels must be a map." {:key :labels :value labels})))
  (doseq [k (keys labels)]
    (when-not (label-keys k)
      (throw (ex-info "Unknown plot label." {:key :labels :value k :allowed label-keys}))))
  (vec (keep (fn [[k {:keys [id defaults column row]}]]
               (when-some [label (get labels k)]
                 {:id id :column column :row row
                  :content (assoc (merge defaults (if (map? label) label {:text label})) :kind :label)}))
             plot-labels)))

(def ^:private axis-items
  {:x [{:id :x-axis :column :panel :row :x-axis :content {:kind :axis :scale :x}}
       {:id :x-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale :x :end :start}}
       {:id :x-axis-end :column :x-overhang :row :x-axis :content {:kind :axis-overhang :scale :x :end :end}}]
   :y [{:id :y-axis :column :y-axis :row :panel :content {:kind :axis :scale :y}}
       {:id :y-axis-start :column :y-axis :row :x-axis :content {:kind :axis-overhang :scale :y :end :start}}
       {:id :y-axis-end :column :y-axis :row :y-overhang :content {:kind :axis-overhang :scale :y :end :end}}]})

(defn single-panel-grid
  "Grid of a single-panel plot: its `:labels`, axes of its `:scale-specs` and the margin of its (merged) `:theme`."
  [{:keys [labels theme scale-specs]}]
  (let [margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})
        measured (fn [id] {:id id :size [:measured]})]
    {:columns [(margin-track :margin-left) (measured :y-title) (measured :y-axis)
               {:id :panel :size [:flexible]} (measured :x-overhang) (margin-track :margin-right)]
     :rows [(margin-track :margin-top) (measured :title) (measured :y-overhang) {:id :panel :size [:flexible]}
            (measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]
     :items (-> [{:id :panel :column :panel :row :panel :content {:kind :panel}}]
                (into (mapcat #(when (contains? scale-specs %) (axis-items %))) [:x :y])
                (into (label-items labels)))}))
