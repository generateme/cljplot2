(ns cljplot2.plot-grid
  "Grid generated from plot keys (see [[cljplot2.grid]] for the grid shape).

  Single panel:

  * columns `:margin-left | :y-title | :y-axis | :panel | :margin-right`
  * rows `:margin-top | :title | :panel | :x-axis | :x-title | :margin-bottom`

  Margins are fixed (theme `[:plot :margin]`), the panel is flexible, every other track is measured.
  Axis tracks are reserved for axes and hold no items yet.

  Plot `:labels` - `{:title t :x t :y t}`, `t` a string or a map of label options (`:text`, style overrides,
  `:align`, `:rotation`) merged into the label content; a missing or `nil` label adds no item.
  `:title` uses the `:title` text role, `:x` and `:y` the `:axis-title` role; `:y` is rotated 270 by default.
  The label `:kind` can't be overridden. Item ids: `:panel`, `:title`, `:x-title`, `:y-title`.")

(def ^:private plot-labels
  "Plot label key -> grid item id, content defaults and placement, in item order."
  [[:title {:id :title :defaults {:role :title} :column {:from :y-title :to :panel} :row :title}]
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

(defn single-panel-grid
  "Grid of a single-panel plot: its `:labels` and the margin of its (merged) `:theme`."
  [{:keys [labels theme]}]
  (let [margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})
        measured (fn [id] {:id id :size [:measured]})]
    {:columns [(margin-track :margin-left) (measured :y-title) (measured :y-axis)
               {:id :panel :size [:flexible]} (margin-track :margin-right)]
     :rows [(margin-track :margin-top) (measured :title) {:id :panel :size [:flexible]}
            (measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]
     :items (into [{:id :panel :column :panel :row :panel :content {:kind :panel}}] (label-items labels))}))
