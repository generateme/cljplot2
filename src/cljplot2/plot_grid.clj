(ns cljplot2.plot-grid
  "Grid generated from plot keys (see [[cljplot2.grid]] for the grid shape).

  Single panel:

  * columns `:margin-left | :y-title | :y-axis | :panel | :margin-right`
  * rows `:margin-top | :title | :panel | :x-axis | :x-title | :margin-bottom`

  Margins are fixed (theme `[:plot :margin]`), the panel is flexible, every other track is measured.
  Axis tracks are reserved for axes and hold no items yet.

  Plot `:labels` - `{:title t :x t :y t}`, `t` a string or a map of label options (`:text`, style overrides,
  `:align`, `:rotation`) merged into the label content; a missing or `nil` label adds no item.
  `:title` uses the `:title` text role, `:x` and `:y` the `:axis-title` role; `:y` is rotated 270 by default.")

(def ^:private label-defaults
  {:title {:role :title}
   :x {:role :axis-title}
   :y {:role :axis-title :rotation 270}})

(defn- label-content
  [k label]
  (when-not (contains? label-defaults k)
    (throw (ex-info "Unknown plot label." {:key :labels :value k :allowed (set (keys label-defaults))})))
  (when (some? label)
    (merge {:kind :label} (label-defaults k) (if (map? label) label {:text label}))))

(def ^:private label-placement
  "Item placement of each plot label."
  {:title {:column {:from :y-title :to :panel} :row :title}
   :x {:column :panel :row :x-title}
   :y {:column :y-title :row :panel}})

(defn single-panel-grid
  "Grid of a single-panel plot with its `:labels`, margins from `theme`."
  [{:keys [labels]} theme]
  (when-not (or (nil? labels) (map? labels))
    (throw (ex-info "Plot :labels must be a map." {:key :labels :value labels})))
  (let [margin (get-in theme [:plot :margin])
        margin-track (fn [id] {:id id :size [:fixed margin]})
        measured (fn [id] {:id id :size [:measured]})]
    (when-not (and (number? margin) (>= (double margin) 0.0))
      (throw (ex-info "Plot :margin must be a non-negative number." {:key :margin :value margin})))
    {:columns [(margin-track :margin-left) (measured :y-title) (measured :y-axis)
               {:id :panel :size [:flexible]} (margin-track :margin-right)]
     :rows [(margin-track :margin-top) (measured :title) {:id :panel :size [:flexible]}
            (measured :x-axis) (measured :x-title) (margin-track :margin-bottom)]
     :items (into [{:id :panel :column :panel :row :panel :content {:kind :panel}}]
                  (keep (fn [[k label]]
                          (when-let [content (label-content k label)]
                            (merge {:id k :content content} (label-placement k)))))
                  labels)}))
