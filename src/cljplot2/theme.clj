(ns cljplot2.theme
  "Plot theme: a nested map of defaults, deep-merged with the plot's `:theme`.

  `:text` maps a role (`:title`, `:strip`, ...) to a text style:

  * `:font` - font family name or `java.awt.Font`, see [[cljplot2.font/resolve-font]]
  * `:size` - points
  * `:font-style` - `:plain`, `:bold`, `:italic` or `:bold-italic`
  * `:color` - any clojure2d color
  * `:padding` - pixels added on every side of the text
  * `:align` - `[h v]` placement of the text in its box, see [[cljplot2.label]]

  `:plot` - `:background` (any clojure2d color, fills the canvas), `:margin` (pixels on every side).

  `:panel` - `:fill`, `:border-color` (`nil` draws nothing), `:border-width` (pixels, drawn inside the panel box).")

(def style-keys
  "Keys of a text style; content may override them inline."
  [:font :size :font-style :color :padding :align])

(def ^:private centered [:center :middle])

(def default-theme
  "Defaults in old cljplot style: light lavender-gray background, 10 px margin, panels without fill and border."
  {:plot {:background 0xe8e8f0 :margin 10}
   :panel {:fill nil :border-color nil :border-width 1}
   :text {:title {:font "Liberation Mono" :size 14 :font-style :bold :color :black :padding 4 :align centered}
          :strip {:font "Liberation Mono" :size 11 :font-style :plain :color :black :padding 2 :align centered}
          :axis-title {:font "Liberation Mono" :size 12 :font-style :plain :color :black :padding 4 :align centered}
          :tick-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}
          :legend-title {:font "Liberation Mono" :size 11 :font-style :bold :color :black :padding 2 :align centered}
          :legend-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}}})

(defn deep-merge
  "Merge maps recursively; a non-map value replaces the value it's merged over, `nil` is ignored
  (it can't remove a default)."
  [a b]
  (cond
    (nil? b) a
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    :else b))

(defn plot-theme
  "Plot theme: `theme` deep-merged over [[default-theme]] (`nil` means the default theme)."
  [theme]
  (deep-merge default-theme theme))

(defn text-style
  "Text style of `content`: the style of its `:role` in `theme` with inline style keys of `content` merged over.
  Throws `ex-info` with `{:key :role}` when the role has no style map."
  [theme content]
  (let [role (:role content)
        style (get-in theme [:text role])]
    (when-not (map? style)
      (throw (ex-info "Unknown text role." {:key :role :value role})))
    (merge style (select-keys content style-keys))))
