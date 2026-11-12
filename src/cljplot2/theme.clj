(ns cljplot2.theme
  "Plot theme: a nested map of defaults, deep-merged with the plot's `:theme`.

  `:text` maps a role (`:title`, `:strip`, ...) to a text style:

  * `:font` - font family name or `java.awt.Font`, see [[cljplot2.font/resolve-font]]
  * `:size` - points
  * `:font-style` - `:plain`, `:bold`, `:italic` or `:bold-italic`
  * `:color` - any clojure2d color
  * `:padding` - pixels added on every side of the text")

(def style-keys
  "Keys of a text style; content may override them inline."
  [:font :size :font-style :color :padding])

(def default-theme
  {:text {:title {:font "Liberation Mono" :size 14 :font-style :bold :color :black :padding 4}
          :strip {:font "Liberation Mono" :size 11 :font-style :plain :color :black :padding 2}
          :axis-title {:font "Liberation Mono" :size 12 :font-style :plain :color :black :padding 4}
          :tick-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2}
          :legend-title {:font "Liberation Mono" :size 11 :font-style :bold :color :black :padding 2}
          :legend-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2}}})

(defn deep-merge
  "Merge maps recursively; a non-map value replaces the value it's merged over."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn plot-theme
  "Plot theme: `theme` deep-merged over [[default-theme]]; `nil` means the default theme."
  [theme]
  (if (nil? theme) default-theme (deep-merge default-theme theme)))

(defn text-style
  "Text style of `content`: the style of its `:role` in `theme` with inline style keys of `content` merged over.
  Throws `ex-info` with `{:key :role}` when the role has no style map."
  [theme content]
  (let [role (:role content)
        style (get-in theme [:text role])]
    (when-not (map? style)
      (throw (ex-info "Unknown text role." {:key :role :value role})))
    (merge style (select-keys content style-keys))))
