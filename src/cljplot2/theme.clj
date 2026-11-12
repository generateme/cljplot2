(ns cljplot2.theme
  "Plot theme: a nested map of defaults, deep-merged with the plot's `:theme`.

  `:text` maps a role (`:title`, `:strip`, ...) to a text style:

  * `:font` - font family name or `java.awt.Font`, see [[cljplot2.font/resolve-font]]
  * `:size` - points
  * `:font-style` - `:plain`, `:bold`, `:italic` or `:bold-italic`
  * `:color` - any clojure2d color
  * `:padding` - pixels added on every side of the text
  * `:align` - `[h v]` placement of the text in its box (optional, default `[:center :middle]`), see [[cljplot2.label]]

  `:plot` - `:background` (fills the canvas), `:margin` (pixels on every side).

  `:panel` - `:fill`, `:border-color`, `:border-width` (pixels, rounded, drawn inside the panel box),
  `:grid {:x line :y line}` - grid lines at tick positions, `line` is `{:color :width :dash}` or `:none`
  (`:dash` - a sequence of positive dash lengths, or `:none` for a solid line).

  `:axis {:x axis :y axis}` - `axis` is `{:line-color :line-width :tick-color :tick-length :tick-width :label-gap
  :tick-spacing}` (pixels; `:tick-spacing` > 0 is the target distance between ticks, a line or tick of width 0
  isn't drawn).

  `:chart` - per chart kind defaults, e.g. `:scatter` (validated when a chart of that kind computes).

  Colors are any clojure2d color or `:none` (draw nothing). `nil` in a theme override means \"not set\"
  and keeps the value underneath, see [[deep-merge]].

  [[plot-theme]] validates the merged theme, so bad values fail at build time."
  (:require [cljplot2.font :as font]))

(def style-keys
  "Keys of a text style; content may override them inline."
  [:font :size :font-style :color :padding :align])

(def ^:private centered [:center :middle])

(def ^:private h-aligns #{:left :center :right})
(def ^:private v-aligns #{:top :middle :bottom})

(defn none?
  "Is `color` the \"draw nothing\" value `:none`?"
  [color]
  (= :none color))

(defn- invalid [message data] (throw (ex-info message data)))

(defn- non-negative? [v] (and (number? v) (>= (double v) 0.0)))

(defn validate-align!
  "Throw `ex-info` with `{:key :align}` unless `align` is `[h v]`, `h` in `#{:left :center :right}`,
  `v` in `#{:top :middle :bottom}`."
  [align]
  (when-not (and (vector? align) (= 2 (count align)) (h-aligns (align 0)) (v-aligns (align 1)))
    (invalid "Text :align must be [h v], h in #{:left :center :right}, v in #{:top :middle :bottom}."
             {:key :align :value align})))

(defn validate-text-style!
  "Throw `ex-info` naming the offending key unless `style` is a valid text style (font resolvable,
  positive size, non-negative padding, valid optional align)."
  [{:keys [font font-style size padding align]}]
  (font/resolve-font font font-style size)
  (when-not (non-negative? padding)
    (invalid "Text :padding must be a non-negative number." {:key :padding :value padding}))
  (when (some? align) (validate-align! align)))

(def default-theme
  "Defaults in old cljplot style: light lavender-gray background, 10 px margin, panels without fill and border."
  {:plot {:background 0xe8e8f0 :margin 10}
   :panel {:fill :none :border-color :none :border-width 1
           :grid {:x {:color 0xb4b4b4 :width 0.5 :dash [4.0]}
                  :y {:color 0xb4b4b4 :width 0.5 :dash [4.0]}}}
   :chart {:scatter {:size 4 :color [49 54 149 180] :stroke-color :none :stroke-width 1 :shape \O}}
   :axis {:x {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 80}
          :y {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 50}}
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

(defn- validate-grid-line!
  [k line]
  (when-not (none? line)
    (let [{:keys [color width dash]} line]
      (when-not (map? line)
        (invalid "Panel grid line must be a map or :none." {:key :grid :scale k :value line}))
      (when (nil? color)
        (invalid "Panel grid line :color must be a color or :none." {:key :color :scale k}))
      (when-not (non-negative? width)
        (invalid "Panel grid line :width must be a non-negative number." {:key :width :scale k :value width}))
      (when-not (or (none? dash) (and (sequential? dash) (seq dash) (every? #(and (number? %) (pos? (double %))) dash)))
        (invalid "Panel grid line :dash must be :none or a sequence of positive numbers." {:key :dash :scale k :value dash})))))

(defn- validate-axis!
  [k axis]
  (when-not (map? axis)
    (invalid "Axis style must be a map." {:key :axis :scale k :value axis}))
  (doseq [color-key [:line-color :tick-color]]
    (when (nil? (get axis color-key))
      (invalid "Axis color must be a color or :none." {:key color-key :scale k})))
  (doseq [size-key [:line-width :tick-length :tick-width :label-gap]
          :let [v (get axis size-key)]]
    (when-not (non-negative? v)
      (invalid "Axis size must be a non-negative number." {:key size-key :scale k :value v})))
  (let [spacing (:tick-spacing axis)]
    (when-not (and (number? spacing) (pos? (double spacing)))
      (invalid "Axis :tick-spacing must be a positive number." {:key :tick-spacing :scale k :value spacing}))))

(defn- validate-theme!
  [theme]
  (let [{:keys [margin background]} (:plot theme)
        {:keys [border-width]} (:panel theme)]
    (when-not (non-negative? margin)
      (invalid "Plot :margin must be a non-negative number." {:key :margin :value margin}))
    (when (nil? background)
      (invalid "Plot :background must be a color or :none." {:key :background :value background}))
    (when-not (non-negative? border-width)
      (invalid "Panel :border-width must be a non-negative number." {:key :border-width :value border-width}))
    (doseq [k [:x :y]]
      (validate-grid-line! k (get-in theme [:panel :grid k]))
      (validate-axis! k (get-in theme [:axis k])))
    (doseq [[role style] (:text theme)]
      (when-not (map? style)
        (invalid "Text role style must be a map." {:key :role :value role}))
      (validate-text-style! style))))

(defn plot-theme
  "Plot theme: `theme` deep-merged over [[default-theme]] (`nil` means the default theme), validated.
  Throws `ex-info` naming the offending key."
  [theme]
  (doto (deep-merge default-theme theme) validate-theme!))

(defn text-style
  "Text style of `content`: the style of its `:role` in `theme` with inline style keys of `content` merged over.
  Throws `ex-info` with `{:key :role}` when the role has no style map."
  [theme content]
  (let [role (:role content)
        style (get-in theme [:text role])]
    (when-not (map? style)
      (throw (ex-info "Unknown text role." {:key :role :value role})))
    (merge style (select-keys content style-keys))))
