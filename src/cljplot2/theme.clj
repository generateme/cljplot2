(ns cljplot2.theme
  "Plot theme: a nested map of defaults, deep-merged with the plot's `:theme`.

  `:text` maps a role (`:title`, `:strip`, ...) to a text style:

  * `:font` - font family name or `java.awt.Font`, see [[cljplot2.graphics.font/resolve-font]]
  * `:size` - points
  * `:font-style` - `:plain`, `:bold`, `:italic` or `:bold-italic`
  * `:color` - any clojure2d color
  * `:padding` - pixels added on every side of the text
  * `:align` - `[h v]` placement of the text in its box (optional, default `[:center :middle]`), see [[cljplot2.content.label]]

  `:plot` - `:background` (fills the canvas), `:margin` (pixels on every side).

  `:panel` - `:fill`, `:border-color`, `:border-width` (pixels, rounded, drawn inside the panel box),
  `:grid {:x line :y line}` - grid lines at tick positions, `line` is `{:color :width :dash}` or `:none`
  (`:dash` - a sequence of positive dash lengths, or `:none` for a solid line).

  `:axis {:x axis :y axis}` - `axis` is `{:line-color :line-width :tick-color :tick-length :tick-width :label-gap
  :tick-spacing}` (pixels; `:tick-spacing` > 0 is the target distance between ticks, a line or tick of width 0
  isn't drawn).

  `:panel :grid :bands` - grid line style used instead of `:x` / `:y` for a bands scale (default dotted, as lattice
  `dotplot`); `:x` / `:y` `:none` still turns that scale's lines off.

  `:panel :alternate-fill` - drawn over the fill of facet panels with odd `row + col` (`:none` turns it off).

  `:strip` - `:fill`, `:border-color`, `:border-width` of facet strips, `:shingle-fill` (bar of a shingle level); `:facet {:gap px}` between panels;
  `:multiplot {:gap px}` between arranged plots.

  `:palette` - aesthetic defaults, see [[cljplot2.scale.aesthetic]]: `:color` (palette name or colors, old cljplot
  `:category20`), `:gradient` (colors or palette name; `:k2` reversed), `:shape` (shape sequence), `:size`
  (`[min max]` pixels), `:na` (color of NA values).

  `:legend` - `:gap` (pixels between key and label and between legends), `:key-size` (key box side, pixels),
  `:fill`, `:border-color`, `:border-width`, color bar `:bar-length` (`:stretch` - over the space left, or a number,
  clamped to it) and `:bar-width` (`nil` - `:key-size`); text roles `:legend-title` and `:legend-label`.

  `:chart` - per chart kind defaults: `:scatter`, `:histogram`, `:density`, `:line`, `:ecdf` (old cljplot blue),
  `:treemap` (`:padding`, `:border-color`, `:border-width`); each checked against its chart's style schema.

  Colors are any clojure2d color or `:none` (draw nothing). `nil` in a theme override means \"not set\"
  and keeps the value underneath, see [[deep-merge]].

  `build` validates the merged theme against [[Theme]] (closed maps: unknown keys are errors), so bad values fail
  before any stage runs."
  (:require [clojure2d.color :as c]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-keys
  "Keys of a text style; content may override them inline."
  [:font :size :font-style :color :padding :align])

(def ^:private centered [:center :middle])

(defn none?
  "Is `color` the \"draw nothing\" value `:none`?"
  [color]
  (= :none color))

(def default-theme
  "Defaults in old cljplot style: light lavender-gray background, 10 px margin, panels without fill and border."
  {:plot {:background 0xe8e8f0 :margin 10}
   :panel {:fill :none :alternate-fill [0 0 0 20] :border-color :none :border-width 1
           :grid {:x {:color 0xb4b4b4 :width 0.5 :dash [4.0]}
                  :y {:color 0xb4b4b4 :width 0.5 :dash [4.0]}
                  :bands {:color 0xb4b4b4 :width 1 :dash [1.0 2.0]}}}
   :strip {:fill [255 255 255 150] :border-color [0 0 0 100] :border-width 1 :shingle-fill [255 165 0 110]}
   :facet {:gap 5}
   :multiplot {:gap 10}
   :palette {:color :category20
             :gradient (vec (reverse (c/palette :k2)))
             :shape [\O \s \A \x \+ \V \o \S \^ \v \{ \} \< \>]
             :size [2 10]
             :na [150 150 150]}
   :legend {:gap 5 :key-size 14 :fill :none :border-color :none :border-width 1 :bar-length :stretch :bar-width nil}
   :chart {:scatter {:size 4 :color [49 54 149 180] :stroke-color :none :stroke-width 1 :shape \O}
           :histogram {:color [49 54 149] :stroke-width 1 :alpha 0.6}
           :density {:color [49 54 149] :width 1 :dash :none :fill :none}
           :line {:color [49 54 149] :width 1 :dash :none :fill :none}
           :ecdf {:color [49 54 149] :width 1 :dash :none}
           :treemap {:padding 3 :border-color :white :border-width 1}}
   :axis {:x {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 80}
          :y {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 50}}
   :text {:title {:font "Liberation Mono" :size 14 :font-style :bold :color :black :padding 4 :align centered}
          :strip {:font "Liberation Mono" :size 11 :font-style :plain :color :black :padding 2 :align centered}
          :axis-title {:font "Liberation Mono" :size 12 :font-style :plain :color :black :padding 4 :align centered}
          :tick-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}
          :legend-title {:font "Liberation Mono" :size 11 :font-style :bold :color :black :padding 2 :align [:left :middle]}
          :legend-label {:font "Liberation Mono" :size 10 :font-style :plain :color :black :padding 2 :align [:left :middle]}
          :treemap-group {:font "Liberation Mono" :size 10 :font-style :bold :color :black :padding 2 :align [:left :middle]}
          :treemap-leaf {:font "Liberation Mono" :size 9 :font-style :plain :color :black :padding 2 :align centered}}})

(defn deep-merge
  "Merge maps recursively; a non-map value replaces the value it's merged over, `nil` is ignored
  (it can't remove a default)."
  [a b]
  (cond
    (nil? b) a
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    :else b))

;; schema

(def ^:private text-style-entries
  [[:font schema/font] [:size schema/pos] [:font-style schema/font-style] [:color schema/solid-color]
   [:padding schema/non-neg]])

(def TextStyle
  "A text role's style."
  (-> [:map {:closed true}] (into text-style-entries) (conj (schema/optional :align schema/align))))

(def text-style-overrides
  "Map entries of the style keys content may override (all optional), see [[style-keys]]."
  (mapv (fn [[k s]] (schema/optional k s)) (conj text-style-entries [:align schema/align])))

(def ^:private grid-line
  (schema/keyword-or :none [:map {:closed true} [:color schema/color] [:width schema/non-neg] [:dash schema/dash]]))

(def ^:private axis-style
  [:map {:closed true}
   [:line-color schema/color] [:line-width schema/non-neg] [:tick-color schema/color] [:tick-length schema/non-neg]
   [:tick-width schema/non-neg] [:label-gap schema/non-neg] [:tick-spacing schema/pos]])

(def Theme
  "The merged theme (see [[default-theme]]). `:chart` subtrees are checked against the charts' style schemas by
  `build`, see [[cljplot2.pipeline/build]]."
  [:map {:closed true}
   [:plot [:map {:closed true} [:background schema/color] [:margin schema/non-neg]]]
   [:panel [:map {:closed true}
            [:fill schema/color] [:alternate-fill schema/color] [:border-color schema/color] [:border-width schema/non-neg]
            [:grid [:map {:closed true} [:x grid-line] [:y grid-line] [:bands grid-line]]]]]
   [:strip [:map {:closed true}
            [:fill schema/color] [:border-color schema/color] [:border-width schema/non-neg] [:shingle-fill schema/color]]]
   [:facet [:map {:closed true} [:gap schema/non-neg]]]
   [:multiplot [:map {:closed true} [:gap schema/non-neg]]]
   [:palette aesthetic/Palette]
   [:legend [:map {:closed true}
             [:gap schema/non-neg] [:key-size schema/non-neg] [:fill schema/color] [:border-color schema/color]
             [:border-width schema/non-neg] [:bar-length (schema/keyword-or :stretch schema/pos)]
             [:bar-width [:maybe schema/pos]]]]
   [:chart [:map-of keyword? :map]]
   [:axis [:map {:closed true} [:x axis-style] [:y axis-style]]]
   [:text [:map-of keyword? TextStyle]]])

(defn plot-theme
  "Plot theme: `theme` deep-merged over [[default-theme]] (`nil` means the default theme); validated against
  [[Theme]] by `build`."
  [theme]
  (deep-merge default-theme theme))

(def ^:private last-validated
  "`[theme errors]` of the last [[errors]] call."
  (atom nil))

(defn errors
  "Errors of the merged `theme` against [[Theme]] (paths under `[:theme]`, see [[cljplot2.schema/errors]]). Checking
  resolves every color and font, so the result for the last theme is reused for an equal one (plots built in a row
  usually share it)."
  [theme]
  (let [[validated errs] @last-validated]
    (if (and validated (= validated theme))
      errs
      (let [errs (schema/errors Theme theme [:theme])]
        (reset! last-validated [theme errs])
        errs))))

(defn text-style
  "Text style of `content`: the style of its `:role` in `theme` with inline style keys of `content` merged over.
  Throws `ex-info` with `{:key :role}` when the role has no style map."
  [theme content]
  (let [role (:role content)
        style (get-in theme [:text role])]
    (when-not (map? style)
      (schema/fail! "Unknown text role." {:key :role :value role}))
    (merge style (select-keys content style-keys))))
