(ns cljplot2.theme
  "Plot theme: a nested map of style defaults, deep-merged with the plot's `:theme`.

  A plot's `:theme` names only what changes; [[plot-theme]] merges it over [[default-theme]] and `build` validates the result against [[theme-schema]] (closed maps: unknown keys are errors), so bad values fail before any stage runs. Colours are any clojure2d colour or `:none` (draw nothing). `nil` in a theme override means not set and keeps the value underneath, see [[deep-merge]].

  Theme keys:

  - `:plot` - `:background` (fills the canvas), `:margin` (pixels on every side).
  - `:panel` - `:fill`, `:border-color`, `:border-width` (pixels, rounded, drawn inside the panel box); `:invalid-band-size` (pixels across the band of invalid values beside a panel's axis - charts' `:invalid :show`, see [[cljplot2.content.invalid-band]]; at least the band's label); `:alternate-fill` - drawn over the fill of facet panels with odd `row + col` (`:none` turns it off); `:grid {:x line :y line :minor line}` - grid lines at tick positions (`:minor` - at the minor ticks of scales with `:minor-ticks`, default `:none`), `line` is `{:color :line-width :dash}` or `:none` (`:dash` - a sequence of positive dash lengths, a dash style name of [[cljplot2.schema/dash-styles]], or `:none` for a solid line); `:grid :bands` - the grid line style used instead of `:x` / `:y` for a bands scale (default dotted, as lattice `dotplot`; `:x` / `:y` `:none` still turns that scale's lines off); `:grid :bands-fill` - fill of every second band of a bands scale (from the second level), behind the charts, so levels with dodged groups read apart (default a light shade; `:none` turns it off).
  - `:axis {:x axis :y axis}` - `axis` is `{:line-color :line-width :tick-color :tick-length :tick-width :label-gap :tick-label-gap :tick-spacing :label-rotation :minor-tick-length :tick-label}` (pixels; `:tick-label` - text style keys of the axis's tick labels, axes and polar guides, merged over the `:tick-label` text role; `:minor-tick-length` - ticks of a scale's `:minor-ticks`; `:tick-label-gap` - the least distance between neighbouring tick labels' texts lying end to end along the axis (unrotated on a horizontal axis, at -90 or 90 degrees on a vertical one; stacked labels are spaced by their line height), closer labels are thinned (1 px less allowed: measured sizes are rounded up); `:tick-spacing` > 0 is the target distance between ticks; a line or tick of width 0 isn't drawn; `:label-rotation` - tick labels rotated clockwise by -90..90 degrees, see [[cljplot2.content.axis]]). Keyed by scale, also when a flipped plot draws `:x` vertically. 3D boxes draw their axes, edges and ticks with `[:axis :x]` (`:line-color`, `:line-width` - the box edges too).
  - `:frame-3d` - `:hidden-edge-dash` - the dash of a 3D box's hidden edges (a dash as in `:panel :grid`).
  - `:strip` - `:fill`, `:border-color`, `:border-width` of facet strips, `:shingle-fill` (bar of a shingle level); `:facet {:gap px}` between panels; `:multiplot {:gap px}` between arranged plots.
  - `:palette` - aesthetic defaults, see [[cljplot2.scale.aesthetic]]: `:color` (palette name or colours; default [[default-palette]] - 14 colours checked for colour vision deficiencies), `:gradient` (colours or palette name; `:k2` reversed), `:shape` (shape sequence), `:size` (`[min max]` pixels), `:dash` (dash sequence: the [[cljplot2.schema/dash-styles]] names, adjacent styles far apart), `:pattern` (pattern sequence: the [[cljplot2.schema/pattern-styles]] names without `:solid`), `:na` (colour of NA values).
  - `:legend` - `:gap` (pixels between key and label and between legends), `:key-size` (key box side, pixels), `:fill`, `:border-color`, `:border-width`, colour bar `:bar-length` (`:stretch` - over the space left, or a number, clamped to it), `:bar-width` (`nil` - `:key-size`), `:bar-ticks` (tick count unless the scale's `:ticks` gives one), `:bar-tick-length` and `:bar-tick-width` (pixels; width 0 - no tick lines), `:min-key-line-width` (least pixel width of line marks in keys, see [[key-line-style]]); text roles `:legend-title` and `:legend-label`.
  - `:chart` - per chart kind styles, each checked against its chart's style schema: `:scatter`, `:histogram`, `:density`, `:line`, `:ecdf` ([[default-color]], the palette's first colour, with cljplot's alphas: scatter 180, stripplot 100, rug 100), `:treemap` (`:gap`, `:stroke-color`, `:stroke-width`), `:partition` (`:stroke-color`, `:stroke-width`), `:pack` (`:gap`, `:stroke-color`, `:stroke-width`), `:waffle` (`:gap`, `:cell-shape`, outlines), `:cloud` (the point style; `:line-width` - spikes and lines; `:projection-alpha-factor`, `:band-alpha-factor` - alpha factors of projected points and density bands), `:venn` (`:alpha`, outlines, `:label-gap` - pixels between a circle and its set name), `:tile` / `:bin2d` (`:cell`, `:cell-scale`, `:stroke-color`, `:stroke-width`, `:alpha-factor`; tile `:size-key-fill` - a size legend key without a colour), `:field` (`:alpha-factor`), `:contour` (`:color`, `:line-width`, `:dash`, `:alpha-factor`), `:correlation` (`:color`, `:stroke-color`, `:stroke-width`), `:pairwise` (`:color`), strip charts `:box`, `:violin`, `:stripplot`, `:beeswarm`, `:dotplot`, `:rug`, `:extent`, `:bar`, annotations `:rule`, `:rect`, `:function`, bands `:ribbon`, `:smooth` (band `:alpha` 0.3), `:segment`, `:polygon`, probability plots `:qq` / `:pp` (scatter's point style, `:line-width`), `:image` (no style), `:candlestick` (green up, red down), `:parallel` (radar `:fill` `:alpha` 0.25); line and function `:bridge` - the style of bridges over gaps (`:alpha-factor` multiplies the line alpha, optional `:line-width`, `:dash`); line `:alpha` - opacity of `:fill :color` and stacked areas. Entries that aren't kinds are named styles (see [[named-styles]], [[chart-style]]).
  - `:text` - a text role (`:title`, `:strip`, ..., `:contour-label` - contour level labels, `:annotation` - text charts and strip n labels, see [[text-roles]]) -> a text style: `:font` (font family name or `java.awt.Font`, see [[cljplot2.graphics.font/resolve-font]]), `:size` (points), `:font-style` (`:plain`, `:bold`, `:italic` or `:bold-italic`), `:color` (any clojure2d colour), `:inset` (pixels between the text and its box edge on every side), `:align` (`[h v]` placement of the text in its box; optional, default `[:center :middle]`, see [[cljplot2.content.label]]), `:background` (a colour, alpha allowed, filling the label's box; optional, default none, `:none` - none: for labels charts place at a point - text chart, tile values ... - the text plus `:inset`; for layout labels - titles, axis titles, strips, legend labels - the whole item box), `:min-size` (optional: the least size in points a text too long for its room shrinks to - axis titles, strips, text charts with `:x-span`, builder labels; a role without it doesn't shrink; see [[cljplot2.content.label/fitted-label]]), `:dollar-math` (optional: `false` keeps every `$` of its texts literal; default on - `$...$` pairs are LaTeX formulas, see [[cljplot2.data.text]]).

  Functions and vars: [[default-theme]], [[default-palette]], [[default-color]], [[plot-theme]], [[deep-merge]], [[none?]]; chart styles [[chart-style]], [[resolved-style]], [[key-style]], [[key-line-style]], [[named-styles]], [[theme-style-entries]]; text [[text-style]], [[style-keys]], [[text-roles]], [[text-style-schema]], [[text-style-overrides]]; validation [[theme-schema]], [[errors]]."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-keys
  "Keys of a text style; content (a label, a text chart) may override them inline."
  [:font :size :font-style :color :inset :align :background :dollar-math])

(def ^:private centered
  "Alignment of a centred text, `[:center :middle]`."
  [:center :middle])

(defn none?
  "Tests whether a colour is the draw-nothing value `:none`.

  Parameters:

  - `color`: a colour or `:none`.

  Returns true for `:none`, else false."
  [color]
  (= :none color))

(def named-styles
  "Named chart styles of the default theme: name -> the chart kind whose style it overrides. Each is a partial theme `[:chart name]` style merged over `[:chart kind]` for charts with `:theme-style name` (see [[chart-style]]); builders draw with them, so a theme restyles a builder's marks. `build` checks each against its kind's style schema."
  {:network-node :scatter
   :network-edge :segment
   :arc-node :scatter
   :arc-edge :line
   :dendrogram-branch :line
   :tree-node :scatter
   :tree-link :line
   :bundle-edge :line
   :bundle-leaf :scatter
   :sankey-link :polygon
   :alluvial-stratum :polygon
   :sequences-cell :partition
   :acf-point :scatter})

(def default-palette
  "The default categorical palette, `[:palette :color]` of [[default-theme]]: 14 colours checked for colour vision deficiencies.

  The Okabe-Ito colours ordered so the second one reads as text and thin lines on the plot background (contrast at least 3) - blue, vermilion, green, orange, purple, sky - then a darker yellow (still apart from orange under deuteranopia), a gray darker than the NA colour in place of black, then lighter versions of the first six: a lighter blue turned toward violet (apart from sky), lighter vermilion, green, orange, purple and sky. The lighter six are weak as thin lines and small text on the background (contrast 1.2 to 2.8). Levels past 14 repeat the colours."
  (mapv c/color ["#0072b2" "#d55e00" "#009e73" "#e69f00" "#cc79a7" "#56b4e9" "#cccd23" "#6e6e6e"
                 "#8289cf" "#fb9864" "#6ec9a4" "#ffd47a" "#f5afd4" "#9be3ff"]))

(def default-color
  "The colour of marks without a colour mapping: the first colour of [[default-palette]], so an ungrouped chart and the first group look alike. Chart styles add their alphas to it."
  (first default-palette))

(def default-theme
  "The default theme in old cljplot style with a palette checked for colour vision deficiencies (see [[default-palette]]): a light lavender-gray background, 10 px margin, panels without fill and border; every key of [[theme-schema]] has a value (see the namespace doc for the keys)."
  {:plot {:background (c/color 236 236 243) :margin 10}
   :panel {:fill :none :alternate-fill (c/gray 0 12) :border-color :none :border-width 1 :invalid-band-size 24
           :grid {:x {:color (c/gray 180) :line-width 0.5 :dash [4.0]}
                  :y {:color (c/gray 180) :line-width 0.5 :dash [4.0]}
                  :bands {:color (c/gray 180) :line-width 1 :dash [1.0 2.0]}
                  :bands-fill (c/gray 0 8)
                  :minor :none}}
   :strip {:fill (c/gray 255 150) :border-color (c/gray 0 100) :border-width 1 :shingle-fill (c/color 255 165 0 110)}
   :facet {:gap 5}
   :multiplot {:gap 10}
   :palette {:color default-palette
             :gradient (vec (reverse (c/palette :k2)))
             :shape [\O \s \A \x \+ \V \o \S \^ \v \{ \} \< \>]
             :size [2 10]
             :dash [:solid :dashed :dotted :dot-dash :long-dash :dot-dot-dash :two-dash :loose-dot]
             :pattern [:hatch :back-hatch :horizontal :vertical :cross :diagonal-cross :dots :dense-dots]
             :na (c/gray 150)}
   :legend {:gap 5 :key-size 14 :fill :none :border-color :none :border-width 1 :bar-length :stretch :bar-width nil
            :bar-ticks 5 :bar-tick-length 3 :bar-tick-width 1 :min-key-line-width 2}
   :frame-3d {:hidden-edge-dash [3 3]}
   :chart {:scatter {:size 6 :color (c/set-alpha default-color 180) :stroke-color :none :stroke-width 1 :shape \O}
           :cloud {:size 6 :color (c/set-alpha default-color 180) :stroke-color :none :stroke-width 1 :line-width 1 :shape \O :projection-alpha-factor 0.35 :band-alpha-factor 0.15}
           :surface {:stroke-color (c/gray 60) :stroke-width 0.5}
           :histogram {:color default-color :stroke-color :darker :stroke-width 1 :alpha 0.6 :zero-line-color :black
                       :pattern :solid :pattern-color :darker
                       :zero-line-width 1}
           :bar {:color default-color :stroke-color :darker :stroke-width 1 :alpha 0.6 :zero-line-color :black
                 :pattern :solid :pattern-color :darker
                 :zero-line-width 1}
           :density {:color default-color :line-width 1 :dash :none :fill :none :alpha 0.6 :zero-line-color :black
                     :pattern :solid :pattern-color :darker
                     :zero-line-width 1}
           :line {:color default-color :line-width 1 :dash :none :fill :none :alpha 0.6 :bridge {:alpha-factor 0.3}
                  :pattern :solid :pattern-color :darker}
           :ecdf {:color default-color :line-width 1 :dash :none}
           :treemap {:gap 3 :stroke-color :white :stroke-width 1
                     :pattern :solid :pattern-color :darker}
           :partition {:stroke-color :white :stroke-width 1 :pattern :solid :pattern-color :darker}
           :pack {:gap 3 :stroke-color :white :stroke-width 1 :pattern :solid :pattern-color :darker}
           :waffle {:gap 0.1 :cell-shape :square :stroke-color :none :stroke-width 1 :pattern :solid :pattern-color :darker}
           :venn {:alpha 0.4 :stroke-color :darker :stroke-width 1 :label-gap 4}
           :tile {:cell :square :cell-scale 1.0 :stroke-color :none :stroke-width 1 :alpha-factor 1.0 :size-key-fill (c/gray 180)}
           :bin2d {:cell :pointy-hex :cell-scale 1.0 :stroke-color :none :stroke-width 1 :alpha-factor 1.0}
           :field {:alpha-factor 1.0}
           :contour {:color default-color :line-width 1 :dash :none :alpha-factor 1.0}
           :correlation {:color default-color :stroke-color :darker :stroke-width 1}
           :pairwise {:color default-color}
           :box {:color default-color :stroke-color :darker :stroke-width 1 :shape \O :size 5
                 :pattern :solid :pattern-color :darker}
           :violin {:color default-color :stroke-color :darker :stroke-width 1 :inner-color :contrast
                    :inner-box-width 5.0 :inner-median-size 4.0 :inner-quartile-dash [4.0 3.0] :inner-rug-width 1.0
                    :inner-point-size 3.0
                    :pattern :solid :pattern-color :darker}
           :stripplot {:size 6 :color (c/set-alpha default-color 100) :stroke-color :none :stroke-width 1 :shape \O}
           :beeswarm {:size 6 :color (c/set-alpha default-color 180) :stroke-color :none :stroke-width 1 :shape \O}
           :dotplot {:color default-color :stroke-color :none :stroke-width 1 :shape \O}
           :rug {:color (c/set-alpha default-color 100) :line-width 1}
           :bullet {:range-colors [(c/gray 204) (c/gray 238)] :measure-colors [default-color (c/color 176 196 222)]
                    :measure-fraction 0.35 :marker-color :black :marker-width 2 :marker-length 0.7}
           :extent {:color default-color :line-width 2 :shape \O :size 7}
           :rule {:color (c/gray 60) :line-width 1 :dash :none}
           :function {:color default-color :line-width 1 :dash :none :fill :none :bridge {:alpha-factor 0.3}}
           :ribbon {:color default-color :fill :color :alpha 0.3 :stroke-color :none :stroke-width 1 :dash :none
                    :pattern :solid :pattern-color :darker}
           :smooth {:color default-color :line-width 1 :dash :none :fill :color :alpha 0.3
                    :pattern :solid :pattern-color :darker}
           :segment {:color default-color :line-width 1 :dash :none}
           :polygon {:color default-color :alpha 1.0 :stroke-color :none :stroke-width 1}
           :qq {:size 6 :color (c/set-alpha default-color 180) :stroke-color :none :stroke-width 1 :shape \O :line-width 1}
           :image {}
           :pp {:size 6 :color (c/set-alpha default-color 180) :stroke-color :none :stroke-width 1 :shape \O :line-width 1}
           :parallel {:color (c/set-alpha default-color 140) :line-width 1 :dash :none :fill :none :alpha 0.25
                      :pattern :solid :pattern-color :darker}
           :candlestick {:up-color (c/color 26 152 80) :down-color (c/color 215 48 39) :width 0.7 :line-width 1 :stroke-color :none :stroke-width 1
                         :up-pattern :solid :down-pattern :solid :pattern-color :darker}
           :rect {:fill (c/gray 0 30) :stroke-color :none :stroke-width 1
                  :pattern :solid :pattern-color :darker}
           ;; named styles of builders (see named-styles): partial, merged over their kind's style
           :network-node {:size 12 :color default-color :stroke-color :white :stroke-width 1}
           :network-edge {:color (c/gray 120 200)}
           :arc-node {:size 8 :color default-color :stroke-color :white :stroke-width 1}
           :arc-edge {:color (c/gray 120) :line-width 1}
           :dendrogram-branch {:color (c/gray 70) :line-width 1}
           :tree-node {:size 7 :color default-color :stroke-color :white :stroke-width 1}
           :tree-link {:color (c/gray 176) :line-width 0.9}
           :bundle-edge {:line-width 1}
           :bundle-leaf {:size 3}
           :sankey-link {:color (c/gray 150)}
           :alluvial-stratum {:color (c/gray 150) :stroke-color :white}
           :sequences-cell {:stroke-width 0.3}
           :acf-point {:size 5}}
   :axis {:x {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-label-gap 3 :tick-spacing 80 :label-rotation 0 :minor-tick-length 3 :tick-label {}}
          :y {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-label-gap 3 :tick-spacing 50 :label-rotation 0 :minor-tick-length 3 :tick-label {}}}
   :text {:title {:font "DejaVu Sans Mono" :size 14 :font-style :bold :color :black :inset 4 :align centered}
          :strip {:font "DejaVu Sans Mono" :size 11 :min-size 7 :font-style :plain :color :black :inset 2 :align centered}
          :axis-title {:font "DejaVu Sans Mono" :size 12 :min-size 8 :font-style :plain :color :black :inset 4 :align centered}
          :tick-label {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :inset 2 :align centered}
          :legend-title {:font "DejaVu Sans Mono" :size 11 :font-style :bold :color :black :inset 2 :align [:left :middle]}
          :legend-label {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :inset 2 :align [:left :middle]}
          :treemap-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :contrast :inset 2 :align [:left :middle]}
          :treemap-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :contrast :inset 2 :align centered}
          :partition-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :contrast :inset 2 :align centered}
          :partition-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :contrast :inset 2 :align centered}
          :pack-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :contrast :inset 2 :align centered}
          :pack-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :contrast :inset 2 :align centered}
          :venn-set {:font "DejaVu Sans Mono" :size 11 :font-style :bold :color :black :inset 2 :align centered}
          :venn-count {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :contrast :inset 2 :align centered}
          :tile-value {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :contrast :inset 1 :align centered}
          :correlation {:font "DejaVu Sans Mono" :size 12 :font-style :plain :color :contrast :inset 1 :align centered}
          :pairwise {:font "DejaVu Sans Mono" :size 12 :font-style :plain :color :contrast :inset 1 :align centered}
          :contour-label {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :inset 1 :align centered
                          :background (c/gray 255 180)}
          :annotation {:font "DejaVu Sans Mono" :size 10 :min-size 6 :font-style :plain :color :black :inset 2 :align centered}
          :bar-value {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :contrast :inset 2 :align centered}
          :network-label {:font "DejaVu Sans Mono" :size 8 :font-style :plain :color :black :inset 1 :align centered
                          :background (c/gray 255 170)}
          :sankey-label {:font "DejaVu Sans Mono" :size 9 :min-size 6 :font-style :plain :color :black :inset 1 :align centered
                         :background (c/gray 255 170)}
          :dendrogram-group-label {:font "DejaVu Sans Mono" :size 8 :font-style :plain :color :black :inset 1 :align centered
                                   :background (c/gray 255 170)}
          :alluvial-label {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :inset 1 :align centered
                           :background (c/gray 255 190)}
          :tree-label {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :inset 2 :align centered}
          :radial-label {:font "DejaVu Sans Mono" :size 9 :min-size 5 :font-style :plain :color :black :inset 2 :align centered}
          :pairs-name {:font "DejaVu Sans Mono" :size 13 :font-style :bold :color :black :inset 2 :align centered}}})

(defn deep-merge
  "Merges two values recursively, `b` over `a`.

  Parameters:

  - `a`: the value underneath (e.g. a theme, scales).
  - `b`: the value merged over it.

  Returns the maps merged key by key at every depth; a non-map `b` replaces `a`; a `nil` `b` keeps `a` (it can't remove a default)."
  [a b]
  (cond
    (nil? b) a
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    :else b))

(def theme-style-entries
  "Map entry of the chart option `:theme-style` (shared group `:theme-style`), taken by every chart kind with a theme style: the name of a theme `[:chart name]` style merged over the kind's, see [[chart-style]]."
  (schema/in-fragment
   :theme-style
   [(schema/optional :theme-style {:description "The name of a theme style `[:chart name]`, a builder's or one the theme adds, merged over the kind's style."
                                   :cljplot2/default-doc "None: the kind's style." :cljplot2/example :network-node}
                     keyword?)]))

(defn chart-style
  "Gives the theme style of a chart: its kind's style `[:chart kind]` with the named style of its `:theme-style` option merged over it.

  Parameters:

  - `theme` (map): the merged theme.
  - `kind` (keyword): the chart kind (`:scatter`, `:line` ...).
  - `options` (map): the chart's options; `:theme-style` (keyword, optional) names a theme `[:chart s]` entry - a partial style overriding the kind's key by key (see [[named-styles]]).

  Returns the merged style map: without `:theme-style`, or naming an entry the theme hasn't, the kind's style as it is; `nil` when the theme has neither. `build` rejects a `:theme-style` naming no named style (see [[cljplot2.pipeline/build]])."
  [theme kind options]
  (let [chart-theme (:chart theme)]
    (merge (get chart-theme kind) (some->> (:theme-style options) (get chart-theme)))))

(defn resolved-style
  "Gives the style a chart draws with: its theme style with its own style options over it.

  The keys taken from the options are the entries of the chart's style schema (`:schema` `:style`), so an option added to the schema is resolved without listing it again. A style key that isn't an option (theme only, e.g. cloud `:projection-alpha-factor`) never comes from the options - the closed options schema rejects it - and keeps its theme value.

  Parameters:

  - `theme` (map): the merged theme.
  - `chart` (map): a chart value - `:kind`, `:options`, `:schema` (`:style` - the kind's style schema; `nil` for a user chart without one - its theme style only).

  Returns [[chart-style]] of the chart with the options' values of its style keys merged over it; options outside the style schema aren't taken (a chart merges such extras itself).

  See also [[cljplot2.schema/entry-keys]]."
  [theme {:keys [kind options schema]}]
  (merge (chart-style theme kind options) (select-keys options (schema/entry-keys (:style schema)))))

(defn key-line-style
  "Gives a legend key glyph's style with its line width at least theme `[:legend :min-key-line-width]`.

  A hairline of a light colour in a small key reads as grey; keys draw thin line marks wider.

  Parameters:

  - `theme` (map): the merged theme.
  - `style` (map): the glyph's style; `:line-width` (pixels) read.

  Returns `style` with a positive `:line-width` raised to the theme's width; unchanged without a numeric
  `:line-width` or with width 0 (no line, e.g. an area without outline)."
  [theme style]
  (let [width (:line-width style)
        least (get-in theme [:legend :min-key-line-width])]
    (if (and (number? width) (m/pos? (double width)) (m/< (double width) (double least)))
      (assoc style :line-width least)
      style)))

(defn key-style
  "Gives the style a chart's legend key glyph draws with: its resolved style with the key's mapped attributes over it.

  Parameters:

  - `env` (map): the drawing environment - `:theme`.
  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes (`:color`, `:shape`, `:size`, `:dash`, `:pattern` ...).
  - `attribute-keys` (sequence or `nil`): the attributes the glyph takes; `nil` - all of them.

  Returns [[resolved-style]] of `chart` (its line width raised by [[key-line-style]]) with the attributes merged over
  it - a mapped `:size` a glyph takes as its width stays as mapped."
  [env chart attributes attribute-keys]
  (merge (key-line-style (:theme env) (resolved-style (:theme env) chart))
         (if attribute-keys (select-keys attributes attribute-keys) attributes)))

;; schema

(def ^:private text-style-entries
  "`[key schema]` of the required text style keys."
  [[:font schema/font] [:size schema/pos] [:font-style schema/font-style] [:color schema/text-color]
   [:inset schema/non-neg]])

(def ^:private text-style-descriptions
  "Text style key -> its description in option tables."
  {:font "font name (bundled, installed or logical) or a java.awt.Font"
   :size "font size in points"
   :font-style "font style"
   :color "text colour; :contrast - black or white by what is behind (a chart's fill under its labels, else the plot background)"
   :inset "space between the text and its box edge on every side, in pixels"
   :align "alignment [horizontal vertical] of the text at its anchor"
   :background "box colour behind the text (:none - no box)"
   :min-size "smallest font size in points a text too long for its room shrinks to (a role without it doesn't shrink)"
   :dollar-math "true draws text between two dollar signs as a LaTeX formula (a dollar after a backslash stays literal), false keeps every dollar literal; default true"})

(def text-style-schema
  "Schema of a text role's style: `:font :size :font-style :color :inset` required, `:align`, `:background`, `:min-size` (a role's least fitted size, not a key content overrides) and `:dollar-math` optional."
  (-> [:map {:closed true :cljplot2/type-doc "a text style (:font :size :font-style :color :inset :align :background :min-size :dollar-math)"}]
      (into (map (fn [[k s]] [k {:description (text-style-descriptions k)} s])) text-style-entries)
      (conj (schema/optional :align {:description (text-style-descriptions :align)} schema/align)
            (schema/optional :background {:description (text-style-descriptions :background)} schema/color)
            (schema/nilable :min-size {:description (text-style-descriptions :min-size)} schema/pos)
            (schema/nilable :dollar-math {:description (text-style-descriptions :dollar-math)} schema/bool))))

(def ^:private text-style-card-props
  "Text style key -> its card properties in the shared group `:text-style` (description, default text, meanings,
  example); the theme's text style tables keep [[text-style-descriptions]] until their own cards."
  {:font {:description "The font: a bundled, installed or logical family name, or a `java.awt.Font`." :cljplot2/example "Serif"}
   :size {:description "The font size in points." :cljplot2/example 20}
   :font-style {:description "The font style." :cljplot2/example :bold
                :cljplot2/value-meanings {:plain "Upright, regular weight." :bold "Bold." :italic "Italic." :bold-italic "Bold and italic."}}
   :color {:description "The text colour." :cljplot2/example :red
           :cljplot2/value-meanings {:contrast "Black or white, whichever reads better on what is behind: the text's own `:background` box, a chart's fill under its labels, else the plot background."}
           :cljplot2/form-meanings {::schema/solid-color "This colour, e.g. `:black`."}}
   :inset {:description "The space between the text and its box's edge on every side, in pixels." :cljplot2/example 8 :cljplot2/example-context {:background :yellow}}
   :align {:description "The alignment `[horizontal vertical]` of a label's lines at its anchor; label maps take it, the text chart doesn't." :cljplot2/no-example true}
   :background {:description "The colour of a box behind the text." :cljplot2/example :yellow
                :cljplot2/value-meanings {:none "No box."}
                :cljplot2/form-meanings {::schema/solid-color "This colour, e.g. `:white`."}}
   :dollar-math {:description "Whether text between two dollar signs draws as a LaTeX formula; a dollar after a backslash stays literal."
                 :cljplot2/example false :cljplot2/example-context {:text ["$x^2$ costs $5"]}
                 :cljplot2/value-meanings {true "Formulas between dollars." false "Every dollar literal."}}})

(def text-style-overrides
  "Map entries of the text style keys content may override (all optional, shared group `:text-style`), see [[style-keys]]."
  (schema/in-fragment
   :text-style
   (conj (mapv (fn [[k s]] (schema/optional k (assoc (text-style-card-props k) :cljplot2/default-doc "The text role's value.") s))
               (conj text-style-entries [:align schema/align] [:background schema/color]))
         (schema/nilable :dollar-math (assoc (text-style-card-props :dollar-math) :cljplot2/default-doc "The text role's value.") schema/bool))))

(def text-roles
  "The text roles of the default theme: role -> `{:section s :description d}` - what the role styles and who draws it (`:plot-text` - titles, strips, axes; `:legend-text`; `:hierarchy-text` - treemap, partition and pack labels; `:chart-text` - texts of other charts; `:builder-text` - labels of builders: network, sankey, dendrogram, alluvial, tree, bundle, chord, pairs). A theme may add roles of its own, used by labels' and text charts' `:role`."
  {:title {:section :plot-text :description "plot and multiplot titles"}
   :strip {:section :plot-text :description "facet strip texts"}
   :axis-title {:section :plot-text :description "axis titles"}
   :tick-label {:section :plot-text :description "tick labels"}
   :legend-title {:section :legend-text :description "legend titles"}
   :legend-label {:section :legend-text :description "legend entry labels and colour bar ticks"}
   :treemap-group {:section :hierarchy-text :description "treemap group headers"}
   :treemap-leaf {:section :hierarchy-text :description "treemap leaf labels"}
   :partition-group {:section :hierarchy-text :description "partition (icicle, sunburst) group labels"}
   :partition-leaf {:section :hierarchy-text :description "partition leaf labels"}
   :pack-group {:section :hierarchy-text :description "circle packing group labels"}
   :pack-leaf {:section :hierarchy-text :description "circle packing leaf labels"}
   :venn-set {:section :chart-text :description "Venn diagram set names"}
   :venn-count {:section :chart-text :description "Venn diagram region counts"}
   :tile-value {:section :chart-text :description "tile value labels"}
   :correlation {:section :chart-text :description "correlation numbers"}
   :pairwise {:section :chart-text :description "pairwise statistic numbers"}
   :contour-label {:section :chart-text :description "contour level labels"}
   :annotation {:section :chart-text :description "text charts and strip n labels"}
   :bar-value {:section :chart-text :description "bar value and count labels"}
   :network-label {:section :builder-text :description "network node labels"}
   :sankey-label {:section :builder-text :description "sankey node labels"}
   :dendrogram-group-label {:section :builder-text :description "dendrogram group (inner node) labels"}
   :alluvial-label {:section :builder-text :description "alluvial stratum labels"}
   :tree-label {:section :builder-text :description "node-link tree node labels"}
   :radial-label {:section :builder-text :description "leaf names around circular builders (dendrogram, tree, bundle, chord); the size shrinks with the leaf count (not to a room), down to the role's :min-size, never above its size"}
   :pairs-name {:section :builder-text :description "variable names on the diagonal of a scatter-plot matrix"}})

(def ^:private grid-line
  "Schema of a grid line style `{:color :line-width :dash}`, or `:none`."
  (schema/keyword-or :none [:map {:closed true :cljplot2/type-doc "a line {:color :line-width :dash}"}
                            [:color {:description "line colour"} schema/color]
                            [:line-width {:description "line width in pixels"} schema/non-neg]
                            [:dash {:description "dash style (:none - solid)"} schema/dash]]))

(def ^:private axis-style
  "Schema of one axis style (see the namespace doc, `:axis`)."
  [:map {:closed true}
   [:line-color {:description "axis line colour"} schema/color]
   [:line-width {:description "axis line width in pixels (0 - not drawn)"} schema/non-neg]
   [:tick-color {:description "tick colour"} schema/color]
   [:tick-length {:description "tick length in pixels"} schema/non-neg]
   [:tick-width {:description "tick width in pixels (0 - not drawn)"} schema/non-neg]
   [:label-gap {:description "gap between ticks and tick labels in pixels"} schema/non-neg]
   [:tick-label-gap {:description "least pixels between neighbouring tick labels lying end to end along the axis; a closer label isn't drawn"} schema/non-neg]
   [:tick-spacing {:description "target distance between ticks in pixels (sets the tick count)"} schema/pos]
   [:label-rotation {:description "tick labels rotated clockwise, degrees"} [:and number? [:>= -90] [:<= 90]]]
   [:minor-tick-length {:description "minor tick length in pixels"} schema/non-neg]
   [:tick-label {:description "text style keys of this axis's tick labels (axes and polar guides) merged over the :tick-label text role"
                 :cljplot2/entries-fragment :text-style}
    (into [:map {:closed true :cljplot2/type-doc "a partial text style (:font, :size, :color ...)"}] text-style-overrides)]])

(def theme-schema
  "Schema of the merged theme (see [[default-theme]]; every key has a value there - overrides are partial). `:chart` subtrees are checked against the charts' style schemas by `build`, see [[cljplot2.pipeline/build]]."
  [:map {:closed true :cljplot2/merged true}
   [:plot {:description "the whole canvas"}
    [:map {:closed true}
     [:background {:description "canvas background colour"} schema/color]
     [:margin {:description "pixels around the plot on every side"} schema/non-neg]]]
   [:panel {:description "the data panels"}
    [:map {:closed true}
     [:fill {:description "panel background colour" :cljplot2/section :panel-box} schema/color]
     [:alternate-fill {:description "drawn over odd facet panels (row + column odd); :none - off" :cljplot2/section :panel-box} schema/color]
     [:border-color {:description "panel border colour" :cljplot2/section :panel-box} schema/color]
     [:border-width {:description "panel border width in pixels (drawn inside the panel)" :cljplot2/section :panel-box} schema/non-neg]
     [:invalid-band-size {:description "pixels across the band of invalid values beside a panel's axis (charts' :invalid :show), at least its label's"
                          :cljplot2/section :panel-box}
      schema/non-neg]
     [:grid{:description "grid lines at the ticks" :cljplot2/section :grid}
      [:map {:closed true}
       [:x {:description "grid lines of the x scale"} grid-line]
       [:y {:description "grid lines of the y scale"} grid-line]
       [:bands {:description "grid lines of a bands scale, instead of :x / :y"} grid-line]
       [:bands-fill {:description "fill of every second band of a bands scale; :none - off"} schema/color]
       [:minor {:description "grid lines at minor ticks"} grid-line]]]]]
   [:strip {:description "facet strips"}
    [:map {:closed true}
     [:fill {:description "strip background colour"} schema/color]
     [:border-color {:description "strip border colour"} schema/color]
     [:border-width {:description "strip border width in pixels"} schema/non-neg]
     [:shingle-fill {:description "colour of the bar marking a shingle level"} schema/color]]]
   [:facet {:description "facet layout"}
    [:map {:closed true} [:gap {:description "pixels between facet panels"} schema/non-neg]]]
   [:multiplot {:description "arranged plots"}
    [:map {:closed true} [:gap {:description "pixels between arranged plots"} schema/non-neg]]]
   [:palette {:description "default aesthetic ranges"} aesthetic/palette-schema]
   [:legend {:description "legends and colour bars"}
    [:map {:closed true}
     [:gap {:description "pixels between key and label and between legends"} schema/non-neg]
     [:key-size {:description "side of a legend key box in pixels"} schema/non-neg]
     [:fill {:description "legend box fill"} schema/color]
     [:border-color {:description "legend box border colour"} schema/color]
     [:border-width {:description "legend box border width in pixels"} schema/non-neg]
     [:bar-length {:description "colour bar length: :stretch (the space left) or pixels"} (schema/keyword-or :stretch schema/pos)]
     [:bar-width {:description "colour bar width in pixels (nil - the key size)"} [:maybe schema/pos]]
     [:bar-ticks {:description "colour bar tick count (a scale's :ticks wins)"} schema/pos-int]
     [:bar-tick-length {:description "colour bar tick length in pixels (labels follow it)"} schema/non-neg]
     [:bar-tick-width {:description "colour bar tick width in pixels (0 - no tick lines)"} schema/non-neg]
     [:min-key-line-width {:description "least width in pixels of the line marks drawn in legend keys (thinner visible lines are drawn this wide there, width 0 stays 0; 0 - as in the panel)"} schema/non-neg]]]
   [:frame-3d {:description "3D box frames (their axes and edges take [:axis :x])"}
    [:map {:closed true}
     [:hidden-edge-dash {:description "dash of the hidden box edges (:none - solid)"} schema/dash]]]
   [:chart {:description "per chart kind styles, see each chart's style options"}
    [:and {:cljplot2/type-doc "map of chart kind to its style"} [:map-of keyword? :map]
     [:map (schema/optional :text (schema/forbidden "isn't used: text charts take the text role [:text :annotation]"))]]]
   [:axis {:description "axes per scale (kept by scale also when a flipped plot draws :x vertically)"}
    [:map {:closed true} [:x {:description "x axis style" :cljplot2/section :x-axis} axis-style]
     [:y {:description "y axis style" :cljplot2/section :y-axis} axis-style]]]
   [:text {:description "text styles by role (see text-roles)"} [:map-of keyword? text-style-schema]]])

(defn plot-theme
  "Returns the theme of a plot: its `theme` overrides deep-merged over [[default-theme]] (see [[deep-merge]]).

  Parameters:

  - `theme` (map or `nil`): the plot's `:theme`; `nil` - the default theme.

  The result isn't validated here; `build` checks it against [[theme-schema]] (see [[errors]])."
  [theme]
  (deep-merge default-theme theme))

(defonce ^{:private true :doc "`[theme errors]` of the last [[errors]] call."} last-validated
  (atom nil))

(defn errors
  "Validates a merged theme against [[theme-schema]].

  Parameters:

  - `theme` (map): the merged theme (see [[plot-theme]]).

  Returns the errors (`{:in :value :message}`, paths under `[:theme]`, see [[cljplot2.schema/errors]]), empty when valid. Checking resolves every colour and font, so the result for the last theme is reused for an equal one (plots built in a row usually share it)."
  [theme]
  (let [[validated cached-errs] @last-validated]
    (if (and validated (= validated theme))
      cached-errs
      (let [errs (schema/errors theme-schema theme [:theme])]
        (reset! last-validated [theme errs])
        errs))))

(defn text-style
  "Returns the text style of a label: its role's style with its inline style keys merged over (an inline `nil` keeps the role's value).

  Parameters:

  - `theme` (map): the merged theme.
  - `content` (map): label content with `:role` (a key of the theme's `:text`) and optional inline keys of [[style-keys]].

  Throws `ex-info` with `{:key :role}` when the role has no style map in `theme`."
  [theme content]
  (let [role (:role content)
        style (get-in theme [:text role])]
    (when-not (map? style)
      (schema/fail! "Unknown text role." {:key :role :value role}))
    (into style (filter (comp some? val)) (select-keys content style-keys))))
