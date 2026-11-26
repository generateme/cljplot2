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

(def ^:private text-style-card-props
  "Text style key -> its card properties (description, meanings, example): the one description of each key, taken by
  the shared group `:text-style` ([[text-style-overrides]] - label maps, the text chart, per-axis tick labels) and by a
  text role's style ([[text-style-schema]])."
  {:font {:description "The font: a bundled, installed or logical family name, or a `java.awt.Font`." :cljplot2/example "Serif"}
   :size {:description "The font size in points." :cljplot2/example 20}
   :font-style {:description "The font style." :cljplot2/example :bold
                :cljplot2/value-meanings {:plain "Upright, regular weight." :bold "Bold." :italic "Italic." :bold-italic "Bold and italic."}}
   :color {:description "The text colour." :cljplot2/example :red
           :cljplot2/value-meanings {:contrast "Black or white, whichever reads better on what is behind: the text's own `:background` box, a chart's fill under its labels, else the plot background."}
           :cljplot2/form-meanings {::schema/solid-color "This colour, e.g. `:black`."}}
   :inset {:description "The space in pixels between the text and its box's edge on every side." :cljplot2/example 8 :cljplot2/example-context {:background :yellow}}
   :align {:description "The placement `[horizontal vertical]` of the text in its box; the horizontal part also aligns its lines. Label maps take it, the text chart doesn't." :cljplot2/no-example true}
   :background {:description "The colour of a box behind the text." :cljplot2/example :yellow
                :cljplot2/value-meanings {:none "No box."}
                :cljplot2/form-meanings {::schema/solid-color "This colour, e.g. `:white`."}}
   :dollar-math {:description "Whether text between two dollar signs draws as a LaTeX formula; a dollar after a backslash stays literal."
                 :cljplot2/example false :cljplot2/example-context {:text ["$x^2$ costs $5"]}
                 :cljplot2/value-meanings {true "Formulas between dollars." false "Every dollar literal."}}
   :min-size {:description "The least font size in points a fitted text (an axis title, a strip, a text chart with `:x-span`, a builder label) shrinks to when it is too long for its room; without it the text keeps its size."
              :cljplot2/example 6}})

(def text-style-schema
  "Schema of a text role's style: `:font :size :font-style :color :inset` required, `:align`, `:background`, `:min-size` (a role's least fitted size, not a key content overrides) and `:dollar-math` optional. Its entries take the descriptions of the shared group `:text-style`, which documents them (`:cljplot2/entries-fragment`)."
  (-> [:map {:closed true :cljplot2/type-doc "a text style (:font :size :font-style :color :inset :align :background :min-size :dollar-math)"
             :cljplot2/type-name "text style" :cljplot2/entries-fragment :text-style}]
      (into (map (fn [[k s]] [k (text-style-card-props k) s])) text-style-entries)
      (conj (schema/optional :align (text-style-card-props :align) schema/align)
            (schema/optional :background (text-style-card-props :background) schema/color)
            (schema/nilable :min-size (text-style-card-props :min-size) schema/pos)
            (schema/nilable :dollar-math (text-style-card-props :dollar-math) schema/bool))))

(def text-style-overrides
  "Map entries of the text style keys content may override (all optional, shared group `:text-style`), see [[style-keys]]."
  (schema/in-fragment
   :text-style
   (conj (mapv (fn [[k s]] (schema/optional k (assoc (text-style-card-props k) :cljplot2/default-doc "The text role's value.") s))
               (conj text-style-entries [:align schema/align] [:background schema/color]))
         (schema/nilable :dollar-math (assoc (text-style-card-props :dollar-math) :cljplot2/default-doc "The text role's value.") schema/bool))))

(def text-roles
  "The text roles of the default theme: role -> `{:section s :description d :example e}` - what the role styles and who draws it (`:plot-text` - titles, strips, axes; `:legend-text`; `:hierarchy-text` - treemap, partition and pack labels; `:chart-text` - texts of other charts; `:builder-text` - labels of builders: network, sankey, dendrogram, alluvial, tree, bundle, chord, pairs) and a partial style changing it, for the role's option card. A theme may add roles of its own, used by labels' and text charts' `:role`."
  {:title {:section :plot-text :description "The style of plot and multiplot titles." :example {:size 20 :font-style :italic}}
   :strip {:section :plot-text :description "The style of facet strip texts." :example {:font-style :bold}}
   :axis-title {:section :plot-text :description "The style of axis titles." :example {:color :darkred}}
   :tick-label {:section :plot-text :description "The style of tick labels; an axis's `:tick-label` keys go over it." :example {:size 8}}
   :legend-title {:section :legend-text :description "The style of legend titles." :example {:font-style :plain}}
   :legend-label {:section :legend-text :description "The style of legend entry labels and colour bar tick labels." :example {:font-style :italic}}
   :treemap-group {:section :hierarchy-text :description "The style of treemap group headers." :example {:size 13}}
   :treemap-leaf {:section :hierarchy-text :description "The style of treemap leaf labels." :example {:font-style :italic}}
   :partition-group {:section :hierarchy-text :description "The style of partition (icicle and sunburst) group labels." :example {:size 13}}
   :partition-leaf {:section :hierarchy-text :description "The style of partition leaf labels." :example {:font-style :italic}}
   :pack-group {:section :hierarchy-text :description "The style of circle packing group labels." :example {:size 13}}
   :pack-leaf {:section :hierarchy-text :description "The style of circle packing leaf labels." :example {:font-style :italic}}
   :venn-set {:section :chart-text :description "The style of Venn diagram set names." :example {:size 14}}
   :venn-count {:section :chart-text :description "The style of Venn diagram region counts." :example {:font-style :bold}}
   :tile-value {:section :chart-text :description "The style of tile value labels." :example {:size 12}}
   :correlation {:section :chart-text :description "The style of correlation numbers; they shrink to fit their cell, down to 6 points." :example {:size 16}}
   :pairwise {:section :chart-text :description "The style of pairwise statistic numbers; they shrink to fit their cell, down to 6 points." :example {:size 16}}
   :contour-label {:section :chart-text :description "The style of contour level labels." :example {:background :none}}
   :annotation {:section :chart-text :description "The style of text charts and strip n labels." :example {:size 14 :color :darkred}}
   :bar-value {:section :chart-text :description "The style of bar value and count labels." :example {:font-style :bold}}
   :network-label {:section :builder-text :description "The style of network node labels." :example {:size 11}}
   :sankey-label {:section :builder-text :description "The style of sankey node labels." :example {:size 12 :font-style :bold}}
   :dendrogram-group-label {:section :builder-text :description "The style of dendrogram group (inner node) labels." :example {:size 11}}
   :alluvial-label {:section :builder-text :description "The style of alluvial stratum labels." :example {:font-style :bold}}
   :tree-label {:section :builder-text :description "The style of node-link tree node labels." :example {:size 11}}
   :radial-label {:section :builder-text :description "The style of leaf names around circular builders (dendrogram, tree, bundle, chord); the builder picks the size from the leaf count, between `:min-size` and `:size`." :example {:color :darkred}}
   :pairs-name {:section :builder-text :description "The style of variable names on the diagonal of a scatter-plot matrix." :example {:size 10 :color :darkred}}})

(defn- grid-line
  "Gives the schema of a grid line entry's value: a line style `{:color :line-width :dash}` (documented by the shared group `:line-style`), or `:none`.

  Parameters:

  - `form-meaning` (string): the meaning of the line style form on the entry's card (how a partial map merges).

  Returns `[props schema]` - the entry's meaning properties and its schema."
  [form-meaning]
  [{:cljplot2/value-meanings {:none "No lines."} :cljplot2/form-meanings {:map form-meaning}}
   (schema/keyword-or :none [:map {:closed true :cljplot2/type-doc "a line {:color :line-width :dash}"
                                   :cljplot2/type-name "line style" :cljplot2/entries-fragment :line-style}
                             [:color {:description "The colour of the lines."} schema/color]
                             [:line-width {:description "The width of the lines in pixels."} schema/non-neg]
                             [:dash {:description "The dash style of the lines."} schema/dash]])])

(def ^:private partial-grid-line
  "[[grid-line]] of an entry whose default is a line style: a partial map merges over it."
  (grid-line "Lines in this style; keys left out keep the default line's."))

(defn- axis-style
  "Gives the schema of one axis style (see the namespace doc, `:axis`).

  Parameters:

  - `k` (keyword): the axis, `:x` or `:y` - the examples' contexts name it.

  Returns a closed map schema."
  [k]
  [:map {:closed true}
   [:line-color {:description "The colour of the axis line." :cljplot2/example :red
                 :cljplot2/value-meanings {:none "No axis line."}}
    schema/color]
   [:line-width {:description "The width of the axis line in pixels; 0 draws none." :cljplot2/example 3} schema/non-neg]
   [:tick-color {:description "The colour of the ticks." :cljplot2/example :red
                 :cljplot2/value-meanings {:none "No ticks."}}
    schema/color]
   [:tick-length {:description "The length of the ticks in pixels." :cljplot2/example 12} schema/non-neg]
   [:tick-width {:description "The width of the ticks in pixels; 0 draws none." :cljplot2/example 3} schema/non-neg]
   [:label-gap {:description "The space in pixels between the ticks and their labels." :cljplot2/example 12} schema/non-neg]
   [:tick-label-gap {:description "The least space in pixels between neighbouring tick labels lying end to end (unrotated on a horizontal axis, turned 90 degrees on a vertical one); a numeric axis skips a label closer than this to the last one it drew."
                     :cljplot2/example 30
                     :cljplot2/example-context (when (= :y k) {:axis {:y {:label-rotation 90}}})}
    schema/non-neg]
   [:tick-spacing {:description "The distance in pixels the axis aims for between ticks; it sets how many ticks fit." :cljplot2/example 25} schema/pos]
   [:label-rotation {:description "The angle in degrees the tick labels turn clockwise, from -90 to 90." :cljplot2/example 45}
    [:and number? [:>= -90] [:<= 90]]]
   [:minor-tick-length {:description "The length of the minor ticks in pixels; a scale with `:minor-ticks` draws them." :cljplot2/example 8} schema/non-neg]
   [:tick-label {:description "Text style keys of this axis's tick labels, merged over the `:tick-label` text role; polar guides take them too, and `[:axis :x]` styles 3D frames."
                 :cljplot2/entries-fragment :text-style :cljplot2/example {:size 14 :color :darkred}}
    (into [:map {:closed true :cljplot2/type-doc "a partial text style (:font, :size, :color ...)"
                 :cljplot2/type-name "text style" :cljplot2/entries-fragment :text-style}]
          text-style-overrides)]])

(def theme-schema
  "Schema of the merged theme (see [[default-theme]]; every key has a value there - overrides are partial). `:chart` subtrees are checked against the charts' style schemas by `build`, see [[cljplot2.pipeline/build]]."
  [:map {:closed true :cljplot2/merged true}
   [:plot {:description "The canvas: its background and margin."}
    [:map {:closed true}
     [:background {:description "The colour of the canvas behind the whole plot." :cljplot2/example :white
                   :cljplot2/value-meanings {:none "No background: the canvas stays transparent."}}
      schema/color]
     [:margin {:description "The space in pixels around the plot on every side." :cljplot2/example 30} schema/non-neg]]]
   [:panel {:description "The data panels: their box and grid lines."}
    [:map {:closed true}
     [:fill {:description "The background colour of each data panel." :cljplot2/section :panel-box :cljplot2/example :white
             :cljplot2/value-meanings {:none "No fill: the plot background shows through."}}
      schema/color]
     [:alternate-fill {:description "A colour drawn over the fill of every facet panel whose row and column numbers add up to an odd number; `plot/pairs` shades its cells the same way."
                       :cljplot2/section :panel-box :cljplot2/example [0 0 0 40]
                       :cljplot2/value-meanings {:none "No shading."}}
      schema/color]
     [:border-color {:description "The colour of the border drawn inside each panel's edge." :cljplot2/section :panel-box :cljplot2/example :black
                     :cljplot2/value-meanings {:none "No border."}}
      schema/color]
     [:border-width {:description "The width of the panel border in pixels." :cljplot2/section :panel-box
                     :cljplot2/example 4 :cljplot2/example-context {:panel {:border-color :black}}}
      schema/non-neg]
     [:invalid-band-size {:description "The size in pixels across the band beside an axis where a chart with `:invalid :show` draws its invalid values; the band's label may widen it."
                          :cljplot2/section :panel-box :cljplot2/example 48}
      schema/non-neg]
     [:grid {:description "The grid lines and band shading, by kind: `:x`, `:y`, `:bands`, `:bands-fill` and `:minor`." :cljplot2/section :grid}
      (let [[meanings line] partial-grid-line
            [minor-meanings minor-line] (grid-line "Lines in this style; the default is `:none`, so give all three keys.")]
        [:map {:closed true}
         [:x (merge {:description "The grid lines at the ticks of the x scale." :cljplot2/example {:color :darkred :line-width 1}} meanings) line]
         [:y (merge {:description "The grid lines at the ticks of the y scale." :cljplot2/example :none} meanings) line]
         [:bands (merge {:description "The grid lines of a bands scale (levels on a categorical axis), drawn in place of its axis's `:x` or `:y` lines; `:none` for that axis turns them off too."
                         :cljplot2/example {:color :darkred :dash :none}}
                        meanings)
          line]
         [:bands-fill {:description "The colour of every second band of a bands scale." :cljplot2/example [0 0 255 30]
                       :cljplot2/value-meanings {:none "No band shading."}}
          schema/color]
         [:minor (merge {:description "The grid lines at the minor ticks of a scale with `:minor-ticks`."
                         :cljplot2/example {:color :gray :line-width 0.5 :dash :none}}
                        minor-meanings)
          minor-line]])]]]
   [:strip {:description "The facet strips."}
    [:map {:closed true}
     [:fill {:description "The background colour of the facet strips." :cljplot2/example :lightblue
             :cljplot2/value-meanings {:none "No strip background."}}
      schema/color]
     [:border-color {:description "The colour of the strip border." :cljplot2/example :black
                     :cljplot2/value-meanings {:none "No border."}}
      schema/color]
     [:border-width {:description "The width of the strip border in pixels." :cljplot2/example 3} schema/non-neg]
     [:shingle-fill {:description "The colour of the bar marking a shingle level's range in its strip." :cljplot2/example :lightgreen
                     :cljplot2/value-meanings {:none "No bar."}}
      schema/color]]]
   [:facet {:description "The facet layout."}
    [:map {:closed true} [:gap {:description "The space in pixels between facet panels, and between a panel and its margins or invalid-value band." :cljplot2/example 25} schema/non-neg]]]
   [:multiplot {:description "The layout of arranged plots."}
    [:map {:closed true} [:gap {:description "The space in pixels between arranged plots." :cljplot2/example 40} schema/non-neg]]]
   [:palette {:description "The default ranges of the aesthetic scales."} aesthetic/palette-schema]
   [:legend {:description "The legends and colour bars."}
    [:map {:closed true}
     [:gap {:description "The space in pixels between a legend's keys and their labels, between two legends and between the panels and the legends." :cljplot2/example 15} schema/non-neg]
     [:key-size {:description "The least side of a legend key's box in pixels; a key grows to fit a mapped size. It is also the default colour bar width." :cljplot2/example 28} schema/non-neg]
     [:fill {:description "The fill colour of the legend box." :cljplot2/example :white
             :cljplot2/value-meanings {:none "No fill."}}
      schema/color]
     [:border-color {:description "The colour of the legend box's border." :cljplot2/example :black
                     :cljplot2/value-meanings {:none "No border."}}
      schema/color]
     [:border-width {:description "The width of the legend box's border in pixels." :cljplot2/example 3
                     :cljplot2/example-context {:legend {:border-color :black}}}
      schema/non-neg]
     [:bar-length {:description "The length of a colour bar in pixels, at most the height available." :cljplot2/example 60
                   :cljplot2/value-meanings {:stretch "The height left in the bar's column of legends, at least three key sizes."}
                   :cljplot2/form-meanings {::schema/pos "This many pixels."}}
      (schema/keyword-or :stretch schema/pos)]
     [:bar-width {:description "The width of a colour bar in pixels." :cljplot2/default-doc "The legend's `:key-size`." :cljplot2/example 30}
      [:maybe schema/pos]]
     [:bar-ticks {:description "The tick count a colour bar aims for; a colour scale's `:ticks` wins, and a binned bar ticks its breaks." :cljplot2/example 2} schema/pos-int]
     [:bar-tick-length {:description "The length of a colour bar's ticks in pixels; the labels sit past them." :cljplot2/example 8} schema/non-neg]
     [:bar-tick-width {:description "The width of a colour bar's ticks in pixels; 0 draws none." :cljplot2/example 3} schema/non-neg]
     [:min-key-line-width {:description "The least width in pixels of a line in a legend key: a key draws a thinner line this wide, and a line of width 0 stays 0. 0 keeps the panel's widths."
                           :cljplot2/example 5}
      schema/non-neg]]]
   [:frame-3d {:description "The box frames of 3D charts; their axes and edges take the x axis style `[:axis :x]`."}
    [:map {:closed true}
     [:hidden-edge-dash {:description "The dash of a 3D box's hidden edges." :cljplot2/example [8 2 2 2]} schema/dash]]]
   [:chart {:description "The style of each chart kind, `[:chart kind]`, and the named styles."}
    [:and {:cljplot2/type-doc "map of chart kind to its style"} [:map-of keyword? :map]
     [:map (schema/optional :text (schema/forbidden "isn't used: text charts take the text role [:text :annotation]"))]]]
   [:axis {:description "The axes, per position scale: `:x` stays the x scale's axis when `:flip` draws it vertically."}
    [:map {:closed true}
     [:x {:description "The keys of the axis drawn for the x scale, also when `:flip` draws it vertically." :cljplot2/section :x-axis} (axis-style :x)]
     [:y {:description "The keys of the axis drawn for the y scale, also when `:flip` draws it horizontally." :cljplot2/section :y-axis} (axis-style :y)]]]
   [:text {:description "The text styles, per text role."} [:map-of keyword? text-style-schema]]])

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
