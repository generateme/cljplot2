(ns cljplot2.theme
  "Plot theme: a nested map of defaults, deep-merged with the plot's `:theme`.

  `:text` maps a role (`:title`, `:strip`, ..., `:contour-label` - contour level labels, `:annotation` - text charts and strip n labels) to a text style:

  * `:font` - font family name or `java.awt.Font`, see [[cljplot2.graphics.font/resolve-font]]
  * `:size` - points
  * `:font-style` - `:plain`, `:bold`, `:italic` or `:bold-italic`
  * `:color` - any clojure2d color
  * `:padding` - pixels added on every side of the text
  * `:align` - `[h v]` placement of the text in its box (optional, default `[:center :middle]`), see [[cljplot2.content.label]]
  * `:background` - a color (alpha allowed) filling the label's box (optional, default none; `:none` - none): for
    labels charts place at a point (text chart, tile values, ...) the text plus `:padding`; for layout labels
    (titles, axis titles, strips, legend labels) the whole item box

  `:plot` - `:background` (fills the canvas), `:margin` (pixels on every side).

  `:panel` - `:fill`, `:border-color`, `:border-width` (pixels, rounded, drawn inside the panel box),
  `:grid {:x line :y line :minor line}` - grid lines at tick positions (`:minor` - at the minor ticks of scales with
  `:minor-ticks`, default `:none`), `line` is `{:color :width :dash}` or `:none`
  (`:dash` - a sequence of positive dash lengths, a dash style name of [[cljplot2.schema/dash-styles]], or `:none`
  for a solid line).

  `:axis {:x axis :y axis}` - `axis` is `{:line-color :line-width :tick-color :tick-length :tick-width :label-gap
  :tick-spacing :label-rotation :minor-tick-length}` (pixels; `:minor-tick-length` - ticks of a scale's `:minor-ticks`; `:tick-spacing` > 0 is the target distance between ticks, a line or tick of
  width 0 isn't drawn; `:label-rotation` - tick labels rotated clockwise by -90..90 degrees, see
  [[cljplot2.content.axis]]). Keyed by scale, also when a flipped plot draws `:x` vertically.

  `:panel :grid :bands-fill` - fill of every second band of a bands scale (from the second level), behind the
  charts, so levels with dodged groups read apart (default a light shade; `:none` turns it off).

  `:panel :grid :bands` - grid line style used instead of `:x` / `:y` for a bands scale (default dotted, as lattice
  `dotplot`); `:x` / `:y` `:none` still turns that scale's lines off.

  `:panel :alternate-fill` - drawn over the fill of facet panels with odd `row + col` (`:none` turns it off).

  `:strip` - `:fill`, `:border-color`, `:border-width` of facet strips, `:shingle-fill` (bar of a shingle level); `:facet {:gap px}` between panels;
  `:multiplot {:gap px}` between arranged plots.

  `:palette` - aesthetic defaults, see [[cljplot2.scale.aesthetic]]: `:color` (palette name or colors, old cljplot
  `:category20`), `:gradient` (colors or palette name; `:k2` reversed), `:shape` (shape sequence), `:size`
  (`[min max]` pixels), `:dash` (dash sequence: the [[cljplot2.schema/dash-styles]] names, adjacent styles far
  apart), `:pattern` (pattern sequence: the [[cljplot2.schema/pattern-styles]] names without `:solid`), `:na` (color
  of NA values).

  `:legend` - `:gap` (pixels between key and label and between legends), `:key-size` (key box side, pixels),
  `:fill`, `:border-color`, `:border-width`, color bar `:bar-length` (`:stretch` - over the space left, or a number,
  clamped to it) and `:bar-width` (`nil` - `:key-size`); text roles `:legend-title` and `:legend-label`.

  `:chart` - per chart kind defaults: `:scatter`, `:histogram`, `:density`, `:line`, `:ecdf` (old cljplot blue
  `[69 117 180]` - clojure2d `(last (palette :rdylbu-9))` - with cljplot's alphas: scatter 180, stripplot 100, rug 100),
  `:treemap` (`:padding`, `:border-color`, `:border-width`), `:partition` (`:border-color`, `:border-width`), `:pack` (`:padding`, `:border-color`, `:border-width`), `:waffle` (`:gap`, `:shape`, borders), `:venn` (`:alpha`, borders), `:tile` / `:bin2d` (`:cell`, `:cell-scale`, `:stroke-color`, `:stroke-width`, `:alpha`), `:field` (`:alpha`), `:contour` (`:color`, `:width`, `:dash`, `:alpha`), `:correlation` (`:color`, `:stroke-color`, `:stroke-width`), strip charts `:box`, `:violin`, `:stripplot`, `:beeswarm`, `:dotplot`, `:rug`,
  `:extent`, `:bar`, annotations `:rule`, `:rect`, `:function`, bands `:ribbon`, `:smooth` (band `:alpha` 0.3), `:segment`, `:polygon`, probability plots `:qq` / `:pp` (scatter's point style, `:line-width`), `:image` (no style), `:candlestick` (green up, red down), `:parallel` (radar `:fill` `:alpha` 0.25); each checked against its chart's style schema; line and
  function `:bridge` - the style of bridges over gaps (`:alpha` multiplies the line alpha, optional `:width`, `:dash`);
  line `:alpha` - opacity of `:fill :color` and stacked areas.

  Colors are any clojure2d color or `:none` (draw nothing). `nil` in a theme override means \"not set\"
  and keeps the value underneath, see [[deep-merge]].

  `build` validates the merged theme against [[theme-schema]] (closed maps: unknown keys are errors), so bad values fail
  before any stage runs."
  (:require [clojure2d.color :as c]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-keys
  "Keys of a text style; content may override them inline."
  [:font :size :font-style :color :padding :align :background])

(def ^:private centered [:center :middle])

(defn none?
  "Is `color` the \"draw nothing\" value `:none`?"
  [color]
  (= :none color))

(def default-theme
  "Defaults in old cljplot style: light lavender-gray background, 10 px margin, panels without fill and border."
  {:plot {:background 0xececf3 :margin 10}
   :panel {:fill :none :alternate-fill [0 0 0 12] :border-color :none :border-width 1
           :grid {:x {:color 0xb4b4b4 :width 0.5 :dash [4.0]}
                  :y {:color 0xb4b4b4 :width 0.5 :dash [4.0]}
                  :bands {:color 0xb4b4b4 :width 1 :dash [1.0 2.0]}
                  :bands-fill [0 0 0 8]
                  :minor :none}}
   :strip {:fill [255 255 255 150] :border-color [0 0 0 100] :border-width 1 :shingle-fill [255 165 0 110]}
   :facet {:gap 5}
   :multiplot {:gap 10}
   :palette {:color :category20
             :gradient (vec (reverse (c/palette :k2)))
             :shape [\O \s \A \x \+ \V \o \S \^ \v \{ \} \< \>]
             :size [2 10]
             :dash [:solid :dashed :dotted :dot-dash :long-dash :dot-dot-dash :two-dash :loose-dot]
             :pattern [:hatch :back-hatch :horizontal :vertical :cross :diagonal-cross :dots :dense-dots]
             :na [150 150 150]}
   :legend {:gap 5 :key-size 14 :fill :none :border-color :none :border-width 1 :bar-length :stretch :bar-width nil}
   :chart {:scatter {:size 6 :color [69 117 180 180] :stroke-color :none :stroke-width 1 :shape \O}
           :cloud {:size 6 :color [69 117 180 180] :stroke-color :none :stroke-width 1 :shape \O}
           :surface {:stroke-color [60 60 60] :stroke-width 0.5}
           :histogram {:color [69 117 180] :stroke-color :darker :stroke-width 1 :alpha 0.6 :zero-line-color :black
                       :pattern :solid :pattern-color :darker
                       :zero-line-width 1}
           :bar {:color [69 117 180] :stroke-color :darker :stroke-width 1 :alpha 0.6 :zero-line-color :black
                 :pattern :solid :pattern-color :darker
                 :zero-line-width 1}
           :density {:color [69 117 180] :width 1 :dash :none :fill :none :alpha 0.6 :zero-line-color :black
                     :pattern :solid :pattern-color :darker
                     :zero-line-width 1}
           :line {:color [69 117 180] :width 1 :dash :none :fill :none :alpha 0.6 :bridge {:alpha 0.3}
                  :pattern :solid :pattern-color :darker}
           :ecdf {:color [69 117 180] :width 1 :dash :none}
           :treemap {:padding 3 :border-color :white :border-width 1
                     :pattern :solid :pattern-color :darker}
           :partition {:border-color :white :border-width 1 :pattern :solid :pattern-color :darker}
           :pack {:padding 3 :border-color :white :border-width 1 :pattern :solid :pattern-color :darker}
           :waffle {:gap 0.1 :shape :square :border-color :none :border-width 1 :pattern :solid :pattern-color :darker}
           :venn {:alpha 0.4 :border-color :darker :border-width 1}
           :tile {:cell :square :cell-scale 1.0 :stroke-color :none :stroke-width 1 :alpha 1.0}
           :bin2d {:cell :pointy-hex :cell-scale 1.0 :stroke-color :none :stroke-width 1 :alpha 1.0}
           :field {:alpha 1.0}
           :contour {:color [69 117 180] :width 1 :dash :none :alpha 1.0}
           :correlation {:color [69 117 180] :stroke-color :darker :stroke-width 1}
           :box {:color [69 117 180] :stroke-color :darker :stroke-width 1 :shape \O :size 5
                 :pattern :solid :pattern-color :darker}
           :violin {:color [69 117 180] :stroke-color :darker :stroke-width 1 :inner-color :none
                    :pattern :solid :pattern-color :darker}
           :stripplot {:size 6 :color [69 117 180 100] :stroke-color :none :stroke-width 1 :shape \O}
           :beeswarm {:size 6 :color [69 117 180 180] :stroke-color :none :stroke-width 1 :shape \O}
           :dotplot {:color [69 117 180] :stroke-color :none :stroke-width 1 :shape \O}
           :rug {:color [69 117 180 100] :line-width 1}
           :bullet {:range-colors [[204 204 204] [238 238 238]] :measure-colors [[70 130 180] [176 196 222]]
                    :measure-width 0.35 :marker-color :black :marker-width 2 :marker-length 0.7}
           :extent {:color [69 117 180] :line-width 2 :shape \O :size 7}
           :rule {:color [60 60 60] :width 1 :dash :none}
           :function {:color [69 117 180] :width 1 :dash :none :fill :none :bridge {:alpha 0.3}}
           :ribbon {:color [69 117 180] :fill :color :alpha 0.3 :stroke-color :none :stroke-width 1 :dash :none
                    :pattern :solid :pattern-color :darker}
           :smooth {:color [69 117 180] :width 1 :dash :none :fill :color :alpha 0.3
                    :pattern :solid :pattern-color :darker}
           :segment {:color [69 117 180] :width 1 :dash :none}
           :polygon {:color [69 117 180] :alpha 1.0 :stroke-color :none :stroke-width 1}
           :qq {:size 6 :color [69 117 180 180] :stroke-color :none :stroke-width 1 :shape \O :line-width 1}
           :image {}
           :pp {:size 6 :color [69 117 180 180] :stroke-color :none :stroke-width 1 :shape \O :line-width 1}
           :parallel {:color [69 117 180 140] :width 1 :dash :none :fill :none :alpha 0.25
                      :pattern :solid :pattern-color :darker}
           :candlestick {:up-color [26 152 80] :down-color [215 48 39] :body-width 0.7 :wick-width 1 :stroke-color :none
                         :up-pattern :solid :down-pattern :solid :pattern-color :darker}
           :rect {:fill [0 0 0 30] :stroke-color :none :stroke-width 1
                  :pattern :solid :pattern-color :darker}}
   :axis {:x {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 80 :label-rotation 0 :minor-tick-length 3}
          :y {:line-color :black :line-width 1 :tick-color :black :tick-length 6 :tick-width 1
              :label-gap 2 :tick-spacing 50 :label-rotation 0 :minor-tick-length 3}}
   :text {:title {:font "DejaVu Sans Mono" :size 14 :font-style :bold :color :black :padding 4 :align centered}
          :strip {:font "DejaVu Sans Mono" :size 11 :font-style :plain :color :black :padding 2 :align centered}
          :axis-title {:font "DejaVu Sans Mono" :size 12 :font-style :plain :color :black :padding 4 :align centered}
          :tick-label {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}
          :legend-title {:font "DejaVu Sans Mono" :size 11 :font-style :bold :color :black :padding 2 :align [:left :middle]}
          :legend-label {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :padding 2 :align [:left :middle]}
          :treemap-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :black :padding 2 :align [:left :middle]}
          :treemap-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :padding 2 :align centered}
          :partition-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :black :padding 2 :align centered}
          :partition-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :padding 2 :align centered}
          :pack-group {:font "DejaVu Sans Mono" :size 10 :font-style :bold :color :black :padding 2 :align centered}
          :pack-leaf {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :padding 2 :align centered}
          :venn-set {:font "DejaVu Sans Mono" :size 11 :font-style :bold :color :black :padding 2 :align centered}
          :venn-count {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}
          :tile-value {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :padding 1 :align centered}
          :correlation {:font "DejaVu Sans Mono" :size 12 :font-style :plain :color :black :padding 1 :align centered}
          :contour-label {:font "DejaVu Sans Mono" :size 9 :font-style :plain :color :black :padding 1 :align centered
                          :background [255 255 255 180]}
          :annotation {:font "DejaVu Sans Mono" :size 10 :font-style :plain :color :black :padding 2 :align centered}}})

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

(def ^:private text-style-descriptions
  {:font "font name (bundled, installed or logical) or a java.awt.Font"
   :size "font size in points"
   :font-style "font style"
   :color "text colour"
   :padding "space around the text in pixels"
   :align "alignment [horizontal vertical] of the text at its anchor"
   :background "box colour behind the text (:none - no box)"})

(def text-style-schema
  "A text role's style."
  (-> [:map {:closed true :cljplot2/type-doc "a text style (:font :size :font-style :color :padding :align :background)"}]
      (into (map (fn [[k s]] [k {:description (text-style-descriptions k)} s])) text-style-entries)
      (conj (schema/optional :align {:description (text-style-descriptions :align)} schema/align)
            (schema/optional :background {:description (text-style-descriptions :background)} schema/color))))

(def text-style-overrides
  "Map entries of the style keys content may override (all optional), see [[style-keys]]."
  (schema/in-fragment
   :text-style
   (mapv (fn [[k s]] (schema/optional k {:description (text-style-descriptions k)} s))
         (conj text-style-entries [:align schema/align] [:background schema/color]))))

(def text-roles
  "What each text role of the default theme styles: role -> description (a theme may add roles of its own, used by
  labels' `:role`)."
  {:title "plot and multiplot titles"
   :strip "facet strip texts"
   :axis-title "axis titles"
   :tick-label "tick labels"
   :legend-title "legend titles"
   :legend-label "legend entry labels and colour bar ticks"
   :treemap-group "treemap group headers"
   :treemap-leaf "treemap leaf labels"
   :partition-group "partition (icicle, sunburst) group labels"
   :partition-leaf "partition leaf labels"
   :pack-group "circle packing group labels"
   :pack-leaf "circle packing leaf labels"
   :venn-set "Venn diagram set names"
   :venn-count "Venn diagram region counts"
   :tile-value "tile value labels"
   :correlation "correlation numbers"
   :contour-label "contour level labels"
   :annotation "text charts and strip n labels"})

(def ^:private grid-line
  (schema/keyword-or :none [:map {:closed true :cljplot2/type-doc "a line {:color :width :dash}"}
                            [:color {:description "line colour"} schema/color]
                            [:width {:description "line width in pixels"} schema/non-neg]
                            [:dash {:description "dash style (:none - solid)"} schema/dash]]))

(def ^:private axis-style
  [:map {:closed true}
   [:line-color {:description "axis line colour"} schema/color]
   [:line-width {:description "axis line width in pixels (0 - not drawn)"} schema/non-neg]
   [:tick-color {:description "tick colour"} schema/color]
   [:tick-length {:description "tick length in pixels"} schema/non-neg]
   [:tick-width {:description "tick width in pixels (0 - not drawn)"} schema/non-neg]
   [:label-gap {:description "gap between ticks and tick labels in pixels"} schema/non-neg]
   [:tick-spacing {:description "target distance between ticks in pixels (sets the tick count)"} schema/pos]
   [:label-rotation {:description "tick labels rotated clockwise, degrees"} [:and number? [:>= -90] [:<= 90]]]
   [:minor-tick-length {:description "minor tick length in pixels"} schema/non-neg]])

(def theme-schema
  "The merged theme (see [[default-theme]]; every key has a value there - overrides are partial). `:chart` subtrees
  are checked against the charts' style schemas by `build`, see [[cljplot2.pipeline/build]]."
  [:map {:closed true}
   [:plot {:description "the whole canvas"}
    [:map {:closed true}
     [:background {:description "canvas background colour"} schema/color]
     [:margin {:description "pixels around the plot on every side"} schema/non-neg]]]
   [:panel {:description "the data panels"}
    [:map {:closed true}
     [:fill {:description "panel background colour"} schema/color]
     [:alternate-fill {:description "drawn over odd facet panels (row + column odd); :none - off"} schema/color]
     [:border-color {:description "panel border colour"} schema/color]
     [:border-width {:description "panel border width in pixels (drawn inside the panel)"} schema/non-neg]
     [:grid {:description "grid lines at the ticks"}
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
     [:bar-width {:description "colour bar width in pixels (nil - the key size)"} [:maybe schema/pos]]]]
   [:chart {:description "per chart kind styles, see each chart's style options"}
    [:and {:cljplot2/type-doc "map of chart kind to its style"} [:map-of keyword? :map]
     [:map (schema/optional :text (schema/forbidden "isn't used: text charts take the text role [:text :annotation]"))]]]
   [:axis {:description "axes per scale (kept by scale also when a flipped plot draws :x vertically)"}
    [:map {:closed true} [:x {:description "x axis style"} axis-style] [:y {:description "y axis style"} axis-style]]]
   [:text {:description "text styles by role (see text-roles)"} [:map-of keyword? text-style-schema]]])

(defn plot-theme
  "Plot theme: `theme` deep-merged over [[default-theme]] (`nil` means the default theme); validated against
  [[theme-schema]] by `build`."
  [theme]
  (deep-merge default-theme theme))

(def ^:private last-validated
  "`[theme errors]` of the last [[errors]] call."
  (atom nil))

(defn errors
  "Errors of the merged `theme` against [[theme-schema]] (paths under `[:theme]`, see [[cljplot2.schema/errors]]). Checking
  resolves every color and font, so the result for the last theme is reused for an equal one (plots built in a row
  usually share it)."
  [theme]
  (let [[validated errs] @last-validated]
    (if (and validated (= validated theme))
      errs
      (let [errs (schema/errors theme-schema theme [:theme])]
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
