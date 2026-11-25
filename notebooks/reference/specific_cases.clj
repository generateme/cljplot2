;; # Specific cases

;; The functions behind some charts are ordinary public functions in internal namespaces: probability plot pairs,
;; kernel density curves, smoothing fits, dot plot bins, circle packing, treemap rectangles and Venn circles. They
;; are not part of the documented API - no option tables, no validation, and they may change - but they return
;; plain data, so a computation a chart doesn't offer can start from them and draw with ordinary charts. Each
;; section shows the data and a chart drawn from it; then the lists of fastmath distributions and kernels the charts
;; accept, a few recipes and links to the use cases and galleries.

(ns reference.specific-cases
  (:require [fastmath.core :as m]
            [fastmath.kernel :as kernel]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.dots :as dots]
            [cljplot2.data.kde :as kde]
            [cljplot2.data.probability :as probability]
            [cljplot2.data.smooth :as smooth]
            [cljplot2.layout.pack :as pack]
            [cljplot2.layout.treemap :as treemap]
            [cljplot2.layout.venn :as venn]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

(def sample
  "40 values from a seeded exponential distribution - a skewed sample."
  (let [d (r/distribution :exponential {:rate 0.5 :rng (r/rng :mersenne 11)})]
    (vec (repeatedly 40 #(r/sample d)))))

(nu/data-table sample)

;; ## cljplot2.data.probability

;; Q-Q and P-P statistics (the `qq` and `pp` charts). The plotting positions `(i - 0.5) / n` of a data side:

(probability/plotting-positions 5)

;; The Q-Q pairs of a normal distribution (x side) and the sample (y side) - the normal quantiles at the sample's
;; plotting positions against its sorted values - and the quartile line through the first and third quartile
;; pairs, `[slope intercept]`:

(def normal-side {:distribution (probability/distribution [:normal])})

(def qq-pairs (probability/quantile-pairs normal-side {:values sample} 100))

(nu/data-table qq-pairs)

(probability/quartile-line normal-side {:values sample})

;; The pairs drawn by hand - points and the quartile line (a [rule](reference.charts_annotations.html#rule) by
;; `:slope` and `:intercept`) - match the [qq](reference.charts_one_variable.html#qq) chart; the skewed sample
;; bends away from the line at both ends:

(plot/render-image (let [[slope intercept] (probability/quartile-line normal-side {:values sample})]
                     (plot/arrange [{:data qq-pairs
                                     :mapping {:x :x :y :y}
                                     :charts [(chart/scatter) (chart/rule {:slope slope :intercept intercept})]
                                     :labels {:title "quantile-pairs, quartile-line" :x "normal" :y "sample"}}
                                    {:data {:v sample}
                                     :mapping {:y :v}
                                     :charts [(chart/qq {:x-distribution [:normal]})]
                                     :labels {:title "the qq chart" :x "normal" :y "sample"}}]
                                   {:columns 2}))
                   {:width 800 :height 320})

;; ### Distributions

;; Any fastmath distribution (`fastmath.random/distributions-list`) is a side of a `qq` / `pp` chart, as `[:name]` or
;; `[:name params]`:

(count r/distributions-list)

(sort r/distributions-list)

;; The sample against three of them - the exponential (the sample's own distribution) comes closest to a straight
;; line; 40 values leave its upper tail scattered:

(plot/render-image (plot/arrange [{:data {:v sample} :mapping {:y :v}
                                   :charts [(chart/qq {:x-distribution [:normal]})]
                                   :labels {:title "[:normal]"}}
                                  {:data {:v sample} :mapping {:y :v}
                                   :charts [(chart/qq {:x-distribution [:uniform-real {:lower 0 :upper 1}]})]
                                   :labels {:title "[:uniform-real {:lower 0 :upper 1}]"}}
                                  {:data {:v sample} :mapping {:y :v}
                                   :charts [(chart/qq {:x-distribution [:exponential {:rate 0.5}]})]
                                   :labels {:title "[:exponential {:rate 0.5}]"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; ## cljplot2.data.kde

;; Kernel density estimates (the `density`, `violin` charts): `estimate` gives the density function and the span the
;; curve covers (`:cut` bandwidths past the data), `curve` evaluates it at given points:

(def eruption-kde (kde/estimate (map :eruptions faithful) {:kernel :epanechnikov :bandwidth 0.3}))

(dissoc eruption-kde :kde)

(def eruption-curve (kde/curve eruption-kde (kde/grid-points (:lo eruption-kde) (:hi eruption-kde) 200)))

(nu/data-table eruption-curve)

;; The curve as a filled line, the data as a rug:

(plot/render-image {:data eruption-curve
                    :mapping {:x :x :y :y}
                    :charts [(chart/line {:fill :color})
                             (chart/rug {:data faithful :mapping {:x :eruptions :y nil}})]
                    :labels {:x "eruptions [min]" :y "density"}}
                   {:width 600 :height 300})

;; ### Kernels

;; The kernels `density` and `violin` take as `:kernel` (`(:kde fastmath.kernel/kernel-list)`):

(sort (:kde kernel/kernel-list))

;; Four of them with the same `:bandwidth` ([density](reference.charts_one_variable.html#density)) - the
;; curves differ in smoothness: the gaussian is the smoothest here, the uniform kernel's box shape makes its curve
;; ragged:

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions}
                                   :charts [(chart/density {:kernel :gaussian :bandwidth 0.4})]
                                   :labels {:title ":gaussian"}}
                                  {:data faithful :mapping {:x :eruptions}
                                   :charts [(chart/density {:kernel :epanechnikov :bandwidth 0.4})]
                                   :labels {:title ":epanechnikov"}}
                                  {:data faithful :mapping {:x :eruptions}
                                   :charts [(chart/density {:kernel :uniform :bandwidth 0.4})]
                                   :labels {:title ":uniform"}}
                                  {:data faithful :mapping {:x :eruptions}
                                   :charts [(chart/density {:kernel :triangular :bandwidth 0.4})]
                                   :labels {:title ":triangular"}}]
                                 {:columns 4})
                   {:width 1000 :height 260})

;; ## cljplot2.data.smooth

;; The fits of the `smooth` chart: `fit-curve` fits ys on xs by a method and evaluates the fit and its bands at the
;; given points - `{:fit [..] :bands [{:level :lo :hi}]}`:

(def eruption-xs (mapv :eruptions faithful))

(nu/data-table eruption-xs)

(def waiting-ys (mapv :waiting faithful))

(nu/data-table waiting-ys)

(def fit-xs (vec (range 1.6 5.2 0.1)))

(nu/data-table fit-xs)

(def quadratic (smooth/fit-curve {:method :lm :params {:degree 2} :band :prediction :levels [0.5 0.95]}
                                 eruption-xs waiting-ys fit-xs))

(nu/data-table {:x fit-xs :fit (:fit quadratic)
                :lo-95 (:lo (first (:bands quadratic))) :hi-95 (:hi (first (:bands quadratic)))})

;; Drawn as ribbons (the 95% and 50% prediction bands) under the fitted line and the points:

(plot/render-image (let [[wide narrow] (:bands quadratic)]
                     {:charts [(chart/ribbon {:data {:x fit-xs :ymin (:lo wide) :ymax (:hi wide)} :mapping {:x :x :ymin :ymin :ymax :ymax}
                                              :alpha 0.2})
                               (chart/ribbon {:data {:x fit-xs :ymin (:lo narrow) :ymax (:hi narrow)} :mapping {:x :x :ymin :ymin :ymax :ymax}
                                              :alpha 0.35})
                               (chart/scatter {:data faithful :mapping {:x :eruptions :y :waiting} :size 4})
                               (chart/line {:data {:x fit-xs :y (:fit quadratic)} :mapping {:x :x :y :y} :line-width 2})]
                      :labels {:x "eruptions [min]" :y "waiting [min]"}})
                   {:width 600 :height 350})

;; ## cljplot2.data.dots

;; Dot plot bins (the `dotplot` chart) of sorted values - here the first 40 waiting times: Wilkinson's dot density
;; bins - each starts at the first value not yet binned and is centred on its values - or fixed bins from an origin:

(def waits (sort (map :waiting (take 40 faithful))))

(nu/data-table waits)

(dots/dot-density-bins waits 4.0)

(dots/fixed-bins waits 4.0 40.0)

;; The dot density bins stacked by hand - one point per value, centred on its bin:

(def dot-rows (for [{:keys [center values]} (dots/dot-density-bins waits 4.0)
                    i (range (count values))]
                {:x center :y (inc i)}))

(nu/data-table dot-rows)

(plot/render-image {:data dot-rows
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter {:size 14})]
                    :labels {:x "waiting [min]" :y "count"}}
                   {:width 600 :height 280})

;; ## cljplot2.layout.pack

;; Circle packing (the `pack` chart): `pack` places a tree of values in a pixel box - every node gets `:x :y :r`;
;; `pack-siblings` gives the centres of circles of the given radii packed side by side, and `enclose` finds the
;; smallest circle around circles:

(def packed (pack/pack {:value 20 :children [{:value 8} {:value 5} {:value 4} {:value 2} {:value 1}]}
                       {:x 0 :y 0 :w 100 :h 100} 2))

(nu/data-table (:children packed))

(pack/pack-siblings [3 2 2 1])

(pack/enclose [{:x 0 :y 0 :r 1} {:x 3 :y 0 :r 1}])

;; The packed circles drawn as [polygons](reference.charts_points_lines.html#polygon) outlined in their own colour
;; (`:stroke-color :color`) - the root's circle in grey, the children in colour:

(def circle-outlines (for [[i {:keys [x y r]}] (map-indexed vector (cons packed (:children packed)))
                           a (range 0 360 2)]
                       {:circle (if (zero? i) "root" (str "value " (:value (nth (:children packed) (dec i)))))
                        :x (m/+ x (m/* r (m/cos (m/radians a))))
                        :y (m/+ y (m/* r (m/sin (m/radians a))))}))

(nu/data-table circle-outlines)

(plot/render-image {:data circle-outlines
                    :mapping {:x :x :y :y :color :circle}
                    :charts [(chart/polygon {:alpha 0.6 :stroke-color :color})]
                    :scales {:color {:domain ["root" "value 8" "value 5" "value 4" "value 2" "value 1"]
                                     :range [[200 200 200] [31 119 180] [255 127 14] [44 160 44] [214 39 40] [148 103 189]]}}}
                   {:width 560 :height 450})

;; ## cljplot2.layout.treemap

;; Treemap rectangles (the `treemap` chart) of values in a box: squarified (near-square rectangles in the given
;; order, best with values in decreasing order, as here) or slice-and-dice (strips along x at even depths, along y at
;; odd ones):

(def shares [6 3 2 1 1])

(nu/data-table (treemap/squarify shares {:x 0 :y 0 :w 100 :h 60}))

(nu/data-table (treemap/slice-dice shares {:x 0 :y 0 :w 100 :h 60} 0))

;; Both layouts drawn with the [rect](reference.charts_annotations.html#rect) chart (an annotation chart: its own data
;; and mapping), outlined in white:

(plot/render-image (plot/arrange (for [[title rects] [["squarify" (treemap/squarify shares {:x 0 :y 0 :w 100 :h 60})]
                                                      ["slice-dice, depth 0" (treemap/slice-dice shares {:x 0 :y 0 :w 100 :h 60} 0)]]]
                                   {:charts [(chart/rect {:data (map-indexed (fn [i {:keys [x y w h]}]
                                                                               {:share (str (nth shares i) " (" i ")")
                                                                                :x0 x :x1 (+ x w) :y0 y :y1 (+ y h)})
                                                                             rects)
                                                          :mapping {:x0 :x0 :x1 :x1 :y0 :y0 :y1 :y1 :color :share}
                                                          :stroke-color :white :stroke-width 2})]
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 2})
                   {:width 900 :height 320})

;; ## cljplot2.layout.venn

;; Venn circles (the `venn` chart): two sets with areas (and the overlap's lens) proportional to their sizes, the
;; classic three-set layout, a label point per region, and the lens area of two circles:

(def two-circles (venn/two-sets 10 6 3 true))

(nu/data-table two-circles)

(venn/region-points two-circles)

(venn/three-sets)

(venn/lens-area 1.0 1.0 1.0)

;; The two proportional circles drawn as polygons, each region labelled at its point:

(def venn-outlines (for [[i {:keys [x y r]}] (map-indexed vector two-circles)
                         a (range 0 360 2)]
                     {:set (nth ["A (10)" "B (6)"] i)
                      :x (m/+ x (m/* r (m/cos (m/radians a))))
                      :y (m/+ y (m/* r (m/sin (m/radians a))))}))

(nu/data-table venn-outlines)

(def venn-labels (for [[region [x y]] (venn/region-points two-circles)]
                   {:x x :y y :text (case region #{0} "A only" #{1} "B only" "both (3)")}))

(nu/data-table venn-labels)

(plot/render-image {:data venn-outlines
                    :mapping {:x :x :y :y :color :set}
                    :charts [(chart/polygon {:alpha 0.4 :stroke-color :color})
                             (chart/text {:data venn-labels :mapping {:x :x :y :y :label :text}})]
                    :scales {:x {:domain [-2 3.2]} :y {:domain [-2.2 2.2]}}}
                   {:width 560 :height 380})

;; ## Recipes

;; Short compositions of the pieces documented in this part.

;; A reference curve over a histogram: the [histogram](reference.charts_one_variable.html#histogram) on the
;; density scale (`:y :density`) and the normal density with the data's mean and standard deviation as a
;; [function](reference.charts_annotations.html#function) chart:

(plot/render-image (let [xs (map :waiting faithful)
                         normal (r/distribution :normal {:mu (stats/mean xs) :sd (stats/stddev xs)})]
                     {:data faithful
                      :mapping {:x :waiting}
                      :charts [(chart/histogram {:y :density :bins 25})
                               (chart/function {:f #(r/pdf normal %) :domain [40 100] :color [200 50 50] :line-width 2})]
                      :labels {:x "waiting [min]" :title "a normal curve fails a bimodal sample"}})
                   {:width 600 :height 320})

;; A highlighted interval with an explanation: a band ([rect](reference.charts_annotations.html#rect) over an x
;; interval), a threshold ([rule](reference.charts_annotations.html#rule)) and a note
;; ([text](reference.charts_annotations.html#text)) layered over a scatter with a loess
;; [smooth](reference.charts_fitted_curves.html#smoothing):

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/rect {:x [2.5 3.5] :fill [255 200 0 60]})
                             (chart/scatter {:size 4})
                             (chart/smooth {:method :loess})
                             (chart/rule {:y 68 :dash :dashed})
                             (chart/text {:x 3.0 :y 95 :text "few eruptions of 2.5-3.5 min"})]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 320})

;; Small multiples built in a loop - one plot per group with a shared domain, arranged in a row:

(plot/render-image (plot/arrange (for [[label pred] [["short" #(< (:eruptions %) 3)] ["long" #(>= (:eruptions %) 3)]]]
                                   {:data (filter pred faithful)
                                    :mapping {:x :waiting}
                                    :charts [(chart/density {:fill :color})]
                                    :scales {:x {:domain [40 100]}}
                                    :labels {:title (str label " eruptions")}})
                                 {:columns 2})
                   {:width 800 :height 260})

;; Rendering to a file - `plot/save` at the end of a `render-image` pipeline - is shown in
;; [Render - Saving](reference.render.html#saving).

;; ## Links

;; The use cases show the charts at work, one family per chapter: [distribution](use_cases.distribution.html),
;; [correlation](use_cases.correlation.html), [ranking](use_cases.ranking.html),
;; [part of a whole](use_cases.part_of_a_whole.html), [evolution](use_cases.evolution.html),
;; [flow](use_cases.flow.html) and [colours](use_cases.colors.html). The galleries reproduce other libraries' examples:
;; [lattice](galleries.lattice.html), [Vega-Lite](galleries.vega.html) and [cljplot](galleries.cljplot.html).
