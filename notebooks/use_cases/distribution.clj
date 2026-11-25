;; # Distribution

;; Charts of how the values of one numeric variable spread: violins, densities, histograms, boxplots and
;; ridgelines (the [R Graph Gallery](https://r-graph-gallery.com/) distribution family), then beeswarms, strip
;; plots, rugs and ECDFs.
;;
;; Distribution charts put the values on `:x` and the groups (if any) on `:y`, so they are horizontal by default.
;; `:flip true` draws the same plot vertically.

(ns use-cases.distribution
  (:require [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [notebook-utils :as nu]))

(def iris (nu/dataset :iris))

(nu/data-table iris)

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

(def barley (nu/dataset :barley))

(nu/data-table barley)

(def chem97 (nu/dataset :chem97))

(nu/data-table chem97)

(def diamonds (nu/dataset :diamonds))

(nu/data-table diamonds)

(def lincoln (nu/dataset :lincoln-weather))

(nu/data-table lincoln)

;; ## Violin

;; A violin is a kernel density estimate mirrored around the centre line of its group, here with a small box
;; inside (the default `:inner`).

;; ### Basic violin

(nu/source-note {:source "R Graph Gallery - Violin plot"
                 :url "https://r-graph-gallery.com/95-violin-plot-with-ggplot2.html"
                 :dataset :iris})

(plot/render-image
 {:data iris
  :mapping {:x :sepal-length :y :species}
  :charts [(chart/violin)]
  :labels {:title "Sepal length by species" :x "sepal length [cm]" :y nil}}
 {:width 800 :height 450})

;; ### Ordered violins

;; [`levels/order-levels`](reference.data_levels.html#levelsorder-levels) orders the levels by any statistic of a
;; value; the result is the scale's `:domain`, its first level at the bottom (y grows upward) - ascending medians put
;; the largest on top (setosa, first in the data and at the bottom by default).

(nu/source-note {:source "R Graph Gallery - Violin plot (reorder)"
                 :url "https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html"
                 :dataset :iris})

(plot/render-image
 {:data iris
  :mapping {:x :sepal-width :y :species}
  :scales {:y {:domain (levels/order-levels iris :species :sepal-width stats/median)}}
  :charts [(chart/violin {:inner :quartiles})]
  :labels {:title "Sepal width, species by median (largest on top)" :x "sepal width [cm]" :y nil}}
 {:width 800 :height 450})

;; ### Horizontal and vertical

;; The R Graph Gallery draws violins and boxplots vertically; here that is `:flip true` on the same plot - values
;; stay on `:x`, which is now drawn vertically.

(nu/source-note {:source "R Graph Gallery - Horizontal violin plot"
                 :url "https://r-graph-gallery.com/violin_horizontal_ggplot2.html"
                 :dataset :iris})

(let [p {:data iris
         :mapping {:x :sepal-width :y :species}
         :charts [(chart/violin)]
         :labels {:x "sepal width [cm]" :y nil}}]
  (plot/render-image
   (plot/arrange [(assoc-in p [:labels :title] "horizontal (default)")
                  (-> p (assoc :flip true) (assoc-in [:labels :title] ":flip true"))]
                 {:columns 2})
   {:width 900 :height 450}))

;; ### Violin with a box and sample sizes

;; Two strip charts share the bands: a violin without inner marks and a narrow box. `:n-label` writes each
;; group's count past its largest value.

(nu/source-note {:source "R Graph Gallery - Violin with boxplot and sample size"
                 :url "https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html"
                 :dataset :barley})

(plot/render-image
 {:data barley
  :mapping {:x :yield :y :site}
  :charts [(chart/violin {:inner :none :color [200 200 220]})
           (chart/box {:width 0.12 :outliers false :n-label true})]
  :labels {:title "Barley yield by site" :x "yield [bushels/acre]" :y nil}}
 {:width 800 :height 500})

;; ### Grouped violins

;; A second grouping (here the year, coloured) dodges the violins inside each band, in legend order; with exactly
;; two groups `:position :mirror` draws split violins, one half per group. The year is a number, so
;; `:scales {:color {:type :categorical}}` declares it categorical: one colour per year instead of a gradient.

(nu/source-note {:source "R Graph Gallery - Grouped violin plot"
                 :url "https://r-graph-gallery.com/violin_grouped_ggplot2.html"
                 :dataset :barley})

(let [p {:data barley
         :mapping {:x :yield :y :site :color :year}
         :scales {:color {:type :categorical}}
         :labels {:x "yield [bushels/acre]" :y nil}}]
  (plot/render-image
   (plot/arrange [(assoc p :charts [(chart/violin {:inner :none})] :labels {:title "dodged" :x "yield" :y nil})
                  (assoc p :charts [(chart/violin {:position :mirror :inner :quartiles})]
                         :labels {:title "split (:position :mirror)" :x "yield" :y nil})]
                 {:columns 2})
   {:width 1000 :height 550}))

;; ### Inner marks

;; `:inner` chooses what a violin shows inside: a box (default), quartile lines, a rug or the points. The data is a
;; fixed 5% sample of chem97 (31022 rows), so the rug and the points stay readable.

(nu/source-note {:source "R Graph Gallery - Violin plot; seaborn violinplot"
                 :url ["https://r-graph-gallery.com/violin.html" "https://seaborn.pydata.org/generated/seaborn.violinplot.html"]
                 :dataset :chem97})

(def chem97-sample
  "A fixed 5% sample of chem97 (the full data has 31022 rows)."
  (let [rng (r/rng :mersenne 3)] (filterv (fn [_] (< (r/drandom rng) 0.05)) chem97)))

(nu/data-table chem97-sample)

(plot/render-image
 (plot/arrange (for [inner [:box :quartiles :rug :points]]
                 {:data chem97-sample
                  :mapping {:x :gcsescore :y :score}
                  :scales {:y {:type :categorical}}
                  :charts [(chart/violin {:inner inner})]
                  :labels {:title (str ":inner " inner) :x "GCSE score"}})
               {:columns 2})
 {:width 1000 :height 800})

;; ## Density

;; A kernel density estimate (fastmath) of the values, per panel and group.

;; ### Basic density

;; `:bandwidth` is a number or a fastmath rule (`:nrd0` by default, as R); `:kernel` any fastmath kernel.

(nu/source-note {:source "R Graph Gallery - Density plot"
                 :url "https://r-graph-gallery.com/21-distribution-plot-using-ggplot2.html"
                 :dataset :faithful})

(plot/render-image
 (plot/arrange (for [[title opts] [["default (:nrd0)" {}]
                                   [":bandwidth 0.1" {:bandwidth 0.1}]
                                   [":bandwidth 0.6" {:bandwidth 0.6}]
                                   [":kernel :epanechnikov" {:kernel :epanechnikov}]]]
                 {:data faithful
                  :mapping {:x :eruptions}
                  :charts [(chart/density (assoc opts :fill [0 114 178 60]))]
                  :labels {:title title :x "eruption time [min]"}})
               {:columns 2})
 {:width 900 :height 600})

;; ### Mirror density

;; Two groups, the second drawn downward, with the y = 0 line (`:zero-line`, on by default). `:symmetric` puts
;; both halves on one range so they compare by height.

(nu/source-note {:source "R Graph Gallery - Mirror density chart"
                 :url "https://r-graph-gallery.com/density_mirror_ggplot2.html"
                 :dataset :chem97})

(plot/render-image
 (plot/arrange [{:data chem97
                 :mapping {:x :gcsescore :color :gender}
                 :charts [(chart/density {:position :mirror :fill :color})]
                 :labels {:title "GCSE score: F up, M down" :x "GCSE score"}}
                {:data chem97
                 :mapping {:x :age :color :gender}
                 :charts [(chart/density {:position :mirror :symmetric true})]
                 :labels {:title "age, :symmetric" :x "age [months, centred]"}}]
               {:columns 2})
 {:width 1000 :height 450})

;; ### Several groups

;; Groups from a colour mapping, overlaid. The line takes the group colour; `:fill :color` fills each area with
;; its group colour at opacity `:alpha`, a fixed `:fill` colour fills every area alike.

(nu/source-note {:source "R Graph Gallery - Density with multiple groups"
                 :url "https://r-graph-gallery.com/density-plot.html"
                 :dataset :iris})

(plot/render-image
 (plot/arrange [{:data iris
                 :mapping {:x :sepal-length :color :species}
                 :charts [(chart/density {:line-width 2})]
                 :labels {:title "lines" :x "sepal length [cm]"}}
                {:data iris
                 :mapping {:x :sepal-length :color :species}
                 :charts [(chart/density {:fill :color :alpha 0.4})]
                 :labels {:title ":fill :color, :alpha 0.4" :x "sepal length [cm]"}}
                {:data iris
                 :mapping {:x :petal-length :color :species}
                 :charts [(chart/density {:fill [0 0 0 30]})]
                 :labels {:title ":fill [0 0 0 30]" :x "petal length [cm]"}}]
               {:columns 3})
 {:width 1000 :height 400})

;; ### Annotated density

;; Annotation charts take literal positions: a rule at the mean, a text label and a band (`chart/rect`) over the
;; central 50% of the values.

(nu/source-note {:source "R Graph Gallery - Density chart with annotation"
                 :url "https://r-graph-gallery.com/density-plot.html"
                 :dataset :faithful})

(let [xs (map :eruptions faithful)
      mean (stats/mean xs)
      [q1 q3] (stats/quantiles xs [0.25 0.75])]
  (plot/render-image
   {:data faithful
    :mapping {:x :eruptions}
    :charts [(chart/rect {:x [q1 q3] :fill [0 114 178 40]})
             (chart/density)
             (chart/rule {:x mean :dash [5.0 3.0] :color :red})
             (chart/text {:x mean :y [:panel 0.95] :text (format "mean %.2f" mean) :anchor [:left :top]
                          :offset [4 0] :color :red})
             (chart/text {:x (* 0.5 (+ q1 q3)) :y [:panel 0.05] :text "IQR" :anchor [:center :bottom]})]
    :labels {:title "Old Faithful eruption times" :x "eruption time [min]"}}
   {:width 800 :height 450}))

;; ### Small multiples

;; A facet mapping gives one panel per level; the colour still groups inside each panel.

(nu/source-note {:source "R Graph Gallery - Density small multiples"
                 :url "https://r-graph-gallery.com/density-plot.html"
                 :dataset :chem97})

(plot/render-image
 {:data chem97
  :mapping {:x :gcsescore :facet :score :color :gender}
  :charts [(chart/density)]
  :labels {:title "GCSE score by A-level score and gender" :x "GCSE score"}}
 {:width 900 :height 600})

;; ### Stacked density

;; `:position :stack` piles the group densities on one common grid; `:normalize` shows each group's share.

(nu/source-note {:source "R Graph Gallery - Stacked density chart"
                 :url "https://r-graph-gallery.com/135-stacked-density-graph.html"
                 :dataset :diamonds})

(let [p {:data diamonds
         :mapping {:x :price :color :cut}
         :scales {:color {:domain ["Fair" "Good" "Very Good" "Premium" "Ideal"]}}}]
  (plot/render-image
   (plot/arrange [(assoc p :charts [(chart/density {:position :stack})] :labels {:title ":stack" :x "price [USD]"})
                  (assoc p :charts [(chart/density {:position :normalize})] :labels {:title ":normalize" :x "price [USD]"})]
                 {:columns 2})
   {:width 1000 :height 450}))

;; ### Marginal density

;; `:margins` adds charts beside the panel: the top margin shares the horizontal axis, the right margin the
;; vertical one. Margin charts read the plot's data and colour mapping, with `:x` mapped to the shared dimension
;; for them; they have no axes of their own, so the scatter keeps the only x and y axes.

(nu/source-note {:source "R Graph Gallery - Marginal distribution"
                 :url "https://r-graph-gallery.com/277-marginal-histogram-for-ggplot2.html"
                 :dataset :iris})

(plot/render-image
 {:data iris
  :mapping {:x :sepal-length :y :sepal-width :color :species}
  :charts [(chart/scatter)]
  :margins {:top (chart/density {:fill :color :alpha 0.4})
            :right (chart/density {:fill :color :alpha 0.4})}
  :labels {:title "Iris sepals with marginal densities" :x "sepal length [cm]" :y "sepal width [cm]"}}
 {:width 750 :height 650})

;; ## Histogram

;; Counts of the values in bins shared by all panels and groups.

;; ### Basic histogram

;; `:y` chooses the bar height: percent of the total (default), count, density (area 1) or probability.

(nu/source-note {:source "R Graph Gallery - Histogram"
                 :url "https://r-graph-gallery.com/220-basic-ggplot2-histogram.html"
                 :dataset :faithful})

(plot/render-image
 (plot/arrange (for [y [:percent :count :density :pmf]]
                 {:data faithful
                  :mapping {:x :waiting}
                  :charts [(chart/histogram {:y y})]
                  :labels {:title (str ":y " y) :x "waiting [min]"}})
               {:columns 2})
 {:width 900 :height 600})

;; ### Bin size

;; `:bins` takes a fastmath estimation rule, a number of equal bins or explicit breaks.

(nu/source-note {:source "R Graph Gallery - Histogram bin size"
                 :url "https://r-graph-gallery.com/220-basic-ggplot2-histogram.html"
                 :dataset :faithful})

(plot/render-image
 (plot/arrange (for [bins [:sturges 5 40 [1.5 2.5 3.5 4 4.5 5.5]]]
                 {:data faithful
                  :mapping {:x :eruptions}
                  :charts [(chart/histogram {:bins bins})]
                  :labels {:title (str ":bins " bins) :x "eruption time [min]"}})
               {:columns 2})
 {:width 900 :height 600})

;; ### Mirror histogram

;; Two groups back to back. Bars are filled by the group colour and outlined by `:stroke-color` - a colour,
;; `:none` or `:darker` (the darkened fill, default); `:symmetric` gives both halves one range.

(nu/source-note {:source "R Graph Gallery - Mirror histogram"
                 :url "https://r-graph-gallery.com/190-mirrored-histogram.html"
                 :dataset :chem97})

(plot/render-image
 (plot/arrange [{:data chem97 :mapping {:x :gcsescore :color :gender}
                 :charts [(chart/histogram {:position :mirror :bins 30})]
                 :labels {:title "default outline" :x "GCSE score"}}
                {:data chem97 :mapping {:x :gcsescore :color :gender}
                 :charts [(chart/histogram {:position :mirror :bins 30 :stroke-color :white :symmetric true})]
                 :labels {:title "white outline, :symmetric" :x "GCSE score"}}]
               {:columns 2})
 {:width 1000 :height 450})

;; ### Groups on one axis

;; `:position` places the groups' bars: over each other (translucent), side by side, stacked or normalized.

(nu/source-note {:source "R Graph Gallery - Histogram with several groups"
                 :url "https://r-graph-gallery.com/histogram_several_group.html"
                 :dataset :iris})

(plot/render-image
 (plot/arrange (for [position [:overlay :dodge :stack :normalize]]
                 {:data iris
                  :mapping {:x :sepal-length :color :species}
                  :charts [(chart/histogram {:position position :bins 15})]
                  :labels {:title (str ":position " position) :x "sepal length [cm]"}})
               {:columns 2})
 {:width 1000 :height 650})

;; ### Small multiples

(nu/source-note {:source "R Graph Gallery - Histogram small multiples"
                 :url "https://r-graph-gallery.com/histogram.html"
                 :dataset :diamonds})

(plot/render-image
 {:data diamonds
  :mapping {:x :price :facet :cut}
  :scales {:facet {:domain ["Fair" "Good" "Very Good" "Premium" "Ideal"]}}
  :charts [(chart/histogram {:bins 30})]
  :labels {:title "Diamond prices by cut" :x "price [USD]"}}
 {:width 900 :height 600})

;; ### Marginal histogram

;; A margin may hold several charts, `:size` sets its thickness in pixels and `:axis true` shows the magnitude
;; (count, density) of a histogram or density margin; strips (boxes, rugs, ...) work in margins too.

(nu/source-note {:source "R Graph Gallery - Marginal histogram"
                 :url "https://r-graph-gallery.com/277-marginal-histogram-for-ggplot2.html"
                 :dataset :iris})

(plot/render-image
 {:data iris
  :mapping {:x :petal-length :y :petal-width :color :species}
  :charts [(chart/scatter)]
  :margins {:top {:charts (chart/histogram {:bins 25 :position :stack :y :count}) :axis true}
            :right {:charts [(chart/box {:outliers false}) (chart/rug)] :size 70}}
  :labels {:title "Iris petals: stacked histogram on top, boxes and a rug on the right"
           :x "petal length [cm]" :y "petal width [cm]"}}
 {:width 750 :height 650})

;; ### Strips on a histogram

;; Any strip chart (box, violin, rug, stripplot, beeswarm, extent) layers on a histogram or density:
;; `{:at v}` centres it on y = v; `:place :bottom` draws it below y = 0 in room the chart asks for.

(nu/source-note {:source "R Graph Gallery - Histogram with boxplot on top; lattice Fig 3.1"
                 :url ["https://r-graph-gallery.com/82-boxplot-on-top-of-histogram.html" "http://lmdvr.r-forge.r-project.org/figures/figures.html"]
                 :dataset :faithful})

(plot/render-image
 (plot/arrange [{:data faithful :mapping {:x :eruptions}
                 :charts [(chart/histogram {:bins 20}) (chart/box {:place {:at 14.5} :thickness 16})]
                 :labels {:title "box above ({:at 14.5})" :x "eruption time [min]"}}
                {:data faithful :mapping {:x :eruptions}
                 :charts [(chart/density) (chart/rug {:place :bottom :width 1.0})]
                 :labels {:title "rug below (:bottom)" :x "eruption time [min]"}}
                {:data faithful :mapping {:x :eruptions}
                 :charts [(chart/density) (chart/stripplot {:place :bottom :rng (r/rng :mersenne 2) :size 6})]
                 :labels {:title "jittered points below" :x "eruption time [min]"}}
                {:data faithful :mapping {:x :eruptions}
                 :charts [(chart/histogram {:bins 20}) (chart/beeswarm {:place :bottom :size 4 :bottom-share 0.3})]
                 :labels {:title "beeswarm below" :x "eruption time [min]"}}]
               {:columns 2})
 {:width 1000 :height 700})

;; ## Boxplot

;; A Tukey box per group: quartiles, whiskers to the most extreme values within 1.5 IQR, outliers beyond.

;; ### Basic boxplot

(nu/source-note {:source "R Graph Gallery - Boxplot"
                 :url "https://r-graph-gallery.com/262-basic-boxplot-with-ggplot2.html"
                 :dataset :mtcars})

(plot/render-image
 {:data mtcars
  :mapping {:x :mpg :y :cyl}
  :scales {:y {:type :categorical}}
  :charts [(chart/box)]
  :labels {:title "Fuel economy by number of cylinders" :x "miles per gallon" :y "cylinders"}}
 {:width 800 :height 400})

;; The cylinder count is a number, so it is declared categorical - one band per value instead of a numeric axis -
;; with `:scales {:y {:type :categorical}}`; strings and keywords are categorical by themselves.

;; ### Boxplot with jittered points

(nu/source-note {:source "R Graph Gallery - Boxplot with jitter"
                 :url "https://r-graph-gallery.com/96-boxplot-with-jitter.html"
                 :dataset :mtcars})

(plot/render-image
 {:data mtcars
  :mapping {:x :mpg :y :cyl}
  :scales {:y {:type :categorical}}
  :charts [(chart/box {:outliers false :color [210 210 225]})
           (chart/stripplot {:rng (r/rng :mersenne 7) :jitter 0.2 :size 8 :color [215 48 39 160]})]
  :labels {:title "Box with every car" :x "miles per gallon" :y "cylinders"}}
 {:width 800 :height 400})

;; ### Group order

;; The band scale's `:order-by` orders the levels by a function of each level's rows, from the bottom up;
;; `:descending true` reverses it.

(nu/source-note {:source "R Graph Gallery - Reorder boxplot"
                 :url "https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html"
                 :dataset :barley})

(plot/render-image
 (plot/arrange [{:data barley :mapping {:x :yield :y :variety}
                 :scales {:y {:order-by #(stats/median (map :yield %))}}
                 :charts [(chart/box)]
                 :labels {:title "by median (largest on top)" :x "yield" :y nil}}
                {:data barley :mapping {:x :yield :y :variety}
                 :scales {:y {:order-by #(stats/mean (map :yield %)) :descending true}}
                 :charts [(chart/box)]
                 :labels {:title "by mean, :descending (largest at the bottom)" :x "yield" :y nil}}]
               {:columns 2})
 {:width 1000 :height 500})

;; ### Highlight a group

;; A derived column and an explicit colour scale: grey for the rest, red for the group in focus. Each band holds one
;; colour group, so every box stays on its band's centre line.

(nu/source-note {:source "R Graph Gallery - Highlight a group in boxplot"
                 :url "https://r-graph-gallery.com/23-add-colors-to-specific-groups-of-a-boxplot.html"
                 :dataset :mtcars})

(plot/render-image
 {:data (map #(assoc % :focus (if (= 6 (:cyl %)) "6 cylinders" "other")) mtcars)
  :mapping {:x :hp :y :cyl :color :focus}
  :scales {:y {:type :categorical}
           :color {:domain ["other" "6 cylinders"] :range [[190 190 190] [215 48 39]]}}
  :charts [(chart/box)]
  :labels {:title "Horse power, 6 cylinders highlighted" :x "horse power" :y "cylinders"}}
 {:width 800 :height 400})

;; ### Grouped boxplot

(nu/source-note {:source "R Graph Gallery - Grouped boxplot"
                 :url "https://r-graph-gallery.com/265-grouped-boxplot-with-ggplot2.html"
                 :dataset :barley})

(plot/render-image
 {:data barley
  :mapping {:x :yield :y :site :color :year}
  :scales {:color {:type :categorical}}
  :charts [(chart/box)]
  :labels {:title "Barley yield by site and year" :x "yield [bushels/acre]" :y nil}}
 {:width 800 :height 500})

;; ### Faceted boxplot

(nu/source-note {:source "R Graph Gallery - Boxplot small multiples"
                 :url "https://r-graph-gallery.com/boxplot.html"
                 :dataset :barley})

(plot/render-image
 {:data barley
  :mapping {:x :yield :y :year :facet :site}
  :scales {:y {:type :categorical}}
  :charts [(chart/box)]
  :labels {:title "Yield by year, one panel per site" :x "yield" :y nil}}
 {:width 900 :height 500})

;; ### Variable width

;; `:varwidth` makes each box's width proportional to the square root of its count.

(nu/source-note {:source "R Graph Gallery - Boxplot with variable width"
                 :url "https://r-graph-gallery.com/266-ggplot2-boxplot-with-variable-width.html"
                 :dataset :chem97})

(plot/render-image
 {:data chem97
  :mapping {:x :gcsescore :y :score}
  :scales {:y {:type :categorical}}
  :charts [(chart/box {:varwidth true :n-label true})]
  :labels {:title "GCSE score by A-level score" :x "GCSE score" :y "A-level score"}}
 {:width 800 :height 550})

;; ### Binned continuous variable

;; `levels/bin` cuts a numeric column into half-open interval bins; the bins are ordered levels for the boxes.

(nu/source-note {:source "R Graph Gallery - Boxplot from continuous variable"
                 :url "https://r-graph-gallery.com/268-ggplot2-boxplot-from-continuous-variable.html"
                 :dataset :diamonds})

(plot/render-image
 {:data (levels/bin diamonds :carat {:as :carat-bin :bins [0 0.5 1 1.5 2 2.5 5.01]})
  :mapping {:x :price :y :carat-bin}
  :charts [(chart/box {:outliers false :n-label true})]
  :labels {:title "Diamond price by carat" :x "price [USD]" :y "carat"}}
 {:width 800 :height 500})

;; ### Mean marker

;; The categorical declaration can also sit on the mapping: `(levels/categorical :gear)` reads `:gear` as usual
;; and makes its dimension categorical, here instead of a `:scales` entry.

(nu/source-note {:source "R Graph Gallery - Boxplot with mean"
                 :url "https://r-graph-gallery.com/269-ggplot2-boxplot-with-average-value.html"
                 :dataset :mtcars})

(plot/render-image
 {:data mtcars
  :mapping {:x :qsec :y (levels/categorical :gear)}
  :charts [(chart/box {:mean true})]
  :labels {:title "Quarter mile time by gears, mean marked" :x "quarter mile [s]" :y "gears"}}
 {:width 800 :height 400})

;; ### Sample size

;; `:n-label` with `:n-format` (a format string for the count).

(nu/source-note {:source "R Graph Gallery - Boxplot with sample size"
                 :url "https://r-graph-gallery.com/26-add-text-over-a-boxplot.html"
                 :dataset :barley})

(plot/render-image
 {:data barley
  :mapping {:x :yield :y :variety :color :year}
  :scales {:color {:type :categorical}}
  :charts [(chart/box {:n-label true :n-format "%d obs"})]
  :labels {:title "Yield by variety and year" :x "yield" :y nil}}
 {:width 900 :height 650})

;; ### Horizontal and vertical

(nu/source-note {:source "R Graph Gallery - Horizontal boxplot"
                 :url "https://r-graph-gallery.com/boxplot.html"
                 :dataset :mtcars})

(let [p {:data mtcars :mapping {:x :wt :y :am} :scales {:y {:type :categorical}} :charts [(chart/box)]
         :labels {:x "weight [1000 lbs]" :y "manual transmission"}}]
  (plot/render-image
   (plot/arrange [(assoc-in p [:labels :title] "horizontal")
                  (-> p (assoc :flip true) (assoc-in [:labels :title] ":flip true"))]
                 {:columns 2})
   {:width 900 :height 400}))

;; ### Intervals

;; `chart/extent` draws an interval per group from fastmath.stats: here the 95% confidence interval of the mean,
;; mean ± standard deviation and a bootstrap percentile interval.

(nu/source-note {:source "seaborn - pointplot (confidence intervals); cljplot - extents"
                 :url ["https://seaborn.pydata.org/generated/seaborn.pointplot.html"
                       "https://github.com/generateme/cljplot/blob/master/sketches/examples.clj"]
                 :dataset :barley})

(plot/render-image
 (plot/arrange (for [[title opts] [[":ci" {:method :ci}]
                                   [":stddev" {:method :stddev}]
                                   [":bootstrap-percentile" {:method :bootstrap-percentile :rng (r/rng :mersenne 1)}]]]
                 {:data barley :mapping {:x :yield :y :site}
                  :charts [(chart/extent opts)]
                  :labels {:title title :x "yield" :y nil}})
               {:columns 3})
 {:width 1000 :height 400})

;; ## Ridgeline

;; Histograms and densities with a categorical `:y` become ridgelines: one profile per level, growing toward the
;; next level, all on one height scale (`:overlap` - the tallest profile in band steps).

;; ### Basic ridgeline

(def months ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November"
             "December"])

(nu/source-note {:source "R Graph Gallery - Ridgeline chart"
                 :url "https://r-graph-gallery.com/294-basic-ridgeline-plot.html"
                 :dataset :lincoln-weather})

(plot/render-image
 {:data lincoln
  :mapping {:x :mean-temp :y :month}
  :scales {:y {:domain months :reverse true}}
  :charts [(chart/density {:overlap 1.5})]
  :labels {:title "Mean temperature in Lincoln, 2016" :x "mean temperature [F]" :y nil}}
 {:width 800 :height 650})

;; ### Ridge shapes

;; Histograms make ridgelines too; any strip chart can share the bands.

(nu/source-note {:source "R Graph Gallery - Ridgeline chart"
                 :url "https://r-graph-gallery.com/294-basic-ridgeline-plot.html"
                 :dataset :diamonds})

(let [p {:data diamonds
         :mapping {:x :price :y :cut}
         :scales {:y {:domain ["Fair" "Good" "Very Good" "Premium" "Ideal"]}}}]
  (plot/render-image
   (plot/arrange [(assoc p :charts [(chart/histogram {:bins 40})] :labels {:title "histogram ridges" :x "price" :y nil})
                  (assoc p :data (take-nth 25 diamonds)
                         :charts [(chart/density {:overlap 0.7}) (chart/stripplot {:rng (r/rng :mersenne 4) :size 3 :jitter 0.2})]
                         :labels {:title "density ridges with points (4% sample)" :x "price" :y nil})]
                 {:columns 2})
   {:width 1000 :height 550}))

;; ### Gradient fill

;; `:fill-gradient` colours the area along `:x` with a clojure2d gradient.

(nu/source-note {:source "R Graph Gallery - Ridgeline with gradient"
                 :url "https://r-graph-gallery.com/294-basic-ridgeline-plot.html"
                 :dataset :lincoln-weather})

(plot/render-image
 {:data lincoln
  :mapping {:x :max-temp :y :month}
  :scales {:y {:domain months :reverse true}}
  :charts [(chart/density {:overlap 1.5 :fill-gradient :mpl/plasma})]
  :labels {:title "Daily maximum temperature" :x "max temperature [F]" :y nil}}
 {:width 800 :height 650})

;; ### Double ridgeline

;; With two groups, `:position :mirror` grows the second group away from the next level.

(nu/source-note {:source "R Graph Gallery - Ridgeline (two groups)"
                 :url "https://r-graph-gallery.com/web-double-ridgeline-plot.html"
                 :dataset :chem97})

(plot/render-image
 {:data chem97
  :mapping {:x :gcsescore :y :score :color :gender}
  :scales {:y {:type :categorical}}
  :charts [(chart/density {:position :mirror :overlap 0.45})]
  :labels {:title "GCSE score: F up, M down, by A-level score" :x "GCSE score" :y "A-level score"}}
 {:width 800 :height 650})

;; ## Beeswarm

;; Points packed across their band so none overlap (`:fit` squeezes a swarm that is too wide into its band).
;; A colour mapping makes groups - here each species is alone in its band, so the swarm stays centred - and the box
;; opts out of the colour with `:mapping {:color nil}`.

(nu/source-note {:source "R Graph Gallery - Beeswarm"
                 :url ["https://r-graph-gallery.com/370-basic-beeswarm-plot.html" "https://r-graph-gallery.com/371-custom-dots-beeswarm.html" "https://r-graph-gallery.com/372-grouped-beeswarm.html"]
                 :dataset :iris})

(plot/render-image
 (plot/arrange [{:data iris :mapping {:x :sepal-length :y :species}
                 :charts [(chart/beeswarm {:size 6})]
                 :labels {:title "basic" :x "sepal length" :y nil}}
                {:data iris :mapping {:x :sepal-length :y :species} :flip true
                 :charts [(chart/beeswarm {:size 6})]
                 :labels {:title "vertical (:flip)" :x "sepal length" :y nil}}
                {:data mtcars :mapping {:x :mpg :y :cyl} :scales {:y {:type :categorical}}
                 :charts [(chart/beeswarm {:size 12 :shape \A :color [215 48 39 200]})]
                 :labels {:title "dot style (:shape, :size)" :x "mpg" :y "cylinders"}}
                {:data iris :mapping {:x :petal-length :y :species :color :species}
                 :charts [(chart/box {:outliers false :color [235 235 240] :width 0.6 :mapping {:color nil}})
                          (chart/beeswarm {:size 5})]
                 :labels {:title "coloured, over boxes" :x "petal length" :y nil}}]
               {:columns 2})
 {:width 1000 :height 700})

;; ## Strip plot and rug

;; ### Strip plot

;; Jittered points; `:rng` makes the jitter reproducible.

(nu/source-note {:source "lattice stripplot; seaborn stripplot"
                 :url ["http://lmdvr.r-forge.r-project.org/figures/figures.html" "https://seaborn.pydata.org/generated/seaborn.stripplot.html"]
                 :dataset :barley})

(plot/render-image
 {:data barley
  :mapping {:x :yield :y :variety :color :year}
  :scales {:color {:type :categorical}}
  :charts [(chart/stripplot {:rng (r/rng :mersenne 1) :size 8 :jitter 0.1})]
  :labels {:title "Barley yield by variety, dodged by year" :x "yield" :y nil}}
 {:width 800 :height 550})

;; ### Rug

;; One line per value along the panel edge (`:edge`, the default without `:y`) or below y = 0 (`:bottom`).

(nu/source-note {:source "seaborn rugplot; lattice densityplot"
                 :url ["https://seaborn.pydata.org/generated/seaborn.rugplot.html" "http://lmdvr.r-forge.r-project.org/figures/figures.html"]
                 :dataset :faithful})

(plot/render-image
 (plot/arrange [{:data faithful :mapping {:x :waiting} :charts [(chart/density) (chart/rug)]
                 :labels {:title "rug on the edge" :x "waiting [min]"}}
                {:data faithful :mapping {:x :waiting} :charts [(chart/density) (chart/rug {:place :bottom :width 1.0})]
                 :labels {:title "rug below zero (:bottom)" :x "waiting [min]"}}]
               {:columns 2})
 {:width 900 :height 400})

;; ## 1d strips

;; Every strip chart also works on one variable alone: without `:y` it is placed in the middle of the panel (the
;; rug, on the panel's edge by default, takes `:place :center`) and the plot has no y axis.

(nu/source-note {:source "seaborn boxplot, swarmplot (1d)"
                 :url ["https://seaborn.pydata.org/generated/seaborn.boxplot.html"
                       "https://seaborn.pydata.org/generated/seaborn.swarmplot.html"]
                 :dataset :faithful})

(plot/render-image
 (plot/arrange (for [[title c] [["box" (chart/box)]
                                ["violin" (chart/violin)]
                                ["stripplot" (chart/stripplot {:rng (r/rng :mersenne 5) :jitter 0.3 :size 6})]
                                ["beeswarm" (chart/beeswarm {:size 5})]
                                ["rug" (chart/rug {:place :center :width 0.6})]
                                ["extent :stddev" (chart/extent {:method :stddev})]]]
                 {:data faithful
                  :mapping {:x :waiting}
                  :charts [c]
                  :labels {:title title :x "waiting [min]"}})
               {:columns 2})
 {:width 900 :height 650})

;; ## Empirical cumulative distribution

(nu/source-note {:source "lattice ecdfplot; seaborn ecdfplot"
                 :url ["http://lmdvr.r-forge.r-project.org/figures/figures.html" "https://seaborn.pydata.org/generated/seaborn.ecdfplot.html"]
                 :dataset :iris})

(plot/render-image
 {:data iris
  :mapping {:x :petal-length :color :species}
  :charts [(chart/ecdf {:line-width 2})]
  :labels {:title "ECDF of petal length by species" :x "petal length [cm]"}}
 {:width 800 :height 450})
