;; # Charts: strip distributions

;; Strip charts showing the distribution of each level's values: summaries (`box`, `violin`) and the values
;; themselves (`stripplot`, `beeswarm`, `dotplot`, `rug`). Placement, width and grouping across the strip are the
;; Strip options group every one of them takes. Each chart lists the mapping keys it reads, its own options and the
;; shared option groups it takes.

(ns reference.charts-strip-distributions
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; ## box

;; A box per level (and group): the quartiles, the median, whiskers to the most extreme values within 1.5 IQR of
;; the box and the outliers past them - computed from the values ...

(nu/options-table (describe/mapping (chart/box)) :values)

(plot/render-image {:data iris
                    :mapping {:x :sepal-width :y :species :color :species}
                    :charts [(chart/box)]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 300})

;; ... or read from rows of precomputed statistics, one row per level and group, with `:stat :identity`:

(nu/options-table (describe/mapping (chart/box)) :summary)

(def summaries [{:site "north" :lower 2.1 :q1 3.4 :median 4.0 :q3 4.9 :upper 6.3 :outliers [7.4 8.0] :mean 4.3 :n 40}
                {:site "south" :lower 1.0 :q1 2.2 :median 2.9 :q3 3.3 :upper 4.4 :outliers [0.2] :mean 2.8 :n 15}
                {:site "east" :lower 3.0 :q1 4.1 :median 5.2 :q3 5.8 :upper 7.0 :outliers [] :mean 5.0 :n 90}])

(nu/data-table summaries)

(plot/render-image {:data summaries
                    :mapping {:y :site :lower :lower :q1 :q1 :median :median :q3 :q3 :upper :upper
                              :outliers :outliers :mean :mean :n :n}
                    :charts [(chart/box {:stat :identity :mean true :varwidth true})]
                    :scales {:y {:reverse true}}
                    :labels {:x "value"}}
                   {:width 600 :height 280})

;; Its options: the box and outlier style, the statistics, outliers on or off, the mean marker, widths by count and
;; the Tufte variant (whisker lines and a median point):

(nu/options-table (describe/own-rows (describe/options (chart/box))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :mpg :y :cyl}
                                   :charts [(chart/box {:color [230 160 60] :stroke-color :black :shape \x :size 9 :mean true})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title "style, :mean true"}}
                                  {:data mtcars
                                   :mapping {:x :mpg :y :cyl}
                                   :charts [(chart/box {:outliers false :varwidth true})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title ":outliers false, :varwidth"}}
                                  {:data mtcars
                                   :mapping {:x :mpg :y :cyl}
                                   :charts [(chart/box {:variant :tufte})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title ":variant :tufte"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

(nu/shared-groups (describe/options (chart/box)))

;; ## violin

;; A mirrored kernel density per level (and group), with inner marks - a small box by default.

(nu/options-table (describe/mapping (chart/violin)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/violin)]
                    :legend false}
                   {:width 600 :height 320})

;; Its options: the fill, outline and inner marks' colour, the density estimate (kernel, bandwidth, points, how far
;; past the data), how widths compare between violins and the inner marks:

(nu/options-table (describe/own-rows (describe/options (chart/violin))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:bandwidth 0.1 :inner :quartiles})]
                                   :labels {:title ":bandwidth 0.1, :inner :quartiles"}}
                                  {:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:cut 2 :inner :rug :inner-color [200 40 40]})]
                                   :labels {:title ":cut 2, :inner :rug"}}
                                  {:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/violin {:scale :count :inner :points})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":scale :count, :inner :points"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/violin)))

;; ## stripplot

;; Every value as a point across its level's strip, jittered so equal values don't hide each other.

(nu/options-table (describe/mapping (chart/stripplot)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species :color :species}
                    :charts [(chart/stripplot {:rng (r/rng :mersenne 1)})]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 280})

;; Its options: the point style, the jitter spread (seeded for the same image on every build) and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/stripplot))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/stripplot {:jitter 0})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":jitter 0"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/stripplot {:jitter 0.8 :size 8 :shape \s :color [200 80 40 90]
                                                              :rng (r/rng :mersenne 1)})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":jitter 0.8, squares"}}]
                                 {:columns 2})
                   {:width 900 :height 280})

(nu/shared-groups (describe/options (chart/stripplot)))

;; ## beeswarm

;; Every value as a point, packed across the strip without overlaps - the swarm's width shows the density.

(nu/options-table (describe/mapping (chart/beeswarm)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/beeswarm)]
                    :legend false}
                   {:width 600 :height 320})

;; Its options: the point style (the size is also the packing diameter), squeezing a wide swarm into its slot and
;; the packing direction:

(nu/options-table (describe/own-rows (describe/options (chart/beeswarm))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:size 14 :fit false})]
                                   :labels {:title ":size 14, :fit false"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:size 14})]
                                   :labels {:title ":size 14, :fit true"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:pack :around :stroke-color :white})]
                                   :labels {:title ":pack :around"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/beeswarm)))

;; ## dotplot

;; Values binned along the value axis, each a dot stacked in its bin (Wilkinson's dot plot).

(nu/options-table (describe/mapping (chart/dotplot)))

(plot/render-image {:data mtcars
                    :mapping {:x :mpg}
                    :charts [(chart/dotplot {:binwidth 1.5})]}
                   {:width 600 :height 280})

;; Its options: the dot style, the binning (density or fixed bins from an origin), where dots sit and how they
;; stack:

(nu/options-table (describe/own-rows (describe/options (chart/dotplot))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:method :histogram :binwidth 2 :origin 10})]
                                   :labels {:title ":method :histogram"}}
                                  {:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :stack-dir :center :color [200 80 40]})]
                                   :labels {:title ":stack-dir :center"}}
                                  {:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :at :value})]
                                   :labels {:title ":at :value"}}]
                                 {:columns 3})
                   {:width 1000 :height 280})

(nu/shared-groups (describe/options (chart/dotplot)))

;; ## rug

;; A short line per value - along the panel's edge, or one strip per level; often beside another chart.

(nu/options-table (describe/mapping (chart/rug)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width}
                    :charts [(chart/scatter) (chart/rug {:mapping {:y nil}})]}
                   {:width 600 :height 320})

;; Its options: the line colour and width, and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/rug))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :y :species}
                                   :charts [(chart/rug)]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "defaults, a rug per level"}}
                                  {:data iris
                                   :mapping {:x :petal-length :y :species}
                                   :charts [(chart/rug {:color [200 40 40 60] :line-width 3})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":line-width 3"}}]
                                 {:columns 2})
                   {:width 900 :height 260})

(nu/shared-groups (describe/options (chart/rug)))
