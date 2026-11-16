;; # Part of a whole

;; How a whole splits into parts: grouped and stacked bars, treemaps, donuts and pies, circle packing, sunbursts,
;; waffles, Venn diagrams and UpSet plots (the [R Graph Gallery](https://r-graph-gallery.com/) part-of-a-whole pages,
;; the [data-to-viz](https://www.data-to-viz.com/) sunburst and the pie caveat).
;;
;; Not here: dendrograms wait for hierarchical clustering in fastmath (backlog); the sunburst of flows belongs to the
;; Flow chapter.

(ns use-cases.part-of-a-whole
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.sets :as sets]
            [notebook-utils :as nu]))

(def world
  "World population by region, subregion and country - the countries with a known population."
  (filterv :population (nu/dataset :world-population)))

(def europe (filterv #(= "Europe" (:region %)) world))

(def words (nu/dataset :artist-words))

(defn- share-label
  "Bar label: the part's name and its share of the whole."
  [{:keys [group share]}]
  (format "%s\n%.0f%%" (:color group) (* 100.0 (double share))))

(defn- percent-label
  "Bar label: the share of the whole only."
  [{:keys [share]}]
  (format "%.0f%%" (* 100.0 (double share))))

(def pie-base
  "A pie of the regions' populations: stacked bars (`:stat :sum` over the countries of a region) in polar coordinates,
  one bar filling the circle."
  {:data world :mapping {:x :population :color :region} :coord {:kind :polar}
   :labels {:x nil :y nil}})

;; ## Grouped and stacked bars

;; Bars of groups within categories: side by side, on top of each other, scaled to the whole, or one panel per
;; category.

;; ### Grouped, stacked, percent

;; R Graph Gallery's specie × condition values (seeded): `:dodge` compares the groups, `:stack` the totals, `:normalize`
;; the shares; facets give a panel per specie; negative values stack downward from 0.

(def species-conditions
  (let [rng (r/rng :mersenne 42)]
    (vec (for [specie ["banana" "poacee" "sorgho" "triticum"]
               condition ["normal" "stress" "Nitrogen"]]
           {:specie specie :condition condition :value (m/abs (r/grandom rng 0.0 15.0))}))))

(def signed-conditions
  (let [rng (r/rng :mersenne 7)]
    (mapv #(assoc % :value (r/grandom rng 0.0 15.0)) species-conditions)))

(nu/source-note {:source "R Graph Gallery - Grouped and Stacked barplot"
                 :url "https://r-graph-gallery.com/stacked-barplot.html"})

(plot/render-image
 (plot/arrange
  (vec (concat
        (for [[title position] [["grouped (:dodge)" :dodge] ["stacked (:stack)" :stack] ["percent (:normalize)" :normalize]]]
          {:data species-conditions :mapping {:x :specie :y :value :color :condition}
           :charts [(chart/bar {:position position})]
           :labels {:title title :x nil :y nil}})
        [{:data species-conditions :mapping {:x :condition :y :value :facet :specie}
          :theme {:axis {:x {:label-rotation -90}}}
          :charts [(chart/bar)]
          :labels {:title "small multiples" :x nil :y nil}}
         {:data signed-conditions :mapping {:x :specie :y :value :color :condition}
          :charts [(chart/bar {:position :stack})]
          :labels {:title "negative values" :x nil :y nil}}]))
  {:columns 3})
 {:width 1000 :height 650})

;; ### Likert scale

;; Survey answers as diverging stacked bars (seeded counts): disagreement to the left of 0, agreement to the right,
;; the neutral answers split half to each side. The colour domain orders the stacks from the middle outward.

(def answers ["Strongly disagree" "Disagree" "Neutral" "Agree" "Strongly agree"])

(def likert
  (let [rng (r/rng :mersenne 3)]
    (vec (for [q (range 1 6)
               [i answer] (map-indexed vector answers)
               :let [n (+ 5 (r/irandom rng 40))
                     sign ({0 -1.0 1 -1.0 3 1.0 4 1.0} i)]
               row (if sign
                     [{:question (str "Q" q) :answer answer :side "" :count (* sign n)}]
                     ;; the neutral answer: half on each side, two groups of one colour
                     [{:question (str "Q" q) :answer answer :side "-" :count (* -0.5 n)}
                      {:question (str "Q" q) :answer answer :side "+" :count (* 0.5 n)}])]
           row))))

(nu/source-note {:source "R Graph Gallery - Grouped and Stacked barplot (the Likert package)"
                 :url "https://r-graph-gallery.com/stacked-barplot.html"})

(plot/render-image
 {:data likert :mapping {:x :count :y :question :color :answer :group :side}
  :scales {:color {:domain ["Neutral" "Disagree" "Agree" "Strongly disagree" "Strongly agree"]
                   :range {"Strongly disagree" [178 24 43] "Disagree" [239 138 98] "Neutral" [200 200 200]
                           "Agree" [103 169 207] "Strongly agree" [33 102 172]}}
          :x {:formatter #(str (long (m/abs (double %))))}}
  :charts [(chart/bar {:position :stack})]
  :labels {:title "Five questions, five answers" :x "answers" :y nil}}
 {:width 800 :height 380})

;; ## Treemap

;; Nested rectangles, areas proportional to the value: the world's population by region, subregion and country.

;; ### Basic, groups and subgroups

;; One level (regions) and three (regions → subregions → countries); `:padding` frames the groups, a theme palette
;; recolours the regions.

(nu/source-note {:source "R Graph Gallery - Treemap; data-to-viz - Treemap"
                 :url ["https://r-graph-gallery.com/treemap.html" "https://www.data-to-viz.com/graph/treemap.html"]
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [{:data world :mapping {:value :population :hierarchy [:region]} :charts [(chart/treemap)]
    :labels {:title "regions"}}
   {:data world :mapping {:value :population :hierarchy [:region :subregion :country]}
    :theme {:palette {:color :set2}}
    :charts [(chart/treemap {:padding 4 :border-color [60 60 60]})]
    :labels {:title "regions, subregions, countries (:set2, :padding 4)"}}]
  {:columns 2 :widths [1 2]})
 {:width 1000 :height 520})

;; ### Annotated treemap

;; Label functions get the node's name, value, depth and shares: the countries with their population in millions.

(nu/source-note {:source "R Graph Gallery - Treemap (annotations, labels and colors)"
                 :url "https://r-graph-gallery.com/treemap.html"
                 :dataset :world-population})

(plot/render-image
 {:data world :mapping {:value :population :hierarchy [:region :country]}
  :charts [(chart/treemap {:leaf-labels #(format "%s\n%.0f M" (:name %) (/ (double (:value %)) 1e6))
                           :group-labels #(format "%s - %.1f%%" (:name %) (* 100.0 (double (:share %))))})]
  :labels {:title "World population (millions)"}}
 {:width 1000 :height 700})

;; ## Donut

;; A ring of stacked shares: the regions' populations around a hole (`:inner` - the hole's radius as a fraction).

;; ### Basic, labels, ring thickness

(nu/source-note {:source "R Graph Gallery - Donut chart; data-to-viz - Donut"
                 :url ["https://r-graph-gallery.com/doughnut-plot.html" "https://www.data-to-viz.com/graph/donut.html"]
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  (vec (for [inner [0.3 0.7]]
         (assoc pie-base
                :coord {:kind :polar :inner inner}
                :scales {:x {:axis false}}
                :charts [(chart/bar {:position :stack :width 1.0 :value-label percent-label})]
                :labels {:title (str ":inner " inner) :x nil :y nil})))
  {:columns 2})
 {:width 1000 :height 450})

;; ### Half donut

;; `:span 180` from `:start -90`: the circle shrinks to a half and fits its panel; outer labels name the parts.

(nu/source-note {:source "data-to-viz - Donut (variations)"
                 :url "https://www.data-to-viz.com/graph/donut.html"
                 :dataset :world-population})

(plot/render-image
 (assoc pie-base
        :coord {:kind :polar :inner 0.5 :span 180 :start -90}
        :legend false
        :charts [(chart/bar {:position :stack :width 1.0 :value-label share-label :label-position :outer})]
        :labels {:title "World population by region" :x nil :y nil})
 {:width 800 :height 420})

;; ## Pie

;; A donut without its hole. data-to-viz: angles are hard to compare - see the caveat at the end.

;; ### Basic, colours, a clean pie

;; The default pie shows its value axis around the circle; a palette recolours it; `:axis false` cleans it up.

(nu/source-note {:source "R Graph Gallery - Pie chart"
                 :url "https://r-graph-gallery.com/pie-plot.html"
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [(assoc pie-base :legend false :charts [(chart/bar {:position :stack :width 1.0})] :labels {:title "default" :x nil :y nil})
   (assoc pie-base :theme {:palette {:color :MetBrewer/Archambault}} :legend false
          :charts [(chart/bar {:position :stack :width 1.0})] :labels {:title ":MetBrewer/Archambault" :x nil :y nil})
   (assoc pie-base :scales {:x {:axis false}} :legend false
          :charts [(chart/bar {:position :stack :width 1.0 :stroke-color :white :stroke-width 1})]
          :labels {:title "clean" :x nil :y nil})]
  {:columns 3})
 {:width 1000 :height 380})

;; ### Slice labels

;; Label functions get each slice's value, count and share: shares inside the slices, or names and shares outside
;; with leader lines (`:label-position :outer` - the angle axis gives way, the circle makes room).

(nu/source-note {:source "R Graph Gallery - Pie chart (labels)"
                 :url "https://r-graph-gallery.com/pie-plot.html"
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [(assoc pie-base :scales {:x {:axis false}}
          :charts [(chart/bar {:position :stack :width 1.0 :value-label percent-label})]
          :labels {:title "inside" :x nil :y nil})
   (assoc pie-base :legend false
          :charts [(chart/bar {:position :stack :width 1.0 :value-label share-label :label-position :outer
                               :label-leader true})]
          :labels {:title "outside, with leaders" :x nil :y nil})]
  {:columns 2})
 {:width 1000 :height 480})

;; ## Circle packing

;; Circles with areas proportional to the value, nested in their groups' enclosing circles - a group's area isn't
;; proportional to its value (packing leaves gaps), compare circles within a group.

;; ### One level

;; Europe's countries: a circle each; leaves coloured by their subregion; wider gaps (`:padding`, pixels).

(nu/source-note {:source "R Graph Gallery - Circular packing; data-to-viz - Circular packing"
                 :url ["https://r-graph-gallery.com/circle-packing.html" "https://www.data-to-viz.com/graph/circularpacking.html"]
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [{:data europe :mapping {:value :population :hierarchy [:country]} :legend false
    :charts [(chart/pack)] :labels {:title "basic"}}
   {:data europe :mapping {:value :population :hierarchy [:country] :color :subregion}
    :charts [(chart/pack {:padding 6 :leaf-labels false})] :labels {:title "by subregion, :padding 6"}}]
  {:columns 2})
 {:width 1000 :height 480})

;; ### Several levels

;; Regions → subregions → countries: the regions' colours lighter at every level, group names under their circles'
;; tops; hiding the regions (`:hidden-levels #{1}`) leaves the subregions floating.

(nu/source-note {:source "R Graph Gallery - Circular packing (several levels)"
                 :url "https://r-graph-gallery.com/circle-packing.html"
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [{:data world :mapping {:value :population :hierarchy [:region :subregion :country]}
    :charts [(chart/pack)] :labels {:title "three levels"}}
   {:data world :mapping {:value :population :hierarchy [:region :subregion :country]}
    :charts [(chart/pack {:hidden-levels #{1}})] :labels {:title "regions hidden"}}]
  {:columns 2})
 {:width 1000 :height 520})

;; ## Sunburst

;; The hierarchy as rings: a partition (an icicle in cartesian coordinates) under polar coordinates - regions inside,
;; subregions, then countries outward.

(nu/source-note {:source "data-to-viz - Sunburst"
                 :url "https://www.data-to-viz.com/graph/sunburst.html"
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [{:data world :mapping {:value :population :hierarchy [:region :subregion :country]} :coord {:kind :polar :inner 0.15}
    :charts [(chart/partition)] :labels {:title "sunburst"}}
   {:data world :mapping {:value :population :hierarchy [:region :subregion]}
    :charts [(chart/partition)] :labels {:title "icicle (two levels)"}}]
  {:columns 2})
 {:width 1000 :height 520})

;; ## Waffle

;; A grid of cells: by default 10 × 10, a cell per percent of the whole (largest-remainder rounding keeps the total at
;; 100).

;; ### Basic, shapes, colours, groups

;; The regions' shares; circle cells; a palette; subregions within each region (a facet per region).

(nu/source-note {:source "R Graph Gallery - Waffle chart"
                 :url "https://r-graph-gallery.com/waffle.html"
                 :dataset :world-population})

(plot/render-image
 (plot/arrange
  [{:data world :mapping {:color :region :value :population} :charts [(chart/waffle)] :labels {:title "regions"}}
   {:data world :mapping {:color :region :value :population} :theme {:palette {:color :dark2}}
    :charts [(chart/waffle {:shape :circle :gap 0.15})] :labels {:title "circles, :dark2"}}]
  {:columns 2})
 {:width 1000 :height 400})

(nu/source-note {:source "R Graph Gallery - Waffle chart (groups and subgroups)"
                 :url "https://r-graph-gallery.com/waffle.html"
                 :dataset :world-population})

(plot/render-image
 {:data world :mapping {:color :subregion :value :population :facet :region}
  :charts [(chart/waffle {:gap 0.15})]
  :labels {:title "Subregions within each region"}}
 {:width 1000 :height 520})

;; ## Venn diagram and UpSet

;; The words three French artists use in their songs: a word is in an artist's set when it appears in their lyrics.
;; data-to-viz: no more than three sets in a Venn diagram, a number in every region - the UpSet plot handles more.

;; ### Two and three sets

;; Two artists: circle areas and the overlap proportional to the word counts. Three: the classic layout, counts and
;; shares from a label function.

(nu/source-note {:source "R Graph Gallery - Venn diagram; data-to-viz - Venn diagram"
                 :url ["https://r-graph-gallery.com/venn-diagram.html" "https://www.data-to-viz.com/graph/venn.html"]
                 :dataset :artist-words})

(plot/render-image
 (plot/arrange
  [{:data words :mapping {:set :artist :item :word}
    :charts [(chart/venn {:sets ["booba" "nekfeu"]})] :labels {:title "two artists"}}
   {:data words :mapping {:set :artist :item :word}
    :charts [(chart/venn {:region-labels #(format "%d\n%.0f%%" (:size %) (* 100.0 (double (:share %))))})]
    :labels {:title "three artists"}}]
  {:columns 2})
 {:width 1000 :height 480})

;; ### UpSet plot

;; Bars of the exact intersections (largest first) over a dot matrix of the sets in each: from
;; `cljplot2.data.sets/upset-rows` and `membership-rows`.

(nu/source-note {:source "R Graph Gallery - Venn diagram (the UpSetR alternative)"
                 :url "https://r-graph-gallery.com/venn-diagram.html"
                 :dataset :artist-words})

(plot/render-image
 (let [m {:set :artist :item :word}
       dots (sets/membership-rows words m)]
   (plot/arrange [{:data (sets/upset-rows words m) :mapping {:x :combination :y :size}
                   :scales {:x {:axis false}}
                   :charts [(chart/bar {:value-label true})] :labels {:title "Words shared by the artists" :x nil :y "words"}}
                  {:data dots :mapping {:x :combination :y :set}
                   :theme {:axis {:x {:label-rotation -45}}}
                   :charts [(chart/scatter {:size 12 :color [210 210 210]})
                            (chart/scatter {:data (filter :member? dots) :size 12 :color :black})]
                   :labels {:x nil :y nil}}]
                 {:columns 1 :heights [3 2]}))
 {:width 900 :height 650})

;; ## Caveats

;; ### The issue with pie charts

;; data-to-viz: three sets of five nearly equal values. As pies the differences hide in the angles; as bars they read
;; at once. (3D pies, exploded slices and legends far from the slices make it worse - not drawn here.)

(def caveat-data
  {"data 1" [17 18 20 22 24] "data 2" [20 18 21 20 20] "data 3" [24 23 21 19 18]})

(defn- caveat-rows [values] (mapv (fn [part v] {:part part :value v}) ["a" "b" "c" "d" "e"] values))

(nu/source-note {:source "data-to-viz - The issue with pie chart"
                 :url "https://www.data-to-viz.com/caveat/pie.html"})

(plot/render-image
 (plot/arrange
  (vec (concat
        (for [[title values] caveat-data]
          {:data (caveat-rows values) :mapping {:x :value :color :part} :coord {:kind :polar} :scales {:x {:axis false}}
           :legend false :charts [(chart/bar {:position :stack :width 1.0})] :labels {:title title :x nil :y nil}})
        (for [[title values] caveat-data]
          {:data (caveat-rows values) :mapping {:x :part :y :value :color :part} :legend false
           :charts [(chart/bar)] :labels {:title title :x nil :y nil}})))
  {:columns 3})
 {:width 1000 :height 650})

;; ### Other caveats

;; - Circle packing: only leaf areas follow the values - a group's circle also holds the gaps between its children.
;; - Treemaps: label at most three levels; deeper ones get too small to read (data-to-viz).
;; - Sunbursts and donuts compare angles, as pies do; outer rings exaggerate the deeper levels (their arcs are
;;   longer for the same share).
