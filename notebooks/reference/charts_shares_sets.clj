;; # Shares and sets

;; Charts of the parts of a whole: nested rectangles (`treemap`), bands or rings (`partition`), nested circles
;; (`pack`) and a grid of cells (`waffle`); `venn` shows how sets overlap.

(ns reference.charts-shares-sets
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "science" :team "chemistry" :cost 18} {:dept "arts" :team "music" :cost 15}
             {:dept "arts" :team "theatre" :cost 10} {:dept "arts" :team "painting" :cost 7}
             {:dept "sport" :team "football" :cost 20} {:dept "sport" :team "tennis" :cost 8}])

(nu/data-table budget)

;; ## treemap

;; Each leaf a rectangle of area proportional to its value, nested inside the rectangle of its group; the levels
;; come from `:hierarchy`, outer level first.

(nu/options-table (describe/mapping (chart/treemap)))

(plot/render-image {:data budget
                    :mapping {:hierarchy [:dept :team] :value :cost}
                    :charts [(chart/treemap)]}
                   {:width 600 :height 380})

;; Its options: the gap inside a group, the cell borders and the layout - near-square cells or slices turning
;; direction at each depth:

(nu/options-table (describe/own-rows (describe/options (chart/treemap))))

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap {:gap 8 :stroke-color :black :stroke-width 2})]
                                   :labels {:title ":gap 8, black borders"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap {:layout :slice-dice})]
                                   :labels {:title ":layout :slice-dice"}}]
                                 {:columns 2})
                   {:width 1000 :height 380})

;; `:order` sets the siblings' order, as in the partition: by descending value (the default of `:squarify`) or in
;; data order (the default of `:slice-dice`) - slices ordered by size make a mosaic read largest first:

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap {:layout :slice-dice :order :value})]
                                   :labels {:title ":slice-dice, :order :value"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap {:order :data})]
                                   :labels {:title ":squarify, :order :data"}}]
                                 {:columns 2})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/treemap)))

;; ## partition

;; The hierarchy as adjacent bands, one per level, each node as wide as its value - an icicle plot; in polar
;; coordinates the bands become rings (a sunburst).

(nu/options-table (describe/mapping (chart/partition)))

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/partition)]
                                   :labels {:title "cartesian: icicle"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/partition)]
                                   :coord {:kind :polar}
                                   :labels {:title "polar: sunburst"}}]
                                 {:columns 2})
                   {:width 1000 :height 420})

;; Its options: borders, the order of siblings, how nodes are coloured (by their top-level node, lighter by depth,
;; or each by its name) and whether the root is drawn. `:order :data` keeps the rows' order - here the budget
;; reversed, so sport comes first and the teams go from the cheapest up (by value, the default above, science leads):

(nu/options-table (describe/own-rows (describe/options (chart/partition))))

(plot/render-image (plot/arrange [{:data (reverse budget)
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/partition {:order :data :root true})]
                                   :coord {:kind :polar}
                                   :labels {:title ":order :data, :root true"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/partition {:color-by :name :stroke-width 2})]
                                   :coord {:kind :polar}
                                   :labels {:title ":color-by :name"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 1000 :height 460})

;; The borders in an icicle: thick dark lines instead of the default thin white ones (the icicle above):

(plot/render-image {:data budget
                    :mapping {:hierarchy [:dept :team] :value :cost}
                    :charts [(chart/partition {:stroke-color [40 40 40] :stroke-width 3})]
                    :labels {:title ":stroke-color, :stroke-width 3"}}
                   {:width 600 :height 360})

(nu/shared-groups (describe/options (chart/partition)))

;; ## pack

;; Each leaf a circle of area proportional to its value, packed inside the circle of its group.

(nu/options-table (describe/mapping (chart/pack)))

(plot/render-image {:data budget
                    :mapping {:hierarchy [:dept :team] :value :cost}
                    :charts [(chart/pack)]}
                   {:width 500 :height 450})

;; Its options: the gap between circles, borders, the packing order, the root circle and the depths left out of the
;; drawing (the layout unchanged - here the group circles, depth 1):

(nu/options-table (describe/own-rows (describe/options (chart/pack))))

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/pack {:gap 10 :root true :stroke-color :black})]
                                   :labels {:title ":gap 10, :root true"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/pack {:hidden-levels #{1}})]
                                   :labels {:title ":hidden-levels #{1}"}}]
                                 {:columns 2})
                   {:width 1000 :height 480})

;; The packing order, here of the budget rows reversed (smallest first): siblings by value (largest first) or in
;; data order; and wider borders:

(plot/render-image (plot/arrange [{:data (reverse budget)
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/pack {:order :value})]
                                   :labels {:title ":order :value (default)"}}
                                  {:data (reverse budget)
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/pack {:order :data})]
                                   :labels {:title ":order :data"}}
                                  {:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/pack {:stroke-color [40 40 40] :stroke-width 3})]
                                   :labels {:title ":stroke-width 3"}}]
                                 {:columns 3})
                   {:width 1000 :height 360})

(nu/shared-groups (describe/options (chart/pack)))

;; ## waffle

;; A grid of cells (10 × 10 by default), each category taking a share of the cells proportional to its value.

(nu/options-table (describe/mapping (chart/waffle)))

(def energy-mix [{:source "coal" :share 35} {:source "gas" :share 23} {:source "nuclear" :share 10}
                 {:source "hydro" :share 15} {:source "wind and solar" :share 17}])

(nu/data-table energy-mix)

(plot/render-image {:data energy-mix
                    :mapping {:color :source :value :share}
                    :charts [(chart/waffle)]}
                   {:width 500 :height 380})

;; Its options: the cell shape, gap and border; the grid size and fill order; or `:unit` - the value of one cell,
;; so the cell count follows the values (here one cell per 2 units, 5 columns wide):

(nu/options-table (describe/own-rows (describe/options (chart/waffle))))

(plot/render-image (plot/arrange [{:data energy-mix
                                   :mapping {:color :source :value :share}
                                   :charts [(chart/waffle {:cell-shape :circle :gap 0.2 :rows 5 :columns 20})]
                                   :labels {:title ":circle, 5 × 20"}
                                   :legend false}
                                  {:data energy-mix
                                   :mapping {:color :source :value :share}
                                   :charts [(chart/waffle {:fill-order :columns :stroke-color :white :stroke-width 2
                                                           :gap 0})]
                                   :labels {:title ":fill-order :columns"}
                                   :legend false}
                                  {:data energy-mix
                                   :mapping {:color :source :value :share}
                                   :charts [(chart/waffle {:unit 2 :columns 5})]
                                   :labels {:title ":unit 2"}}]
                                 {:columns 3 :widths [1.3 1 1.1]})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/waffle)))

;; ## venn

;; Two or three sets as overlapping circles, each region labelled with the number of items it holds; a row names
;; an item and a set it belongs to.

(nu/options-table (describe/mapping (chart/venn)))

(def memberships (concat (for [i (range 12)] {:club "chess" :member (str "p" i)})
                         (for [i (range 8 14)] {:club "running" :member (str "p" i)})
                         (for [i (range 0 24 3)] {:club "choir" :member (str "p" i)})))

(nu/data-table memberships)

(plot/render-image {:data memberships
                    :mapping {:set :club :item :member}
                    :charts [(chart/venn)]}
                   {:width 500 :height 420})

;; Its options: the fill and outline, which sets are shown, proportional areas (two sets), and the set and region
;; labels - a function of `{:sets :size :share}` writes a region's text:

(nu/options-table (describe/own-rows (describe/options (chart/venn))))

(plot/render-image (plot/arrange [{:data memberships
                                   :mapping {:set :club :item :member}
                                   :charts [(chart/venn {:sets ["chess" "running"]})]
                                   :labels {:title "two sets, proportional"}}
                                  {:data memberships
                                   :mapping {:set :club :item :member}
                                   :charts [(chart/venn {:sets ["chess" "running"] :proportional false :alpha 0.6
                                                         :stroke-color :none})]
                                   :labels {:title ":proportional false"}}
                                  {:data memberships
                                   :mapping {:set :club :item :member}
                                   :charts [(chart/venn {:region-labels #(format "%.0f%%" (* 100.0 (double (:share %))))})]
                                   :labels {:title ":region-labels - shares"}}]
                                 {:columns 3})
                   {:width 1000 :height 380})

;; Without set names, and with thick outlines in the darkened set colours:

(plot/render-image (plot/arrange [{:data memberships
                                   :mapping {:set :club :item :member}
                                   :charts [(chart/venn {:set-labels false})]
                                   :labels {:title ":set-labels false"}}
                                  {:data memberships
                                   :mapping {:set :club :item :member}
                                   :charts [(chart/venn {:stroke-width 4})]
                                   :labels {:title ":stroke-width 4"}}]
                                 {:columns 2})
                   {:width 900 :height 380})

;; The gap between a circle and its set name is a theme key of `[:chart :venn]`, not an option:

(nu/options-table (describe/style-rows :venn [:label-gap]))

(plot/render-image {:data memberships
                    :mapping {:set :club :item :member}
                    :charts [(chart/venn)]
                    :theme {:chart {:venn {:label-gap 16}}}}
                   {:width 500 :height 420})

(nu/shared-groups (describe/options (chart/venn)))
