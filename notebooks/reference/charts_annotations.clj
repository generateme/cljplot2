;; # Annotations

;; These charts draw marks over the data to point things out: reference lines (`rule`), intervals (`rect`), labels
;; (`text`) and a picture (`image`). `rule`, `rect` and `text` can also draw one mark per row.

(ns reference.charts-annotations
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

(nu/source-note {:dataset :economics})

(def economics (nu/dataset :economics))

(nu/data-table economics)

;; ## Reference lines

;; `chart/rule` draws straight lines across the panel: vertical at an x, horizontal at a y, or sloped,
;; y = intercept + slope · x. Use it for thresholds, means or a trend; it draws a line per row of data, or at positions
;; its options give.

(nu/shared-groups (describe/options (chart/rule)))

(nu/mapping-list (describe/mapping (chart/rule)))

(def thresholds [{:at 55 :kind "short wait"} {:at 80 :kind "long wait"}])

(nu/data-table thresholds)

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/scatter)
                             (chart/rule {:data thresholds :mapping {:y :at :color :kind} :line-width 2})]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 360})

;; Its options place the lines without data:

(nu/option-cards 'chart/rule (describe/own-rows (describe/options (chart/rule))))

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/rule {:x [2 3 4] :dash :dashed})]
                                   :labels {:title ":x [2 3 4]"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter)
                                            (chart/rule {:slope [10.7 12] :intercept [33.5 25] :line-width 2
                                                         :color [200 80 40]})]
                                   :labels {:title ":slope, :intercept (two lines)"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/rule {:y [55 80] :line-width 2 :color [40 140 60]})]
                                   :labels {:title ":y [55 80]"}}]
                                 {:columns 3})
                   {:width 1000 :height 330})

;; ## Rectangles

;; `chart/rect` draws rectangles over x and y intervals, or a band across the panel over one of them; an open end
;; (`nil`) reaches the panel's edge. Use it to shade periods, ranges or regions; list it before the charts it should
;; stay behind.

(nu/shared-groups (describe/options (chart/rect)))

(nu/mapping-list (describe/mapping (chart/rect)))

(def recessions [{:name "1973-75" :from (LocalDate/parse "1973-11-01") :to (LocalDate/parse "1975-03-01")}
                 {:name "1980-82" :from (LocalDate/parse "1980-01-01") :to (LocalDate/parse "1982-11-01")}
                 {:name "1990-91" :from (LocalDate/parse "1990-07-01") :to (LocalDate/parse "1991-03-01")}
                 {:name "2001" :from (LocalDate/parse "2001-03-01") :to (LocalDate/parse "2001-11-01")}
                 {:name "2007-09" :from (LocalDate/parse "2007-12-01") :to (LocalDate/parse "2009-06-01")}])

(nu/data-table recessions)

(plot/render-image {:data economics
                    :mapping {:x #(LocalDate/parse (:date %)) :y :unemploy}
                    :charts [(chart/rect {:data recessions :mapping {:x0 :from :x1 :to}})
                             (chart/line)]
                    :labels {:y "unemployed [thousands]"}}
                   {:width 700 :height 340})

;; Its options give literal intervals, their fill and outline, and how much of a level's band they cover. On the
;; left, two y intervals with a fill colour each; on the right, one rectangle from level b to level c, with
;; `:width 0.6` covering only part of each level's band:

(nu/option-cards 'chart/rect (describe/own-rows (describe/options (chart/rect))))

(def levels-data [{:k "a" :v 3} {:k "b" :v 5} {:k "c" :v 4} {:k "d" :v 6}])

(nu/data-table levels-data)

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/rect {:y [[nil 60] [75 nil]] :fill [[0 114 178] [200 80 40]] :alpha 0.2
                                                         :stroke-color :darker :stroke-width 2})
                                            (chart/scatter)]
                                   :labels {:title ":y intervals, :fill per interval, :stroke-width 2"}}
                                  {:data levels-data :mapping {:x :k :y :v}
                                   :charts [(chart/rect {:x ["b" "c"] :width 0.6 :fill [255 200 0 110]})
                                            (chart/bar)]
                                   :scales {:x {:domain ["a" "b" "c" "d"]}}
                                   :labels {:title ":x [\"b\" \"c\"] :width 0.6"}}]
                                 {:columns 2})
                   {:width 900 :height 330})

;; ## Text labels

;; `chart/text` draws labels at positions: a label per row of data, with `:label` as its text, or at positions its
;; options give, also at a fraction of the panel (`[:panel f]`). Use it to name points, mark values or add notes. It
;; reads the plot's data but takes only the facet keys of the plot's mapping; its positions and texts come from its
;; own `:mapping`.

(nu/shared-groups (describe/options (chart/text)))

(nu/mapping-list (describe/mapping (chart/text)))

(def cities [{:city "Oslo" :lon 10.8 :lat 59.9} {:city "Rome" :lon 12.5 :lat 41.9} {:city "Madrid" :lon -3.7 :lat 40.4}
             {:city "Paris" :lon 2.4 :lat 48.9} {:city "Warsaw" :lon 21.0 :lat 52.2}])

(nu/data-table cities)

(plot/render-image {:data cities
                    :mapping {:x :lon :y :lat}
                    :charts [(chart/scatter)
                             (chart/text {:mapping {:x :lon :y :lat :label :city} :anchor [:left :bottom] :offset [4 2]})]
                    :labels {:x "longitude" :y "latitude"}}
                   {:width 550 :height 340})

;; ### Labels

;; These options give the labels' positions, texts and text role, or keep one label per group at a line's end:

(nu/option-cards 'chart/text (describe/own-rows (describe/options (chart/text))) :labels)

(def growth (for [g ["alpha" "beta" "gamma"] t (range 10)]
              {:g g :t t :v (+ (case g "alpha" 2.0 "beta" 0.0 "gamma" 9.0) (* t (case g "alpha" 1.0 "beta" 1.6 "gamma" 0.4)))}))

(nu/data-table growth)

;; On the left, a note in the panel's top left corner and a label turned by -30 degrees; on the right, `:select :last` names each
;; line at its end:

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter)
                                            (chart/text {:x [:panel 0.02] :y [:panel 0.98] :anchor [:left :top]
                                                         :text "272 eruptions"})
                                            (chart/text {:x 3 :y 70 :text "few here" :rotation -30})]
                                   :labels {:title "[:panel f], :rotation -30"}}
                                  {:data growth :mapping {:x :t :y :v :color :g}
                                   :charts [(chart/line)
                                            (chart/text {:mapping {:x :t :y :v :color :g :label :g}
                                                         :select :last :reserve-room :anchor})]
                                   :labels {:title ":select :last, :reserve-room :anchor"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 330})

;; Here the label changes along each line with its phase (beta's early phase is shorter): the default
;; `:select-within` gives an end label per text, `:group` one per line. A `[:panel f]` position also works on an axis
;; no chart maps, such as the dot plot's vertical:

(plot/render-image (plot/arrange [{:data growth :mapping {:x :t :y :v :color :g}
                                   :charts [(chart/line)
                                            (chart/text {:mapping {:x :t :y :v :color :g
                                                                   :label #(str (:g %) (if (< (:t %) (if (= "beta" (:g %)) 3 5)) " early" " late"))}
                                                         :select :last :select-within :group-and-label :reserve-room :anchor})]
                                   :labels {:title ":select-within :group-and-label"}
                                   :legend false}
                                  {:data growth :mapping {:x :t :y :v :color :g}
                                   :charts [(chart/line)
                                            (chart/text {:mapping {:x :t :y :v :color :g
                                                                   :label #(str (:g %) (if (< (:t %) (if (= "beta" (:g %)) 3 5)) " early" " late"))}
                                                         :select :last :select-within :group :reserve-room :anchor})]
                                   :labels {:title ":select-within :group"}
                                   :legend false}
                                  {:data faithful :mapping {:x :eruptions}
                                   :charts [(chart/dotplot {:binwidth 0.1})
                                            (chart/text {:x [:panel 0.02] :y [:panel 0.95] :anchor [:left :top]
                                                         :text "eruption lengths"})]
                                   :labels {:title "[:panel f] without a y scale"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; `:role` picks the [theme text role](reference.theme.html#text) the labels take their style from: here one the
;; theme adds (`:callout`, bottom right) and a builder's (`:network-label`, small names on white boxes):

(plot/render-image {:data cities
                    :mapping {:x :lon :y :lat}
                    :charts [(chart/scatter)
                             (chart/text {:mapping {:x :lon :y :lat :label :city} :anchor [:left :bottom] :offset [4 2]
                                          :role :network-label})
                             (chart/text {:x [:panel 0.98] :y [:panel 0.03] :anchor [:right :bottom] :text "European cities"
                                          :role :callout})]
                    :theme {:text {:callout {:font "DejaVu Sans Mono" :size 13 :font-style :bold :color [178 34 34]
                                             :inset 2}}}
                    :labels {:x "longitude" :y "latitude"}}
                   {:width 550 :height 340})

;; A text draws `$...$` as a LaTeX formula (left; [Formulas](reference.labels_text.html#formulas) has the rules), as
;; every label in cljplot2 does. `:dollar-math false` keeps the dollars as typed (right); `\$` keeps one dollar
;; literal in a text with formulas:

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter)
                                            (chart/text {:x 3.5 :y 95 :text "mean wait $\\bar{w} = 71$ min"})]
                                   :labels {:title "formula in the text"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter)
                                            (chart/text {:x 3.5 :y 95 :text "mean wait $\\bar{w} = 71$ min" :dollar-math false})]
                                   :labels {:title ":dollar-math false"}}]
                                 {:columns 2})
                   {:width 900 :height 330})

;; ### Placement

;; These options place each label around its position: which point of its box sits there, a shift in pixels, a
;; turn, and room kept past the data:

(nu/option-cards 'chart/text (describe/own-rows (describe/options (chart/text))) :placement)

;; On the left, each line's start label ends 5 pixels before the line begins, with room kept for the
;; labels (`:anchor [:right :middle]`, `:offset [-5 0]`, `:reserve-room :anchor`). In the middle and on the right, a
;; label turned by -90 degrees is anchored at its left top on the red point: `:anchor-frame :box` takes the turned
;; label's bounding box, `:anchor-frame :text` the start of the text's top edge:

(def anchor-points [{:x 1 :y 1 :label "frame"}])

(nu/data-table anchor-points)

(plot/render-image (plot/arrange [{:data growth :mapping {:x :t :y :v :color :g}
                                   :charts [(chart/line)
                                            (chart/text {:mapping {:x :t :y :v :color :g :label :g}
                                                         :select :first :anchor [:right :middle] :offset [-5 0]
                                                         :reserve-room :anchor})]
                                   :labels {:title ":select :first"}
                                   :legend false}
                                  {:data anchor-points :mapping {:x :x :y :y}
                                   :charts [(chart/scatter {:color :red})
                                            (chart/text {:mapping {:x :x :y :y :label :label} :rotation -90 :size 18
                                                         :anchor [:left :top] :anchor-frame :box})]
                                   :scales {:x {:domain [0 2]} :y {:domain [0 2]}}
                                   :labels {:title ":anchor-frame :box"}}
                                  {:data anchor-points :mapping {:x :x :y :y}
                                   :charts [(chart/scatter {:color :red})
                                            (chart/text {:mapping {:x :x :y :y :label :label} :rotation -90 :size 18
                                                         :anchor [:left :top] :anchor-frame :text})]
                                   :scales {:x {:domain [0 2]} :y {:domain [0 2]}}
                                   :labels {:title ":anchor-frame :text"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Crowding

;; These options handle labels that overlap: draw them anyway, leave some out, or move them apart:

(nu/option-cards 'chart/text (describe/own-rows (describe/options (chart/text))) :crowding)

;; From left to right: every label kept, labels overlapping a larger city's left out, then those labels fitted to a
;; room. The rows are sorted by population, so the larger cities win. On the right a mapped `:x-span` gives each label
;; room up to the next city east of it at about its latitude, else to the panel's edge: Vienna (next to Bratislava)
;; and Budapest (near the edge) shrink to fit, down to the `:annotation` role's `:min-size` of 6 points (a role
;; without `:min-size` doesn't shrink). Builders use both for their node labels:

(def capitals [{:city "Paris" :lon 2.35 :lat 48.86 :pop 2.1} {:city "Berlin" :lon 13.40 :lat 52.52 :pop 3.7}
               {:city "Vienna" :lon 16.37 :lat 48.21 :pop 2.0} {:city "Budapest" :lon 19.04 :lat 47.50 :pop 1.7}
               {:city "Prague" :lon 14.42 :lat 50.08 :pop 1.3} {:city "Brussels" :lon 4.35 :lat 50.85 :pop 1.2}
               {:city "Amsterdam" :lon 4.90 :lat 52.37 :pop 0.9} {:city "Bratislava" :lon 17.11 :lat 48.15 :pop 0.5}
               {:city "Luxembourg" :lon 6.13 :lat 49.61 :pop 0.1} {:city "Ljubljana" :lon 14.51 :lat 46.06 :pop 0.3}
               {:city "Zagreb" :lon 15.98 :lat 45.81 :pop 0.8} {:city "Bern" :lon 7.45 :lat 46.95 :pop 0.1}])

(nu/data-table capitals)

(plot/render-image (plot/arrange (map (fn [[title text-chart]]
                                        {:data (sort-by (comp - :pop) capitals)
                                         :mapping {:x :lon :y :lat}
                                         :charts [(chart/scatter) text-chart]
                                         :scales {:x {:domain [0 24]}}
                                         :labels {:title title :x :none :y :none}})
                                      [[":overlapping-labels :keep"
                                        (chart/text {:mapping {:x :lon :y :lat :label :city} :anchor [:left :middle] :offset [4 0]
                                                     :overlapping-labels :keep})]
                                       [":overlapping-labels :drop"
                                        (chart/text {:mapping {:x :lon :y :lat :label :city} :anchor [:left :middle] :offset [4 0]
                                                     :overlapping-labels :drop})]
                                       [":drop, :x-span to the next city"
                                        (chart/text {:mapping {:x :lon :y :lat :label :city
                                                               :x-span (fn [{:keys [lon lat]}]
                                                                         (- (reduce min 24.0 (keep #(when (and (> (:lon %) lon) (< (abs (- (:lat %) lat)) 0.8)) (:lon %))
                                                                                                   capitals))
                                                                            lon))}
                                                     :anchor [:left :middle] :offset [4 0] :overlapping-labels :drop})]])
                                 {:columns 3})
                   {:width 1000 :height 330})

;; `:overlapping-labels :repel` finds a free place inside the panel, off the labels placed before it and off every
;; other label's point, but not off other charts' marks. It compares the drawn boxes in pixels, so it works the same in
;; flipped, polar and faceted plots; a label larger than the panel stays where it is. A leader line (`:label-leader`)
;; never crosses a label. A label in a box (`:background`, `:inset`) keeps its
;; whole box apart and inside the panel (right). Here every car of `mtcars`, heaviest first; `:repel-distance 100`
;; leaves room for the heavy cars' labels to move left of their points at the panel's right edge:

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(plot/render-image (plot/arrange (map (fn [[title text-chart]]
                                        {:data (sort-by (comp - :wt) mtcars)
                                         :mapping {:x :wt :y :mpg}
                                         :charts [(chart/scatter {:size 5}) text-chart]
                                         :labels {:title title :x "weight" :y "mpg"}})
                                      [[":drop"
                                        (chart/text {:mapping {:x :wt :y :mpg :label :model} :size 8 :anchor [:left :middle]
                                                     :offset [3 0] :overlapping-labels :drop})]
                                       [":repel, :repel-distance 100"
                                        (chart/text {:mapping {:x :wt :y :mpg :label :model} :size 8 :anchor [:left :middle]
                                                     :offset [3 0] :overlapping-labels :repel :repel-distance 100
                                                     :label-leader true})]
                                       [":repel in label boxes"
                                        (chart/text {:mapping {:x :wt :y :mpg :label :model} :size 8 :anchor [:left :middle]
                                                     :offset [3 0] :overlapping-labels :repel :repel-distance 100 :label-leader true
                                                     :background [255 255 255 210] :inset 2})]])
                                 {:columns 3})
                   {:width 1000 :height 420})

;; ### Path

;; These options lay a label's text along a line instead of at a point:

(nu/option-cards 'chart/text (describe/own-rows (describe/options (chart/text))) :path)

;; The path comes from a line's own rows (left) or from literal positions with one text: a fitted trend line (middle)
;; and a sine wave (right). The second value of `:anchor` picks the side of the path: `:bottom` puts the text's
;; bottom on the path, so the text sits above it; `:middle` centres it on the path; `:top` hangs it below.
;; `:offset [dx dy]` moves the text along the path and up from it in pixels. The letters keep their spacing through
;; bends and read upright by the path's overall direction (a vertical path reads bottom to top). A `:background`,
;; inline or the role's, fills a band under the letters, `:inset` past them (right); a text longer than its path isn't
;; drawn.

(let [eruptions (map :eruptions faithful)
      waiting (map :waiting faithful)
      slope (/ (stats/covariance eruptions waiting) (stats/variance eruptions))
      intercept (- (stats/mean waiting) (* slope (stats/mean eruptions)))
      xs [1.5 5.2]
      ys (mapv #(+ intercept (* slope %)) xs)
      wave (m/slice-range 0.0 m/TWO_PI 60)]
  (plot/render-image (plot/arrange [{:data growth :mapping {:x :t :y :v :color :g}
                                     :charts [(chart/line)
                                              (chart/text {:mapping {:x :t :y :v :color :g :label :g} :along-path true
                                                           :path-position 0.7 :anchor [:center :bottom] :offset [0 2]})]
                                     :labels {:title "data: along each line"}
                                     :legend false}
                                    {:data faithful :mapping {:x :eruptions :y :waiting}
                                     :charts [(chart/scatter {:size 4})
                                              (chart/function {:f #(+ intercept (* slope %)) :domain [1.5 5.2]})
                                              (chart/text {:x xs :y ys :text (format "waiting = %.1f + %.1f eruptions" intercept slope)
                                                           :along-path true :path-position 0.6 :anchor [:center :bottom]})]
                                     :labels {:title "literal: along a trend line"}}
                                    {:data (map (fn [x] {:x x :y (m/sin x)}) wave) :mapping {:x :x :y :y}
                                     :charts [(chart/line)
                                              (chart/text {:x (vec wave) :y (mapv m/sin wave) :text "a text along a sine wave"
                                                           :along-path true :path-position 0.0 :anchor [:left :bottom] :offset [0 2]})
                                              (chart/text {:x (vec wave) :y (mapv m/sin wave) :text "below the trough" :along-path true
                                                           :path-position 0.75 :anchor [:center :top]
                                                           :background [255 240 160] :inset 2})]
                                     :labels {:title "[:left :bottom], [:center :top]"}}]
                                   {:columns 3})
                     {:width 1000 :height 330}))

;; ## Images

;; `chart/image` draws a picture into the panel: a function takes the panel's scales, domains and pixel size and
;; returns the image. Use it for a background picture, a geographic map or a raster computed per panel; it reads no
;; data. Here a radial gradient over the data range [-1 1] × [-1 1], which `:domain` sets:

(plot/render-image {:charts [(chart/image {:domain {:x [-1 1] :y [-1 1]}
                                           :f (fn [{[w h] :size}]
                                                (let [w (int w) h (int h)
                                                      img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
                                                  (doseq [i (range w) j (range h)
                                                          :let [d (m/hypot (- (/ (* 2.0 i) w) 1) (- (/ (* 2.0 j) h) 1))
                                                                c (int (* 255 (max 0.0 (- 1.0 (* 0.7 d)))))]]
                                                    (.setRGB img i j (+ (* 65536 c) (* 256 (quot c 2)) 80)))
                                                  img))})]}
                   {:width 450 :height 350})

;; Its options give the function and the data range the axes must show; the image always fills the panel. Without
;; `:domain` the axes follow the other charts' domains, here a scatter's, which draws over the image:

(nu/option-cards 'chart/image (describe/own-rows (describe/options (chart/image))))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/image {:f (fn [{[w h] :size}]
                                                (let [w (int w) h (int h)
                                                      img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
                                                  (doseq [i (range w) j (range h)
                                                          :let [c (int (+ 180 (* 60 (/ (double i) w))))]]
                                                    (.setRGB img i j (+ (* 65536 c) (* 256 c) 235)))
                                                  img))})
                             (chart/scatter)]}
                   {:width 550 :height 340})
