;; # Charts: annotations

;; Marks placed by literal positions rather than by data: reference lines (`rule`), intervals (`rect`), labels
;; (`text`), a picture (`image`) and curves of functions (`function`). Rule, rect and text also take data and a
;; mapping - one mark per row. Each chart lists the mapping keys it reads, its own options and the shared option
;; groups it takes (documented once in the Shared options chapter).

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

;; ## rule

;; Straight lines across the panel: vertical at an x, horizontal at a y, or sloped y = intercept + slope · x - one
;; per row of data, or at literal positions.

(nu/options-table (describe/mapping (chart/rule)))

(def thresholds [{:at 55 :kind "short wait"} {:at 80 :kind "long wait"}])

(nu/data-table thresholds)

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/scatter)
                             (chart/rule {:data thresholds :mapping {:y :at :color :kind} :line-width 2})]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 360})

;; Its options: literal positions - one or a vector of them, across x (`:x`) or along it (`:y`) - and sloped lines:

(nu/options-table (describe/own-rows (describe/options (chart/rule))))

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

(nu/shared-groups (describe/options (chart/rule)))

;; ## rect

;; Rectangles spanning x and / or y intervals; an open end (`nil`) reaches the panel edge - shaded periods or
;; ranges.

(nu/options-table (describe/mapping (chart/rect)))

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

;; Its options: literal intervals (here on y, with one fill colour each), the fill, its opacity and outline, and
;; how much of a band a level end covers:

(nu/options-table (describe/own-rows (describe/options (chart/rect))))

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

(nu/shared-groups (describe/options (chart/rect)))

;; ## text

;; Labels at positions - from data (one per row, `:label` the text) or literal, also at a fraction of the panel
;; (`[:panel f]`). As an annotation it reads the plot's data but only its own mapping (and the plot's facet keys).

(nu/options-table (describe/mapping (chart/text)))

(def cities [{:city "Oslo" :lon 10.8 :lat 59.9} {:city "Rome" :lon 12.5 :lat 41.9} {:city "Madrid" :lon -3.7 :lat 40.4}
             {:city "Paris" :lon 2.4 :lat 48.9} {:city "Warsaw" :lon 21.0 :lat 52.2}])

(nu/data-table cities)

(plot/render-image {:data cities
                    :mapping {:x :lon :y :lat}
                    :charts [(chart/scatter)
                             (chart/text {:mapping {:x :lon :y :lat :label :city} :anchor [:left :bottom] :offset [4 2]})]
                    :labels {:x "longitude" :y "latitude"}}
                   {:width 550 :height 340})

;; Its options come in four parts. The labels: literal positions and texts, the text role, labels of only the last
;; (or first) row per group:

(nu/options-table (describe/own-rows (describe/options (chart/text))) :labels)

(def growth (for [g ["alpha" "beta" "gamma"] t (range 10)]
              {:g g :t t :v (+ (case g "alpha" 2.0 "beta" 0.0 "gamma" 9.0) (* t (case g "alpha" 1.0 "beta" 1.6 "gamma" 0.4)))}))

(nu/data-table growth)

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


;; Their placement: the anchor and offset, rotation, what the anchor points into and room kept for labels past the
;; data:

(nu/options-table (describe/own-rows (describe/options (chart/text))) :placement)

;; End labels of the first row per group instead (`:select :first`), and what a rotated label's `:anchor` points
;; into: the rotated label's bounding box (`:box`, the default) or the text itself, turned about the anchor point
;; (`:text`) - each label turned upright, anchored at its left top on the red point: the box's top left corner,
;; or the start of the text's top edge:

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

;; `:select` keeps one label per group and label text (`:select-within :group-and-label`, the default) - a label
;; that changes along a line (here its phase; beta's early phase is shorter) gives one end label per text.
;; `:select-within :group` keeps one per group, whatever its label. A `[:panel f]` position also works on an axis no
;; chart maps - the dot plot's vertical:

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

;; The labels' style is a [theme text role](reference.theme.html#text), `:annotation` by default; `:role` picks
;; another - a role the theme adds (here `:callout`, in the bottom right corner), or a builder's (here
;; `:network-label`, small names on white boxes), the inline style keys still over it:

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

;; Crowded labels - left out, moved apart, fitted to a room:

(nu/options-table (describe/own-rows (describe/options (chart/text))) :crowding)

;; `:overlapping-labels :drop` leaves out each label overlapping one drawn before it - the rows'
;; order is the priority (here the larger cities first). A mapped `:x-span` gives each label a room along x in x
;; units (here up to the next city east of it at about its latitude, else the panel's edge): a longer label shrinks,
;; down to its text role's `:min-size` (the `:annotation` role's 6 points by default; a role without one doesn't
;; shrink): on the right the labels with room keep their size, while Vienna (next to Bratislava) and Budapest (near
;; the panel's edge) are drawn smaller to fit their room. Only labels running along x are fitted - unrotated, in an unflipped cartesian plot. Builders use both for their
;; node labels:

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
                                         :labels {:title title :x nil :y nil}})
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

;; `:overlapping-labels :repel` moves such a label instead, in the same priority order: to the nearest free place
;; within `:repel-distance` pixels (default 60) - inside the panel, off the labels placed before it and off the other
;; labels' points (not other charts' marks) - and leaves out only a label with no free place there (a label larger
;; than the panel stays where it is). It works in pixels on the drawn boxes, the same in flipped, polar and faceted
;; plots and for rotated labels. `:label-leader true` joins a label moved more than 6 pixels to its point with a thin
;; line in its colour, ending on the edge of its (turned) box; a place whose leader would cross another label, or
;; whose box would cross another label's leader, isn't free - leaders never run through labels. Labels in boxes
;; (`:background`, `:inset`) are repelled by their whole box - the text with its inset - and kept inside the panel
;; with it (right). Here every car of `mtcars`, heaviest first; a distance of 100 pixels leaves room for the heavy cars'
;; labels to move left of their points at the panel's right edge:

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

;; Text along a path - a label following its line:

(nu/options-table (describe/own-rows (describe/options (chart/text))) :path)

;; `:along-path true` lays each label text along the polyline of the positions of its group with that text, in data
;; order, glyph by glyph turned to the path, upright: data rows of one group and label (a line's own rows - left), or
;; literal positions with one text (a fitted trend line - middle; a curve - right). `:path-position` is where on the path
;; (a fraction of its length; default 0.5), `:anchor` places the text there (`h`: start, centre or end) and on a side
;; of the path (`v`: `:bottom` - above it, `:middle` - on it, `:top` - below it), `:offset [dx dy]` moves it along and
;; up from the path in pixels. The glyphs follow one another along the path shifted to the text's baseline, so they
;; keep their spacing through bends; the text reads upright by the whole path's direction (a vertical path - bottom to
;; top). A `:background` (inline or the text role's) fills a band under the glyphs, `:inset` past them (right). A text
;; longer than its path isn't drawn.

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

(nu/shared-groups (describe/options (chart/text)))

;; ## image

;; A picture drawn into the panel: a function gets the panel's scales, domains and pixel size and returns an
;; image; it reads no mapping. Here a radial gradient covering the data range [-1 1] × [-1 1]:

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

;; Its options: the function and the data range the image covers - without `:domain` the image follows the other
;; charts' domains (here a scatter's), drawn below them:

(nu/options-table (describe/own-rows (describe/options (chart/image))))

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

;; ## function

;; Curves of functions y = f(x), or parametric [x y] = f(t), sampled at drawing; several functions are labelled and
;; may be mapped to colour or dash by the chart's own mapping.

(nu/options-table (describe/mapping (chart/function)))

(plot/render-image {:charts [(chart/function {:f {"sin" #(m/sin %) "cos" #(m/cos %)}
                                              :domain [0 m/TWO_PI]
                                              :mapping {:color :function :dash :function}})]}
                   {:width 600 :height 320})

;; ### Curve

;; The functions, their domain (or `:t` range of a parametric curve), the sampling step, the fill and the chart's own
;; mapping:

(nu/options-table (describe/own-rows (describe/options (chart/function))) :curve)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(* % % 0.5) :domain [-2 2] :fill [0 114 178 60]})]
                                   :labels {:title ":f, :fill"}}
                                  {:charts [(chart/function {:parametric (fn [t] [(m/sin (* 3 t)) (m/sin (* 4 t))])
                                                             :t [0 m/TWO_PI] :fill [200 80 40 60]})]
                                   :aspect :iso
                                   :labels {:title ":parametric (Lissajous)"}}
                                  {:charts [(chart/function {:f #(m/sin %) :domain [0 10] :sample-step 25})]
                                   :labels {:title ":sample-step 25 px"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Gaps

;; A `nil` or non-finite result breaks the curve, or a faint segment bridges the gap:

(nu/options-table (describe/own-rows (describe/options (chart/function))) :gaps)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(m/sqrt (- 1 (* % %))) :domain [-2 2]})]
                                   :labels {:title ":gaps :break (default)"}}
                                  {:charts [(chart/function {:f #(when-not (< 1 % 2) (m/sin %)) :domain [0 6]
                                                             :gaps :bridge :bridge {:alpha-factor 0.6 :dash :dotted}})]
                                   :labels {:title ":gaps :bridge"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; ### Extent

;; Which samples count for the y axis: the 1% - 99% quantiles by default (a pole doesn't stretch the axis), all of
;; them, or those within `:y-limits`:

(nu/options-table (describe/own-rows (describe/options (chart/function))) :extent)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(m/tan %) :domain [0 3]})]
                                   :labels {:title ":y-extent :quantiles (default)"}}
                                  {:charts [(chart/function {:f #(m/tan %) :domain [0 3] :y-extent :min-max})]
                                   :labels {:title ":y-extent :min-max"}}
                                  {:charts [(chart/function {:f #(m/tan %) :domain [0 3] :y-limits [-5 5]})]
                                   :labels {:title ":y-limits [-5 5]"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})


;; The padding of the sampled span: none on x (the curve meets the panel's sides), a quarter of the span on y:

(plot/render-image {:charts [(chart/function {:f #(m/sin %) :domain [0 6.3] :padding {:x [0 0] :y [0.25 0.25]}})]
                    :labels {:title ":padding {:x [0 0] :y [0.25 0.25]}"}}
                   {:width 500 :height 280})

(nu/shared-groups (describe/options (chart/function)))
