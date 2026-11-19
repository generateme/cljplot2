(ns cljplot2.demo.lattice-3d
  "Lattice cycle 3 demo (3D): clouds - LMDVR Fig 6.1 (quakes by magnitude), Fig 6.3 (one cloud from four views), Fig
  6.4 (spikes), Fig 6.15 (3D bars on categorical axes), side projections (also without the cloud), a 2D histogram;
  surfaces - Fig 6.8 (faceted model surfaces), Fig 6.11c / 13.7 (a shaded wireframe), Fig 6.18 (a parametric
  surface), height and color from one function, a density surface and a cloud over a fitted plane; perspective
  (`:view {:distance 0.2}`, lattice's) - a turning cloud, 3D bars, a shaded wireframe; on the datasets of
  `notebooks/data` and synthetic functions."
  (:require [fastmath.core :as m]
            [fastmath.ml.regression :as regression]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.demo.data :as data]))

(def ^:private quakes (data/json-rows "quakes.json"))

(def fig-6-1
  "Fig 6.1: earthquake locations and depths (depth growing downward), a panel per magnitude interval."
  {:data (shingle/shingle quakes :mag {:count 4 :overlap 0.1 :as :magnitude :label "Magnitude"})
   :mapping {:x :long :y :lat :z :depth :facet :magnitude}
   :charts [(chart/cloud {:domain {:z [690 40]} :view {:azimuth 105 :elevation 20} :size 3
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
   :facets {:columns 2}})

(defn- view-panel
  "The quakes cloud seen from `azimuth` / `elevation` (perspective `distance`, default 0 - none), axes labelled so the
  turn reads; the box wider east-west than north-south (about the data's extent) - a cube looks the same every 90
  degrees."
  ([azimuth elevation] (view-panel azimuth elevation 0))
  ([azimuth elevation distance]
   {:data quakes
    :mapping {:x :long :y :lat :z :depth}
    :charts [(chart/cloud {:domain {:z [690 40]} :view (cond-> {:azimuth azimuth :elevation elevation}
                                                          (pos? (double distance)) (assoc :distance distance))
                           :size 2 :aspect [1 1.3 0.8] :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
    :labels {:title (str "azimuth " azimuth ", elevation " elevation)}}))

(def fig-6-3
  "Fig 6.3: one cloud turned about its vertical axis - four azimuths 40 degrees apart at one elevation (one box size,
  see [[cljplot2.data.space3d/projector]])."
  (plot/arrange [(view-panel 20 30) (view-panel 60 30) (view-panel 100 30) (view-panel 140 30)] {:columns 2}))

(def fig-6-4
  "Fig 6.4 (quakes instead of US states): the number of stations reporting each of the strongest earthquakes as
  spikes over their location."
  {:data (filterv #(>= (double (:mag %)) 5.5) quakes)
   :mapping {:x :long :y :lat :z :stations :color :mag}
   :scales {:color {:range [[120 170 220] [10 40 110]]}}
   :charts [(chart/cloud {:type :spikes :stroke-width 2 :domain {:z [0 140]} :view {:azimuth 30 :elevation 25}
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Stations"}})]})

(def groups
  "Groups in 3D: quakes colored by magnitude band (a legend) and sized by the number of stations reporting them."
  {:data (mapv #(assoc % :band (cond (< (double (:mag %)) 4.5) "< 4.5" (< (double (:mag %)) 5.0) "4.5 - 5" :else ">= 5")) quakes)
   :mapping {:x :long :y :lat :z :depth :color :band :size :stations}
   :scales {:color {:domain ["< 4.5" "4.5 - 5" ">= 5"]} :size {:range [1 8]}}
   :charts [(chart/cloud {:domain {:z [690 40]} :view {:azimuth 30 :elevation 25}
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
   :labels {:title "Magnitude band (color) and stations (size)"}})

(def ^:private chem97
  (mapv (fn [row] {:score (parse-long (:score row)) :gender (:gender row) :gcsescore (parse-double (:gcsescore row))})
        (data/csv-rows "chem97.csv")))

(def ^:private chem97-table
  "Chem97 counts by A-level score, GCSE score decile (intervals, see [[cljplot2.data.levels/bin]]) and gender - every
  combination, zeros included (R's `as.data.frame(xtabs(~ score + gcd + gender))`)."
  (let [binned (levels/bin chem97 :gcsescore {:as :gcd :bins {:quantiles 10}})
        counts (frequencies (map (juxt :score :gcd :gender) binned))]
    (for [gender (sort (distinct (map :gender binned)))
          score (sort (distinct (map :score binned)))
          gcd (sort (distinct (map :gcd binned)))]
      {:score score :gcd gcd :gender gender :count (get counts [score gcd gender] 0)})))

(def fig-6-15
  "Fig 6.15: Chem97 counts of A-level score by GCSE score decile as 3D bars on categorical axes (6 x 10 per panel), a
  panel per gender."
  {:data chem97-table
   :mapping {:x (levels/categorical :score) :y :gcd :z :count :facet :gender}
   :charts [(chart/cloud {:type :bars :base 0 :color [150 190 230] :stroke-color [60 60 60] :stroke-width 1
                          :view {:azimuth 220 :elevation 42} :aspect [1 1.5 0.8]
                          :axis-titles {:x "A-level score" :y "GCSE score decile" :z "Count"}})]
   :facets {:columns 2}})

(def projections
  "The quakes cloud with its side projections: density bands on the floor, density lines on the longitude-depth
  wall, points on the latitude-depth wall."
  {:data quakes
   :mapping {:x :long :y :lat :z :depth}
   :charts [(chart/cloud {:domain {:z [690 40]} :size 2 :view {:azimuth 40 :elevation 25}
                          :projections {:xy :density :xz :contour :yz :points}
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
   :labels {:title "Side projections"}})

(def projections-only
  "Side projections without the cloud (`:type :none`): the quakes' density bands on all three walls."
  {:data quakes
   :mapping {:x :long :y :lat :z :depth}
   :charts [(chart/cloud {:type :none :domain {:z [690 40]} :view {:azimuth 40 :elevation 25}
                          :projections {:xy :density :xz :density :yz :density}
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
   :labels {:title "Projections only"}})

(def histogram-2d
  "A 2D histogram: quakes counted on a 12 x 12 longitude-latitude grid, the counts as 3D bars."
  {:data quakes
   :mapping {:x :long :y :lat}
   :charts [(chart/cloud {:bins [12 12] :type :bars :color [150 190 230] :stroke-color [60 60 60] :stroke-width 1
                          :view {:azimuth 35 :elevation 35} :axis-titles {:x "Longitude" :y "Latitude" :z "Count"}})]
   :labels {:title "Quakes per cell"}})

(def ^:private models
  "Synthetic model surfaces over [-2 2]^2 (Fig 6.8 shows four fitted models of one response)."
  {"plane" (fn [[x y]] (+ (* 0.3 x) (* 0.2 y)))
   "saddle" (fn [[x y]] (* 0.25 (- (* x x) (* y y))))
   "bump" (fn [[x y]] (m/exp (- (* 0.8 (+ (* x x) (* y y))))))
   "ripple" (fn [[x y]] (* 0.6 (m/sin (* 1.5 x)) (m/cos (* 1.5 y))))})

(def fig-6-8
  "Fig 6.8 (synthetic): four model surfaces on one gridded table, a panel per model, one z domain; colored by height."
  {:data (for [[model f] models x (range -2.0 2.01 0.25) y (range -2.0 2.01 0.25)]
           {:model model :x x :y y :z (f [x y])})
   :mapping {:x :x :y :y :z :z :facet :model}
   :charts [(chart/surface {:style :both :view {:azimuth 35 :elevation 30}})]
   :facets {:columns 2}
   :scales {:color {:range [[40 90 160] [250 250 250] [190 60 40]]}}})

(defn- volcano
  "A volcano-like height at `[x y]`: a ring ridge with a crater, a little east-west asymmetry."
  [[x y]]
  (let [r (m/hypot x y)]
    (+ (* 1.2 (m/exp (- (/ (m/sq (- r 1.0)) 0.18)))) (* -0.5 (m/exp (- (/ (* r r) 0.08)))) (* 0.15 x (m/exp (- (* r r)))))))

(def fig-6-11c
  "Fig 6.11c / 13.7: a shaded wireframe - one translucent color, faces lit by their slope, outlined; the crater's cone
  below the ground shows through the near slope."
  {:charts [(chart/surface {:f volcano :domain {:x [-2 2] :y [-2 2]} :n 41 :style :both :color [120 170 110 150]
                            :stroke-color [40 60 40] :aspect [1 1 0.5] :view {:azimuth 30 :elevation 35}})]
   :labels {:title "Volcano-like surface"}})

(def fig-6-18
  "Fig 6.18: a parametric surface - a torus from u, v angles, colored by height."
  {:charts [(chart/surface {:fu (fn [[u v]] [(* (+ 2.0 (m/cos v)) (m/cos u)) (* (+ 2.0 (m/cos v)) (m/sin u)) (m/sin v)])
                            :u [0.0 m/TWO_PI] :v [0.0 m/TWO_PI] :n [48 24] :aspect [1 1 0.34] :view {:azimuth 30 :elevation 40}})]
   :scales {:color {:range [[40 70 140] [120 190 200] [250 240 200]]}}
   :labels {:title "Torus"}})

(def height-and-color
  "A function giving height and color value `[z c]`: ripples by the distance from the centre, colored by x·y (a
  quad's color is its corners' mean - the color value should be continuous)."
  {:charts [(chart/surface {:f (fn [[x y]] [(m/cos (* 2.0 (m/hypot x y))) (* x y)])
                            :domain {:x [-3 3] :y [-3 3]} :n 40 :aspect [1 1 0.4] :view {:azimuth 30 :elevation 40}})]
   :scales {:color {:range [[40 90 160] [245 245 235] [190 60 40]] :title "x·y"}}
   :labels {:title "Height and color from one function"}})

(def density-surface
  "The 2D kernel density of the quakes' locations as a surface."
  {:data quakes
   :mapping {:x :long :y :lat}
   :charts [(chart/surface {:density true :n 40 :view {:azimuth 30 :elevation 35}
                            :axis-titles {:x "Longitude" :y "Latitude" :z "Density"}})]
   :scales {:color {:range [[250 250 220] [240 140 40] [120 20 20]]}}})

(def ^:private slab
  "Least-squares plane of quake depth on longitude and latitude."
  (regression/lm (mapv :depth quakes) (mapv (juxt :long :lat) quakes)))

(def cloud-over-surface
  "A cloud over a surface in one box: the quakes and the least-squares plane of their depth (explicit domains on
  both charts)."
  (let [domain {:x [165 190] :y [-40 -10] :z [690 40]}]
    {:data quakes
     :mapping {:x :long :y :lat :z :depth}
     ;; the first 3D chart's frame (view, titles) is the box's
     :charts [(chart/surface {:f #(slab %) :domain domain :n 12 :color [200 200 200 160] :shade false
                              :style :both :stroke-color [120 120 120] :view {:azimuth 50 :elevation 15}
                              :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})
              (chart/cloud {:domain domain :size 2 :color [180 40 40 200]})]
     :labels {:title "Quakes and their least-squares plane"}}))

(def perspective-views
  "Fig 6.3 in perspective (lattice's distance 0.2): one box size and place at every azimuth, near corners larger."
  (plot/arrange (mapv #(view-panel % 30 0.2) [20 60 100 140]) {:columns 2}))

(def perspective-bars
  "The 2D histogram of quakes in perspective: bars nearer the eye larger, each showing the faces turned to the eye."
  {:data quakes
   :mapping {:x :long :y :lat}
   :charts [(chart/cloud {:bins [12 12] :type :bars :color [150 190 230] :stroke-color [60 60 60] :stroke-width 1
                          :view {:azimuth 35 :elevation 35 :distance 0.2}
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Count"}})]
   :labels {:title "Quakes per cell, perspective"}})

(def perspective-wireframe
  "Fig 6.11c / 13.7 in perspective: the shaded volcano-like wireframe seen from distance 0.2."
  {:charts [(chart/surface {:f volcano :domain {:x [-2 2] :y [-2 2]} :n 41 :style :both :color [120 170 110 150]
                            :stroke-color [40 60 40] :aspect [1 1 0.5] :view {:azimuth 30 :elevation 35 :distance 0.2}})]
   :labels {:title "Volcano-like surface, perspective"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/lattice-3d/` by [[cljplot2.demo/render!]]."
  [["fig-6-1-quakes-800x700.png" fig-6-1 {:width 800 :height 700}]
   ["fig-6-3-views-800x700.png" fig-6-3 {:width 800 :height 700}]
   ["fig-6-4-spikes-600x500.png" fig-6-4 {:width 600 :height 500}]
   ["fig-6-15-bars-1000x550.png" fig-6-15 {:width 1000 :height 550}]
   ["groups-700x550.png" groups {:width 700 :height 550}]
   ["projections-700x600.png" projections {:width 700 :height 600}]
   ["projections-only-700x600.png" projections-only {:width 700 :height 600}]
   ["histogram-2d-700x600.png" histogram-2d {:width 700 :height 600}]
   ["fig-6-8-models-800x700.png" fig-6-8 {:width 800 :height 700}]
   ["fig-6-11c-volcano-700x550.png" fig-6-11c {:width 700 :height 550}]
   ["fig-6-18-torus-700x550.png" fig-6-18 {:width 700 :height 550}]
   ["height-and-color-700x550.png" height-and-color {:width 700 :height 550}]
   ["density-surface-700x600.png" density-surface {:width 700 :height 600}]
   ["cloud-over-surface-700x600.png" cloud-over-surface {:width 700 :height 600}]
   ["perspective-views-800x700.png" perspective-views {:width 800 :height 700}]
   ["perspective-bars-700x600.png" perspective-bars {:width 700 :height 600}]
   ["perspective-wireframe-700x550.png" perspective-wireframe {:width 700 :height 550}]])
