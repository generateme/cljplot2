(ns cljplot2.demo.lattice-3d
  "Lattice cycle 3a demo (3D clouds): LMDVR Fig 6.1 (quakes by magnitude), Fig 6.3 (one cloud from four views), Fig
  6.4 (spikes) and Fig 6.15 (3D bars) on the datasets of `notebooks/data`."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
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
  "The quakes cloud seen from `azimuth` / `elevation`, axes labelled so the turn reads; the box wider east-west than
  north-south (about the data's extent) - a cube looks the same every 90 degrees."
  [azimuth elevation]
  {:data quakes
   :mapping {:x :long :y :lat :z :depth}
   :charts [(chart/cloud {:domain {:z [690 40]} :view {:azimuth azimuth :elevation elevation} :size 2 :aspect [1 1.3 0.8]
                          :axis-titles {:x "Longitude" :y "Latitude" :z "Depth"}})]
   :labels {:title (str "azimuth " azimuth ", elevation " elevation)}})

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

(def fig-6-15
  "Fig 6.15: Chem97 counts of A-level score by rounded GCSE score as 3D bars, a panel per gender."
  {:data (for [[[score gcd gender] n] (frequencies (map (juxt :score #(m/round (double (:gcsescore %))) :gender) chem97))]
           {:score score :gcd gcd :gender gender :count n})
   :mapping {:x :score :y :gcd :z :count :facet :gender}
   :charts [(chart/cloud {:type :bars :base 0 :color [150 190 230] :stroke-color [60 60 60] :stroke-width 1
                          :view {:azimuth 220 :elevation 42} :aspect [1 1.5 0.8]
                          :axis-titles {:x "A-level score" :y "GCSE score" :z "Count"}})]
   :facets {:columns 2}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/lattice-3d/` by [[cljplot2.demo/render!]]."
  [["fig-6-1-quakes-800x700.png" fig-6-1 {:width 800 :height 700}]
   ["fig-6-3-views-800x700.png" fig-6-3 {:width 800 :height 700}]
   ["fig-6-4-spikes-600x500.png" fig-6-4 {:width 600 :height 500}]
   ["fig-6-15-bars-900x450.png" fig-6-15 {:width 900 :height 450}]
   ["groups-700x550.png" groups {:width 700 :height 550}]])
