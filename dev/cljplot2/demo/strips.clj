(ns cljplot2.demo.strips
  "1d strips demo (cycle 3): box, violin (inner marks, split), stripplot, rug, extent; groups dodged, flipped vertical
  strips, overlays (3a); beeswarm, histogram and density ridgelines, `:bottom` placement (3b)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(defn- read-json [file] (json/read-str (slurp (io/resource file)) :key-fn keyword))

(def barley (read-json "barley.json"))
(def faithful (read-json "faithful.json"))
(def chem97 (let [rng (r/rng :mersenne 3)] (vec (filter (fn [_] (< (r/drandom rng) 0.05)) (read-json "chem97.json")))))

(def box-barley
  {:data barley
   :mapping {:x :yield :y :variety}
   :charts [(chart/box)]
   :labels {:title "Box: barley yield by variety" :x "yield"}})

(def box-grouped
  {:data barley
   :mapping {:x :yield :y :site :color :year}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/box {:mean true})]
   :labels {:title "Box: dodged by year, mean marker" :x "yield"}})

(def box-vertical
  {:data chem97
   :mapping {:x :gcsescore :y :score}
   :scales {:y {:kind :bands}}
   :flip true
   :charts [(chart/box {:varwidth true})]
   :labels {:title "Box: vertical (:flip), :varwidth, numeric levels as bands" :x "gcsescore" :y "score"}})

(defn- violin [inner]
  {:data chem97
   :mapping {:x :gcsescore :y :score}
   :scales {:y {:kind :bands}}
   :charts [(chart/violin {:inner inner})]
   :labels {:title (str "Violin :inner " inner) :x "gcsescore"}})

(def violins (plot/arrange (map violin [:box :quartiles :rug :points]) {:columns 2}))

(def violin-split
  {:data chem97
   :mapping {:x :gcsescore :y :score :color :gender}
   :scales {:y {:kind :bands}}
   :flip true
   :charts [(chart/violin {:position :mirror :inner :quartiles})]
   :labels {:title "Split violin by gender (:position :mirror), vertical" :x "gcsescore" :y "score"}})

(def violin-count
  {:data chem97
   :mapping {:x :gcsescore :y :score}
   :scales {:y {:kind :bands}}
   :charts [(chart/violin {:scale :count})]
   :labels {:title "Violin :scale :count" :x "gcsescore"}})

(def box-strip
  {:data barley
   :mapping {:x :yield :y :variety}
   :charts [(chart/box {:outliers false :color [200 200 220]}) (chart/stripplot {:rng (r/rng :mersenne 1) :jitter 0.6})]
   :labels {:title "Box with jittered points" :x "yield"}})

(def strip-grouped
  {:data barley
   :mapping {:x :yield :y :variety :color :year}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/stripplot {:rng (r/rng :mersenne 1) :size 6})]
   :labels {:title "Stripplot: dodged by year" :x "yield"}})

(def density-rug
  {:data faithful
   :mapping {:x :eruptions}
   :charts [(chart/density) (chart/rug)]
   :labels {:title "Density with a rug (:edge)" :x "eruptions"}})

(def histogram-box
  {:data faithful
   :mapping {:x :eruptions}
   :charts [(chart/histogram {:bins 20}) (chart/box {:place {:at 14.5} :thickness 16})]
   :labels {:title "Histogram with a box above it ({:at 14.5})" :x "eruptions"}})

(defn- extent [method args]
  {:data barley
   :mapping {:x :yield :y :site}
   :charts [(chart/extent (cond-> {:method method} args (assoc :method-args args)))]
   :labels {:title (str "Extent " method) :x "yield"}})

(def extents (plot/arrange [(extent :min-max nil) (extent :stddev nil) (extent :ci nil) (extent :percentile [10.0 90.0])]
                           {:columns 2}))

(def single-box
  {:data faithful
   :mapping {:x :waiting}
   :charts [(chart/violin {:color [200 200 220]}) (chart/box {:width 0.15})]
   :labels {:title "No :y - centred violin with a narrow box" :x "waiting"}})

(def beeswarm-chem
  {:data chem97
   :mapping {:x :gcsescore :y :score :color :gender}
   :scales {:y {:kind :bands}}
   :charts [(chart/beeswarm {:size 4})]
   :labels {:title "Beeswarm: dodged by gender (:fit)" :x "gcsescore" :y "score"}})

(def beeswarm-barley
  {:data barley
   :mapping {:x :yield :y :site}
   :flip true
   :charts [(chart/box {:outliers false :color [220 220 230]}) (chart/beeswarm {:size 8 :fit false})]
   :labels {:title "Beeswarm over boxes, vertical, :fit false" :x "yield"}})

(def ridges-density
  {:data chem97
   :mapping {:x :gcsescore :y :score}
   :scales {:y {:kind :bands}}
   :charts [(chart/density {:fill-gradient :mpl/viridis})]
   :labels {:title "Density ridgelines, :fill-gradient" :x "gcsescore" :y "score"}})

(def ridges-histogram
  {:data chem97
   :mapping {:x :gcsescore :y :score}
   :scales {:y {:kind :bands}}
   :charts [(chart/histogram {:bins 25 :overlap 1.5})]
   :labels {:title "Histogram ridgelines, :overlap 1.5" :x "gcsescore" :y "score"}})

(def ridges-mirror
  {:data chem97
   :mapping {:x :gcsescore :y :score :color :gender}
   :scales {:y {:kind :bands}}
   :charts [(chart/density {:position :mirror :overlap 0.45})]
   :labels {:title "Double ridgeline by gender (:position :mirror)" :x "gcsescore" :y "score"}})

(def density-bottom
  {:data faithful
   :mapping {:x :eruptions}
   :charts [(chart/density) (chart/stripplot {:place :bottom :rng (r/rng :mersenne 2) :size 6})]
   :labels {:title "Density with jittered points below (:place :bottom)" :x "eruptions"}})

(def density-bottom-rug
  {:data faithful
   :mapping {:x :eruptions}
   :flip true
   :charts [(chart/density {:fill [69 117 180 60]}) (chart/rug {:place :bottom :bottom-share 0.08 :width 1.0})]
   :labels {:title "Density with a :bottom rug, vertical" :x "eruptions"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/strips/` by [[cljplot2.demo/render!]]."
  [["box-barley-700x500.png" box-barley {:width 700 :height 500}]
   ["box-grouped-700x500.png" box-grouped {:width 700 :height 500}]
   ["box-vertical-600x500.png" box-vertical {:width 600 :height 500}]
   ["violins-900x700.png" violins {:width 900 :height 700}]
   ["violin-split-600x500.png" violin-split {:width 600 :height 500}]
   ["violin-count-600x400.png" violin-count {:width 600 :height 400}]
   ["box-strip-700x500.png" box-strip {:width 700 :height 500}]
   ["strip-grouped-700x500.png" strip-grouped {:width 700 :height 500}]
   ["density-rug-600x400.png" density-rug {:width 600 :height 400}]
   ["histogram-box-600x400.png" histogram-box {:width 600 :height 400}]
   ["extents-900x600.png" extents {:width 900 :height 600}]
   ["single-box-600x300.png" single-box {:width 600 :height 300}]
   ["beeswarm-chem-800x600.png" beeswarm-chem {:width 800 :height 600}]
   ["beeswarm-barley-700x500.png" beeswarm-barley {:width 700 :height 500}]
   ["ridges-density-700x600.png" ridges-density {:width 700 :height 600}]
   ["ridges-histogram-700x600.png" ridges-histogram {:width 700 :height 600}]
   ["ridges-mirror-700x600.png" ridges-mirror {:width 700 :height 600}]
   ["density-bottom-600x400.png" density-bottom {:width 600 :height 400}]
   ["density-bottom-rug-400x500.png" density-bottom-rug {:width 400 :height 500}]])
