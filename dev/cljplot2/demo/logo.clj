(ns cljplot2.demo.logo
  "The cljplot2 logo (after old cljplot's): the word \"cljplot2\" as 2500 jittered points sampled from its rendered
  text, coloured along x, of random sizes, with density margins on top and right. Written to the tracked `img/` (the
  README's logo); seeded and drawn in Comfortaa Bold (SIL OFL 1.1, `dev/resources/logo/` - demo only, not in the
  library), so every render is the same."
  (:require [clojure.java.io :as io]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.graphics.font :as font])
  (:import [java.awt Font]))

(def output-dir
  "The logo is tracked in git (see [[cljplot2.demo]])."
  "img")

(def ^:private text-width 250)
(def ^:private text-height 85)

(def ^:private logo-font
  "Comfortaa Bold - round `c`, `o`, `p` - from the demo's classpath resources."
  (delay (with-open [in (io/input-stream (io/resource "logo/Comfortaa-Bold.ttf"))]
           (Font/createFont Font/TRUETYPE_FONT in))))

(defn- text-pixels
  "Pixels of `text` drawn white on black in Comfortaa Bold, `text-width` x `text-height`."
  [text]
  (p/to-pixels (c2d/with-canvas-> (c2d/canvas text-width text-height)
                 (c2d/set-background :black)
                 (c2d/set-color :white)
                 (c2d/set-font (font/resolve-font @logo-font :bold 56))
                 (c2d/text text 4 62))))

(defn- lit? [pixels x y] (pos? (double (c/luma (p/get-color pixels x y)))))

(defn logo-points
  "`n` rows `{:x :y :size}`: random lit pixels of `text` (y up) moved by Gaussian jitter, a size `10 u^3` (u uniform
  in [0.1 1]) each - seeded by `seed`."
  [text n seed]
  (let [pixels (text-pixels text)
        g (r/rng :mersenne seed)]
    (->> (repeatedly #(vector (r/irandom g text-width) (r/irandom g text-height)))
         (filter (fn [[x y]] (lit? pixels x y)))
         (take n)
         (mapv (fn [[x y]] {:x (+ (double x) (r/grandom g))
                            :y (+ (double (- text-height (long y))) (r/grandom g))
                            :size (* 10.0 (m/pow (r/drandom g 0.1 1.0) 3.0))})))))

(def ^:private margin-density
  (chart/density {:fill [60 60 130 170] :color [40 40 100] :mapping {:color nil :size nil}}))

(def ^:private gradient
  "Dark to mid colours only (blue, purple, red, orange): every letter stands out on the light panel."
  [[30 50 140] [120 50 160] [200 40 90] [230 110 20]])

(def logo
  {:data (logo-points "cljplot2" 2500 2)
   :mapping {:x :x :y :y :color :x :size :size}
   :charts [(chart/scatter {:stroke-color :none})]
   :scales {:color {:range (mapv #(c/set-alpha (c/color %) 110) gradient) :legend false}
            :size {:range [0.5 10] :area true :legend false}}
   :margins {:top {:charts margin-density :size 22} :right {:charts margin-density :size 22}}
   :labels {:x "cljplot2 - functional plotting library" :y nil}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to [[output-dir]] by [[cljplot2.demo/render!]]."
  [["logo.png" logo {:width 600 :height 300}]])
