(ns cljplot2.field-antialias-test
  "Tests of antialiased rasters: `graphics.raster/raster-pixels`, the field chart's `:antialias` and the clojure2d renderer's coverage alpha, background blend and centred footprint."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.core :as plot]
            [cljplot2.chart :as chart]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.raster :as raster]
            [cljplot2.test-util :refer [error-ins pixels same-pixels?]])
  (:import [fastmath.vector Vec2 Vec4]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- alpha
  "The alpha (0-255) of pixel `x`, `y`."
  ^double [px x y]
  (.w ^Vec4 (p/get-color px x y)))

(defn- alphas
  "Every pixel's alpha of `w x h` pixels."
  [px ^long w ^long h]
  (for [y (range h) x (range w)] (alpha px x y)))

(defn- near?
  "Tests whether doubles `a` and `b` differ by at most `tolerance`."
  [^double tolerance ^double a ^double b]
  (m/<= (m/abs (m/- a b)) tolerance))

(defn- rgb-near?
  "Tests whether the RGB of colours `a` and `b` differ by at most 1 per channel."
  [a b]
  (every? (fn [[^double u ^double v]] (near? 1.0 u v)) (map vector (take 3 (c/to-color a)) (take 3 (c/to-color b)))))

(defn- opaque-or-empty?
  "Tests whether an alpha is 0 or 255."
  [^double a]
  (or (m/zero? a) (m/== 255.0 a)))

(defn- disc-color
  "A colour of a canvas point inside the disc of radius `r` around (`cx`, `cy`), `nil` outside."
  [^double cx ^double cy ^double r col]
  (fn [^Vec2 p] (when (m/< (m/hypot (m/- (.x p) cx) (m/- (.y p) cy)) r) col)))

(defn- raw
  "The ints of pixels `px` (compared by value)."
  [px]
  (vec (.p ^clojure2d.pixels.Pixels px)))

(def ^:private red (c/color 200 40 40))

(def ^:private disc
  "A red disc of radius 6.3 centred in a 20 x 20 raster."
  (disc-color 10.0 10.0 6.3 red))

(deftest antialias-settings-test
  (testing "off: nil and false; true: the defaults; a map over the defaults, nil entries (also in :to-pixels) keep them"
    (is (nil? (raster/antialias-settings nil)))
    (is (nil? (raster/antialias-settings false)))
    (is (= raster/antialias-defaults (raster/antialias-settings true)))
    (is (= {:splats 2 :filter :sinc :radius 1.1 :filter-params nil :to-pixels nil} raster/antialias-defaults))
    (is (= (assoc raster/antialias-defaults :splats 3 :to-pixels {}) (raster/antialias-settings {:splats 3})))
    (is (= (assoc raster/antialias-defaults :to-pixels {}) (raster/antialias-settings {:filter nil :radius nil})))
    (is (= {:saturation 1.5} (:to-pixels (raster/antialias-settings {:to-pixels {:saturation 1.5 :background nil}}))))))

(deftest raster-pixels-test
  (testing "off: one sample per pixel - alpha 0 or 255 only"
    (doseq [aa [nil false]]
      (is (every? opaque-or-empty? (alphas (raster/raster-pixels 20 20 disc aa) 20 20)) aa)))
  (testing "on: the disc's edge blends - some alpha strictly between 0 and 255; inside opaque in the colour, far outside transparent"
    (doseq [aa [true {:splats 1} {:splats 3} {:splats 8} {:filter :gaussian :radius 1.5} {:filter :box :radius 0.5} {:radius 5.0}]]
      (let [px (raster/raster-pixels 20 20 disc aa)]
        (is (some #(m/< 0.0 ^double % 255.0) (alphas px 20 20)) aa)
        (is (m/== 255.0 (alpha px 10 10)) aa)
        (is (m/zero? (alpha px 0 0)) aa)
        (is (rgb-near? red (p/get-color px 10 10)) aa))))
  (testing "every filter draws: inside opaque, far outside transparent, the edge blended; equal calls give equal pixels"
    (is (= 10 (count raster/filters)))
    (doseq [f raster/filters]
      (let [px (raster/raster-pixels 20 20 disc {:filter f})]
        (is (m/== 255.0 (alpha px 10 10)) f)
        (is (m/zero? (alpha px 0 0)) f)
        (is (some #(m/< 0.0 ^double % 255.0) (alphas px 20 20)) f)
        (is (= (raw px) (raw (raster/raster-pixels 20 20 disc {:filter f}))) f))))
  (testing "the output is centred on the samples: a centred disc mirrors left / right (the -0.5 shift to the renderer's pixel centres)"
    (doseq [aa [true {:filter :gaussian :radius 1.5} {:splats 3}]]
      (let [px (raster/raster-pixels 20 20 disc aa)]
        (doseq [y (range 20) x (range 10)]
          (is (near? 1.0E-9 (alpha px x y) (alpha px (m/long-sub 19 x) y)) [aa x y])))))
  (testing "non-square rasters keep x and y: a left band of a wide raster, a top band of a tall one"
    (doseq [aa [nil true]]
      (let [wide (raster/raster-pixels 30 4 (fn [^Vec2 p] (when (m/< (.x p) 10.0) red)) aa)
            tall (raster/raster-pixels 4 30 (fn [^Vec2 p] (when (m/< (.y p) 10.0) red)) aa)]
        (is (m/== 255.0 (alpha wide 3 2)) aa)
        (is (m/zero? (alpha wide 25 2)) aa)
        (is (m/== 255.0 (alpha tall 2 3)) aa)
        (is (m/zero? (alpha tall 2 25)) aa))))
  (testing "a colour function without any colour gives a transparent raster, on and off"
    (doseq [aa [nil true]]
      (is (every? m/zero? (alphas (raster/raster-pixels 8 8 (constantly nil) aa) 8 8)) aa)))
  (testing "a translucent colour keeps its alpha: coverage times the colour's alpha (:alpha-factor 0.5)"
    (let [px (raster/raster-pixels 20 20 (disc-color 10.0 10.0 6.3 (draw/faded (c/to-color red) 0.5)) true)]
      (is (near? 1.0 127.5 (alpha px 10 10)))))
  (testing "the samples sit inside their pixel: splats x splats calls per pixel at offsets (i + 0.5) / splats"
    (doseq [^long n [1 2 3]]
      (let [points (atom [])]
        (raster/raster-pixels 2 1 (fn [^Vec2 p] (swap! points conj [(.x p) (.y p)]) nil) {:splats n})
        (is (= (m/* 2 n n) (count @points)) n)
        (is (= (set (for [x (range 2) i (range n) j (range n)]
                      [(m/+ (double x) (m// (m/+ 0.5 (double i)) (double n))) (m// (m/+ 0.5 (double j)) (double n))]))
               (set @points))
            n))))
  (testing ":filter-params change the filter"
    (is (not= (raw (raster/raster-pixels 20 20 disc {:filter :gaussian}))
              (raw (raster/raster-pixels 20 20 disc {:filter :gaussian :filter-params [0.5]}))))
    (is (not= (raw (raster/raster-pixels 20 20 disc {:filter :mitchell}))
              (raw (raster/raster-pixels 20 20 disc {:filter :mitchell :filter-params [1.0 0.3 0.3]})))))
  (testing ":to-pixels: :background lies under uncovered and partly covered pixels; :saturation acts; :splats? stays on"
    (let [blue (c/color 0 0 255)
          px (raster/raster-pixels 20 20 disc {:to-pixels {:background blue}})
          plain (raster/raster-pixels 20 20 disc true)
          edge (first (filter #(m/< 0.0 (alpha plain % 10) 255.0) (range 20)))]
      (is (= (c/to-color blue) (c/to-color (p/get-color px 0 0))))
      (is (every? #(m/== 255.0 ^double %) (alphas px 20 20)) "every pixel opaque over an opaque background")
      (is (m/< 0.0 (.z ^Vec4 (p/get-color px edge 10)) 255.0) "an edge pixel mixes the background's blue"))
    (is (not= (raw (raster/raster-pixels 20 20 disc true)) (raw (raster/raster-pixels 20 20 disc {:to-pixels {:saturation 1.5}}))))
    (is (= (raw (raster/raster-pixels 20 20 disc true)) (raw (raster/raster-pixels 20 20 disc {:to-pixels {:splats? false}})))))
  (testing "1 x 1 rasters, on and off; 0-size rasters are empty"
    (doseq [aa [nil true]]
      (is (m/== 255.0 (alpha (raster/raster-pixels 1 1 (constantly red) aa) 0 0)) aa)
      (is (some? (raster/raster-pixels 0 5 (constantly nil) aa)) aa)
      (is (some? (raster/raster-pixels 5 0 (constantly nil) aa)) aa))))

(defn- kernel-weights
  "The plain kernel weights `k` of a renderer."
  ^doubles [rd]
  (.k ^clojure2d.java.LogDensity (:buff rd)))

(deftest renderer-coverage-test
  (testing "clojure2d: the output alpha under :splats? is coverage - opaque 255, half 128, none the background"
    (let [rd (p/renderer 5 1 :sinc 1.1)]
      (p/add-pixel! rd 0.0 0.0 (c/color 255 0 0 255))
      (p/add-pixel! rd 2.0 0.0 (c/color 0 0 255 0))
      (p/add-pixel! rd 4.0 0.0 (c/color 0 255 0 128))
      (let [px (p/to-pixels rd {:splats? true :background (c/color 0 0 0 0)})]
        (is (m/== 255.0 (alpha px 0 0)))
        (is (m/zero? (alpha px 2 0)) "a transparent splat alone, out of the others' reach")
        (is (near? 1.0 128.0 (alpha px 4 0))))))
  (testing "clojure2d: opaque splats average their colours by kernel weight, rounded, opaque with and without :splats?"
    (let [single (fn ^double [^double x] (let [rd (p/renderer 2 1 :gaussian 1.1)] (p/add-pixel! rd x 0.0 (c/color 255 0 0)) (aget (kernel-weights rd) 0)))
          w0 (double (single 0.0))
          w1 (double (single 1.0))
          rd (p/renderer 2 1 :gaussian 1.1)]
      (p/add-pixel! rd 0.0 0.0 (c/color 255 0 0))
      (p/add-pixel! rd 1.0 0.0 (c/color 0 0 255))
      (let [expected (c/color (m// (m/* 255.0 w0) (m/+ w0 w1)) 0.0 (m// (m/* 255.0 w1) (m/+ w0 w1)))]
        (is (rgb-near? expected (p/get-color (p/to-pixels rd {:splats? true}) 0 0)))
        (is (m/== 255.0 (alpha (p/to-pixels rd {:splats? true}) 0 0)))
        (is (m/== 255.0 (alpha (p/to-pixels rd {}) 0 0))))))
  (testing "clojure2d: a flat colour comes back exactly under :splats? (channels rounded)"
    (let [rd (p/renderer 4 4 :box 0.6)]
      (doseq [y (range 8) x (range 8)]
        (p/add-pixel! rd (m/- (m// (m/+ 0.5 (double x)) 2.0) 0.5) (m/- (m// (m/+ 0.5 (double y)) 2.0) 0.5) (c/color 70 130 180)))
      (is (= (c/to-color (c/color 70 130 180)) (c/to-color (p/get-color (p/to-pixels rd {:splats? true}) 1 1))))))
  (testing "clojure2d: without :splats? every hit pixel stays opaque"
    (let [rd (p/renderer 2 1 :sinc 1.1)]
      (p/add-pixel! rd 0.0 0.0 (c/color 255 0 0 128))
      (is (m/== 255.0 (alpha (p/to-pixels rd {}) 0 0)))))
  (testing "clojure2d: a kernel sum of 0 or less gives the background"
    (let [rd (p/renderer 1 1 :sinc 3.0)
          bg (c/color 1 2 3 4)]
      (p/add-pixel! rd 1.75 0.0 (c/color 255 0 0 255))
      (p/add-pixel! rd 1.25 0.0 (c/color 0 0 0 0))
      (is (m/<= (aget (kernel-weights rd) 0) 0.0) "the case: negative lobes outweigh")
      (is (= (c/to-color bg) (c/to-color (p/get-color (p/to-pixels rd {:splats? true :background bg}) 0 0))))))
  (testing "clojure2d: merged renderers add the kernel weights too"
    (let [a (p/renderer 1 1 :gaussian 1.0)
          b (p/renderer 1 1 :gaussian 1.0)]
      (p/add-pixel! a 0.0 0.0 (c/color 255 0 0 255))
      (p/add-pixel! b 0.0 0.0 (c/color 255 0 0 0))
      (p/merge-renderers a b)
      (is (near? 1.0 128.0 (alpha (p/to-pixels a {:splats? true :background (c/color 0 0 0 0)}) 0 0)))))
  (testing "clojure2d: the footprint is the pixels within the radius, symmetric around the sample"
    (let [rd (p/renderer 7 7 :sinc 1.5)]
      (p/add-pixel! rd 3.0 3.0 (c/color 255 0 0 255))
      (let [k (kernel-weights rd)
            at (fn ^double [^long x ^long y] (aget k (m/long-add x (m/long-mult 7 y))))]
        (doseq [^long d [1 2]]
          (is (m/== (at (m/long-sub 3 d) 3) (at (m/long-add 3 d) 3)) ["left / right" d])
          (is (m/== (at 3 (m/long-sub 3 d)) (at 3 (m/long-add 3 d))) ["up / down" d])
          (is (m/== (at (m/long-sub 3 d) 3) (at 3 (m/long-sub 3 d))) ["left / up" d]))
        (is (m/zero? (at 1 3)) "2 px away, beyond the radius")
        (is (m/zero? (at 5 3)))))
    (let [rd (p/renderer 7 1 :gaussian 1.1)]
      (p/add-pixel! rd 3.3 0.0 (c/color 255 0 0 255))
      (let [k (kernel-weights rd)]
        (is (m/pos? (aget k 4)) "0.7 px away, within the radius")
        (is (m/zero? (aget k 2)) "1.3 px away")
        (is (m/zero? (aget k 5)) "1.7 px away")))))

(defn- disc-plot
  "A plot of a `:pixel-color` disc with bands, field options `extra` merged."
  [extra]
  {:charts [(chart/field (merge {:domain {:x [-1.1 1.1] :y [-1.1 1.1]}
                                 :pixel-color (fn [[^double x ^double y]]
                                                (when (m/< (m/hypot x y) 1.0) (if (m/pos? (m/* x y)) :steelblue :orange)))}
                                extra))]})

(def ^:private render-options {:width 160 :height 140})

(deftest field-antialias-test
  (testing "off: no option, nil and false render the same pixels"
    (is (same-pixels? (disc-plot {}) (disc-plot {:antialias nil}) render-options))
    (is (same-pixels? (disc-plot {}) (disc-plot {:antialias false}) render-options)))
  (testing "on: true equals the explicit defaults, {} and nil entries, and differs from off"
    (is (same-pixels? (disc-plot {:antialias true}) (disc-plot {:antialias raster/antialias-defaults}) render-options))
    (is (same-pixels? (disc-plot {:antialias true}) (disc-plot {:antialias {}}) render-options))
    (is (same-pixels? (disc-plot {:antialias true}) (disc-plot {:antialias {:filter nil :splats nil}}) render-options))
    (is (not (same-pixels? (disc-plot {:antialias true}) (disc-plot {}) render-options))))
  (testing "a field from a grid, a flipped plot, a log axis and facets render antialiased - unlike off"
    (doseq [plot-of [(fn [aa] {:charts [(chart/field {:f (fn [[^double x ^double y]] (m/sin (m/* 3.0 (m/* x y)))) :domain {:x [-3 3] :y [-3 3]} :antialias aa})]
                               :scales {:color {:kind :binned :bins 4}}})
                     (fn [aa] (assoc (disc-plot {:antialias aa}) :flip true))
                     (fn [aa] {:charts [(chart/field {:pixel-color (fn [[^double x ^double y]] (when (m/< x (m/* 4.0 y)) :steelblue)) :domain {:x [1 100] :y [1 10]} :antialias aa})]
                               :scales {:x {:kind :log}}})
                     (fn [aa] {:data (for [k ["a" "b"] i (range 30)] {:k k :x (m/sin (double i)) :y (m/cos (m/* 1.3 (double i)))})
                               :mapping {:x :x :y :y :facet :k}
                               :scales {:color {:kind :binned :bins 4}}
                               :charts [(chart/field {:antialias aa})]})]]
      (is (instance? BufferedImage (plot/render-image (plot-of true) {:width 200 :height 160})))
      (is (not (same-pixels? (plot-of true) (plot-of nil) {:width 200 :height 160})))))
  (testing ":alpha-factor keeps its opacity inside: the centre pixel equal on and off, unlike an empty field"
    (let [centre (fn [extra] (aget (pixels (plot/render-image (disc-plot extra) render-options)) (m/long-add 70 (m/long-mult 160 50))))]
      (is (= (centre {:alpha-factor 0.5}) (centre {:alpha-factor 0.5 :antialias true})))
      (is (not= (centre {:alpha-factor 0.5}) (centre {:pixel-color (constantly nil)})))))
  (testing "a bad setting is rejected at the :antialias option"
    (doseq [bad [{:radius 0} {:radius 0.4} {:radius ##Inf} {:radius -1} {:filter :lanczos} {:splats 0} {:splats 1.5}
                 {:filter-params [1 2 3 4]} {:filter-params []} {:filter-params [0.0]} {:filter-params [-1.0]} {:filter-params [1.0 ##NaN]}
                 {:to-pixels 3} {:unknown 1} "yes"]]
      (is (some #(= [:charts 0 :options :antialias] (subvec (vec %) 0 (min 4 (count %))))
                (error-ins #(plot/render-image (disc-plot {:antialias bad}) {:width 100 :height 100})))
          (pr-str bad)))))
