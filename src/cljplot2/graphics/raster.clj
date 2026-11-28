(ns cljplot2.graphics.raster
  "Rasters: pixels coloured by a function of a canvas point, sampled once per pixel or supersampled and antialiased.

  [[raster-pixels]] samples a colour function at each pixel centre, or with antialias settings at `splats x splats` points per pixel (a regular grid, no randomness) splatted into a clojure2d log-density renderer and gathered by its reconstruction filter; the output alpha is each pixel's coverage, so samples without a colour (transparent) and translucent colours blend at edges. Colours are averaged as stored (sRGB, not linear light). A filter that weighs some samples negatively can ring slightly at hard edges: `:sinc` at a radius over 1 pixel, `:mitchell` and `:catmull-rom` over 1.125, `:hann` over 1.2.

  Functions: [[raster-pixels]], [[antialias-settings]]; schema - [[antialias-schema]], [[antialias-settings-schema]], [[filters]], [[antialias-defaults]]."
  (:require [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private transparent
  "Colour of a pixel or a sample without a colour."
  (c/color 0 0 0 0))

(def ^:private splat-pixel-options
  "clojure2d `to-pixels` options of a supersampled raster that a user's `:to-pixels` merges over."
  {:background transparent})

(def filters
  "The reconstruction filters of clojure2d's renderer that an antialiased raster accepts."
  #{:gaussian :box :sinc :mitchell :cubic :catmull-rom :triangle :cosinebell :blackmann-harris :hann})

(def antialias-defaults
  "Antialias settings a raster takes for `true` or for keys a settings map leaves out."
  {:splats 2 :filter :sinc :radius 1.1 :filter-params nil :to-pixels nil})

(def antialias-settings-schema
  "Schema of antialias settings: a map overriding some of [[antialias-defaults]] (a `nil` entry keeps its default)."
  [:map {:closed true :cljplot2/type-name "antialias settings"}
    (schema/nilable :splats {:description "How many samples the raster takes along each axis of a pixel: `2` takes 4 per pixel, on a regular grid."
                             :default 2 :cljplot2/example 3}
                    schema/pos-int)
    (schema/nilable :filter {:description "The reconstruction filter that gathers the samples into pixels."
                             :default :sinc
                             :cljplot2/value-meanings {:sinc "A windowed sinc (Lanczos): sharp, with slight ringing at hard edges over a radius of 1."
                                                       :gaussian "A Gaussian: soft, no ringing."
                                                       :box "A box: every sample within the radius counts equally."
                                                       :triangle "A tent: weights fall linearly with distance."}
                             :cljplot2/other-values "Other clojure2d filters: `:mitchell`, `:cubic`, `:catmull-rom`, `:cosinebell`, `:blackmann-harris`, `:hann`."
                             :cljplot2/example :gaussian}
                    (into [:enum] (sort filters)))
    (schema/nilable :radius {:description "How far, in pixels, the filter spreads a sample; at least half a pixel, so every sample reaches a pixel centre."
                             :default 1.1 :cljplot2/example 1.5}
                    [:and schema/finite [:>= 0.5]])
    (schema/nilable :filter-params {:description "The filter's further parameters in clojure2d's order: its spread (positive), then `B` and `C` of `:mitchell`; other filters ignore the parameters they don't take."
                                    :cljplot2/default-doc "Each filter's own defaults."
                                    :cljplot2/example [2.0]}
                    [:cat schema/finite-pos [:? schema/finite] [:? schema/finite]])
    (schema/nilable :to-pixels {:description "Options of clojure2d's `to-pixels`, merged over a transparent `:background`. `:background` lies under the raster's uncovered and partly covered pixels, and `:gamma-color` (with `:vibrancy`), `:brightness`, `:contrast` and `:saturation` adjust the colours."
                                :cljplot2/default-doc "None: coverage alpha over a transparent background."
                                :cljplot2/example {:saturation 1.5}}
                    [:map-of :keyword :any])])

(def antialias-schema
  "Schema of a raster's antialiasing (an option schema takes it as nilable): `false` - off, `true` - [[antialias-defaults]], or [[antialias-settings-schema]]."
  [:or :boolean antialias-settings-schema])

(defn antialias-settings
  "Gives the full antialias settings of a raster option.

  Parameters:

  - `antialias`: `nil` or `false` (off), `true`, or a map of some of the keys of [[antialias-defaults]] (see [[antialias-schema]]); a `nil` entry, also inside `:to-pixels`, keeps its default.

  Returns `nil` when off, else [[antialias-defaults]] with the map's non-`nil` entries over them, `:to-pixels` without `nil` values."
  [antialias]
  (let [without-nils (fn [m] (into {} (remove (comp nil? val)) m))]
    (cond (true? antialias) antialias-defaults
          (map? antialias) (-> (merge antialias-defaults (without-nils antialias))
                               (update :to-pixels without-nils)))))

(defn- sampled-pixels
  "Gives the pixels of a colour function sampled once per pixel, at its centre."
  [^long w ^long h color-at]
  (p/filter-colors-xy (fn [_ ^long x ^long y] (or (color-at (Vec2. (m/+ 0.5 x) (m/+ 0.5 y))) transparent))
                      (p/pixels w h)))

(defn- supersampled-pixels
  "Gives the pixels of a colour function sampled `splats x splats` times per pixel and gathered by a reconstruction filter (see the namespace doc)."
  [^long w ^long h color-at {:keys [^long splats filter radius filter-params to-pixels]}]
  (let [renderer (apply p/renderer w h filter radius filter-params)
        offsets (mapv (fn [^long i] (m// (m/+ 0.5 i) splats)) (range splats))]
    (dotimes [y h]
      (dotimes [x w]
        (doseq [^double dy offsets
                ^double dx offsets
                :let [px (m/+ x dx)
                      py (m/+ y dy)]]
          ;; the renderer's pixel centres sit on whole numbers; a sample without a colour still counts in the coverage
          (p/add-pixel! renderer (m/- px 0.5) (m/- py 0.5) (or (color-at (Vec2. px py)) transparent)))))
    ;; :splats? true is the coverage output; a user's :to-pixels can't turn it off
    (p/to-pixels renderer (assoc (merge splat-pixel-options to-pixels) :splats? true))))

(defn raster-pixels
  "Gives the pixels of a raster coloured by a function of a canvas point.

  Parameters:

  - `w`, `h` (longs): the raster size in pixels (0 gives an empty raster).
  - `color-at` (function): a canvas point (`Vec2`, pixels from the raster's top left) -> a clojure2d colour, `nil` for none (transparent).
  - `antialias`: `nil` or `false` - one sample per pixel centre; `true` or a settings map - supersampled and antialiased (see [[antialias-settings]] and the namespace doc).

  Returns clojure2d pixels of `w x h`. Calls `color-at` once per pixel, or `splats` squared times per pixel when antialiased. Samples sit on a regular grid, so equal arguments give equal pixels."
  [w h color-at antialias]
  (if-let [settings (antialias-settings antialias)]
    (supersampled-pixels w h color-at settings)
    (sampled-pixels w h color-at)))
