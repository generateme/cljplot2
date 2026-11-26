(ns cljplot2.chart.image
  "Image: a free chart - a callback draws an image of the whole panel from the panel's finalized position scales
  (cartesian plots only). No data, mapping or legend: the chart is drawn in every panel (as annotations, it inherits
  only the plot's facet keys).

  * `:f` (required) - a function of `{:x-scale fx :y-scale fy :x-domain [lo hi] :y-domain [lo hi] :facet k :size [w
    h]}` returning a `java.awt.image.BufferedImage`, a clojure2d canvas or clojure2d pixels (anything with
    `clojure2d.core/get-image`), drawn scaled to the panel; `nil` draws nothing, another value throws `ex-info {:key
    :f}`. `fx` / `fy` map a data value to its pixel in the panel (`(fx x)` horizontal, `(fy y)` vertical, top down;
    with the plot's `:flip` `fx` is vertical and `fy` horizontal), `:facet` - the panel's facet level (`[col row]`
    for a grid; `nil` without facets), `:size` - the panel's pixels (the image's natural size).
  * `:domain {:x [lo hi] :y [lo hi]}` - the data range the image covers: added to the extents without padding or nice
    ends; without it the image follows the other charts' domains (a plot of only an image needs it).

  The callback is user code, called once per panel and render: seed its randomness, mind its cost.

  Functions: [[compute]] - the callback in every panel, extents of `:domain`; [[draw]] - the callback's image over a panel."
  (:require [clojure2d.core :as c2d]
            [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the image chart: `:style` (none), `:options`, `:mapping` (merged; only facet keys)."
  {:style [:map {:closed true}]
   :options [:map {:closed true}
             [:f {:description "A function of the panel's `{:x-scale :y-scale :x-domain :y-domain :facet :size}` that returns a `BufferedImage`, a clojure2d canvas or pixels, or `nil` for nothing. The chart scales the image over the whole panel."
                  :cljplot2/example '(fn [{[w h] :size}] (c2d/with-canvas-> (c2d/canvas w h) (c2d/set-background :lightblue)))
                  :cljplot2/example-context {:domain {:x [0 1] :y [0 1]}}}
              schema/function]
             (schema/optional :domain {:description "The data range the axes show at least, without padding or rounding; the image still fills the whole panel."
                                       :cljplot2/default-doc "The axes follow the other charts' domains."
                                       :cljplot2/example-context {:f '(fn [{[w h] :size}] (c2d/with-canvas-> (c2d/canvas w h) (c2d/set-background :lightblue))) :domain {:x [0 1] :y [0 1]}}}
                              [:map {:closed true}
                               [:x {:description "The x range the axes show at least, in data units." :cljplot2/example [0 4]} schema/finite-interval]
                               [:y {:description "The y range the axes show at least, in data units." :cljplot2/example [0 4]} schema/finite-interval]])]
   :mapping [:map]})

(defn compute
  "Computes the image chart's stats: the callback in every panel.

  Parameters:

  - `chart` (map): the chart value; its `:options` - `:f` (the callback), `:domain` (`{:x [lo hi] :y [lo hi]}`, optional).
  - `_chart-split`, `_env`: unused - the chart reads no data.

  Returns `{:data {facet/all-panels {:f f}} :extents .. :na #{} :dropped {}}`: with `:domain` its ranges as numeric extents, no padding and no nice ends (`:scale-defaults`); without it no extents.

  See also [[draw]]."
  [{:keys [options]} _chart-split _env]
  (let [{:keys [f domain]} options]
    (cond-> {:data {facet/all-panels {:f f}}
             :extents (if domain
                        {facet/all-panels (update-vals domain (fn [[lo hi]] {:kind :numeric :min (double lo) :max (double hi)}))}
                        {})
             :na #{}
             :dropped {}}
      domain (assoc :padding data/zero-padding
                    :scale-defaults {:x {:nice false} :y {:nice false}}))))

(defn- image-of
  "Gives the `BufferedImage` of a callback's result.

  Parameters:

  - `result`: what the `:f` callback returned.

  Returns a `BufferedImage` of anything clojure2d gets an image from (an image, a canvas, pixels), `nil` for `nil`.

  Throws `ex-info {:key :f}` for any other value."
  [result]
  (cond
    (nil? result) nil
    (satisfies? pr/ImageProto result) (c2d/get-image result)
    :else (schema/fail! "chart/image :f should return a BufferedImage, a clojure2d canvas or pixels (or nil)."
                        {:key :f :value (type result)})))

(defn draw
  "Draws the callback's image of one panel over the panel box.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:f` the callback.
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box, `:w` and `:h` in pixels (rounded to whole pixels).
  - `scales` (map): the panel's finalized position scales `:x`, `:y`.
  - `env` (map): the drawing environment; `:panel-key` - the panel's facet level.

  Calls `:f` with `{:x-scale :y-scale :x-domain :y-domain :facet :size}` (see the namespace doc) and draws the image it returns scaled to the panel; nothing for an empty panel or a `nil` result.

  Throws `ex-info {:key :f}` for a result that isn't an image (see [[image-of]])."
  [_chart {:keys [f]} canvas {:keys [^double w ^double h]} scales env]
  (let [width (m/round w)
        height (m/round h)
        panel-key (:panel-key env)]
    (when (and (m/pos? width) (m/pos? height))
      (when-let [img (image-of (f {:x-scale (scale/forward-fn (:x scales))
                                   :y-scale (scale/forward-fn (:y scales))
                                   :x-domain (:domain (:x scales))
                                   :y-domain (:domain (:y scales))
                                   :facet (when-not (= panel-key facet/single-panel) panel-key)
                                   :size [width height]}))]
        (c2d/image canvas img 0 0 width height)))))
