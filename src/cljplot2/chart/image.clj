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

  The callback is user code, called once per panel and render: seed its randomness, mind its cost."
  (:require [clojure2d.core :as c2d]
            [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [cljplot2.data.facet :as facet]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private range-schema
  (schema/with-rules [:tuple schema/finite schema/finite]
    [#(when-not (< (double (first %)) (double (second %))) "should be [lo hi] with lo < hi")]))

(def schemas
  "Schemas of the image chart: `:style` (none), `:options`, `:mapping` (merged; only facet keys)."
  {:style [:map {:closed true}]
   :options [:map {:closed true}
             [:f {:description "callback of {:x-scale :y-scale :x-domain :y-domain :facet :size} returning the panel's image (a BufferedImage, clojure2d canvas or pixels; nil - nothing)"}
              schema/function]
             (schema/optional :domain {:description "the data range the image covers, added to the extents without padding or nice ends"
                                       :cljplot2/default-doc "the image follows the other charts' domains"}
                              [:map {:closed true}
                               [:x {:description "x range [lo hi] (data units)"} range-schema]
                               [:y {:description "y range [lo hi] (data units)"} range-schema]])]
   :mapping [:map]})

(defn compute
  "Every panel (`facet/all-panels`): `{:f f}`; extents and no padding of `:domain` when given."
  [{:keys [options]} _chart-split _env]
  (let [{:keys [f domain]} options]
    (cond-> {:data {facet/all-panels {:f f}}
             :extents (if domain
                        {facet/all-panels (update-vals domain (fn [[lo hi]] {:kind :numeric :min (double lo) :max (double hi)}))}
                        {})
             :na #{}
             :dropped {}}
      domain (assoc :padding {:x [0.0 0.0] :y [0.0 0.0]}
                    :scale-defaults {:x {:nice false} :y {:nice false}}))))

(defn- image-of
  "`BufferedImage` of the callback's `result`, `nil` for `nil`; throws `ex-info {:key :f}` for anything else."
  [result]
  (cond
    (nil? result) nil
    (satisfies? pr/ImageProto result) (c2d/get-image result)
    :else (schema/fail! "chart/image :f should return a BufferedImage, a clojure2d canvas or pixels (or nil)."
                        {:key :f :value (type result)})))

(defn draw
  "Call the callback for this panel (`env` `:panel-key`) and draw its image over the panel box."
  [_chart {:keys [f]} canvas {:keys [w h]} scales env]
  (let [w (long (m/round (double w))) h (long (m/round (double h)))
        panel-key (:panel-key env)]
    (when (and (pos? w) (pos? h))
      (when-let [img (image-of (f {:x-scale (scale/forward-fn (:x scales))
                                   :y-scale (scale/forward-fn (:y scales))
                                   :x-domain (:domain (:x scales))
                                   :y-domain (:domain (:y scales))
                                   :facet (when-not (= panel-key facet/single-panel) panel-key)
                                   :size [w h]}))]
        (c2d/image canvas img 0 0 w h)))))
