(ns cljplot2.content.legend
  "Legends (D7), derived from the aesthetic scales (see [[cljplot2.scale.aesthetic]]) and drawn as grid content.

  [[legends]]: one legend per aesthetic dimension with entries (ordinal scales, linear size) and a color bar for a
  linear or binned `:color` scale (`:legend false` on the plot or the scale hides them; identity scales have none). Dimensions mapped by every chart with the same
  accessor (equal keyword or the same function) and with equal entry labels merge into one legend whose entries
  combine their attributes. Title: the scale's `:title` when given (`nil` - none), else the accessor's name when it
  is a keyword.

  Content `{:kind :legend :title t :entries [{:label l :attributes {dim attribute}}] :charts [{:chart i :dims [dim]}]}`:
  title (`:legend-title` role), then one row per entry - a key box, gap, label (`:legend-label` role). A key box is
  at least theme `[:legend :key-size]` square and grows to the entry's mapped `:size`; a row is as tall as its key
  and label. Entries that don't fit the available height (the item box from the previous layout round) wrap into
  further columns. Each chart `i` (index into the plot's `:charts`) draws its glyph into every key with its `:legend`
  function, in order (overlay), getting only the attributes of the dimensions `dims` it maps. Theme `:legend`
  `:fill`, `:border-color`, `:border-width` box it (a `:none` border takes no space).

  Content `{:kind :colorbar :title t :scale spec :ticks [v ...] :labels [s ...]}`: title, then a vertical gradient bar
  (top = domain maximum, width theme `[:legend :bar-width]`, default `:key-size`) with tick marks and labels
  (`:legend-label`); a binned scale's bar is one equal block per bin, ticked at the inner breaks. Its length is theme `[:legend :bar-length]`: `:stretch` (default) - the height left in its stack
  column (at least 3 x `:key-size`, else it starts a new column); a number - that length, clamped to the height
  available.

  Content `{:kind :legends :legends [legend ...]}`: legends (`:legend` or `:colorbar`) stacked top-down with theme `[:legend :gap]` between them;
  a legend that doesn't fit below the previous one starts a new stack column to the right. [[legend-boxes]] gives
  where each legend is drawn."
  (:require [cljplot2.schema :as schema]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [wadogo.scale :as s]
            [cljplot2.data.levels :as levels]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as position]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; derivation

(defn- dim-legend
  "Legend candidate of aesthetic `k`: scale, entries and the set of accessors charts map it with; `nil` without entries."
  [k scale-specs mappings]
  (when-let [spec (get scale-specs k)]
    (when-let [entries (seq (aesthetic/legend-entries k spec))]
      {:dim k :spec spec :entries (vec entries) :accessors (set (keep #(get % k) mappings))})))

(defn- merge-key
  [{:keys [dim entries accessors]}]
  (if (= 1 (count accessors)) [(first accessors) (mapv :label entries)] [::own dim]))

(defn- title
  [members]
  (if-let [titled (first (filter #(contains? (:spec %) :title) members))]
    (get-in titled [:spec :title])
    (let [acc (first (:accessors (first members)))]
      (when (and (= 1 (count (:accessors (first members)))) (keyword? acc)) (name acc)))))

(defn- legend-content
  [members theme charts mappings]
  (let [dims (mapv :dim members)
        na {:color (get-in theme [:palette :na])}
        attribute-fns (mapv (fn [{:keys [dim spec]}] (aesthetic/attribute-fn dim spec (na dim))) members)]
    {:kind :legend
     :title (title members)
     :entries (mapv (fn [i]
                      {:label (:label (get-in (first members) [:entries i]))
                       :attributes (into {} (keep (fn [[{:keys [dim entries]} f]]
                                                    (when-some [a (f (:value (entries i)))] [dim a])))
                                         (map vector members attribute-fns))})
                    (range (count (:entries (first members)))))
     :charts (vec (keep-indexed (fn [i [chart mapping]]
                                  (let [mapped (filterv #(some? (get mapping %)) dims)]
                                    (when (and (:legend chart) (seq mapped)) {:chart i :dims mapped})))
                                (map vector charts mappings)))}))

(def ^:private ^:const colorbar-ticks 5)

(defn- with-midpoint-tick
  "Tick `[value label]` pairs (ascending) with a linear scale's `midpoint` added as `[midpoint midpoint-label]` when it
  lies in `[lo hi]` and isn't a tick; ticks closer to it than half the tick step are dropped (their labels would
  collide). The other labels stay as they were."
  [ticks midpoint midpoint-label lo hi]
  (if (and (some? midpoint) (<= (double lo) (double midpoint) (double hi)) (not-any? #(== (double (first %)) (double midpoint)) ticks))
    (let [mid (double midpoint)
          step (if (> (count ticks) 1) (- (double (first (second ticks))) (double (first (first ticks)))) (- (double hi) (double lo)))]
      (vec (sort-by first (conj (filterv #(>= (m/abs (- (double (first %)) mid)) (* 0.5 step)) ticks)
                                [mid midpoint-label]))))
    ticks))

(defn- colorbar-content
  "Color bar of the linear or binned color scale `spec` mapped with `accessors` (title rule as legends). Ticks:
  linear - nice ticks of the domain, labelled as axis ticks, and the `:midpoint` (see [[with-midpoint-tick]];
  labelled by `:formatter` or [[cljplot2.data.levels/break-labels]]); binned - the inner breaks, labelled by
  `:formatter` or [[cljplot2.data.levels/break-labels]]."
  [spec accessors]
  (let [[lo hi] (:domain spec)
        lo (double lo) hi (double hi)
        formatter (:formatter spec)
        ws (s/scale :linear (cond-> {:domain (if (== lo hi) [lo (inc lo)] [lo hi]) :ticks colorbar-ticks}
                              formatter (assoc :formatter formatter)))
        axis-labels (fn [ts] (position/tick-labels ws {:kind :linear :formatter formatter} ts))
        pairs (cond
                (= :binned (:kind spec))
                (let [breaks (:breaks spec)
                      inner (subvec breaks 1 (dec (count breaks)))
                      labels (if formatter
                               (axis-labels inner)
                               (let [ls (levels/break-labels breaks)] (subvec ls 1 (dec (count ls)))))]
                  (mapv vector inner labels))
                (== lo hi) (mapv vector [lo] (axis-labels [lo]))
                :else (let [nice (filterv #(<= lo (double %) hi) (s/ticks ws))
                            mid (:midpoint spec)]
                        (with-midpoint-tick (mapv vector nice (axis-labels nice)) mid
                          (when (some? mid) (if formatter (str (formatter mid)) (first (levels/break-labels [mid]))))
                          lo hi)))]
    {:kind :colorbar
     :title (title [{:spec spec :accessors accessors}])
     :scale spec
     :ticks (mapv first pairs)
     :labels (mapv second pairs)}))

(defn legend-charts
  "Charts whose glyphs the legends draw, in key index order: the plot's charts, then its margin charts (`:top`, then
  `:right`; see `cljplot2.pipeline`) - `{:charts [..] :mappings [..] :stats [..]}`."
  [{:keys [charts split stats margin-stats]}]
  (let [sides (keep margin-stats [:top :right])]
    {:charts (into (vec charts) (mapcat :charts sides))
     :mappings (into (mapv :mapping (:charts split)) (mapcat :mappings sides))
     :stats (into (vec stats) (mapcat :stats sides))}))

(defn legends
  "Legend contents of a plot built up to `:scale-specs`, in aesthetic order (`:color`, `:shape`, `:size`, `:dash`); empty when
  the plot's `:legend` is `false` or nothing with a legend is mapped. Keys draw the glyphs of [[legend-charts]] (margin
  charts included)."
  [{:keys [scale-specs theme legend] :as plot}]
  (if (false? legend)
    []
    (let [{:keys [charts mappings]} (legend-charts plot)
          groups (reduce (fn [acc candidate]
                           (let [k (merge-key candidate)
                                 i (first (keep-indexed (fn [i [gk]] (when (= gk k) i)) acc))]
                             (if i (update-in acc [i 1] conj candidate) (conj acc [k [candidate]]))))
                         [] (keep #(dim-legend % scale-specs mappings) aesthetic/dims))]
      (cond->> (mapv (fn [[_ members]] (legend-content members theme charts mappings)) groups)
        (let [spec (:color scale-specs)] (and (#{:linear :binned} (:kind spec)) (not (false? (:legend spec)))))
        (into [(colorbar-content (:color scale-specs) (set (keep :color mappings)))])))))

;; content

(def legend-schema
  "`{:kind :legend}` content: `:title`, `:entries` `[{:label :attributes}]`, `:charts` `[{:chart index :dims [dim]}]`."
  [:map
   (schema/nilable :title string?)
   [:entries [:vector [:map [:label string?] [:attributes :map]]]]
   [:charts [:vector [:map [:chart nat-int?] [:dims vector?]]]]])

(def colorbar-schema
  "`{:kind :colorbar}` content: `:title`, a linear or binned `:scale`, `:ticks` and one label per tick in `:labels`."
  (schema/with-rules
   [:map
    (schema/nilable :title string?)
    [:scale [:map [:kind [:enum :linear :binned]]]]
    [:ticks vector?]
    [:labels vector?]]
   [#(when-not (= (count (:ticks %)) (count (:labels %))) "should have one label per tick") [:labels]]))

(def legends-schema
  "`{:kind :legends}` content: `:legends`, a vector of [[legend-schema]] and [[colorbar-schema]] contents."
  [:map [:legends [:vector [:multi {:dispatch #(if (= :colorbar (:kind %)) :colorbar :legend)}
                            [:colorbar colorbar-schema]
                            [:legend legend-schema]]]]])

(defmethod measure/validate-content :legend [content] (schema/explain! [[legend-schema content [:content]]]))
(defmethod measure/validate-content :colorbar [content] (schema/explain! [[colorbar-schema content [:content]]]))
(defmethod measure/validate-content :legends [content] (schema/explain! [[legends-schema content [:content]]]))

(defn- title-label [text] {:kind :label :role :legend-title :text text})
(defn- entry-label [text] {:kind :label :role :legend-label :text text})

(defn- glyph-extent
  "Key box side needed by `attributes`: the mapped `:size` (0 when not mapped)."
  ^double [attributes]
  (let [size (:size attributes)] (if (number? size) (double size) 0.0)))

(defn- wrap-columns
  "Rows (`{:entry i :key-w :h}`) split greedily into columns no taller than `max-h` (each column holds at least one row)."
  [rows ^double max-h]
  (reduce (fn [cols {:keys [^double h] :as row}]
            (let [col (peek cols)
                  col-h (double (reduce + 0.0 (map :h col)))]
              (if (or (nil? col) (and (seq col) (> (+ col-h h) max-h)))
                (conj cols [row])
                (conj (pop cols) (conj col row)))))
          [] rows))

(defn- legend-geometry
  "Layout of a legend no taller than `max-h` (when possible), measured on `env` `:canvas`, relative to its top-left
  corner: `:w`, `:h`, `:border`, `:title-h`, `:columns` - `[{:x :key-w :rows [{:entry i :y :h}]}]`."
  [{:keys [title entries]} env ^double max-h]
  (let [{:keys [gap key-size border-width border-color]} (get-in env [:theme :legend])
        gap (double gap) key-size (double key-size)
        border (if (theme/none? border-color) 0.0 (double (m/round (double border-width))))
        ^Vec2 title-size (if title (measure/natural-size (title-label title) env) (v/vec2 0.0 0.0))
        labels (mapv #(measure/natural-size (entry-label (:label %)) env) entries)
        rows (vec (map-indexed (fn [i {:keys [attributes]}]
                                 (let [key-w (m/max key-size (glyph-extent attributes))]
                                   {:entry i :key-w key-w :label-w (.x ^Vec2 (labels i))
                                    :h (m/max key-w (.y ^Vec2 (labels i)))}))
                               entries))
        columns (wrap-columns rows (- max-h (* 2.0 border) (.y title-size)))
        [columns ^double width] (reduce (fn [[acc ^double x] col]
                                          (let [key-w (double (reduce max 0.0 (map :key-w col)))
                                                label-w (double (reduce max 0.0 (map :label-w col)))
                                                ys (reductions + 0.0 (map :h col))]
                                            [(conj acc {:x x :key-w key-w
                                                        :rows (mapv (fn [r y] {:entry (:entry r) :y y :h (:h r)}) col ys)})
                                             (+ x key-w gap label-w gap)]))
                                        [[] 0.0] columns)
        entries-w (m/max 0.0 (- width gap))
        entries-h (double (reduce max 0.0 (map (fn [{:keys [rows]}] (let [r (peek rows)] (+ (double (:y r)) (double (:h r))))) columns)))]
    {:border border :title-h (.y title-size) :columns columns :labels labels
     :w (+ (* 2.0 border) (m/max (.x title-size) entries-w))
     :h (+ (* 2.0 border) (.y title-size) entries-h)}))

(def ^:private ^:const tick-length 3.0)

(defn- colorbar-geometry
  "Layout of color bar `content` with bar length `length` (pixels), relative to its top-left corner: `:w`, `:h`,
  `:border`, `:title-h`, `:bar-w`, `:bar-len`, `:labels` (sizes), `:label-h` (room above and below the bar for the
  end labels, together)."
  [{:keys [title labels]} env ^double length]
  (let [{:keys [gap key-size bar-width border-width border-color]} (get-in env [:theme :legend])
        gap (double gap)
        bar-w (double (or bar-width key-size))
        border (if (theme/none? border-color) 0.0 (double (m/round (double border-width))))
        ^Vec2 title-size (if title (measure/natural-size (title-label title) env) (v/vec2 0.0 0.0))
        sizes (mapv #(measure/natural-size (entry-label %) env) labels)
        label-h (double (reduce max 0.0 (map #(.y ^Vec2 %) sizes)))
        label-w (double (reduce max 0.0 (map #(.x ^Vec2 %) sizes)))]
    {:border border :title-h (.y title-size) :bar-w bar-w :bar-len length :labels sizes :label-h label-h
     :w (+ (* 2.0 border) (m/max (.x title-size) (+ bar-w tick-length gap label-w)))
     :h (+ (* 2.0 border) (.y title-size) label-h length)}))

(defn- colorbar-length
  "Bar length for `content` with `room` pixels available: the room minus the bar's fixed parts (title, label room,
  border), at least 3 x `:key-size` (also for unbounded room), for theme `[:legend :bar-length] :stretch`; a number
  is used up to that stretched length (never longer than the room allows)."
  ^double [content env ^double room]
  (let [{:keys [bar-length key-size]} (get-in env [:theme :legend])
        minimum (* 3.0 (double key-size))
        fixed (double (:h (colorbar-geometry content env 0.0)))
        stretched (if (m/valid-double? room) (m/max minimum (- room fixed)) ##Inf)]
    (if (number? bar-length)
      (m/min (double bar-length) stretched)
      (if (m/valid-double? stretched) stretched minimum))))

(defn- stretching? [content env]
  (and (= :colorbar (:kind content)) (not (number? (get-in env [:theme :legend :bar-length])))))

(defn- content-geometry
  "Geometry of a legend or color bar content, `room` - height available to it."
  [content env ^double room]
  (if (= :colorbar (:kind content))
    (colorbar-geometry content env (colorbar-length content env room))
    (legend-geometry content env room)))

(defn- available-height
  "Height a legend may take: its item box from the previous layout round, unbounded before the first one."
  ^double [env]
  (if-let [box (:box env)] (double (:h box)) ##Inf))

(defn- stack-geometry
  "Placement of `legends` stacked top-down within `max-h`, new stack columns to the right:
  `{:w :h :placed [{:x :y :geometry}]}` relative to the stack's top-left corner."
  [legends env ^double max-h]
  (let [gap (double (get-in env [:theme :legend :gap]))
        {:keys [placed ^double col-x ^double col-w ^double h]}
        (reduce (fn [{:keys [placed ^double col-x ^double col-w ^double y ^double h] :as acc} legend]
                  (let [stretch? (stretching? legend env)
                        minimum (* 3.0 (double (get-in env [:theme :legend :key-size])))
                        fixed (when stretch? (double (:h (colorbar-geometry legend env 0.0))))
                        ;; a stretching color bar too short below the previous legends starts a new column
                        new-col? (and (pos? y)
                                      (if stretch?
                                        (< (- max-h y (double fixed)) minimum)
                                        (> (+ y (double (:h (content-geometry legend env max-h)))) max-h)))
                        y' (if new-col? 0.0 y)
                        g (content-geometry legend env (- max-h y'))
                        lw (double (:w g)) lh (double (:h g))
                        col-x (if new-col? (+ col-x col-w gap) col-x)
                        col-w (if new-col? 0.0 col-w)
                        y y']
                    (assoc acc :placed (conj placed {:x col-x :y y :geometry g})
                           :col-x col-x :col-w (m/max col-w lw) :y (+ y lh gap) :h (m/max h (+ y lh)))))
                {:placed [] :col-x 0.0 :col-w 0.0 :y 0.0 :h 0.0}
                legends)]
    {:w (+ col-x col-w) :h h :placed placed}))

(defmethod measure/natural-size :legend
  [content env]
  (let [{:keys [w h]} (legend-geometry content env (available-height env))]
    (v/vec2 w h)))

(defmethod measure/natural-size :colorbar
  [content env]
  (let [{:keys [w h]} (colorbar-geometry content env (colorbar-length content env (available-height env)))]
    (v/vec2 w h)))

(defmethod measure/natural-size :legends
  [{:keys [legends]} env]
  (let [{:keys [w h]} (stack-geometry legends env (available-height env))]
    (v/vec2 w h)))

(defn legend-boxes
  "Boxes `{:x :y :w :h}` of the legends of `:legends` content drawn in `box`, in order (`env` - `:theme` and a
  `:canvas` in graphics context for measuring)."
  [{:keys [legends]} env {:keys [x y h]}]
  (mapv (fn [{px :x py :y g :geometry}]
          {:x (+ (double x) (double px)) :y (+ (double y) (double py)) :w (:w g) :h (:h g)})
        (:placed (stack-geometry legends env (double h)))))

(defn- draw-key!
  "Overlay the glyphs of the legend's charts in key `box`, each isolated, clipped to the box and given the entry's
  attributes of the dimensions it maps; the glyph's `env` carries the chart's computed stats (`:chart-stats`)."
  [c attributes {:keys [x y w h] :as box} chart-keys env]
  (doseq [{i :chart dims :dims} chart-keys
          :let [chart (get-in env [:legend-charts i])]]
    (canvas/call-with-isolated-graphics
     c (fn [c']
         (.clip ^Graphics2D (:graphics c') (Rectangle. (int x) (int y) (int (m/ceil w)) (int (m/ceil h))))
         ((:legend chart) chart (select-keys attributes dims) c' box (assoc env :chart-stats (get-in env [:legend-stats i])))))))

(defn- draw-legend!
  "Draw legend `content` with its `geometry` at top-left corner `origin` (`Vec2`)."
  [{:keys [title entries charts]} geometry c ^Vec2 origin env]
  (let [x (.x origin)
        y (.y origin)
        {:keys [fill border-color border-width]} (get-in env [:theme :legend])
        {:keys [^double w ^double h ^double border ^double title-h columns labels]} geometry
        gap (double (get-in env [:theme :legend :gap]))
        x0 (+ x border)
        y0 (+ y border)
        label! (fn [label-content box] (canvas/call-with-isolated-graphics c #(draw/draw-content label-content % box env)))]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c (m/round x) (m/round y) (m/round w) (m/round h)))
    (draw/inner-border! c {:x (m/round x) :y (m/round y) :w (m/round w) :h (m/round h)} border-color border-width)
    (when title
      (label! (title-label title) {:x x0 :y y0 :w (- w (* 2.0 border)) :h title-h}))
    (doseq [{cx :x key-w :key-w rows :rows} columns
            {i :entry ry :y rh :h} rows
            :let [kx (+ x0 (double cx))
                  ky (+ y0 title-h (double ry))
                  entry (entries i)]]
      (draw-key! c (:attributes entry) {:x kx :y ky :w key-w :h rh} charts env)
      (label! (entry-label (:label entry))
              {:x (+ kx (double key-w) gap) :y ky :w (.x ^Vec2 (labels i)) :h rh}))))

(defn- bar-value-fn
  "Function of a position `t` along the color bar of scale `spec` (0 - bottom, 1 - top) giving the value drawn there:
  linear - proportional over the domain; binned - every bin an equal block, proportional within it."
  [{:keys [kind domain breaks]}]
  (let [[lo hi] domain
        lo (double lo) hi (double hi)]
    (if (= :binned kind)
      (let [n (dec (count breaks))]
        (fn [^double t]
          (let [x (* t n)
                i (long (m/constrain (m/floor x) 0 (dec n)))
                a (double (breaks i))
                b (double (breaks (inc i)))]
            (+ a (* (- x i) (- b a))))))
      (fn [^double t] (+ lo (* t (- hi lo)))))))

(defn- bar-position-fn
  "Inverse of [[bar-value-fn]]: function of a value within the domain giving its position along the bar."
  [{:keys [kind domain breaks]}]
  (let [[lo hi] domain
        lo (double lo) hi (double hi)]
    (if (= :binned kind)
      (let [n (dec (count breaks))
            arr (double-array breaks)]
        (fn [v]
          (let [v (double v)
                i (long (m/constrain (levels/bin-index arr v) 0 (dec n)))
                a (aget arr i)
                b (aget arr (inc i))]
            (/ (+ i (/ (- v a) (- b a))) n))))
      (fn [v] (if (== lo hi) 0.5 (/ (- (double v) lo) (- hi lo)))))))

(defn- draw-colorbar!
  "Draw color bar `content` with its `geometry` at top-left corner `origin` (`Vec2`)."
  [{:keys [title scale ticks labels]} geometry c ^Vec2 origin env]
  (let [x (.x origin)
        y (.y origin)
        {:keys [fill border-color border-width gap]} (get-in env [:theme :legend])
        {:keys [^double w ^double h ^double border ^double title-h ^double bar-w ^double bar-len ^double label-h]
         sizes :labels} geometry
        color-of (aesthetic/attribute-fn :color scale (get-in env [:theme :palette :na]))
        [lo hi] (:domain scale)
        lo (double lo) hi (double hi)
        value-at (bar-value-fn scale)
        position-of (bar-position-fn scale)
        x0 (+ x border)
        bar-top (+ y border title-h (* 0.5 label-h))
        bar-x (m/round x0)
        bar-w-px (m/round bar-w)
        len (long (m/round bar-len))
        tick-y (fn ^double [t] (+ bar-top (if (== lo hi) (* 0.5 bar-len) (* (- 1.0 (double (position-of t))) bar-len))))
        label-color (get-in env [:theme :text :legend-label :color])]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c (m/round x) (m/round y) (m/round w) (m/round h)))
    (draw/inner-border! c {:x (m/round x) :y (m/round y) :w (m/round w) :h (m/round h)} border-color border-width)
    (when title
      (canvas/call-with-isolated-graphics
       c #(draw/draw-content (title-label title) % {:x x0 :y (+ y border) :w (- w (* 2.0 border)) :h title-h} env)))
    ;; the bar: one pixel row per value, top = domain maximum
    (dotimes [i len]
      (let [t (if (== lo hi) 0.5 (- 1.0 (/ (+ (double i) 0.5) (double len))))]
        (c2d/set-color c (color-of (value-at t)))
        (c2d/rect c bar-x (+ (m/round bar-top) i) bar-w-px 1)))
    (c2d/set-stroke-custom c {:size 1.0 :cap :butt})
    (doseq [[t text ^Vec2 size] (map vector ticks labels sizes)
            :let [ty (draw/pixel-center (tick-y t))
                  bar-right (+ (double bar-x) (double bar-w-px))]]
      (c2d/set-color c label-color)
      (c2d/line c bar-right ty (+ bar-right tick-length) ty)
      (canvas/call-with-isolated-graphics
       c #(draw/draw-content (entry-label text) %
                             {:x (+ bar-right tick-length (double gap)) :y (- ty (* 0.5 (.y size))) :w (.x size) :h (.y size)}
                             env)))))

(defn- draw-content-at!
  [content geometry c origin env]
  (if (= :colorbar (:kind content))
    (draw-colorbar! content geometry c origin env)
    (draw-legend! content geometry c origin env)))

(defmethod draw/draw-content :colorbar
  [content c {:keys [x y h]} env]
  (let [env (assoc env :canvas c)]
    (draw-colorbar! content (colorbar-geometry content env (colorbar-length content env (double h)))
                    c (v/vec2 x y) env)))

(defmethod draw/draw-content :legend
  [content c {:keys [x y h]} env]
  (let [env (assoc env :canvas c)]
    (draw-legend! content (legend-geometry content env (double h)) c (v/vec2 x y) env)))

(defmethod draw/draw-content :legends
  [{:keys [legends]} c {:keys [x y h]} env]
  (let [env (assoc env :canvas c)]
    (doseq [[legend {px :x py :y g :geometry}] (map vector legends (:placed (stack-geometry legends env (double h))))]
      (draw-content-at! legend g c (v/vec2 (+ (double x) (double px)) (+ (double y) (double py))) env))))
