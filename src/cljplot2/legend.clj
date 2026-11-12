(ns cljplot2.legend
  "Legends (D7), derived from the aesthetic scales (see [[cljplot2.aesthetic]]) and drawn as grid content.

  [[legends]]: one legend per aesthetic dimension with entries (ordinal scales, linear size; linear color has no
  legend yet, `:legend false` on the plot or the scale hides it). Dimensions mapped by every chart with the same
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

  Content `{:kind :legends :legends [legend ...]}`: legends stacked top-down with theme `[:legend :gap]` between them;
  a legend that doesn't fit below the previous one starts a new stack column to the right. [[legend-boxes]] gives
  where each legend is drawn."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.aesthetic :as aesthetic]
            [cljplot2.canvas :as canvas]
            [cljplot2.draw :as draw]
            [cljplot2.measure :as measure]
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

(defn legends
  "Legend contents of a plot built up to `:scale-specs`, in aesthetic order (`:color`, `:shape`, `:size`); empty when
  the plot's `:legend` is `false` or nothing with a legend is mapped."
  [{:keys [scale-specs split charts theme legend]}]
  (if (false? legend)
    []
    (let [mappings (mapv :mapping (:charts split))
          groups (reduce (fn [acc candidate]
                           (let [k (merge-key candidate)
                                 i (first (keep-indexed (fn [i [gk]] (when (= gk k) i)) acc))]
                             (if i (update-in acc [i 1] conj candidate) (conj acc [k [candidate]]))))
                         [] (keep #(dim-legend % scale-specs mappings) aesthetic/dims))]
      (mapv (fn [[_ members]] (legend-content members theme charts mappings)) groups))))

;; content

(defn- validate-legend!
  [{:keys [title entries charts]}]
  (when-not (or (nil? title) (string? title))
    (throw (ex-info "Legend :title must be a string or nil." {:key :title :value title})))
  (when-not (and (vector? entries) (every? #(and (map? %) (string? (:label %)) (map? (:attributes %))) entries))
    (throw (ex-info "Legend :entries must be a vector of {:label string :attributes map}." {:key :entries})))
  (when-not (and (vector? charts) (every? #(and (nat-int? (:chart %)) (vector? (:dims %))) charts))
    (throw (ex-info "Legend :charts must be a vector of {:chart index :dims [dim]}." {:key :charts}))))

(defmethod measure/validate-content :legend [content] (validate-legend! content))

(defmethod measure/validate-content :legends
  [{:keys [legends]}]
  (when-not (vector? legends)
    (throw (ex-info "Legends :legends must be a vector of legend contents." {:key :legends})))
  (run! validate-legend! legends))

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
                  (let [g (legend-geometry legend env max-h)
                        lw (double (:w g)) lh (double (:h g))
                        new-col? (and (pos? y) (> (+ y lh) max-h))
                        col-x (if new-col? (+ col-x col-w gap) col-x)
                        col-w (if new-col? 0.0 col-w)
                        y (if new-col? 0.0 y)]
                    (assoc acc :placed (conj placed {:x col-x :y y :geometry g})
                           :col-x col-x :col-w (m/max col-w lw) :y (+ y lh gap) :h (m/max h (+ y lh)))))
                {:placed [] :col-x 0.0 :col-w 0.0 :y 0.0 :h 0.0}
                legends)]
    {:w (+ col-x col-w) :h h :placed placed}))

(defmethod measure/natural-size :legend
  [content env]
  (let [{:keys [w h]} (legend-geometry content env (available-height env))]
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
  attributes of the dimensions it maps."
  [c attributes {:keys [x y w h] :as box} chart-keys env]
  (doseq [{i :chart dims :dims} chart-keys
          :let [chart (get-in env [:charts i])]]
    (canvas/call-with-isolated-graphics
     c (fn [c']
         (.clip ^Graphics2D (:graphics c') (Rectangle. (int x) (int y) (int (m/ceil w)) (int (m/ceil h))))
         ((:legend chart) chart (select-keys attributes dims) c' box env)))))

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

(defmethod draw/draw-content :legend
  [content c {:keys [x y h]} env]
  (let [env (assoc env :canvas c)]
    (draw-legend! content (legend-geometry content env (double h)) c (v/vec2 x y) env)))

(defmethod draw/draw-content :legends
  [{:keys [legends]} c {:keys [x y h]} env]
  (let [env (assoc env :canvas c)]
    (doseq [[legend {px :x py :y g :geometry}] (map vector legends (:placed (stack-geometry legends env (double h))))]
      (draw-legend! legend g c (v/vec2 (+ (double x) (double px)) (+ (double y) (double py))) env))))
