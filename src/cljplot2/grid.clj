(ns cljplot2.grid
  "Track grid: a box divided into column and row tracks, with items placed on track ranges.

  A grid is a map:

  * `:columns`, `:rows` - non-empty vectors of tracks `{:id id :size size}`;
    `:id` is optional, unique within the axis and never an integer (integers are indices)
  * `:items` - vector of items `{:id id :column span :row span :content content}`; `:id` is required and unique

  Track `:size`:

  * `[:fixed px]` - `px` pixels, `px >= 0`; a fractional `px` comes out +/-1 px depending on the track's offset
  * `[:fraction f]` - fraction of the grid's own box along the axis, `0 <= f <= 1`
  * `[:measured]` - largest natural size of the items occupying exactly this one track along the axis
  * `[:flexible weight]` - share of the remaining space proportional to `weight > 0` (default `1`)

  Span: a track reference (integer index or track id) or `{:from ref :to ref}` (inclusive).

  Content with `:kind :grid` is a nested grid (same shape), laid out inside the item's box.
  Other content is checked by [[cljplot2.measure/validate-content]] in [[prepare]] and reports its natural size
  via [[cljplot2.measure/natural-size]].

  Without flexible tracks along an axis the unused space stays after the last track.

  Workflow: [[prepare]] (validate, resolve spans) -> [[measure-items]] -> [[solve]].
  Coordinates are integer pixels, `y` grows downward; track edges (not lengths) are rounded."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.measure :as measure])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const overflow-tolerance 1.0e-9)

(def ^:private axis->item-key {:columns :column :rows :row})

(defn- invalid [message data] (throw (ex-info message data)))

(def ^:private ^:const reported-duplicates 10)

(defn- duplicates
  "First few values occurring more than once in `xs` (for error data)."
  [xs]
  (vec (take reported-duplicates (keep (fn [[x n]] (when (> (long n) 1) x)) (frequencies xs)))))

(defn grid?
  "Is `content` a nested grid?"
  [content]
  (= :grid (:kind content)))

;; validation and span resolution

(defn- validate-track!
  [axis ^long idx track]
  (when-not (map? track) (invalid "Track must be a map." {:key axis :index idx}))
  (let [{:keys [id size]} track
        [kind value] (when (vector? size) size)
        error-data {:key axis :index idx :size size}]
    (when (integer? id) (invalid "Track id can't be an integer." {:key axis :index idx :id id}))
    (case kind
      :fixed (when-not (and (number? value) (>= (double value) 0.0))
               (invalid "Fixed track size must be a non-negative number." error-data))
      :fraction (when-not (and (number? value) (>= (double value) 0.0) (<= (double value) 1.0))
                  (invalid "Fraction track size must be a number in [0,1]." error-data))
      :measured nil
      :flexible (when-not (or (nil? value) (and (number? value) (pos? (double value))))
                  (invalid "Flexible track weight must be a positive number." error-data))
      (invalid "Unknown track size." error-data))))

(defn- validate-axis!
  [grid axis]
  (let [tracks (get grid axis)]
    (when-not (and (vector? tracks) (seq tracks))
      (invalid "Grid axis must be a non-empty vector of tracks." {:key axis}))
    (dorun (map-indexed (fn [idx track] (validate-track! axis idx track)) tracks))
    (let [ids (keep :id tracks)]
      (when-not (apply distinct? ::sentinel ids)
        (invalid "Track ids must be unique." {:key axis :duplicates (duplicates ids)})))))

(defn- track-index
  [tracks axis item-id ref]
  (let [error-data {:key (axis->item-key axis) :item item-id :ref ref}]
    (cond
      (nil? ref) (invalid "Item span is missing." error-data)
      (integer? ref) (let [idx (long ref)]
                       (if (and (>= idx 0) (< idx (count tracks)))
                         idx
                         (invalid "Track index out of range." (assoc error-data :count (count tracks)))))
      :else (or (first (keep-indexed (fn [idx track] (when (= ref (:id track)) idx)) tracks))
                (invalid "No track with this id." error-data)))))

(defn- resolve-span
  "Inclusive `[from to]` track indices of the item's span along `axis`."
  [tracks axis item]
  (let [span (get item (axis->item-key axis))
        item-id (:id item)]
    (if (map? span)
      (let [from (long (track-index tracks axis item-id (:from span)))
            to (long (track-index tracks axis item-id (:to span)))]
        (when (> from to) (invalid "Span :from is after :to." {:key (axis->item-key axis) :item item-id :span span}))
        [from to])
      (let [idx (track-index tracks axis item-id span)] [idx idx]))))

(defn prepare
  "Validate `grid` and resolve item spans to track indices (stored under `::span`), recursively for nested grids.
  Throws `ex-info` naming the offending key."
  [grid]
  (when-not (map? grid) (invalid "Grid must be a map." {:key :grid}))
  (validate-axis! grid :columns)
  (validate-axis! grid :rows)
  (let [items (or (:items grid) [])]
    (when-not (vector? items) (invalid "Grid :items must be a vector." {:key :items}))
    (doseq [item items]
      (when-not (and (map? item) (contains? item :id))
        (invalid "Item must be a map with :id." {:key :items}))
      (when-not (grid? (:content item))
        (measure/validate-content (:content item))))
    (let [ids (map :id items)]
      (when-not (apply distinct? ::sentinel ids)
        (invalid "Item ids must be unique." {:key :items :duplicates (duplicates ids)})))
    (assoc grid :items (mapv (fn [item]
                               (cond-> (assoc item ::span {:columns (resolve-span (:columns grid) :columns item)
                                                           :rows (resolve-span (:rows grid) :rows item)})
                                 (grid? (:content item)) (update :content prepare)))
                             items))))

;; sizes

(defn- natural-length
  ^double [^Vec2 size axis]
  (if (= axis :columns) (.x size) (.y size)))

(defn- measured-lengths
  "Map track index -> largest natural length of items occupying only that track along `axis`."
  [axis items measurements]
  (reduce (fn [acc [item measurement]]
            (let [[from to] (get-in item [::span axis])]
              (if (= from to)
                (update acc from (fnil max 0.0) (natural-length (:size measurement) axis))
                acc)))
          {} (map vector items measurements)))

(defn- base-lengths
  "Track lengths before flexible distribution: fixed, fraction of `length`, `measured` (see [[measured-lengths]]);
  flexible tracks get 0."
  [tracks measured ^double length]
  (vec (map-indexed (fn [idx {[kind value] :size}]
                      (case kind
                        :fixed (double value)
                        :fraction (* length (double value))
                        :measured (double (get measured idx 0.0))
                        :flexible 0.0))
                    tracks)))

(defn- flexible-weight
  ^double [{[kind weight] :size}]
  (if (= kind :flexible) (double (or weight 1.0)) 0.0))

(defn- natural-grid-length
  "Smallest length `L` of a nested grid along `axis` with flexible tracks at 0: `L = base + fractions * L`,
  `base` = fixed + measured. Rounded up, so integer track edges never overflow. Infinite when fractions sum to 1 or more and `base > 0`."
  ^double [grid axis measurements]
  (let [tracks (get grid axis)
        base (double (reduce + (base-lengths tracks (measured-lengths axis (:items grid) measurements) 0.0)))
        fractions (double (reduce + (keep (fn [{[kind value] :size}] (when (= kind :fraction) value)) tracks)))]
    (cond
      (zero? base) 0.0
      (>= fractions 1.0) ##Inf
      :else (m/ceil (- (/ base (- 1.0 fractions)) overflow-tolerance)))))

(defn- natural-grid-size
  "Smallest size of a nested grid, see [[natural-grid-length]]."
  [grid measurements]
  (v/vec2 (natural-grid-length grid :columns measurements)
          (natural-grid-length grid :rows measurements)))

(defn- checked-natural-size
  "[[cljplot2.measure/natural-size]] of `item`'s content, checked to be a `Vec2` with finite, non-negative coordinates."
  [item env]
  (let [size (measure/natural-size (:content item) env)]
    (if (and (instance? Vec2 size)
             (m/valid-double? (.x ^Vec2 size)) (>= (.x ^Vec2 size) 0.0)
             (m/valid-double? (.y ^Vec2 size)) (>= (.y ^Vec2 size) 0.0))
      size
      (invalid "Natural size must be a Vec2 with finite, non-negative coordinates."
               {:key :content :item (:id item) :kind (:kind (:content item)) :size size}))))

(defn measure-items
  "Natural sizes of the items of prepared `grid`: a vector aligned with `:items`, each `{:size Vec2}`;
  nested grids add `:items` (their own measurements).

  `env` is passed to [[cljplot2.measure/natural-size]] with `:box` set to the item's box in `layout`
  (the previous round's result of [[solve]], or `nil`)."
  [grid env layout]
  (mapv (fn [item item-layout]
          (let [content (:content item)
                item-env (assoc env :box (:box item-layout))]
            (if (grid? content)
              (let [children (measure-items content item-env (:layout item-layout))]
                {:size (natural-grid-size content children) :items children})
              {:size (checked-natural-size item item-env)})))
        (:items grid)
        (concat (:items layout) (repeat nil))))

;; solving

(defn- axis-edges
  "Integer track edges along `axis` for the `[start length]` interval: `(inc (count tracks))` positions, first = `start`.
  `measured` - see [[measured-lengths]].
  Cumulative edges are rounded (not lengths), so tracks tile without gaps.
  With flexible tracks the last edge is `start + length`; without them the remaining space stays after the last track."
  [tracks axis measured [^long start ^long length]]
  (let [base (base-lengths tracks measured length)
        used (double (reduce + base))]
    (when (> (- used length) overflow-tolerance)
      (invalid "Not enough space for tracks." {:axis axis :available length :required used}))
    (let [weights (mapv flexible-weight tracks)
          total-weight (double (reduce + weights))
          free (- length used)
          lengths (if (pos? total-weight)
                    (mapv (fn [b w] (+ (double b) (/ (* free (double w)) total-weight))) base weights)
                    base)]
      (mapv (fn [edge] (m/round (double edge)))
            (reductions (fn [^double edge ^double len] (+ edge len)) (double start) lengths)))))

(defn- tracks-layout
  [tracks edges]
  (vec (map-indexed (fn [^long idx track]
                      (let [offset (long (edges idx))]
                        {:index idx
                         :id (:id track)
                         :offset offset
                         :size (- (long (edges (inc idx))) offset)}))
                    tracks)))

(defn solve
  "Lay out prepared `grid` inside `box` (`{:x :y :w :h}`, integer pixels) using item `measurements` (see [[measure-items]]).

  Returns a layout map:

  * `:box` - the grid's box
  * `:columns`, `:rows` - tracks as `{:index :id :offset :size}`
  * `:items` - in input order, `{:id :box :span}`, plus `:layout` for a nested grid

  Throws `ex-info` with `{:axis :available :required}` when fixed, fraction and measured tracks don't fit,
  and with `{:key ...}` when `box` or `measurements` (one per item) are malformed."
  [grid box measurements]
  (let [{:keys [x y w h]} box
        items (:items grid)]
    (when-not (and (every? integer? [x y w h]) (>= (long w) 0) (>= (long h) 0))
      (invalid "Box must have integer :x :y and non-negative integer :w :h." {:key :box :box box}))
    (when-not (and (or (nil? measurements) (sequential? measurements)) (= (count measurements) (count items)))
      (invalid "Measurements must align with items." {:key :measurements :items (count items) :measurements (when (or (nil? measurements) (coll? measurements)) (count measurements))}))
    (let [xs (axis-edges (:columns grid) :columns (measured-lengths :columns items measurements) [(long x) (long w)])
          ys (axis-edges (:rows grid) :rows (measured-lengths :rows items measurements) [(long y) (long h)])]
      {:box box
       :columns (tracks-layout (:columns grid) xs)
       :rows (tracks-layout (:rows grid) ys)
       :items (mapv (fn [item measurement]
                      (let [span (::span item)
                            [^long c0 ^long c1] (:columns span)
                            [^long r0 ^long r1] (:rows span)
                            left (long (xs c0))
                            top (long (ys r0))
                            item-box {:x left :y top
                                      :w (- (long (xs (inc c1))) left)
                                      :h (- (long (ys (inc r1))) top)}
                            content (:content item)]
                        (cond-> {:id (:id item) :box item-box :span span}
                          (grid? content) (assoc :layout (solve content item-box (:items measurement))))))
                    items measurements)})))

(defn item-box
  "Box of the top-level item with `id` in `layout`, or `nil`."
  [layout id]
  (some (fn [item] (when (= id (:id item)) (:box item))) (:items layout)))

(defn find-item-box
  "Box of the first item with `id` in `layout` or its nested layouts (depth-first, in item order), or `nil`."
  [layout id]
  (some (fn [item]
          (if (= id (:id item))
            (:box item)
            (some-> (:layout item) (find-item-box id))))
        (:items layout)))
