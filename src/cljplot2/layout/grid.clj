(ns cljplot2.layout.grid
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
  Other content is checked by [[cljplot2.graphics.measure/validate-content]] in [[prepare]] and reports its natural size
  via [[cljplot2.graphics.measure/natural-size]].

  Without flexible tracks along an axis the unused space stays after the last track.

  Workflow: [[prepare]] (validate, resolve spans) -> [[measure-items]] -> [[solve]].
  Coordinates are integer pixels, `y` grows downward; track edges (not lengths) are rounded.

  Tracks that don't fit their box throw `ex-info` keyed by the dimension too small, `:width` or `:height`, and marked `:cause :track-space` (see [[space-error?]]; [[cljplot2.pipeline]] words the message for a plot).

  Functions: [[grid?]], [[space-error?]], [[content-items]], [[item-box]], [[find-item-box]], [[find-content-box]]."
  (:require [cljplot2.schema :as schema]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.measure :as measure])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'double} overflow-tolerance
  "Pixels track lengths may exceed the available length by (floating point noise)."
  1.0e-9)

(def ^:private axis->item-key
  "Item span key of a grid axis."
  {:columns :column :rows :row})

(def ^{:private true :const true :tag 'long} reported-duplicates
  "Duplicates an error reports at most."
  10)

(defn- duplicates
  "Gives the first few values occurring more than once in `xs`, for error data.

  Parameters:

  - `xs` (sequence): the values.

  Returns a vector of at most [[reported-duplicates]] values; empty without duplicates."
  [xs]
  (vec (take reported-duplicates (keep (fn [[x n]] (when (m/> (long n) 1) x)) (frequencies xs)))))

(defn grid?
  "Tests whether a content is a nested grid.

  Parameters:

  - `content` (map): a grid item's content.

  Returns true for `:kind :grid`, else false."
  [content]
  (= :grid (:kind content)))

;; validation and span resolution

(defn- validate-track!
  "Checks one track of a grid axis.

  Parameters:

  - `axis` (keyword): `:columns` or `:rows`.
  - `idx` (long): the track's index.
  - `track`: the track, `{:id :size}`.

  Returns `nil`; throws `ex-info {:key axis :index idx}` for a track that isn't a map, an integer id, or a size that isn't one of the namespace doc's."
  [axis ^long idx track]
  (when-not (map? track) (schema/fail! "Track must be a map." {:key axis :index idx}))
  (let [{:keys [id size]} track
        [kind value] (when (vector? size) size)
        error-data {:key axis :index idx :size size}]
    (when (integer? id) (schema/fail! "Track id can't be an integer." {:key axis :index idx :id id}))
    (case kind
      :fixed (when-not (and (number? value) (m/>= (double value) 0.0))
               (schema/fail! "Fixed track size must be a non-negative number." error-data))
      :fraction (when-not (and (number? value) (m/<= 0.0 (double value) 1.0))
                  (schema/fail! "Fraction track size must be a number in [0,1]." error-data))
      :measured nil
      :flexible (when-not (or (nil? value) (and (number? value) (m/pos? (double value))))
                  (schema/fail! "Flexible track weight must be a positive number." error-data))
      (schema/fail! "Unknown track size." error-data))))

(defn- validate-axis!
  "Checks the tracks of a grid axis.

  Parameters:

  - `grid` (map): the grid.
  - `axis` (keyword): `:columns` or `:rows`.

  Returns `nil`; throws `ex-info {:key axis}` for an axis that isn't a non-empty vector, a bad track (see [[validate-track!]]) or duplicate track ids."
  [grid axis]
  (let [tracks (get grid axis)]
    (when-not (and (vector? tracks) (seq tracks))
      (schema/fail! "Grid axis must be a non-empty vector of tracks." {:key axis}))
    (dorun (map-indexed (fn [idx track] (validate-track! axis idx track)) tracks))
    (let [ids (keep :id tracks)]
      (when-not (apply distinct? ::sentinel ids)
        (schema/fail! "Track ids must be unique." {:key axis :duplicates (duplicates ids)})))))

(defn- track-index
  "Gives the index of the track a span reference points at.

  Parameters:

  - `tracks` (vector): the axis's tracks.
  - `axis` (keyword): `:columns` or `:rows`.
  - `item-id`: the item's id (for error data).
  - `track-ref`: an integer index or a track id.

  Returns the index (a long); throws `ex-info {:key :column / :row :item :ref}` for a missing reference, an index out of range or an unknown id."
  [tracks axis item-id track-ref]
  (let [error-data {:key (axis->item-key axis) :item item-id :ref track-ref}]
    (cond
      (nil? track-ref) (schema/fail! "Item span is missing." error-data)
      (integer? track-ref) (let [idx (long track-ref)]
                             (if (and (m/>= idx 0) (m/< idx (count tracks)))
                               idx
                               (schema/fail! "Track index out of range." (assoc error-data :count (count tracks)))))
      :else (or (first (keep-indexed (fn [idx track] (when (= track-ref (:id track)) idx)) tracks))
                (schema/fail! "No track with this id." error-data)))))

(defn- resolve-span
  "Gives the inclusive `[from to]` track indices of an item's span along an axis.

  Parameters:

  - `tracks` (vector): the axis's tracks.
  - `axis` (keyword): `:columns` or `:rows`.
  - `item` (map): the item, its `:column` / `:row` a reference or `{:from :to}`.

  Returns `[from to]` (longs); throws the errors of [[track-index]] and `ex-info` for `:from` after `:to`."
  [tracks axis item]
  (let [span (get item (axis->item-key axis))
        item-id (:id item)]
    (if (map? span)
      (let [from (long (track-index tracks axis item-id (:from span)))
            to (long (track-index tracks axis item-id (:to span)))]
        (when (m/> from to) (schema/fail! "Span :from is after :to." {:key (axis->item-key axis) :item item-id :span span}))
        [from to])
      (let [idx (track-index tracks axis item-id span)] [idx idx]))))

(defn prepare
  "Gives a validated grid with item spans resolved to track indices, recursively for nested grids.

  Parameters:

  - `grid` (map): a grid (see the namespace doc).

  Returns the grid with every item's spans under `::span` (`{:columns [from to] :rows [from to]}`); content is validated by [[cljplot2.graphics.measure/validate-content]].

  Throws `ex-info` (see [[cljplot2.schema]]): `:key` is the grid part at fault - `:columns` / `:rows` for tracks, `:column` / `:row` for item spans, `:items`, `:grid` - with `:index`, `:item` or `:duplicates` locating it."
  [grid]
  (when-not (map? grid) (schema/fail! "Grid must be a map." {:key :grid}))
  (validate-axis! grid :columns)
  (validate-axis! grid :rows)
  (let [items (or (:items grid) [])]
    (when-not (vector? items) (schema/fail! "Grid :items must be a vector." {:key :items}))
    (doseq [item items]
      (when-not (and (map? item) (contains? item :id))
        (schema/fail! "Item must be a map with :id." {:key :items}))
      (when-not (grid? (:content item))
        (measure/validate-content (:content item))))
    (let [ids (map :id items)]
      (when-not (apply distinct? ::sentinel ids)
        (schema/fail! "Item ids must be unique." {:key :items :duplicates (duplicates ids)})))
    (assoc grid :items (mapv (fn [item]
                               (cond-> (assoc item ::span {:columns (resolve-span (:columns grid) :columns item)
                                                           :rows (resolve-span (:rows grid) :rows item)})
                                 (grid? (:content item)) (update :content prepare)))
                             items))))

;; sizes

(defn- natural-length
  "Gives the length of a natural size along a grid axis.

  Parameters:

  - `size` (`Vec2`): a natural size in pixels.
  - `axis` (keyword): `:columns` (the size's x) or `:rows` (its y).

  Returns the length as a double."
  ^double [^Vec2 size axis]
  (if (= axis :columns) (.x size) (.y size)))

(defn- measured-lengths
  "Gives the largest natural length of the items occupying exactly one track, per track.

  Parameters:

  - `axis` (keyword): `:columns` or `:rows`.
  - `items` (sequence): the prepared items.
  - `measurements` (sequence): their measurements (see [[measure-items]]), aligned with `items`.

  Returns a map track index -> length; tracks without such items are absent."
  [axis items measurements]
  (reduce (fn [acc [item measurement]]
            (let [[from to] (get-in item [::span axis])]
              (if (= from to)
                (update acc from (fnil m/max 0.0) (natural-length (:size measurement) axis))
                acc)))
          {} (map vector items measurements)))

(defn- base-lengths
  "Gives the track lengths before the remaining space is shared.

  Parameters:

  - `tracks` (vector): the axis's tracks.
  - `measured` (map): track index -> measured length (see [[measured-lengths]]).
  - `length` (double): the grid's length along the axis.

  Returns a vector of doubles: fixed sizes, fractions of `length`, measured lengths (0 when none), 0 for flexible tracks."
  [tracks measured ^double length]
  (vec (map-indexed (fn [idx {[kind value] :size}]
                      (case kind
                        :fixed (double value)
                        :fraction (m/* length (double value))
                        :measured (double (get measured idx 0.0))
                        :flexible 0.0))
                    tracks)))

(defn- flexible-weight
  "Gives the weight of a track in the remaining space.

  Parameters:

  - `track` (map): a track with `:size` `[kind weight]`.

  Returns a flexible track's weight (default 1), 0 for any other kind."
  ^double [{[kind weight] :size}]
  (if (= kind :flexible) (double (or weight 1.0)) 0.0))

(defn- natural-grid-length
  "Gives the smallest length of a nested grid along an axis, flexible tracks at 0.

  The length `L` solves `L = base + fractions * L` (`base` - the fixed and measured lengths).

  Parameters:

  - `grid` (map): the prepared nested grid.
  - `axis` (keyword): `:columns` or `:rows`.
  - `measurements` (sequence): its items' measurements.

  Returns the length rounded up (integer track edges never overflow); 0 without fixed or measured length; `##Inf` when fractions sum to 1 or more."
  ^double [grid axis measurements]
  (let [tracks (get grid axis)
        base (v/sum (base-lengths tracks (measured-lengths axis (:items grid) measurements) 0.0))
        fractions (v/sum (keep (fn [{[kind value] :size}] (when (= kind :fraction) value)) tracks))]
    (cond
      (m/zero? base) 0.0
      (m/>= fractions 1.0) ##Inf
      :else (m/ceil (m/- (m// base (m/- 1.0 fractions)) overflow-tolerance)))))

(defn- natural-grid-size
  "Gives the smallest size of a nested grid.

  Parameters:

  - `grid` (map): the prepared nested grid.
  - `measurements` (sequence): its items' measurements.

  Returns a `Vec2` of its smallest width and height, see [[natural-grid-length]]."
  [grid measurements]
  (v/vec2 (natural-grid-length grid :columns measurements)
          (natural-grid-length grid :rows measurements)))

(defn- checked-natural-size
  "Gives the natural size of an item's content (see [[cljplot2.graphics.measure/natural-size]]).

  Parameters:

  - `item` (map): the item.
  - `env` (map): the measuring environment.

  Returns the size, a `Vec2`; throws `ex-info {:key :content}` unless it is a `Vec2` with finite, non-negative coordinates."
  [item env]
  (let [size (measure/natural-size (:content item) env)]
    (if (and (instance? Vec2 size)
             (m/valid-double? (.x ^Vec2 size)) (m/>= (.x ^Vec2 size) 0.0)
             (m/valid-double? (.y ^Vec2 size)) (m/>= (.y ^Vec2 size) 0.0))
      size
      (schema/fail! "Natural size must be a Vec2 with finite, non-negative coordinates."
               {:key :content :item (:id item) :kind (:kind (:content item)) :size size}))))

(defn measure-items
  "Gives the natural sizes of the items of a prepared grid.

  Parameters:

  - `grid` (map): the prepared grid (see [[prepare]]).
  - `env` (map): passed to [[cljplot2.graphics.measure/natural-size]] with `:box` set to the item's box in `layout`.
  - `layout` (map): the previous round's result of [[solve]], or `nil`.

  Returns a vector aligned with `:items`, each `{:size Vec2}`; a nested grid adds `:items` (its own measurements)."
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

(def ^:private axis->dimension
  "The box dimension (a render option) a grid axis spans."
  {:columns :width :rows :height})

(defn- space-error-data
  "Gives the data of the error thrown when the tracks of a grid axis don't fit its box.

  Parameters:

  - `axis` (keyword): `:columns` or `:rows`.
  - `available` (long): the box's length along the axis, pixels.
  - `required` (double): the length the fixed, fraction and measured tracks need, pixels.

  Returns `{:key :axis :available :required :cause :track-space}`: `:key` - the dimension too small, `:width` (columns) or `:height` (rows), the render option to raise; `:required` rounded up to whole pixels (`##Inf` stays, fractions summing to 1 or more); `:cause` - the marker [[space-error?]] tests."
  [axis ^long available ^double required]
  {:key (axis->dimension axis) :axis axis :available available :cause :track-space
   :required (if (m/valid-double? required) (long (m/ceil (m/- required overflow-tolerance))) required)})

(defn space-error?
  "Tests whether error data is that of a grid axis whose tracks don't fit its box.

  Parameters:

  - `data` (map or `nil`): `ex-data` of an error.

  Returns true for data marked `:cause :track-space` (see [[solve]]: `:key` `:width` / `:height`, `:axis`, `:available`, `:required` pixels), else false."
  [data]
  (= :track-space (:cause data)))

(defn- axis-edges
  "Gives the integer track edges along an axis.

  Parameters:

  - `tracks` (vector): the axis's tracks.
  - `axis` (keyword): `:columns` or `:rows`.
  - `measured` (map): see [[measured-lengths]].
  - `[start length]` (longs): the interval the tracks fill.

  Returns a vector of `(inc (count tracks))` longs, the first `start`. Cumulative edges are rounded (not lengths), so tracks tile without gaps. With flexible tracks the last edge is `start + length`; without them the remaining space stays after the last track.

  Throws `ex-info {:key :axis :available :required :cause :track-space}` when the fixed, fraction and measured tracks don't fit (see [[space-error-data]])."
  [tracks axis measured [^long start ^long length]]
  (let [base (base-lengths tracks measured length)
        used (v/sum base)]
    (when (m/> (m/- used length) overflow-tolerance)
      (let [data (space-error-data axis length used)]
        (schema/fail! (str "Not enough space: the grid's fixed and measured tracks need " (:required data) " px of " (name (:key data)) ", " length " px given.") data)))
    (let [weights (mapv flexible-weight tracks)
          total-weight (v/sum weights)
          free (m/- length used)
          lengths (if (m/pos? total-weight)
                    (map (fn [^double b ^double w] (m/+ b (m// (m/* free w) total-weight))) base weights)
                    base)]
      (mapv (fn [^double edge] (m/round edge))
            (reductions (fn [^double edge ^double len] (m/+ edge len)) (double start) lengths)))))

(defn- tracks-layout
  "Gives the solved tracks of an axis.

  Parameters:

  - `tracks` (vector): the axis's tracks.
  - `edges` (vector of longs): the track edges, one more than tracks (see [[axis-edges]]).

  Returns a vector of `{:index :id :offset :size}` (pixels), one per track."
  [tracks edges]
  (vec (map-indexed (fn [^long idx track]
                      (let [offset (long (edges idx))]
                        {:index idx
                         :id (:id track)
                         :offset offset
                         :size (m/long-sub (long (edges (m/long-inc idx))) offset)}))
                    tracks)))

(defn solve
  "Gives the layout of a prepared grid inside a box.

  Parameters:

  - `grid` (map): the prepared grid (see [[prepare]]).
  - `box` (map): `{:x :y :w :h}`, integer pixels, non-negative size.
  - `measurements` (sequence): one per item (see [[measure-items]]).

  Returns a layout map: `:box` - the grid's box; `:columns`, `:rows` - tracks as `{:index :id :offset :size}`; `:items` - in input order, `{:id :box :span}`, plus `:layout` for a nested grid.

  Throws `ex-info` with `{:key :axis :available :required :cause :track-space}` when fixed, fraction and measured tracks don't fit (see [[space-error?]]), and with `{:key ...}` when `box` or `measurements` are malformed."
  [grid box measurements]
  (let [{:keys [x y w h]} box
        items (:items grid)]
    (when-not (and (every? integer? [x y w h]) (m/>= (long w) 0) (m/>= (long h) 0))
      (schema/fail! "Box must have integer :x :y and non-negative integer :w :h." {:key :box :box box}))
    (when-not (and (or (nil? measurements) (sequential? measurements)) (= (count measurements) (count items)))
      (schema/fail! "Measurements must align with items." {:key :measurements :items (count items) :measurements (when (or (nil? measurements) (coll? measurements)) (count measurements))}))
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
                            cell-box {:x left :y top
                                      :w (m/long-sub (long (xs (m/long-inc c1))) left)
                                      :h (m/long-sub (long (ys (m/long-inc r1))) top)}
                            content (:content item)]
                        (cond-> {:id (:id item) :box cell-box :span span}
                          (grid? content) (assoc :layout (solve content cell-box (:items measurement))))))
                    items measurements)})))

(defn item-box
  "Gives the box of a top-level item of a layout.

  Parameters:

  - `layout` (map): a solved layout (see [[solve]]).
  - `id`: the item's `:id`.

  Returns the item's `{:x :y :w :h}`; `nil` when no top-level item has `id` (nested layouts aren't searched, see [[find-item-box]])."
  [layout id]
  (some (fn [item] (when (= id (:id item)) (:box item))) (:items layout)))

(defn find-item-box
  "Gives the box of the first item with an id in a layout or its nested layouts.

  Parameters:

  - `layout` (map): a solved layout (see [[solve]]).
  - `id`: the item's `:id`.

  Returns the `{:x :y :w :h}` of the first match, depth-first in item order; `nil` when none. See also [[item-box]]."
  [layout id]
  (some (fn [item]
          (if (= id (:id item))
            (:box item)
            (some-> (:layout item) (find-item-box id))))
        (:items layout)))

(defn content-items
  "Gives the items of a grid and its nested grids whose content satisfies a predicate.

  Parameters:

  - `grid` (map): the grid.
  - `layout` (map): its solved layout (see [[solve]]), items in the same order.
  - `pred` (fn of content): selects the items.

  Returns a vector of `{:id :content :box}` (depth-first, in item order), boxes from `layout`; a nested grid matching `pred` isn't searched."
  [grid layout pred]
  (vec (mapcat (fn [item item-layout]
                 (let [content (:content item)]
                   (cond
                     (pred content) [{:id (:id item) :content content :box (:box item-layout)}]
                     (grid? content) (content-items content (:layout item-layout) pred))))
               (:items grid) (:items layout))))

(defn find-content-box
  "Gives the box of the first grid item whose content satisfies a predicate.

  Parameters:

  - `grid` (map): the grid whose items' contents are tested (nested grids searched depth-first, in item order).
  - `layout` (map): the grid's solved layout (see [[solve]]), its items in the grid's order.
  - `pred` (fn of a content): the test.

  Returns the matching item's `{:x :y :w :h}`; `nil` when none."
  [grid layout pred]
  (some (fn [[item item-layout]]
          (let [content (:content item)]
            (cond
              (pred content) (:box item-layout)
              (grid? content) (find-content-box content (:layout item-layout) pred))))
        (map vector (:items grid) (:items layout))))
