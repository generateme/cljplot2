(ns cljplot2.content.axis
  "Axis content: an axis line, its ticks and tick labels beside a panel.

  Content `{:kind :axis :scale :x|:y}` draws a line on the panel edge, outward ticks and tick labels
  (`:tick-label` text role) of the panel-local finalized scale in `env` `:scales` (the axis box spans its panel
  column for a horizontal scale, its panel row for a vertical one, so positions are offset by the box's x / y).
  Styled by theme `[:axis :x|:y]` - by scale key, also when a flipped plot draws `:x` vertically.

  Geometry follows the scale's direction ([[cljplot2.scale.position/direction]]): a horizontal axis sits below the
  panel (line in its box's top pixel row), a vertical one left of it (line in its box's rightmost pixel column).
  `{:kind :axis ... :margin side}` - the magnitude axis of a plot margin: its scale from `env` `[:margins side :scales]`.
  `{:kind :axis ... :panel k}` - the axis of facet panel `k` (its panel-local scale, see
  [[cljplot2.scale.position/panel-scales-of]]). An axis shows a scale's display scale (its `:axis-domain`, see
  [[cljplot2.scale.position/display-scale]]).
  Minor ticks (a finalized scale's `:minor-ticks`) are drawn unlabelled, theme `[:axis k :minor-tick-length]` long.
  Natural size: horizontal - height = tick length + label gap + tallest label; vertical - width = tick length +
  label gap + widest label. Without a finalized scale in `env` the axis is empty.

  Tick labels at the axis ends stick out past the axis box. `{:kind :axis-overhang :scale :x|:y :end :start|:end}`
  reserves that room: its natural size along the axis is how far the label of the tick nearest that end of the
  range sticks out beyond it (0 when it doesn't).

  Bands axes tick every band; when tick labels would overlap, only every `s`-th label is drawn (`s` the smallest step
  without overlap, starting at the first band) - see [[label-step]]. Other axes (linear, log, symlog, dates ...) draw
  the first label, then each label not overlapping the last drawn one - see [[drawn-label-indices]]; a scale with
  `:overlapping-labels :keep` draws them all (axes naming data items, e.g. the leaves of a dendrogram).

  Tick labels rotate by theme `[:axis k :label-rotation]` (clockwise degrees, -90..90; e.g. -90 or -45 for long
  bands labels): a horizontal axis puts the end of each rotated text nearest the axis at its tick (see
  [[label-offset-fn]]), a vertical axis keeps labels centred on their ticks; natural sizes, thinning and overhangs use
  the rotated label boxes. An axis followed by another panel's axis `:next-panel-gap` pixels further (set by the facet
  grid: right of a horizontal axis, below a vertical one) skips its labels that would overlap the next panel's drawn
  labels (the next panel keeps its own): those of the next panel's own scale with `:next-panel k` (panel-local scales;
  `k` may be `nil`, the NA facet level - see [[crossing-labels]]), else its identical axis.

  Functions: [[label-spans]], [[label-step]], [[crossing-labels]], [[drawn-label-indices]]; schemas,
 ; methods of [[cljplot2.graphics.measure/validate-content]],
  [[cljplot2.graphics.measure/natural-size]] and [[cljplot2.graphics.draw/draw-content]] for `:axis` and
  `:axis-overhang`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private axis-schema
  "Schema of `{:kind :axis}` content: `:scale` `:x` or `:y`; optional `:next-panel-gap` - pixels to the next panel's
  axis (facet columns or rows): labels overlapping that axis's labels aren't drawn; optional `:margin` (`:top`,
  `:right`), `:panel`, `:next-panel` (see the namespace doc)."
  [:map [:scale [:enum :x :y]] (schema/optional :next-panel-gap schema/non-neg)
   (schema/optional :margin [:enum :top :right]) (schema/optional :panel any?) (schema/optional :next-panel any?)])

(defn- axis-scale
  "Gives the finalized display scale of an axis.

  Parameters:

  - `content` (map): the axis content - `:scale`, optional `:panel` and `:margin`.
  - `env` (map): `:scales` (shared), facet panel scales, `:margins`.

  Returns the display scale (see [[cljplot2.scale.position/display-scale]]) of `:scale` among the scales of the
  content's `:panel` (see [[cljplot2.scale.position/panel-scales-of]]; the shared `env` `:scales` without one), or
  of the plot margin's scales (`env` `[:margins side :scales]`) for `:margin side`; `nil` without one."
  [{k :scale margin :margin :as content} env]
  (some-> (if margin (get-in env [:margins margin :scales k]) (get (scale/panel-scales-of env content) k))
          scale/display-scale))

(def ^:private overhang-schema
  "Schema of `{:kind :axis-overhang}` content: `:scale` `:x` or `:y`, `:end` `:start` or `:end`; optional `:margin`
  and `:panel` as the axis's."
  [:map [:scale [:enum :x :y]] [:end [:enum :start :end]] (schema/optional :margin [:enum :top :right])
   (schema/optional :panel any?)])

(defmethod measure/validate-content :axis [content] (schema/explain! [[axis-schema content [:content]]]))
(defmethod measure/validate-content :axis-overhang [content] (schema/explain! [[overhang-schema content [:content]]]))

(defn- tick-label
  "Gives the label content of a tick label.

  Parameters:

  - `text`: the label text.
  - `rotation` (number): clockwise degrees (default 0).

  Returns `{:kind :label :role :tick-label :text text :rotation rotation}`."
  ([text] (tick-label text 0))
  ([text rotation] {:kind :label :role :tick-label :text text :rotation rotation}))

(defn- label-rotation
  "Gives the tick label rotation of an axis.

  Parameters:

  - `k` (keyword): the axis's scale.
  - `env` (map): `:theme`, `[:axis k :label-rotation]` read.

  Returns the rotation in clockwise degrees; 0 when the theme gives none."
  [k env]
  (get-in env [:theme :axis k :label-rotation] 0))

(defn- label-sizes
  "Gives the natural sizes of the rotated tick labels of an axis.

  Parameters:

  - `k` (keyword): the axis's scale.
  - `finalized` (map): the finalized axis scale, `:tick-labels` read.
  - `env` (map): `:theme`, `:canvas` (for measuring).

  Returns a vector of `Vec2`s, one per label (indexed and read several times)."
  [k finalized env]
  (let [rotation (label-rotation k env)]
    (mapv #(measure/natural-size (tick-label % rotation) env) (:tick-labels finalized))))

(defn- label-offset-fn
  "Gives the offset of a tick label's box centre from its tick along the axis of scale `k`, as a function of the
  label text.

  Labels of a horizontal axis rotated by `r` (not 0) put the end of the text nearest the axis at the tick: the centre
  moves by `sign(r) * w/2 * cos(r)` (`w` - the unrotated text frame width); vertical axes and unrotated labels keep
  labels centred (offset 0).

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.
  - `env` (map): `:theme`, `:canvas` (measuring).

  Returns a function of a text giving a double."
  [k finalized env]
  (let [rotation (double (label-rotation k env))]
    (if (or (m/zero? rotation) (not (scale/horizontal? k finalized)))
      (constantly 0.0)
      (let [factor (m/* 0.5 (m/signum rotation) (m/cos (m/radians rotation)))]
        (fn [text] (m/* factor (double (first (label/text-frame (tick-label text) (:canvas env) (:theme env))))))))))

(defn- label-offsets
  "Gives the offset of each tick label's box centre from its tick, along the axis (see [[label-offset-fn]]).

  Parameters:

  - `k` (keyword): the axis's scale.
  - `finalized` (map): the finalized axis scale, `:tick-labels` read.
  - `env` (map): `:theme`, `:canvas` (for measuring).

  Returns a vector of offsets in pixels, one per label (read several times)."
  [k finalized env]
  (mapv (label-offset-fn k finalized env) (:tick-labels finalized)))

(defn label-spans
  "Gives per tick label the length along the axis it needs so it doesn't overlap its neighbour.

  Rotated labels are parallel, so a label needs the smaller of its box length along the axis and its text frame
  height over the sine of the angle between the text and the axis.

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.
  - `env` (map): `:theme`, `:canvas` (measuring).
  - `sizes` (sequence of `Vec2`): the labels' natural sizes.

  Returns a vector of doubles, one per label; `nil` for unrotated labels (their size along the axis applies). A text
  parallel to the axis needs its box length.

  See also [[label-step]], [[drawn-label-indices]]."
  [k finalized env sizes]
  (let [rotation (double (label-rotation k env))]
    (when-not (m/zero? rotation)
      (let [horizontal? (scale/horizontal? k finalized)
            a (m/radians rotation)
            sine (m/abs (if horizontal? (m/sin a) (m/cos a)))]
        (mapv (fn [text ^Vec2 size]
                (let [along (if horizontal? (.x size) (.y size))
                      frame-h (double (second (label/text-frame (tick-label text) (:canvas env) (:theme env))))]
                  (if (m/pos? sine) (m/min along (m// frame-h sine)) along)))
              (:tick-labels finalized) sizes)))))

(defn- label-inset
  "Gives the inset of tick labels (their text role's `:inset`).

  Parameters:

  - `env` (map): `:theme`, the tick label text role read.

  Returns the inset in pixels on every side."
  [env]
  (:inset (theme/text-style (:theme env) (tick-label nil))))

(defn- label-intervals
  "Gives the intervals of an axis's tick labels along the axis.

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.
  - `sizes` (sequence of `Vec2`): the labels' natural sizes.
  - `inset` (number): the labels' inset (pixels on every side).
  - `offsets` (sequence of doubles): the labels' offsets from their ticks (see [[label-offsets]]); `nil` - none.
  - `spans` (sequence of doubles): the labels' lengths along the axis (see [[label-spans]]); `nil` - the sizes along
    the axis.

  Returns `[centres extents]`, vectors (indexed): a centre is the tick position moved by its offset, an extent the
  label's text block length (span or size, without the inset on both sides)."
  [k finalized sizes inset offsets spans]
  (let [forward (scale/forward-fn finalized)
        inset2 (m/* 2.0 (double inset))
        horizontal? (scale/horizontal? k finalized)]
    [(mapv (fn [tick off] (m/+ (double (forward tick)) (double off))) (:ticks finalized) (or offsets (repeat 0.0)))
     (if spans
       (mapv #(m/- (double %) inset2) spans)
       (mapv (fn [^Vec2 size] (m/- (if horizontal? (.x size) (.y size)) inset2)) sizes))]))

(defn label-step
  "Gives the step between the drawn tick labels of a bands axis.

  Labels overlap when their text blocks overlap by more than 1 px (see [[label-intervals]]).

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.
  - `sizes` (sequence of `Vec2`): the labels' natural sizes.
  - `inset` (number): the labels' inset.
  - `offsets`, `spans`: as [[label-intervals]] (default `nil`).

  Returns 1 unless `finalized` is a bands scale with at least 2 labels; then the smallest `s` such that labels
  `0, s, 2s, ...` don't overlap along the axis - at most the label count (only the first label drawn).

  See also [[drawn-label-indices]]."
  ([k finalized sizes inset] (label-step k finalized sizes inset nil nil))
  ([k finalized sizes inset offsets spans]
   (let [n (count sizes)]
     (if (or (not= :bands (:kind finalized)) (m/< n 2))
       1
       (let [[centres extents] (label-intervals k finalized sizes inset offsets spans)
             fits? (fn [^long step]
                     (every? (fn [^long i] (let [j (m/long-add i step)]
                                             (not (measure/spans-overlap? (centres i) (extents i) (centres j) (extents j)))))
                             (range 0 (m/long-sub n step) step)))]
         (or (first (filter fits? (range 1 n))) n))))))

(defn crossing-labels
  "Gives the drawn labels that overlap a drawn label of the next panel's axis.

  These are skipped so labels never cross into a neighbouring facet panel's labels (the next panel keeps its own).

  Parameters:

  - `[centres extents]`: the intervals of this axis's labels along the axis (see [[label-intervals]]).
  - `drawn` (sequence of longs): the indices of the labels this axis draws.
  - `[next-centres next-extents]`, `next-drawn`: the same of the next panel's axis (panel-local); without them the
    next axis is identical (the same labels).
  - `shift` (number): how much further along the next axis starts (panel length plus gap).

  Returns the set of the overlapping indices of `drawn`."
  ([intervals drawn shift] (crossing-labels intervals drawn intervals drawn shift))
  ([[centres extents] drawn [next-centres next-extents] next-drawn shift]
   (set (filter (fn [i]
                  (some (fn [j] (measure/spans-overlap? (centres i) (extents i) (m/+ (double shift) (double (next-centres j))) (next-extents j)))
                        next-drawn))
                drawn))))

(defn drawn-label-indices
  "Gives the indices of the tick labels an axis draws.

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.
  - `sizes`, `inset`, `offsets`, `spans`: as [[label-intervals]].

  Returns a vector of indices: all with `:overlapping-labels :keep` (axes naming data items); a bands scale every
  [[label-step]]-th label from the first; other scales the labels kept by
  [[cljplot2.graphics.measure/kept-label-indices]] (the first, then each not overlapping the last drawn one). Empty
  for no labels."
  [k finalized sizes inset offsets spans]
  (cond
    (= :keep (:overlapping-labels finalized)) (vec (range (count sizes)))
    (= :bands (:kind finalized))
    (let [step (long (label-step k finalized sizes inset offsets spans))]
      (filterv #(m/zero? (m/long-mod (long %) step)) (range (count sizes))))
    :else (let [[centres extents] (label-intervals k finalized sizes inset offsets spans)]
            (measure/kept-label-indices centres extents))))

(defn- drawn-labels
  "Gives the intervals of an axis's tick labels and the labels it draws.

  Parameters:

  - `k` (keyword): the axis's scale.
  - `finalized` (map): the finalized axis scale, `:tick-labels` read.
  - `env` (map): `:theme`, `:canvas` (for measuring).

  Returns `[[centres extents] drawn]`: the labels' intervals along the axis (panel-local, see [[label-intervals]]) and the indices of the drawn labels (see [[drawn-label-indices]])."
  [k finalized env]
  (let [sizes (label-sizes k finalized env)
        offsets (label-offsets k finalized env)
        spans (label-spans k finalized env sizes)
        inset (label-inset env)]
    [(label-intervals k finalized sizes inset offsets spans)
     (drawn-label-indices k finalized sizes inset offsets spans)]))

(defn- tick-room
  "Gives the pixels an axis's ticks take across it.

  Parameters:

  - `finalized` (map): the finalized axis scale, `:minor-ticks` read.
  - `style` (map): the theme axis style, `:tick-length` and `:minor-tick-length` read.

  Returns the tick length, or the minor tick length when the scale has minor ticks and they are longer."
  ^double [finalized {:keys [tick-length minor-tick-length]}]
  (if (seq (:minor-ticks finalized))
    (m/max (double tick-length) (double minor-tick-length))
    (double tick-length)))

(defmethod measure/natural-size :axis
  [{k :scale :as content} env]
  (let [finalized (axis-scale content env)
        {:keys [label-gap] :as style} (get-in env [:theme :axis k])
        room (when finalized (tick-room finalized style))
        all-sizes (when finalized (label-sizes k finalized env))
        drawn (when finalized
                (set (drawn-label-indices k finalized all-sizes (label-inset env) (label-offsets k finalized env)
                                          (label-spans k finalized env all-sizes))))
        sizes (keep-indexed (fn [i size] (when (contains? drawn i) size)) all-sizes)
        ^Vec2 largest (measure/max-size sizes)
        widest (.x largest)
        tallest (.y largest)]
    (cond
      (nil? finalized) (v/vec2 0.0 0.0)
      (scale/horizontal? k finalized) (v/vec2 0.0 (m/+ (double room) (double label-gap) tallest))
      :else (v/vec2 (m/+ (double room) (double label-gap) widest) 0.0))))

(defmethod measure/natural-size :axis-overhang
  [{k :scale end :end :as content} env]
  (let [finalized (axis-scale content env)
        ticks (:ticks finalized)]
    (if (empty? ticks)
      (v/vec2 0.0 0.0)
      (let [forward (scale/forward-fn finalized)
            [a b] (:range finalized)
            edge (double (if (= end :start) a b))
            nearest (apply min-key #(m/abs (m/- edge (double (forward %)))) ticks)
            i (.indexOf ^java.util.List ticks nearest)
            ^Vec2 size (measure/natural-size (tick-label ((:tick-labels finalized) i) (label-rotation k env)) env)
            horizontal? (scale/horizontal? k finalized)
            half (m/* 0.5 (if horizontal? (.x size) (.y size)))
            label-offset (double ((label-offset-fn k finalized env) ((:tick-labels finalized) i)))
            p (double (forward nearest))
            ;; rounded up like label sizes, so integer track edges never clip the label
            overhang (m/ceil (m/max 0.0 (if (m/zero? label-offset)
                                          (m/- half (m/abs (m/- edge p)))
                                          ;; a moved label box: how far it reaches past the edge on the edge's side
                                          (let [centre (m/+ p label-offset)
                                                other (double (if (= end :start) b a))]
                                            (if (m/>= edge other)
                                              (m/- (m/+ centre half) edge)
                                              (m/- edge (m/- centre half)))))))]
        (if horizontal? (v/vec2 overhang 0.0) (v/vec2 0.0 overhang))))))

(defn- set-line-style!
  "Sets the canvas colour and stroke of an axis line or its ticks: solid, butt caps.

  Parameters:

  - `c`: the canvas in graphics context.
  - `color`: the line colour.
  - `width` (number): the stroke width in pixels.

  Returns the canvas. See also [[cljplot2.graphics.draw/visible-stroke?]]."
  [c color width]
  (c2d/set-color c color)
  (c2d/set-stroke-custom c {:size width :cap :butt}))

(defmethod draw/draw-content :axis
  [{k :scale :as content} c {:keys [^double x ^double y ^double w ^double h]} env]
  (when-let [finalized (axis-scale content env)]
    (let [{:keys [line-color line-width tick-color ^double tick-length tick-width ^double label-gap ^double minor-tick-length]
           :as style} (get-in env [:theme :axis k])
          room (tick-room finalized style)
          horizontal? (scale/horizontal? k finalized)
          ;; panel-local positions; the axis box spans its panel column (horizontal) / row (vertical)
          offset (if horizontal? x y)
          forward (scale/forward-fn finalized)
          ;; read by the ticks and the labels
          positions (mapv #(m/+ offset (double (forward %))) (:ticks finalized))
          right (m/+ x w)
          line-x (draw/pixel-center (m/dec right)) ;; vertical axis: rightmost column of its box
          line-y (draw/pixel-center y)             ;; horizontal axis: top row of its box
          measure-env (assoc env :canvas c) ;; target canvas: same rendering hint as layout
          rotation (label-rotation k measure-env)
          sizes (label-sizes k finalized measure-env)
          offsets (label-offsets k finalized measure-env)
          spans (label-spans k finalized measure-env sizes)
          drawn (drawn-label-indices k finalized sizes (label-inset measure-env) offsets spans)
          drawn-set (set drawn)
          next-gap (:next-panel-gap content)
          skipped (if next-gap
                    (let [own (label-intervals k finalized sizes (label-inset measure-env) offsets spans)
                          next-finalized (when (contains? content :next-panel)
                                           (some-> (get (scale/panel-scales-of measure-env {:panel (:next-panel content)}) k)
                                                   scale/display-scale))
                          [next-intervals next-drawn] (if next-finalized (drawn-labels k next-finalized measure-env) [own drawn])]
                      (crossing-labels own drawn next-intervals next-drawn (m/+ (if horizontal? w h) (double next-gap))))
                    #{})
          draw-label! (fn [text box] (c2d/with-isolated-canvas [isolated c]
                                       (draw/draw-content (tick-label text rotation) isolated box measure-env)))]
      (when (draw/visible-stroke? line-color line-width)
        (set-line-style! c line-color line-width)
        (if horizontal?
          (c2d/line c x line-y right line-y)
          (c2d/line c line-x y line-x (m/+ y h))))
      (when (draw/visible-stroke? tick-color tick-width)
        (set-line-style! c tick-color tick-width)
        (doseq [^double position positions
                :let [p (draw/pixel-center position)]]
          (if horizontal?
            (c2d/line c p y p (m/+ y tick-length))
            (c2d/line c right p (m/- right tick-length) p)))
        (doseq [tick (:minor-ticks finalized)
                :let [p (draw/pixel-center (m/+ offset (double (forward tick))))]]
          (if horizontal?
            (c2d/line c p y p (m/+ y minor-tick-length))
            (c2d/line c right p (m/- right minor-tick-length) p))))
      (doseq [[i ^double p text ^Vec2 size ^double off] (map vector (range) positions (:tick-labels finalized) sizes offsets)
              :when (and (contains? drawn-set i) (not (skipped i)))
              :let [lw (.x size) lh (.y size)]]
        (if horizontal?
          (draw-label! text {:x (m/- (m/+ p off) (m/* 0.5 lw)) :y (m/+ y room label-gap) :w lw :h lh})
          (draw-label! text {:x (m/- right room label-gap lw) :y (m/- p (m/* 0.5 lh)) :w lw :h lh}))))))
