(ns cljplot2.chart.text
  "Text: labels at positions - an annotation chart (see [[cljplot2.data.annotation]]: literal options or data through
  the chart's own `:mapping`, `:extent`).

  Literal: `:x`, `:y` (a position or a vector), `:text` (a value or a vector; a value converted with `str` unless a formula or runs value, see [[cljplot2.data.text]]), vectors of 1 or n
  items paired. `:x` / `:y` also take `[:panel f]` - fraction `f` (0..1) of the panel from its low-value end, not in
  the extents (a vector starting with `:panel` is always such a position); on an axis without a scale (no chart maps
  it, e.g. the cross axis of a dot plot) the fraction is of the panel's width / height (polar: its angular / radial
  extent). Data: `:x :y :label` in the chart's own `:mapping`, one label per row (rows with an invalid position
  skipped); ordinal aesthetics (`:color`) through the chart's own mapping colour the text and give legend keys.
  Positions are numbers, `java.time` values or levels.

  End labels (data mode): `:select` `:last` / `:first` keeps one label per group (aesthetics, `:group`) and label
  text (`:select-within :group` - per group only, whatever its label; only `:group` needs `:select`, a schema rule -
  the default `:group-and-label` is accepted without it) - the row with the largest / smallest x (numeric or temporal; ties - the later row; other x - the last / first
  row in data order). Without an `:anchor` / `:offset` it places the text past the point - `:last` `[:left :middle]`
  offset `[5 0]`, `:first` `[:right :middle]` `[-5 0]` (flipped plots: `[:center :bottom]` `[0 5]`, `[:center :top]`
  `[0 -5]`) - and keeps room for the labels on that side of x (`:padding-px`: the widest label - the tallest when
  flipped, `:rotation` included - and the offset).

  Options: `:anchor` - the point of the text box placed at the position, `[h v]` (`h` one of `:left :center :right`,
  `v` one of `:top :middle :bottom`; default `[:center :middle]`); `:offset [dx dy]` - pixels, `dy` up (default
  `[0 0]`); `:rotation` - degrees, clockwise (default 0; the anchor applies to the rotated text's bounding box), or `:radial` -
  in a polar plot each label along its angle, labels on the left half turned to read upright, anchored away from the
  centre unless `:anchor` is given (0 in cartesian plots);
  A mapped `:angle` (degrees, clockwise; `nil` or not a finite number - `:rotation`) turns each label on its own,
  over `:rotation` and `:radial`. `:anchor-frame` - `:box` (default: `:anchor` is a point of the rotated label's
  bounding box) or `:text` (a point of the unrotated text, the text turned about it - `[:left :middle]` starts every
  label at its position whatever its angle, as radial labels around a circle).
  `:font`, `:size`, `:font-style`, `:color`, `:inset` (pixels between the text and its box edge), `:background` (a color, alpha allowed, filling the
  text plus its inset - a label box, ggplot `geom_label`; the legend key keeps the background with the role's inset) - over the text role `[:text role]`
  of `:role` (default `:annotation`; the text chart's theme style; a theme `[:chart :text]` entry is rejected); `:extent`, `:data` /
  `:mapping`. Text stays upright when the plot is flipped; only the anchor position enters the extents.
  `:reserve-room true` keeps pixel room past the data on every side (`:padding-px` of x and y) for the labels at any
  angle: the largest label's diagonal plus its offset - labels around a circle stay inside the panel. `:reserve-room
  :anchor` keeps on each side only the room the labels reach past their positions, from `:anchor`, `:offset` and the
  largest label's size (e.g. `[:left :middle]` - the width on the high x side, half the height below and above;
  flipped plots exchange the axes); labels with a `:rotation` or a mapped `:angle` - as `true`.

  Crowded labels (builders use both): a mapped `:x-span` is each label's room along x - a length in x units from its
  numeric x (a negative span is an error, `{:key :mapping :in [:mapping :x-span]}`), its pixel length less the
  offset along x; a label running along x (unrotated - a `:rotation` and mapped `:angle` of 0 - in an unflipped
  cartesian plot) longer than its room shrinks down to its text role's `:min-size` (default `:annotation` 6 points;
  a role without one - not shrunk), its text kept whole (at the least size it may stay longer than its room) - see
  [[cljplot2.content.label/fitted-label]]. Other labels (rotated, a flipped or polar plot, a non-numeric x, a `nil`
  or non-finite span) aren't fitted.
  `:overlapping-labels :drop` leaves out each label whose box overlaps one kept before it - groups in order, then
  rows in data order, so the rows' order is the priority (the axes' rule, [[cljplot2.graphics.measure/kept-box-indices]];
  empty texts never overlap; needs `:anchor-frame :box`, a schema rule); `:keep` (default) draws them all.
  `:overlapping-labels :repel` (deterministic, in draw, by box overlap - also `:anchor-frame :box` only) moves such a
  label instead, in the same priority order: a label stays where it is when its box lies inside the panel and
  overlaps no label placed before it; else it takes the nearest free place of a 4 px grid of shifts within
  `:repel-distance` pixels (default 60; ties counterclockwise from the right) - inside the panel, off the labels
  placed before it and off the other labels' points (a 6 px square; points of the labels, not other charts' marks),
  else it is left out (as `:drop`); a label larger than the panel (no place inside it) stays where it is - see
  [[cljplot2.graphics.measure/repelled-placements]] (the candidate shifts are bounded by the panel, so a large
  distance costs no more than the panel's size). The boxes compared and kept inside the panel are the drawn label
  boxes - the text plus its `:inset` on every side, the area a `:background` fills ([[cljplot2.content.label/box-at]]);
  rotated labels by their rotated boxes' bounding boxes. Only the chart's own labels repel one another (labels of
  another text chart don't). It works in pixels, so flipped, polar and faceted plots alike (each panel on its own).
  `:label-leader true` gives a label moved more than 6 px from where it would sit a leader line from its point to the
  nearest point of its drawn frame's edge (the turned text frame of a rotated label, [[cljplot2.content.label/frame-corners]];
  none when the point lies in the frame; starting where it leaves labels placed over the point), drawn by
  [[cljplot2.content.label/draw-leader!]]; with leaders a place whose leader would cross a label placed before it, or
  whose box would cross a leader placed before it, isn't free - no leader runs through a label.
  `:repel-distance` and `:label-leader true` need `:overlapping-labels :repel` (schema rules; the default distance 60
  is accepted without it, as `:select-within`'s default).

  Text along a path: `:along-path true` lays each label text along the polyline of the positions of its group with
  that text, in data order (literal: the positions given with one text form one path; data: the rows of a group and
  label, as `:select` groups them), glyph by glyph turned to the path - [[cljplot2.content.label/draw-along-path!]]
  over [[cljplot2.graphics.text-path/draw!]]: the glyphs follow one another by arc length along the path shifted to
  the text's baseline, at `:path-position` (a fraction of that line's length from the path's first position, default
  0.5; another fraction needs `:along-path`, a schema rule) with `:anchor`'s `h` (`:left` - the text starts there, `:center`, `:right`
  - ends there) and `v` the side (`:middle` - on the path, `:bottom` - above it, `:top` - below it), `:offset [dx dy]`
  pixels along the path and up from it; read upright by the whole path's direction (a path ending left of its start
  is reversed, an exactly vertical one reads bottom to top); not drawn when longer than the path. The label's
  `:background` (inline or its text role's) fills a band under the glyphs, `:inset` past them. The path joins the
  positions' pixels with straight segments (polar plots too - give dense positions for a curve). Without boxes: no
  `:select`, `:rotation`, `:anchor-frame :text`, `:overlapping-labels` but `:keep` (schema rules); a mapped `:angle` /
  `:x-span` is ignored.

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[panel-position-schema]] - the schema of `[:panel f]`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.text :as text]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- numeric-rotation
  "Gives the `:rotation` option in degrees.

  Parameters:

  - `options` (map): the chart options; `:rotation` - degrees or `:radial` (default 0).

  Returns the rotation; `:radial` (polar only) counts as 0."
  [options]
  (let [r (data/option options :rotation 0)] (if (number? r) r 0)))

(defn- panel-position?
  "Tests whether a position is a panel position `[:panel f]`.

  Parameters:

  - `p`: a position.

  Returns a truthy value for a two-element vector starting with `:panel`."
  [p]
  (and (vector? p) (m/== 2 (count p)) (= :panel (first p))))

(def panel-position-schema
  "Schema of a panel position `[:panel f]`, `f` in `[0 1]` - a fraction of the panel from its low-value end."
  (schema/named-type ::panel-position #(and (panel-position? %) (number? (second %)) (m/<= 0.0 (double (second %)) 1.0))
                     "should be [:panel f], f in [0 1]"
                     {:cljplot2/type-name "panel position"
                      :cljplot2/type-meaning "A place in the panel rather than in the data: `[:panel f]`, a fraction `f` from 0 to 1 of the way from the low-value end, e.g. `[:panel 0.5]` - the middle. It doesn't widen the axis."}))

(def ^:private text-position
  "Schema of one text position: a data position or a panel position."
  [:or annotation/position panel-position-schema])

(def ^:private text-positions
  "Schema of the literal `:x` / `:y`: a position or a vector of 1 or n; a vector starting with `:panel` is always a panel position, never a vector of positions."
  [:multi {:dispatch #(and (vector? %) (= :panel (first %)))}
   [true panel-position-schema]
   [false (annotation/positions-schema text-position)]])

(def ^:private style-keys
  "Text style keys the chart takes as options, over its text role."
  [:font :size :font-style :color :inset :background :dollar-math])

(defn- text-role
  "Gives the theme text role of the chart's labels.

  Parameters:

  - `options` (map): the chart options.

  Returns `:role`, else `:annotation`."
  [options]
  (data/option options :role :annotation))

(def schemas
  "Schemas of the text chart: `:options`, `:mapping` (merged); the style is the text role `[:text role]` of `:role` (default `:annotation`)."
  {:options (schema/with-rules
             (-> [:map {:closed true}]
                 (into (filter #(contains? (set style-keys) (first %)) theme/text-style-overrides))
                 (into data/common-options)
                 (into annotation/options)
                 (conj (schema/optional :role {:cljplot2/section :labels :description "The theme text role the labels take their style from, e.g. a builder's role or one your theme adds; the inline style options win over it."
                                               :default :annotation :cljplot2/example :title :cljplot2/example-context {:x 5 :y 5 :text "label"}}
                                        keyword?)
                       (schema/nilable :x {:cljplot2/section :labels :description "The x of each literal label. Literal labels need `:x`, `:y` and `:text`, and replace the chart's mapping."
                                              :cljplot2/default-doc "None; the labels come from the mapping."
                                              :cljplot2/form-meanings {::panel-position "A place across the panel, whatever the data." ::annotation/position "One x for every label." :vector "An x per label, paired in order with `:y` and `:text`."}
                                              :cljplot2/example [:panel 0.2] :cljplot2/example-context {:y 5 :text "label"}}
                                       text-positions)
                       (schema/nilable :y {:cljplot2/section :labels :description "The y of each literal label. Literal labels need `:x`, `:y` and `:text`, and replace the chart's mapping."
                                              :cljplot2/default-doc "None; the labels come from the mapping."
                                              :cljplot2/form-meanings {::panel-position "A place across the panel, whatever the data." ::annotation/position "One y for every label." :vector "A y per label, paired in order with `:x` and `:text`."}
                                              :cljplot2/example [:panel 0.9] :cljplot2/example-context {:x 5 :text "label"}}
                                       text-positions)
                       (schema/nilable :text {:cljplot2/section :labels :description "The text of the literal labels: one value for all, or a vector paired in order with `:x` and `:y`. A value becomes a string; `$...$` in it and `plot/math` values draw as formulas, unless `:dollar-math false` keeps dollars as typed."
                                                 :cljplot2/default-doc "None; the labels come from the mapping." :cljplot2/example "peak" :cljplot2/example-context {:x 5 :y 5}}
                                       any?)
                       (schema/optional :anchor {:cljplot2/section :placement :description "The point of each label's box placed at its position, e.g. `[:left :top]` puts the box's top left corner there."
                                                 :cljplot2/default-doc "`[:center :middle]`; with `:select :last` `[:left :middle]`, with `:first` `[:right :middle]`, past the line's end; with `:rotation :radial`, the side facing the centre."
                                                 :cljplot2/example [:left :bottom] :cljplot2/example-context {:x 5 :y 5 :text "label"}}
                                        schema/align)
                       (schema/optional :offset {:cljplot2/section :placement :description "Shifts each label by `[dx dy]` pixels, `dy` up."
                                                 :cljplot2/default-doc "`[0 0]`; with `:select`, 5 pixels past the line's end."
                                                 :cljplot2/example [10 10] :cljplot2/example-context {:x 5 :y 5 :text "label"}}
                                        [:tuple schema/finite schema/finite])
                       (schema/optional :rotation {:cljplot2/section :placement :description "Turns the labels; a mapped `:angle` wins."
                                                   :default 0
                                                   :cljplot2/value-meanings {:radial "In a polar plot, each label along its angle, turned to read upright; no turn in a cartesian plot."}
                                                   :cljplot2/form-meanings {::schema/finite "Degrees, clockwise."}
                                                   :cljplot2/example -30 :cljplot2/example-context {:x 5 :y 5 :text "label"}}
                                        [:or schema/finite [:= :radial]])
                       (schema/optional :select {:cljplot2/section :labels :description "Keeps one label per group (see `:select-within`), for labels at the ends of lines."
                                                 :cljplot2/default-doc "A label for every row."
                                                 :cljplot2/value-meanings {:last "The row with the largest x." :first "The row with the smallest x."}
                                                 :cljplot2/example-context {:data [{:x 1 :y 1 :g "a"} {:x 3 :y 2 :g "a"}] :mapping {:x :x :y :y :label :g}}}
                                        [:enum :last :first])
                       (schema/nilable :select-within {:cljplot2/section :labels :description "What `:select` keeps one label in."
                                                        :default :group-and-label
                                                        :cljplot2/value-meanings {:group-and-label "Each group and label text." :group "Each group, whatever its labels."}
                                                        :cljplot2/example-context {:select :last :data [{:x 1 :y 1 :g "a" :l "early"} {:x 3 :y 2 :g "a" :l "late"}]
                                                                                   :mapping {:x :x :y :y :group :g :label :l}}}
                                        [:enum :group-and-label :group])
                       (schema/optional :anchor-frame {:cljplot2/section :placement :description "What `:anchor` points into on a turned label."
                                                       :default :box
                                                       :cljplot2/value-meanings {:box "The bounding box of the turned label." :text "The text before turning; the text then turns about that point."}
                                                       :cljplot2/example-context {:x 5 :y 5 :text "label" :rotation -45 :anchor [:left :top]}}
                                        [:enum :box :text])
                       (schema/optional :reserve-room {:cljplot2/section :placement :description "Keeps room past the data for the labels, so labels at the data's edge stay inside the panel."
                                                       :default false
                                                       :cljplot2/value-meanings {true "The largest label's diagonal on every side, for labels at any angle." false "No room." :anchor "Only where the anchored labels reach past their positions."}
                                                       :cljplot2/example-context {:x 5 :y 5 :text "label"}}
                                        [:or schema/bool [:= :anchor]])
                       (schema/optional :overlapping-labels {:cljplot2/section :crowding :description "What the chart does with a label that overlaps one placed before it. It places labels group by group, each group's rows in data order, so earlier labels win."
                                                             :default :keep
                                                             :cljplot2/value-meanings {:keep "Draws it anyway." :drop "Leaves it out." :repel "Moves it to the nearest free place within `:repel-distance`, else leaves it out."}
                                                             :cljplot2/example-context {:x 5 :y 5 :text ["first" "second"]}}
                                        [:enum :keep :drop :repel])
                       (schema/nilable :repel-distance {:cljplot2/section :crowding :description "The farthest `:overlapping-labels :repel` moves a label, in pixels."
                                                        :default 60 :cljplot2/example 4 :cljplot2/example-context {:x 5 :y 5 :text ["first" "second"] :overlapping-labels :repel}}
                                       schema/finite-pos)
                       (schema/nilable :label-leader {:cljplot2/section :crowding :description "Draws a line in the label's colour from a label's point to the label when `:overlapping-labels :repel` moved it more than 6 pixels."
                                                      :default false
                                                      :cljplot2/value-meanings {true "Draws the lines." false "No lines."}
                                                      :cljplot2/example-context {:x 5 :y [5 5.1] :text ["first" "second"] :anchor [:left :middle] :offset [4 0]
                                                                                 :overlapping-labels :repel}}
                                       schema/bool)
                       (schema/nilable :along-path {:cljplot2/section :path :description "Lays each label's text along a path through its group's positions with that text, in data order. Each letter turns with the path."
                                                    :default false
                                                    :cljplot2/value-meanings {true "Along the path." false "Each label at its own position."}
                                                    :cljplot2/example-context {:x [1 3 5 7 9] :y [2 4 3 5 4] :text "along a path"}}
                                       schema/bool)
                       (schema/nilable :path-position {:cljplot2/section :path :description "Where on its path a label sits, as a fraction of the path's length from its first position. `:anchor` says whether the text starts, centres or ends there."
                                                       :default 0.5 :cljplot2/example 0.2
                                                       :cljplot2/example-context {:along-path true :x [1 3 5 7 9] :y [2 4 3 5 4] :text "along"}}
                                       schema/fraction)))
             [(fn [{:keys [select select-within]}]
                (when (and (= :group select-within) (nil? select))
                  ":select-within :group needs :select (it says what :select keeps one label in)"))
              [:select-within]]
             [(fn [{:keys [overlapping-labels anchor-frame]}]
                (when (and (#{:drop :repel} overlapping-labels) (= :text anchor-frame))
                  (str ":overlapping-labels " overlapping-labels " needs :anchor-frame :box (it compares the labels' boxes)")))
              [:overlapping-labels]]
             ;; options working only with another: an other than default value without it is an error (as
             ;; :select-within)
             [(fn [{:keys [overlapping-labels repel-distance]}]
                (when (and (some? repel-distance) (not (m/== 60.0 (double repel-distance))) (not= :repel overlapping-labels))
                  ":repel-distance needs :overlapping-labels :repel (it limits how far a label moves)"))
              [:repel-distance]]
             [(fn [{:keys [overlapping-labels label-leader]}]
                (when (and label-leader (not= :repel overlapping-labels))
                  ":label-leader needs :overlapping-labels :repel (it joins a moved label to its point)"))
              [:label-leader]]
             [(fn [{:keys [along-path path-position]}]
                (when (and (some? path-position) (not (m/== 0.5 (double path-position))) (not along-path))
                  ":path-position needs :along-path true (it places the text on its path)"))
              [:path-position]]
             [(fn [{:keys [along-path select overlapping-labels anchor-frame rotation]}]
                (when along-path
                  (cond select ":along-path lays every label along its path: no :select"
                        (#{:drop :repel} overlapping-labels) ":along-path labels aren't boxes: :overlapping-labels :keep only"
                        (= :text anchor-frame) ":along-path turns each glyph to the path: no :anchor-frame :text"
                        (and (some? rotation) (not (and (number? rotation) (m/zero? (double rotation)))))
                        ":along-path turns each glyph to the path: no :rotation")))
              [:along-path]])
   :mapping [:map
             (schema/nilable :x {:description "The x of each row's label."} schema/accessor)
             (schema/nilable :y {:description "The y of each row's label."} schema/accessor)
             (schema/nilable :label {:description "The text of each row's label; a value becomes a string. `$...$` in it and `plot/math` values draw as formulas, unless `:dollar-math false` keeps dollars as typed."} schema/accessor)
             (schema/nilable :angle {:description "Turns each row's label by degrees, clockwise; a `nil` or non-finite value takes `:rotation`."} schema/accessor)
             (schema/nilable :x-span {:description "Each label's room along x, in x units from its x: a longer label shrinks, down to its text role's `:min-size`. The chart fits only unrotated labels in an unflipped cartesian plot, and not for a `nil` span."}
                             schema/accessor)
             (schema/nilable :color {:description "Splits the rows into groups, each with its own text colour."} schema/accessor)
             (schema/nilable :group {:description "Splits the rows into groups that look the same; `:select` keeps labels per group."} schema/accessor)]})

(defn- one?
  "Tests whether a literal value is one item rather than a vector of items.

  Parameters:

  - `v`: the literal value.

  Returns a truthy value for anything but a vector, and for a panel position."
  [v]
  (not (and (vector? v) (not (panel-position? v)))))

(defn- label-positions
  "Gives the literal labels.

  Parameters:

  - `xs`, `ys`, `ts` (vectors of 1 or n): the positions and texts.

  Returns `[{:x :y :text} ..]`, the vectors paired (see [[cljplot2.data.annotation/broadcast]]).

  Throws as [[cljplot2.data.annotation/broadcast]] for vectors of different lengths."
  [xs ys ts]
  (mapv (fn [[x y t]] {:x x :y y :text t}) (annotation/broadcast [:x :y :text] [xs ys ts])))

(defn- selected
  "Gives the labels kept by `:select`.

  Parameters:

  - `labels` (vector of maps): labels with valid positions, in data order.
  - `select`: `:last`, `:first` or `nil` - all.
  - `within`: `:group-and-label` (`nil`) - one label per text, `:group` - one label of all.

  Returns per label text (`:group` - of all labels) the label with the largest (`:last`) / smallest (`:first`) x key (see [[cljplot2.data.rows/x-key]]; ties - the later label), or the last / first in data order when some x has no key; `labels` for `nil`."
  [labels select within]
  (if-not select
    labels
    (let [by-text (cond (not= :group within) (vals (group-by :text labels))
                        (seq labels) [labels])
          later? (if (= :last select) m/>= m/<=)
          pick (fn [ls]
                 (let [ks (mapv #(data/x-key (:x %)) ls)]
                   (if (every? some? ks)
                     (ls (reduce (fn [best i] (if (later? (double (ks i)) (double (ks best))) i best)) 0 (range (count ls))))
                     (if (= :last select) (peek ls) (first ls)))))]
      (mapv pick by-text))))

(defn- select-placement
  "Gives the `:anchor` / `:offset` defaults of end labels.

  Parameters:

  - `select`: `:last` or `:first`.
  - `flip`: is the plot flipped (labels placed along the vertical)?

  Returns `{:anchor [h v] :offset [dx dy]}` - just past the point, 5 pixels away."
  [select flip]
  (case select
    :last (if flip {:anchor [:center :bottom] :offset [0 5]} {:anchor [:left :middle] :offset [5 0]})
    :first (if flip {:anchor [:center :top] :offset [0 -5]} {:anchor [:right :middle] :offset [-5 0]})))

(defn- label-room
  "Gives the pixel room end labels need past the data.

  Parameters:

  - `texts` (sequence): the selected labels' texts.
  - `options` (map): style keys, `:role`, `:rotation`, `:select`, `:offset`.
  - `env` (map): `:theme`, `:flip`, `:hint`.

  Returns `:padding-px` `{:x [lo hi]}`: the widest text (the tallest when flipped) plus the offset along x, on the high side for `:last`, the low side for `:first`; `nil` without texts."
  [texts options {:keys [theme flip hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation (numeric-rotation options)} (select-keys options style-keys))
             along (v/mx (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text t) {:canvas c :theme theme})]
                                        (if flip (.y size) (.x size))))
                              texts))
             [dx dy] (:offset options)
             room (m/+ along (m/abs (double (if flip dy dx))))]
         {:x (if (= :last (:select options)) [0.0 room] [room 0.0])})))))

(defn- reserved-room
  "Gives the pixel room labels need at any angle.

  Parameters:

  - `texts` (sequence): the labels' texts.
  - `options` (map): style keys, `:role`, `:offset`.
  - `env` (map): `:theme`, `:hint`.

  Returns `:padding-px` `{:x [r r] :y [r r]}`, `r` - the largest text's diagonal (unrotated) plus the larger offset component; `nil` without texts."
  [texts options {:keys [theme hint]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation 0} (select-keys options style-keys))
             diagonal (v/mx (map (fn [t] (let [^Vec2 size (measure/natural-size (assoc spec :text t) {:canvas c :theme theme})]
                                           (m/hypot (.x size) (.y size))))
                                 texts))
             [dx dy] (data/option options :offset [0 0])
             r (m/+ diagonal (m/max (m/abs (double dx)) (m/abs (double dy))))]
         {:x [r r] :y [r r]})))))

(defn- anchored-room
  "Gives the pixel room labels reach past their positions as anchored.

  Parameters:

  - `texts` (sequence): the labels' texts.
  - `options` (map): style keys, `:role`, `:anchor`, `:offset` (no rotation).
  - `env` (map): `:theme`, `:hint`, `:flip` - x room on the vertical scale and y room on the horizontal one.

  Returns `:padding-px` `{:x [lo hi] :y [lo hi]}`: per side the part of the largest text box (width and height measured separately) past the position; `nil` without texts."
  [texts options {:keys [theme hint flip]}]
  (when (seq texts)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [spec (merge {:kind :label :role (text-role options) :rotation 0} (select-keys options style-keys))
             sizes (map (fn [t] (measure/natural-size (assoc spec :text t) {:canvas c :theme theme})) texts)
             w (v/mx (map (fn [^Vec2 size] (.x size)) sizes))
             h (v/mx (map (fn [^Vec2 size] (.y size)) sizes))
             [ha va] (data/option options :anchor [:center :middle])
             [dx dy] (data/option options :offset [0 0])
             x0 (m/- (double dx) (case ha :left 0.0 :center (m/* 0.5 w) :right w))
             y0 (m/- (double dy) (case va :bottom 0.0 :middle (m/* 0.5 h) :top h))
             horizontal [(m/max 0.0 (m/- x0)) (m/max 0.0 (m/+ x0 w))]
             vertical [(m/max 0.0 (m/- y0)) (m/max 0.0 (m/+ y0 h))]]
         (if flip {:x vertical :y horizontal} {:x horizontal :y vertical}))))))

(defn- with-reserved-room
  "Adds the room `:reserve-room` asks for to chart stats.

  Parameters:

  - `stats` (map): the chart's stats.
  - `options` (map): the chart options (`:reserve-room`, `:rotation`, ...).
  - `mapping` (map or `nil`): the chart's mapping (`:angle`).
  - `env` (map): the compute environment.

  Returns `stats` with [[reserved-room]] (`:reserve-room :anchor` without a rotation or a mapped angle - [[anchored-room]]) for the texts of its labels merged into its `:padding-px` (per side the larger); `stats` unchanged without `:reserve-room`."
  [stats options mapping env]
  (if-not (:reserve-room options)
    stats
    (let [texts (distinct (map :text (mapcat (fn [panel] (mapcat :labels (:groups panel))) (vals (:data stats)))))
          anchored? (and (= :anchor (:reserve-room options)) (nil? (:angle mapping))
                         (contains? #{nil 0 0.0} (:rotation options)))
          room (if anchored? (anchored-room texts options env) (reserved-room texts options env))]
      (cond-> stats
        room (update :padding-px #(merge-with (fn [a b] (mapv m/max a b)) % room))))))

(defn compute
  "Computes the labels of the chart.

  Parameters:

  - `chart` (map): the chart value (`:options` - literal `:x`, `:y`, `:text`, `:select`, ...).
  - `chart-split` (map): `:groups`, `:mapping` (the chart's own: `:x`, `:y`, `:label`, `:angle`, aesthetics).
  - `env` (map): the compute environment (`:flip`, `:theme`, `:hint` ...).

  Returns literal - `{:groups [{:labels [{:x :y :text}]}] :options o}` in every panel, extents of the positions; data - per panel the labels of each group (with `:aesthetics`), with `:select` one per group and text (`:select-within :group` - per group), the `:anchor` / `:offset` defaults in `o` and `:padding-px` room; `:reserve-room` room (see [[with-reserved-room]]).

  Throws `ex-info {:key :select}` for `:select` with literal labels, `{:key k}` for a missing literal `:x`, `:y` or `:text`, `{:key :mapping}` without a mapped `:x :y :label`, `{:key :text}` for a mapped label with formulas along a path."
  [{:keys [options]} {:keys [mapping] :as chart-split} env]
  (if (annotation/literal? options [:x :y :text])
    (let [_ (when (:select options)
              (schema/fail! "Text :select picks labels from data rows (a :mapping); literal labels are placed as given."
                            {:key :select :value (:select options)}))
          many (fn [k] (annotation/as-vector (get options k) one?))
          _ (doseq [k [:x :y :text]
                    :when (empty? (many k))]
              (schema/fail! (str "Text " k " is required (literal options).") {:key k :value nil}))
          labels (label-positions (many :x) (many :y) (many :text))]
      (with-reserved-room (annotation/literal-stats {:groups [{:labels labels}] :options options} options
                                                    {:x (map :x labels) :y (map :y labels)})
        options nil env))
    (let [_ (annotation/require-mapping! "Text" mapping [[:x :y :label]])
          [x-of y-of t-of] (map #(data/row-accessor % (get mapping %)) [:x :y :label])
          angle-of (when (some? (:angle mapping)) (data/row-accessor :angle (:angle mapping)))
          span-of (when (some? (:x-span mapping)) (data/row-accessor :x-span (:x-span mapping)))
          checked-span (fn [r]
                         (let [span (span-of r)]
                           (when (and (number? span) (m/neg? (double span)))
                             (schema/fail! "Text :x-span is a room along x: it can't be negative."
                                           {:key :mapping :in [:mapping :x-span] :value span}))
                           span))
          select (:select options)
          placed-options (cond->> options select (merge (select-placement select (:flip env))))
          labels-of (fn [rows] (selected (vec (for [r rows
                                                    :let [x (x-of r) y (y-of r)]
                                                    :when (and (annotation/position? x) (annotation/position? y))]
                                                (cond-> {:x x :y y :text (t-of r)}
                                                  angle-of (assoc :angle (angle-of r))
                                                  span-of (assoc :x-span (checked-span r)))))
                                         select (:select-within options)))
          stats (annotation/data-stats chart-split placed-options env
                                       (fn [gs]
                                         (let [groups (mapv (fn [g] {:labels (labels-of (:rows g)) :aesthetics (:aesthetics g)}) gs)
                                               ls (mapcat :labels groups)]
                                           [{:groups groups :options placed-options} {:x (map :x ls) :y (map :y ls)}]))
                                       {:x #(not (annotation/position? (x-of %))) :y #(not (annotation/position? (y-of %)))})]
      (when (:along-path options)
        (doseq [t (distinct (map :text (mapcat :labels (mapcat :groups (vals (:data stats))))))
                :when (label/formulas? (:theme env) {:kind :label :role (text-role options) :dollar-math (:dollar-math options) :text t})]
          (schema/fail! "A text along a path can't hold LaTeX formulas (:dollar-math false keeps dollars literal)."
                        {:key :text :value t})))
      (-> (cond-> stats
            select (assoc :padding-px (label-room (distinct (map :text (mapcat :labels (mapcat :groups (vals (:data stats))))))
                                                  placed-options env)))
          (with-reserved-room placed-options mapping env)))))

(defn- base-style
  "Gives the base text style of the labels, the group's `:color` drawn over it.

  Parameters:

  - `options` (map): the chart options (style keys, `:role`).
  - `theme` (map): the merged theme.

  Returns the colour of the chart's text role with the style keys of `options` over it."
  [options theme]
  (merge {:color (get-in theme [:text (text-role options) :color])} (select-keys options style-keys)))

(defn- texts-in-order
  "Gives a group's labels by text, in the order the texts first appear.

  Parameters:

  - `labels` (sequence of maps): labels with `:text`.

  Returns a lazy sequence of `[text labels]`, the texts as labels take them (formula and runs values kept, others
  converted with `str`, see [[cljplot2.data.text/label-text]]), each text's labels in data order."
  [labels]
  (let [by-text (group-by #(text/label-text (:text %)) labels)]
    (map (fn [t] [t (by-text t)]) (distinct (map #(text/label-text (:text %)) labels)))))

(defn- anchor-point-fn
  "Gives the function placing labels at their positions' pixels in one panel.

  Parameters:

  - `options` (map): the chart options (`:rotation`, `:anchor`).
  - `scales` (map): the finalized scales (`:flip`, a polar coord's geometry).
  - `box` (map): the panel box - `:w`, `:h` (the range of `[:panel f]` on an axis without a scale).

  Returns a function of a label `{:x :y}` giving `[px py rotation anchor]` - the pixel of its position (no
  `:offset`; NaN when unplaceable), the rotation (`:radial` along its angle in a polar plot, else `:rotation`) and
  the anchor (`:radial` labels anchored away from the centre unless `:anchor` is given)."
  [options scales {:keys [^double w ^double h]}]
  (let [geometry (scale/coord-geometry scales)
        radial? (and geometry (= :radial (:rotation options)))
        [hk vk] (if (scale/flipped? scales) [:y :x] [:x :y])
        ;; ranges of an axis without a scale, from its low-value end
        [h-range v-range] (if geometry
                            [(:theta geometry) [(:inner-radius geometry) (:radius geometry)]]
                            [[0.0 w] [h 0.0]])
        h-pixel (annotation/axis-pixel-fn (get scales hk) h-range)
        v-pixel (annotation/axis-pixel-fn (get scales vk) v-range)
        rotation (numeric-rotation options)
        anchor (data/option options :anchor [:center :middle])]
    (fn [label]
      (let [range-h (double (h-pixel (get label hk)))
            range-v (double (v-pixel (get label vk)))]
        (if geometry
          (let [^Vec2 p (coord/to-pixel geometry range-h range-v)]
            [(.x p) (.y p)
             (if radial? (coord/radial-rotation range-h) rotation)
             (if radial? (data/option options :anchor (coord/outward-anchor range-h)) anchor)])
          [range-h range-v rotation anchor])))))

(defn- draw-along-paths!
  "Draws the labels of `:along-path` - each text along the polyline of its group's positions with that text.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `groups` (sequence of maps): the panel's groups - `:labels`, `:aesthetics`.
  - `options` (map): the chart options (`:path-position`, `:anchor`, `:offset`, style keys, `:role`).
  - `anchor-point` (function): a label -> `[px py ..]`, its pixel (see [[anchor-point-fn]]).
  - `scales`, `env`: the finalized scales, the drawing environment (`:theme`).

  Returns `nil`; positions without a pixel are left out of a path, a text without room on its path isn't drawn (see
  [[cljplot2.content.label/draw-along-path!]])."
  [canvas groups options anchor-point scales env]
  (let [placement {:position (data/option options :path-position 0.5)
                   :anchor (data/option options :anchor [:center :middle])
                   :offset (data/option options :offset [0 0])}
        pixel (fn [l] (let [[^double px ^double py] (anchor-point l)]
                        (when (and (m/valid-double? px) (m/valid-double? py)) [px py])))]
    (doseq [{:keys [labels aesthetics]} groups
            :let [st (group/style (base-style options (:theme env)) aesthetics scales (:theme env))]
            :when st
            [text ls] (texts-in-order labels)]
      (label/draw-along-path! canvas (merge {:kind :label :role (text-role options) :text text} st)
                              (keep pixel ls) placement env))))

(defn- placed-labels
  "Gives the boxed labels of one panel, placed at their pixels.

  Parameters:

  - `measuring`: a canvas in graphics context, measuring.
  - `groups` (sequence of maps): the panel's groups - `:labels`, `:aesthetics`.
  - `options` (map): the chart options.
  - `anchor-point` (function): see [[anchor-point-fn]].
  - `scales`, `env`: the finalized scales, the drawing environment (`:theme`).

  Returns a vector of `{:content :anchor :x :y :point}` in groups' then data order - the label content (turned by a
  mapped `:angle`, else the placed rotation; fitted to a mapped `:x-span`'s room along x, see
  [[cljplot2.content.label/fitted-label]]), its anchor, its pixel with `:offset` and `:point` - the pixel without
  it; unplaceable labels left out."
  [measuring groups options anchor-point scales env]
  (let [[^double dx ^double dy] (data/option options :offset [0 0])
        flipped? (scale/flipped? scales)
        polar? (some? (scale/coord-geometry scales))
        x-pixel (annotation/axis-pixel-fn (:x scales))
        ;; pixels of a label's room along x: unflipped cartesian, a numeric x and a finite :x-span; the label must
        ;; run along x (checked with its rotation)
        room-px (fn [x x-span]
                  (when (and (not polar?) (not flipped?) (number? x) (schema/finite? x-span))
                    (m/- (m/abs (m/- (double (x-pixel (m/+ (double x) (double x-span)))) (double (x-pixel x))))
                         (m/abs dx))))]
    (vec (for [{:keys [labels aesthetics]} groups
               :let [st (group/style (base-style options (:theme env)) aesthetics scales (:theme env))]
               :when st
               {:keys [x text angle x-span] :as l} labels
               :let [[^double px ^double py placed-rotation anchor] (anchor-point l)
                     rotation (if (schema/finite? angle) angle placed-rotation)]
               :when (and (m/valid-double? px) (m/valid-double? py))
               :let [content (merge {:kind :label :role (text-role options) :text text :rotation rotation} st)
                     room (when (= 0 (label/quarter-turn rotation)) (room-px x x-span))]]
           {:content (if room
                       (label/fitted-label content {:length (m/max 0.0 (double room)) :cut false} (assoc env :canvas measuring))
                       content)
            :anchor anchor :x (m/+ px dx) :y (m/- py dy) :point [px py]}))))

(defn- repelled
  "Gives the placed labels moved by `:overlapping-labels :repel`.

  Parameters:

  - `measuring`: a canvas in graphics context, measuring.
  - `placed` (vector of maps): see [[placed-labels]], in priority order.
  - `boxes` (vector of maps): each label's pixel box.
  - `area` (map): `:w`, `:h` - the panel size, `:distance` - `:repel-distance`, `:leaders` - `:label-leader`.
  - `env` (map): the drawing environment (`:theme`).

  Returns a lazy sequence of the labels with a free position (see
  [[cljplot2.graphics.measure/repelled-placements]]) moved there, with their `:leader` (`nil` - none)."
  [measuring placed boxes area env]
  (let [frames (when (:leaders area)
                 (mapv (fn [{:keys [content anchor x y]}] (label/frame-corners measuring content anchor x y env)) placed))]
    (keep (fn [[{:keys [^double x ^double y] :as l} placement]]
            (when-let [{[^double sx ^double sy] :shift leader :leader} placement]
              (assoc l :x (m/+ x sx) :y (m/+ y sy) :leader leader)))
          (map vector placed (measure/repelled-placements boxes (mapv :point placed) (assoc area :frames frames))))))

(defn- draw-boxed!
  "Draws the labels of one panel as label boxes at their positions, crowded ones kept, dropped or repelled.

  Parameters:

  - `canvas`: the canvas.
  - `groups` (sequence of maps): the panel's groups.
  - `options` (map): the chart options (`:overlapping-labels`, `:repel-distance`, `:label-leader`, `:anchor-frame` ...).
  - `anchor-point` (function): see [[anchor-point-fn]].
  - `box` (map): the panel box `:w`, `:h`.
  - `scales`, `env`: the finalized scales, the drawing environment (`:theme`).

  Returns `nil`."
  [canvas groups options anchor-point {:keys [^double w ^double h]} scales env]
  (let [draw! (if (= :text (:anchor-frame options)) label/draw-at-frame! label/draw-at!)
        crowding (data/option options :overlapping-labels :keep)]
    (c2d/with-isolated-canvas [measuring canvas]
      (let [placed (placed-labels measuring groups options anchor-point scales env)
            boxes (when-not (= :keep crowding)
                    (mapv (fn [{:keys [content anchor x y]}]
                            (if (= "" (:text content))
                              {:x x :y y :w 0.0 :h 0.0}
                              (label/box-at measuring content anchor x y env)))
                          placed))
            kept (case crowding
                   :keep placed
                   :drop (mapv placed (measure/kept-box-indices boxes))
                   :repel (vec (repelled measuring placed boxes
                                         {:w w :h h :distance (double (data/option options :repel-distance 60))
                                          :leaders (data/option options :label-leader false)}
                                         env)))]
        (doseq [{:keys [content leader]} kept :when leader]
          (label/draw-leader! canvas content leader env))
        (doseq [{:keys [content anchor x y]} kept]
          (draw! canvas content anchor x y env))))))

(defn draw
  "Draws the labels of one panel.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:groups` (labels per group) and `:options`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box (`:w`, `:h` - the range of `[:panel f]` on an axis without a scale).
  - `scales` (map): the finalized scales.
  - `env` (map): the drawing environment (`:theme`).

  Both modes place labels at their positions' pixels ([[anchor-point-fn]]); unplaceable positions are skipped.
  Boxed labels ([[draw-boxed!]]) are drawn through [[cljplot2.content.label/draw-at!]] (`:anchor-frame :text` -
  [[cljplot2.content.label/draw-at-frame!]]) past their `:offset`, turned by a mapped `:angle`, else `:rotation`; a
  label with a mapped `:x-span` running along x is fitted to that room (see
  [[cljplot2.content.label/fitted-label]], without cutting); with `:overlapping-labels :drop` a label whose box
  overlaps one kept before it is left out (see [[cljplot2.graphics.measure/kept-box-indices]]; empty texts never
  overlap); `:repel` moves it (see [[cljplot2.graphics.measure/repelled-placements]]), with `:label-leader` a leader
  line from its point (see [[cljplot2.content.label/draw-leader!]]). `:along-path` lays each text along its
  positions' polyline instead ([[draw-along-paths!]]). `[:panel f]` works also on an axis without a scale (the
  panel's width / height; polar - its angular / radial extent)."
  [_chart {:keys [groups options]} canvas box scales env]
  (let [anchor-point (anchor-point-fn options scales box)]
    (if (:along-path options)
      (draw-along-paths! canvas groups options anchor-point scales env)
      (draw-boxed! canvas groups options anchor-point box scales env))))

(defn legend
  "Draws the key glyph: the letter `a` in the text style (the role's inset, so it fits the key), centred in the key box.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `:x`, `:y`, `:w`, `:h`.
  - `env` (map): the drawing environment (`:theme`)."
  [{:keys [options]} attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (merge (dissoc (base-style options (:theme env)) :inset) (select-keys attributes [:color]))]
    (label/draw-at! canvas (merge {:kind :label :role (text-role options) :text "a"} st) [:center :middle]
                    (m/+ x (m/* 0.5 w)) (m/+ y (m/* 0.5 h)) env)))
