(ns cljplot2.graphics.shape
  "Point shapes (old cljplot set), drawn centered at a position with a size (bounding box side, pixels).

  Built-in shapes are characters (see [[shapes]]):

  * `O` / `o` - filled / outlined circle
  * `S` / `s` - filled / outlined square
  * `A` `V` and the braces - filled triangle pointing up / down / left / right
  * the caret, `v` and the angle brackets - outlined triangle pointing up / down / left / right
  * the hyphen, the bar, the backslash and the slash - horizontal, vertical and diagonal strokes
  * `+` - plus, `x` - cross

  Filled shapes use the fill color and an optional outline; outlined shapes and strokes use the line color.

  A shape may also be a map (see [[shape-problem]]), fitted into the size square (aspect kept, centred):

  * `{:path d}` - SVG path data (e.g. `M0 0 L10 0 L5 8 Z`), filled with the color, outlined like a filled shape
  * `{:text t}` - a text outline (e.g. an emoji) filled with the color; `:font` (default [[*text-font*]] - the plot
    theme's annotation font while a plot draws), characters it lacks from the bundled monochrome Noto Emoji (see
    [[cljplot2.graphics.font]])
  * `{:svg source}` - an SVG file path, classpath resource or URL (string, `java.io.File`, `java.net.URL`), in its own
    colors; transcoded once per source and pixel size (see [[svg-image]])
  * `{:image img}` - a `BufferedImage` or clojure2d image, in its own colors

  Functions: [[shape-problem]], [[shape?]], [[draw!]], [[draw-key!]], [[styled-key!]], [[svg-image]]."
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.cache :as cache]
            [cljplot2.error :as error]
            [cljplot2.graphics.font :as font])
  (:import [fastmath.vector Vec2 Vec3]
           [java.awt Shape]
           [java.awt.image BufferedImage]
           [java.io File]
           [java.net URL]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def filled-shapes
  "Built-in shapes filled with the fill colour (and outlined)."
  #{\O \S \A \V \{ \}})

(def line-shapes
  "Built-in shapes stroked with the line colour."
  #{\o \s \^ \v \< \> \- \| \\ \/ \+ \x})

(def shapes
  "Every built-in shape character."
  (into filled-shapes line-shapes))

(def shape-kinds
  "Keys of a shape map, one per map: SVG path data, a text, an SVG source, an image."
  #{:path :text :svg :image})

(defn- unit-shape
  "Gives a shape moved and scaled so its bounding box is centred at the origin with the longer side 1.

  Parameters:

  - `shape` (java.awt `Shape`): the shape.

  Returns the unit shape; `nil` for an empty shape."
  ^Shape [^Shape shape]
  (let [b (.getBounds2D shape)
        side (m/max (.getWidth b) (.getHeight b))
        rside (m// side)]
    (when (m/pos? side)
      (sh/transform-shape shape [[:scale rside] [:translate (m/- (.getCenterX b)) (m/- (.getCenterY b))]]))))

(defn- parse-path
  "Gives the java.awt shape of SVG path data (non-zero winding).

  Parameters:

  - `d` (string): the path data.

  Returns the shape; `nil` when the data doesn't parse."
  ^Shape [d]
  (try (sh/svg-path d)
       (catch Exception _ nil)))

(def ^{:private true :const true :tag 'double} text-font-size
  "Point size text outlines are built at (scaled to the shape size when drawn)."
  100.0)

(defn- text-outline
  "Gives the java.awt outline of a text, at [[text-font-size]] points.

  Parameters:

  - `text` (string): the text.
  - `font`: a font name or `java.awt.Font` (see [[cljplot2.graphics.font/resolve-font]]); characters it lacks come from the bundled emoji font.

  Returns the outline shape (empty for an empty text)."
  ^Shape [text font]
  (sh/text-outline (font/resolve-font font :plain text-font-size) text (font/resolve-font "Noto Emoji" :plain text-font-size)))

(def ^:dynamic *text-font*
  "Font of a text shape without `:font`: the default theme's annotation font; while a plot draws, its theme's
  `[:text :annotation :font]` (bound by [[cljplot2.render]])."
  "DejaVu Sans Mono")

(def ^{:private true :const true :tag 'long} outline-cache-limit
  "Unit outlines [[outlines]] keeps."
  256)

(defonce ^{:private true :doc "Unit outlines by shape map, the least recently used evicted beyond [[outline-cache-limit]]."}
  outlines
  (cache/lru-cache outline-cache-limit))

(defn- outline-of
  "Gives the unit shape (see [[unit-shape]]) of a `:path` shape map or a `:text` one with its `:font`; `nil` when it
  draws nothing. Cached in [[outlines]]."
  [shape]
  (cache/lookup-or-compute outlines shape
                           (fn [{:keys [path text font]}]
                             (unit-shape (if path
                                           (or (parse-path path) (sh/path [] false))
                                           (text-outline text font))))))

(defn- unit-outline
  "Gives the unit outline (see [[outline-of]]) of a shape map, a text without `:font` in [[*text-font*]].

  Parameters:

  - `shape` (map): a `:path` or `:text` shape map.

  Returns the unit shape; `nil` when it draws nothing."
  [shape]
  (outline-of (if (and (:text shape) (nil? (:font shape))) (assoc shape :font *text-font*) shape)))

(defn- svg-uri
  "Gives the URI string of an SVG source.

  Parameters:

  - `source`: an existing file (path string or `java.io.File`), a classpath resource or a URL (string with a scheme or `java.net.URL`).

  Returns the URI string; `nil` when none is found."
  [source]
  (cond
    (instance? URL source) (str source)
    (instance? File source) (when (.isFile ^File source) (str (.toURI ^File source)))
    (string? source) (let [f (io/file source)]
                       (cond
                         (.isFile f) (str (.toURI f))
                         (io/resource source) (str (io/resource source))
                         (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*://" source) source))))

(defn- image?
  "Tests whether a value is a `BufferedImage` or a clojure2d image."
  [v]
  (or (instance? BufferedImage v) (satisfies? pr/ImageProto v)))

(defn shape-problem
  "Gives the reason a value isn't a shape.

  A shape is a built-in character of [[shapes]], or a map with exactly one of [[shape-kinds]] - `:path` (non-empty SVG path data that parses), `:text` (a non-empty string, optional `:font` - a bundled, installed or logical font name or `java.awt.Font`), `:svg` (a file, resource or URL that exists), `:image` (an image).

  Parameters:

  - `v`: any value.

  Returns the reason (a string); `nil` for a shape."
  [v]
  (if-not (map? v)
    (when-not (contains? shapes v)
      (str "should be a shape: one of " (apply str (sort shapes))
           ", or a map {:path svg-path-data}, {:text string}, {:svg source} or {:image image}"))
    (let [kinds (filter shape-kinds (keys v))
          extra (remove (conj shape-kinds :font) (keys v))]
      (cond
        (m/not== 1 (count kinds)) "a shape map should have exactly one of :path, :text, :svg, :image"
        (seq extra) (str "a shape map doesn't take " (pr-str (vec extra)))
        (and (contains? v :font) (not (contains? v :text))) "only a :text shape takes :font"
        (contains? v :path) (let [d (:path v)]
                              (cond
                                (not (string? d)) "a :path shape should be SVG path data (a string)"
                                (nil? (parse-path d)) (str "a :path shape's data doesn't parse: " (pr-str d))
                                (nil? (unit-outline v)) "a :path shape should draw something (empty path)"))
        (contains? v :text) (cond
                              (not (and (string? (:text v)) (seq (:text v)))) "a :text shape should be a non-empty string"
                              (and (contains? v :font) (not (font/known-font? (:font v))))
                              "a :text shape's :font should be a bundled, installed or logical font name, or a java.awt.Font")
        (contains? v :svg) (when-not (svg-uri (:svg v))
                             (str "a :svg shape's source isn't an existing file, classpath resource or URL: " (pr-str (:svg v))))
        :else (when-not (image? (:image v)) "an :image shape should be a BufferedImage or a clojure2d image")))))

(defn shape?
  "Tests whether a value is a shape (see [[shape-problem]]).

  Parameters:

  - `v`: any value.

  Returns true or false."
  [v]
  (nil? (shape-problem v)))

(def ^{:private true :const true :tag 'long} svg-cache-limit
  "Transcoded SVG images [[svg-images]] keeps."
  256)

(defonce ^{:private true :doc "Transcoded SVG images by source and pixel size, the least recently used evicted beyond [[svg-cache-limit]]."}
  svg-images
  (cache/lru-cache svg-cache-limit))

(defn- transcode
  "Gives an SVG source transcoded into a square image (aspect kept, centred).

  Parameters:

  - `source`: the SVG source (see [[svg-uri]]).
  - `n` (long): the image side in pixels.

  Returns a `BufferedImage`; throws `ex-info {:key :shape}` when it fails."
  ^BufferedImage [source ^long n]
  (try (c2d/transcode-svg (c2d/load-svg (svg-uri source)) n n)
       (catch Exception e
         (error/fail! (str "the :svg shape can't be drawn (" (ex-message e) ")") {:key :shape :value source}))))

(defn svg-image
  "Gives the SVG `source` as an `n` x `n` image, transcoded once per source and size while cached (see [[transcode]],
  [[svg-images]]).

  Parameters:

  - `source`: an SVG file path, classpath resource or URL (string, `java.io.File`, `java.net.URL`).
  - `n` (long): the image side in pixels.

  Returns a `BufferedImage`; a source that can't be transcoded is an error keyed `:shape` (nothing cached)."
  ^BufferedImage [source ^long n]
  (cache/lookup-or-compute svg-images [source n] (fn [[s side]] (transcode s side))))

(defn- triangle
  "Gives the corners of a triangle inscribed in a square around a point.

  Parameters:

  - `x`, `y` (doubles): the square's centre.
  - `r` (double): the square's half side.
  - `dir` (keyword): `:up`, `:down`, `:left` or `:right` - where the triangle points.

  Returns `[x1 y1 x2 y2 x3 y3]`."
  [^double x ^double y ^double r dir]
  (case dir
    :up [x (m/- y r) (m/+ x r) (m/+ y r) (m/- x r) (m/+ y r)]
    :down [x (m/+ y r) (m/+ x r) (m/- y r) (m/- x r) (m/- y r)]
    :left [(m/- x r) y (m/+ x r) (m/- y r) (m/+ x r) (m/+ y r)]
    :right [(m/+ x r) y (m/- x r) (m/- y r) (m/- x r) (m/+ y r)]))

(def ^:private triangle-dirs
  "Triangle shape character -> the direction it points."
  {\A :up \V :down \{ :left \} :right \^ :up \v :down \< :left \> :right})

(defn- outline!
  "Draws a built-in shape's outline (or stroke) with the canvas stroke and colour.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape` (character): a built-in shape.
  - `c` (`Vec3`): the centre x, y and the half size.

  Returns the canvas."
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (m/* 2.0 r)]
    (case shape
      (\O \o) (c2d/ellipse canvas x y d d true)
      (\S \s) (c2d/crect canvas x y d d true)
      (\A \V \{ \} \^ \v \< \>) (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
                                  (c2d/triangle canvas x1 y1 x2 y2 x3 y3 true))
      \- (c2d/line canvas (m/- x r) y (m/+ x r) y)
      \| (c2d/line canvas x (m/- y r) x (m/+ y r))
      \\ (c2d/line canvas (m/- x r) (m/- y r) (m/+ x r) (m/+ y r))
      \/ (c2d/line canvas (m/- x r) (m/+ y r) (m/+ x r) (m/- y r))
      \+ (do (c2d/line canvas (m/- x r) y (m/+ x r) y)
             (c2d/line canvas x (m/- y r) x (m/+ y r)))
      \x (do (c2d/line canvas (m/- x r) (m/- y r) (m/+ x r) (m/+ y r))
             (c2d/line canvas (m/- x r) (m/+ y r) (m/+ x r) (m/- y r))))))

(defn- fill!
  "Fills a built-in filled shape with the canvas colour.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape` (character): one of [[filled-shapes]].
  - `c` (`Vec3`): the centre x, y and the half size.

  Returns the canvas."
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (m/* 2.0 r)]
    (case shape
      \O (c2d/ellipse canvas x y d d false)
      \S (c2d/crect canvas x y d d false)
      (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
        (c2d/triangle canvas x1 y1 x2 y2 x3 y3 false)))))

(defn- draw-image!
  "Draws an image scaled to a side along its longer side, centred at a point.

  Parameters:

  - `canvas`: the canvas in graphics context (its transform is restored afterwards).
  - `img`: a `BufferedImage` or clojure2d image.
  - `center` (`Vec2`): the centre in pixels.
  - `side` (double): the longer side in pixels.

  Returns the canvas."
  [canvas img ^Vec2 center ^double side]
  (let [^BufferedImage image (pr/get-image img)
        w (.getWidth image)
        h (.getHeight image)
        k (m// side (m/max w h))]
    (-> canvas
        (c2d/push-matrix)
        (c2d/translate (m/- (.x center) (m/* 0.5 k w)) (m/- (.y center) (m/* 0.5 k h)))
        (c2d/scale k k)
        (c2d/image image 0 0 w h)
        (c2d/pop-matrix))))

(defn- draw-map!
  "Draws a shape map fitted into its size square.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape` (map): the shape map (see [[shape-problem]]).
  - `center` (`Vec2`): the centre in pixels.
  - `style` (map): see [[draw!]].

  Returns `nil`."
  [canvas shape ^Vec2 center {:keys [^double size color stroke-color ^double stroke-width]}]
  (cond
    (:svg shape) (let [n (m/max 1 (m/round size))]
                   (draw-image! canvas (svg-image (:svg shape) n) center n))
    (:image shape) (draw-image! canvas (:image shape) center size)
    :else (when-let [unit (unit-outline shape)]
            (let [outline (sh/transform-shape unit [[:translate (.x center) (.y center)] [:scale size]])]
              (when-not (= :none color)
                (c2d/set-color canvas color)
                (c2d/shape canvas outline false))
              (when (and (:path shape) (not= :none stroke-color) (m/pos? stroke-width))
                (c2d/set-color canvas stroke-color)
                (c2d/shape canvas outline true))))))

(declare draw!)

(defn draw-key!
  "Draws a shape as a legend key glyph centred in a key box.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape`: the shape.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `style` (map): see [[draw!]]; a `:size` larger than the box (a constant chart `:size` above the key size) shrinks to the box's shorter side, so glyphs don't cross into the next row.

  Returns `nil`."
  [canvas shape {:keys [^double x ^double y ^double w ^double h]} style]
  (let [side (m/min w h)]
    (draw! canvas shape (Vec2. (m/+ x (m/* 0.5 w)) (m/+ y (m/* 0.5 h)))
           (update style :size #(m/min (double %) side)))))

(defn styled-key!
  "Draws a point chart's key glyph: its style's shape fitted and centred in a legend key box (see [[draw-key!]]).

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` (pixels).
  - `st` (map): the chart's style with the key's attributes merged - `:shape`, `:size`, `:color`, `:stroke-color`, `:stroke-width`.
  - `stroke?`: truthy - the canvas stroke is set to `:stroke-width` first (line shapes and outlines drawn at the chart's width); else the caller's stroke stays."
  [canvas box st stroke?]
  (when stroke?
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))}))
  (draw-key! canvas (:shape st) box st))

(defn draw!
  "Draws a shape centred at a point.

  Lines use the canvas stroke - the caller sets it once (width `:stroke-width`) for many points.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape`: a built-in shape character or a shape map (see [[shape-problem]]).
  - `center` (`Vec2`): the centre in pixels.
  - `style` (map): `:size` - the bounding box side in pixels; `:color` - fills filled shapes and strokes the others (`:none` - nothing); `:stroke-color` and `:stroke-width` - the outline of filled shapes (`:none` or width 0 - none).

  Returns `nil`. A shape map is fitted into the size square: `:path` filled and outlined as a filled shape, `:text` filled, `:svg` / `:image` in their own colours."
  [canvas shape ^Vec2 center {:keys [^double size color stroke-color stroke-width] :as style}]
  (if (map? shape)
    (draw-map! canvas shape center style)
    (let [centre-size (v/vec3 (.x center) (.y center) (m/* 0.5 size))
          ink? (not= :none color)]
      (if (filled-shapes shape)
        (do (when ink?
              (c2d/set-color canvas color)
              (fill! canvas shape centre-size))
            (when (and (not= :none stroke-color) (m/pos? (double stroke-width)))
              (c2d/set-color canvas stroke-color)
              (outline! canvas shape centre-size)))
        (when ink?
          (c2d/set-color canvas color)
          (outline! canvas shape centre-size))))))
