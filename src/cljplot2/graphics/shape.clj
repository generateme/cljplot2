(ns cljplot2.graphics.shape
  "Point shapes (old cljplot set), drawn centered at a position with a size (bounding box side, pixels).

  * `\\O` / `\\o` - filled / outlined circle
  * `\\S` / `\\s` - filled / outlined square
  * `\\A \\V \\{ \\}` - filled triangle pointing up / down / left / right
  * `\\^ \\v \\< \\>` - outlined triangle pointing up / down / left / right
  * `\\-` `\\|` `\\\\` `\\/` - horizontal, vertical and diagonal strokes
  * `\\+` - plus, `\\x` - cross

  Filled shapes use the fill color and an optional outline; outlined shapes and strokes use the line color.

  A shape may also be a map (see [[shape-problem]]), fitted into the size square (aspect kept, centred):

  * `{:path \"M0 0 L10 0 L5 8 Z\"}` - SVG path data, filled with the color, outlined like a filled shape
  * `{:text \"🐄\"}` - a text outline filled with the color; `:font` (default the bundled `\"DejaVu Sans Mono\"`),
    characters it lacks from the bundled monochrome `\"Noto Emoji\"` (see [[cljplot2.graphics.font]])
  * `{:svg source}` - an SVG file path, classpath resource or URL (string, `java.io.File`, `java.net.URL`), in its own
    colors; transcoded once per source and pixel size
  * `{:image img}` - a `BufferedImage` or clojure2d image, in its own colors"
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.font :as font])
  (:import [fastmath.vector Vec2 Vec3]
           [java.awt Font Graphics2D RenderingHints Shape]
           [java.awt.font FontRenderContext TextAttribute TextLayout]
           [java.awt.geom AffineTransform Path2D Path2D$Double]
           [java.awt.image BufferedImage]
           [java.io File StringReader]
           [java.net URL]
           [java.text AttributedString]
           [org.apache.batik.parser AWTPathProducer]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def filled-shapes #{\O \S \A \V \{ \}})
(def line-shapes #{\o \s \^ \v \< \> \- \| \\ \/ \+ \x})
(def shapes (into filled-shapes line-shapes))

(def shape-kinds
  "Keys of a shape map, one per map: SVG path data, a text, an SVG source, an image."
  #{:path :text :svg :image})

(defn- unit-shape
  "`shape` (java.awt) moved and scaled so its bounding box is centred at the origin with the longer side 1; `nil`
  for an empty shape."
  ^Shape [^Shape shape]
  (let [b (.getBounds2D shape)
        side (m/max (.getWidth b) (.getHeight b))]
    (when (pos? side)
      (let [t (AffineTransform/getScaleInstance (/ 1.0 side) (/ 1.0 side))]
        (.translate t (- (.getCenterX b)) (- (.getCenterY b)))
        (.createTransformedShape t shape)))))

(defn- parse-path
  "java.awt shape of SVG path data `d` (non-zero winding); `nil` when it doesn't parse."
  ^Shape [^String d]
  (try (AWTPathProducer/createShape (StringReader. d) Path2D/WIND_NON_ZERO)
       (catch Exception _ nil)))

(def ^:private text-font-size
  "Point size text outlines are built at (scaled to the shape size when drawn)."
  100.0)

(defn- text-runs
  "`[start end font]` runs of `text`: `main` where it can show the character, the bundled emoji font elsewhere."
  [^String text ^Font main]
  (let [emoji (font/resolve-font "Noto Emoji" :plain text-font-size)
        n (.length text)]
    (loop [i 0 runs []]
      (if (>= i n)
        runs
        (let [cp (.codePointAt text (int i))
              j (+ i (Character/charCount cp))
              f (if (.canDisplay main (int cp)) main emoji)
              [s _ g :as last-run] (peek runs)]
          (recur j (if (and last-run (identical? g f)) (conj (pop runs) [s j f]) (conj runs [i j f]))))))))

(defn- text-outline
  "java.awt outline of `text` in `font` (a font name or `java.awt.Font`, see [[cljplot2.graphics.font/resolve-font]]),
  characters the font lacks from the bundled emoji font."
  ^Shape [^String text font]
  (let [main (font/resolve-font font :plain text-font-size)
        attributed (AttributedString. text)]
    (doseq [[s e f] (text-runs text main)]
      (.addAttribute attributed TextAttribute/FONT f (int s) (int e)))
    (.getOutline (TextLayout. (.getIterator attributed) (FontRenderContext. nil RenderingHints/VALUE_TEXT_ANTIALIAS_ON RenderingHints/VALUE_FRACTIONALMETRICS_ON)) nil)))

(def ^:private default-text-font
  "Font of a text shape without `:font`: the default theme font."
  "DejaVu Sans Mono")

(def ^:private unit-outline
  "Unit shape (see [[unit-shape]]) of a `:path` or `:text` shape map; `nil` when it draws nothing. Cached."
  (memoize (fn [{:keys [path text font]}]
             (unit-shape (if path
                           (or (parse-path path) (Path2D$Double.))
                           (text-outline text (or font default-text-font)))))))

(defn- svg-uri
  "URI string of the SVG `source`: an existing file, a classpath resource or a URL (string with a scheme or
  `java.net.URL`); `nil` when none is found."
  [source]
  (cond
    (instance? URL source) (str source)
    (instance? File source) (when (.isFile ^File source) (str (.toURI ^File source)))
    (string? source) (let [f (io/file source)]
                       (cond
                         (.isFile f) (str (.toURI f))
                         (io/resource source) (str (io/resource source))
                         (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*://" source) source))))

(defn- image? [v] (or (instance? BufferedImage v) (satisfies? pr/ImageProto v)))

(defn shape-problem
  "Why `v` isn't a shape, or `nil`: a built-in character of [[shapes]], or a map with exactly one of
  [[shape-kinds]] - `:path` (non-empty SVG path data that parses), `:text` (a non-empty string, optional `:font` -
  a bundled, installed or logical font name or `java.awt.Font`), `:svg` (a file, resource or URL that exists),
  `:image` (an image)."
  [v]
  (if-not (map? v)
    (when-not (contains? shapes v)
      (str "should be a shape: one of " (apply str (sort shapes))
           ", or a map {:path svg-path-data}, {:text string}, {:svg source} or {:image image}"))
    (let [kinds (filter shape-kinds (keys v))
          extra (remove (conj shape-kinds :font) (keys v))]
      (cond
        (not= 1 (count kinds)) "a shape map should have exactly one of :path, :text, :svg, :image"
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
  "Is `v` a shape (see [[shape-problem]])?"
  [v]
  (nil? (shape-problem v)))

(def ^:private svg-cache-limit
  "Transcoded SVG images kept (the cache is emptied when full)."
  1024)

(def ^:private svg-images (atom {}))

(defn- transcode
  "SVG `source` transcoded into an `n` x `n` image (aspect kept, centred); `ex-info {:key :shape}` when it fails."
  ^BufferedImage [source ^long n]
  (try (c2d/transcode-svg (c2d/load-svg (svg-uri source)) n n)
       (catch Exception e
         (throw (ex-info (str "Invalid input: the :svg shape can't be drawn: " (pr-str source) " (" (ex-message e) ")")
                         {:key :shape :value source :errors [{:in nil :value source :message "the :svg shape can't be drawn"}]}
                         e)))))

(defn svg-image
  "The SVG `source` as an `n` x `n` image, transcoded once per source and size (see [[transcode]])."
  ^BufferedImage [source ^long n]
  (let [k [source n]]
    (or (@svg-images k)
        (let [img (transcode source n)]
          (swap! svg-images #(assoc (if (>= (count %) (long svg-cache-limit)) {} %) k img))
          img))))

(defn- triangle
  "Corners of a triangle inscribed in the square of half side `r` around (x, y), pointing `dir`."
  [^double x ^double y ^double r dir]
  (case dir
    :up [x (- y r) (+ x r) (+ y r) (- x r) (+ y r)]
    :down [x (+ y r) (+ x r) (- y r) (- x r) (- y r)]
    :left [(- x r) y (+ x r) (- y r) (+ x r) (+ y r)]
    :right [(+ x r) y (- x r) (- y r) (- x r) (+ y r)]))

(def ^:private triangle-dirs {\A :up \V :down \{ :left \} :right \^ :up \v :down \< :left \> :right})

(defn- outline!
  "Draw `shape` outline (or stroke) around center `c` - `Vec3` (x, y, half size)."
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (* 2.0 r)]
    (case shape
      (\O \o) (c2d/ellipse canvas x y d d true)
      (\S \s) (c2d/crect canvas x y d d true)
      (\A \V \{ \} \^ \v \< \>) (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
                                  (c2d/triangle canvas x1 y1 x2 y2 x3 y3 true))
      \- (c2d/line canvas (- x r) y (+ x r) y)
      \| (c2d/line canvas x (- y r) x (+ y r))
      \\ (c2d/line canvas (- x r) (- y r) (+ x r) (+ y r))
      \/ (c2d/line canvas (- x r) (+ y r) (+ x r) (- y r))
      \+ (do (c2d/line canvas (- x r) y (+ x r) y) (c2d/line canvas x (- y r) x (+ y r)))
      \x (do (c2d/line canvas (- x r) (- y r) (+ x r) (+ y r)) (c2d/line canvas (- x r) (+ y r) (+ x r) (- y r))))))

(defn- fill!
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (* 2.0 r)]
    (case shape
      \O (c2d/ellipse canvas x y d d false)
      \S (c2d/crect canvas x y d d false)
      (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
        (c2d/triangle canvas x1 y1 x2 y2 x3 y3 false)))))

(defn- draw-image!
  "Draw `img` scaled to `side` pixels along its longer side, centred at `center`."
  [canvas img ^Vec2 center ^double side]
  (let [^BufferedImage img (pr/get-image img)
        w (.getWidth img) h (.getHeight img)
        k (/ side (double (m/max w h)))
        t (AffineTransform/getTranslateInstance (- (.x center) (* 0.5 k w)) (- (.y center) (* 0.5 k h)))]
    (.scale t k k)
    (.drawImage ^Graphics2D (:graphics canvas) img t nil)))

(defn- draw-map!
  "Draw the shape map `shape` (see [[shape-problem]]) centred at `center` in `style` (see [[draw!]])."
  [canvas shape ^Vec2 center {:keys [size color stroke-color stroke-width]}]
  (let [size (double size)]
    (cond
      (:svg shape) (let [n (m/max 1 (m/round size))]
                     (draw-image! canvas (svg-image (:svg shape) n) center (double n)))
      (:image shape) (draw-image! canvas (:image shape) center size)
      :else (when-let [unit (unit-outline shape)]
              (let [t (AffineTransform/getTranslateInstance (.x center) (.y center))
                    _ (.scale t size size)
                    outline (.createTransformedShape t unit)]
                (c2d/set-color canvas color)
                (c2d/shape canvas outline false)
                (when (and (:path shape) (not= :none stroke-color) (pos? (double stroke-width)))
                  (c2d/set-color canvas stroke-color)
                  (c2d/shape canvas outline true)))))))

(defn draw!
  "Draw `shape` centered at `center` (`Vec2`). `style` - `{:size :color :stroke-color :stroke-width}`, `:size` is
  the bounding box side: filled shapes are filled with `:color` and outlined with `:stroke-color` (`:none` or
  `:stroke-width` 0 skips the outline); outlined shapes and strokes are drawn with `:color`. Lines use the canvas
  stroke - the caller sets it once (width `:stroke-width`) for many points. A shape map (see [[shape-problem]]) is
  fitted into the size square: `:path` filled and outlined as a filled shape, `:text` filled, `:svg` / `:image` in
  their own colors."
  [canvas shape ^Vec2 center {:keys [size color stroke-color stroke-width] :as style}]
  (if (map? shape)
    (draw-map! canvas shape center style)
    (let [c (v/vec3 (.x center) (.y center) (* 0.5 (double size)))]
    (c2d/set-color canvas color)
    (if (filled-shapes shape)
      (do (fill! canvas shape c)
          (when (and (not= :none stroke-color) (pos? (double stroke-width)))
            (c2d/set-color canvas stroke-color)
            (outline! canvas shape c)))
      (outline! canvas shape c)))))
