(ns cljplot2.graphics.font
  "Font resolution: bundled font families (loaded from resources), system font names or `java.awt.Font` objects.

  Bundled: `DejaVu Sans Mono` (the default theme's font; Bitstream Vera licence, DejaVu changes public domain,
  see `resources/fonts/LICENSE-DejaVu.txt`; it covers the superscript digits and minus of power labels), one TTF file
  per font style, so bold and italic (oblique) are real font faces, not synthesized ones; `Noto Emoji` (monochrome
  emoji outlines, SIL Open Font License 1.1, see `resources/fonts/LICENSE-NotoEmoji.txt`; one file - the variable
  font's default Regular instance - for every style), the fallback of text shapes (see [[cljplot2.graphics.shape]]).
  A bundled name wins over an installed family of the same name, so plots render alike on every platform.

  Functions and vars: [[font-styles]], [[bundled-fonts]], [[known-font?]], [[resolve-font]]."
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.error :as error])
  (:import [java.awt Font]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def font-styles
  "Font styles and their `java.awt.Font` style constants."
  {:plain Font/PLAIN :bold Font/BOLD :italic Font/ITALIC :bold-italic (bit-or Font/BOLD Font/ITALIC)})

(def bundled-fonts
  "Bundled font family name -> font style -> classpath resource of the TTF file."
  {"DejaVu Sans Mono" {:plain "fonts/DejaVuSansMono.ttf"
                       :bold "fonts/DejaVuSansMono-Bold.ttf"
                       :italic "fonts/DejaVuSansMono-Oblique.ttf"
                       :bold-italic "fonts/DejaVuSansMono-BoldOblique.ttf"}
   "Noto Emoji" {:plain "fonts/NotoEmoji.ttf"
                 :bold "fonts/NotoEmoji.ttf"
                 :italic "fonts/NotoEmoji.ttf"
                 :bold-italic "fonts/NotoEmoji.ttf"}})

(defn- bundled-font-delays
  "Gives the delays of the bundled font files.

  Returns a map of classpath resource -> delay of its loaded `Font` (size 1); a missing file throws `ex-info {:key :font}` when forced."
  []
  (into {} (for [styles (vals bundled-fonts)
                 resource (vals styles)]
             [resource (delay (c2d/load-font (or (io/resource resource)
                                                 (error/fail! "Bundled font file not found."
                                                              {:key :font :resource resource}))))])))

(def ^:private bundled-font-files
  "Delays of the bundled font files; every file is loaded at most once per JVM, also under concurrent first use."
  (bundled-font-delays))

(def ^:private logical-fonts
  "Lower-cased Java logical font family names, always resolvable."
  #{"dialog" "dialoginput" "serif" "sansserif" "monospaced"})

(def ^:private installed-families
  "Lower-cased installed font family names; `nil` when the platform can't list them (an empty `c2d/fonts-list`)."
  (delay (when (seq c2d/fonts-list)
           (set (map #(.toLowerCase ^String %) c2d/fonts-list)))))

(defn- known-family?
  "Tests whether a font family name is logical or installed (case-insensitive).

  Parameters:

  - `family` (string): the family name.

  Returns truthy for a logical or installed family, and for any name when the platform can't list its fonts."
  [^String family]
  (let [lower (.toLowerCase family)]
    (or (logical-fonts lower)
        (nil? @installed-families)
        (contains? @installed-families lower))))

(defn known-font?
  "Tests whether [[resolve-font]] resolves a font.

  Parameters:

  - `font`: a `java.awt.Font`, a font family name or any value.

  Returns truthy for a `java.awt.Font`, a bundled family name or an installed or logical family name (any name when the platform can't list its fonts); falsey otherwise."
  [font]
  (or (instance? Font font)
      (contains? bundled-fonts font)
      (and (string? font) (known-family? font))))

(defn resolve-font
  "Gives the `java.awt.Font` of a font name or font object in a style and size.

  Parameters:

  - `font`: a bundled family name (see [[bundled-fonts]]), an installed or logical (`SansSerif`, ...) font family name (case-insensitive), or a `java.awt.Font` (its size and style are replaced).
  - `font-style` (keyword): a key of [[font-styles]].
  - `size` (positive number): points.

  Returns the font. Throws `ex-info` - `{:key :font}` for an unknown family (unless the platform can't list its fonts) or a value that isn't a font, `{:key :font-style}`, `{:key :size}` for invalid ones."
  {:cljplot2/api true}
  ^Font [font font-style size]
  (let [style (font-styles font-style)]
    (when-not style
      (error/fail! "Unknown font style." {:key :font-style :value font-style :allowed (set (keys font-styles))}))
    (when-not (and (number? size) (m/pos? (double size)))
      (error/fail! "Font size must be a positive number." {:key :size :value size}))
    (let [points (float size)]
      (cond
        (instance? Font font) (.deriveFont ^Font font (int style) points)
        (contains? bundled-fonts font) (.deriveFont ^Font @(bundled-font-files (get-in bundled-fonts [font font-style])) points)
        (and (string? font) (known-family? font)) (.deriveFont (Font. ^String font (int style) (int 1)) points)
        (string? font) (error/fail! "Unknown font family." {:key :font :value font})
        :else (error/fail! "Font must be a font name or java.awt.Font." {:key :font :value font})))))
