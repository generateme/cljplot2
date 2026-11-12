(ns cljplot2.font
  "Font resolution: bundled font families (loaded from resources), system font names or `java.awt.Font` objects.

  Bundled: `\"Liberation Mono\"` (SIL OFL 1.1, see `resources/fonts/`), one TTF file per font style,
  so bold and italic are real font faces, not synthesized ones."
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d])
  (:import [java.awt Font]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def font-styles
  "Font styles and their `java.awt.Font` style constants."
  {:plain Font/PLAIN :bold Font/BOLD :italic Font/ITALIC :bold-italic (bit-or Font/BOLD Font/ITALIC)})

(def bundled-fonts
  "Bundled font family name -> font style -> classpath resource of the TTF file."
  {"Liberation Mono" {:plain "fonts/LiberationMono-Regular.ttf"
                      :bold "fonts/LiberationMono-Bold.ttf"
                      :italic "fonts/LiberationMono-Italic.ttf"
                      :bold-italic "fonts/LiberationMono-BoldItalic.ttf"}})

(defn- bundled-font-delays
  "Classpath resource -> delay of its loaded `Font` (size 1)."
  []
  (into {} (for [styles (vals bundled-fonts)
                 resource (vals styles)]
             [resource (delay (c2d/load-font (or (io/resource resource)
                                                 (throw (ex-info "Bundled font file not found."
                                                                 {:key :font :resource resource})))))])))

(def ^:private bundled-font-files
  "Delays of the bundled font files; every file is loaded at most once per JVM, also under concurrent first use."
  (bundled-font-delays))

(def ^:private logical-fonts #{"dialog" "dialoginput" "serif" "sansserif" "monospaced"})

(def ^:private installed-families
  "Lower-cased installed font family names; `nil` when the platform can't list them."
  (delay (try (set (map #(.toLowerCase ^String %)
                        (.getAvailableFontFamilyNames (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))))
              (catch Exception _ nil))))

(defn- known-family?
  [^String family]
  (let [family (.toLowerCase family)]
    (or (logical-fonts family)
        (nil? @installed-families)
        (contains? @installed-families family))))

(defn resolve-font
  "`java.awt.Font` of `size` points for `font` and `font-style` (a key of [[font-styles]]).

  `font` is a bundled family name (see [[bundled-fonts]]), an installed or logical (`\"SansSerif\"`, ...) font
  family name (case-insensitive; an unknown name throws `ex-info` with `{:key :font}` unless the platform
  can't list its fonts) or a `java.awt.Font` (its size and style are replaced by `size` and `font-style`)."
  ^Font [font font-style size]
  (let [style (font-styles font-style)]
    (when-not style
      (throw (ex-info "Unknown font style." {:key :font-style :value font-style :allowed (set (keys font-styles))})))
    (when-not (and (number? size) (pos? (double size)))
      (throw (ex-info "Font size must be a positive number." {:key :size :value size})))
    (let [size (float size)]
      (cond
        (instance? Font font) (.deriveFont ^Font font (int style) size)
        (contains? bundled-fonts font) (.deriveFont ^Font @(bundled-font-files (get-in bundled-fonts [font font-style])) size)
        (and (string? font) (known-family? font)) (.deriveFont (Font. ^String font (int style) (int 1)) size)
        (string? font) (throw (ex-info "Unknown font family." {:key :font :value font}))
        :else (throw (ex-info "Font must be a font name or java.awt.Font." {:key :font :value font}))))))
