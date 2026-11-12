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

(def ^:private loaded-fonts
  "Classpath resource -> loaded `Font` (size 1); every file is loaded at most once per JVM."
  (atom {}))

(defn- load-bundled
  ^Font [resource]
  (or (@loaded-fonts resource)
      (let [url (or (io/resource resource)
                    (throw (ex-info "Bundled font file not found." {:key :font :resource resource})))
            font (c2d/load-font url)]
        (swap! loaded-fonts assoc resource font)
        font)))

(defn resolve-font
  "`java.awt.Font` of `size` points for `font` and `font-style` (a key of [[font-styles]]).

  `font` is a bundled family name (see [[bundled-fonts]]), another font name (`java.awt.Font/decode`,
  falls back to a logical font when unknown) or a `java.awt.Font` (only its size and style are changed)."
  ^Font [font font-style size]
  (let [style (font-styles font-style)]
    (when-not style
      (throw (ex-info "Unknown font style." {:key :font-style :value font-style :allowed (set (keys font-styles))})))
    (when-not (and (number? size) (pos? (double size)))
      (throw (ex-info "Font size must be a positive number." {:key :size :value size})))
    (let [size (float size)]
      (cond
        (instance? Font font) (.deriveFont ^Font font (int style) size)
        (contains? bundled-fonts font) (.deriveFont (load-bundled (get-in bundled-fonts [font font-style])) size)
        (string? font) (.deriveFont (Font/decode ^String font) (int style) size)
        :else (throw (ex-info "Font must be a font name or java.awt.Font." {:key :font :value font}))))))
