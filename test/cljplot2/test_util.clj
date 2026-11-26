(ns cljplot2.test-util
  "Helpers shared by the tests."
  (:require [clojure.string :as str]
            [malli.core :as malli]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.theme :as theme]))

(defn schema-entries
  "`[path props schema]` of every map entry anywhere in `schema` - nested maps, `:multi` branches, `:and` / `:or` /
  `:maybe` / collection children."
  [schema]
  (letfn [(walk [s path]
            (let [compiled (malli/schema s)]
              (case (malli/type compiled)
                :map (mapcat (fn [[k props v]] (cons [(conj path k) props v] (walk v (conj path k)))) (malli/children compiled))
                :multi (mapcat (fn [[_ _ v]] (walk v path)) (malli/children compiled))
                (:maybe :and :or :vector :sequential :set :map-of :tuple)
                (mapcat #(when (malli/schema? (try (malli/schema %) (catch Exception _ nil))) (walk % path)) (malli/children compiled))
                nil)))]
    (walk schema [])))

(defn card-text?
  "Tests whether text `t` follows the writing rule of option cards: one or two full sentences (capitalised or opening
  with code, ending with a period), at most 300 characters. A sentence ends at a period followed by a capital, code or
  the text's end (`e.g.` and `1 .. n` don't end one). `nil` and non-strings fail."
  [t]
  (and (string? t) (re-find #"^[A-Z`]" t) (str/ends-with? t ".") (<= (count t) 300)
       (<= (count (re-seq #"\.(?=\s+[A-Z`]|$)" (str/replace t "e.g." "eg"))) 2)))

(defn path-under?
  "Tests whether key `path` is `root` or lies below it (`[]` - every path)."
  [root path]
  (= (seq root) (seq (take (count root) path))))

(defn error-data
  "`ex-data` of the `ExceptionInfo` thrown by calling `f`, `nil` when it doesn't throw one."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn error
  "[[error-data]] without `:errors` (the first error: `:key`, `:in`, `:value` and any extra data)."
  [f]
  (some-> (error-data f) (dissoc :errors)))

(defn drawn-style
  "Style keys a chart value draws with under the default theme (or `theme`, merged): its kind's style, its named
  `:theme-style`, then its options over them (style keys only matter; other options come along)."
  ([chart] (drawn-style chart nil))
  ([{:keys [kind options]} theme]
   (merge (theme/chart-style (theme/plot-theme theme) kind options) options)))

(defn error-ins
  "Set of the `:in` paths of every error thrown by calling `f`."
  [f]
  (set (map :in (:errors (error-data f)))))

;; defaults (reference part: every schema `:default` is the real one)

(defn explicit-defaults
  "Options `options` with every constant `:default` of option `rows` set where absent (`nil` - not set); nested rows
  only inside a given map (an absent map - e.g. no `:arrow` - is no map of defaults)."
  [rows options]
  (reduce (fn [opts {:keys [path default]}]
            (if (and (some? default) (nil? (get-in opts path))
                     (or (= 1 (count path)) (map? (get-in opts (pop path)))))
              (assoc-in opts path default)
              opts))
          (into {} (remove (comp nil? val)) options)
          (remove #(nil? (:default %)) rows)))

(defn pixels
  "ARGB pixels of `img`."
  ^ints [^java.awt.image.BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn same-pixels?
  "Do plots `p` and `q` render to the same pixels with render `options`?"
  [p q options]
  (java.util.Arrays/equals (pixels (plot/render-image p options)) (pixels (plot/render-image q options))))

(defn builder-defaults-hold?
  "Does builder `v` (a var with `:cljplot2/options`) applied to `args` and `options` render the same pixels as with
  every constant `:default` of its options set explicitly?"
  [v args options render-options]
  (let [f @v
        explicit (explicit-defaults (describe/options v) options)]
    (same-pixels? (apply f (concat args [options])) (apply f (concat args [explicit])) render-options)))
