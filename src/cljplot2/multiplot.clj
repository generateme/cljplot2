(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.subplot]])."
  (:require [fastmath.core :as m]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- invalid [message data] (throw (ex-info message (assoc data :key :arrange))))

(defn- positive-int? [v] (and (integer? v) (pos? (long v))))

(defn- shape
  "`[rows cols]` for `n` plots and optional `columns` / `rows` (default one column)."
  [^long n columns rows]
  (doseq [[k v] [[:columns columns] [:rows rows]]]
    (when-not (or (nil? v) (positive-int? v))
      (invalid "Arrange :columns / :rows must be positive integers." {:value k})))
  (let [[^long r ^long c] (cond
                            (and rows columns) [(long rows) (long columns)]
                            columns [(long (m/ceil (/ (double n) (double columns)))) (long columns)]
                            rows [(long rows) (long (m/ceil (/ (double n) (double rows))))]
                            :else [n 1])]
    (when (< (* r c) n)
      (invalid "Arrange layout is too small for the plots." {:plots n :rows r :columns c}))
    [r c]))

(defn- weights
  [k ws ^long n]
  (cond
    (nil? ws) (vec (repeat n 1))
    (and (sequential? ws) (= n (count ws)) (every? #(and (number? %) (pos? (double %))) ws)) (vec ws)
    :else (invalid "Arrange :widths / :heights must be positive numbers, one per column / row." {:value k :count n})))

(defn- tracks
  "Margin, flexible tracks `[prefix i]` weighted by `ws` with fixed gaps, margin."
  [prefix ws gap margin]
  (-> [{:id :margin-start :size [:fixed margin]}]
      (into (butlast (mapcat (fn [i w] [{:id [prefix i] :size [:flexible w]} {:id [:gap i] :size [:fixed gap]}])
                             (range) ws)))
      (conj {:id :margin-end :size [:fixed margin]})))

(defn arrange
  "Plot map arranging `plots` (a non-empty vector of plot maps) in a grid of cells filled row by row.

  `options`:

  * `:columns`, `:rows` - grid shape (default one column; one given, the other computed)
  * `:widths`, `:heights` - relative column / row weights (default equal)
  * `:title` - overall title above the cells (`:labels {:title t}`)
  * `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]`
    around them; sub-plots keep their own themes

  Cells are items `[:plot i]` (row `[:row r]`, column `[:column c]`). Throws `ex-info {:key :arrange}` for an
  empty `plots`, a too small shape or bad weights."
  ([plots] (arrange plots {}))
  ([plots {:keys [columns rows widths heights title] :as options}]
   (when-not (and (sequential? plots) (seq plots))
     (invalid "Arrange needs a non-empty sequence of plots." {:value plots}))
   (let [plots (vec plots)
         [^long r ^long c] (shape (count plots) columns rows)
         th (theme/plot-theme (:theme options))
         gap (get-in th [:multiplot :gap])
         margin (get-in th [:plot :margin])]
     (cond-> {:custom-grid {:columns (tracks :column (weights :widths widths c) gap margin)
                            :rows (tracks :row (weights :heights heights r) gap margin)
                            :items (vec (map-indexed (fn [i p]
                                                       {:id [:plot i] :column [:column (rem (long i) c)] :row [:row (quot (long i) c)]
                                                        :content {:kind :plot :plot p}})
                                                     plots))}}
       (:theme options) (assoc :theme (:theme options))
       (some? title) (assoc :labels {:title title})))))
