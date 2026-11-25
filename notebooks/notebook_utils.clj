(ns notebook-utils
  "Notebook helpers: datasets read through the manifest `data/datasets.edn`, the attribution line of an example, the
  option tables and option cards of the reference part ([[options-table]], [[option-cards]], [[mapping-list]], the
  Types page's [[type-entries]]) and data tables ([[data-table]]).
  No plotting helpers - examples use the plain cljplot2 API."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [fastmath.core :as m]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [wadogo.scale :as ws]
            [malli.core :as malli]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.describe :as describe])
  (:import [fastmath.vector Vec2 Vec3 Vec4]))

(def ^:private data-folder "data/")

(defn datasets
  "The dataset manifest: key -> `{:file :format :title :source :licence :via :strings :notes}` (`data/datasets.edn` on the
  classpath)."
  []
  (if-let [manifest (io/resource (str data-folder "datasets.edn"))]
    (edn/read-string (slurp manifest))
    (throw (ex-info "Dataset manifest data/datasets.edn not found on the classpath." {:key :manifest}))))

(defn- manifest-entry
  "Manifest entry of dataset `k`; throws `ex-info` naming the known keys for an unknown one."
  [k]
  (let [manifest (datasets)]
    (or (get manifest k)
        (throw (ex-info (str "Unknown dataset " (pr-str k) ", known: " (str/join ", " (sort (keys manifest))) ".")
                        {:key :dataset :value k :known (set (keys manifest))})))))

(def ^:private long-pattern #"[-+]?\d+")
(def ^:private double-pattern #"[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?")

(defn- parse-number
  "Number of the string `s` (long when integral and in range, else double), `nil` when `s` isn't a number."
  [^String s]
  (cond
    (re-matches long-pattern s) (try (Long/parseLong s) (catch NumberFormatException _ (Double/parseDouble s)))
    (re-matches double-pattern s) (Double/parseDouble s)
    :else nil))

(defn- numeric-column?
  "Does every non-empty value of `values` parse as a number (and at least one value is non-empty)?"
  [values]
  (let [present (remove str/blank? values)]
    (and (seq present) (every? parse-number present))))

(defn- drop-blank-lines
  "CSV `rows` (after the header) without blank lines (`[\"\"]`). With a single column a blank line is a missing value,
  so only trailing blank lines go."
  [single-column? rows]
  (let [blank? #(= [""] %)]
    (if single-column?
      (vec (reverse (drop-while blank? (reverse rows))))
      (vec (remove blank? rows)))))

(defn parse-csv
  "Rows (maps with keyword keys from the header row, in the header's order) of CSV `text`. Empty values become `nil`; a column whose every
  non-empty value is a number holds numbers (long or double), other columns and the columns named in `:strings`
  (keywords; e.g. ids with leading zeros) hold strings. Blank lines are skipped (with a single column only trailing
  ones; inner ones are missing values).
  Throws `ex-info` for a row whose length differs from the header's."
  ([text] (parse-csv text {}))
  ([text {:keys [strings]}]
   (let [[header & rows] (drop-while #(= [""] %) (csv/read-csv text))
         ks (mapv keyword header)
         rows (drop-blank-lines (= 1 (count ks)) rows)
         strings (set strings)]
     (doseq [[i row] (map-indexed vector rows)
             :when (not= (count ks) (count row))]
       (throw (ex-info (str "CSV row " (inc i) " has " (count row) " values, the header has " (count ks) ".")
                       {:key :csv :row (inc i) :value row})))
     (let [numeric (set (filter #(and (not (strings (ks %)))
                                      (numeric-column? (map (fn [row] (nth row %)) rows)))
                                (range (count ks))))]
       (mapv (fn [row]
               (apply array-map (mapcat (fn [i v]
                                          [(ks i) (cond (str/blank? v) nil
                                                        (numeric i) (parse-number v)
                                                        :else v)])
                                        (range) row)))
             rows)))))

(defn- json-key-order
  "Object keys (keywords) of JSON `text` in order of first appearance."
  [text]
  (distinct (map (comp keyword second) (re-seq #"\"((?:[^\"\\]|\\.)*)\"\s*:" text))))

(defn- json-rows
  "Data of JSON `text`; a sequence of objects as rows whose keys follow the text's order (`clojure.data.json` builds
  hash maps, which scramble more than 8 keys)."
  [text]
  (let [data (json/read-str text :key-fn keyword)]
    (if (and (sequential? data) (every? map? data))
      (let [order (json-key-order text)
            ordered? (set order)]
        (mapv (fn [row]
                (let [ks (concat (filter #(contains? row %) order) (remove ordered? (keys row)))]
                  (apply array-map (mapcat (fn [k] [k (get row k)]) ks))))
              data))
      data)))

(defn dataset
  "Rows (maps with keyword keys) of dataset `k` from the manifest, see [[datasets]]; a CSV entry's `:strings`
  (column keywords) keep those columns as strings, see [[parse-csv]].
  Throws `ex-info` for an unknown key, a missing file or an unknown `:format`."
  [k]
  (let [{:keys [file format strings]} (manifest-entry k)
        resource (and file (io/resource (str data-folder file)))]
    (when-not resource
      (throw (ex-info (str "File of dataset " (pr-str k) " not found: " (pr-str file) ".")
                      {:key :file :value file :dataset k})))
    (case format
      :json (json-rows (slurp resource))
      :csv (parse-csv (slurp resource) {:strings strings})
      (throw (ex-info (str "Dataset " (pr-str k) " has an unknown :format " (pr-str format) ", known: :json, :csv.")
                      {:key :format :value format :dataset k})))))

(defn levels
  "Level order of column `column` of dataset `k` - the R factor levels recorded in `data/levels.edn` (CSV and JSON keep
  no factor order), for a categorical scale's `:domain`. Levels are typed as the dataset loads the column: numbers
  when every level is one (unless the manifest lists the column under `:strings`), else strings.
  Throws `ex-info {:key :levels}` for a dataset or column without recorded levels."
  [k column]
  (let [recorded (some-> (io/resource (str data-folder "levels.edn")) slurp edn/read-string)
        ls (get-in recorded [k column])]
    (when-not ls
      (throw (ex-info (str "No recorded levels for " (pr-str k) " " (pr-str column) ".")
                      {:key :levels :value [k column] :known (update-vals recorded (comp set keys))})))
    (if (and (not (contains? (set (:strings (manifest-entry k))) column)) (every? parse-number ls))
      (mapv parse-number ls)
      ls)))

(defn- source-text
  "Markdown of the source part: `source` linked to its single URL, or followed by numbered links to several; URLs
  alone are linked as they are."
  [source urls]
  (cond
    (nil? source) (str "Source: " (str/join ", " (map #(str "[" % "](" % ")") urls)) ".")
    (= 1 (count urls)) (str "Source: [" source "](" (first urls) ").")
    :else (str "Source: " source " (" (str/join ", " (map-indexed (fn [i u] (str "[" (inc i) "](" u ")")) urls)) ").")))

(defn source-note
  "Attribution line of an example (Markdown, its own code hidden on the page):
  `:source` - where the use case or figure comes from (e.g. \"R Graph Gallery - Histogram\", \"Lattice Fig 1.1\"),
  `:url` - the original example's page: a URL string or a non-empty vector of them (linked from the source text),
  `:dataset` - manifest key of the data, or a non-empty vector of keys for several datasets (each one's title,
  original source and licence are read from the manifest).
  `:source` names an outside example, so it needs `:url`; a note may credit only data (`:dataset`); examples written
  for cljplot2 carry no source, only their data's credit. `:url` or `:dataset` is required.
  Throws `ex-info {:key :url}` for `:source` without `:url` or a `:url` that isn't a string or a non-empty vector of
  strings, `ex-info {:key :dataset}` for a `:dataset` that isn't a key or a non-empty vector of keys, and like
  [[dataset]] for an unknown dataset."
  [{:keys [source url dataset]}]
  (when (and source (nil? url))
    (throw (ex-info "source-note :source names an outside example: give its :url." {:key :url :value source})))
  (when-not (or url dataset)
    (throw (ex-info "source-note needs :url or :dataset." {:key :source-note})))
  (let [urls (cond (nil? url) []
                   (string? url) [url]
                   (and (vector? url) (seq url) (every? string? url)) url
                   :else (throw (ex-info "source-note :url should be a URL string or a non-empty vector of them."
                                         {:key :url :value url})))
        dataset-keys (cond (nil? dataset) []
                           (keyword? dataset) [dataset]
                           (and (vector? dataset) (seq dataset) (every? keyword? dataset)) dataset
                           :else (throw (ex-info "source-note :dataset should be a manifest key or a non-empty vector of them."
                                                 {:key :dataset :value dataset})))
        data-texts (for [k dataset-keys
                         :let [{:keys [title licence] data-source :source} (manifest-entry k)]]
                     (str "Data: " title ", " data-source "; licence: " licence "."))]
    (kindly/hide-code
     (kind/md (str/join " " (remove nil? (cons (when (seq urls) (source-text source urls)) data-texts)))))))

;; option tables (reference part)

(defn- color-key?
  "Does keyword `k` name a colour (`:color`, `:stroke-color`, `:fill`, `:background` ...)?"
  [k]
  (and (keyword? k) (boolean (re-find #"(color|fill|background)$" (name k)))))

(defn- hex-color
  "Integer colour `c` as a `0xrrggbb` symbol (printed without quotes)."
  [c]
  (symbol (format "0x%06x" (long c))))

(defn- readable-color
  "`Vec4` colour `c` as a vector of its rounded channels, `[r g b]` when opaque (alpha 255), else `[r g b a]`."
  [c]
  (let [channels (mapv (fn [^double v] (m/round v)) c)]
    (if (m/== 255 (long (peek channels))) (pop channels) channels)))

(defn- readable-value
  "`v` with colours and functions readable when printed: a `Vec4` colour as [[readable-color]], an integer under a
  colour key as `0xrrggbb`, a function as `fn`."
  [v]
  (walk/prewalk (fn [x]
                  (cond (instance? Vec4 x) (readable-color x)
                        (fn? x) 'fn
                        (and (map-entry? x) (color-key? (key x)) (integer? (val x))) [(key x) (hex-color (val x))]
                        :else x))
                v))

(defn- default-text
  "Default cell of option `row`: its printed `:default` (an integer colour of a colour key as `0xrrggbb`) and its
  `:default-doc`, either or both, `\"\"` without."
  [{:keys [key default default-doc]}]
  (let [value (when (some? default)
                (pr-str (if (and (color-key? key) (integer? default)) (hex-color default) (readable-value default))))]
    (str/join "; " (remove nil? [value default-doc]))))

(defn- option-text
  "Option cell of `row`: its path's keys (`:grid :x`), `(required)` after a required one."
  [{:keys [path required]}]
  (cond-> (str/join " " (map pr-str path)) required (str " (required)")))

(defn- section-rows
  "The rows of `rows` in `section` (their `:section`); throws `ex-info {:key :section}` naming the sections of `rows`
  when none is."
  [rows section]
  (let [selected (filterv #(= section (:section %)) rows)]
    (when (empty? selected)
      (throw (ex-info (str "No rows in section " (pr-str section) ", sections: "
                           (str/join " " (distinct (keep :section rows))) ".")
                      {:key :section :value section :known (set (keep :section rows))})))
    selected))

(defn options-table
  "Table of option `rows` (from `cljplot2.describe`: `options`, `mapping`, `fragment-rows`, `theme-rows`), its code
  hidden: option path, type, default (value and / or text) and description; with `section` only the rows of that
  slice (`:section`, see `cljplot2.describe`). A Group column - the shared group's title, documented once in the
  Shared options chapter - when the rows mix groups (rows of one group are named by the text around the table).
  Throws `ex-info {:key :rows}` for no rows, `{:key :section}` for a section without rows - a table is never empty."
  ([rows]
   (when (empty? rows)
     (throw (ex-info "An options table needs rows." {:key :rows :value rows})))
   (let [grouped? (< 1 (count (distinct (map :fragment rows))))
         group-title #(get-in describe/fragments [(:fragment %) :title] "")]
     (kindly/hide-code
      (kind/table {:column-names (cond-> ["Option" "Type" "Default" "Description"] grouped? (conj "Group"))
                   :row-vectors (mapv (fn [row]
                                        (cond-> [(option-text row) (:type row) (default-text row) (:description row)]
                                          grouped? (conj (group-title row))))
                                      rows)}))))
  ([rows section]
   (options-table (section-rows rows section))))

(def ^:private shared-options-page
  "The book page of the Shared options chapter (Clay names a page by its notebook namespace)."
  "reference.shared_options.html")

(defn- heading-anchor
  "The id Quarto (pandoc) gives a heading of text `title`: lowercase, characters other than letters, digits, `_`, `-`,
  `.` and spaces removed, spaces as `-`, everything before the first letter dropped (`\"3D view\"` -> `d-view`)."
  [title]
  (-> (str/lower-case title)
      (str/replace #"[^\p{L}\p{N}_\-. ]" "")
      (str/replace #"\s+" "-")
      (str/replace #"^[^\p{L}]+" "")))

(defn shared-groups
  "Markdown, its code hidden, naming the shared groups option `rows` take (`cljplot2.describe/fragment-keys`), each
  linked to its section of the Shared options chapter, with the keys of the group the rows hold.
  Throws `ex-info {:key :rows}` for rows without groups."
  [rows]
  (let [groups (describe/fragment-keys rows)]
    (when (empty? groups)
      (throw (ex-info "shared-groups needs rows of shared groups." {:key :rows :value rows})))
    (kindly/hide-code
     (kind/md (str "Shared options: "
                   (str/join "; " (for [[fragment ks] groups
                                        :let [title (get-in describe/fragments [fragment :title])]]
                                    (format "[%s](%s#%s) (%s)" title shared-options-page (heading-anchor title)
                                            (str/join " " (map #(str "`" (pr-str %) "`") ks)))))
                   ".")))))

;; option cards (reference pages rewritten for readability)

(def ^:private types-page
  "The book page of the Types chapter (Clay names a page by its notebook namespace)."
  "reference.types.html")

(defn- rich-text
  "Hiccup of `text` with its backtick spans as code: `nil` for `nil`."
  [text]
  (when text
    (map-indexed (fn [i part] (if (odd? i) [:code part] part)) (str/split text #"`" -1))))

(defn- printed
  "`v` as printed code, colours and functions readable (see [[readable-value]])."
  [v]
  (pr-str (readable-value v)))

(defn- card-default
  "Hiccup of the default of option `row` on its card's header line: the value as code (`theme` after a theme
  default), the default text, both or `nil`."
  [{:keys [key default default-doc theme-default]}]
  (when (or (some? default) default-doc)
    (into [:span {:class "option-default"} "Default: "]
          (concat (when (some? default)
                    [[:code (if (and (color-key? key) (integer? default)) (str (hex-color default)) (printed default))]
                     (when theme-default " (theme)")])
                  (when (and (some? default) default-doc) ["; "])
                  (rich-text default-doc)))))

(defn- type-link
  "Hiccup of the type text of a form: a link to its section of the Types page for a documented type."
  [{:keys [form type]}]
  (if (contains? describe/documented-types form)
    [:a {:href (str types-page "#" (heading-anchor type))} type]
    (into [:span] (rich-text type))))

(defn- bullet
  "A list item: `head` (hiccup) and the rich text of `meaning` after a dash, when there is one."
  [head meaning]
  (into [:li head] (when meaning (cons " – " (rich-text meaning)))))

(defn- card-values
  "Hiccup list items of what option `row` accepts: its values with meanings (the default first), the values without
  one on one line after the row's `:other-values` text, then its other forms."
  [{:keys [values forms other-values default]}]
  (let [meant (sort-by #(not= default (:value %)) (filter :meaning values))
        rest-values (remove :meaning values)]
    (concat (map #(bullet [:code (printed (:value %))] (:meaning %)) meant)
            (when (seq rest-values)
              [(into [:li {:class "option-other"}]
                     (concat (rich-text other-values) (when other-values [" "])
                             (interpose ", " (map #(vector :code (printed (:value %))) rest-values))))])
            (map #(bullet (type-link %) (:meaning %)) forms))))

(defn- example-call
  "The example of option `row` of `rows` as code `(call {...})`: its options map (`cljplot2.describe/example-options`,
  nested paths as nested maps, with the options it needs); `nil` without an example."
  [call rows row]
  (when-let [options (describe/example-options rows row)]
    (str "(" call " " (printed options) ")")))

(defn- option-card
  "Hiccup of the card of option `row`, three parts: the header (the option and its default), its example as a call of
  `call` (from the option `rows` it belongs to), then its description and what it accepts."
  [call rows {:keys [path] :as row}]
  (let [items (card-values row)
        example (example-call call rows row)]
    [:div {:class "option-card"}
     [:dt [:code (str/join " " (map pr-str path))] (card-default row)]
     (cond-> [:dd]
       example (conj [:div {:class "option-example"} [:code example]])
       :always (conj (cond-> (into [:div {:class "option-text"}] (rich-text (:description row)))
                       (seq items) (conj (into [:ul] items)))))]))

(defn option-cards
  "Option cards of `rows` (from `cljplot2.describe/options`, the chart's or function's own), its code hidden: per
  option a boxed card - a header (the option and its default), an example call of `call` (a symbol, e.g.
  `chart/smooth`), then its description, a bullet per value or form with its meaning and the values without one on
  one line (styles in `clay.edn`). With `section`
  only the rows of that slice (see [[options-table]]).
  Throws `ex-info {:key :rows}` for no rows, `{:key :section}` for a section without rows."
  ([call rows] (option-cards call rows nil))
  ([call rows section]
   (let [shown (if section (section-rows rows section) rows)]
     (when (empty? shown)
       (throw (ex-info "Option cards need rows." {:key :rows :value rows})))
     (kindly/hide-code
      (kind/hiccup (into [:dl {:class "options"}] (map #(option-card call rows %)) shown))))))

(defn mapping-list
  "The mapping keys of a chart in a titled box, its code hidden: each key, `required` when the chart needs it, and its
  description. `rows` - `cljplot2.describe/mapping` of the chart. Throws `ex-info {:key :rows}` for no rows."
  [rows]
  (when (empty? rows)
    (throw (ex-info "A mapping list needs rows." {:key :rows :value rows})))
  (kindly/hide-code
   (kind/hiccup [:div {:class "mapping"}
                 [:div {:class "mapping-title"} "Mapping"
                  [:span {:class "mapping-hint"} " – the row values the chart reads, under " [:code ":mapping"]]]
                 (into [:ul]
                       (map (fn [{:keys [path required description]}]
                              (into [:li [:code (str/join " " (map pr-str path))]]
                                    (concat (when required [" " [:span {:class "required"} "required"]])
                                            [" – "] (rich-text description)))))
                       rows)])))

(defn type-entries
  "The Types page's sections, its code hidden: a `##` heading per documented type (`cljplot2.describe/documented-types`,
  sorted by name) with its meaning - the targets of the option cards' type links."
  []
  (kindly/hide-code
   (kind/md (str/join "\n\n" (for [{:cljplot2/keys [type-name type-meaning]}
                                    (sort-by :cljplot2/type-name (map malli/type-properties (vals describe/documented-types)))]
                                (str "## " type-name "\n\n" type-meaning))))))

(defn scale-kind-doc
  "wadogo's description of position scale kind `k` (`wadogo.scale/kind-info`, Markdown), its code hidden."
  [k]
  (kindly/hide-code (kind/md (ws/kind-info k))))

;; data tables

(def data-table-rows
  "The most rows [[data-table]] shows (its box scrolls past a few); also the rows checked to tell rows from other
  values."
  100)

(def ^:private truncation-mark
  "The text of every cell of the row [[data-table]] adds below the shown rows of longer data."
  "…")

(defn- number-row?
  "Tests whether `v` is a number or a non-empty sequential of numbers (an observation of a sample, a matrix row) - not
  a fastmath point or colour (`Vec2`, `Vec3`, `Vec4`)."
  [v]
  (or (number? v)
      (and (sequential? v) (not (instance? Vec2 v)) (not (instance? Vec3 v)) (not (instance? Vec4 v))
           (seq v) (every? number? v))))

(defn- data-rows
  "Rows of `data`: rows (a non-empty sequence of maps) as they are, columns (a map of equal-length non-empty
  sequences) as rows, a sample (a non-empty sequence of numbers or number vectors) as rows `{:value v}`; `nil` for
  anything else."
  [data]
  (cond
    (and (sequential? data) (seq data) (every? map? (take data-table-rows data))) data
    (and (sequential? data) (seq data) (every? number-row? (take data-table-rows data))) (map (fn [v] {:value v}) data)
    (and (map? data) (seq data) (every? sequential? (vals data)) (pos? (count (first (vals data))))
         (apply = (map count (vals data))))
    (let [ks (keys data)] (apply map (fn [& vs] (zipmap ks vs)) (map data ks)))
    :else nil))

(defn- cell-text
  "Text of a table cell: empty for a missing value, a string as it is, a `java.time` value as its ISO text, a shingle
  interval as its label text, else printed (colours and functions readable, see [[readable-value]])."
  [v]
  (cond
    (nil? v) ""
    (string? v) v
    (instance? java.time.temporal.Temporal v) (str v)
    (shingle/interval? v) (str v)
    :else (pr-str (readable-value v))))

(def data-table-class
  "CSS class of the box holding a [[data-table]] - compact, left-aligned and scrolling past a few rows (the style is in
  `clay.edn`, `:include-in-header`, under the same class name); option tables keep the book's table style."
  "data-table")

(defn data-table
  "The first 100 rows ([[data-table-rows]]) of `data` - rows (maps), columns (a map of equal-length sequences) or a
  sample of numbers or number vectors (one `value` column) - as a table in a box of class [[data-table-class]], its
  code hidden: a column per key of the shown rows (first-appearance order), cells as [[cell-text]]; longer data gets
  a last row of [[truncation-mark]] cells (the data is cut there). Throws `ex-info {:key :data}` for empty data or
  data that is none of these."
  [data]
  (let [taken (some->> (data-rows data) (take (inc (long data-table-rows))) vec)
        rows (vec (take data-table-rows taken))
        truncated? (< (long data-table-rows) (count taken))]
    (when (empty? rows)
      (throw (ex-info "data-table needs rows (maps), columns (a map of sequences) or a sample (numbers or number vectors)." {:key :data :value data})))
    (let [ks (vec (distinct (mapcat keys rows)))]
      (kindly/hide-code
       (kind/hiccup
        [:div {:class data-table-class}
         (kind/table {:column-names (mapv #(if (keyword? %) (name %) (str %)) ks)
                      :row-vectors (cond-> (mapv (fn [row] (mapv #(cell-text (get row %)) ks)) rows)
                                     truncated? (conj (vec (repeat (count ks) truncation-mark))))})])))))
