(ns cljplot2.option-cards-test
  "Option cards (reference readability): the option model of `cljplot2.describe` - accepted values and forms with
  their meanings, examples, theme defaults, concept titles - and the rules a chart migrated to cards follows: full
  sentence descriptions, a meaning for every value of a short list (a long list's other values under one meaning),
  a meaning for every form of an option taking several, a default for every optional option, an example for every
  option. `test-util/migrated` is the one switch: the chart kinds whose pages show option cards."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [malli.core :as malli]
            [cljplot2.chart :as chart]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.core :as plot]
            [cljplot2.test-util :refer [migrated same-pixels?]]
            [notebook-utils]))

(def ^:private max-listed-values
  "The most values an option may list without each having its own meaning."
  6)

(def ^:private max-description
  "The longest description of a migrated option, in characters."
  300)

(defn- kind-rows
  "The own option rows and the mapping rows of chart `kind`."
  [kind]
  (let [c ((get (describe/chart-constructors) kind))]
    {:options (describe/own-rows (describe/options c)) :mapping (describe/mapping c)}))

(defn- leaf-rows
  "The option rows of `rows` that aren't a map of options (a container row)."
  [rows]
  (remove describe/container-row? rows))

;; the option model

(def ^:private fixture
  [:map {:closed true}
   (schema/optional :e {:description "e" :cljplot2/value-meanings {:a "A."}} [:enum :a :b])
   (schema/nilable :b {:description "b" :default false} schema/bool)
   (schema/optional :o {:description "o" :cljplot2/form-meanings {:vector "Several."}}
                    [:or [:= :auto] schema/fraction [:vector schema/fraction]])
   (schema/nilable :n {:description "n" :cljplot2/value-meanings {nil "Nothing."}} [:enum :x])
   (schema/optional :c {:description "c"} schema/color)
   (schema/optional :x {:description "x" :cljplot2/example false} schema/bool)
   (schema/optional :f {:description "f"} schema/function)
   (schema/optional :d {:description "d"} [:or [:= :none] schema/color])])

(defn- row
  "The row of key `k` of [[fixture]]."
  [k]
  (first (filter #(= k (:key %)) (describe/schema-rows fixture))))

(deftest forms-test
  (testing "an enum: its values, the entry's meanings"
    (is (= [{:value :a :meaning "A."} {:value :b :meaning nil}] (:values (row :e))))
    (is (= [] (:forms (row :e)))))
  (testing "a boolean: true and false, nil left out"
    (is (= [true false] (map :value (:values (row :b))))))
  (testing "an :or: a constant, a named type and a vector - each form once, the form meaning by its key"
    (is (= [:auto] (map :value (:values (row :o)))))
    (is (= [[::schema/fraction "fraction" nil] [:vector "vector of fractions" "Several."]]
           (map (juxt :form :type :meaning) (:forms (row :o))))))
  (testing "boundary: nil is a value only when documented, then first"
    (is (= [nil :x] (map :value (:values (row :n)))))
    (is (= "Nothing." (:meaning (first (:values (row :n)))))))
  (testing "a named type with a value set: its values with the type's meanings and its other types' forms"
    (is (= [{:value :none :meaning "Draws nothing."}] (:values (row :c))))
    (is (= ["solid colour"] (map :type (:forms (row :c))))))
  (testing "smooth :method - 25 methods, 4 explained, the rest under one meaning, the function form"
    (let [r (first (filter #(= [:method] (:path %)) (:options (kind-rows :smooth))))]
      (is (= 25 (count (:values r))))
      (is (= #{:loess :lm :glm :gp} (set (map :value (filter :meaning (:values r))))))
      (is (string? (:other-values r)))
      (is (= [::schema/function] (map :form (:forms r))))))
  (testing "boundary: a value two parts of the schema give is listed once, with the meaning one of them has"
    (is (= [{:value :none :meaning "Draws nothing."}] (:values (row :d)))))
  (testing "boundary: an other type outside the documented types throws"
    (let [odd (schema/named-type ::odd keyword? "should be odd" {:cljplot2/other-types [::unknown]})]
      (is (thrown? clojure.lang.ExceptionInfo (describe/schema-rows [:map [:k odd]]))))))

(deftest example-test
  (testing "an explicit example wins, also false"
    (is (= [false] (describe/example (row :x)))))
  (testing "else the first value other than the default, meanings first"
    (is (= [true] (describe/example (row :b))))
    (is (= [:a] (describe/example (row :e)))))
  (testing "boundary: no literal value and no example - none"
    (is (nil? (describe/example (row :f)))))
  (let [rows (describe/options (chart/line))
        at #(first (filter (fn [r] (= % (:path r))) rows))]
    (testing "example options: a nested option inside its container's theme default, a context the option needs"
      (is (= {:gaps :bridge :bridge {:alpha-factor 0.3 :line-width 3}} (describe/example-options rows (at [:bridge :line-width]))) "the container's context applies too")
      (is (= {:fill-gradient :mpl/viridis :fill-gradient-axis :y} (describe/example-options rows (at [:fill-gradient-axis]))))
      (is (= {:position :stack} (describe/example-options rows (at [:position])))))
    (testing "boundary: a row without an example has no example options"
      (is (nil? (describe/example-options [] (row :f)))))))

(deftest theme-default-test
  (let [rows (:options (kind-rows :smooth))]
    (testing "style rows take their default from the theme, flagged"
      (is (every? :theme-default (filter #(#{[:fill] [:alpha]} (:path %)) rows))))
    (testing "behaviour options aren't flagged"
      (is (not-any? :theme-default (filter #(#{[:method] [:n]} (:path %)) rows))))))

(defn- reference-files
  "The reference notebooks."
  []
  (filter #(str/ends-with? (.getName ^java.io.File %) ".clj") (file-seq (io/file "notebooks/reference"))))

(defn- concept-section
  "The source of the reference section headed `## title`, up to the next `## ` heading; `nil` when no page has it."
  [title]
  (some #(second (re-find (re-pattern (str "(?ms)^;; ## " (java.util.regex.Pattern/quote title) "$(.*?)(?=^;; ## |\\z)"))
                          (slurp %)))
        (reference-files)))

(deftest migration-switch-test
  (testing "the migrated kinds are exactly the kinds with a concept title"
    (is (= migrated (set (keys describe/concepts)))))
  (doseq [kind migrated
          :let [section (concept-section (describe/concepts kind))]]
    (testing (str kind ": its section shows option cards and a mapping list, no options table")
      (is (some? section))
      (is (str/includes? (str section) "nu/option-cards"))
      (is (str/includes? (str section) "nu/mapping-list"))
      (is (not (str/includes? (str section) "nu/options-table")))))
  (testing "boundary: a section ends at the next ## heading"
    (is (nil? (concept-section "No such concept")))))

;; the card rules of migrated charts

(defn- sentence-breaks
  "Paths of `rows` whose description isn't one or two sentences: starting uppercase (or with code), ending with a
  period, at most [[max-description]] characters."
  [rows]
  (vec (for [{:keys [path description]} rows
             :let [sentences (count (re-seq #"\.(\s|$)" (str/replace (str description) "e.g." "eg")))]
             :when (or (not (string? description)) (not (re-find #"^[A-Z`]" description))
                       (not (str/ends-with? description ".")) (< max-description (count description))
                       (< 2 sentences))]
         path)))

(defn- meaning-gaps
  "Paths of `rows` missing a meaning: a value of a list of at most [[max-listed-values]], a long list's other values
  without `:other-values`, a form of an option taking more than one value or form."
  [rows]
  (vec (for [{:keys [path values forms other-values]} rows
             :let [unexplained (remove :meaning values)]
             :when (or (and (seq unexplained) (<= (count values) max-listed-values))
                       (and (seq unexplained) (nil? other-values))
                       (and (< 1 (+ (count values) (count forms))) (some (complement :meaning) forms)))]
         path)))

(defn- validation-text?
  "Tests whether a form's type text reads like a validation message rather than a short type text: an article
  anywhere (`a finite number`), `should be`, `or nil`, or a plural glued to code or a parenthesis."
  [type-text]
  (re-find #"\b(a|an) |should be| or nil|`s\b|\)s\b" type-text))

(deftest short-type-text-test
  (testing "plurals: regular, -es, -ies, a named type's own"
    (is (= ["numbers" "dashes" "deficiencies" "keys" "boxes"] (map #'describe/plural ["number" "dash" "deficiency" "key" "box"])))
    (is (= "vector of dates or times" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:vector schema/temporal]]]))))))))
  (testing "boundary: a composite or code item goes in parentheses, not pluralised"
    (is (= "vector of (pair of fractions)" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:vector [:tuple schema/fraction schema/fraction]]]])))))))
    (is (= "set of (`:x` or `:y`)" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:set [:enum :x :y]]]]))))))))
  (testing "bounds read as words; an :and keeps its predicate (by its message), never drops it"
    (is (= "integer at least 2" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:and :int [:>= 2]]]])))))))
    (is (= "integer, odd" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:and :int [:fn {:error/message "should be odd"} odd?]]]]))))))))
  (testing "boundary: the validation-text guard"
    (is (every? validation-text? ["a finite number" "pair of (integer)s" "vector of `:x`s" "keyword or nil"]))
    (is (not-any? validation-text? ["vector of open fractions" "map of `:x` or `:y` to padding" "number above 0 and below 90"]))))

(defn- default-gaps
  "Paths of optional `rows` without a default value, a default text or a theme default."
  [rows]
  (vec (for [{:keys [path required default default-doc theme-default]} rows
             :when (and (not required) (nil? default) (nil? default-doc) (not theme-default))]
         path)))

(deftest card-rules-rule-test
  (testing "boundary: sentences"
    (is (= [[:a] [:b] [:c] [:d]]
           (sentence-breaks [{:path [:a] :description "lowercase."} {:path [:b] :description "No period"}
                             {:path [:c] :description (str "A" (apply str (repeat 300 "x")) ".")}
                             {:path [:d] :description "One. Two. Three."}
                             {:path [:e] :description "One, e.g. this. Two."}
                             {:path [:f] :description "`:code` first."}]))))
  (testing "boundary: meanings - 6 values all explained, 7 need other-values, several forms each explained"
    (let [vs (fn [n] (mapv (fn [i] {:value i}) (range n)))]
      (is (= [[:a]] (meaning-gaps [{:path [:a] :values (vs 6) :forms []}])))
      (is (= [[:b]] (meaning-gaps [{:path [:b] :values (vs 7) :forms []}])))
      (is (= [] (meaning-gaps [{:path [:c] :values (vs 7) :forms [] :other-values "Others."}])))
      (is (= [] (meaning-gaps [{:path [:d] :values [] :forms [{:form :f}]}])) "one form needs no meaning")
      (is (= [[:e]] (meaning-gaps [{:path [:e] :values [{:value 1 :meaning "One."}] :forms [{:form :f}]}])))))
  (testing "boundary: defaults - false is a default, required rows need none"
    (is (= [[:b]] (default-gaps [{:path [:a] :default false} {:path [:b]} {:path [:c] :required true}
                                 {:path [:d] :theme-default true :default 1}])))))

(deftest card-rules-test
  (doseq [kind migrated
          :let [{:keys [options mapping]} (kind-rows kind)
                leaves (leaf-rows options)]]
    (testing (str kind ": descriptions are one or two sentences")
      (is (= [] (sentence-breaks (concat options mapping)))))
    (testing (str kind ": default texts are sentences")
      (is (= [] (sentence-breaks (map #(assoc % :description (:default-doc %)) (filter :default-doc options))))))
    (testing (str kind ": values and forms have meanings")
      (is (= [] (meaning-gaps leaves))))
    (testing (str kind ": every optional option has a default")
      (is (= [] (default-gaps leaves))))
    (testing (str kind ": every form names a documented type or reads as a short type text (no validation-message text)")
      (is (= [] (vec (for [{:keys [path forms]} leaves
                           {:keys [form type]} forms
                           :when (or (and (qualified-keyword? form) (not (contains? describe/documented-types form)))
                                     (validation-text? type))]
                       [path type])))))
    (testing (str kind ": every option has an example")
      (is (= [] (vec (keep #(when-not (describe/example %) (:path %)) leaves)))))
    (testing (str kind ": every example is a valid option on its own (a code form evaluated first)")
      (let [options-schema (get-in ((get (describe/chart-constructors) kind)) [:schema :options])]
        (doseq [row leaves
                :let [example (walk/postwalk #(if (seq? %) (binding [*ns* (the-ns 'cljplot2.option-cards-test)] (eval %)) %)
                                             (describe/example-options options row))]]
          (is (malli/validate options-schema example) (str (:path row) " " example)))))))

(def ^:private example-fixtures
  "Kind -> a small plot (`:data`, `:mapping`) on which every option example of the kind changes the picture: unsorted
  x, two groups, a missing y, up and down candles."
  (let [xs [3 1 4 2 6 5 8 7]
        rows (for [g ["a" "b"] [i x] (map-indexed vector xs)]
               {:x x :y (when-not (and (= g "a") (= i 3)) (+ (* 0.5 x) (if (= g "a") 1.0 2.5) (* 0.3 (m/sin (* 3.0 x)))))
                :g g})]
    {:scatter {:data (remove #(nil? (:y %)) rows) :mapping {:x :x :y :y}}
     :line {:data rows :mapping {:x :x :y :y :color :g}}
     :segment {:data [{:x 0 :y 0 :xend 2 :yend 1} {:x 1 :y 2 :xend 3 :yend 0}] :mapping {:x :x :y :y :xend :xend :yend :yend}}
     :polygon {:data [{:x 0 :y 0 :g "a"} {:x 2 :y 0 :g "a"} {:x 1 :y 2 :g "a"} {:x 2 :y 1 :g "b"} {:x 4 :y 1 :g "b"} {:x 3 :y 3 :g "b"}]
               :mapping {:x :x :y :y :group :g}}
     :ribbon {:data (map (fn [x] {:x x :lo (- (m/sin x) 1.0) :hi (+ (m/sin x) 1.0)}) (range 0 6.5 0.5)) :mapping {:x :x :ymin :lo :ymax :hi}}
     :candlestick {:data [{:x 1 :o 2 :h 5 :l 1 :c 4} {:x 2 :o 4 :h 4.5 :l 2 :c 3} {:x 3 :o 3 :h 6 :l 2.5 :c 5.5}]
                   :mapping {:x :x :open :o :high :h :low :l :close :c}}
     :smooth {:data (remove #(nil? (:y %)) (map #(update % :x (fn [x] (if (= "a" (:g %)) x (+ x 0.5)))) rows))
              :mapping {:x :x :y :y :color :g}}}))

(defn- without-option
  "`options` without the option at `path`: removed, or - nested in a container with a map default (the theme's) - back
  at the container default's value, so a required key stays."
  [options rows path]
  (let [container (first (filter #(= (pop path) (:path %)) rows))
        default (when (map? (:default container)) (get (:default container) (peek path)))]
    (cond
      (some? default) (assoc-in options path default)
      (m/one? (count path)) (dissoc options (first path))
      :else (update-in options (pop path) dissoc (peek path)))))

(def ^:private inert-in-fixtures
  "`[kind path]` -> why the option's example acts only beyond a chart's own options (no pixels change in the fixture)."
  {[:smooth [:stat-space]] "acts only on a :log axis, a plot scale"
   [:scatter [:descending]] "acts only where points of different colours overlap"})

(deftest examples-act-test
  (doseq [kind migrated
          :let [constructor (get (describe/chart-constructors) kind)
                rows (describe/options (constructor))
                {:keys [data mapping]} (example-fixtures kind)
                draw (fn [options] {:data data :mapping mapping :charts [(constructor options)]})]
          row (leaf-rows (describe/own-rows rows))
          :when (not (contains? inert-in-fixtures [kind (:path row)]))
          :let [example (walk/postwalk #(if (seq? %) (binding [*ns* (the-ns 'cljplot2.option-cards-test)] (eval %)) %)
                                       (describe/example-options rows row))]]
    (testing (str kind " " (:path row) ": the example changes the picture - " (pr-str example))
      (is (not (same-pixels? (draw (without-option example rows (:path row))) (draw example) {:width 320 :height 240}))))))

;; types and links

(deftest documented-types-test
  (let [props (map malli/type-properties (vals describe/documented-types))]
    (testing "every documented type has a name and a meaning with an example (code)"
      (is (every? #(and (string? (:cljplot2/type-name %)) (str/includes? (str (:cljplot2/type-meaning %)) "`")) props)))
    (testing "names are distinct (one Types page section each)"
      (is (apply distinct? (map :cljplot2/type-name props)))))
  (testing "every named type of cljplot2.schema is documented"
    (is (= (set (for [[_ v] (ns-publics 'cljplot2.schema)
                      :let [x @v]
                      :when (malli/into-schema? x)
                      :let [t (malli/type x)]
                      :when (and (qualified-keyword? t) (= "cljplot2.schema" (namespace t)))]
                  t))
           (set (keys schema/named-types))))))

(defn- heading-anchors
  "The anchors of the headings of a book page (`reference.<file>`, `use_cases.<file>`, `galleries.<file>` - the
  notebook `notebooks/<dir>/<file>.clj`) - on the Types page also its generated type sections - `nil` for no such
  notebook."
  [page]
  (let [[_ dir file] (re-matches #"(reference|use_cases|galleries)\.(.+)" page)
        f (io/file "notebooks" dir (str file ".clj"))
        anchor #'notebook-utils/heading-anchor]
    (when (.exists f)
      (into (set (map #(anchor (second %)) (re-seq #"(?m)^;; #+ (.+)$" (slurp f))))
            (when (= "reference.types" page)
              (map #(anchor (:cljplot2/type-name (malli/type-properties %))) (vals describe/documented-types)))))))

(defn- broken-links
  "`[page anchor]` of the links in `text` into the book's reference, use case and gallery pages that don't resolve: a page that doesn't
  exist (with or without an anchor), or an anchor its headings don't give; relative links and the book's absolute
  URLs (the README's)."
  [text]
  (vec (for [[_ page _ anchor] (re-seq #"\((?:https://generateme\.github\.io/cljplot2/)?((?:reference|use_cases|galleries)\.[a-z_0-9]+)\.html(#([^)\s]+))?\)" text)
             :let [anchors (heading-anchors page)]
             :when (or (nil? anchors) (and anchor (not (contains? anchors anchor))))]
         [page anchor])))

(deftest reference-links-test
  (testing "every link of the book's prose (notebooks, README) into a reference, use case or gallery page resolves to the page and its heading"
    (is (= [] (vec (for [^java.io.File f (cons (io/file "README.md") (file-seq (io/file "notebooks")))
                         :when (re-find #"\.(clj|md)$" (.getName f))
                         link (broken-links (slurp f))]
                     [(.getName f) link])))))
  (testing "boundary: a missing page fails with or without an anchor, a missing anchor fails, a page alone passes"
    (is (= [["reference.no_such_page" "x"] ["reference.no_such_page" nil] ["reference.types" "no-such-type"]]
           (broken-links (str "(reference.no_such_page.html#x) (https://generateme.github.io/cljplot2/reference.no_such_page.html) "
                              "(reference.types.html#no-such-type) (reference.types.html) (reference.types.html#fraction)")))))
  (testing "boundary: use case and gallery pages - a renamed gallery fails, existing pages pass"
    (is (= [["galleries.cljplot_extras" nil]]
           (broken-links "(galleries.cljplot_extras.html) (galleries.cljplot.html) (use_cases.flow.html)")))))
