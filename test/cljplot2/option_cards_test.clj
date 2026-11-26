(ns cljplot2.option-cards-test
  "Option cards (reference readability): the option model of `cljplot2.describe` - accepted values and forms with
  their meanings, examples, theme defaults, concept titles - and the rules a chart migrated to cards follows: full
  sentence descriptions, a meaning for every value of a short list (a long list's other values under one meaning),
  a meaning for every form of an option taking several, a default for every optional option, an example for every
  option. The switches of `test-util` - `migrated` (chart kinds), `migrated-builders` and `migrated-functions` (data
  functions) - list the owners whose pages show option cards."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [clojure.walk :as walk]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [fastmath.complex :as cx]
            [fastmath.vector :as v]
            [malli.core :as malli]
            [cljplot2.chart :as chart]
            [cljplot2.data.contour]
            [cljplot2.data.field]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy]
            [cljplot2.data.series]
            [cljplot2.data.sets]
            [cljplot2.data.vector-field]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.tree :as tree]
            [fastmath.ml.clustering :as cl]
            [cljplot2.chart.extent :as extent-chart]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.core :as plot]
            [cljplot2.test-util :refer [error-data migrated migrated-builders migrated-functions migrated-groups same-pixels?]]
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
  "The option rows of `rows` without rows of their own below them: a container row holding nested rows is left out, a
  map option without nested rows (e.g. a style map merged over a theme style) stays."
  [rows]
  (remove (fn [{:keys [path] :as row}]
            (and (describe/container-row? row) (some #(= path (pop (:path %))) rows)))
          rows))

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

(declare function-fixtures group-fixtures)

(defn- group-section
  "The source of the Shared options section headed `### title`, up to the next `##` or `###` heading (its `####`
  slices included); `nil` when the page has none."
  [title]
  (second (re-find (re-pattern (str "(?ms)^;; ### " (java.util.regex.Pattern/quote title) "$(.*?)(?=^;; ###? |\\z)"))
                   (slurp "notebooks/reference/shared_options.clj"))))

(deftest migration-switch-test
  (testing "the migrated kinds and builders are exactly those with a concept title (kinds keywords, builders symbols)"
    (is (= migrated (set (filter keyword? (keys describe/concepts)))))
    (is (= migrated-builders (set (filter simple-symbol? (keys describe/concepts))))))
  (testing "every data function of a data page has a concept title (qualified symbols); fonts belong to Labels and text"
    (is (= (disj (set (map symbol (describe/api-vars))) 'cljplot2.graphics.font/resolve-font)
           (set (filter qualified-symbol? (keys describe/concepts))))))
  (testing "every chart kind is migrated, so a new chart can't miss its cards"
    (is (= migrated (set (keys (describe/chart-constructors))))))
  (doseq [kind migrated
          :let [section (concept-section (describe/concepts kind))]]
    (testing (str kind ": its section shows option cards and - for a chart with mapping keys - a mapping list, no options table")
      (is (some? section))
      (is (str/includes? (str section) "nu/option-cards"))
      (is (= (empty? (describe/mapping ((get (describe/chart-constructors) kind))))
             (not (str/includes? (str section) "nu/mapping-list"))))
      (is (not (str/includes? (str section) "nu/options-table"))))
    (testing (str kind ": its shared groups (when it takes any) open the section, before the mapping list and cards")
      (let [text (str section)
            at (fn [s] (str/index-of text s))
            groups (seq (describe/fragment-keys (describe/options ((get (describe/chart-constructors) kind)))))]
        (is (= (nil? groups) (nil? (at "nu/shared-groups"))))
        (when groups
          (is (every? #(< (at "nu/shared-groups") %) (keep at ["nu/mapping-list" "nu/option-cards"])))))))
  (doseq [sym migrated-builders
          :let [section (str (concept-section (describe/concepts sym)))
                at (fn [s] (str/index-of section s))]]
    (testing (str sym ": its section shows option cards, no options table, its shared groups first")
      (is (str/includes? section "nu/option-cards"))
      (is (not (str/includes? section "nu/options-table")))
      (is (some? (at "nu/shared-groups")))
      (is (and (at "nu/shared-groups") (at "nu/option-cards") (< (at "nu/shared-groups") (at "nu/option-cards"))))))
  (testing "every user-facing function taking options is migrated and has a fixture call"
    (is (= migrated-functions (set (keep #(when (seq (describe/options %)) (symbol %)) (describe/api-vars)))))
    (is (= migrated-functions (set (keys function-fixtures)))))
  (doseq [sym migrated-functions
          :let [section (str (concept-section (describe/concepts sym)))]]
    (testing (str sym ": its section (headed by its concept) shows its own option cards, no options table")
      (is (str/includes? section "nu/option-cards"))
      (is (re-find (re-pattern (str "\\(describe/options #'[\\w.-]+/" (java.util.regex.Pattern/quote (name sym)) "\\)")) section)
          "the cards are the function's own")
      (is (not (str/includes? section "nu/options-table")))))
  (doseq [[sym v] (concat (map #(vector % (ns-resolve 'cljplot2.core %)) migrated-builders) (map #(vector % (resolve %)) migrated-functions))
          :let [section (str (concept-section (describe/concepts sym)))
                [_ placeholders] (re-find #"\(nu/option-cards '\([^\s)]+ ?([^)]*)\)" section)]]
    (testing (str sym ": its card calls name their arguments by the function's parameters (placeholders)")
      (is (some? placeholders))
      (is (every? (set (map (comp str :name) (:cljplot2/arguments (meta v))))
                  (filter #(re-matches #"[a-z][\w-]*" %) (str/split (str placeholders) #"\s+"))))))
  (testing "every shared group is migrated or owned by a data function (documented in its section), and has a fixture"
    (is (= (set (keys describe/fragments)) (into migrated-groups (keep (fn [[k {:keys [owner]}]] (when owner k))) describe/fragments)))
    (is (empty? (filter #(:owner (describe/fragments %)) migrated-groups)))
    (is (= migrated-groups (set (keys group-fixtures))))
    (is (every? #(contains? (set (map symbol (describe/api-vars))) (:owner %)) (filter :owner (vals describe/fragments)))))
  (doseq [g migrated-groups
          :let [section (str (group-section (:title (describe/fragments g))))]]
    (testing (str g ": its Shared options section shows option cards of the group through its tested representative, no options table")
      (is (str/includes? section (str "(describe/fragment-rows " g ")")))
      (is (str/includes? section "nu/option-cards"))
      (let [via (:via (group-fixtures g))]
        (is (str/includes? section (if (symbol? via) (str "(nu/option-cards '(plot/" via " ") (str "(nu/option-cards 'chart/" (name via) " ")))
            (str "the cards print the call of " via)))
      (is (not (str/includes? section "nu/options-table")))))
  (doseq [[g {:keys [owner]}] describe/fragments :when owner]
    (testing (str g ": its owner is a migrated data function taking the group")
      (is (contains? migrated-functions owner))
      (is (some #(= g (first %)) (describe/fragment-keys (describe/options (resolve owner)))))))
  (testing "boundary: a group section ends at the next ## or ### heading, not at a #### slice"
    (is (str/includes? (str (group-section "Strip options")) "#### Counts"))
    (is (not (str/includes? (str (group-section "Strip options")) "### Mirrored strips")))
    (is (nil? (group-section "No such group"))))
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
    (is (= "integer, odd" (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} [:and :int [:fn {:error/message "should be odd"} odd?]]]])))))))
    (testing "boundary: :min / :max properties of :int and :double read as the comparator bounds; none - the bare type"
      (are [s text] (= text (:type (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} s]]))))))
        [:int {:min 2}] "integer at least 2"
        [:int {:max 5}] "integer at most 5"
        [:double {:min 0 :max 1}] "number at least 0 and at most 1"
        [:int {:min 0}] "integer at least 0"
        :int "integer")))
  (testing "rules of with-rules aren't card forms: the shape stays; a predicate with a message stays; several kept parts stay an :and"
    (let [form (fn [s] (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} s]])))))]
      (is (= {:form :tuple :type "pair of finite numbers"}
             (select-keys (form (schema/with-rules [:tuple schema/finite schema/finite] [(constantly nil) nil])) [:form :type])))
      (is (= "integer above 0" (:type (form (schema/with-rules [:and :int [:> 0]] [(constantly nil) nil] [(constantly nil) nil])))))
      (is (= "integer, odd" (:type (form (schema/with-rules [:and :int [:fn {:error/message "should be odd"} odd?]] [(constantly nil) nil])))))
      (is (= [:a :b] (map :value (:values (first (describe/schema-rows [:map [:k {:description "k"} (schema/with-rules [:enum :a :b] [(constantly nil) nil])]]))))))
      (is (= "pair of finite numbers"
             (:type (form (schema/with-rules (schema/with-rules [:tuple schema/finite schema/finite] [(constantly nil) nil]) [(constantly nil) nil]))))
          "rules over rules")
      (is (= "pair of finite numbers" (:type (form [:maybe (schema/with-rules [:tuple schema/finite schema/finite] [(constantly nil) nil])])))
          "rules under :maybe")
      (is (= "vector of (pair of finite numbers)" (:type (form [:vector (schema/with-rules [:tuple schema/finite schema/finite] [(constantly nil) nil])])))
          "rules on a vector item")
      (is (schema/rule? (last (schema/with-rules :int [(constantly nil) nil]))))
      (is (not (schema/rule? [:fn {:error/message "x"} odd?])))))
  (testing "a forbidden :multi branch (a form the option rejects) is no card form; a value-set branch gives values"
    (let [bins-row (first (filter #(= [:bins] (:path %)) (describe/options (chart/histogram))))]
      (is (= ["breaks" "positive integer"] (map :type (:forms bins-row))))
      (is (= levels/bin-rules (map :value (:values bins-row))))))
  (testing "boundary: the validation-text guard"
    (is (every? validation-text? ["a finite number" "pair of (integer)s" "vector of `:x`s" "keyword or nil"]))
    (is (not-any? validation-text? ["vector of open fractions" "map of `:x` or `:y` to padding" "number above 0 and below 90"]))))

(defn- default-gaps
  "Paths of optional `rows` without a default value, a default text or a theme default."
  [rows]
  (vec (for [{:keys [path required default default-doc theme-default]} rows
             :when (and (not required) (nil? default) (nil? default-doc) (not theme-default))]
         path)))

(deftest example-call-test
  (let [example-call #'notebook-utils/example-call
        rows (describe/options (chart/line))
        row (first (filter #(= [:position] (:path %)) rows))]
    (testing "a chart's card example is (call {...}); a builder's or function's takes its arguments before the options"
      (is (= "(chart/line {:position :stack})" (example-call 'chart/line rows row)))
      (is (= "(plot/network edges {:position :stack})" (example-call '(plot/network edges) rows row)))
      (is (= "(plot/bundle calls tree {:position :stack})" (example-call '(plot/bundle calls tree) rows row))))
    (testing "boundary: no example - no call"
      (is (nil? (example-call 'chart/line [] {:path [:x]}))))))

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

(declare owners without-examples)

(defn- evaluated
  "`options` with each code form (a list, e.g. `(fn [x] (m/sin x))`) replaced by its value, evaluated whole in this
  namespace."
  [options]
  (walk/prewalk #(if (seq? %) (binding [*ns* (the-ns 'cljplot2.option-cards-test)] (eval %)) %) options))

(deftest card-rules-test
  (doseq [{kind :id :keys [options mapping schema]} (owners)
          :let [leaves (leaf-rows options)]]
    (testing (str kind ": descriptions are one or two sentences")
      (is (= [] (sentence-breaks (concat options mapping)))))
    (testing (str kind ": default texts are sentences")
      (is (= [] (sentence-breaks (map #(assoc % :description (:default-doc %)) (filter :default-doc options))))))
    (testing (str kind ": values and forms have meanings")
      (is (= [] (meaning-gaps leaves))))
    (testing (str kind ": every optional option has a default")
      (is (= [] (default-gaps leaves))))
    (testing (str kind ": every form names a documented type or reads as a short type text (no validation-message text)")
      (is (= [] (vec (for [{:keys [path forms]} options
                           {:keys [form type]} forms
                           :when (or (and (qualified-keyword? form) (not (contains? describe/documented-types form)))
                                     (validation-text? type))]
                       [path type])))))
    (testing (str kind ": every option has an example (but those of without-examples)")
      (is (= [] (vec (keep #(when-not (or (describe/example %) (without-examples [kind (:path %)])) (:path %)) leaves)))))
    (testing (str kind ": every example is a valid option on its own (a code form evaluated first)")
      (doseq [row options
              :let [example (evaluated (describe/example-options options row))]
              :when example]
        (is (malli/validate schema example) (str (:path row) " " example))))))

(def ^:private correlated
  "Rows of two related variables, the fixture of the statistic cells."
  (map (fn [i] {:x i :y (m/+ i (m/* 4.0 (m/sin (m/* 1.7 i))))}) (range 20)))

(def ^:private per-level
  "Rows of two levels, 12 and 7 spread values (unequal counts) and an outlier - the fixture of the distribution per level
  charts."
  (for [l ["a" "b"] i (range (if (= l "a") 12 7))]
    {:l l :v (m/+ (m/* 0.4 i) (m/sin (m/* 2.3 i)) (if (= l "a") 0.0 2.0) (if (m/== i 11) 6.0 0.0))}))

(def ^:private one-sample
  "Rows of one numeric sample in two groups (`:g`) and two ridgeline levels (`:level`), 15 and 10 skewed values, plus a
  row without a value - the fixture of the distribution of one variable charts."
  (conj (vec (for [g ["a" "b"] i (range (if (= g "a") 15 10))]
               {:g g :level (if (even? i) "p" "q") :v (m/+ (m/* 0.05 i i) (m/sin (m/* 1.9 i)) (if (= g "a") 0.0 1.5))}))
        {:g "a" :level "p" :v nil}))

(def ^:private points-2d
  "Rows of 60 points of two correlated variables with a second cluster - the fixture of the 2D bins and fields."
  (for [i (range 60)
        :let [t (m/* 0.37 i) cluster (if (m/< i 40) 0.0 4.0)]]
    {:x (m/+ cluster (m/* 2.0 (m/sin t)) (m/* 0.05 i)) :y (m/+ cluster (m/* 1.5 (m/cos (m/* 1.3 t))) (m/* 0.04 i))}))

(def ^:private lattice
  "Rows of a 5 x 4 lattice with a value per node - the fixture of tiles and gridded surfaces."
  (for [x (range 5) y (range 4)] {:x x :y y :v (m/+ (m/* 0.5 x) (m/sin (m/* 1.1 y)))}))

(def ^:private wide-rows
  "Wide rows of three variables (the names of the parallel chart's option examples) and a row with a missing value - the
  fixture of parallel coordinates."
  (conj (vec (for [i (range 6)] {:mpg (m/+ 20 i (m/* 3.0 (m/sin i))) :hp (m/+ 100 (m/* 9.0 i i)) :wt (m/- 5.0 (m/* 0.4 i))}))
        {:mpg 31 :hp 150 :wt nil}))

(def ^:private points-3d
  "Rows of 12 points in 3D - the fixture of clouds."
  (for [i (range 12)] {:x (m/sin (m/* 0.6 i)) :y (m/cos (m/* 0.4 i)) :z (m/* 0.25 i)}))

(def ^:private tree-rows
  "Rows of a two-level tree with values and a colour, in data order other than value order - the fixture of the
  hierarchy charts."
  [{:g "a" :n "a1" :v 1 :c "x"} {:g "a" :n "a2" :v 3 :c "y"} {:g "a" :n "a3" :v 5 :c "x"}
   {:g "b" :n "b1" :v 2 :c "y"} {:g "b" :n "b2" :v 4 :c "x"} {:g "c" :n "c1" :v 12 :c "y"}])

(def ^:private categories
  "Rows of three weighted categories - the fixture of waffles."
  [{:k "p" :w 37} {:k "q" :w 21} {:k "r" :w 12} {:k "p" :w 5}])

(def ^:private set-items
  "Rows of items in three overlapping sets - the fixture of Venn diagrams."
  (for [[s items] {"a" (range 0 12) "b" (range 6 16) "c" (range 10 20)} i items] {:s s :i i}))

(def ^:private example-fixtures
  "Kind -> a small plot (`:data`, `:mapping`, `:scales`, and `:options` under every example, e.g. a seeded `:rng`) on
  which every option example of the kind changes the picture:
  unsorted x, two groups, a missing y, up and down candles, bars of two groups below 0, two bullets, two related
  variables (statistic cells), two levels of spread values with an outlier (distribution per level), one sample in two groups
  with a missing value (distribution of one variable; Q-Q and P-P of the sample against itself); annotation charts draw from their examples' options, on
  fixed scales where an example adds no extent (rule, text, function)."
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
              :mapping {:x :x :y :y :color :g}}
     :rule {:scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
     :rect {}
     :text {:scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
     :image {}
     :function {:scales {:x {:domain [0 6]}}}
     :bar {:data [{:k "a" :v 3 :g "p"} {:k "a" :v 1 :g "p"} {:k "a" :v 2 :g "q"} {:k "b" :v 5 :g "p"} {:k "b" :v -2 :g "q"}
                  {:k "c" :v 4 :g "q"} {:k "d" :v -1 :g "p"} {:k "d" :v -2 :g "q"}]
           :mapping {:x :k :y :v :group :g}}
     :bullet {:data [{:k "revenue" :r [60 80 100] :m [92 75] :t [95]} {:k "costs" :r [50 75 100] :m [70 60] :t [80]}]
              :mapping {:y :k :ranges :r :measures :m :markers :t}}
     :box {:data per-level :mapping {:x :v :y :l}}
     :violin {:data per-level :mapping {:x :v :y :l}}
     :extent {:data per-level :mapping {:x :v :y :l}}
     :stripplot {:data per-level :mapping {:x :v :y :l} :options {:rng (r/rng :mersenne 3)}}
     :beeswarm {:data per-level :mapping {:x :v :y :l}}
     :dotplot {:data per-level :mapping {:x :v :y :l}}
     :rug {:data per-level :mapping {:x :v :y :l}}
     :histogram {:data one-sample :mapping {:x :v :group :g}}
     :density {:data one-sample :mapping {:x :v :group :g}}
     :ecdf {:data one-sample :mapping {:x :v}}
     :qq {:data one-sample :mapping {:x :v :y :v}}
     :pp {:data one-sample :mapping {:x :v :y :v}}
     :tile {:data lattice :mapping {:x :x :y :y :color :v}}
     :bin2d {:data points-2d :mapping {:x :x :y :y}}
     :field {:data points-2d :mapping {:x :x :y :y}}
     :contour {:data points-2d :mapping {:x :x :y :y}}
     :parallel {:data wide-rows :mapping {} :options {:variables [:mpg :hp :wt]}}
     :cloud {:data points-3d :mapping {:x :x :y :y :z :z}}
     :surface {:data lattice :mapping {:x :x :y :y :z :v}}
     :treemap {:data tree-rows :mapping {:value :v :hierarchy [:g :n]}}
     :partition {:data tree-rows :mapping {:value :v :hierarchy [:g :n]}}
     :pack {:data tree-rows :mapping {:value :v :hierarchy [:g :n]}}
     :waffle {:data categories :mapping {:color :k :value :w}}
     :venn {:data set-items :mapping {:set :s :item :i}}
     :correlation {:data correlated :mapping {:x :x :y :y}}
     :pairwise {:data correlated :mapping {:x :x :y :y}}}))

(def ^:private edges
  "Weighted edge rows of four nodes, one node without an edge out - the fixture of the graph builders."
  [{:from "a" :to "b" :value 3} {:from "a" :to "c" :value 1} {:from "b" :to "c" :value 2} {:from "c" :to "d" :value 4}
   {:from "b" :to "d" :value 1}])

(def ^:private place-rows
  "Rows of a three-level hierarchy of places - the source of the fixture tree."
  [{:region "east" :country "p" :city "p1"} {:region "east" :country "p" :city "p2"} {:region "east" :country "q" :city "q1"}
   {:region "west" :country "r" :city "r1"} {:region "west" :country "r" :city "r2"} {:region "west" :country "s" :city "s1"}])

(def ^:private place-tree
  "The fixture tree of the tree builders: regions, countries, cities."
  (tree/from-levels place-rows {:hierarchy [:region :country :city]}))

(def ^:private three-observations
  "An `hclust` result of three observations - the fixture of the options naming a clustering's leaves."
  (cl/hclust [[0.0 0.0] [1.0 0.0] [5.0 5.0]]))

(def ^:private builder-fixtures
  "Builder symbol -> its fixture call: `:args`, the arguments before the options, `:per-option` (an option path -> its
  own `:args`, for an option a fixture of another source acts on) and `:options` under every example
  (a seeded `:rng`; required options come with every example, see the builders' example contexts): small edges, wide rows, a matrix in long form, a tree, sequences and series on
  which every option example of the builder changes the picture."
  {'network {:args [edges] :options {:rng (r/rng :mersenne 1)}}
   'chord {:args [edges]}
   'arc {:args [edges]}
   'sankey {:args [edges]}
   'alluvial {:args [[{:class "first" :sex "f" :n 5} {:class "first" :sex "m" :n 2} {:class "crew" :sex "f" :n 3} {:class "crew" :sex "m" :n 4}]]}
   'dendrogram {:args [place-tree] :per-option {[:leaf-names] {:args [three-observations]}}}
   'tree {:args [place-tree] :per-option {[:leaf-names] {:args [three-observations]}}}
   'clustered-heatmap {:args [(for [[i car] (map-indexed vector ["p" "q" "r" "s" "t"]) [j variable] (map-indexed vector ["mpg" "hp" "wt"])]
                                {:car car :variable variable :origin (if (m/< i 2) "eu" "us") :unit (if (m/zero? j) "mi" "x")
                                 :score (m/sin (m/+ (m/* 1.3 j) (m/* 0.7 i i)))})]}
   'bundle {:args [[{:from "p1" :to "r1"} {:from "p2" :to "s1"} {:from "q1" :to "r2"} {:from "p1" :to "q1"}] place-tree]}
   'sequences {:args [[{:sequence "a-b-end" :n 3} {:sequence "a-c-end" :n 2} {:sequence "b-end" :n 2} {:sequence "a-b-c-end" :n 1}]]
               :options {:value :n}}
   'acf {:args [(mapv (fn [i] (m/+ (m/sin (m/* 0.5 i)) (m/* 0.3 (m/cos (m/* 2.3 i))))) (range 60))]}
   'horizon {:args [(for [s ["a" "b"] t (range 30)] {:t t :v (m/* (if (= s "a") 1.0 -0.6) (m/sin (m/* 0.3 t))) :series s})]}})

;; data function fixtures (placeholders of the pages' card calls: rows, edges, g, root, laid, values, f, seeds)

(def ^:private wave-rows
  "100 rows of a wave `:x` - the fixture of the binning functions."
  (mapv (fn [i] {:x (m/sin (m/* 1.7 (double i)))}) (range 100)))

(def ^:private tied-rows
  "Rows of two x values, one tie each - the fixture of ranking and level ordering."
  [{:x 1 :level "a" :value 3} {:x 1 :level "b" :value 5} {:x 1 :level "c" :value 5}
   {:x 2 :level "a" :value 9} {:x 2 :level "b" :value 1} {:x 2 :level "c" :value 4}])

(def ^:private named-edges
  "The fixture [[edges]] with other end and weight columns (`:source`, `:target`, `:weight`) - examples naming them change
  the graph."
  (mapv (fn [{:keys [from to value]}] {:from from :to to :value value :source to :target from :weight (m/* 2 (long value))}) edges))

(def ^:private source-edges
  "The fixture [[edges]] and a second source `e`, whose nearest target `d` is three columns on."
  (conj edges {:from "e" :to "d" :value 2}))

(def ^:private fixture-graph
  "The directed graph of [[source-edges]] with node rows giving a column per node (`:col`), `e` first."
  (graph/graph source-edges {:directed true :nodes [{:id "e" :col 1} {:id "a" :col 0} {:id "b" :col 2} {:id "c" :col 3} {:id "d" :col 4}]}))

(def ^:private crossing-graph
  "A directed graph of two sources and two targets whose heavy flows cross in input order - relaxation uncrosses them."
  (graph/graph [{:from "s1" :to "t2" :value 5} {:from "s2" :to "t1" :value 5} {:from "s1" :to "t1" :value 1}]
               {:directed true :nodes [{:id "s1"} {:id "s2"} {:id "t1"} {:id "t2"}]}))

(def ^:private wide-rows-fixture
  "Wide rows of three categorical variables and a weight - the fixture of the alluvial graph."
  [{:class "first" :sex "f" :age "adult" :n 5} {:class "first" :sex "m" :age "child" :n 2}
   {:class "crew" :sex "f" :age "adult" :n 3} {:class "crew" :sex "m" :age "adult" :n 4}])

(def ^:private sequence-rows
  "Sequence rows - strings, steps under `:path`, weights - the fixture of the sequence levels."
  [{:sequence "a-b-end" :path ["b" "a"] :n 3} {:sequence "a-c-end" :path ["c"] :n 2}
   {:sequence "b-end" :path ["a" "b"] :n 2} {:sequence "a-b-c" :path ["a"] :n 1}])

(def ^:private series-values
  "60 values of a damped wave - the fixture of the autocorrelation rows."
  (mapv (fn [i] (m/+ (m/sin (m/* 0.5 (double i))) (m/* 0.3 (m/cos (m/* 2.3 (double i)))))) (range 60)))

(def ^:private horizon-fixture
  "Rows of two series under `:s`, with second x and y columns (`:t`, `:v`) - the fixture of the horizon rows; the
  series' x values differ, so the rows are one series with distinct x too."
  (for [s ["a" "b"] t (range 30)
        :let [x (if (= s "a") (double t) (m/+ 0.5 (double t)))
              y (m/* (if (= s "a") 1.0 -0.6) (m/sin (m/* 0.3 x)))]]
    {:x x :y y :t (m/* 2.0 x) :v (m/* 0.5 y) :s s}))

(def ^:private parent-rows
  "Rows linked by parent ids, with a second parent column (`:up`), leaf keys (`:key`) and names (`:label`)."
  [{:id "r" :key "r" :label "root"} {:id "a" :parent "r" :up "r" :key "a" :label "A"}
   {:id "b" :parent "a" :up "r" :key "z" :label "B"} {:id "c" :parent "a" :up "a" :key "c" :label "C"}])

(def ^:private bundle-edges
  "Edges between the fixture tree's leaves, with swapped end columns (`:source`, `:target`)."
  [{:from "p1" :to "r1" :source "p2" :target "s1"} {:from "p2" :to "s1" :source "q1" :target "r2"}
   {:from "q1" :to "r2" :source "p1" :target "r1"}])

(defn- rotation-field
  "A vector field turning around the origin and growing outwards."
  [[^double x ^double y]]
  (v/vec2 (m/- (m/* 0.3 x) y) (m/+ x (m/* 0.3 y))))

(def ^:private colour-points
  "Points sampled to compare domain colourings."
  (for [x [-1.5 -0.4 0.3 1.2] y [-1.1 0.2 0.9]] [x y]))

(def ^:private function-fixtures
  "Data function (qualified symbol) -> its fixture call: `:args`, the arguments before the options (named as the
  placeholders of its card calls), `:per-option` (an option path -> its own `:args`), `:options` under every example and
  `:view` - a function of the result giving the compared value (a returned function sampled), else the result itself."
  {'cljplot2.data.levels/bin {:args [wave-rows :x]}
   'cljplot2.data.levels/order-levels {:args [tied-rows :level :value stats/mean]}
   'cljplot2.data.levels/rank-rows {:args [tied-rows :x :value]}
   'cljplot2.data.shingle/shingle {:args [wave-rows :x]}
   'cljplot2.data.pairwise/matrix-rows {:args [(map (fn [i] {:a (double i) :b (m/sin (double i)) :c (m/* (double i) (double i))}) (range 20))]}
   'cljplot2.data.graph/graph {:args [named-edges]}
   'cljplot2.data.graph/layout {:args [(graph/graph edges)] :options {:rng (r/rng :mersenne 1)}}
   'cljplot2.data.graph/adjacency->edges {:args [[[0 1 2] [3 0 1] [2 1 0]] ["a" "b" "c"]]}
   'cljplot2.data.graph/arc-layout {:args [fixture-graph]}
   'cljplot2.data.graph/chord-layout {:args [fixture-graph]}
   'cljplot2.data.graph/sankey-layout {:args [fixture-graph] :per-option {[:order] {:args [crossing-graph]}}}
   'cljplot2.data.graph/alluvial-graph {:args [wide-rows-fixture]}
   'cljplot2.data.hierarchy/sequence-levels {:args [sequence-rows]}
   'cljplot2.data.series/acf-rows {:args [series-values]}
   'cljplot2.data.series/horizon-rows {:args [horizon-fixture]}
   'cljplot2.data.tree/from-levels {:args [place-rows]}
   'cljplot2.data.tree/from-parents {:args [parent-rows]}
   'cljplot2.data.tree/layout {:args [place-tree]}
   'cljplot2.data.tree/node-link-layout {:args [place-tree]}
   'cljplot2.data.tree/bundle-paths {:args [bundle-edges (tree/layout place-tree {:circular true})]}
   'cljplot2.data.vector-field/arrows {:args [rotation-field]}
   'cljplot2.data.vector-field/traces {:args [rotation-field [[0.5 0.0] [0.0 -0.8]]]}
   'cljplot2.data.field/domain-color {:args [cx/sq] :view (fn [color-of] (mapv color-of colour-points))}})

(defn- chart-owner
  "The card owner of chart `kind` (see [[owners]])."
  [kind]
  (let [constructor (get (describe/chart-constructors) kind)
        {:keys [data mapping scales] base :options} (example-fixtures kind)
        rows (describe/options (constructor))]
    {:id kind :rows rows :options (describe/own-rows rows) :mapping (describe/mapping (constructor))
     :schema (get-in (constructor) [:schema :options]) :base base
     :draw (fn [options] (cond-> {:data data :mapping mapping :charts [(constructor (merge base options))]} scales (assoc :scales scales)))}))

(defn- builder-owner
  "The card owner of builder `sym` of `cljplot2.core` (see [[owners]])."
  [sym]
  (let [v (ns-resolve 'cljplot2.core sym)
        {:keys [args per-option] base :options} (builder-fixtures sym)
        rows (describe/options v)
        call (fn [call-args] (fn [options] (apply @v (conj (vec call-args) (merge base options)))))]
    {:id sym :rows rows :options (describe/own-rows rows) :mapping [] :schema (:cljplot2/options (meta v)) :base base
     :draw (call args) :draws (update-vals per-option #(call (:args %)))}))

(def ^:private group-fixtures
  "Shared group -> its representative's fixture: `:via` - a chart kind (its fixture plot of [[example-fixtures]], with
  `:data`, `:mapping`, `:scales`, `:before` (charts drawn under it) and `:options` given here winning) or a builder
  symbol of `cljplot2.core` (its fixture call `:args`), `:per-option` - option path -> fixture overrides; the group's
  rows act on it."
  {:annotation {:via :rule :data [{:x 1 :y 1} {:x 3 :y 2}] :mapping {:x :x :y :y} :scales nil :before [(chart/scatter)] :options {:y [5]}}
   :arrange {:via 'arrange :args [[{:data [{:x 1 :y 12000} {:x 2 :y 18000}] :mapping {:x :x :y :y} :charts [(chart/scatter)]}
                                   {:data [{:x 1 :y 2 :g "p"} {:x 2 :y 3 :g "q"}] :mapping {:x :x :y :y :color :g} :charts [(chart/line)]}]]}
   :builder {:via 'network :args [edges] :options {:rng (r/rng :mersenne 1)}}
   :cell {:via :tile}
   :common {:via :line}
   :corner-radius {:via :bar}
   :curve {:via :line :per-option {[:interpolator-params] {:data (for [x (range 20)] {:x x :y (m/sin (m/* 0.7 x))}) :mapping {:x :x :y :y}}}}
   :field {:via :field
           :per-option {[:interpolation] {:data lattice :mapping {:x :x :y :y :value :v}}
                        [:n] {:data lattice :mapping {:x :x :y :y :value :v}}}}
   :frame-3d {:via :surface}
   :hierarchy-labels {:via :treemap}
   :line-style {:via :line :per-option {[:color] {:mapping {:x :x :y :y}} [:dash] {:mapping {:x :x :y :y}}}}
   :mirror {:via :bar}
   :pattern {:via :bar}
   :ridge {:via :density}
   :shading {:via :surface}
   :strip {:via :box :data (map-indexed (fn [i row] (assoc row :g (if (even? i) "p" "q"))) per-level)
           :per-option (into {} (for [path [[:place] [:thickness] [:bottom-share]]]
                                  [path {:data points-2d :mapping {:x :x :y :y} :before [(chart/scatter)] :options {:mapping {:y nil}}}]))}
   :text-style {:via :text :options {:x [5] :y [5] :text ["label"]}}
   :theme-style {:via :scatter}})

(def ^:private without-examples
  "`[owner path]` -> why the option has no example on its card (and so no example test)."
  {[:group/arrange [:plots]] "`plot/arrange` takes the plots as its first argument; a plot's `:arrange` map holds them"
   [:group/text-style [:align]] "the text chart, the group's representative, takes no `:align`; label maps do (Labels and text)"
   [:group/strip [:level-axis]] "it decides only where both axes or neither are categorical, which no drawable strip fixture has: values need a numeric axis"})

(defn- group-owner
  "The card owner of shared group `fragment` (see [[owners]]), its id `:group/<name>` (distinct from a chart kind): its
  rows (`cljplot2.describe/fragment-rows`, as the Shared options page shows them) drawn by the representative of
  [[group-fixtures]]."
  [fragment]
  (let [{:keys [via args per-option] fixture-options :options :as fixture} (group-fixtures fragment)
        rows (describe/fragment-rows fragment)]
    (if (symbol? via)
      (let [v (ns-resolve 'cljplot2.core via)
            call (fn [call-args] (fn [options] (apply @v (conj (vec call-args) (merge fixture-options options)))))]
        {:id (keyword "group" (name fragment)) :rows rows :options rows :mapping [] :schema (:cljplot2/options (meta v)) :base fixture-options
         :draw (call args) :draws (update-vals per-option #(call (:args %)))})
      (let [constructor (get (describe/chart-constructors) via)
            kind-fixture (example-fixtures via)
            base (merge (:options kind-fixture) fixture-options)
            plot (fn [over] (fn [options]
                              (let [{:keys [data mapping scales before]} (merge kind-fixture (dissoc fixture :per-option :options) over)]
                                (cond-> {:mapping mapping :charts (conj (vec before) (constructor (merge base (:options over) options)))}
                                  data (assoc :data data)
                                  scales (assoc :scales scales)))))]
        {:id (keyword "group" (name fragment)) :rows rows :options rows :mapping [] :schema (get-in (constructor) [:schema :options]) :base base
         :draw (plot {}) :draws (update-vals per-option plot)}))))

(defn- function-owner
  "The card owner of data function `sym` (a qualified symbol, see [[owners]]): every option row is its own (a group's
  rows too - the function owns them), its draw function returns the compared value (`:compare :value`)."
  [sym]
  (let [v (resolve sym)
        {:keys [args per-option view] base :options :or {view identity}} (function-fixtures sym)
        rows (describe/options v)
        call (fn [call-args] (fn [options] (view (apply @v (conj (vec call-args) (merge base options))))))]
    {:id sym :rows rows :options rows :mapping [] :schema (:cljplot2/options (meta v)) :base base :compare :value
     :draw (call args) :draws (update-vals per-option #(call (:args %)))}))

(defn- owners
  "Every migrated chart kind, builder and data function as a card owner: `:id` (the kind or the builder's symbol), `:rows` (every option
  row), `:options` (its own rows), `:mapping` (mapping rows, none for a builder or a data function), `:schema` (its options schema), `:base`
  (the fixture's base options), `:draw` (a function of options - the fixture's base options merged under them -
  returning the fixture plot - a data function's: its compared value, `:compare :value`) and `:draws` (option path ->
  its own draw function, for an option acting on another fixture)."
  []
  (concat (map chart-owner (sort migrated)) (map builder-owner (sort migrated-builders))
          (map function-owner (sort migrated-functions)) (map group-owner (sort migrated-groups))))

(defn- without-option
  "`options` without the option at `path`: removed, or - nested in a container - back at the container's map default
  (the theme's) or its example context's value, so a required key stays."
  [options rows path]
  (let [container (first (filter #(= (pop path) (:path %)) rows))
        default (or (when (map? (:default container)) (get (:default container) (peek path)))
                    (get-in (:example-context container) path))]
    (cond
      (some? default) (assoc-in options path default)
      (m/one? (count path)) (dissoc options (first path))
      :else (update-in options (pop path) dissoc (peek path)))))

(def ^:private inert-in-fixtures
  "`[kind path]` -> why the option's example acts only beyond a chart's own options (a plot scale or coord): no pixels
  change in the fixture, or the example needs a plot the fixture isn't."
  {[:smooth [:stat-space]] "acts only on a :log axis, a plot scale"
   [:scatter [:descending]] "acts only where points of different colours overlap"
   [:bar [:label-leader]] "its example (:outer labels) throws outside a polar plot, a plot :coord"
   [:histogram [:stat-space]] "acts only on a :log axis, a plot scale"
   [:density [:stat-space]] "acts only on a :log axis, a plot scale"
   [:parallel [:fill]] "fills radar polygons only: a polar plot, a plot :coord"
   [:parallel [:alpha]] "fades the radar fill only: a polar plot, a plot :coord"
})

(def ^:private needed-to-draw
  "`[id path]` -> why the chart draws nothing without the option (a data function's call fails without it): its
  example has no picture or result to compare with, so it only has to render (return). Every other baseline must draw
  (see [[drawable?]], [[callable?]])."
  {[:rule [:x]] "literal positions or a mapping make the lines" [:rule [:y]] "literal positions or a mapping make the lines"
   [:rule [:slope]] "a literal :intercept needs :slope" [:rule [:intercept]] "a literal :slope needs :intercept"
   [:text [:x]] "literal labels need :x, :y and :text" [:text [:y]] "literal labels need :x, :y and :text"
   [:text [:text]] "literal labels need :x, :y and :text"
   [:image [:f]] "the chart's one required option"
   [:function [:f]] "a function chart needs :f or :parametric" [:function [:parametric]] "a function chart needs :f or :parametric"
   [:function [:t]] ":parametric needs :t"
   [:box [:stat]] "its example's own data holds statistics, not values: :tukey has nothing to compute"
   [:pp [:domain]] "two distribution sides need :domain"
   [:surface [:fu]] "its :u and :v context is valid only with :fu"
   [:surface [:u]] "a parametric :fu needs both :u and :v" [:surface [:v]] "a parametric :fu needs both :u and :v"
   ['clustered-heatmap [:row]] "a required option" ['clustered-heatmap [:col]] "a required option"
   ['clustered-heatmap [:value]] "a required option"
   ['cljplot2.data.levels/bin [:as]] "a required option" ['cljplot2.data.shingle/shingle [:as]] "a required option"
   ['cljplot2.data.graph/alluvial-graph [:variables]] "a required option"
   ['cljplot2.data.pairwise/matrix-rows [:variables]] "a matrix needs :variables or both :x-variables and :y-variables"
   ['cljplot2.data.pairwise/matrix-rows [:x-variables]] "a rectangular matrix needs both :x-variables and :y-variables"
   ['cljplot2.data.pairwise/matrix-rows [:y-variables]] "a rectangular matrix needs both :x-variables and :y-variables"})

(defn- drawable?
  "Tests whether a fixture plot of options draws: the options valid and the plot made and rendered without an error.

  Parameters:

  - `owner` (map): a card owner (see [[owners]]) - `:schema`, `:base`.
  - `draw` (function): the owner's draw function for the option (its `:draws` entry, else `:draw`).
  - `options` (map): the options, the fixture's base options left out.
  - `size` (map): render options."
  [{:keys [schema base]} draw options size]
  (and (malli/validate schema (merge base options))
       (try (some? (plot/render-image (draw options) size)) (catch clojure.lang.ExceptionInfo _ false))))

(defn- callable?
  "Tests whether a data function's fixture call of options returns: the options valid and the call made without an
  error (see [[drawable?]] for plots)."
  [{:keys [schema base]} draw options]
  (and (malli/validate schema (merge base options))
       (try (draw options) true (catch clojure.lang.ExceptionInfo _ false))))

(deftest exemptions-test
  (let [rows-of (into {} (map (juxt :id :options)) (owners))]
    (testing "every without-examples entry names an option of its owner that has no example"
      (doseq [[[id path] reason] without-examples
              :let [row (first (filter #(= path (:path %)) (rows-of id)))]]
        (is (some? row) [id path])
        (is (nil? (describe/example row)) [id path reason])))
    (testing "every inert-in-fixtures and needed-to-draw entry names an option of its owner"
      (doseq [[id path] (concat (keys inert-in-fixtures) (keys needed-to-draw))]
        (is (some #(= path (:path %)) (rows-of id)) [id path])))))

(deftest function-owner-test
  (let [graph-owner (function-owner 'cljplot2.data.graph/graph)
        bin-owner (function-owner 'cljplot2.data.levels/bin)]
    (testing "a function owns its group's rows: graph/graph has no rows outside :graph/graph, its owner all six"
      (is (empty? (describe/own-rows (:rows graph-owner))))
      (is (= [:from :to :value :nodes :id :directed] (map :key (:options graph-owner)))))
    (testing "the owner's draw returns the call's result, the fixture's base options under the given ones"
      (is (= :value (:compare bin-owner)))
      (is (= 100 (count ((:draw bin-owner) {:as :bin})))))
    (testing "boundary: callable? - a valid call returns, an invalid option, a missing required option or a data error fails"
      (is (callable? bin-owner (:draw bin-owner) {:as :bin}))
      (is (not (callable? bin-owner (:draw bin-owner) {:as :bin :bins 0})))
      (is (not (callable? bin-owner (:draw bin-owner) {})))
      (is (not (callable? graph-owner (:draw graph-owner) {:nodes [{:code "c"}]}))))))

(deftest overlap-type-test
  (let [row (first (filter #(= [:overlap] (:path %)) (describe/options #'shingle/shingle)))]
    (testing "the overlap is a documented type: its form names it"
      (is (= ["overlap fraction"] (map :type (:forms row))))
      (is (contains? describe/documented-types ::shingle/overlap))))
  (testing "boundary: 0 and just below 1 pass; 1, a negative number and a non-number fail, at :overlap"
    (is (every? #(malli/validate shingle/overlap-schema %) [0 0.0 0.5 0.999]))
    (is (not-any? #(malli/validate shingle/overlap-schema %) [1 1.0 -0.1 ##NaN ##Inf "0.5" nil]))
    (is (= :overlap (:key (error-data #(shingle/shingle [{:x 1} {:x 2}] :x {:as :i :overlap 1})))))
    (is (seq (shingle/shingle wave-rows :x {:as :i :overlap 0.999})) "the largest overlap still cuts intervals")))

(deftest examples-act-test
  (doseq [{:keys [id rows options draws] :as owner} (owners)
          row (leaf-rows options)
          :when (not (contains? inert-in-fixtures [id (:path row)]))
          :when (not (contains? without-examples [id (:path row)]))
          :let [example (evaluated (describe/example-options rows row))
                draw (get draws (:path row) (:draw owner))]]
    (let [baseline (without-option example rows (:path row))
          size {:width 320 :height 240}]
      (cond
        (and (= :value (:compare owner)) (needed-to-draw [id (:path row)]))
        (testing (str id " " (:path row) ": needed (" (needed-to-draw [id (:path row)]) "): the call fails without it, returns with it - " (pr-str example))
          (is (not (callable? owner draw baseline)))
          (is (callable? owner draw example)))

        (= :value (:compare owner))
        (testing (str id " " (:path row) ": the example changes the result (the baseline's result the same on every call) - " (pr-str example))
          (is (callable? owner draw baseline))
          (when (callable? owner draw example)
            (let [result (draw baseline)]
              (is (= result (draw baseline)) "the result compares as a value: no NaN, function or unseeded randomness")
              (is (not= result (draw example))))))

        (needed-to-draw [id (:path row)])
        (testing (str id " " (:path row) ": needed to draw (" (needed-to-draw [id (:path row)]) "): the baseline doesn't draw, the example renders - " (pr-str example))
          (is (not (drawable? owner draw baseline size)))
          (is (some? (plot/render-image (draw example) size))))

        :else
        (testing (str id " " (:path row) ": the example changes the picture - " (pr-str example))
          (is (drawable? owner draw baseline size))
          (is (not (same-pixels? (draw baseline) (draw example) size))))))))

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

(deftest group-links-test
  (testing "every shared group's link (Shared options boxes, owned group lists) resolves to a heading of its page"
    (let [link #'notebook-utils/group-link]
      (is (= [] (vec (for [g (keys describe/fragments)
                           :let [[_ page anchor] (re-matches #"(reference\.[a-z_0-9]+)\.html#(.+)" (link g))]
                           :when (not (contains? (heading-anchors page) anchor))]
                       [g (link g)]))))))
  (testing "an owned group links to its owner's section, another to Shared options"
    (is (= "reference.data_graphs_trees.html#graphs" (#'notebook-utils/group-link :graph/graph)))
    (is (= "reference.shared_options.html#line-style" (#'notebook-utils/group-link :line-style)))))

(deftest statistic-descriptions-test
  (testing "every built-in statistic says what it measures (its card meaning, `pairwise/statistic-meanings`)"
    (is (every? #(seq (:description %)) (vals pairwise/statistics)))))

(deftest none-colours-test
  (testing "every migrated chart's and builder's colour option documenting :none (a colour form beside it) draws with it (data functions draw nothing)"
    (doseq [{:keys [id rows options draw draws compare]} (owners)
            :when (not= :value compare)
            row (leaf-rows options)
            :when (and (some #(= :none (:value %)) (:values row)) (some #(= ::schema/solid-color (:form %)) (:forms row)))
            :let [example (assoc-in (or (evaluated (describe/example-options rows row)) {}) (:path row) :none)]]
      (testing (str id " " (:path row))
        (is (some? (plot/render-image ((get draws (:path row) draw) example) {:width 160 :height 120})))))))

(deftest extent-method-descriptions-test
  (testing "every extent method says what it gives (its card meaning)"
    (is (= (set (concat (keys extent-chart/extent-methods) (keys extent-chart/bootstrap-intervals)))
           (set (keys extent-chart/method-descriptions))))
    (is (every? #(re-find #"^[A-Z`].*\.$" %) (vals extent-chart/method-descriptions)))))
