(ns cljplot2.robustness-r-test
  "Batch R (round 08) - robustness: an aesthetic without a valid value is a mapping error in cloud and the hierarchy
  charts, gridded holes are counted under the mapped value, literal texts' formulas are checked at build by path
  (aesthetic scale titles, pairwise labels, histogram invalid labels, the pairs and alluvial variable names) with the
  cause's message once."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.graphics.latex :as latex]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.content.axis :as axis]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error error-data]]))

(def ^:private cube
  "Points of a unit cube, valid in every position."
  (for [x [0 1] y [0 1] z [0 1]] {:x x :y y :z z}))

(defn- cloud-plot
  "A cloud of `rows` with the aesthetic `k` mapped to `:a`."
  [rows k]
  {:data rows :mapping {:x :x :y :y :z :z k :a} :charts [(chart/cloud)]})

(deftest cloud-aesthetics-test
  (doseq [k [:color :size :shape]]
    (testing (str k " without a valid value throws at build, at its mapping path")
      (is (= {:key :mapping :in [:mapping k]}
             (select-keys (error #(plot/build (cloud-plot (map (fn [r] (assoc r :a nil)) cube) k))) [:key :in])))
      (is (= [:mapping k] (:in (error #(plot/build (cloud-plot [{:x 0 :y 0 :z 0 :a nil}] k))))) "one row"))
    (testing (str k " with one valid value renders; no rows build")
      (is (some? (plot/render-image (cloud-plot (cons (assoc (first cube) :a 1) (map (fn [r] (assoc r :a nil)) (rest cube))) k)
                                    {:width 200 :height 200})))
      (is (map? (plot/build (assoc (cloud-plot [] k) :charts [(chart/cloud {:domain {:x [0 1] :y [0 1] :z [0 1]}})]))))))
  (testing "a NaN size and a non-colour under an identity scale are invalid too"
    (is (= [:mapping :size] (:in (error #(plot/build (cloud-plot (map (fn [r] (assoc r :a ##NaN)) cube) :size))))))
    (is (= [:mapping :color] (:in (error #(plot/build (assoc (cloud-plot (map (fn [r] (assoc r :a "no-colour")) cube) :color)
                                                             :scales {:color {:kind :identity}}))))))
    (is (map? (plot/build (assoc (cloud-plot (map (fn [r] (assoc r :a :red)) cube) :color) :scales {:color {:kind :identity}})))))
  (testing ":type :lines - a nil :group is a line, not an invalid value"
    (let [p (fn [a g] {:data (map (fn [r] (assoc r :a a :g g)) cube) :mapping {:x :x :y :y :z :z :group :g :color :a}
                       :charts [(chart/cloud {:type :lines})]})]
      (is (= [:mapping :color] (:in (error #(plot/build (p nil 1))))))
      (is (map? (plot/build (p "u" nil))))))
  (testing "an invalid position is still reported before an aesthetic"
    (is (= [:mapping :x] (:in (error #(plot/build (cloud-plot (map (fn [r] (assoc r :x nil :a nil)) cube) :color))))))))

(def ^:private budget
  "Hierarchy rows: two departments, three teams."
  [{:dept "a" :team "x" :cost 3 :c nil} {:dept "a" :team "y" :cost 2 :c nil} {:dept "b" :team "z" :cost 4 :c nil}])

(deftest hierarchy-color-test
  (doseq [make [chart/treemap chart/pack chart/partition]
          :let [p (fn [rows] {:data rows :mapping {:hierarchy [:dept :team] :value :cost :color :c} :charts [(make)]})
                kind (:kind (make))]]
    (testing (str kind ": a colour without a valid value throws at build")
      (is (= {:key :mapping :in [:mapping :color]} (select-keys (error #(plot/build (p budget))) [:key :in]))))
    (testing (str kind ": one valid colour renders; no colour mapping and no rows build")
      (is (some? (plot/render-image (p (assoc-in budget [0 :c] "red")) {:width 200 :height 200})))
      (is (map? (plot/build (update (p budget) :mapping dissoc :color))))
      (is (map? (plot/build (p [])))))
    (testing (str kind ": the check ignores the NA switch; an identity scale takes colours only")
      (is (= [:mapping :color] (:in (error #(plot/build (assoc (p budget) :groups {:na false}))))))
      (is (map? (plot/build (assoc (p (assoc-in budget [0 :c] "red")) :groups {:na false}))))
      (is (= [:mapping :color] (:in (error #(plot/build (assoc (p (mapv (fn [r] (assoc r :c "no-colour")) budget)) :scales {:color {:kind :identity}}))))))
      (is (map? (plot/build (assoc (p (mapv (fn [r] (assoc r :c :red)) budget)) :scales {:color {:kind :identity}})))))
    (testing (str kind ": the value is checked first")
      (is (= [:mapping :value] (:in (error #(plot/build (p (mapv (fn [r] (assoc r :cost nil)) budget))))))))))

(def ^:private lattice
  "A full 3 x 3 lattice, `:v` = x + y."
  (vec (for [x [0 1 2] y [0 1 2]] {:x x :y y :v (+ x y)})))

(defn- dropped
  "The first chart's `:dropped` of a built plot."
  [p]
  (get-in (plot/build p) [:stats 0 :dropped]))

(deftest gridded-holes-test
  (doseq [hole [##NaN nil "high" ##Inf]]
    (testing (str "a " (pr-str hole) " value is counted once under the mapped value")
      (let [rows (assoc-in lattice [4 :v] hole)]
        (is (= {:x 0 :y 0 :z 1} (select-keys (dropped {:data rows :mapping {:x :x :y :y :z :v} :charts [(chart/surface)]}) [:x :y :z])))
        (is (= {:x 0 :y 0 :value 1} (select-keys (dropped {:data rows :mapping {:x :x :y :y :value :v} :charts [(chart/field)]}) [:x :y :value])))
        (is (= {:x 0 :y 0 :value 1} (select-keys (dropped {:data rows :mapping {:x :x :y :y :value :v} :charts [(chart/contour)]}) [:x :y :value]))))))
  (testing "a row bad in a position and the value is counted once, under the position"
    (let [rows (conj lattice {:x nil :y 1 :v nil} {:x 1 :y ##NaN :v ##NaN})
          d (dropped {:data rows :mapping {:x :x :y :y :value :v} :charts [(chart/field)]})]
      (is (= {:x 1 :y 1 :value 0} (select-keys d [:x :y :value])))))
  (testing "a full lattice drops nothing"
    (is (= {:x 0 :y 0 :z 0} (select-keys (dropped {:data lattice :mapping {:x :x :y :y :z :v} :charts [(chart/surface)]}) [:x :y :z]))))
  (testing "density sources read positions only; a function source reads no rows"
    (let [pts (conj (vec (for [i (range 20)] {:x (mod (* 7 i) 11) :y (mod (* 5 i) 13) :v nil})) {:x nil :y 0 :v 1})]
      (is (= {:x 1 :y 0} (select-keys (dropped {:data pts :mapping {:x :x :y :y} :charts [(chart/surface {:density true})]}) [:x :y :z])))
      (is (= {:x 1 :y 0} (select-keys (dropped {:data pts :mapping {:x :x :y :y} :charts [(chart/field)]}) [:x :y :value]))))
    (is (= {} (dissoc (dropped {:charts [(chart/field {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]}})]}) :field)))))

(def ^:private base
  "A small scatter plot coloured by a category."
  {:data [{:x 1 :y 2 :g "a"} {:x 2 :y 3 :g "b"}] :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]})

(deftest formula-message-test
  (testing "the cause's message once: no doubled prefix"
    (is (re-matches #"Invalid LaTeX formula: [^\n]*'nope'" (latex/problem "\\nope")))
    (is (not (str/includes? (latex/problem "\\nope") "Invalid input")))
    (is (nil? (latex/problem "x^2")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"^Invalid input: \[:labels :x\] Invalid LaTeX formula: (?:(?!Invalid).)*$"
                          (plot/build (assoc base :labels {:x "$\\nope$"}))))))

(deftest scale-and-chart-label-formulas-test
  (testing "an aesthetic scale title: a text, a formula or runs value, at its scale path"
    (is (= [:scales :color :title] (:in (error #(plot/build (assoc base :scales {:color {:title "$\\nope$"}}))))))
    (is (= [:scales :color :title] (:in (error #(plot/build (assoc base :scales {:color {:title (plot/runs "n " (plot/math "\\nope"))}}))))))
    (is (= [:scales :shape :title] (:in (error #(plot/build (-> base (assoc-in [:mapping :shape] :g) (assoc :scales {:shape {:title "$\\nope$"}})))))))
    (is (map? (plot/build (assoc base :scales {:color {:title :none}}))))
    (is (map? (plot/build (assoc base :scales {:color {:title nil}}))))
    (is (map? (plot/build (assoc base :scales {:color {:title "$x^2$"}}))))
    (is (map? (plot/build (assoc base :theme {:text {:legend-title {:dollar-math false}}} :scales {:color {:title "$\\nope$"}})))
        "the role's switch"))
  (testing "every failing text is listed in one error"
    (is (= #{[:labels :title] [:scales :color :title]}
           (set (map :in (:errors (error-data #(plot/build (assoc base :labels {:title "$\\nope$"} :scales {:color {:title "$\\nope$"}})))))))))
  (testing "a pairwise chart's :label, at its chart path"
    (let [p (fn [label & [glyph]] {:data [{:a 1 :b 2} {:a 2 :b 1} {:a 3 :b 3}] :mapping {:x :a :y :b}
                                   :charts [(chart/pairwise (cond-> {:method :rmse :label label} glyph (assoc :glyph glyph)))]})
          off {:text {:pairwise {:dollar-math false}}}]
      (is (= [:charts 0 :options :label] (:in (error #(plot/build (p "$\\nope$"))))))
      (is (= [:charts 0 :options :label] (:in (error #(plot/build (p (plot/runs "e " (plot/math "\\nope"))))))))
      (is (map? (plot/build (p "$e_1$"))))
      (is (map? (plot/build (assoc (p "$\\nope$") :theme off))) "the role's switch, no colour bar")
      (is (= [:charts 0 :options :label] (:in (error #(plot/build (assoc (p "$\\nope$" :shade) :theme off)))))
          "coloured by value: also the colour bar title")
      (is (map? (plot/build (assoc (p "$\\nope$" :shade) :theme off :labels {:color "v"}))) "a plot colour title replaces it")))
  (testing "a histogram's :invalid-label, as an x tick label"
    (let [p {:data [{:v 1} {:v nil}] :mapping {:x :v} :charts [(chart/histogram {:invalid :show :invalid-label "$\\nope$"})]}]
      (is (= [:charts 0 :options :invalid-label] (:in (error #(plot/build p)))))
      (is (map? (plot/build (assoc p :theme {:axis {:x {:tick-label {:dollar-math false}}}}))) "the axis's text style"))))

(def ^:private wide
  "Wide rows of three numeric variables."
  [{:a 1 :b 2 :c 3} {:a 2 :b 1 :c 2} {:a 3 :b 3 :c 1}])

(deftest builder-variable-name-formulas-test
  (testing "pairs: an error at the option path, before any plot is built"
    (is (= {:key :variable-names :in [:variable-names :b]}
           (select-keys (error #(plot/pairs wide {:variables [:a :b] :variable-names {:a "a" :b "$\\nope$"}})) [:key :in])))
    (is (= [:variable-names :b] (:in (error #(plot/pairs wide {:row-variables [:a] :column-variables [:b :c] :variable-names {:b "$\\nope$"}})))))
    (is (= [:variable-names :c] (:in (error #(plot/pairs wide {:variables [:a :b] :variable-names {:c "$\\nope$"}}))))
        "a name of a variable not drawn is checked too")
    (is (= [:variable-names :a] (:in (error #(plot/pairs wide {:variables [:a :b] :variable-names {:a (plot/math "\\nope")}})))))
    (is (map? (plot/pairs wide {:variables [:a :b] :variable-names {:a "$\\alpha$" :b (plot/math "\\beta")}}))))
  (testing "pairs: the builder's theme decides; :names adds the diagonal's role"
    (let [off {:text {:axis-title {:dollar-math false}}}]
      (is (map? (plot/pairs wide {:variables [:a :b] :variable-names {:a "$\\nope$"} :theme off})))
      (is (= [:variable-names :a] (:in (error #(plot/pairs wide {:variables [:a :b] :variable-names {:a "$\\nope$"} :theme off :diagonal :names})))))))
  (testing "alluvial: an error at the name's index; non-texts pass as their text"
    (let [rows [{:p "x" :q "u" :n 1} {:p "y" :q "v" :n 2}]]
      (is (= {:key :variable-names :in [:variable-names 1]}
             (select-keys (error #(plot/alluvial rows {:variables [:p :q] :value :n :variable-names ["P" "$\\nope$"]})) [:key :in])))
      (is (map? (plot/alluvial rows {:variables [:p :q] :value :n :variable-names [:p 2]})))
      (is (map? (plot/alluvial rows {:variables [:p :q] :value :n :variable-names ["P" "$\\nope$"]
                                     :theme {:text {:tick-label {:dollar-math false}}}})))
      (is (map? (plot/alluvial rows {:variables [:p :q] :value :n :variable-names ["P" "$\\nope$"]
                                     :theme {:axis {:x {:tick-label {:dollar-math false}}}}}))
          "the names' axis style, as they are drawn")))
  (testing "formula-problems: one text's label content"
    (let [t (theme/plot-theme nil)
          content (fn [role text] {:kind :label :role role :text text})]
      (is (= [{:in [:k] :value "\\nope" :message (latex/problem "\\nope")}]
             (pipeline/formula-problems t (content :annotation "a $\\nope$") [:k])))
      (is (empty? (pipeline/formula-problems t (content :annotation "a $x$") [:k])))
      (is (empty? (pipeline/formula-problems t (content :annotation "") [:k])))
      (is (empty? (pipeline/formula-problems t (content :no-such-role "$\\nope$") [:k])) "an unknown role is the layout's error")
      (is (empty? (pipeline/formula-problems t (assoc (content :annotation "$\\nope$") :dollar-math false) [:k])) "inline style keys")
      (is (seq (pipeline/formula-problems t (axis/tick-label t :x "$\\nope$") [:k]))))))
