(ns cljplot2.text-formulas-test
  "Batch C4 - LaTeX formulas in label texts: formula and runs values, dollar parsing (`data.text`), typesetting
  (`graphics.latex`), measuring, fitting and drawing in label content, the switch `:dollar-math`, the `:power-math`
  formatter, build-time checks and formulas in every place text goes."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.data.text :as text]
            [cljplot2.graphics.latex :as latex]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as position]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error same-pixels? pixels]])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]))

(def ^:private m text/math)
(def ^:private runs text/runs)

(deftest dollar-runs-test
  (testing "plain text and pairs"
    (are [line rs] (= rs (text/dollar-runs line))
      "" []
      "abc" ["abc"]
      "$x$" [(m "x")]
      "a $x^2$ b" ["a " (m "x^2") " b"]
      "$a$$b$" [(m "a") (m "b")]
      "$\\alpha$ set" [(m "\\alpha") " set"]
      "$$x$$" [(m "x")]
      "a $$\\frac{1}{2}$$ b" ["a " (m "\\frac{1}{2}") " b"]))
  (testing "the pairing rule: no space inside the dollars, a closing dollar not followed by a digit"
    (are [line rs] (= rs (text/dollar-runs line))
      "$5 – $10" ["$5 – $10"]
      "between $5 and $10" ["between $5 and $10"]
      "cost $5" ["cost $5"]
      "$ x$" ["$ x$"]
      "$x $" ["$x $"]
      "$x$5" ["$x$5"]
      "$x$y$" [(m "x") "y$"]
      " $ x$" [" $ x$"]))
  (testing "boundary: escapes, single and empty dollars stay literal"
    (are [line rs] (= rs (text/dollar-runs line))
      "\\$5" ["$5"]
      "a$" ["a$"]
      "$" ["$"]
      "$$" ["$$"]
      "$$ $$" ["$$ $$"]
      "\\$x$" ["$x$"]
      "\\\\$x$" ["\\" (m "x")]
      "$a\\$b$" [(m "a\\$b")]
      "\\x $y$" ["\\x " (m "y")]
      "end\\" ["end\\"]))
  (testing "ambiguous text needs an escape"
    (is (= ["Revenue (" (m "m) vs cost (") "m)"] (text/dollar-runs "Revenue ($m) vs cost ($m)")))
    (is (= ["Revenue ($m) vs cost ($m)"] (text/dollar-runs "Revenue (\\$m) vs cost (\\$m)")))))

(deftest text-lines-test
  (testing "values, lines and runs values"
    (are [t lines] (= lines (text/text-lines t true))
      nil []
      "" []
      "a" [["a"]]
      "a\nb" [["a"] ["b"]]
      "a\r\nb" [["a"] ["b"]]
      "a\n" [["a"] []]
      "\n" [[] []]
      12 [["12"]]
      :k [[":k"]]
      (m "x") [[(m "x")]]
      (m 5) [[(m "5")]]
      (runs) []
      (runs nil "") []
      (runs "a " (m "x")) [["a " (m "x")]]
      (runs "a\nb " (m "x") " c") [["a"] ["b " (m "x") " c"]]
      (runs (m "x") "\n" (m "y")) [[(m "x")] [(m "y")]]
      (runs 1 (m "x") :k) [["1" (m "x") ":k"]]
      (runs "$y$ " (m "x")) [[(m "y") " " (m "x")]]
      (runs "a" (runs "b" (m "x"))) [["a" "b" (m "x")]]))
  (testing "vectors are values converted with str"
    (is (= [["[\"a\" \"b\"]"]] (text/text-lines ["a" "b"] true)))
    (is (= [["[]"]] (text/text-lines [] true))))
  (testing "dollar math off: dollars and escapes literal, formula values still formulas"
    (is (= [["$x$ \\$"]] (text/text-lines "$x$ \\$" false)))
    (is (= [["$y$" (m "x")]] (text/text-lines (runs "$y$" (m "x")) false)))
    (is (= [[(m "x")]] (text/text-lines (m "x") false))))
  (testing "formulas? and plain lines"
    (is (text/formulas? (text/text-lines "a $x$" true)))
    (is (not (text/formulas? (text/text-lines "a $x" true))))
    (is (not (text/formulas? [])))
    (is (= ["a" "" "b"] (text/plain-lines [["a"] [] ["b"]])))))

(deftest text-values-test
  (testing "formula and runs values are plain data"
    (is (= {:cljplot2/math "x"} (m "x") (plot/math "x")))
    (is (= {:cljplot2/math "1"} (m 1)))
    (is (= {:cljplot2/runs ["a" (m "x")]} (runs "a" (m "x")) (plot/runs "a" (m "x"))))
    (is (text/formula? (m "")))
    (is (text/formula? {:cljplot2/math 1}) "a hand-written formula value is a formula")
    (is (not (text/formula? "$x$")))
    (is (not (text/formula? nil)))
    (is (text/runs? (runs)))
    (is (not (text/runs? ["a" (m "x")]))))
  (testing "formula-text?, label-text and source-text"
    (is (text/formula-text? (m "x")))
    (is (text/formula-text? (runs "a")))
    (is (not (text/formula-text? ["a" (m "x")])))
    (is (= "12" (text/label-text 12)))
    (is (= "" (text/label-text nil)))
    (is (= (m "x") (text/label-text (m "x"))))
    (is (= (runs "a" (m "x")) (text/label-text (runs "a" (m "x")))))
    (is (= "$x$" (text/source-text (m "x"))))
    (is (= "a $x$1" (text/source-text (runs "a " (m "x") 1)))))
  (testing "a label value is an options map unless it is a formula or runs value"
    (is (label/label-options? {:text "a"}))
    (is (not (label/label-options? (m "x"))))
    (is (not (label/label-options? (runs "a"))))
    (is (not (label/label-options? "a")))))

(deftest typeset-test
  (testing "metrics in pixels: height above and depth below the baseline, scaled with the size, cached"
    (let [f10 (latex/typeset "x^2" 10 false)
          f20 (latex/typeset "x^2" 20 false)
          frac (latex/typeset "\\frac{a}{b}" 10 false)]
      (is (every? pos? (map f10 [:width :height])))
      (is (< 1.8 (/ (:width f20) (:width f10)) 2.2) "about twice - glyph metrics round per size")
      (is (identical? f10 (latex/typeset "x^2" 10.0 false)) "same source, size and weight - one entry")
      (is (pos? (:depth frac)))
      (is (< (:height frac) 10.0) "the height excludes the depth")))
  (testing "bold is its own entry and wider"
    (is (> (:width (latex/typeset "x+y" 10 true)) (:width (latex/typeset "x+y" 10 false)))))
  (testing "boundary: an empty formula has no width; unbalanced braces are completed"
    (is (zero? (:width (latex/typeset "" 10 false))))
    (is (pos? (:width (latex/typeset "\\frac{1}{" 10 false)))))
  (testing "Greek in math works; Cyrillic and Greek text mode fail keyed :text"
    (is (pos? (:width (latex/typeset "α+\\beta" 10 false))))
    (is (= {:key :text :value "ж"} (select-keys (error #(latex/typeset "ж" 10 false)) [:key :value])))
    (is (= :text (:key (error #(latex/typeset "\\text{ж}" 10 false)))) "the same answer every time")
    (is (= :text (:key (error #(latex/typeset "\\text{α}" 10 false))))))
  (testing "a formula that doesn't parse fails keyed :text, with its source; problem gives the message"
    (is (= {:key :text :value "\\nocommandlikethis"} (select-keys (error #(latex/typeset "\\nocommandlikethis" 10 false)) [:key :value])))
    (is (string? (latex/problem "x \\undefinedcommand y")))
    (is (nil? (latex/problem "x^2")))))

(deftest concurrent-draw-test
  (testing "one cached formula drawn by many threads in two colours keeps each canvas's colour"
    (let [formula (latex/typeset "x^{ab}{cd}^{ef} q_{rs} \\frac{u}{v} w" 20 false)
          draw (fn [color] (let [cv (c2d/canvas 220 60)]
                             (c2d/with-canvas [c cv] (latex/draw-typeset! c formula 5 40 color))
                             (set (remove #(zero? (bit-and (unchecked-int 0xff000000) (int %))) (pixels (c2d/get-image cv))))))
          colors (pmap (fn [i] [(even? i) (draw (if (even? i) [255 0 0] [0 0 255]))]) (range 200))]
      (is (every? (fn [[red argb]] (every? (fn [^long p] (let [r (bit-and (bit-shift-right p 16) 0xff) b (bit-and p 0xff)]
                                                           (if red (zero? b) (zero? r))))
                                           argb))
                  colors)))))

(def ^:private theme (theme/plot-theme nil))

(defn- size-of
  "Natural size `[w h]` of label content."
  [content]
  (measure/call-with-scratch-canvas
   :high
   (fn [canvas] (let [^Vec2 s (measure/natural-size (merge {:kind :label :role :axis-title} content) {:canvas canvas :theme theme})]
                  [(.x s) (.y s)]))))

(defn- ink-gaps
  "Pixels between a label's ink and the top and bottom of its box (black text on a white background)."
  [content]
  (let [[w h] (size-of content)
        cv (c2d/canvas (long w) (long h))
        content (merge {:kind :label :role :axis-title :color :black :background :white} content)]
    (c2d/with-canvas [c cv] (label/draw-at! c content [:left :top] 0 0 {:theme theme}))
    (let [^BufferedImage img (c2d/get-image cv)
          ink-row? (fn [y] (some (fn [x] (< (bit-and (.getRGB img x y) 0xff) 128)) (range (.getWidth img))))
          ink (filter ink-row? (range (.getHeight img)))]
      [(first ink) (- (.getHeight img) 1 (last ink))])))

(deftest label-size-test
  (testing "a formula is measured; text without one keeps its size"
    (let [[pw ph] (size-of {:text "ab"})
          [fw fh] (size-of {:text "$ab$"})]
      (is (pos? fw))
      (is (not= pw fw))
      (is (= [pw ph] (size-of {:text "ab" :dollar-math false})))
      (is (= [pw ph] (size-of {:text "ab" :dollar-math nil})) "nil - the default")
      (is (>= fh ph) "a line is the font's line height at least")))
  (testing "a runs line is as wide as its runs together"
    (let [[aw] (size-of {:text "speed "})
          [bw] (size-of {:text (m "v^2")})
          [cw] (size-of {:text (runs "speed " (m "v^2"))})
          inset2 (* 2.0 (get-in theme [:text :axis-title :inset]))]
      (is (<= (Math/abs (- (- cw inset2) (+ (- aw inset2) (- bw inset2)))) 2.0))))
  (testing "a tall formula makes its line taller; two lines stack"
    (let [[_ h1] (size-of {:text "x"})
          [_ h2] (size-of {:text (m "\\frac{\\sum_i x_i}{\\sqrt{n}}")})
          [_ h3] (size-of {:text (runs "x\n" (m "\\frac{\\sum_i x_i}{\\sqrt{n}}"))})]
      (is (> h2 h1))
      (is (<= (Math/abs (- h3 (+ h1 h2 (- (* 2.0 (get-in theme [:text :axis-title :inset])))))) 2.0))))
  (testing "a formula's ink sits in its box without blank space above it"
    (let [[top bottom] (ink-gaps {:text (m "\\sqrt{\\frac{a^2}{b_j}}") :size 20 :inset 0})]
      (is (<= top 2) (str "top gap " top))
      (is (<= bottom 2) (str "bottom gap " bottom))))
  (testing "rotation: 90 / 270 swap the size"
    (let [[w h] (size-of {:text (runs "two\nlines " (m "x^2"))})]
      (is (= [h w] (size-of {:text (runs "two\nlines " (m "x^2")) :rotation 270})))
      (is (= [h w] (size-of {:text (runs "two\nlines " (m "x^2")) :rotation 90})))))
  (testing "a bold role's formula is bold (wider)"
    (is (> (first (size-of {:text "$x+y$" :font-style :bold})) (first (size-of {:text "$x+y$"})))))
  (testing "empty text has no size; an invalid formula fails keyed :text"
    (is (= [0.0 0.0] (size-of {:text ""})))
    (is (= [0.0 0.0] (size-of {:text (runs)})))
    (is (= :text (:key (error #(size-of {:text "$\\undefinedcommand$"})))))))

(deftest fitted-label-test
  (measure/call-with-scratch-canvas
   :high
   (fn [canvas]
     (let [env {:canvas canvas :theme theme}
           along (fn [c] (label/along-text c env))
           content {:kind :label :role :axis-title :text "a long axis title with a formula $x^2$ in it"}]
       (testing "a text with formulas is wrapped at spaces; formulas stay whole"
         (let [fitted (label/fitted-label content {:length 120.0 :wrap true} env)
               lines (text/text-lines (:text fitted) false)]
           (is (text/runs? (:text fitted)))
           (is (false? (:dollar-math fitted)) "its dollars already read")
           (is (< 1 (count lines) 4))
           (is (= 1 (count (filter text/formula? (apply concat lines)))))
           (is (<= (along fitted) 120.0))))
       (testing "then shrunk and cut only in its plain pieces"
         (let [fitted (label/fitted-label (assoc content :text "averylongwordwithoutspaces $x^2$ andmore") {:length 60.0} env)
               runs (first (text/text-lines (:text fitted) false))]
           (is (= 8.0 (double (:size fitted))))
           (is (<= (along fitted) 60.0))
           (is (re-find #"…$" (last runs)))
           (is (not-any? text/formula? runs) "the formula past the cut is dropped whole")))
       (testing ":cut false keeps the text whole"
         (let [fitted (label/fitted-label content {:length 60.0 :cut false} env)]
           (is (= (:text content) (:text fitted)))
           (is (= 8.0 (double (:size fitted))))))
       (testing "boundary: a length too short for anything leaves the ellipsis"
         (is (= [["…"]] (text/text-lines (:text (label/fitted-label (assoc content :text "$x^2$ $y^2$") {:length 1.0} env)) false))))
       (testing "an escaped dollar stays literal after wrapping"
         (let [fitted (label/fitted-label (assoc content :text "prices \\$a and b\\$ for the whole year round") {:length 120.0 :wrap true} env)]
           (is (not (text/formulas? (text/text-lines (:text fitted) (not (false? (:dollar-math fitted)))))))))))))

(deftest power-math-test
  (testing "whole powers typeset, other values as numbers"
    (is (= (m "10^{3}") (position/power-math-label 10 1000)))
    (is (= (m "10^{-3}") (position/power-math-label 10 0.001)))
    (is (= (m "10^{0}") (position/power-math-label 10 1)))
    (is (= (m "2^{5}") (position/power-math-label 2 32)))
    (is (= (m "e^{2}") (position/power-math-label Math/E (* Math/E Math/E))))
    (is (= "3" (position/power-math-label 2 3)))
    (is (= "0" (position/power-math-label 10 0)))
    (is (= "-10" (position/power-math-label 10 -10))))
  (testing ":power-math needs a log scale"
    (is (= :formatter (:key (error #(plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                                 :scales {:x {:formatter :power-math}}})))))))

(def ^:private rows [{:x 1 :y 10 :g "$\\alpha$"} {:x 2 :y 100 :g "$\\beta$"} {:x 3 :y 1000 :g "$\\alpha$"}])

(def ^:private base {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]})

(defn- renders?
  "Tests whether a plot renders to an image."
  [p options]
  (instance? BufferedImage (plot/render-image p options)))

(deftest everywhere-test
  (testing "formulas in the title, axis titles, ticks, legend, strips and text render and differ from literal dollars"
    (let [p (assoc base
                   :mapping {:x :x :y :y :color :g :facet-col :g}
                   :charts [(chart/scatter) (chart/text {:x 2 :y 100 :text (runs "v " (m "\\sqrt{x}"))})]
                   :scales {:y {:kind :log :formatter :power-math}}
                   :labels {:title (runs "title " (m "\\sigma")) :x (m "x_i") :y {:text "two\nlines $y$"}})
          literal (assoc-in p [:theme :text] (into {} (map (fn [r] [r {:dollar-math false}])) (keys (:text theme))))]
      (is (renders? p {:width 500 :height 300}))
      (is (not (same-pixels? p literal {:width 500 :height 300})))))
  (testing "text without a pair renders as before (dollar math on or off)"
    (let [p (assoc base :labels {:title "cost $5 and $10" :x "a \\$ b"})]
      (is (same-pixels? p (assoc-in p [:labels :title] {:text "cost $5 and $10" :dollar-math false}) {:width 300 :height 200}))))
  (testing "a per-label and a per-chart :dollar-math override the role"
    (let [p (assoc base :labels {:title "$x$"} :theme {:text {:title {:dollar-math false}}})]
      (is (same-pixels? p (assoc base :labels {:title {:text "$x$" :dollar-math false}}) {:width 300 :height 200}))
      (is (not (same-pixels? p (assoc base :labels {:title {:text "$x$" :dollar-math true}} :theme {:text {:title {:dollar-math false}}})
                             {:width 300 :height 200}))))
    (let [t (fn [o] (assoc base :charts [(chart/scatter) (chart/text (merge {:x 2 :y 100 :text "$x^2$"} o))]))]
      (is (not (same-pixels? (t {}) (t {:dollar-math false}) {:width 300 :height 200})))
      (is (same-pixels? (t {:dollar-math false}) (assoc (t {:dollar-math false}) :theme {:text {:annotation {:dollar-math false}}}) {:width 300 :height 200}))))
  (testing "bar value labels (a format string and a function), contour-free tile labels and rotated ticks"
    (let [bars {:data [{:k "$m_1$" :v 1.5} {:k "$m_2$" :v 2.5}] :mapping {:x :k :y :v}
                :theme {:axis {:x {:label-rotation 45}}}}]
      (is (renders? (assoc bars :charts [(chart/bar {:value-label true :value-format "$%.1f\\,\\mathrm{kg}$"})]) {:width 300 :height 250}))
      (is (renders? (assoc bars :charts [(chart/bar {:value-label (fn [{:keys [value]}] (runs (m "\\mu") "=" value)) :n-label true})]) {:width 300 :height 250}))
      (is (renders? {:data [{:x "a" :y "b" :v 1.0}] :mapping {:x :x :y :y :color :v}
                     :charts [(chart/tile {:value-label (fn [{:keys [value]}] (m (str "\\sqrt{" value "}")))})]} {:width 300 :height 250}))))
  (testing "legends: ordinal, linear and binned size formatters and a formula title"
    (is (= (m "a") (:label (first (aesthetic/legend-entries :shape {:kind :ordinal :domain ["a"] :formatter (fn [_] (m "a"))})))))
    (is (= (runs (m "0.0") " – " (m "1.0"))
           (:label (first (aesthetic/legend-entries :size {:kind :binned :breaks [0.0 1.0 2.0] :formatter (fn [v] (m v))}))))))
  (doseq [scales [{:shape {:formatter (fn [v] (m (str "\\mathcal{" v "}"))) :title (m "\\gamma")}}
                  {:shape {:formatter (fn [v] (runs "grp " (m v)))}}
                  {:size {:formatter (fn [v] (m v))}}
                  {:size {:kind :binned :formatter (fn [v] (m (str "\\sqrt{" v "}")))}}]]
    (testing (str "legend renders " (keys scales))
      (is (renders? (assoc base :mapping {:x :x :y :y (if (:shape scales) :shape :size) (if (:shape scales) :g :y)} :scales scales)
                    {:width 500 :height 300}))))
  (testing "a facet strip formatter's formula and pairs variable names"
    (let [p (assoc base :mapping {:x :x :y :y :facet :x} :scales {:facet {:formatter (fn [v] (m (str "x_" v)))}})]
      (is (renders? p {:width 500 :height 300}))
      (is (not (same-pixels? p (assoc-in p [:scales :facet :formatter] (fn [v] (str "x_" v))) {:width 500 :height 300}))))
    (is (renders? (plot/pairs [{:a 1 :b 2} {:a 2 :b 1} {:a 3 :b 3}] {:variables [:a :b] :variable-names {:a (m "\\alpha") :b "$\\beta$"}})
                  {:width 400 :height 400})))
  (testing "invalid formulas: plot labels and literal texts at build, by their path; formatters at render, keyed :text"
    (is (= [:labels :title] (:in (error #(plot/build (assoc base :labels {:title "$\\undefinedcommand$"}))))))
    (is (= [:labels :x] (:in (error #(plot/build (assoc base :labels {:x {:text (m "\\undefinedcommand")}}))))))
    (is (= [:charts 1 :options :text] (:in (error #(plot/build (assoc base :charts [(chart/scatter) (chart/text {:x 1 :y 1 :text ["ok" "$\\nope$"]})]))))))
    (is (map? (plot/build (assoc base :labels {:title {:text "$\\undefinedcommand$" :dollar-math false}}))) "no formula when off")
    (is (= :text (:key (error #(plot/render-image (assoc base :scales {:y {:formatter (fn [_] (m "\\undefinedcommand"))}}) {:width 300 :height 200}))))))
  (testing "schemas take formula and runs values; :dollar-math is a boolean or nil"
    (is (map? (plot/build (assoc base :labels {:title (m "x") :x (runs "a" (m "x"))}))))
    (is (map? (plot/build (assoc base :labels {:title {:text "x" :dollar-math nil}}))))
    (is (= :dollar-math (last (:in (error #(plot/build (assoc base :labels {:title {:text "x" :dollar-math 1}})))))))
    (is (= :dollar-math (last (:in (error #(plot/build (assoc base :theme {:text {:title {:dollar-math "no"}}}))))))))
  (testing "a 3D axis title with formulas gets its margin"
    (let [p (fn [t] {:data [{:x 0 :y 0 :z 0} {:x 1 :y 1 :z 1}] :mapping {:x :x :y :y :z :z}
                     :charts [(chart/cloud)] :labels {:x t}})]
      (is (renders? (p (runs "a\nb\nc " (m "x"))) {:width 400 :height 400})))))

(deftest along-path-test
  (let [p {:data [{:x 0 :y 0 :g "a"} {:x 10 :y 1 :g "a"}] :mapping {:x :x :y :y :group :g}
           :charts [(chart/line) (chart/text {:mapping {:x :x :y :y :label :t :group :g} :along-path true})]}
        labelled (fn [t] (assoc p :data (mapv #(assoc % :t t) (:data p))))]
    (testing "formulas can't be laid along a path - dollar pairs and formula values, at build (compute)"
      (is (= :text (:key (error #(plot/build (labelled "$x$"))))))
      (is (= :text (:key (error #(plot/build (labelled (m "x"))))))))
    (testing "literal dollars can"
      (is (renders? (assoc (labelled "$x$") :theme {:text {:annotation {:dollar-math false}}}) {:width 400 :height 200}))
      (is (renders? (assoc-in (labelled "$x$") [:charts 1 :options :dollar-math] false) {:width 400 :height 200})))))
