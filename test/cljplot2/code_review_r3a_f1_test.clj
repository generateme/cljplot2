(ns cljplot2.code-review-r3a-f1-test
  "Code review round 3, batch A, strip and distribution charts: shared legend glyphs, the fill-gradient rule checked
  at build, the violin's inner-mark style keys from the theme, nil options read as defaults."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [cljplot2.chart.legend :as chart-legend]
            [cljplot2.chart :as chart]
            [cljplot2.chart.beeswarm :as beeswarm]
            [cljplot2.chart.common :as common]
            [cljplot2.chart.ecdf :as ecdf]
            [cljplot2.chart.probability :as probability]
            [cljplot2.chart.stripplot :as stripplot]
            [cljplot2.core :as plot]))

(def ^:private rows
  (for [i (range 40)] {:v (double (mod (* i 7) 13)) :g (if (even? i) "a" "b")}))

(defn- pixels [img]
  (let [^java.awt.image.BufferedImage i img]
    (vec (.getRGB i 0 0 (.getWidth i) (.getHeight i) nil 0 (.getWidth i)))))

(deftest shared-legends-test
  (is (identical? chart-legend/line-legend (:legend (chart/ecdf))))
  (is (identical? chart-legend/stroked-point-legend (:legend (chart/qq))))
  (is (identical? chart-legend/point-legend (:legend (chart/beeswarm))))
  (is (identical? chart-legend/point-legend (:legend (chart/stripplot)))))

(deftest fill-gradient-rule-test
  (testing ":fill-gradient-axis without :fill-gradient fails at build validation"
    (let [e (try (plot/build {:data rows :mapping {:x :v} :charts [(chart/density {:fill-gradient-axis :y})]})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (some #(= :fill-gradient-axis (last (:in %))) (:errors (ex-data e)))))))

(deftest violin-inner-style-test
  (let [p (fn [theme] {:data rows :mapping {:x :g :y :v} :charts [(chart/violin {:inner :box})] :theme theme})
        o {:width 300 :height 250}]
    (testing "the theme's inner-mark keys restyle the violin"
      (is (not= (pixels (plot/render-image (p {}) o))
                (pixels (plot/render-image (p {:chart {:violin {:inner-box-width 1.0}}}) o)))))
    (testing "the default theme values draw as before"
      (is (= (pixels (plot/render-image (p {}) o))
             (pixels (plot/render-image (p {:chart {:violin {:inner-box-width 5.0 :inner-median-size 4.0}}}) o)))))))

(deftest nil-options-are-defaults-test
  (let [o {:width 300 :height 250}]
    (doseq [[c c-nil] [[(chart/density) (chart/density {:n nil :cut nil :symmetric nil :zero-line nil})]
                       [(chart/box) (chart/box {:n-label nil})]
                       [(chart/stripplot {:rng (r/rng :mersenne 1)}) (chart/stripplot {:rng (r/rng :mersenne 1) :n-label nil})]]]
      (is (= (pixels (plot/render-image {:data rows :mapping {:x :v} :charts [c]} o))
             (pixels (plot/render-image {:data rows :mapping {:x :v} :charts [c-nil]} o)))
          (str (:kind c))))))
