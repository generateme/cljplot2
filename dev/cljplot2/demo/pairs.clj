(ns cljplot2.demo.pairs
  "Correlation cycle 3 demo: scatter-plot matrices and correlograms - iris ggpairs-style by species, an mtcars
  correlogram (ellipses below, pies above, names on the diagonal), shade and number glyphs, a two-variable matrix."
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.parallel :refer [iris mtcars]]
            [cljplot2.demo.smooth :refer [faithful]]))

(def iris-pairs
  (plot/pairs iris {:variables [:sepal_length :sepal_width :petal_length :petal_width] :mapping {:color :species}
                    :variable-names {:sepal_length "sepal length" :sepal_width "sepal width" :petal_length "petal length" :petal_width "petal width"}
                    :title "Iris: scatter, densities and correlations by species"}))

(def mtcars-correlogram
  (plot/pairs mtcars {:variables [:mpg :disp :hp :drat :wt :qsec]
                      :lower (chart/correlation {:glyph :ellipse}) :upper (chart/correlation {:glyph :pie}) :diagonal :names
                      :title "mtcars correlogram: ellipses and pies"}))

(def shade-number
  (plot/pairs mtcars {:variables [:mpg :hp :wt :qsec]
                      :lower (chart/correlation {:glyph :shade}) :upper (chart/correlation {:glyph :number :method :spearman})
                      :diagonal :names :title "Shade below, Spearman r above"}))

(def two-variables
  (plot/pairs faithful {:variables [:eruptions :waiting] :lower [(chart/scatter {:size 3}) (chart/smooth)]
                        :diagonal (chart/histogram {:bins 20}) :title "Two variables"}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/pairs/` by [[cljplot2.demo/render!]]."
  [["iris-pairs-950x800.png" iris-pairs {:width 950 :height 800}]
   ["mtcars-correlogram-800x800.png" mtcars-correlogram {:width 800 :height 800}]
   ["shade-number-650x650.png" shade-number {:width 650 :height 650}]
   ["two-variables-550x500.png" two-variables {:width 550 :height 500}]])
