;; # Errors

;; Invalid input fails before anything is computed. `plot/build` (and `render`, `render-image`) validates in three
;; rounds, each throwing one `ex-info` listing all its errors:
;;
;; 1. the render options, the plot keys and the merged theme;
;; 2. when those hold, every chart's options and mapping;
;; 3. when those hold, the rules across keys (e.g. `:flip` needs the generated grid, not `:arrange`).
;;
;; Builders and data functions check their options before reading their data.

(ns reference.errors
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

;; ## Error data

;; `ex-data` of the error: `:key` - the first offending key, `:in` - its path, `:value` - the value, `:errors` -
;; every problem as `{:in path :value v :message text}`; the exception message lists the same problems as text. A
;; misspelled key gets a spelling suggestion.

(def bad-plot
  {:data [{:x 1 :y 2} {:x 2 :y 3}]
   :mapping {:x :x :y :y}
   :charts [(chart/scatter {:szie 4})]
   :scales {:x {:kind :logarithmic}}})

(try (plot/build bad-plot)
     (catch clojure.lang.ExceptionInfo e {:message (ex-message e) :data (ex-data e)}))

;; With the scale fixed, the next round reports the chart's misspelled option:

(try (plot/build (assoc-in bad-plot [:scales :x :kind] :log))
     (catch clojure.lang.ExceptionInfo e (ex-data e)))

;; Fixed:

(plot/render-image (-> bad-plot
                       (assoc :charts [(chart/scatter {:size 4})])
                       (assoc-in [:scales :x :kind] :log))
                   {:width 500 :height 300})

;; ## Builders and data functions

;; Options are checked against the builder's schema before its data is read:

(try (plot/sankey [{:from "a" :to "b" :value 1}] {:node-widht 10})
     (catch clojure.lang.ExceptionInfo e (ex-data e)))

;; Checks of the data itself (missing columns, wrong shapes) throw the same kind of error, keyed by the option or
;; argument at fault.

;; ## Finding the cause

;; * `:until` (render options) stops the build after a stage - the built plot is data to inspect.
;; * `:debug true` paints the layout boxes (Render chapter).
