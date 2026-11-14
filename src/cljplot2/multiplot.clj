(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).
  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]]."
  (:require [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private options-schema
  "Keys of [[arrange]]'s `options` (their values are validated with the plot, at build)."
  [:map {:closed true}
   (schema/optional :columns any?) (schema/optional :rows any?) (schema/optional :widths any?)
   (schema/optional :heights any?) (schema/optional :align any?) (schema/optional :title any?)
   (schema/optional :theme any?)])

(defn arrange
  "Plot map arranging `plots` in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

  `options`:

  * `:columns`, `:rows` - grid shape (default one column; one given, the other computed)
  * `:widths`, `:heights` - relative column / row weights (default equal)
  * `:align` - aligns the panels of sub-plots in rows and columns (default true; `false` - each sub-plot laid out
    on its own), see [[cljplot2.layout.align]]
  * `:title` - overall title above the cells (`:labels {:title t}`)
  * `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]`
    around them; sub-plots keep their own themes

  An unknown option key throws `ex-info {:key :arrange}` (with a spelling suggestion when one is close).
  The grid is generated at build time from the merged theme ([[cljplot2.layout.plot-grid/arrange-grid]], cells `[:plot i]`),
  where the spec is validated (`ex-info {:key :arrange}`)."
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (when-let [{:keys [in value message]} (first (schema/errors options-schema options [:options]))]
     (schema/fail! (str "Invalid arrange option " (pr-str (last in)) ": " message ".") {:key :arrange :in in :value value}))
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :align]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))
