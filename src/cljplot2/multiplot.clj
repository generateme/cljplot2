(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.content.subplot]]).
  Panels of arranged plots are aligned in rows and columns by default, see [[cljplot2.layout.align]].")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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

  The grid is generated at build time from the merged theme ([[cljplot2.layout.plot-grid/arrange-grid]], cells `[:plot i]`),
  where the spec is validated (`ex-info {:key :arrange}`)."
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :align]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))
