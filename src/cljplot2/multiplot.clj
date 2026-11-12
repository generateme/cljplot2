(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.subplot]]).")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn arrange
  "Plot map arranging `plots` in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

  `options`:

  * `:columns`, `:rows` - grid shape (default one column; one given, the other computed)
  * `:widths`, `:heights` - relative column / row weights (default equal)
  * `:title` - overall title above the cells (`:labels {:title t}`)
  * `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]`
    around them; sub-plots keep their own themes

  The grid is generated at build time from the merged theme ([[cljplot2.plot-grid/arrange-grid]], cells `[:plot i]`),
  where the spec is validated (`ex-info {:key :arrange}`)."
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))
