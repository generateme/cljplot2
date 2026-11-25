<p align="center">
  <img src="img/logo.png" alt="cljplot2 logo: the word cljplot2 drawn as a scatter plot with density margins"/>
</p>

# cljplot2

A plotting library for Clojure: plots are plain maps of data, mappings and charts, built and rendered by functions.

- Pure JVM: renders `BufferedImage`s and image files, no browser or JavaScript.
- Built on [fastmath](https://github.com/generateme/fastmath) (statistics, distributions, layouts),
  [wadogo](https://github.com/scicloj/wadogo) (scales), [clojure2d](https://github.com/Clojure2D/clojure2d)
  (drawing, colours) and [malli](https://github.com/metosin/malli) (validated options with readable errors).
- Charts: scatter, line, bar, histogram, density, box, violin, ribbon, smooth, heatmaps, 2D fields and contours,
  Q-Q / P-P, treemap, sunburst, circle packing, waffle, Venn, networks, dendrograms, chord, arc and sankey diagrams,
  and more; facets, polar coordinates, margins, multiplots.
- LaTeX formulas in any text (titles, axes, legends, labels); a default palette checked for colour vision
  deficiencies.

## Install

There's no release yet; a snapshot is on [Clojars](https://clojars.org/org.clojars.generateme/cljplot2):

```clojure
org.clojars.generateme/cljplot2 {:mvn/version "0.1.0-SNAPSHOT"}        ; deps.edn
[org.clojars.generateme/cljplot2 "0.1.0-SNAPSHOT"]                     ; project.clj
```

or a fixed commit through a git coordinate in `deps.edn`:

```clojure
io.github.generateme/cljplot2 {:git/sha "ac8e8e0c1e41c3c0461a43cb6f83337f0c17a917"}
```

cljplot2 depends on SNAPSHOT versions of fastmath (`3.0.0-alpha10-SNAPSHOT`), clojure2d (`1.5.0-alpha3-SNAPSHOT`) and
wadogo (`1.1.0-alpha2-SNAPSHOT`), all on Clojars.

## Example

```clojure
(require '[cljplot2.core :as plot]
         '[cljplot2.chart :as chart])

(def data
  (for [series ["sin" "cos"]
        x (range 0 6.3 0.1)]
    {:x x :y ((if (= series "sin") Math/sin Math/cos) x) :series series}))

(-> {:data data
     :mapping {:x :x :y :y :color :series}
     :charts [(chart/line) (chart/scatter)]
     :labels {:title "Hello, cljplot2"}}
    (plot/render-image {:width 600 :height 350})
    (plot/save "example.png"))
```

<p align="center">
  <img src="img/example.png" alt="the example: sin and cos lines with points, coloured by series"/>
</p>

## Documentation

The book: <https://generateme.github.io/cljplot2/>.

[Reference](https://generateme.github.io/cljplot2/reference.intro.html): concepts (data and mapping, aesthetics and
legends, scales and coordinates, facets, theme, errors), every chart, builder and data function with its option
tables and examples.

Use cases after the
[R Graph Gallery](https://r-graph-gallery.com/) and [data-to-viz](https://www.data-to-viz.com/):

- [Distribution](https://generateme.github.io/cljplot2/use_cases.distribution.html)
- [Correlation](https://generateme.github.io/cljplot2/use_cases.correlation.html)
- [Ranking](https://generateme.github.io/cljplot2/use_cases.ranking.html)
- [Part of a whole](https://generateme.github.io/cljplot2/use_cases.part_of_a_whole.html)
- [Evolution](https://generateme.github.io/cljplot2/use_cases.evolution.html)
- [Flow](https://generateme.github.io/cljplot2/use_cases.flow.html)
- [Colours](https://generateme.github.io/cljplot2/use_cases.colors.html)

Galleries:

- [cljplot extras](https://generateme.github.io/cljplot2/galleries.cljplot_extras.html) - the examples of the
  original [cljplot](https://github.com/generateme/cljplot) (Q-Q / P-P, ACF, vector fields, complex functions, ...)
- [Lattice](https://generateme.github.io/cljplot2/galleries.lattice.html) - the figures of Deepayan Sarkar's
  *Lattice: Multivariate Data Visualization with R*
- [Vega](https://generateme.github.io/cljplot2/galleries.vega.html) - the examples of the Vega-Lite and Vega galleries

Every public function has a docstring; namespaces: `cljplot2.core` (build, render, builders), `cljplot2.chart`
(charts), `cljplot2.data.*` (row and layout functions).

## Development

```shell
clojure -M:test     # tests
clojure -M:check    # reflection and boxed-math warnings
clojure -M:demo     # demo images to results/ (and the logo to img/)
clojure -M:clay     # the book into docs/
```

## License

Copyright © GenerateMe and contributors.

Developed with the support of an LLM (Anthropic's Claude) - design, code, tests and documentation were reviewed
by the author.

Distributed under the [Eclipse Public License 2.0](LICENSE).

The bundled fonts (`resources/fonts/`) keep their own licences:

- DejaVu Sans Mono - Bitstream Vera Fonts licence, DejaVu changes in the public domain, see
  [LICENSE-DejaVu.txt](resources/fonts/LICENSE-DejaVu.txt);
- Noto Emoji - SIL Open Font License 1.1, see [LICENSE-NotoEmoji.txt](resources/fonts/LICENSE-NotoEmoji.txt).

LaTeX formulas are typeset by the [JLaTeXMath](https://github.com/opencollab/jlatexmath) dependency (GPL 2.0 with
the Classpath Exception); its Greek and Cyrillic font jars, GPL without the exception, are excluded.
