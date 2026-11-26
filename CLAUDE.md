# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

cljplot2 is a Clojure plotting library: plots are plain maps of data, mappings and charts, built and rendered by
functions (no DSL, no grammar of graphics). Pure JVM - it renders `BufferedImage`s / image files. Built on fastmath
(statistics, distributions, layouts), wadogo (scales), clojure2d (drawing, colours) and malli (validated options).
The book (Clay + Quarto) is published from `docs/` at https://generateme.github.io/cljplot2/.

Design history, specs and the roadmap live in the user's Obsidian vault: `~/notes/Topics/cljplot2/`
(`01 Build Steps/Architecture Design.md`; `02 Malli Validation/` - option schemas and validation; `03 Use Case
Notebooks/` - a folder per section or gallery, the roadmap with section status and backlog in `00 Frame and
Roadmap/Roadmap - Use Case Notebooks.md`; `04 Reference/` - the reference part of the book; `05 Code Review/` - code
review rounds, their specs and implementation notes; `06 Reference Readability/` - option cards, concept headings
and prose of the reference part). Read them when a decision's rationale is unclear.

## Commands

```shell
clojure -M:test                                   # all tests (cognitect test-runner)
clojure -M:test -n cljplot2.extras-cycle1-test    # one namespace
clojure -M:test -v cljplot2.extras-cycle1-test/pp-test   # one test var
clojure -M:check      # loads every src namespace; must print no reflection / boxed-math warnings
clojure -M:demo                       # every demo image -> results/<demo>/ (the logo demo -> img/)
clojure -M:demo extras                # one demo
clojure -M:demo extras/pp-t-500x450.png   # one image
clojure -M:clay                       # build the book into docs/ (wiped first); `clojure -M:clay serve [port]`
clojure -T:build jar | install | deploy | clean   # target/ jar; ~/.m2 install; signed Clojars deploy (build.clj)
```

Deploy credentials stay outside the repo: `CLOJARS_USERNAME` / `CLOJARS_PASSWORD`, else the `clojars` server of
`~/.m2/settings.xml`; signing uses gpg's default key. The version is `build/version`.

REPL / scripts: `:dev` (dev + test sources, demo data), add `:notebooks` for notebook work
(`clojure -M:dev:notebooks -i script.clj`).

Regression practice: copy `results/` before a change, re-run `clojure -M:demo`, and `diff -rq` - existing demo
images should stay byte-identical unless the change is meant to alter them.

## Architecture

**Pipeline** (`cljplot2.pipeline`): a plot map passes through pure stages, each adding one key -
`:split` (facet split per chart) → `:stats` (chart compute) → `:extents` → `:scale-specs` → `:grid` → `:layout` →
`:finalized-scales` → `:subplots`; `cljplot2.render` draws the result. `build` first validates everything with malli
(`pipeline/validate!`) in two rounds: first the render options, the plot, the merged theme and - when the plot's keys
hold - the theme's chart styles and every chart's `:schema`; then, when all of those hold, the rules across plot keys.
Each round throws one `ex-info {:key :in :value :errors}` listing its errors.

**Chart contract** (`cljplot2.chart` ns doc): a chart value is a plain map `{:kind :options :compute :draw :legend
:schema}` with top-level functions, so built plots print and compare as data.
- `compute` gets all panels' grouped rows and returns stats (`:data` per panel, `:extents`, `:padding`, `:dropped`,
  optional `:axis-titles`, `:scale-defaults`, `:padding-px`, `:invalid-bands` …); `draw` renders one panel's
  computed data (and a band's, in band coordinates).
- Draw through `cljplot2.scale.position/projection` (or the coord helpers) - never assume `:x` is horizontal
  (`:flip`, polar `:coord`).
- Optional keys: `:computed-aesthetics` (e.g. a computed colour), `:coords` (`#{:cartesian :polar}`; `#{:three-d}`
  for 3D charts, which also have `:primitives` - their primitives of one panel, drawn by the panel in one scene, see
  `cljplot2.content.space3d`), `:inherit-mapping :facets` (annotation charts inherit only facet keys; data under `facet/all-panels` is drawn in
  every panel).
- Every built-in kind except `:text` has a theme entry `[:chart kind]` checked against `chart/builtin-styles`; text
  charts take their style from text roles (`[:text :annotation]` ...), and `[:chart :text]` is rejected by the theme
  schema. A chart draws with
  `(theme/resolved-style theme chart)` - its style keys are the entries of its `:schema` `:style` (extras a chart
  takes outside it are merged explicitly); a style key that isn't an option (theme only, e.g. cloud
  `:projection-alpha-factor`) is shown on the chart page with `describe/style-rows kind keys` (on a migrated page in a
  Theme keys box, `nu/theme-key-list`). The theme style is
  `(theme/chart-style theme kind options)`: `[:chart kind]` with the named style of its `:theme-style` option
  (shared group `:theme-style`, every kind with a theme style) merged over it, its own style options over both.
  Named styles are partial `[:chart name]` entries; builders draw with registered ones (`theme/named-styles` - name
  -> kind, values in `default-theme`, e.g. `:network-node`, `:tree-link`), so a theme restyles builders; `build`
  checks each merged over its kind (`pipeline/named-style-checks`), an unknown name is an error. Builder labels use
  text roles of section `:builder-text` through the text chart's `:role` (default `:annotation`). Builder pages show
  each named style with `describe/style-rows`.
- Tick labels are drawn without collisions: axes, polar guides, 3D frames and colour bars keep the first label, then
  each not overlapping one drawn (`measure/kept-label-indices` along an axis, `measure/kept-box-indices` for boxes).
  Axes and polar guides draw them all for a scale with `:overlapping-labels :keep` or explicit `:ticks` (without
  `:drop`); colour bars for explicit `:ticks`; 3D frames always thin (no scale spec). Every tick label of a
  position axis (axes, polar guides, parallel axes, 3D frames, the size estimate) is made by `content.axis/tick-label`:
  the `:tick-label` role with the theme's per-axis `[:axis k :tick-label]` merged over it.
- Stat space: histogram, density and smooth take `:stat-space` (`:data` | `:scale` - the statistic in the position
  scale's `scale/stat-transform` space, results back in data units; values the transform can't place are dropped
  and counted); only `:log` transforms; the default is the chart's previous behaviour - `:data` for histogram and
  density, `:scale` for smooth; charts without the option (bin2d, contour, field, scatter jitter) always work in
  `:scale` space; helpers `scale.position/stat-transform-for`, `data.rows/transformed-numbers` (`chart.common`
  holds only `stat-space-option`).
- A per-mark continuous colour value lives in `:color-value` (bar, rect). Among strip charts only bar takes a numeric
  `:color` (it never groups; `:color-stat` - how a bar's rows give it; an explicit colour domain filters no rows -
  a bar whose value lies outside it is drawn in the NA colour); box / violin take a categorical one only. Strip end
  labels go past the larger values (`scale.position/value-direction`, reversed axes too); an item's `:n-label-gap`
  keeps its n label past drawn marks (box outliers).
- Label repel and paths: text `:overlapping-labels :repel` moves a label that overlaps one placed before it (row
  order the priority), or leaves the panel, to the nearest free shift within `:repel-distance` pixels
  (`measure/repelled-placements`: off placed boxes - the whole drawn box with its inset - and other labels' points;
  candidates bounded by the panel), else leaves it out (a label larger than the panel stays put); `:label-leader`
  draws a line (`content.label/draw-leader!` - every chart's leaders, the label's drawn colour) from the point to the
  moved label's turned frame, for a move over 6 px, and no leader crosses a label (such places aren't free). Text
  `:along-path` lays each label along its group's polyline (`content.label/draw-along-path!` over
  `graphics.text-path`): glyphs by arc length along the path offset to the baseline, upright by the whole path's
  direction (vertical - bottom to top), the label's `:background` as a band; text longer than its path is skipped
  (`:anchor` places the text's start, centre or end at `:path-position`). Point along a polyline:
  `polyline/point-at-length`.
- Label collisions: the text chart's `:overlapping-labels :drop` keeps each label not overlapping one kept before it
  (row order is the priority; `measure/kept-box-indices`); builders (sankey, network, alluvial) use it, larger nodes
  first; mapping `:x-span` is a label's room along x in x units (fitted only for text along x: unrotated, unflipped
  cartesian; negative - a mapping error); a radar tick label crossed by another spoke's path is
  dropped (except at a spoke's inner end).
- Pairwise statistics: the registry `cljplot2.data.pairwise/statistics` (fastmath functions with labels, families,
  symmetry, paired or independent samples, `:signed`, descriptions - the option cards' `:method` meanings through
  `pairwise/statistic-meanings`) serves `chart/pairwise` and `chart/correlation` (one
  statistic per panel, cells of `plot/pairs`; shared cell parts in `cljplot2.chart.statistic-cell`) and
  `pairwise/matrix-rows` (a standalone matrix is `chart/tile` over its rows). A cell is always f(x sample, y sample) -
  in `pairs` f(column variable, row variable). Paired statistics take pairwise-complete rows, independent-sample ones
  each variable's own values. Values have no fixed range: `pairs` computes a value-coloured statistic once and gives
  the cells colouring by it its range as `[:scales :color :domain]` (`cljplot2.chart.pairwise/matrix-settings`; a
  matrix has one colour bar - two shaded statistics are an error); signed statistics use a diverging range centred on 0.
- Formulas: every label text (titles, axis titles, tick labels, legends, strips, text charts, chart labels, pairs
  names) may hold LaTeX formulas - a `plot/math` value (`{:cljplot2/math tex}`), a `plot/runs` value
  (`{:cljplot2/runs [pieces]}` - text and formulas on one baseline; vectors are never texts), or `$...$` in strings
  (Pandoc's rule: no space inside the dollars, a closing dollar not followed by a digit; `$$...$$`; `\$` literal;
  text role / label / text chart `:dollar-math false` - all literal). `data.text` parses (pure), `graphics.latex`
  typesets with JLaTeXMath (cached by source, size and weight - bold roles typeset bold; Cyrillic rejected; any
  failure `{:key :text}`; a cached box is drawn under a lock), `content.label` measures and draws lines of runs (one
  metrics shape; baseline at the largest ascent; text without formulas byte-identical). Fitting wraps at spaces and
  cuts in plain pieces only (formulas are whole words); a rebuilt text carries `:dollar-math false`. Formulas of plot
  `:labels` and literal text-chart `:text` are checked at build (`pipeline/formula-errors`, by path); along a path
  they are an error (build or compute). Code passing a text on keeps formula and runs values with
  `text/label-text`, never `str`; text schemas use `text/label-text-schema`; a label value is an options map only by
  `label/label-options?`. Log ticks: scale `:formatter :power-math` (`e^{k}` for base e).
- Text drawn over a fill takes its colour from its text role through `draw/text-color` (role `:color` `:contrast` -
  black or white by the fill; never a hard-coded contrast colour), so a theme colour always wins.

**Scales** are two-phase: phase 1 specs from merged extents + user `:scales`; phase 2 (after layout measurement)
finalized, panel-local position scales. Aesthetic scales (`:color :shape :size :dash :pattern`) are final at
phase 1; legends are derived from them and each chart draws its key glyph via `:legend`; a chart with the common
option `:legend false` draws none, and a legend no chart draws a key into is dropped. Key glyphs raise a positive
line width to the theme's `[:legend :min-key-line-width]` (`theme/key-style`). A scale's `:domain` may leave one end
`nil` (`[lo nil]`, `[nil hi]`): the data fill it, the fixed end keeps the charts' padding and nice rounding; `[nil
nil]` is no domain; every reader goes through `data.rows` (`given-domain`, `full-domain`, `completed-domain`); a free
or sliced facet scale takes no partial domain.

**Layout** is a track grid (`layout/grid`, `layout/plot-grid`) with a measure → solve → re-tick loop; a grid cell may
hold a whole plot (`:arrange`, `plot/pairs`, `:margins`).
- Axis overhangs beside margins and bands: a margin's magnitude axis reserves its start label in its own `-axis-start`
  track; facet panel axes put their start / end overhangs in their column's / row's inner tracks (left / bottom
  margin or band first); `plot-grid/axis-panels` / `axis-panel?` say which facet panel carries an axis.
- Facets with margins: every facet panel gets its margins, computed from its rows and sharing its scale (margin
  content `{:kind :panel :margin side :panel k}`, `:panel-scales` in the margin env); with `:same` scales margins
  align along rows / columns; one magnitude scale per side.
- Statistics per panel-local scale: compute env `:panel-scale-groups {dim [#{panel-key} ...]}` (free / sliced facet
  scales; a margin chart's `:x` gets the shared dimension's); histograms bin each group over its own values (main
  and margin; a count or method per group, explicit breaks shared), otherwise bins are shared by all panels.
- Invalid-value bands: a chart with `:invalid :show` (histogram) returns `:invalid-bands {dim {:label :data}}` -
  its invalid rows (nil, NaN, non-numeric, or unplaceable on the scale) drawn in a band on the left / bottom side of
  the panel (also on a reversed axis) (`content.invalid-band`: `:invalid-band`, `:invalid-band-axis`; theme `[:panel :invalid-band-size]`), labelled
  `:invalid-label` (the first chart's per dimension), bars in the chart's own colour; rows shown there are not
  `:dropped`; a row invalid in two band dimensions is dropped under the first; `:invalid :show` in a margin chart or a
  custom grid is a rule error (no band there).
- `:margins` take four sides (`content.axis/margin-sides`, the one list): a margin shares the main panel's scale
  along it, its magnitude grows away from the panel, the main panel's axes stay outermost; left / bottom margins
  have a measured inner track (as the overhang beside right / top ones), margin axes sit next to their own panel.
- An arranged plot's cell comes only from `plot-grid/arrange-cells` (`:row-spans {plot-index rows}` - a cell
  covering rows, later plots skip it; without `:rows` the rows follow the placement). A `:measured` column holds
  legend cells (`plot-grid/legend-cell?`), measured at the height each cell gets (a wrapped legend widens it); a
  `:measured` row holds `:legend-only` plots at their natural height, scaled down together when they don't fit.
  Sub-plot errors name a legend cell `(legend)`.
- `:equal-panels` rounds correct the tracks towards their mean panel (the total kept; rounds after the first by half
  the difference) and keep the round of the smallest panel spread; a round that doesn't fit ends them.
  Then gap rounds even the space between neighbouring panels (`align/gap-paddings` - each column / row boundary
  raised to the largest, as the arrange's internal `:gap-padding`; panel tracks recomputed around the insets) and
  keep the round of the least panel spread plus gap spread among those within 4 px of the equal-panel spread; a
  matrix too tight keeps uneven gaps.
- Text fitting is one function, `content.label/fitted-label` (wrap, shrink, ellipsis; `:cut false` keeps the text
  whole): strips (no wrap), axis titles (label content `:fit true`, internal; always on - a declared fix) and text
  charts with `:x-span` (no cut) use it. One least size: the text role's `:min-size` (default `:axis-title` 8,
  `:strip` 7, `:annotation` 6, `:sankey-label` 6, `:radial-label` 5); a role without it doesn't shrink.
- Axis tick labels lying end to end (unrotated on a horizontal axis, ±90 on a vertical one) keep theme
  `[:axis k :tick-label-gap]` pixels (default 3) apart (`measure/spans-overlap?` gap); stacked labels are spaced by
  their line height (colour bars, vertical axes) and take no gap.

**Source layout**: `chart/` (chart kinds; `chart.common` - schema pieces several charts share, `chart.legend` - shared key glyphs, `chart.strip` - the strip
chart contract, placement and end labels, `chart.statistic-cell` - correlation / pairwise cells), `data/` (rows, groups, facets,
levels, the strip frame, and row/layout functions such as `data.graph`, `data.tree`, `data.series`,
`data.vector-field`, `data.pairwise`, `data.text` - label text runs and `$...$` parsing), `scale/` (position and aesthetic scales, coords), `graphics/` (fonts, shapes,
polylines, curves, patterns, cells, measuring, text along a path, LaTeX formulas), `content/` (panel, axis, legend, strip, label,
subplot, the 3D frame, invalid-value bands),
`layout/` (grid, plot grid, alignment, pack, treemap, venn), `cache.clj` (bounded caches), `error.clj`, `schema.clj`,
`theme.clj`, `pipeline.clj`, `render.clj`, `describe.clj` (option rows for the book). Builders (`graphs.clj`,
`series_plots.clj`, `multiplot.clj`) turn layout rows into ordinary plot maps; their `:scales` option is deep-merged
(`theme/deep-merge`) into the builder's. `cljplot2.core` is the public API (`build`, `render`, `render-image`,
`cvd-image`, `save`, `find-palette`, `find-gradient`, `palette`, `gradient` - the last five calls to clojure2d - the
builders, and the text values `math`, `runs`).

**Layers** (`layering-test` reads every `ns` form): a namespace requires only its own layer or lower ones, bottom up -
leaves (`error`, `cache`, `data.accessor`, `graphics.font`, `graphics.shape`: they require no other layer; `schema`
needs fonts and shapes as option types, `scale.aesthetic` the shape test) → `schema` → `data`, `scale` → `theme` →
`graphics` → `content` → `layout` → `chart` (`cljplot2.chart` and `chart.*`) → `pipeline`, `render` → builders
(`multiplot`, `graphs`, `series-plots`) → `core`, `describe`. `data/` and `scale/` never draw: they require neither
`clojure2d.core` nor a `graphics` namespace other than the leaves (tested). A chart namespace never requires another
chart namespace except `chart.common` (schema pieces two charts share), `chart.legend` (shared key glyphs),
`chart.strip` (the strip chart contract) and `chart.statistic-cell`; anything general goes lower (statistics to
`data`, geometry to `graphics`, drawing of a frame to `content`).

**Glossary** (option names; `vocabulary-test` checks every `describe` row of charts, builders, API functions and
the theme: banned names, and one type family per name across all owners). One name per concept, one meaning per name:
- `:line-width` - a line mark's stroke in pixels (line, density, ecdf, function, smooth, segment, rule, contour,
  parallel, candlestick wicks, `[:bridge ...]`, theme grid and axis lines); a named sub-part keeps `<part>-width`
  (`:zero-line-width`, `:marker-width`, `:tick-width`) - always pixels; a sub-part's fraction is `<part>-fraction`
  (bullet `:measure-fraction`).
- `:stroke-color` / `:stroke-width` - the outline of a filled mark (width 0 - no outline); layout boxes (theme
  `:panel`, `:legend`, `:strip`) keep `:border-color` / `:border-width`.
- `:width` - a band fraction across the level axis (strips, rect, candlestick bodies); never pixels in a chart.
- `:alpha` - an absolute opacity; a factor multiplying the colours' own alpha is `:alpha-factor` (bin2d, contour,
  field, tile, `[:bridge ...]`); a confidence is `:level` (smooth, acf: 0.95). One exception: area fills of line,
  density, smooth, ribbon and parallel radar (`:fill :color`, stacked areas without a fill) and polygon fills take the
  mark's colour faded by `:alpha` - its own alpha times `:alpha` (`polyline/area-color`, `draw/faded`).
- `:fill` - a colour (`:color` = the mark's colour at `:alpha` - faded by it in the area fills above, `:none`);
  flags and accessors get other names (contour `:filled`, alluvial `:strand-color`).
- `:padding` - span fractions `{dim [lo hi]}` only; pixels between marks are `:gap` (pack, treemap, theme), inside a
  label box `:inset` (text roles, the text chart).
- `:gap` - a gap: a fraction (sankey, alluvial, waffle) or pixels (pack, treemap, theme, line / segment `:start-gap`
  `:end-gap`); chord's gap in degrees is `:gap-angle`.
- `:n` - a count of samples, points or intervals (pp/qq, density, smooth, violin, shingle) or grid points per axis
  of a sampled field (contour, field, surface, arrows); `:bins` - binning (histogram, bin2d, cloud). Option values
  keep their own keys (`:sample-step {:count n}`).
- `:order` (an enum or ids) + `:descending` (a boolean) - ordering; never `:sort`, `:order-direction`, `:node-sort`.
- `:value-format`, `:n-format` - a chart's format string (checked at build: `schema/number-format`,
  `schema/count-format`) or function of the value; scales take `:formatter`.
- `:inner-radius` - a radius fraction (chord, sequences; violin `:inner` names its inner marks); `:variables` - the
  accessors of wide rows' variables (parallel, alluvial, pairs), their display names `:variable-names`; `:columns` -
  a count of grid columns (arrange, facets, waffle); `:axes` - a 3D or parallel flag; `:cell-shape` - a waffle cell's
  shape (`:shape` is a point shape); `:length-scale` - the arrows' length factor (`:scale` is violin's width mode).
- `:invalid` - what a chart does with rows whose mapped value is invalid: `:drop` (default, counted in `:dropped`)
  or `:show` (drawn in a band beside the axis, labelled `:invalid-label`). Invalid means nil, NaN, non-numeric or
  unplaceable on the scale; NA (`:na-panel`, `:na-level`, `[:palette :na]`) means nil only.
- `:overlapping-labels` - `:keep` / `:drop` (scales, text); `:repel` only for the text chart (labels move to free
  places - tick labels never move); `:repel-distance` - pixels, the farthest a repelled label moves.
- `:dollar-math` - a text role's, label's or text chart's switch of `$...$` formulas (default on; `false` keeps
  dollars literal; an inline `nil` style key keeps the role's value).
- `:as` - the output key of a data function; booleans never end in `?` (also data fields a function returns and keys of internal data -
  `:closed`, `:leaf`, `:flip`; only local names and predicate functions end in `?`); one flag one key; label toggles are named
  after what they label (`:labels`, `:group-labels`, `:n-label`, `:value-label`).
- `:none` always means draw nothing (also an empty `pairs` slot); automatic black-or-white contrast is `:contrast`;
  `nil` means the default. The one exception: `sequence-levels :end nil` - no end marker.

**Contracts**:
- `:dropped` (chart compute): one key per mapped dimension, each row counted once, plus only `:group`, `:field`,
  `:duplicate`, `:value`, `:set`, `:variables` (the `cljplot2.chart` ns doc); rows a chart draws in an invalid-value
  band (`:invalid :show`) are not dropped; data functions outside charts report a plain count.
- Checks: option shapes and conflicts between options are schemas - closed maps, `schema/with-rules` for relations
  between keys - so `build` reports them before any stage runs; compute checks only what depends on the data, the
  theme or the plot (`fail!` there). Every data chart calls `rows/check-mapped!` (all-invalid mapped data throws
  `{:key :mapping}`); ecdf, density, histogram and probability check by counting rows; annotation charts (rule,
  text, rect) draw what they can. Dropped rows are counted in `:dropped`.
- Errors about the mapping have one shape: `{:key :mapping :in [:mapping k] :value v}` (strip charts name the user's
  dimension, not the strip frame's); other error keys name an option, never a pipeline stage.
- Options: `nil` means the default (read with `data/option` / `data/options-with-defaults`; `:or` only for keys their
  schema doesn't let be `nil`). Builders and API functions whose options have no required key take the options map
  optionally; the parameter is `options`, rows are `rows`.
- Kept on purpose: scatter, cloud and tile keep rows in data order (no `group/split`: it reorders by group, changing
  overplotting); the colour bar widens a single value to `[v, v + 1]` (its ticks), other ranges by
  `rows/widened-range`.

## Notebooks, demos, data

- `notebooks/reference/*.clj` (four reference parts - Concepts, Charts, Builders, Data functions - before Use cases;
  chapter titles name their topic without the part's name; the galleries are lattice, Vega and cljplot;
  Quarto books have one level of parts), `notebooks/use_cases/*.clj` and
  `notebooks/galleries/*.clj` are Clay notebooks; order in `notebooks/chapters.edn`. Reference option tables come
  only from `nu/options-table` (option cards on migrated pages - see below) over `cljplot2.describe` rows (never hand-written; code hidden), one section at a
  time for sliced rows, each table with a sentence or two and followed by its example (a chart or a value);
  `reference-notebook-test` checks every schema of `describe/plot-level-schemas`, shared group, theme key (but
  `:chart`) and section has a table (cards on migrated pages) of at most 12 rows, each followed by an example. On every book page (reference, use cases, galleries;
  `book-data-test`) every dataset or data defined is shown right after its `def`, and every dataset read is shown by
  some table (of the dataset, or of a var defined from it), with `nu/data-table` (first 100 rows, a last row of `…` when cut, code hidden; a box of
  class `data-table` - compact, left-aligned, scrolling, styled in `clay.edn` `:include-in-header`); a function's result
  is shown with `nu/result-table` (the same box, its call shown). Chart pages (`charts_*.clj`, ten chapters grouped by what
  they show: points, lines and areas; annotations; bars; distribution per level; distribution of one variable; fitted
  curves; 2D bins and fields; pairwise statistics; shares and sets; multivariate and 3D - kinds per page in
  `reference-notebook-test/chart-chapters`): per kind a `;; ## kind` heading (a migrated kind: its concept, see
  below), its mapping table, its own
  options (`describe/own-rows` - shared groups left to the Shared options chapter, sliced by `:cljplot2/section` when
  over 12 rows), each followed by an example, then `nu/shared-groups` (a Shared options box, a point per group linking to its `###`
  heading there; on a migrated page it opens the section, right after the opening sentences);
  `chart-coverage-test`: every own top-level option set in a literal `(chart/kind {...})` map on the page and every
  value of an enum of at most 6 values given (a default counts when a call omits the option). Builder pages
  (`builders_*.clj`) likewise (own options + shared groups); data function pages (`data_*.clj`, a `;; ## concept`
  heading per `:cljplot2/api` function, `describe/concepts`, its full options rows); the same coverage rule over a call's literal last
  argument map (`reference-notebook-test` `builder-chapters`, `data-chapters`); `specific_cases.clj` shows internal
  namespaces by example.
  Option cards (pages being rewritten for readability; chart kinds in `test-util/migrated`, builders in
  `test-util/migrated-builders`, data functions in `test-util/migrated-functions`, shared groups in
  `test-util/migrated-groups`, top-level theme keys in `test-util/migrated-theme-keys` (all but `:chart` - the Theme
  chapter's Chart section links the chart pages) - the switches; theme cards are `nu/option-cards :theme
  (describe/theme-rows k)` (the rows under `k`, without its own row; each example printed as a plot key `{:theme
  {...}}`, drawn on `option-cards-test/theme-fixtures` plots - text roles on the chart or builder drawing them); a
  group's cards on Shared options print its representative's call
  (`chart/line`, `'(plot/network edges)`) and `option-cards-test` checks them through it (`group-fixtures`); a group a
  data function owns (`:owner` in `describe/fragments`) is documented in that function's section, Shared options
  boxes link there; a style group (`:style true`) gives its rows the default text `The chart's theme style.`; `describe/concepts`
  keyed by kind keyword, builder symbol or data function's qualified symbol; data function sections show cards of all
  their `describe/options` rows - a function owns its shared group's rows, whose texts read in the card style for the
  builders forwarding them too):
  `nu/option-cards call rows [section]` (`call` a symbol, `chart/smooth`, or a builder or function with its arguments
  before the options as placeholders shared with the test fixtures, `'(plot/network edges)`, `'(graph/layout g)`) instead of options tables (per option a boxed card: a header with the option
  and its default, an example call, then its description, a bullet per value or form with its meaning and the other
  values of a long list on one line), each card group after a sentence or two on what the options are for, and
  `nu/mapping-list` (a titled Mapping box; a section of sliced mapping rows) instead of the mapping table; the section heading names the concept
  (`describe/concepts`, e.g. `## Smoothing`), its first sentence names the function and says what it's for; type
  names link to the Types chapter (`nu/type-entries`, from `describe/documented-types` - `schema/named-types` and higher layers' documented named
  types). Links into a reference section
  are checked against its headings (`option-cards-test/reference-links-test`).
  Conventions: a heading, 1-3 lines, code ending in `plot/render-image`; every random value from a seeded rng
  (`(r/rng :mersenne n)`); no notebook plotting helpers. `book-data-test` checks the rest:
  - images at most 1000×800 - a bigger one states its reason in the prose and is listed in
    `book-data-test/oversize-images`;
  - source notes: an example from an outside source has a note with `:source` and its `:url` (`nu/source-note`
    throws without it); examples written for cljplot2 have no `:source` - a note crediting their data, or none for
    synthetic data; every dataset a page reads is credited by some note's `:dataset` (a key or a vector of keys);
  - data shown: rows, columns and samples (more than 12 numbers or number vectors);
  - use cases in the R Graph Gallery site order - distribution, correlation, ranking, part of a whole, evolution,
    flow, colours - in `chapters.edn`, the index, `specific_cases` and the README.
- Book prose (reference rewrites): natural English as a person explaining - the actor named (no hiding passive), the
  verbs developers use for code (returns, takes, parses, checks - not "gives nil", "writes", "tells"), subject and
  verb first, no comma chains or habitual ", and" joins, short names (`chart/smooth`, not fully qualified), no spec
  jargon, nothing said twice (no intro that lists what the cards show), no verbless fragments; not longer than
  before; every claim checked against the code.
- `docs/` holds only Clay output (wiped on every build); the `:clay` classpath is `notebooks/` only, so notebooks
  can't require `dev/` code.
- Datasets: `notebooks/data/` + `datasets.edn` (source, licence, via); load with `(nu/dataset :key)`. A test checks
  every dataset is rows of maps (JSON arrays of objects or CSV). Demos read the same files via `cljplot2.demo.data`.
- Demos: `dev/cljplot2/demo/<name>.clj` each define `images` (`[[file-name plot options] …]`) and optionally
  `output-dir`; register new demos in `cljplot2.demo/demos`. `img/` (logo, README example) is tracked;
  `results/` (every demo image) is tracked too (since `a332745`).

## Code conventions

- Reuse existing functions before writing new ones; small, idiomatic kebab-case functions with docstrings.
- Every namespace sets `*warn-on-reflection*` and `*unchecked-math* :warn-on-boxed`; `clojure -M:check` must stay
  silent. Functions with primitive type hints take at most 4 arguments.
- Never mutate a caller's arrays.
- Option schemas are closed; new options are off by default (existing images unchanged).
- Every schema entry documents itself (`cljplot2.schema` ns doc): `:description` (one sentence, lowercase, no
  period - option tables; or one or two full sentences, at most 300 characters, saying what the option controls -
  option cards; a text a migrated chart shares with charts still on tables may read in the card style; migrated
  charts take the card style),
  `:default` for a constant behaviour default (never on style entries - the theme holds those) or
  `:cljplot2/default-doc` for a computed one; `:cljplot2/internal` for keys users don't set; shared groups via
  `schema/in-fragment` (registered in `cljplot2.describe/fragments`), `:cljplot2/entries-fragment` for a map whose
  entries a group documents; `:cljplot2/section` slices the table of the map holding the entry (in one map all user
  entries or none - for a chart, its own entries; nested entries inherit it unless their own map is sliced -
  `describe/top-level-rows` gives a schema's own table; `schema/in-section` tags shared entries for one map); schema properties `:cljplot2/type-doc` (short type text of a large map or composite)
  and `:cljplot2/merged` (a map merged over defaults - theme, render options). `cljplot2.describe` turns schemas into
  option rows for the book's reference part;
  `schema-metadata-test` checks every chart and plot-level entry (plot, scales, facets, labels, coord, theme ...) is
  described and every `:default` is the real one (pixel-equal renders); theme entries take their defaults from
  `default-theme` (a row with a default text shows the text only), text roles their descriptions, sections and
  examples from `theme/text-roles`; text style keys have one description map (`theme/text-style-card-props`: the
  `:text-style` group, a role's style).
  For option cards an entry also gives `:cljplot2/value-meanings` (value meanings), `:cljplot2/form-meanings` (meanings of
  other forms, keyed by named type or malli type), `:cljplot2/other-values` (one meaning for a long list's rest) and
  `:cljplot2/example` (with `:cljplot2/example-context` - options the example needs to be valid or to act, a
  container's applying to its entries; a nested example sits inside its container's theme default,
  `describe/example-options`; give an explicit example when the default lives only in a default text, or the
  derived example shows it); a named type gives
  `:cljplot2/type-name`, `:cljplot2/type-meaning` and, for a predicate over values, `:cljplot2/value-set` /
  `:cljplot2/other-types` (value and form meanings shared by its entries); card forms read as short type text
  (`vector of open fractions`, never validation-message text; `:or` / `:multi` alternatives as `X or Y` (a `schema/forbidden` branch - a form rejected with its own message,
  e.g. histogram quantile bins - is none), a vector of them `vector of Xs or Ys`; `:int` / `:double` bounds as words
  (`integer at least 2`); rules of `schema/with-rules` (`schema/rule?`) are left out - the description states the
  relation; a map schema names itself with `:cljplot2/type-name`, e.g. `mapping`, and with `:cljplot2/entries-fragment`
  links its card form to the shared group documenting its keys - a text style, a grid line's line style). `option-cards-test` checks a migrated chart, builder or data function (card owners: fixture plots of charts, fixture
  calls of builders and data functions - a function's example must change its result, `:compare :value`): sentences, a
  meaning for every value of a list of at most 6 (a longer list's rest under `:other-values`), for every form of an
  option taking several, a default for every optional option, a valid example for every option that changes the
  picture of a small fixture plot (`examples-act-test`; an option acting only beyond the chart's options - a plot scale -
  is listed with its reason; one the chart can't draw without - `:f`, a rule's positions - is listed in `needed-to-draw`
  and only has to render); an option whose card documents `:none` renders with it (`none-colours-test`; a colour that
  can't draw nothing takes `schema/solid-color`);
  `migrated` equals the keys of `describe/concepts` and each migrated section uses cards, never an options table, and a
  mapping list when the chart has mapping keys.
- Builders (`cljplot2.core` - `graphs`, `series_plots`, `multiplot`) and user-facing data functions (var metadata
  `:cljplot2/api true`) take a closed options schema in var metadata `:cljplot2/options` (also on the `plot/…`
  var) and check it first - `(schema/check-options! schema options)` before reading their data; a builder's schema
  is `(schema/options-map own forwarded... multiplot/builder-entries)` (own entries win), a forwarded function's
  entries a shared group (`:graph/sankey-layout` ...) registered in `cljplot2.describe/fragments`; data checks stay
  `fail!`. They document their positional parameters in var metadata `:cljplot2/arguments` (the longest arity's, in
  order, without the options map: `:name` - the parameter's symbol, a card-style `:description`, `:optional` for one a
  shorter arity leaves out, `:columns` - the columns rows hold, keyword keys or a string naming a column an option
  picks - and `:keys` of a map argument) and `:cljplot2/returns` (a card-style sentence); a section shows them with
  `nu/argument-list` right after its opening, and its card calls name the parameters as placeholders.
  `reference-r2-test` checks every builder and API function.
- Errors: `cljplot2.schema/fail!` with `{:key option …}`; namespaces `cljplot2.schema` itself requires (e.g.
  `graphics.shape`) use `cljplot2.error/fail!` (the same function, no dependency cycle).
- Docstrings follow the user's `docstring` skill (statement, context, parameters with map keys and defaults, return
  value, corner cases and errors, `[[links]]`; no implementation details). A `def`'s doc goes positionally after
  the name, metadata before it: `(def ^{:private true :const true :tag 'double} name "..." value)`; only `defonce`
  (no docstring arity) keeps it in metadata: `(defonce ^{:private true :doc "..."} name value)`.

### Clojure style (user review)

- Primitive hints work in destructuring (`{:keys [^double x]}`, `[[^double a ^double b]]`); prefer them to
  `(double x)` rebinding.
- A hint truncates: a `^long` argument receiving a double drops the fraction - hint what the value is (e.g.
  `m/slice-range` yields doubles, so `(fn [^double t] ...)`).
- A function returning a primitive (`^double`) needs no explicit conversion of its result.
- Arithmetic and comparisons on numbers with `fastmath.core` (`m/+ m/* m/< m/== m/max m/round m/pos? m/one? ...`),
  not `Math/...` or `clojure.core` operators.
- Primitive longs in loops are always forced: `(long ...)`.
- A primitive put into a Clojure collection is boxed anyway - don't force it with `double` / `long` there.
- No `boolean` coercion: `nil` and `false` are false, everything else true; predicates return truthy / `nil`. A
  returned data field with a documented boolean default stays `true` / `false` for a nilable option: `(true?
  directed?)` - `:or` defaults don't apply to a key given as `nil`.
- `m/inc` / `m/dec` for doubles, `m/long-inc` / `m/long-dec` / `m/long-add` / `m/long-sub` / `m/long-mod` for longs;
  `v/mx` / `v/mn` / `v/sum` instead of `reduce max` / `min` / `+` for input known non-empty (they throw on an empty
  seq); where empty input has a meaning, keep its initial value: `(reduce m/max init xs)`.
- Collections are realized when consumed: `vec` / `mapv` only for stored results (a chart's computed data, extents,
  padding), indexed access, `peek` / `pop` / `assoc`, several consumers, or realization inside a dynamic binding or
  a canvas; intermediates stay lazy (`map`, `filter`, `keep`, `for`).
- fastmath constants instead of computing them: `m/HALF_PI`, `m/QUARTER_PI`, `m/-QUARTER_PI`, `m/TWO_PI`, `m/SQRT5`
  ... (`position/golden-angle` for the golden angle).
- `m//` of two longs is integer division (`(m// 1 4)` is 0): for a fraction at least one operand must be a double.
- `m/slice-range` for evenly spaced values (inclusive ends; one value - the midpoint; zero - empty).
- `fastmath.java.Array` (`Array/aget`, `Array/aset`, `Array/add`) for primitive arrays; object arrays keep core
  `aget` / `aset`.
- Rotations from fastmath (`fastmath.matrix` rotation matrices, `fastmath.vector`, `fastmath.quaternion`), built
  once per view.
- No shadowing: a local never reuses an enclosing local's name, an earlier binding's name, or the name of a
  `clojure.core` or same-namespace var (`{layout-kind :layout}`, not `{:keys [layout]}` beside `layout`).
- Docstring openings are statements (`Gives ...`, `Tests whether ...`), never questions.
- Reuse fastmath, clojure2d and cljplot2 functions before writing new ones (e.g. `aesthetic/level-key`).
- Compute constants once (a `def`, e.g. `(def ^{:private true :const true :tag 'double} frame-padding+ (m/inc
  frame-padding))`), never per call; `:const` defs carry `:tag`.
- `delay` only to defer a computation that may not be needed; a value used anyway is computed once in a `let`.
- Hard-coded colours are made once with `c/color` (`(c/color r g b a)`) or `c/gray` (`(c/gray v a)`), not vectors;
  users may pass any colour representation. clojure2d colour functions coerce their arguments - no explicit
  `c/to-color` before them. Tests compare colours through `c/to-color` on both sides (a `Vec4` is never `=` to a
  vector).
- Caches of costly values asked for again: a `defonce` `cljplot2.cache/lru-cache` with a hinted `:const` threshold,
  read only through `cache/lookup-or-compute` (least recently used evicted, computed once per key); not Java
  collections, not an unbounded global `memoize` (a `memoize` local to one draw call, e.g. `draw/pattern-fill-cache`,
  is fine). A hit is a `swap!` (~0.5 µs): fine per plot or per drawn mark, not in a tight loop that draws nothing.
- General drawing comes from clojure2d, never direct Java2D (one exception: `graphics.latex` draws JLaTeXMath boxes
  on the canvas's `Graphics2D` - clojure2d has no function for them; JLaTeXMath's GPL-only Greek / Cyrillic font jars
  are excluded in `deps.edn`, `registerExternalFont` never called): `c2d/add-clip` intersects the clip (`c2d/clip`
  replaces it), `c2d/with-isolated-canvas` draws with every state change discarded afterwards (`push-matrix` restores
  only the transform), `c2d/set-rendering-hint` sets one hint, `sh/transform-shape`, `c2d/shape->points`
  (flattening), `c2d/path-def->shape`, `sh/svg-path`, `sh/text-outline`, `sh/rounded-rect`. A general drawing
  function clojure2d lacks is added there (the user commits clojure2d, then `lein install`); a drawing interop call
  kept in cljplot2 has a comment with its reason (Java value queries - `.getBounds2D`, image sizes, `Font.` /
  `.deriveFont` - need none).
- Style defaults (colours, sizes, label boxes) belong to the theme, never private vars or inline literals in
  builders or charts. Exceptions: defaults already exposed as options (`:edge-color`, acf / horizon `:colors`, the
  alphas), scale defaults (`node-size-range`, `arc-width-range`), layout geometry and the debug palette.

### General rules (from the design notes)

- Theme values: `nil` - inherit / the computed default, `:none` - nothing drawn, `:contrast` - black or white by the
  fill, `:stretch` - the space left (colour bar length). Option and theme maps are closed; open are only value maps
  users extend (shape maps, pattern maps, dash arrays).
- Orientation comes from the mapping: strip charts put their levels on the categorical mapping axis (`:level-axis`
  wins); `:flip` and polar coords are drawing concerns only - charts compute in their own `:x` / `:y` frame.
- Polar shapes that follow the circle are built in range space and turned into pixels by `cljplot2.scale.coord`.
- 3D charts (`:coords #{:three-d}`) draw only with other 3D charts; several of them need explicit `:domain`s; their
  primitives are drawn far to near in one scene per panel.
- Point functions (projections, placements) take and give `Vec2`.
- Randomness: a chart's `:rng` option goes through `rng/for-build`, and random values are drawn in compute, so every
  render of one build draws the same image. Interpolators drawing random values while fitting (`:kriging`,
  `:microsphere-projection`) get a fresh generator of a fixed seed per fit (`data.interpolation/fit-fn`) unless their
  params give an `:rng` - a fit is a function of its points.
- Default categorical palette (`theme/default-palette`, `[:palette :color]`): 14 colours checked for colour vision
  deficiencies - Okabe-Ito ordered so the second colour reads as text (contrast >= 3 on the background), a darker
  yellow, a gray darker than `[:palette :na]`, then lighter versions of the first six (the blue turned toward violet,
  away from sky); `default-palette-test` keeps the checks. Every single-colour default (chart styles, named styles,
  builder defaults) is `theme/default-color`, the palette's first colour. Examples set another palette only on
  purpose (a palette demonstration, a source's colours), never to work around the default - `palette-override-test`
  lists each with its reason. Text over a fill (`:contrast`) is black or white by the larger WCAG contrast ratio.
- A `nil` facet value is a real level (the NA panel, last; `:facets {:na-panel false}` drops it).
- `case` matches boxed values by `=`: a double never matches a long literal - coerce first; compare numbers across
  types with `m/==`, not `=` or set membership (`(contains? #{1} 1.0)` is false).
- Padding: a chart's `:padding` are fractions of the span, `:padding-explicit` its requests (see
  `data/padding-stats`); an extent with `:companion true` counts only beside another chart's extents; an aesthetic
  extent with `:fallback true` only when no other chart has extents of that aesthetic (correlation / pairwise cells
  give their statistic's range).
- Datasets: every file in `notebooks/data/` has an entry in `datasets.edn` (source, licence, via); scripts that make
  data are the only scripts kept in the repo; every book example drawn from an outside source credits it with
  `nu/source-note` (`:source` and `:url`).

## Related repositories

fastmath, wadogo and clojure2d are SNAPSHOT dependencies, often changed alongside cljplot2 and installed locally.
Never commit to the fastmath repository (commits must be signed - the user commits). After a local `lein install` of
fastmath, reinstall clojure2d. The user commits cljplot2 changes.
