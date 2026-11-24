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
review rounds, their specs and implementation notes). Read them when a decision's rationale is unclear.

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
```

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
  optional `:axis-titles`, `:scale-defaults`, `:padding-px` …); `draw` renders one panel's computed data.
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
  `:projection-alpha-factor`) is shown on the chart page with `describe/style-rows kind keys`. The theme style is
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
  `:drop`); colour bars for explicit `:ticks`; 3D frames always thin (no scale spec).
- Pairwise statistics: the registry `cljplot2.data.pairwise/statistics` (fastmath functions with labels, families,
  symmetry, paired or independent samples, `:signed?`) serves `chart/pairwise` and `chart/correlation` (one
  statistic per panel, cells of `plot/pairs`; shared cell parts in `cljplot2.chart.statistic-cell`) and
  `pairwise/matrix-rows` (a standalone matrix is `chart/tile` over its rows). A cell is always f(x sample, y sample) -
  in `pairs` f(column variable, row variable). Paired statistics take pairwise-complete rows, independent-sample ones
  each variable's own values. Values have no fixed range: `pairs` computes a value-coloured statistic once and gives
  the cells colouring by it its range as `[:scales :color :domain]` (`cljplot2.chart.pairwise/matrix-settings`; a
  matrix has one colour bar - two shaded statistics are an error); signed statistics use a diverging range centred on 0.
- Text drawn over a fill takes its colour from its text role through `draw/text-color` (role `:color` `:contrast` -
  black or white by the fill; never a hard-coded contrast colour), so a theme colour always wins.

**Scales** are two-phase: phase 1 specs from merged extents + user `:scales`; phase 2 (after layout measurement)
finalized, panel-local position scales. Aesthetic scales (`:color :shape :size :dash :pattern`) are final at
phase 1; legends are derived from them and each chart draws its key glyph via `:legend`.

**Layout** is a track grid (`layout/grid`, `layout/plot-grid`) with a measure → solve → re-tick loop; a grid cell may
hold a whole plot (`:arrange`, `plot/pairs`, `:margins`).

**Source layout**: `chart/` (chart kinds; `chart.common` - schema pieces several charts share, `chart.legend` - shared key glyphs, `chart.strip` - the strip
chart contract, placement and end labels, `chart.statistic-cell` - correlation / pairwise cells), `data/` (rows, groups, facets,
levels, the strip frame, and row/layout functions such as `data.graph`, `data.tree`, `data.series`,
`data.vector-field`, `data.pairwise`), `scale/` (position and aesthetic scales, coords), `graphics/` (fonts, shapes,
polylines, curves, patterns, cells, measuring), `content/` (panel, axis, legend, strip, label, subplot, the 3D frame),
`layout/` (grid, plot grid, alignment, pack, treemap, venn), `cache.clj` (bounded caches), `error.clj`, `schema.clj`,
`theme.clj`, `pipeline.clj`, `render.clj`, `describe.clj` (option rows for the book). Builders (`graphs.clj`,
`series_plots.clj`, `multiplot.clj`) turn layout rows into ordinary plot maps; their `:scales` option is deep-merged
(`theme/deep-merge`) into the builder's. `cljplot2.core` is the public API (`build`, `render`, `render-image`,
`cvd-image`, `save`, `find-palette`, `find-gradient`, `palette`, `gradient` - the last five calls to clojure2d - and the
builders).

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
  field, tile, `[:bridge ...]`); a confidence is `:level` (smooth, acf: 0.95).
- `:fill` - a colour (`:color` = the mark's colour at `:alpha`, `:none`); flags and accessors get other names
  (contour `:filled`, alluvial `:strand-color`).
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
- `:as` - the output key of a data function; booleans never end in `?`; one flag one key; label toggles are named
  after what they label (`:labels`, `:group-labels`, `:n-label`, `:value-label`).
- `:none` always means draw nothing (also an empty `pairs` slot); automatic black-or-white contrast is `:contrast`;
  `nil` means the default. The one exception: `sequence-levels :end nil` - no end marker.

**Contracts**:
- `:dropped` (chart compute): one key per mapped dimension, each row counted once, plus only `:group`, `:field`,
  `:duplicate`, `:value`, `:set`, `:variables` (the `cljplot2.chart` ns doc); data functions outside charts report a
  plain count.
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

- `notebooks/reference/*.clj` (four Reference parts - concepts, charts, builders, data functions - before Use cases;
  Quarto books have one level of parts), `notebooks/use_cases/*.clj` and
  `notebooks/galleries/*.clj` are Clay notebooks; order in `notebooks/chapters.edn`. Reference option tables come
  only from `nu/options-table` over `cljplot2.describe` rows (never hand-written; code hidden), one section at a
  time for sliced rows, each table with a sentence or two and followed by its example (a chart or a value);
  `reference-notebook-test` checks every schema of `describe/plot-level-schemas`, shared group, theme key and section
  has a table of at most 12 rows, each followed by an example. On every book page (reference, use cases, galleries;
  `book-data-test`) every dataset or data defined is shown right after its `def`, and every dataset read is shown by
  some table (of the dataset, or of a var defined from it), with `nu/data-table` (first 100 rows, a last row of `…` when cut, code hidden; a box of
  class `data-table` - compact, left-aligned, scrolling, styled in `clay.edn` `:include-in-header`). Chart pages (`charts_*.clj`, kinds per
  page in `reference-notebook-test/chart-chapters`): per kind a `;; ## kind` heading, its mapping table, its own
  options (`describe/own-rows` - shared groups left to the Shared options chapter, sliced by `:cljplot2/section` when
  over 12 rows), each followed by an example, then `nu/shared-groups` (links to the group's `###` heading there);
  `chart-coverage-test`: every own top-level option set in a literal `(chart/kind {...})` map on the page and every
  value of an enum of at most 6 values given (a default counts when a call omits the option). Builder pages
  (`builders_*.clj`) likewise (own options + shared groups); data function pages (`data_*.clj`, a `;; ## ns/name`
  heading per `:cljplot2/api` function, its full options table); the same coverage rule over a call's literal last
  argument map (`reference-notebook-test` `builder-chapters`, `data-chapters`); `specific_cases.clj` shows internal
  namespaces by example.
  Conventions: a heading, 1-3 lines, `nu/source-note` with `:url`, code ending in `plot/render-image` at most
  1000×800; every random value from a seeded rng (`(r/rng :mersenne n)`); no notebook plotting helpers.
- `docs/` holds only Clay output (wiped on every build); the `:clay` classpath is `notebooks/` only, so notebooks
  can't require `dev/` code.
- Datasets: `notebooks/data/` + `datasets.edn` (source, licence, via); load with `(nu/dataset :key)`. A test checks
  every dataset is rows of maps (JSON arrays of objects or CSV). Demos read the same files via `cljplot2.demo.data`.
- Demos: `dev/cljplot2/demo/<name>.clj` each define `images` (`[[file-name plot options] …]`) and optionally
  `output-dir`; register new demos in `cljplot2.demo/demos`. `img/` (logo, README example) is tracked;
  `results/` is gitignored.

## Code conventions

- Reuse existing functions before writing new ones; small, idiomatic kebab-case functions with docstrings.
- Every namespace sets `*warn-on-reflection*` and `*unchecked-math* :warn-on-boxed`; `clojure -M:check` must stay
  silent. Functions with primitive type hints take at most 4 arguments.
- Never mutate a caller's arrays.
- Option schemas are closed; new options are off by default (existing images unchanged).
- Every schema entry documents itself (`cljplot2.schema` ns doc): `:description` (one sentence, lowercase, no
  period), `:default` for a constant behaviour default (never on style entries - the theme holds those) or
  `:cljplot2/default-doc` for a computed one; `:cljplot2/internal` for keys users don't set; shared groups via
  `schema/in-fragment` (registered in `cljplot2.describe/fragments`), `:cljplot2/entries-fragment` for a map whose
  entries a group documents; `:cljplot2/section` slices the table of the map holding the entry (in one map all user
  entries or none - for a chart, its own entries; nested entries inherit it unless their own map is sliced -
  `describe/top-level-rows` gives a schema's own table; `schema/in-section` tags shared entries for one map); schema properties `:cljplot2/type-doc` (short type text of a large map or composite)
  and `:cljplot2/merged` (a map merged over defaults - theme, render options). `cljplot2.describe` turns schemas into
  option rows for the book's reference part;
  `schema-metadata-test` checks every chart and plot-level entry (plot, scales, facets, labels, coord, theme ...) is
  described and every `:default` is the real one (pixel-equal renders); theme entries take their defaults from
  `default-theme`, text roles their descriptions and sections from `theme/text-roles`.
- Builders (`cljplot2.core` - `graphs`, `series_plots`, `multiplot`) and user-facing data functions (var metadata
  `:cljplot2/api true`) take a closed options schema in var metadata `:cljplot2/options` (also on the `plot/…`
  var) and check it first - `(schema/check-options! schema options)` before reading their data; a builder's schema
  is `(schema/options-map own forwarded... multiplot/builder-entries)` (own entries win), a forwarded function's
  entries a shared group (`:graph/sankey-layout` ...) registered in `cljplot2.describe/fragments`; data checks stay
  `fail!`. `reference-r2-test` checks every builder and API function.
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
- General drawing comes from clojure2d, never direct Java2D: `c2d/add-clip` intersects the clip (`c2d/clip`
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
  render of one build draws the same image.
- A `nil` facet value is a real level (the NA panel, last; `:facets {:na-panel false}` drops it).
- `case` matches boxed values by `=`: a double never matches a long literal - coerce first; compare numbers across
  types with `m/==`, not `=` or set membership (`(contains? #{1} 1.0)` is false).
- Padding: a chart's `:padding` are fractions of the span, `:padding-explicit` its requests (see
  `data/padding-stats`); an extent with `:companion? true` counts only beside another chart's extents.
- Datasets: every file in `notebooks/data/` has an entry in `datasets.edn` (source, licence, via); scripts that make
  data are the only scripts kept in the repo; every book example drawn from an outside source credits it with
  `nu/source-note` (`:url`).

## Related repositories

fastmath, wadogo and clojure2d are SNAPSHOT dependencies, often changed alongside cljplot2 and installed locally.
Never commit to the fastmath repository (commits must be signed - the user commits). After a local `lein install` of
fastmath, reinstall clojure2d. The user commits cljplot2 changes.
