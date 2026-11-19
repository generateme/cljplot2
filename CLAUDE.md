# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

cljplot2 is a Clojure plotting library: plots are plain maps of data, mappings and charts, built and rendered by
functions (no DSL, no grammar of graphics). Pure JVM - it renders `BufferedImage`s / image files. Built on fastmath
(statistics, distributions, layouts), wadogo (scales), clojure2d (drawing, colours) and malli (validated options).
The book (Clay + Quarto) is published from `docs/` at https://generateme.github.io/cljplot2/.

Design history, specs and the roadmap live in the user's Obsidian vault: `~/notes/Topics/cljplot2/`
(`01 Build Steps/Architecture Design.md`, `03 Use Case Notebooks/Roadmap - Use Case Notebooks.md` - section status
and backlog). Read them when a decision's rationale is unclear.

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
(`pipeline/validate!`: render options, plot, merged theme, every chart's `:schema`, cross-key rules) and throws one
`ex-info {:key :in :value :errors}` listing all errors - charts don't validate their own input.

**Chart contract** (`cljplot2.chart` ns doc): a chart value is a plain map `{:kind :options :compute :draw :legend
:schema}` with top-level functions, so built plots print and compare as data.
- `compute` gets all panels' grouped rows and returns stats (`:data` per panel, `:extents`, `:padding`, `:dropped`,
  optional `:axis-titles`, `:scale-defaults`, `:padding-px` …); `draw` renders one panel's computed data.
- Draw through `cljplot2.scale.position/projection` (or the coord helpers) - never assume `:x` is horizontal
  (`:flip`, polar `:coord`).
- Optional keys: `:computed-aesthetics` (e.g. a computed colour), `:coords` (`#{:cartesian :polar}`),
  `:inherit-mapping :facets` (annotation charts inherit only facet keys; data under `facet/all-panels` is drawn in
  every panel).
- Every built-in kind has a theme entry `[:chart kind]` checked against `chart/builtin-styles`.

**Scales** are two-phase: phase 1 specs from merged extents + user `:scales`; phase 2 (after layout measurement)
finalized, panel-local position scales. Aesthetic scales (`:color :shape :size :dash :pattern`) are final at
phase 1; legends are derived from them and each chart draws its key glyph via `:legend`.

**Layout** is a track grid (`layout/grid`, `layout/plot-grid`) with a measure → solve → re-tick loop; a grid cell may
hold a whole plot (`:arrange`, `plot/pairs`, `:margins`).

**Source layout**: `chart/` (chart kinds), `data/` (rows, groups, facets, and row/layout functions such as
`data.graph`, `data.tree`, `data.series`, `data.vector-field`), `scale/`, `layout/`, `content/` (panel, axis, legend,
strip, label), `graphics/` (canvas, fonts, polylines, shapes). Builders (`graphs.clj`, `series_plots.clj`,
`multiplot.clj`) turn layout rows into ordinary plot maps; their `:scales` option is deep-merged (`theme/deep-merge`)
into the builder's. `cljplot2.core` is the public API (`build`, `render`, `render-image`, builders).

## Notebooks, demos, data

- `notebooks/use_cases/*.clj` and `notebooks/galleries/*.clj` are Clay notebooks; order in `notebooks/chapters.edn`.
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
- Errors: `cljplot2.schema/fail!` with `{:key option …}`; namespaces `cljplot2.schema` itself requires (e.g.
  `graphics.shape`) use `cljplot2.error/fail!` (the same function, no dependency cycle).

## Related repositories

fastmath, wadogo and clojure2d are SNAPSHOT dependencies, often changed alongside cljplot2 and installed locally.
Never commit to the fastmath repository (commits must be signed - the user commits). After a local `lein install` of
fastmath, reinstall clojure2d. The user commits cljplot2 changes.
