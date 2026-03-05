# *selecta* 0.2.0 (2026-03-02)

## New EQUATOR guideline support

* **PRISMA 2020** — `sources()` initialises multi-source flows with up to
  three parallel identification columns (previous studies, databases and
  registers, other methods).
  `combine()` merges parallel streams into a single flow via inverted-Y
  convergence arrows.
  Column headers are auto-generated or user-supplied.

* **STARD** — `assess()` models test/procedure receipt steps with inverted
  label semantics (side box: "Did not receive …"; main flow: "Received …").
  `classify()` adds a terminal cross-classification grid for index test ×
  reference standard results.

* **STROBE / MOOSE** — `stratify()` splits flows by any characteristic
  (exposure status, diagnostic result, etc.) with a generic default label
  ("Stratified").
  `allocate()` is now a convenience alias for `stratify()` with default
  label "Randomized", preserving the CONSORT calling convention.

## New functions

* `sources()` — multi-source entry point (replaces `enroll()` for PRISMA
  flows).
* `combine()` — merge parallel streams after deduplication.
* `assess()` — test/procedure receipt step (STARD).
* `classify()` — terminal cross-classification grid (STARD).
* `stratify()` — generic population split; `allocate()` becomes its alias.
* `suggest_size()` — compute recommended figure dimensions from diagram
  content.
* `autodiagram()` — save diagram to file (PDF, PNG, SVG, TIFF) with
  auto-computed dimensions.
  Replaces the previous `export_diagram()`.

## Pipeline enhancements

* `exclude()` gains `show_count` (default `FALSE`) to optionally suppress
  intermediate count boxes.
  Count boxes are also auto-suppressed when the next step is `stratify()`,
  `endpoint()`, or `allocate()`.
* `exclude()` gains `included_label` for custom text on intermediate count
  boxes (always renders a box when supplied).
* `exclude()` and `endpoint()` accept per-arm `label` and
  `included_label` vectors after `stratify()`, for labelling attrition
  differently across strata.
* `endpoint()` gains `reasons` for sub-item counts (e.g., STARD final
  diagnosis breakdowns).

## Rendering

* **Count-first display mode** — `count_first = TRUE` renders side-box
  labels as `"214  Discontinued"` (bold count before label) rather than
  `"Discontinued (n = 214)"`.
* **Font-scaled line spacing** — `line_height` scales proportionally with
  `cex`, maintaining consistent box heights across font sizes.
* **Inch-based positioning** — layout is computed in physical inches with
  user-controllable margins (`margin`, default 0.25 in), providing
  precise control over element placement.
* **Phase box alignment** — phase labels are left-aligned consistently
  regardless of diagram width.
* STARD canvas width accounts for asymmetric pre-split sections.
* 2-arm side boxes use right-edge alignment (left arm) and left-edge
  alignment (right arm) for visual consistency.

## Performance

* Memoised text measurement cache (`tw_in`) eliminates repeated
  `strwidth()` calls for identical strings.
* Vectorised box and edge drawing replaces per-element loops.
* `autodiagram()` reuses the computed graph from `suggest_size()`,
  avoiding a redundant compute + layout pass.
* Grouped `data.table` aggregations replace per-row cursor loops for
  row heights, pair gaps, and exclusion-edge offsets.

## Data and testing

* Four built-in datasets: `rctselect0` (observational, 0-arm),
  `rctselect2` (2-arm RCT), `rctselect3` (3-arm trial),
  `rctselect6` (6-arm dose-finding).
* Comprehensive `testthat` suite: 117 tests across 5 files covering
  core pipeline functions, stream model, compute engine, rendering
  output, and S3 methods.
  Replaces ad-hoc test scripts.

## Code quality

* Input validation for `combine()` (requires preceding `sources()` step)
  and `sources()` (non-negative numeric counts).
* Defensive guard for empty graphs (warns and returns early).
* Transient rendering columns (`fill_col`, `n_reason`) cleaned from the
  graph object before return.
* Roxygen documentation follows summata conventions: enriched `@return`
  and `@seealso` cross-references, `\emph{e.g.,}` formatting, runnable
  `@examples` using package datasets.

## Breaking changes

* `export_diagram()` renamed to `autodiagram()`.
* `exclude()` default for `show_count` changed from `TRUE` to `FALSE`.
* Testing framework changed from `tinytest` to `testthat` (>= 3.0.0).

---

# *selecta* 0.1.0 (2025-02-22)

* Initial commit
* Core functions established
* First-draft README
* Basic CONSORT functionality, tested on 0-, 2-, and *n*-arm setups
