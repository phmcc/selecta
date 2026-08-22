# *selecta* 0.6.1 (2026-08-22)

* Add `quiet` argument to `flowsave()` to suppress the message reporting the file written and the dimensions used.
* Add `units` argument to `flowsave()` and `recdims()`, accepting "in" (default), "cm", and "mm"; graphics devices are driven in inches internally.
* Edited `flowsave()` to report the file written and the dimensions used, as well as whether those dimensions were computed or supplied, matching the reporting convention of `summata::forestsave()`. Previously, the message was emitted only when a dimension was left unspecified.
* Changed `flowsave()` argument order: `units` follows `height` and `quiet` follows `sans_serif`. Calls passing `dpi` or `sans_serif` positionally require updating.
* Edited `recdims()` records the units on its result as a "units" attribute.
* Removed `side_gap_in` argument in the DOT engine exporter; redundant with `node_sep`.
* Internal: removed unreachable spacer-geometry code from the DOT engine.

# *selecta* 0.6.0 (2026-06-17)

* Enhance Graphviz (DOT) output for publication-quality figures. The DOT engine now renders full flow diagrams for complete feature parity with `grid` (default) outputs.
* Add factorial (double-split) designs, with sub-arms grouped under their parent and the trunk centered over all leaves. Compatible with both `grid` and DOT outputs.
* Add nested exclusion reasons, with each parent reason carrying its own named sub-reasons. Accessed by supplying a nested named vector (manual mode) or a reason/sub-reason column pair that is cross-tabulated automatically (data mode). Compatible with both `grid` and DOT outputs.
* Rename the `criteria` parameter of `exclude()` and `assess()` to `criterion`. The singular reflects that each step defines one accountable exclusion with a single reason breakdown; compound conditions (`&`, `|`, `!`)  remain supported.
* Expand arithmetic checks. `exclude()` and `assess()` now warn when a reason column has no value for some removed rows---such rows are grouped under "Other". Like the manual-flow arithmetic checks, this is advisory and toggleable via `options(selecta.check_arithmetic)`.
* Expand vignettes and gallery.

# *selecta* 0.5.0 (2026-06-01)

* Remove `classify()`. STARD diagrams now use `stratify()` on the index-test result with a per-arm `endpoint(breakdown = ...)` for the target-condition breakdown, matching the standard STARD layout.
* `cohorts()` snapshot fields renamed: `remaining` to `included` and `n_remaining` to `n_included`, mirroring `excluded`/`n_excluded`.
* Complete CRAN-compliant documentation for all exported functions, adding `@details` and `@family` sections alongside full `@param`, `@return`, `@seealso`, and `@examples`; non-executed examples now use `\donttest` rather than `\dontrun`.
* DOT engine: source box and header colors now match the grid engine; fix multi-source header alignment; add orthogonal-edge and three-source examples.
* Arithmetic checks for manual flows now also validate per-arm exclusion sub-reason totals; checks remain toggleable via `options(selecta.check_arithmetic)`.
* Performance: `recdims()` no longer issues drawing primitives when only measuring dimensions, and data-mode exclusions avoid materializing the excluded subset.
* Expand vignettes and gallery.

# *selecta* 0.4.0 (2026-05-26)

* Phase boxes rework. Adaptive formatting for both overly long and multi-line phase labels, including toggleable soft-returns for the former and user-insertable hard-returns (`\n`) for the latter.
* Add regional number formatting: US, EU, SI/ISO 31-0 standard, custom.
* Refine vignette workflows and documentation.
* Rename `suggest_size()` function to `recdims()`.
* Add logo.

# *selecta* 0.3.0 (2026-03-14)

* Add "split-and-recombine" topology, accessible through use of successive `stratify()` and `combine()` calls.
* Add exclusion box formatting edits.
* Make enhancements to `classify()` to ensure proper alignment.
* Reorganize mock datasets with expanded documentation.
* Update documentation headers.

# *selecta* 0.2.1 (2026-03-12)

* Add refinements to reason ordering.
* Fix `expr` parameter expression handling.

# *selecta* 0.2.0 (2026-03-04)

* Add support for multi-source diagrams (PRISMA, MOOSE).
* Improve rendering consistency and add specializations for "split" diagrams (CONSORT, STROBE).
* New functions: `sources()`, `combine()`, `assess()`, `classify()`, `stratify()`/`allocate()`, `suggest_size()`, `autoflow()`.
* Multiple pipeline enhancements for `exclude()`, `endpoint()`, and `stratify()`/`allocate()`.
* Add font-scaled line spacing.
* Add count-first display mode.
* Add comprehensive `testthat` suite.
* `export_diagram()` renamed to `autodiagram()`.
* `exclude()` default for `show_count` changed from `TRUE` to `FALSE`.
* Add vectorization where possible for performance enhancements.


# *selecta* 0.1.0 (2025-02-22)

* Initial commit.
* Core functions established.
* First-draft README.
* Basic CONSORT functionality, tested on 0-, 2-, and *n*-arm setups.
