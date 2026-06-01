# *selecta* 0.5.0 (2026-06-01)

* Remove `classify()`. STARD diagrams now use `stratify()` on the index-test
  result with a per-arm `endpoint(reasons = ...)` for the target-condition
  breakdown, matching the standard STARD layout.
* `cohorts()` snapshot fields renamed: `remaining` to `included` and
  `n_remaining` to `n_included`, mirroring `excluded`/`n_excluded`.
* DOT engine: source box and header colors now match the grid engine; fix
  multi-source header alignment; add orthogonal-edge and three-source examples.
* Arithmetic checks for manual flows now also validate per-arm exclusion
  sub-reason totals; checks remain toggleable via
  `options(selecta.check_arithmetic)`.
* Performance: `recdims()` no longer issues drawing primitives when only
  measuring dimensions, and data-mode exclusions avoid materializing the
  excluded subset.
* Expand documentation and README

# *selecta* 0.4.0 (2026-05-26)

* Phase boxes rework
* Add regional number formatting
* Refine vignette workflows and documentation
* Rename `expr` parameter to `criteria`
* Rename `suggest_size()` function to `recdims()`
* Add logo

# *selecta* 0.3.0 (2026-03-14)

* Add "split-and-recombine" functionality
* Exclusion box formatting edits
* Enhancements to `classify()` to ensure proper alignment
* Mock datasets reorganization and documentation
* Update documentation headers

# *selecta* 0.2.1 (2026-03-12)

* Refinements to reason ordering
* Fix `expr` parameter expression handling

# *selecta* 0.2.0 (2026-03-04)

* Add support for multi-source diagrams (PRISMA, MOOSE)
* Improve rendering consistency and add specializations for "split" 
  diagrams (CONSORT, STROBE)
* New functions: `sources()`, `combine()`, `assess()`, `classify()`,
  `stratify()`/`allocate()`, `suggest_size()`, `autoflow()`
* Multiple pipeline enhancements for `exclude()`, `endpoint()`, and
  `stratify()`/`allocate()`
* Add font-scaled line spacing
* Add count-first display mode
* Add comprehensive `testthat` suite
* `export_diagram()` renamed to `autodiagram()`.
* `exclude()` default for `show_count` changed from `TRUE` to `FALSE`.
* Performance enhancements


# *selecta* 0.1.0 (2025-02-22)

* Initial commit
* Core functions established
* First-draft README
* Basic CONSORT functionality, tested on 0-, 2-, and *n*-arm setups
