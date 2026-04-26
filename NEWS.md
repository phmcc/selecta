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
