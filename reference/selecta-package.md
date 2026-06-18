# selecta: Declarative EQUATOR-Style Flow Diagrams for Clinical Studies

Build EQUATOR-style flowcharts for clinical studies by sequentially
defining inclusion and exclusion criteria, study arms, and endpoints.
The pipe-friendly API supports CONSORT (randomized trials), STROBE
(observational cohorts), STARD (diagnostic accuracy), PRISMA (systematic
reviews), and MOOSE (observational meta-analysis) diagram layouts, as
well as multi-source convergence, split-and-recombine, factorial, and
hybrid topologies. Diagrams are rendered via 'grid' graphics in both
data-driven (automatic counting) and manual-count modes, with optional
'DiagrammeR'/'Graphviz' output.

## Package options

`selecta` reads the following session options, each settable with
[`options()`](https://rdrr.io/r/base/options.html) and each with a safe
default:

- `selecta.number_format`:

  Default count formatting when `number_format` is not passed
  explicitly. A preset (`"us"`, `"eu"`, `"space"`, `"none"`) or a custom
  `c(big.mark, decimal.mark)` pair. Defaults to `"us"`.

- `selecta.vpad`:

  Default vertical padding between rows, in inches, used by the grid
  engine and by
  [`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md).
  Defaults to `0.25`.

- `selecta.check_arithmetic`:

  Whether manual-mode count consistency checks emit advisory warnings
  (arm counts not summing to the split total, an exclusion exceeding the
  available count, sub-reasons not summing to their total, or a manual
  [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md)
  disagreeing with its streams). The counts are never altered. Defaults
  to `TRUE`.

- `selecta.debug_layout`:

  Whether the computation and rendering functions print a structured
  layout trace via [`message()`](https://rdrr.io/r/base/message.html)
  (node and edge tables, computed positions, recommended dimensions,
  per-phase band heights, and the generated DOT source). Useful for bug
  reports. Defaults to `FALSE`.

## See also

Useful links:

- <https://phmcc.codeberg.page/selecta>

- <https://codeberg.org/phmcc/selecta>

- <https://github.com/phmcc/selecta>

- Report bugs at <https://github.com/phmcc/selecta/issues>

## Author

**Maintainer**: Paul Hsin-ti McClelland <PaulHMcClelland@protonmail.com>
([ORCID](https://orcid.org/0000-0002-3119-6531)) \[copyright holder\]

Authors:

- Paul Hsin-ti McClelland <PaulHMcClelland@protonmail.com>
  ([ORCID](https://orcid.org/0000-0002-3119-6531)) \[copyright holder\]

## Examples

``` r
# \donttest{
opts <- options()  # save to restore afterwards
options(selecta.number_format = "eu")     # 1.234 instead of 1,234
options(selecta.vpad = 0.35)              # looser default spacing
options(selecta.check_arithmetic = FALSE) # silence manual-count warnings
options(selecta.debug_layout = TRUE)      # print a layout trace
options(opts)                             # restore previous options
# }
```
