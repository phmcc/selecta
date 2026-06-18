# Initialize an Enrollment Flow

Entry point for building an EQUATOR-style enrollment diagram from a
single starting population. Accepts either a `data.frame` (data mode,
where counts are computed automatically from exclusion expressions) or a
starting count `n` (manual mode, where counts are supplied explicitly at
each step).

## Usage

``` r
enroll(data = NULL, id = NULL, n = NULL, label = "Study Population")
```

## Arguments

- data:

  A `data.frame` or `data.table` in which each row represents one
  participant. When supplied, exclusion expressions passed to
  [`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
  are evaluated against this data to compute counts automatically. If
  `NULL` (default), the flow operates in manual mode.

- id:

  Character string naming the participant ID column in `data`. Defaults
  to the first column. Ignored in manual mode.

- n:

  Integer. Starting population count for manual mode. Must be a
  non-negative scalar. Ignored when `data` is supplied.

- label:

  Character string for the top-level box in the diagram. Default is
  `"Study Population"`.

## Value

An object of class `"selecta"` containing the data (if supplied), mode,
starting count, label, and an empty step list. Subsequent pipeline
functions
([`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
*etc.*) append steps to this object.

## Details

`enroll()` begins every single-source pipeline and fixes the operating
mode for all subsequent steps. Supplying `data` (with `id`) selects
*data mode*, in which later
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
and
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
steps filter and partition the dataset and counts are derived from the
data. Alternatively, supplying `n` instead selects *manual mode*, in
which counts are taken from the numbers given at each step. The two
modes are mutually exclusive, and the resulting object is intended to be
extended with the pipe operator. For diagrams with several entry sources
that converge (PRISMA, MOOSE), use
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md)
instead of `enroll()`.

## See also

[`sources`](https://phmcc.codeberg.page/selecta/reference/sources.md)
for multi-source entry,
[`exclude`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
for adding exclusion criteria,
[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for rendering

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
# Manual mode
enroll(n = 500, label = "Assessed for eligibility")
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 0

# Data mode
enroll(selectaex2, id = "patient_id", label = "Study Population")
#> selecta flow (data mode)
#>   Starting N: 2,400
#>   Steps: 0

# Minimal CONSORT pipeline
enroll(n = 500) |>
  exclude("Ineligible", n = 65) |>
  allocate(labels = c("Treatment", "Control"), n = c(218, 217)) |>
  endpoint("Analyzed")
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 3
#>   [1] exclude: "Ineligible" (n = 65)
#>   [2] stratify: Treatment, Control
#>          label: "Randomized"
#>   [3] endpoint: "Analyzed"
```
