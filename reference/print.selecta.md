# Print an Enrollment Flow Summary

Displays a concise text summary of the pipeline steps and their
parameters. Intended for interactive inspection of a `selecta` object
before rendering.

## Usage

``` r
# S3 method for class 'selecta'
print(x, ...)
```

## Arguments

- x:

  A `selecta` object.

- ...:

  Ignored.

## Value

Invisibly returns `x`.

## Details

The `print` method gives a compact, text-only view of a `selecta` object
for interactive inspection before rendering. It lists the operating
mode, the starting count, and each pipeline step with its key parameters
(exclusion reasons, arm labels, endpoint sub-items), and marks phase
boundaries with a “— Label —” banner. It does not draw the diagram or
open a graphics device; for that use
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
or
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).

## See also

[`summary.selecta`](https://phmcc.codeberg.page/selecta/reference/summary.selecta.md)
for a tabular per-node summary,
[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for rendering

Other flowchart output functions:
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md),
[`summary.selecta()`](https://phmcc.codeberg.page/selecta/reference/summary.selecta.md)

## Examples

``` r
flow <- enroll(n = 500) |>
  exclude("Ineligible", n = 65,
    reasons = c("No consent" = 30, "Under 18" = 35)) |>
  allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
  endpoint("Analyzed")
flow
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 3
#>   [1] exclude: "Ineligible" (n = 65)
#>          • No consent = 30
#>          • Under 18 = 35
#> 
#>   [2] stratify: Drug A, Placebo
#>          label: "Randomized"
#>   [3] endpoint: "Analyzed"
```
