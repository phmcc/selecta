# Summarize an Enrollment Flow

Computes all counts from the pipeline and returns a `data.table`
summarizing each node in the diagram.

## Usage

``` r
# S3 method for class 'selecta'
summary(object, ...)
```

## Arguments

- object:

  A `selecta` object.

- ...:

  Ignored.

## Value

A `data.table` with columns `phase`, `role`, `arm`, `text`, and `n`.
Each row corresponds to one node in the computed diagram.

## Details

The `summary` method runs the same count computation that underlies
rendering and returns the result as a clean `data.table`, one row per
node, rather than drawing anything. This is convenient for programmatic
checks (confirming arm totals, extracting the final analyzed count) and
for embedding flow figures in tables or reports. The returned object is
a plain `data.table` and may be filtered or joined like any other. For a
human-readable console view use
[`print.selecta()`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md);
to render the diagram use
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md).

## See also

[`print.selecta`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md)
for a console summary,
[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for rendering

Other flowchart output functions:
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
[`print.selecta()`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md),
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)

## Examples

``` r
flow <- enroll(n = 500) |>
  exclude("Ineligible", n = 65) |>
  allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
  endpoint("Analyzed")
summary(flow)
#>    phase     role   arm             text     n
#>    <int>   <char> <int>           <char> <num>
#> 1:     1     main    NA Study Population   500
#> 2:     2     side    NA       Ineligible    65
#> 3:     3    alloc    NA       Randomized   435
#> 4:     4      arm     1           Drug A   218
#> 5:     4      arm     2          Placebo   217
#> 6:     5 endpoint     1         Analyzed   218
#> 7:     5 endpoint     2         Analyzed   217
```
