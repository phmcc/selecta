# Label a Phase of the Enrollment Flow

Adds a vertical phase label to the left margin of the diagram (*e.g.,*
`"Enrollment"`, `"Allocation"`, `"Follow-up"`, `"Analysis"`). Phase
labels span all subsequent steps until the next `phase()` call or the
end of the flow.

## Usage

``` r
phase(.flow, label)
```

## Arguments

- .flow:

  A `selecta` object.

- label:

  Character string. The phase label, rendered as rotated text on the
  left margin.

## Value

The updated `selecta` object with a phase marker appended.

## Details

`phase()` inserts a stage boundary rather than a flow node. Each call
opens a phase whose label is drawn in the left margin, spanning every
subsequent step until the next `phase()` or the end of the flow. The
purpose of these phase markers is to reflect the stages of analysis in
the diagram; as such, they are purely presentational, and they do not
alter counts or topology. In the `grid` engine, phase labels are
rendered vertically and are wrapped to fit their band by default;
conversely, the `dot` engine renders phase labels horizontally due to
engine limitations.

## See also

[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for rendering with phase labels

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
# Phase labels divide a flow into labeled stages. The printed summary
# marks each phase with a "--- Label ---" banner.
enroll(n = 1200, label = "Records identified") |>
  phase("Enrollment") |>
  exclude("Duplicates", n = 84) |>
  phase("Allocation") |>
  stratify(labels = c("Drug A", "Placebo"), n = c(520, 533)) |>
  phase("Follow-up") |>
  exclude("Lost to follow-up", n = c(23, 31)) |>
  phase("Analysis") |>
  endpoint("Final Analysis")
#> selecta flow (manual mode)
#>   Starting N: 1,200
#>   Steps: 8
#>   --- Enrollment ---
#>   [2] exclude: "Duplicates" (n = 84)
#>   --- Allocation ---
#>   [4] stratify: Drug A, Placebo
#>          label: "Stratified"
#>   --- Follow-up ---
#>   [6] exclude: "Lost to follow-up" (n = 54)
#>   --- Analysis ---
#>   [8] endpoint: "Final Analysis"
```
