# Mark the Final Analysis Endpoint

Adds the terminal node(s) to the enrollment flow. If arms have been
defined via
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
one endpoint box appears per arm.

## Usage

``` r
endpoint(
  .flow,
  label = "Final Analysis",
  breakdown = NULL,
  groups = NULL,
  n = NULL,
  variable = NULL
)
```

## Arguments

- .flow:

  A `selecta` object.

- label:

  Character string for the final box. With `groups` it labels the shared
  distributor box above the group boxes. Default `"Final Analysis"`.

- breakdown:

  Optional named numeric vector (or, for a per-arm endpoint, a list of
  them) itemizing the box total into parts printed *within* the box,
  beneath the total. This is the STARD final-diagnosis form, where each
  terminal box reports its target-condition composition, *e.g.*
  `breakdown = c("Target +" = 160, "Target -" = 40)`. Mutually exclusive
  with `groups`.

- groups:

  Optional character vector of group labels (manual mode). When
  supplied, the endpoint splits into one *separate* terminal box per
  group, fanning from a shared distributor. Use this for study-design
  diagrams that end by displaying the groups to be analyzed (“Group A”,
  “Group B”, ...). A split endpoint requires a single incoming stream;
  it cannot follow an unrecombined
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
  or
  [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md).
  Mutually exclusive with `breakdown`.

- n:

  Optional numeric vector of per-group counts (manual mode), parallel to
  `groups`.

- variable:

  Optional character naming a grouping column (data mode). Splits the
  terminal endpoint by that column, one box per level, with counts
  tabulated automatically. The data-mode counterpart of `groups`/`n`;
  same single-stream requirement.

## Value

The updated `selecta` object with an endpoint step appended.

## Details

`endpoint()` closes the flow with its terminal node(s) and is usually
the last step in a pipeline. When the flow has been split with
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
or
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
and not recombined, one endpoint box is drawn per arm, and `label` and
`breakdown` may be supplied per arm.

Two distinct presentations of detail are available, which are mutually
exclusive. `breakdown` itemizes a single box's total as text lines
inside that box (the STARD final-diagnosis form, reporting each box's
target-condition composition). Conversely, `groups` divides the endpoint
into separate side-by-side boxes, one per group, fanning from a shared
distributor; this design favors study diagrams that end by displaying
the groups to be analyzed. The completed object is then passed to
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
or
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md).

## See also

[`assess`](https://phmcc.codeberg.page/selecta/reference/assess.md) for
the diagnostic test-receipt steps that precede a STARD endpoint,
[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for rendering

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
enroll(n = 300) |>
  exclude("Excluded", n = 40) |>
  endpoint("Included in analysis")
#> selecta flow (manual mode)
#>   Starting N: 300
#>   Steps: 2
#>   [1] exclude: "Excluded" (n = 40)
#>   [2] endpoint: "Included in analysis"

# STARD-style per-arm endpoint with a within-box breakdown
enroll(n = 500) |>
  stratify(labels = c("Positive", "Negative"), n = c(200, 300),
           label = "Index test result") |>
  endpoint("Final diagnosis",
           breakdown = list(c("Target +" = 160, "Target -" = 40),
                            c("Target +" = 25, "Target -" = 275)))
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 2
#>   [1] stratify: Positive, Negative
#>          label: "Index test result"
#>   [2] endpoint: "Final diagnosis"
#>        Arm 1:
#>          • Target + = 160
#>          • Target - = 40
#> 
#>        Arm 2:
#>          • Target + = 25
#>          • Target - = 275
#> 

# Split endpoint into separate terminal group boxes (manual)
enroll(n = 300, label = "Eligible cohort") |>
  endpoint("Allocated to study group",
           groups = c("Group A", "Group B", "Group C"),
           n = c(100, 100, 100))
#> selecta flow (manual mode)
#>   Starting N: 300
#>   Steps: 1
#>   [1] endpoint: "Allocated to study group"

# Split endpoint by a grouping column (data mode)
df <- data.frame(id = 1:300, grp = sample(c("A", "B", "C"), 300, TRUE))
enroll(df, id = "id", label = "Eligible cohort") |>
  endpoint("Allocated to study group", variable = "grp")
#> selecta flow (data mode)
#>   Starting N: 300
#>   Steps: 1
#>   [1] endpoint: "Allocated to study group"
```
