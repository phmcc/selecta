# Exclude Participants by Criteria

Appends an exclusion step to the enrollment flow. Participants matching
the criteria are removed and shown in a side box. Optionally, itemized
sub-reasons can be displayed below the total.

## Usage

``` r
exclude(
  .flow,
  label,
  criterion,
  n = NULL,
  reasons = NULL,
  show_zero = FALSE,
  show_count = FALSE,
  included_label = NULL,
  collapse_singletons = FALSE
)
```

## Arguments

- .flow:

  A `selecta` object (piped from
  [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md)
  or a previous step).

- label:

  Character. Human-readable description for the side box (*e.g.,*
  `"Excluded"` or `"Lost to follow-up"`). After
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
  may be a character vector with one label per arm (*e.g.,*
  `c("Treatment discontinued", "Initiated treatment")`).

- criterion:

  An unquoted logical expression evaluated against the data. Should
  evaluate to `TRUE` for rows to be removed. Compound conditions are
  supported using the vectorized operators `&` (and), `|` (or), and `!`
  (not). Do not use the scalar short-circuit operators `&&` or `||`,
  which evaluate only the first element of each vector. Data mode only.

- n:

  Integer. Number of participants removed at this step. After a
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
  step, supply a vector with one value per arm. Manual mode only.

- reasons:

  Exclusion sub-reasons. Accepts these forms:

  - A *character string* (data mode): the name of a column whose values
    are tabulated automatically into a flat breakdown.

  - A *length-2 character vector* (data mode): the names of a reason
    column and a sub-reason column, cross-tabulated automatically into a
    two-level breakdown—parents ordered by total, sub-reasons by count.

  - A *named numeric vector* (manual mode): counts per reason, *e.g.,*
    `c("Disease progression" = 12, "Declined" = 8)`. An entry may itself
    be a named numeric vector, giving a two-level breakdown (a reason
    and its sub-reasons).

  - A *list* of any of the above (data or manual mode after
    [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)):
    one entry per arm.

- show_zero:

  Logical. If `FALSE` (default), sub-reasons with a count of zero are
  hidden. Set to `TRUE` to display all pre-specified reason categories,
  including those with zero participants.

- show_count:

  Logical. If `FALSE` (default), the intermediate count box is
  suppressed—the count still updates internally but no box is rendered.
  Set to `TRUE` to force a count box. Overridden by `included_label`:
  providing any `included_label` always creates a count box regardless
  of `show_count`. Also automatically suppressed when the next step is
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
  [`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
  or
  [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md).

- included_label:

  Character string (or vector). Optional text for the box showing the
  count remaining after exclusion. When provided, a count box is always
  rendered regardless of `show_count`. After
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
  may be a character vector with one label per arm.

- collapse_singletons:

  Logical. When `TRUE`, a parent reason that resolves to a single
  sub-reason is collapsed to a plain leaf carrying the parent's label
  and count (dropping the lone, redundant sub-line). Applies to
  two-level reasons from either a manual nested specification or a
  two-column data-mode cross-tabulation. Default `FALSE` keeps every
  parent expanded, for full transparency.

## Value

The updated `selecta` object with an exclusion step appended.

## Details

`exclude()` records participants removed at a step and is the most
common pipeline verb. In data mode, `criterion` is an unquoted logical
expression evaluated against the dataset (rows for which it is `TRUE`
are removed) and `reasons` may name one column (a flat breakdown) or two
columns (a reason and a sub-reason, cross-tabulated into a two-level
breakdown); in manual mode, `n` gives the number removed and `reasons`
may be a named numeric vector. After a
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
or
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
split the exclusion applies per arm, in which case `n`, `reasons`, and
`included_label` accept per-arm vectors or lists. By default the running
count box is suppressed between consecutive exclusions for a compact
diagram; supplying `included_label` (or `show_count = TRUE`) forces a
count box to be drawn.

When `getOption("selecta.check_arithmetic")` is `TRUE`, the manual
counts of the whole flow are audited together before export: an
over-exclusion, a split or combine whose parts do not match the running
total, and sub-reasons that do not sum to their exclusion total each
raise an advisory warning without altering the figures. The audit runs
whenever the flow is computed; this includes calls to
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
and [`summary()`](https://rdrr.io/r/base/summary.html), so a single call
to any of these functions reports every discrepancy at once.

Eligibility that is more naturally framed as inclusion fits this same
model: express it as the exclusion of those who fail the criteria, and
use `included_label` to label the retained count (*e.g.,*
`included_label = "Eligible cohort"`).

After a
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
step, both `label` and `included_label` accept character vectors (one
element per arm) for per-arm labeling—useful in observational designs
where attrition mechanisms differ across strata.

## See also

[`assess`](https://phmcc.codeberg.page/selecta/reference/assess.md) for
assessment/procedure steps (STARD),
[`enroll`](https://phmcc.codeberg.page/selecta/reference/enroll.md) for
initializing a flow

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
enroll(n = 500) |>
  exclude("Ineligible", n = 65)
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 1
#>   [1] exclude: "Ineligible" (n = 65)

# With sub-reasons (manual)
enroll(n = 500) |>
  exclude("Excluded", n = 65,
    reasons = c("Did not meet criteria" = 22,
                "Ineligible comorbidities" = 18,
                "Declined to participate" = 15,
                "Lost to follow-up" = 10))
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 1
#>   [1] exclude: "Excluded" (n = 65)
#>          • Did not meet criteria = 22
#>          • Ineligible comorbidities = 18
#>          • Declined to participate = 15
#>          • Lost to follow-up = 10
#> 

# Show intermediate count box (opt-in)
enroll(n = 500) |>
  exclude("Ineligible", n = 65, show_count = TRUE) |>
  exclude("Declined", n = 20) |>
  endpoint("Final")
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 3
#>   [1] exclude: "Ineligible" (n = 65)
#>   [2] exclude: "Declined" (n = 20)
#>   [3] endpoint: "Final"

# Or use included_label (always shows count box)
enroll(n = 500) |>
  exclude("Ineligible", n = 65,
          included_label = "Eligible") |>
  endpoint("Final")
#> selecta flow (manual mode)
#>   Starting N: 500
#>   Steps: 2
#>   [1] exclude: "Ineligible" (n = 65)
#>   [2] endpoint: "Final"

# Per-arm labels (observational)
enroll(n = 1000) |>
  stratify(labels = c("Exposed", "Unexposed"), n = c(500, 500),
           label = "Classified by exposure") |>
  exclude(c("Treatment discontinued", "Initiated treatment"),
          n = c(45, 52))
#> selecta flow (manual mode)
#>   Starting N: 1,000
#>   Steps: 2
#>   [1] stratify: Exposed, Unexposed
#>          label: "Classified by exposure"
#>   [2] exclude: "Treatment discontinued" / "Initiated treatment" (n = 97)

# Per-arm reasons (list of named vectors)
enroll(n = 900) |>
  allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
  exclude("Discontinued", n = c(30, 25),
          reasons = list(
              c("Adverse event" = 18, "Withdrew consent" = 12),
              c("Adverse event" = 10, "Lost to follow-up" = 15)
          )) |>
  endpoint("Analyzed")
#> selecta flow (manual mode)
#>   Starting N: 900
#>   Steps: 3
#>   [1] stratify: Drug A, Placebo
#>          label: "Randomized"
#>   [2] exclude: "Discontinued" (n = 55)
#>   [3] endpoint: "Analyzed"

# Compound expression (data mode)
data(selectaex2)
enroll(selectaex2, id = "patient_id") |>
  exclude("Ineligible or duplicate",
          criterion = eligible == FALSE | is_duplicate == TRUE)
#> selecta flow (data mode)
#>   Starting N: 2,400
#>   Steps: 1
#>   [1] exclude: "Ineligible or duplicate"
```
