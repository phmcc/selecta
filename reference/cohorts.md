# Extract Cohorts at Every Stage

Returns a named list of datasets at each step of the enrollment flow,
enabling cross-cohort comparisons. Results are reported as a named list,
organized by step label. Data mode only.

## Usage

``` r
cohorts(.flow)
```

## Arguments

- .flow:

  A `selecta` object created in data mode (`data` supplied to
  [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md)).

## Value

A named list of cohort snapshots, keyed by step label. Each snapshot is
itself a list with:

- `included`:

  A `data.table` of participants still in the flow after this step.

- `excluded`:

  A `data.table` of participants removed at this step (for exclusion
  steps; `NULL` otherwise).

- `n_included`:

  Integer count of included participants.

- `n_excluded`:

  Integer count of excluded participants (or `NA`).

## Details

`cohorts()` replays a *data mode* flow and captures the dataset at every
step, returning a named list keyed by step label (with `"_start"` for
the initial cohort). Each snapshot exposes both the `included` and the
`excluded` rows together with their counts, which is useful for
validating a diagram against the data, auditing why particular
participants were dropped, or extracting an intermediate population.
After a
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
or
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
split, the `included` and `excluded` elements of a per-arm step are
themselves named lists with one entry per arm; after a factorial
(two-level) split the entries are the cells, keyed
`"<parent>: <child>"`. A manual-mode flow has no underlying data and
therefore raises an error. To obtain only the final analyzed population,
use
[`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md).

## See also

[`cohort`](https://phmcc.codeberg.page/selecta/reference/cohort.md) for
extracting only the final cohort

Other cohort extraction functions:
[`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md)

## Examples

``` r
flow <- enroll(selectaex2, id = "patient_id") |>
  exclude("Ineligible", criterion = eligible == FALSE) |>
  endpoint("Final")

stages <- cohorts(flow)
names(stages)
#> [1] "_start"     "Ineligible" "Final"     
stages[["Ineligible"]]$n_excluded
#> [1] 851
```
