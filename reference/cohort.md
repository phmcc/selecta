# Extract the Final Cohort

Returns the dataset remaining after all exclusion criteria have been
applied. When arms are defined via
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
the result is either a single combined `data.table` or a named list of
per-arm `data.table` objects. Data mode only.

## Usage

``` r
cohort(.flow, split = FALSE, arm = NULL)
```

## Arguments

- .flow:

  A `selecta` object created in data mode (`data` supplied to
  [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md)).

- split:

  Logical. If `TRUE` and arms are defined, return a named list of
  `data.table`s (one per arm). Default `FALSE` returns a single combined
  `data.table`.

- arm:

  Character. Name of a specific arm to extract. If supplied, returns
  only that arm's `data.table`.

## Value

A `data.table` containing the participants remaining after all exclusion
criteria. When `split = TRUE`, a named list of `data.table`s (one per
arm). When `arm` is specified, a single-arm `data.table`.

## Details

`cohort()` replays the exclusion criteria of a *data-mode* flow against
the original dataset and returns the rows that survive to the end, so
the analyst can pass the exact analyzed population to downstream
modeling. It requires a flow created by supplying `data` to
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md);
manual-mode flows carry only counts and therefore raise an error. For an
unsplit flow the result is a single `data.table`; after
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
or
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
`split = TRUE` returns one table per arm and `arm` extracts a single
named arm. To inspect the cohort at every intermediate step rather than
only the end, use
[`cohorts()`](https://phmcc.codeberg.page/selecta/reference/cohorts.md).

## See also

[`cohorts`](https://phmcc.codeberg.page/selecta/reference/cohorts.md)
for stage-by-stage snapshots,
[`enroll`](https://phmcc.codeberg.page/selecta/reference/enroll.md) for
initializing a data-mode flow

Other cohort extraction functions:
[`cohorts()`](https://phmcc.codeberg.page/selecta/reference/cohorts.md)

## Examples

``` r
flow <- enroll(selectaex2, id = "patient_id") |>
  exclude("Ineligible", criterion = eligible == FALSE) |>
  endpoint("Final")

final <- cohort(flow)
nrow(final)
#> [1] 1549
```
