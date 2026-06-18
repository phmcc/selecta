# Resolve an Exclusion Step

Evaluates a single exclusion step in either data or manual mode and
returns the excluded and remaining counts, the remaining data (data
mode), and any tabulated sub-reasons.

## Usage

``` r
resolve_exclusion(
  mode,
  step,
  data = NULL,
  current_n = NULL,
  manual_n_override = NULL
)
```

## Arguments

- mode:

  Character, either `"data"` or `"manual"`.

- step:

  The exclusion step (list) from the pipeline.

- data:

  A `data.table` of current participants (data mode).

- current_n:

  Integer current count (manual mode).

- manual_n_override:

  Optional integer overriding `step$n`.

## Value

A list with `n_excluded`, `n_included`, `included_data`, and `reasons`.
