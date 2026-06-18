# Split a Dataset into Arm Streams by a Variable

Partitions a `data.table` by the levels of a splitting variable,
optionally relabeling levels, and returns the per-arm data and labels.

## Usage

``` r
split_by_var(dt, var, labels = NULL, keys = NULL)
```

## Arguments

- dt:

  A `data.table` to partition.

- var:

  Character name of the splitting variable.

- labels:

  Optional character vector of arm labels; may be named to relabel
  specific factor levels.

- keys:

  Optional explicit set of factor levels to split against (shared across
  parents in a factorial split), keeping partitions rectangular.

## Value

A list with `data` (named list of per-arm `data.table`s) and `labels`
(character arm labels).
