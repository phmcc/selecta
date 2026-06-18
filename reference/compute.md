# Compute Enrollment Counts

Walks the step list and resolves all counts, producing a graph of nodes,
edges, and phases. Maintains a generalized stream model where parallel
tracks (from
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md)
or
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md))
are stored as a list of active streams.

## Usage

``` r
compute(x)
```

## Arguments

- x:

  A `selecta` object.

## Value

A list with components `nodes`, `edges`, and `phases`, each a
`data.table`.
