# Convert a Length Between Measurement Units

Converts a numeric length between the units accepted by
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
and
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).
Inches are the package's internal representation, so conversion is
routed through inches in both directions.

## Usage

``` r
convert_units(value, from = "in", to = "in")
```

## Arguments

- value:

  Numeric length, or a vector of lengths.

- from, to:

  Character strings naming the source and target units, each one of
  `"in"`, `"cm"`, or `"mm"`.

## Value

A numeric vector parallel to `value`, expressed in `to`.
