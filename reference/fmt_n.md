# Format integer counts with a locale-aware thousands separator

Formats integer participant counts for display in diagram boxes and text
summaries. Values below 1000 are returned without a separator. The
function is vectorized: a vector of counts yields a parallel character
vector, so an entire set of exclusion sub-reasons can be formatted in a
single call.

## Usage

``` r
fmt_n(n, marks = NULL)
```

## Arguments

- n:

  Integer count value, or a vector of counts. `NA` elements are returned
  as empty strings.

- marks:

  List with `big.mark` and `decimal.mark` as returned by
  [`resolve_number_marks()`](https://phmcc.codeberg.page/selecta/reference/resolve_number_marks.md).
  May be `NULL`, in which case the current global setting is resolved
  automatically. `decimal.mark` is forwarded to
  [`format()`](https://rdrr.io/r/base/format.html) so that locales whose
  thousands separator is a period (*e.g.,* the `"eu"` preset) do not
  trip `format`'s "big.mark and decimal.mark are both '.'" warning.

## Value

A character vector of formatted counts, parallel to `n`.
