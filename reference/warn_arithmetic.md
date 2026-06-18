# Warn About an Inconsistency in a Flow

Emits a [`warning()`](https://rdrr.io/r/base/warning.html) describing a
counting or attribution inconsistency in a flow—for example, manual arm
counts that do not sum to the number entering a split, an exclusion
larger than the available count, or a data-mode reason column that does
not account for every removed row. Counts are never altered or rejected,
since an author may have a legitimate reason for figures that do not
reconcile; the warning is purely advisory and may be silenced with
`options(selecta.check_arithmetic = FALSE)`.

## Usage

``` r
warn_arithmetic(fmt, ...)
```

## Arguments

- fmt:

  A `sprintf` format string for the message.

- ...:

  Values substituted into `fmt`.

## Value

Invisibly `NULL`; called for its side effect.
