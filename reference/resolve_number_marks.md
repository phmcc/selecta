# Resolve number format marks

Converts a `number_format` specification into a list of `big.mark` and
`decimal.mark` values used by all downstream formatting functions.
Supports named presets, custom two-element vectors, and the global
`selecta.number_format` option.

## Usage

``` r
resolve_number_marks(number_format = NULL)
```

## Arguments

- number_format:

  Character string specifying a named preset, a two-element character
  vector `c(big.mark, decimal.mark)`, or `NULL` to use the global option
  (falling back to `"us"`).

  Named presets:

  `"us"`

  :   Comma thousands, period decimal: 1,234.56

  `"eu"`

  :   Period thousands, comma decimal: 1.234,56

  `"space"`

  :   Thin-space thousands, period decimal: 1 234.56 (SI/ISO 31-0
      standard)

  `"none"`

  :   No thousands separator, period decimal: 1234.56

  Custom vector: `c(",", ".")` or `c(".", ",")` *etc.* The first element
  is `big.mark`, the second is `decimal.mark`.

## Value

A list with components `big.mark` and `decimal.mark`.
