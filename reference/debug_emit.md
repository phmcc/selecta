# Emit a Debug Section When Layout Debugging Is Enabled

Prints a titled section followed by one or more objects via
[`message()`](https://rdrr.io/r/base/message.html), but only when
`options(selecta.debug_layout = TRUE)` is set. Used by the computation
and rendering functions to expose intermediate state for diagnosis; a
no-op otherwise.

## Usage

``` r
debug_emit(title, ...)
```

## Arguments

- title:

  Character section title.

- ...:

  Named or unnamed objects to print; data frames and tables are captured
  via [`print()`](https://rdrr.io/r/base/print.html), scalars are shown
  inline.

## Value

Invisibly `NULL`; called for its side effect.
