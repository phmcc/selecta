# Validate number_format parameter

Checks that a `number_format` value is valid before use. Called early in
top-level functions to fail fast with a clear error message.

## Usage

``` r
validate_number_format(number_format)
```

## Arguments

- number_format:

  Value to validate.

## Value

Invisibly returns `TRUE` if valid.
