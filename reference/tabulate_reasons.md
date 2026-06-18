# Tabulate Exclusion Sub-Reasons

Counts occurrences of each reason category in a vector, treating `NA` as
`"Other"`, and returns counts sorted descending.

## Usage

``` r
tabulate_reasons(reason_col, sub_col = NULL)
```

## Arguments

- reason_col:

  A vector of reason values for the excluded participants.

## Value

A named integer vector of counts, ordered by descending count.
