# Collapse Single-Child Parents in a Two-Level Reason List

For a nested reasons list, any parent whose breakdown is a single
sub-reason is replaced by a plain leaf carrying the parent's label and
count—the lone sub-reason is redundant. A flat reasons vector (no
parents) passes through unchanged.

## Usage

``` r
collapse_singleton_reasons(reasons)
```

## Arguments

- reasons:

  A reasons object: a named numeric vector (flat), or a list mixing
  scalar leaves and named sub-reason vectors (nested).

## Value

The reasons object with single-child parents collapsed to leaves.
