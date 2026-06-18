# Record an Assessment or Procedure Step

Models a step where participants undergo (or fail to undergo) a test or
procedure. This is the primary building block for STARD-style diagnostic
accuracy diagrams. The side box shows who did *not* receive the
procedure (with optional reasons), and the main flow continues with
those who *were* assessed.

## Usage

``` r
assess(
  .flow,
  label,
  criterion,
  not_received = NULL,
  reasons = NULL,
  show_zero = FALSE
)
```

## Arguments

- .flow:

  A `selecta` object.

- label:

  Character string naming the test or procedure (*e.g.,* `"Index test"`,
  `"Reference standard"`).

- criterion:

  An unquoted logical expression that evaluates to `TRUE` for rows that
  did **not** receive the test. Data mode only.

- not_received:

  Integer (manual mode). Number of participants who did not receive this
  test.

- reasons:

  Named integer vector of reasons for non-receipt (*e.g.,*
  `c("Refused" = 12, "Contraindicated" = 10)`).

- show_zero:

  Logical. If `TRUE`, display zero-count reasons. Default `FALSE`.

## Value

The updated `selecta` object with an assessment step appended.

## Details

`assess()` models a test or procedure that only part of the cohort
undergoes, the recurring motif of STARD diagnostic-accuracy diagrams. It
is implemented as an
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
step with inverted label semantics: the side box reads “Did not receive
*label*” and the continuing box reads “Received *label*”, so the main
flow carries those who *were* assessed. In data mode, `criterion` is an
unquoted logical expression that is `TRUE` for participants who did
**not** receive the test; in manual mode, `not_received` gives that
count and `reasons` an optional named breakdown. Chained `assess()`
steps commonly precede a
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
split on the index-test result, with each terminal box reporting its
target-condition breakdown.

## See also

[`exclude`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
for general exclusion steps,
[`endpoint`](https://phmcc.codeberg.page/selecta/reference/endpoint.md)
for the terminal diagnosis boxes (STARD)

Other flow construction functions:
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
# STARD diagnostic accuracy flow
enroll(n = 360, label = "Eligible patients") |>
  assess("Index test", not_received = 22,
         reasons = c("Refused" = 12, "Contraindicated" = 10)) |>
  assess("Reference standard", not_received = 18) |>
  stratify(labels = c("Index test positive", "Index test negative"),
           n = c(150, 170), label = "Index test result") |>
  endpoint("Final diagnosis",
           breakdown = list(c("Target +" = 130, "Target -" = 20),
                            c("Target +" = 15, "Target -" = 155)))
#> selecta flow (manual mode)
#>   Starting N: 360
#>   Steps: 4
#>   [1] exclude: "Did not receive index test" (n = 22)
#>          • Refused = 12
#>          • Contraindicated = 10
#> 
#>   [2] exclude: "Did not receive reference standard" (n = 18)
#>   [3] stratify: Index test positive, Index test negative
#>          label: "Index test result"
#>   [4] endpoint: "Final diagnosis"
#>        Arm 1:
#>          • Target + = 130
#>          • Target - = 20
#> 
#>        Arm 2:
#>          • Target + = 15
#>          • Target - = 155
#> 
```
