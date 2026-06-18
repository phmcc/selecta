# Split into Parallel Study Arms or Strata

Divides the enrollment flow into parallel arms. This is the primary
function for splitting a population by any characteristic: treatment
assignment, exposure status, diagnostic test result, etc. Subsequent
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
calls apply within each arm independently. While `stratify()` is the
primary function, `allocate()` is provided as a convenience alias with
default label `"Randomized"`, suitable for interventional trials
(CONSORT).

## Usage

``` r
stratify(.flow, variable = NULL, labels = NULL, n = NULL, label = "Stratified")

allocate(.flow, variable = NULL, labels = NULL, n = NULL, label = "Randomized")
```

## Arguments

- .flow:

  A `selecta` object.

- variable:

  Character string naming the column that defines the arms. Data mode
  only.

- labels:

  A character vector of arm labels. In data mode, this can be a named
  vector to relabel factor levels (*e.g.,*
  `c(A = "Drug A", B = "Placebo")`). In manual mode, these are the arm
  names.

- n:

  Integer vector. Number of participants in each arm, in the same order
  as `labels`. Manual mode only.

- label:

  Character string for the split box. Defaults to `"Stratified"` for
  `stratify()` and `"Randomized"` for `allocate()`.

## Value

The updated `selecta` object with a stratification step appended. All
subsequent pipeline steps operate independently within each arm.

## Details

`stratify()` splits the flow into parallel arms, after which each
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
(and the eventual
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md))
applies within every arm. In data mode, `variable` names a column whose
levels define the arms, optionally relabeled through a named `labels`
vector; in manual mode, `labels` and `n` give the arm names and per-arm
counts directly.

`allocate()` is an identical alias differing only in its default `label`
(`"Randomized"`), provided so that interventional trials (CONSORT) read
naturally; both record the same step type.

Parallel arms may later be merged with
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md)
to form a split-and-recombine diagram, and a flow may be split again
after combining. A second `stratify()` or `allocate()` before combining
produces a factorial (two-level) split, supported in both data and
manual modes.

## See also

[`exclude`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
for per-arm exclusions after splitting,
[`endpoint`](https://phmcc.codeberg.page/selecta/reference/endpoint.md)
for per-arm endpoints

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md)

## Examples

``` r
# Observational study (STROBE)
enroll(n = 3860) |>
  stratify(labels = c("Exposed", "Unexposed"), n = c(1900, 1960),
           label = "Classified by exposure")
#> selecta flow (manual mode)
#>   Starting N: 3.860
#>   Steps: 1
#>   [1] stratify: Exposed, Unexposed
#>          label: "Classified by exposure"

# Randomized trial (CONSORT)
enroll(n = 400) |>
  allocate(labels = c("Drug A", "Placebo"), n = c(200, 200))
#> selecta flow (manual mode)
#>   Starting N: 400
#>   Steps: 1
#>   [1] stratify: Drug A, Placebo
#>          label: "Randomized"
```
