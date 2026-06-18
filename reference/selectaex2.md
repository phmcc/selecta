# Simulated Two-Arm Randomized Trial

A synthetic dataset of 2,400 patients in a two-arm randomized controlled
trial. Includes screening, eligibility, treatment assignment, and
discontinuation variables suitable for demonstrating CONSORT-style
enrollment diagrams in data mode.

## Usage

``` r
selectaex2
```

## Format

A `data.table` with 2,400 rows and the following columns:

- patient_id:

  Unique patient identifier.

- is_duplicate:

  Logical. Whether the record is a duplicate.

- eligible:

  Logical. Whether the patient meets eligibility criteria.

- exclusion_reason:

  Character. Reason for exclusion, if applicable.

- treatment:

  Character. Treatment arm assignment (*e.g.,* `"Drug A"`, `"Placebo"`).

- discontinued:

  Logical. Whether the patient discontinued the study.

- discontinuation_reason:

  Character. Reason for discontinuation, if applicable.

## Examples

``` r
data(selectaex2)
str(selectaex2)
#> 'data.frame':    2400 obs. of  17 variables:
#>  $ patient_id            : chr  "SCR0001" "SCR0002" "SCR0003" "SCR0004" ...
#>  $ site                  : Factor w/ 6 levels "Site Alpha","Site Beta",..: 1 2 1 3 1 6 4 6 1 2 ...
#>  $ screening_date        : Date, format: "2021-03-01" "2021-03-01" "2021-03-01" "2021-03-01" ...
#>  $ age                   : num  64 71 71 49 52 54 56 46 69 75 ...
#>  $ has_consent           : logi  TRUE TRUE TRUE TRUE FALSE TRUE ...
#>  $ prior_chemo           : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
#>  $ ecog                  : int  0 1 0 2 2 4 1 2 3 0 ...
#>  $ creatinine            : num  0.74 1.57 1.58 0.69 1.13 0.43 1.96 1.14 0.65 0.72 ...
#>  $ eligible              : logi  TRUE TRUE TRUE TRUE FALSE FALSE ...
#>  $ exclusion_reason      : chr  NA NA NA NA ...
#>  $ is_duplicate          : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ treatment             : Factor w/ 2 levels "Drug A","Drug B": 2 2 2 1 NA NA 2 1 NA 1 ...
#>  $ received_treatment    : logi  TRUE TRUE TRUE TRUE NA NA ...
#>  $ discontinued          : logi  FALSE FALSE FALSE FALSE NA NA ...
#>  $ discontinuation_reason: chr  NA NA NA NA ...
#>  $ completed_study       : logi  TRUE TRUE TRUE TRUE NA NA ...
#>  $ protocol_violation    : logi  FALSE FALSE FALSE FALSE NA NA ...
table(selectaex2$treatment)
#> 
#> Drug A Drug B 
#>    770    779 
```
