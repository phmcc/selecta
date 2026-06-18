# Simulated Observational Cohort (No Arms)

A synthetic dataset of 3,000 patients in an observational study with no
treatment arms. Includes eligibility flags, exclusion reasons, and
follow-up loss indicators suitable for demonstrating STROBE-style
enrollment diagrams in data mode.

## Usage

``` r
selectaex0
```

## Format

A `data.table` with 3,000 rows and the following columns:

- patient_id:

  Unique patient identifier.

- is_duplicate:

  Logical. Whether the record is a duplicate.

- eligible:

  Logical. Whether the patient meets eligibility criteria.

- exclusion_reason:

  Character. Reason for exclusion, if applicable.

- lost_to_followup:

  Logical. Whether the patient was lost to follow-up.

- followup_loss_reason:

  Character. Reason for follow-up loss, if applicable.

## Examples

``` r
data(selectaex0)
str(selectaex0)
#> 'data.frame':    3000 obs. of  14 variables:
#>  $ patient_id          : chr  "OBS0001" "OBS0002" "OBS0003" "OBS0004" ...
#>  $ site                : Factor w/ 5 levels "Site Alpha","Site Beta",..: 3 2 3 1 3 5 2 5 3 3 ...
#>  $ enrollment_date     : Date, format: "2020-06-01" "2020-06-01" "2020-06-01" "2020-06-02" ...
#>  $ age                 : num  57 73 51 73 89 73 50 65 57 53 ...
#>  $ has_consent         : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ prior_surgery       : logi  FALSE FALSE TRUE FALSE FALSE FALSE ...
#>  $ bmi                 : num  21.8 20.8 23.9 28.5 24.3 33.1 16.9 25.8 29.1 24.1 ...
#>  $ hba1c               : num  5.2 7.1 4.6 6.4 5.6 4.9 4.6 7.3 6.6 7.5 ...
#>  $ eligible            : logi  TRUE TRUE FALSE TRUE FALSE TRUE ...
#>  $ exclusion_reason    : chr  NA NA "Prior surgery" NA ...
#>  $ is_duplicate        : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ lost_to_followup    : logi  TRUE TRUE NA FALSE NA FALSE ...
#>  $ followup_loss_reason: chr  "Deceased" "Withdrew consent" NA NA ...
#>  $ completed_study     : logi  FALSE FALSE NA TRUE NA TRUE ...
```
