#' Simulated Observational Cohort Dataset (No Arms)
#'
#' A simulated dataset representing the screening and follow-up pipeline for a
#' single-arm observational cohort study with 3,000 screened patients. The
#' dataset includes variables that drive eligibility decisions and
#' post-enrollment attrition — designed for building single-flow CONSORT
#' diagrams with \code{\link{selecta}}.
#'
#' @format A data frame with 3,000 rows and 14 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (OBS0001-OBS3000)}
#'   \item{site}{Factor. Study site (Site Alpha, Site Beta, Site Gamma,
#'     Site Delta, Site Epsilon)}
#'
#'   \strong{Screening & Eligibility:}
#'   \item{age}{Numeric. Age in years at screening (16-95)}
#'   \item{has_consent}{Logical. Informed consent obtained}
#'   \item{prior_surgery}{Logical. Prior relevant surgery within 12 months
#'     (exclusion criterion)}
#'   \item{bmi}{Numeric. Body mass index at screening (15-55)}
#'   \item{hba1c}{Numeric. Baseline HbA1c in \% (3.5-14). Patients above
#'     10 are typically excluded.}
#'   \item{eligible}{Logical. Overall eligibility flag. \code{TRUE} if the
#'     patient meets all inclusion criteria (age 18-80, consent, no prior
#'     surgery, BMI <= 40, HbA1c <= 10). Computed from the individual
#'     screening variables.}
#'   \item{exclusion_reason}{Character. Primary reason for exclusion if
#'     \code{eligible == FALSE}. One of: \code{"Age outside 18-80"},
#'     \code{"No informed consent"}, \code{"Prior surgery"},
#'     \code{"BMI > 40"}, \code{"HbA1c > 10"}, or \code{NA}
#'     if eligible. When multiple criteria fail, the first in protocol
#'     order is recorded.}
#'
#'   \strong{Post-enrollment Follow-up:}
#'   \item{lost_to_followup}{Logical. Lost to follow-up before primary
#'     endpoint. \code{NA} if not enrolled.}
#'   \item{followup_loss_reason}{Character. Reason for loss to follow-up if
#'     \code{lost_to_followup == TRUE}. One of: \code{"Withdrew consent"},
#'     \code{"Lost to contact"}, \code{"Moved away"}, \code{"Deceased"},
#'     \code{"Other"}, or \code{NA} if still in study or not enrolled.}
#'   \item{completed_study}{Logical. Reached the primary analysis endpoint.
#'     \code{NA} if not enrolled.}
#'
#'   \strong{Duplicate / Data Quality:}
#'   \item{is_duplicate}{Logical. Record flagged as a duplicate screening
#'     entry (approximately 2.5\%). These would typically be excluded in the
#'     first step of a CONSORT diagram.}
#'   \item{enrollment_date}{Date. Date of screening visit}
#' }
#'
#' @details
#' The dataset is structured to support a single-flow CONSORT diagram (no
#' randomization or treatment arms):
#'
#' \strong{Screening:} 3,000 patients screened across 5 sites.
#'
#' \strong{Pre-enrollment exclusions (typical flow):}
#' \enumerate{
#'   \item Remove duplicate records (\code{is_duplicate == TRUE})
#'   \item Apply age criteria (\code{age < 18 | age > 80})
#'   \item Require informed consent (\code{has_consent == FALSE})
#'   \item Exclude prior surgery (\code{prior_surgery == TRUE})
#'   \item Exclude high BMI (\code{bmi > 40})
#'   \item Exclude high HbA1c (\code{hba1c > 10})
#' }
#'
#' \strong{Follow-up attrition:}
#' \itemize{
#'   \item ~15-35\% of enrolled patients are lost to follow-up (rate
#'     increases with age)
#'   \item Loss reasons include withdrawal, lost contact, relocation,
#'     death, and other
#' }
#'
#' \strong{Exclusion reasons} are provided both as individual logical columns
#' (for use with \code{exclude(expr = ...)}) and as a summary column
#' \code{exclusion_reason} (for use with \code{exclude(reasons = ...)}).
#'
#' @source Simulated data generated for package demonstration purposes.
#'
#' @examples
#' data(rctselect0)
#'
#' # Single-flow CONSORT diagram (no arms)
#' library(selecta)
#'
#' flow <- enroll(rctselect0, id = "patient_id") |>
#'   phase("Screening") |>
#'   exclude("Duplicate records", expr = is_duplicate == TRUE) |>
#'   exclude("Failed eligibility",
#'           expr = eligible == FALSE & is_duplicate == FALSE,
#'           reasons = "exclusion_reason") |>
#'   phase("Follow-up") |>
#'   exclude("Lost to follow-up",
#'           expr = lost_to_followup == TRUE,
#'           reasons = "followup_loss_reason") |>
#'   phase("Analysis") |>
#'   endpoint("Analysis cohort", expr = completed_study == TRUE)
#'
#' # Render the diagram
#' flowchart(flow)
#'
#' @name rctselect0
#' @docType data
#' @keywords datasets
NULL

set.seed(42)
n <- 3000

create_rctselect0 <- function() {

    ## ==========================================================================
    ## SITES
    ## ==========================================================================

    site_names <- c("Site Alpha", "Site Beta", "Site Gamma",
                    "Site Delta", "Site Epsilon")
    site_probs <- c(0.25, 0.20, 0.22, 0.18, 0.15)
    site <- factor(
        sample(site_names, n, replace = TRUE, prob = site_probs),
        levels = site_names
    )

    ## ==========================================================================
    ## ENROLLMENT DATE
    ## ==========================================================================

    enrollment_date <- as.Date("2020-06-01") +
        sort(sample(0:900, n, replace = TRUE))

    ## ==========================================================================
    ## DUPLICATES
    ## ==========================================================================

    is_duplicate <- logical(n)
    is_duplicate[sample(n, size = round(n * 0.025))] <- TRUE

    ## ==========================================================================
    ## SCREENING VARIABLES
    ## ==========================================================================

    ## Age: mostly 40-75 but include some out-of-range
    age <- round(rnorm(n, mean = 58, sd = 14))
    age <- pmax(16, pmin(age, 95))

    ## Consent: ~5% do not consent
    has_consent <- runif(n) > 0.05

    ## Prior surgery: ~12%
    prior_surgery <- runif(n) < 0.12

    ## BMI: normal distribution centered at 27
    bmi <- round(rnorm(n, mean = 27, sd = 5), 1)
    bmi <- pmax(15, pmin(bmi, 55))

    ## HbA1c: slightly right-skewed
    hba1c <- round(rnorm(n, mean = 6.2, sd = 1.5), 1)
    hba1c <- pmax(3.5, pmin(hba1c, 14))

    ## ==========================================================================
    ## ELIGIBILITY
    ## ==========================================================================

    fail_age      <- age < 18 | age > 80
    fail_consent  <- !has_consent
    fail_surgery  <- prior_surgery
    fail_bmi      <- bmi > 40
    fail_hba1c    <- hba1c > 10

    eligible <- !is_duplicate & !fail_age & !fail_consent &
        !fail_surgery & !fail_bmi & !fail_hba1c

    ## Primary exclusion reason (first in protocol order, among non-duplicates)
    exclusion_reason <- character(n)
    exclusion_reason[] <- NA_character_
    for (i in seq_len(n)) {
        if (is_duplicate[i]) next
        if (fail_age[i])      { exclusion_reason[i] <- "Age outside 18-80"; next }
        if (fail_consent[i])  { exclusion_reason[i] <- "No informed consent"; next }
        if (fail_surgery[i])  { exclusion_reason[i] <- "Prior surgery"; next }
        if (fail_bmi[i])      { exclusion_reason[i] <- "BMI > 40"; next }
        if (fail_hba1c[i])    { exclusion_reason[i] <- "HbA1c > 10"; next }
    }

    ## ==========================================================================
    ## POST-ENROLLMENT: LOSS TO FOLLOW-UP (no randomization)
    ## ==========================================================================

    idx_elig <- which(eligible)
    n_elig <- length(idx_elig)

    lost_to_followup <- rep(NA, n)
    followup_loss_reason <- rep(NA_character_, n)

    for (i in idx_elig) {
        ## Loss rate increases with age
        loss_rate <- 0.15 + 0.005 * pmax(age[i] - 60, 0)
        loss_rate <- pmin(loss_rate, 0.35)

        if (runif(1) < loss_rate) {
            lost_to_followup[i] <- TRUE
            followup_loss_reason[i] <- sample(
                c("Withdrew consent", "Lost to contact",
                  "Moved away", "Deceased", "Other"),
                1, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)
            )
        } else {
            lost_to_followup[i] <- FALSE
        }
    }

    ## ==========================================================================
    ## COMPLETED STUDY
    ## ==========================================================================

    completed_study <- rep(NA, n)
    completed_study[idx_elig] <- !lost_to_followup[idx_elig]

    ## ==========================================================================
    ## ASSEMBLE
    ## ==========================================================================

    rctselect0 <- data.frame(
        patient_id           = sprintf("OBS%04d", 1:n),
        site                 = site,
        enrollment_date      = enrollment_date,
        age                  = age,
        has_consent          = has_consent,
        prior_surgery        = prior_surgery,
        bmi                  = bmi,
        hba1c                = hba1c,
        eligible             = eligible,
        exclusion_reason     = exclusion_reason,
        is_duplicate         = is_duplicate,
        lost_to_followup     = lost_to_followup,
        followup_loss_reason = followup_loss_reason,
        completed_study      = completed_study,
        stringsAsFactors     = FALSE
    )

    return(rctselect0)
}

rctselect0 <- create_rctselect0()

usethis::use_data(rctselect0, overwrite = TRUE)
