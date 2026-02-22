#' Simulated Screening and Enrollment Dataset (2 Arms)
#'
#' A simulated dataset representing the screening and enrollment pipeline for a
#' multi-site oncology trial with 2,400 screened patients. The dataset includes
#' variables that drive eligibility decisions, treatment allocation, and
#' post-randomization attrition — designed for building CONSORT diagrams with
#' \code{\link{selecta}}.
#'
#' @format A data frame with 2,400 rows and 18 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (SCR0001-SCR2400)}
#'   \item{site}{Factor. Study site (Athens, Barcelona, Cleveland, Dublin,
#'     Edinburgh, Frankfurt)}
#'
#'   \strong{Screening & Eligibility:}
#'   \item{age}{Numeric. Age in years at screening (14-92)}
#'   \item{has_consent}{Logical. Informed consent obtained}
#'   \item{prior_chemo}{Logical. Prior chemotherapy within 6 months
#'     (exclusion criterion)}
#'   \item{ecog}{Integer. ECOG performance status (0-4). Patients with
#'     ECOG >= 3 are typically excluded.}
#'   \item{creatinine}{Numeric. Baseline serum creatinine in mg/dL. Patients
#'     above 2.0 are typically excluded for renal insufficiency.}
#'   \item{eligible}{Logical. Overall eligibility flag. \code{TRUE} if the
#'     patient meets all inclusion criteria (age 18-85, consent, no prior
#'     chemo, ECOG <= 2, creatinine <= 2.0). Computed from the individual
#'     screening variables.}
#'   \item{exclusion_reason}{Character. Primary reason for exclusion if
#'     \code{eligible == FALSE}. One of: \code{"Age outside 18-85"},
#'     \code{"No informed consent"}, \code{"Prior chemotherapy"},
#'     \code{"ECOG >= 3"}, \code{"Renal insufficiency"}, or \code{NA}
#'     if eligible. When multiple criteria fail, the first in protocol
#'     order is recorded.}
#'
#'   \strong{Randomization:}
#'   \item{treatment}{Factor. Treatment arm (\code{"Drug A"},
#'     \code{"Drug B"}, or \code{NA} if not randomized). Only assigned
#'     for eligible patients.}
#'
#'   \strong{Post-randomization Follow-up:}
#'   \item{received_treatment}{Logical. Actually received the allocated
#'     intervention. \code{FALSE} for patients who withdrew or had
#'     contraindications after randomization. \code{NA} if not randomized.}
#'   \item{discontinued}{Logical. Discontinued the study before the primary
#'     endpoint. \code{NA} if not randomized.}
#'   \item{discontinuation_reason}{Character. Reason for discontinuation if
#'     \code{discontinued == TRUE}. One of: \code{"Adverse event"},
#'     \code{"Withdrew consent"}, \code{"Lost to follow-up"},
#'     \code{"Disease progression"}, \code{"Physician decision"}, or
#'     \code{NA} if still in study or not randomized.}
#'   \item{completed_study}{Logical. Reached the primary analysis endpoint.
#'     \code{NA} if not randomized.}
#'   \item{protocol_violation}{Logical. Had a major protocol deviation.
#'     \code{NA} if not randomized.}
#'
#'   \strong{Duplicate / Data Quality:}
#'   \item{is_duplicate}{Logical. Record flagged as a duplicate screening
#'     entry (approximately 3\%). These would typically be excluded in the
#'     first step of a CONSORT diagram.}
#'   \item{screening_date}{Date. Date of screening visit}
#' }
#'
#' @details
#' The dataset is structured to support a realistic CONSORT flow:
#'
#' \strong{Screening:} 2,400 patients screened across 6 sites.
#'
#' \strong{Pre-randomization exclusions (typical flow):}
#' \enumerate{
#'   \item Remove duplicate records (\code{is_duplicate == TRUE})
#'   \item Apply age criteria (\code{age < 18 | age > 85})
#'   \item Require informed consent (\code{has_consent == FALSE})
#'   \item Exclude prior chemotherapy (\code{prior_chemo == TRUE})
#'   \item Exclude poor performance status (\code{ecog >= 3})
#'   \item Exclude renal insufficiency (\code{creatinine > 2.0})
#' }
#'
#' \strong{Randomization:} Eligible patients are allocated 1:1 to Drug A
#' or Drug B (\code{treatment}).
#'
#' \strong{Post-randomization attrition:}
#' \itemize{
#'   \item Some patients never receive treatment (\code{received_treatment == FALSE})
#'   \item Some discontinue for various reasons (\code{discontinued == TRUE})
#'   \item Remaining patients complete the study (\code{completed_study == TRUE})
#' }
#'
#' \strong{Exclusion reasons} are provided both as individual logical columns
#' (for use with \code{exclude(expr = ...)}) and as a summary column
#' \code{exclusion_reason} (for use with \code{exclude(reasons_var = ...)}).
#'
#' @source Simulated data generated for package demonstration purposes.
#'
#' @examples
#' data(rctselect2)
#'
#' # Full CONSORT diagram with phases and sub-reasons
#' library(selecta)
#'
#' flow <- enroll(consort_trial, id = "patient_id") |>
#'   phase("Screening") |>
#'   exclude("Duplicate records", expr = is_duplicate == TRUE) |>
#'   exclude("Failed eligibility",
#'           expr = eligible == FALSE,
#'           reasons_var = "exclusion_reason") |>
#'   phase("Allocation") |>
#'   arm("treatment", labels = c("Drug A" = "Drug A", "Drug B" = "Drug B")) |>
#'   phase("Follow-up") |>
#'   exclude("Discontinued", expr = discontinued == TRUE,
#'           reasons_var = "discontinuation_reason") |>
#'   phase("Analysis") |>
#'   endpoint("Completed study")
#'
#' # Render the diagram
#' flowchart(flow)
#'
#' # Extract the analysis-ready cohort
#' final <- cohort(flow)
#'
#' # Pipe directly to summata
#' \dontrun{
#' library(summata)
#' cohort(flow) |> summata(...)
#' }
#'
#' @name rctselect2
#' @docType data
#' @keywords datasets
NULL

set.seed(71)
n <- 2400

create_rctselect2 <- function() {

    ## ==========================================================================
    ## SITES
    ## ==========================================================================

    site_names <- c("Site Alpha", "Site Beta", "Site Gamma",
                    "Site Delta", "Site Epsilon", "Site Zeta")
    site_probs <- c(0.20, 0.18, 0.22, 0.15, 0.13, 0.12)
    site <- factor(
        sample(site_names, n, replace = TRUE, prob = site_probs),
        levels = site_names
    )

    ## ==========================================================================
    ## SCREENING DATE
    ## ==========================================================================

    screening_date <- as.Date("2021-03-01") +
        sort(sample(0:730, n, replace = TRUE))

    ## ==========================================================================
    ## DUPLICATES
    ## ==========================================================================

    is_duplicate <- logical(n)
    is_duplicate[sample(n, size = round(n * 0.03))] <- TRUE

    ## ==========================================================================
    ## SCREENING VARIABLES
    ## ==========================================================================

    ## Age: mostly 40-80 but include some out-of-range
    age <- round(rnorm(n, mean = 61, sd = 13))
    age <- pmax(14, pmin(age, 92))

    ## Consent: ~6% do not consent
    has_consent <- runif(n) > 0.06

    ## Prior chemo: ~10% (higher in older patients)
    prior_chemo_prob <- 0.08 + 0.002 * pmax(age - 50, 0)
    prior_chemo <- runif(n) < prior_chemo_prob

    ## ECOG: 0-4, most are 0-2
    ecog_probs <- c(0.30, 0.35, 0.20, 0.10, 0.05)
    ecog <- sample(0:4, n, replace = TRUE, prob = ecog_probs)

    ## Creatinine: right-skewed, most under 2.0
    creatinine <- round(rlnorm(n, meanlog = 0.0, sdlog = 0.35), 2)

    ## ==========================================================================
    ## ELIGIBILITY
    ## ==========================================================================

    fail_age      <- age < 18 | age > 85
    fail_consent  <- !has_consent
    fail_chemo    <- prior_chemo
    fail_ecog     <- ecog >= 3
    fail_renal    <- creatinine > 2.0

    eligible <- !is_duplicate & !fail_age & !fail_consent &
        !fail_chemo & !fail_ecog & !fail_renal

    ## Primary exclusion reason (first in protocol order, among non-duplicates)
    exclusion_reason <- character(n)
    exclusion_reason[] <- NA_character_
    for (i in seq_len(n)) {
        if (is_duplicate[i]) next
        if (fail_age[i])      { exclusion_reason[i] <- "Age outside 18-85"; next }
        if (fail_consent[i])  { exclusion_reason[i] <- "No informed consent"; next }
        if (fail_chemo[i])    { exclusion_reason[i] <- "Prior chemotherapy"; next }
        if (fail_ecog[i])     { exclusion_reason[i] <- "ECOG >= 3"; next }
        if (fail_renal[i])    { exclusion_reason[i] <- "Renal insufficiency"; next }
    }

    ## ==========================================================================
    ## RANDOMIZATION (eligible patients only)
    ## ==========================================================================

    treatment <- rep(NA_character_, n)
    idx_elig <- which(eligible)
    n_elig <- length(idx_elig)

    ## 1:1 allocation with slight site-level imbalance
    arm_assignment <- sample(c("Drug A", "Drug B"), n_elig, replace = TRUE)
    treatment[idx_elig] <- arm_assignment
    treatment <- factor(treatment, levels = c("Drug A", "Drug B"))

    ## ==========================================================================
    ## POST-RANDOMIZATION: RECEIVED TREATMENT
    ## ==========================================================================

    received_treatment <- rep(NA, n)
    ## ~5% of randomized patients don't receive treatment
    received_treatment[idx_elig] <- runif(n_elig) > 0.05

    ## ==========================================================================
    ## POST-RANDOMIZATION: DISCONTINUATION
    ## ==========================================================================

    discontinued <- rep(NA, n)
    discontinuation_reason <- rep(NA_character_, n)

    ## Among those who received treatment: ~18% discontinue
    ## Drug B has slightly higher discontinuation (~22% vs ~14%)
    idx_treated <- idx_elig[received_treatment[idx_elig] == TRUE]

    for (i in idx_treated) {
        base_rate <- if (treatment[i] == "Drug B") 0.22 else 0.14
        ## Higher ECOG and older age increase discontinuation
        adj_rate <- base_rate + 0.02 * ecog[i] + 0.001 * pmax(age[i] - 65, 0)
        adj_rate <- pmin(adj_rate, 0.45)

        if (runif(1) < adj_rate) {
            discontinued[i] <- TRUE

            ## Reason depends on arm and patient characteristics
            if (treatment[i] == "Drug B") {
                reason_probs <- c(0.35, 0.15, 0.20, 0.20, 0.10)
            } else {
                reason_probs <- c(0.20, 0.20, 0.25, 0.20, 0.15)
            }
            discontinuation_reason[i] <- sample(
                c("Adverse event", "Withdrew consent", "Lost to follow-up",
                  "Disease progression", "Physician decision"),
                1, prob = reason_probs
            )
        } else {
            discontinued[i] <- FALSE
        }
    }

    ## Patients who didn't receive treatment are also "discontinued"
    idx_not_treated <- idx_elig[received_treatment[idx_elig] == FALSE]
    discontinued[idx_not_treated] <- TRUE
    discontinuation_reason[idx_not_treated] <- sample(
        c("Withdrew consent", "Physician decision"),
        length(idx_not_treated), replace = TRUE, prob = c(0.7, 0.3)
    )

    ## ==========================================================================
    ## COMPLETED STUDY
    ## ==========================================================================

    completed_study <- rep(NA, n)
    completed_study[idx_elig] <- FALSE
    completed_study[idx_elig[discontinued[idx_elig] == FALSE]] <- TRUE

    ## ==========================================================================
    ## PROTOCOL VIOLATIONS (among those who completed)
    ## ==========================================================================

    protocol_violation <- rep(NA, n)
    protocol_violation[idx_elig] <- FALSE
    idx_completed <- idx_elig[completed_study[idx_elig] == TRUE]
    protocol_violation[sample(idx_completed,
                              size = round(length(idx_completed) * 0.04))] <- TRUE

    ## ==========================================================================
    ## ASSEMBLE
    ## ==========================================================================

    rctselect2 <- data.frame(
        patient_id            = sprintf("SCR%04d", 1:n),
        site                  = site,
        screening_date        = screening_date,
        age                   = age,
        has_consent           = has_consent,
        prior_chemo           = prior_chemo,
        ecog                  = ecog,
        creatinine            = creatinine,
        eligible              = eligible,
        exclusion_reason      = exclusion_reason,
        is_duplicate          = is_duplicate,
        treatment             = treatment,
        received_treatment    = received_treatment,
        discontinued          = discontinued,
        discontinuation_reason = discontinuation_reason,
        completed_study       = completed_study,
        protocol_violation    = protocol_violation,
        stringsAsFactors      = FALSE
    )

    return(rctselect2)
}

rctselect2 <- create_rctselect2()

usethis::use_data(rctselect2, overwrite = TRUE)
