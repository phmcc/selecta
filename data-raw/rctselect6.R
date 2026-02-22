#' Simulated Dose-Finding Trial Dataset (6 Arms)
#'
#' A simulated dataset representing the screening and enrollment pipeline for a
#' multi-site dose-finding trial with 3,600 screened patients and 6 treatment
#' arms (placebo + 5 dose levels). The dataset includes variables that drive
#' eligibility decisions, treatment allocation, and post-randomization
#' attrition — designed for stress-testing multi-arm CONSORT diagram layouts
#' with \code{\link{selecta}}.
#'
#' @format A data frame with 3,600 rows and 16 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (DOS0001-DOS3600)}
#'   \item{site}{Factor. Study site (Site Alpha through Site Theta; 8 sites)}
#'
#'   \strong{Screening & Eligibility:}
#'   \item{age}{Numeric. Age in years at screening (16-90)}
#'   \item{has_consent}{Logical. Informed consent obtained}
#'   \item{prior_therapy}{Logical. Prior relevant therapy within 6 months
#'     (exclusion criterion)}
#'   \item{ecog}{Integer. ECOG performance status (0-4). Patients with
#'     ECOG >= 3 are typically excluded.}
#'   \item{creatinine}{Numeric. Baseline serum creatinine in mg/dL. Patients
#'     above 1.8 are typically excluded for renal insufficiency.}
#'   \item{eligible}{Logical. Overall eligibility flag. \code{TRUE} if the
#'     patient meets all inclusion criteria (age 18-75, consent, no prior
#'     therapy, ECOG <= 2, creatinine <= 1.8). Computed from the individual
#'     screening variables.}
#'   \item{exclusion_reason}{Character. Primary reason for exclusion if
#'     \code{eligible == FALSE}. One of: \code{"Age outside 18-75"},
#'     \code{"No informed consent"}, \code{"Prior therapy"},
#'     \code{"ECOG >= 3"}, \code{"Renal insufficiency"}, or \code{NA}
#'     if eligible. When multiple criteria fail, the first in protocol
#'     order is recorded.}
#'
#'   \strong{Randomization:}
#'   \item{treatment}{Factor. Treatment arm (\code{"Placebo"},
#'     \code{"5 mg QD"}, \code{"10 mg QD"}, \code{"25 mg QD"},
#'     \code{"50 mg QD"}, \code{"100 mg QD"}, or \code{NA} if not
#'     randomized). Only assigned for eligible patients.}
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
#'
#'   \strong{Duplicate / Data Quality:}
#'   \item{is_duplicate}{Logical. Record flagged as a duplicate screening
#'     entry (approximately 2.8\%). These would typically be excluded in the
#'     first step of a CONSORT diagram.}
#'   \item{screening_date}{Date. Date of screening visit}
#' }
#'
#' @details
#' The dataset is structured to support a 6-arm CONSORT flow:
#'
#' \strong{Screening:} 3,600 patients screened across 8 sites.
#'
#' \strong{Pre-randomization exclusions (typical flow):}
#' \enumerate{
#'   \item Remove duplicate records (\code{is_duplicate == TRUE})
#'   \item Apply age criteria (\code{age < 18 | age > 75})
#'   \item Require informed consent (\code{has_consent == FALSE})
#'   \item Exclude prior therapy (\code{prior_therapy == TRUE})
#'   \item Exclude poor performance status (\code{ecog >= 3})
#'   \item Exclude renal insufficiency (\code{creatinine > 1.8})
#' }
#'
#' \strong{Randomization:} Eligible patients are allocated 1:1:1:1:1:1 to
#' Placebo, 5 mg QD, 10 mg QD, 25 mg QD, 50 mg QD, or 100 mg QD
#' (\code{treatment}).
#'
#' \strong{Post-randomization attrition:}
#' \itemize{
#'   \item Discontinuation rates are dose-dependent: ~10\% (5 mg) to ~30\%
#'     (100 mg), with higher-dose arms showing more adverse-event-driven
#'     discontinuation
#'   \item Some patients never receive treatment
#'     (\code{received_treatment == FALSE})
#'   \item Remaining patients complete the study
#'     (\code{completed_study == TRUE})
#' }
#'
#' \strong{Exclusion reasons} are provided both as individual logical columns
#' (for use with \code{exclude(expr = ...)}) and as a summary column
#' \code{exclusion_reason} (for use with \code{exclude(reasons = ...)}).
#'
#' @source Simulated data generated for package demonstration purposes.
#'
#' @examples
#' data(rctselect6)
#'
#' # 6-arm dose-finding CONSORT diagram
#' library(selecta)
#'
#' flow <- enroll(rctselect6, id = "patient_id") |>
#'   phase("Screening") |>
#'   exclude("Duplicate records", expr = is_duplicate == TRUE) |>
#'   exclude("Failed eligibility",
#'           expr = eligible == FALSE & is_duplicate == FALSE,
#'           reasons = "exclusion_reason") |>
#'   phase("Allocation") |>
#'   arm("treatment") |>
#'   phase("Follow-up") |>
#'   exclude("Discontinued",
#'           expr = discontinued == TRUE,
#'           reasons = "discontinuation_reason") |>
#'   phase("Analysis") |>
#'   endpoint("Completed study", expr = completed_study == TRUE)
#'
#' # Render the diagram
#' flowchart(flow)
#'
#' @name rctselect6
#' @docType data
#' @keywords datasets
NULL

set.seed(99)
n <- 3600

create_rctselect6 <- function() {

    ## ==========================================================================
    ## SITES
    ## ==========================================================================

    site_names <- c("Site Alpha", "Site Beta", "Site Gamma",
                    "Site Delta", "Site Epsilon", "Site Zeta",
                    "Site Eta", "Site Theta")
    site_probs <- c(0.16, 0.14, 0.15, 0.13, 0.12, 0.11, 0.10, 0.09)
    site <- factor(
        sample(site_names, n, replace = TRUE, prob = site_probs),
        levels = site_names
    )

    ## ==========================================================================
    ## SCREENING DATE
    ## ==========================================================================

    screening_date <- as.Date("2022-01-01") +
        sort(sample(0:600, n, replace = TRUE))

    ## ==========================================================================
    ## DUPLICATES
    ## ==========================================================================

    is_duplicate <- logical(n)
    is_duplicate[sample(n, size = round(n * 0.028))] <- TRUE

    ## ==========================================================================
    ## SCREENING VARIABLES
    ## ==========================================================================

    ## Age: mostly 40-70 but include some out-of-range
    age <- round(rnorm(n, mean = 55, sd = 12))
    age <- pmax(16, pmin(age, 90))

    ## Consent: ~4% do not consent
    has_consent <- runif(n) > 0.04

    ## Prior therapy: ~10%
    prior_therapy <- runif(n) < 0.10

    ## ECOG: 0-4, most are 0-2
    ecog_probs <- c(0.32, 0.35, 0.20, 0.08, 0.05)
    ecog <- sample(0:4, n, replace = TRUE, prob = ecog_probs)

    ## Creatinine: right-skewed, most under 1.8
    creatinine <- round(rlnorm(n, meanlog = 0.0, sdlog = 0.30), 2)

    ## ==========================================================================
    ## ELIGIBILITY
    ## ==========================================================================

    fail_age      <- age < 18 | age > 75
    fail_consent  <- !has_consent
    fail_therapy  <- prior_therapy
    fail_ecog     <- ecog >= 3
    fail_renal    <- creatinine > 1.8

    eligible <- !is_duplicate & !fail_age & !fail_consent &
        !fail_therapy & !fail_ecog & !fail_renal

    ## Primary exclusion reason (first in protocol order, among non-duplicates)
    exclusion_reason <- character(n)
    exclusion_reason[] <- NA_character_
    for (i in seq_len(n)) {
        if (is_duplicate[i]) next
        if (fail_age[i])      { exclusion_reason[i] <- "Age outside 18-75"; next }
        if (fail_consent[i])  { exclusion_reason[i] <- "No informed consent"; next }
        if (fail_therapy[i])  { exclusion_reason[i] <- "Prior therapy"; next }
        if (fail_ecog[i])     { exclusion_reason[i] <- "ECOG >= 3"; next }
        if (fail_renal[i])    { exclusion_reason[i] <- "Renal insufficiency"; next }
    }

    ## ==========================================================================
    ## RANDOMIZATION: 6-ARM DOSE-FINDING (eligible patients only)
    ## ==========================================================================

    arm_labels <- c("Placebo", "5 mg QD", "10 mg QD",
                    "25 mg QD", "50 mg QD", "100 mg QD")

    treatment <- rep(NA_character_, n)
    idx_elig <- which(eligible)
    n_elig <- length(idx_elig)

    ## 1:1:1:1:1:1 allocation
    arm_assignment <- sample(arm_labels, n_elig, replace = TRUE)
    treatment[idx_elig] <- arm_assignment
    treatment <- factor(treatment, levels = arm_labels)

    ## ==========================================================================
    ## POST-RANDOMIZATION: RECEIVED TREATMENT
    ## ==========================================================================

    received_treatment <- rep(NA, n)
    ## ~4% of randomized patients don't receive treatment
    received_treatment[idx_elig] <- runif(n_elig) > 0.04

    ## ==========================================================================
    ## POST-RANDOMIZATION: DISCONTINUATION
    ## ==========================================================================

    discontinued <- rep(NA, n)
    discontinuation_reason <- rep(NA_character_, n)

    idx_treated <- idx_elig[received_treatment[idx_elig] == TRUE]

    ## Dose-dependent discontinuation rates
    disc_rates <- c("Placebo" = 0.12, "5 mg QD" = 0.10, "10 mg QD" = 0.14,
                    "25 mg QD" = 0.18, "50 mg QD" = 0.24, "100 mg QD" = 0.30)

    for (i in idx_treated) {
        trt <- as.character(treatment[i])
        base_rate <- disc_rates[trt]
        ## Higher ECOG and older age increase discontinuation
        adj_rate <- base_rate + 0.01 * ecog[i] + 0.001 * pmax(age[i] - 60, 0)
        adj_rate <- pmin(adj_rate, 0.50)

        if (runif(1) < adj_rate) {
            discontinued[i] <- TRUE

            ## Reason probabilities vary by dose level
            if (trt %in% c("50 mg QD", "100 mg QD")) {
                reason_probs <- c(0.40, 0.15, 0.15, 0.15, 0.15)
            } else if (trt == "Placebo") {
                reason_probs <- c(0.10, 0.25, 0.25, 0.20, 0.20)
            } else {
                reason_probs <- c(0.25, 0.20, 0.20, 0.20, 0.15)
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
    ## ASSEMBLE
    ## ==========================================================================

    rctselect6 <- data.frame(
        patient_id             = sprintf("DOS%04d", 1:n),
        site                   = site,
        screening_date         = screening_date,
        age                    = age,
        has_consent            = has_consent,
        prior_therapy          = prior_therapy,
        ecog                   = ecog,
        creatinine             = creatinine,
        eligible               = eligible,
        exclusion_reason       = exclusion_reason,
        is_duplicate           = is_duplicate,
        treatment              = treatment,
        received_treatment     = received_treatment,
        discontinued           = discontinued,
        discontinuation_reason = discontinuation_reason,
        completed_study        = completed_study,
        stringsAsFactors       = FALSE
    )

    return(rctselect6)
}

rctselect6 <- create_rctselect6()

usethis::use_data(rctselect6, overwrite = TRUE)
