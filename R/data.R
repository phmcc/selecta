#' Simulated Observational Cohort (No Arms)
#'
#' A synthetic dataset of 3,000 patients in an observational study with
#' no treatment arms. Includes eligibility flags, exclusion reasons,
#' and follow-up loss indicators suitable for demonstrating STROBE-style
#' enrollment diagrams in data mode.
#'
#' @format A \code{data.table} with 3,000 rows and the following columns:
#' \describe{
#'   \item{patient_id}{Unique patient identifier.}
#'   \item{is_duplicate}{Logical. Whether the record is a duplicate.}
#'   \item{eligible}{Logical. Whether the patient meets eligibility criteria.}
#'   \item{exclusion_reason}{Character. Reason for exclusion, if applicable.}
#'   \item{lost_to_followup}{Logical. Whether the patient was lost to follow-up.}
#'   \item{followup_loss_reason}{Character. Reason for follow-up loss, if applicable.}
#' }
#'
#' @examples
#' data(selectaex0)
#' str(selectaex0)
"selectaex0"


#' Simulated Two-Arm Randomized Trial
#'
#' A synthetic dataset of 2,400 patients in a two-arm randomized
#' controlled trial. Includes screening, eligibility, treatment
#' assignment, and discontinuation variables suitable for demonstrating
#' CONSORT-style enrollment diagrams in data mode.
#'
#' @format A \code{data.table} with 2,400 rows and the following columns:
#' \describe{
#'   \item{patient_id}{Unique patient identifier.}
#'   \item{is_duplicate}{Logical. Whether the record is a duplicate.}
#'   \item{eligible}{Logical. Whether the patient meets eligibility criteria.}
#'   \item{exclusion_reason}{Character. Reason for exclusion, if applicable.}
#'   \item{treatment}{Character. Treatment arm assignment (\emph{e.g.,} \code{"Drug A"}, \code{"Placebo"}).}
#'   \item{discontinued}{Logical. Whether the patient discontinued the study.}
#'   \item{discontinuation_reason}{Character. Reason for discontinuation, if applicable.}
#' }
#'
#' @examples
#' data(selectaex2)
#' str(selectaex2)
#' table(selectaex2$treatment)
"selectaex2"


#' Simulated Three-Arm Randomized Trial
#'
#' A synthetic dataset of 2,400 patients in a three-arm randomized
#' controlled trial. Structure matches \code{\link{selectaex2}} with an
#' additional treatment arm.
#'
#' @format A \code{data.table} with 2,400 rows. See \code{\link{selectaex2}}
#'   for column descriptions.
#'
#' @examples
#' data(selectaex3)
#' str(selectaex3)
#' table(selectaex3$treatment)
"selectaex3"


#' Simulated Six-Arm Dose-Finding Trial
#'
#' A synthetic dataset of 3,600 patients in a six-arm dose-finding
#' trial. Structure matches \code{\link{selectaex2}} with six treatment
#' arms.
#'
#' @format A \code{data.table} with 3,600 rows. See \code{\link{selectaex2}}
#'   for column descriptions.
#'
#' @examples
#' data(selectaex6)
#' str(selectaex6)
#' table(selectaex6$treatment)
"selectaex6"
