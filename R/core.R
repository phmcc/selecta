#' Initialize an Enrollment Flow
#'
#' Entry point for building a CONSORT-style enrollment diagram. Supply either
#' a \code{data.frame} (for data-driven mode where counts are computed
#' automatically) or a starting count \code{n} (for manual mode).
#'
#' @param data A \code{data.frame} or \code{data.table}. Each row represents
#'   one participant. If supplied, expressions in \code{exclude()} are
#'   evaluated against this data to compute counts automatically.
#' @param id Character string naming the participant ID column in \code{data}.
#'   Defaults to the first column.
#' @param n Integer. Starting population count for manual mode. Ignored when
#'   \code{data} is supplied.
#' @param label Character string for the top-level box. Defaults to
#'   \code{"Assessed for eligibility"}.
#'
#' @return An object of class \code{"selecta"}.
#'
#' @examples
#' # Manual mode
#' enroll(n = 500, label = "Records identified")
#'
#' # Data-driven
#' \dontrun{
#' enroll(trial_data, id = "patient_id")
#' }
#'
#' @export
enroll <- function(data = NULL, id = NULL, n = NULL,
                   label = "Study Population") {

  mode <- if (!is.null(data)) "data" else "manual"

  if (mode == "data") {
    if (!is.data.frame(data))
      stop("'data' must be a data.frame or data.table", call. = FALSE)
    if (!is.data.table(data)) data <- as.data.table(data)
    if (is.null(id)) id <- names(data)[1L]
    if (!id %chin% names(data))
      stop(sprintf("Column '%s' not found in data", id), call. = FALSE)
    starting_n <- nrow(data)
  } else {
    if (is.null(n) || !is.numeric(n) || length(n) != 1L || n < 0)
      stop("Supply a non-negative integer 'n' for manual mode", call. = FALSE)
    n <- as.integer(n)
    starting_n <- n
    data <- NULL
    id <- NULL
  }

  structure(
    list(
      data    = data,
      id      = id,
      mode    = mode,
      steps   = list(),
      label   = label,
      n_start = starting_n
    ),
    class = "selecta"
  )
}


#' Exclude Participants by a Criterion
#'
#' Appends an exclusion step to the enrollment flow. Participants matching the
#' criterion are removed and shown in a side box. Optionally, itemized
#' sub-reasons can be displayed below the total.
#'
#' @param .flow A \code{selecta} object (piped from \code{enroll()} or a
#'   previous step).
#' @param label Character string. Human-readable description for the side box
#'   (e.g., \code{"Excluded"} or \code{"Lost to follow-up"}).
#' @param expr An unquoted expression evaluated against the data. Should
#'   evaluate to \code{TRUE} for rows to be \strong{removed}. (Data-driven
#'   mode only.)
#' @param n Integer (manual mode). Number of participants removed at this step.
#'   After an \code{allocate()} step, supply a vector with one value per arm.
#' @param reasons Exclusion sub-reasons. Accepts three forms:
#'   \itemize{
#'     \item A \strong{named integer vector} (manual mode): counts per reason,
#'       e.g. \code{c("Disease progression" = 12, "Declined" = 8)}.
#'     \item A \strong{character string} (data-driven mode): column name whose
#'       values are tabulated automatically.
#'     \item A \strong{list} of named vectors (manual mode after \code{allocate()}):
#'       one vector per arm.
#'   }
#' @param show_zero Logical. If \code{FALSE} (default), sub-reasons with a
#'   count of zero are hidden. Set to \code{TRUE} to display all pre-specified
#'   reason categories, including those with zero participants.
#'
#' @return The updated \code{selecta} object.
#'
#' @examples
#' # Simple exclusion
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65)
#'
#' # With sub-reasons (manual)
#' enroll(n = 500) |>
#'   exclude("Excluded", n = 65,
#'     reasons = c("Progressive disease" = 22,
#'                 "Unacceptable comorbidities" = 18,
#'                 "Declined surgery" = 15,
#'                 "Lost to follow-up" = 10))
#'
#' @export
exclude <- function(.flow, label, expr, n = NULL, reasons = NULL,
                    show_zero = FALSE, remaining_label = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  expr_call <- substitute(expr)
  has_expr <- !missing(expr)

  if (.flow$mode == "data" && !has_expr)
    stop("Supply 'expr' in data-driven mode", call. = FALSE)
  if (.flow$mode == "manual" && is.null(n))
    stop("Supply 'n' in manual mode", call. = FALSE)

  ## Dispatch: character scalar → column name; named numeric → manual counts
  reasons_var <- NULL
  reasons_manual <- NULL

  if (!is.null(reasons)) {
    if (is.character(reasons) && length(reasons) == 1L) {
      if (.flow$mode != "data")
        stop("Column-name 'reasons' only works in data-driven mode", call. = FALSE)
      reasons_var <- reasons
    } else {
      if (!is.list(reasons) && is.null(names(reasons)))
        stop("'reasons' must be a named vector, a column name, or a list",
             call. = FALSE)
      reasons_manual <- reasons
    }
  }

  step <- list(
    type            = "exclude",
    label           = label,
    expr_call       = if (has_expr) expr_call else NULL,
    n               = n,
    reasons         = reasons_manual,
    reasons_var     = reasons_var,
    show_zero       = show_zero,
    remaining_label = remaining_label
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


#' Label a Phase of the Enrollment Flow
#'
#' Adds a vertical phase label to the left margin of the diagram (e.g.,
#' \code{"Enrollment"}, \code{"Allocation"}, \code{"Follow-up"},
#' \code{"Analysis"}). Phase labels span all subsequent steps until the next
#' \code{phase()} call or the end of the flow.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string. The phase label, rendered as vertical text
#'   on the left margin.
#'
#' @return The updated \code{selecta} object.
#'
#' @examples
#' enroll(n = 1200, label = "Records identified") |>
#'   phase("Enrollment") |>
#'   exclude("Duplicates", n = 84) |>
#'   exclude("Age criteria", n = 63) |>
#'   phase("Allocation") |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(520, 533)) |>
#'   phase("Follow-up") |>
#'   exclude("Lost to follow-up", n = c(23, 31)) |>
#'   phase("Analysis") |>
#'   endpoint("Final Analysis") |>
#'   flowchart()
#'
#' @export
phase <- function(.flow, label) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  step <- list(
    type  = "phase",
    label = label
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


#' Allocate into Study Arms
#'
#' Splits the enrollment flow into parallel arms (e.g., treatment groups).
#' Subsequent \code{exclude()} calls apply within each arm independently.
#'
#' @param .flow A \code{selecta} object.
#' @param variable Character string naming the column that defines the arms
#'   (data-driven mode). Ignored in manual mode.
#' @param labels A character vector of arm labels. In data-driven mode, this
#'   can be a named vector to relabel factor levels (e.g.,
#'   \code{c(A = "Drug A", B = "Placebo")}). In manual mode, these are the
#'   arm names.
#' @param n Integer vector (manual mode). Number of participants allocated to
#'   each arm, in the same order as \code{labels}.
#' @param label Character string for the allocation box. Defaults to
#'   \code{"Randomized"}.
#'
#' @return The updated \code{selecta} object.
#'
#' @examples
#' enroll(n = 400) |>
#'   exclude("Ineligible", n = 50) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(175, 175))
#'
#' @export
allocate <- function(.flow, variable = NULL, labels = NULL, n = NULL,
                label = "Randomized") {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  has_alloc <- any(vapply(.flow$steps, function(s) s$type == "allocate", logical(1L)))
  if (has_alloc)
    stop("Only one allocate() split is currently supported", call. = FALSE)

  if (.flow$mode == "data" && is.null(variable))
    stop("Supply 'variable' in data-driven mode", call. = FALSE)
  if (.flow$mode == "manual" && (is.null(labels) || is.null(n)))
    stop("Supply 'labels' and 'n' in manual mode", call. = FALSE)
  if (.flow$mode == "manual" && length(labels) != length(n))
    stop("'labels' and 'n' must have the same length", call. = FALSE)

  step <- list(
    type     = "allocate",
    variable = variable,
    labels   = labels,
    n        = n,
    label    = label
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


#' Define the Endpoint
#'
#' Adds a terminal node to the enrollment flow.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string for the final box. Defaults to
#'   \code{"Final Analysis"}.
#'
#' @return The updated \code{selecta} object.
#'
#' @examples
#' enroll(n = 300) |>
#'   exclude("Excluded", n = 40) |>
#'   endpoint("Included in analysis")
#'
#' @export
endpoint <- function(.flow, label = "Final Analysis") {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  step <- list(
    type  = "endpoint",
    label = label
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}
