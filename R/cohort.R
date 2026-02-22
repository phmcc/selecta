#' Extract the Final Cohort
#'
#' Returns the dataset remaining after all exclusion criteria have been
#' applied. If arms have been defined, returns either a single combined
#' \code{data.table} or a named list of per-arm \code{data.table}s.
#'
#' This enables a seamless analysis workflow — define your enrollment flow,
#' render it, and immediately pass the resulting cohort into your analysis:
#'
#' \preformatted{
#' flow <- enroll(trial_data, id = "patient_id") |>
#'   exclude("Under 18", expr = age < 18) |>
#'   exclude("No consent", expr = is.na(consent_date)) |>
#'   allocate("treatment") |>
#'   exclude("Lost to follow-up", expr = ltfu == 1) |>
#'   endpoint("Final Analysis")
#'
#' flowchart(flow)
#' cohort(flow)                    # combined data.table
#' cohort(flow, split = TRUE)     # list by arm
#' cohort(flow, arm = "Drug A")   # single arm
#' }
#'
#' @param .flow A \code{selecta} object.
#' @param split Logical. If \code{TRUE} and arms are defined, return a named
#'   list of \code{data.table}s (one per arm). Default \code{FALSE} returns a
#'   single combined \code{data.table}.
#' @param arm Character. Name of a specific arm to extract. If supplied,
#'   returns only that arm's data.
#'
#' @return A \code{data.table} (or named list of \code{data.table}s when
#'   \code{split = TRUE}).
#'
#' @examples
#' \dontrun{
#' flow <- enroll(trial_data, id = "patient_id") |>
#'   exclude("Under 18", expr = age < 18) |>
#'   allocate("treatment") |>
#'   endpoint("Final Analysis")
#'
#' final <- cohort(flow)
#' by_arm <- cohort(flow, split = TRUE)
#' drug_a <- cohort(flow, arm = "Drug A")
#' }
#'
#' @export
cohort <- function(.flow, split = FALSE, arm = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)
  if (.flow$mode != "data")
    stop("cohort() requires a data-driven flow (supply 'data' to enroll())",
         call. = FALSE)

  snap  <- compute_snapshots(.flow)
  final <- snap$final

  has_arms <- !".all" %chin% names(final)

  if (!is.null(arm)) {
    if (!has_arms)
      stop("No arms defined in this flow", call. = FALSE)
    if (!arm %chin% names(final))
      stop(sprintf("Arm '%s' not found. Available: %s",
                   arm, paste(names(final), collapse = ", ")), call. = FALSE)
    return(copy(final[[arm]]))
  }

  if (has_arms && split)
    return(lapply(final, copy))

  if (has_arms)
    return(rbindlist(final))

  copy(final$.all)
}


#' Extract Cohorts at Every Stage
#'
#' Returns a named list of datasets at each step of the enrollment flow. This
#' enables cross-cohort comparisons — e.g., examining characteristics of
#' participants excluded by a specific exclusion.
#'
#' Each element of the returned list is itself a list with:
#' \describe{
#'   \item{\code{remaining}}{A \code{data.table} of participants still in the
#'     flow after this step.}
#'   \item{\code{excluded}}{A \code{data.table} of participants removed at
#'     this step (for exclusion steps; \code{NULL} otherwise).}
#'   \item{\code{n_remaining}}{Integer count of remaining participants.}
#'   \item{\code{n_excluded}}{Integer count of excluded participants (or
#'     \code{NA}).}
#' }
#'
#' @param .flow A \code{selecta} object.
#'
#' @return A named list of cohort snapshots keyed by step label.
#'
#' @examples
#' \dontrun{
#' flow <- enroll(trial_data, id = "patient_id") |>
#'   exclude("Under 18", expr = age < 18) |>
#'   exclude("No consent", expr = is.na(consent_date)) |>
#'   endpoint("Final Analysis")
#'
#' stages <- cohorts(flow)
#'
#' ## Who was excluded for being under 18?
#' stages[["Under 18"]]$excluded
#'
#' ## Dataset after first exclusion but before second
#' stages[["Under 18"]]$remaining
#' }
#'
#' @export
cohorts <- function(.flow) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)
  if (.flow$mode != "data")
    stop("cohorts() requires a data-driven flow (supply 'data' to enroll())",
         call. = FALSE)

  snap <- compute_snapshots(.flow)
  snap$stages
}


## ---- Internal snapshot engine ----

#' Compute Snapshots at Each Stage
#'
#' Walks the step list and captures the dataset state at each step, including
#' both retained and excluded participants.
#'
#' @param x A \code{selecta} object.
#' @return A list with \code{final} and \code{stages}.
#' @keywords internal
compute_snapshots <- function(x) {

  stages <- list()

  current_data <- list(.all = copy(x$data))
  in_arms    <- FALSE
  arm_labels <- NULL
  n_arms     <- 0L

  ## Starting snapshot
  stages[["_start"]] <- list(
    remaining   = current_data$.all,  # copy deferred to caller if needed
    excluded    = NULL,
    n_remaining = .row_count(current_data$.all),
    n_excluded  = NA_integer_
  )

  for (step in x$steps) {

    if (step$type == "exclude") {

      if (!in_arms) {
        mask <- eval(step$expr_call, envir = current_data$.all,
                     enclos = parent.frame(2L))
        mask[is.na(mask)] <- FALSE

        ## which()-based subsetting avoids double logical scan
        idx_excl <- which(mask)
        idx_keep <- which(!mask)
        excluded  <- current_data$.all[idx_excl]
        remaining <- current_data$.all[idx_keep]
        current_data$.all <- remaining

        stages[[step$label]] <- list(
          remaining   = copy(remaining),
          excluded    = copy(excluded),
          n_remaining = length(idx_keep),
          n_excluded  = length(idx_excl)
        )

      } else {
        ## Per-arm exclusions via lapply
        arm_results <- lapply(arm_labels, function(aname) {
          dt   <- current_data[[aname]]
          mask <- eval(step$expr_call, envir = dt, enclos = parent.frame(3L))
          mask[is.na(mask)] <- FALSE
          idx_excl <- which(mask)
          idx_keep <- which(!mask)
          list(excluded  = dt[idx_excl],
               remaining = dt[idx_keep],
               n_excluded  = length(idx_excl),
               n_remaining = length(idx_keep))
        })
        names(arm_results) <- arm_labels

        ## Update current data
        for (aname in arm_labels)
          current_data[[aname]] <- arm_results[[aname]]$remaining

        stages[[step$label]] <- list(
          remaining   = lapply(arm_results, function(r) copy(r$remaining)),
          excluded    = lapply(arm_results, function(r) copy(r$excluded)),
          n_remaining = vapply(arm_results, `[[`, integer(1L), "n_remaining"),
          n_excluded  = vapply(arm_results, `[[`, integer(1L), "n_excluded")
        )
      }

    } else if (step$type == "allocate") {
      in_arms <- TRUE

      ## Reuse the shared split helper
      split_result <- split_by_var(current_data$.all, step$variable,
                                   step$labels)
      arm_labels   <- split_result$labels
      n_arms       <- length(arm_labels)
      current_data <- split_result$data

      stages[["_arm"]] <- list(
        remaining   = lapply(current_data, copy),
        excluded    = NULL,
        n_remaining = vapply(current_data, .row_count, integer(1L)),
        n_excluded  = NA_integer_
      )

    } else if (step$type == "endpoint") {
      if (!in_arms) {
        stages[[step$label]] <- list(
          remaining   = copy(current_data$.all),
          excluded    = NULL,
          n_remaining = .row_count(current_data$.all),
          n_excluded  = NA_integer_
        )
      } else {
        stages[[step$label]] <- list(
          remaining   = lapply(current_data, copy),
          excluded    = NULL,
          n_remaining = vapply(current_data, .row_count, integer(1L)),
          n_excluded  = NA_integer_
        )
      }
    }
  }

  list(final = current_data, stages = stages)
}
