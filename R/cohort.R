### * Main functions

#' Extract the Final Cohort
#'
#' Returns the dataset remaining after all exclusion criteria have been
#' applied. When arms are defined via \code{stratify()}, the result
#' is either a single combined \code{data.table} or a named list of
#' per-arm \code{data.table} objects. Data mode only.
#'
#' @param .flow A \code{selecta} object created in data mode
#'   (\code{data} supplied to \code{enroll()}).
#' @param split Logical. If \code{TRUE} and arms are defined, return a named
#'   list of \code{data.table}s (one per arm). Default \code{FALSE} returns a
#'   single combined \code{data.table}.
#' @param arm Character. Name of a specific arm to extract. If supplied,
#'   returns only that arm's \code{data.table}.
#'
#' @return A \code{data.table} containing the participants remaining after
#'   all exclusion criteria. When \code{split = TRUE}, a named list of
#'   \code{data.table}s (one per arm). When \code{arm} is specified, a
#'   single-arm \code{data.table}.
#'
#' @details
#' \code{cohort()} replays the exclusion criteria of a \emph{data-mode} flow
#' against the original dataset and returns the rows that survive to the
#' end, so the analyst can pass the exact analyzed population to downstream
#' modeling. It requires a flow created by supplying \code{data} to
#' \code{enroll()}; manual-mode flows carry only counts and therefore
#' raise an error. For an unsplit flow the result is a single
#' \code{data.table}; after \code{stratify()} or \code{allocate()},
#' \code{split = TRUE} returns one table per arm and \code{arm} extracts a
#' single named arm. To inspect the cohort at every intermediate step rather
#' than only the end, use \code{cohorts()}.
#'
#' @seealso \code{\link{cohorts}} for stage-by-stage snapshots,
#'   \code{\link{enroll}} for initializing a data-mode flow
#'
#' @examples
#' flow <- enroll(selectaex2, id = "patient_id") |>
#'   exclude("Ineligible", criterion = eligible == FALSE) |>
#'   endpoint("Final")
#'
#' final <- cohort(flow)
#' nrow(final)
#'
#' @family cohort extraction functions
#' @export
cohort <- function(.flow, split = FALSE, arm = NULL) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)
    if (.flow$mode != "data")
        stop("cohort() requires a data-mode flow (supply 'data' to enroll())",
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
#' Returns a named list of datasets at each step of the enrollment flow,
#' enabling cross-cohort comparisons. Results are reported as a named list,
#' organized by step label. Data mode only.
#'
#' @param .flow A \code{selecta} object created in data mode
#'   (\code{data} supplied to \code{enroll()}).
#'
#' @return A named list of cohort snapshots, keyed by step label. Each
#'   snapshot is itself a list with:
#'   \describe{
#'     \item{\code{included}}{A \code{data.table} of participants still in
#'       the flow after this step.}
#'     \item{\code{excluded}}{A \code{data.table} of participants removed at
#'       this step (for exclusion steps; \code{NULL} otherwise).}
#'     \item{\code{n_included}}{Integer count of included participants.}
#'     \item{\code{n_excluded}}{Integer count of excluded participants (or
#'       \code{NA}).}
#'   }
#'
#' @details
#' \code{cohorts()} replays a \emph{data mode} flow and captures the dataset
#' at every step, returning a named list keyed by step label (with
#' \code{"_start"} for the initial cohort). Each snapshot exposes both the
#' \code{included} and the \code{excluded} rows together with their counts,
#' which is useful for validating a diagram against the data, auditing why
#' particular participants were dropped, or extracting an intermediate
#' population. After a \code{stratify()} or \code{allocate()}
#' split, the \code{included} and \code{excluded} elements of a per-arm
#' step are themselves named lists with one entry per arm; after a factorial
#' (two-level) split the entries are the cells, keyed
#' \code{"<parent>: <child>"}. A manual-mode flow has no underlying data and
#' therefore raises an error. To obtain only the final analyzed population,
#' use \code{cohort()}.
#'
#' @seealso \code{\link{cohort}} for extracting only the final cohort
#'
#' @examples
#' flow <- enroll(selectaex2, id = "patient_id") |>
#'   exclude("Ineligible", criterion = eligible == FALSE) |>
#'   endpoint("Final")
#'
#' stages <- cohorts(flow)
#' names(stages)
#' stages[["Ineligible"]]$n_excluded
#'
#' @family cohort extraction functions
#' @export
cohorts <- function(.flow) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)
    if (.flow$mode != "data")
        stop("cohorts() requires a data-mode flow (supply 'data' to enroll())",
             call. = FALSE)

    snap <- compute_snapshots(.flow)
    snap$stages
}


### * Internal snapshot engine

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
    in_arms     <- FALSE
    arm_labels  <- NULL
    split_level <- 0L       # 0 trunk, 1 single split, 2 factorial
    cell_parent  <- NULL    # factorial: leaf label -> parent label

    ## Starting snapshot
    stages[["_start"]] <- list(
        included   = current_data$.all,  # copy deferred to caller if needed
        excluded    = NULL,
        n_included = .row_count(current_data$.all),
        n_excluded  = NA_integer_
    )

    for (step in x$steps) {

        if (step$type == "exclude") {

            if (!in_arms) {
                mask <- eval(step$expr_call, envir = current_data$.all,
                             enclos = parent.frame(2L))
                mask[is.na(mask)] <- FALSE

                idx_excl <- which(mask)
                idx_keep <- which(!mask)
                excluded  <- current_data$.all[idx_excl]
                included <- current_data$.all[idx_keep]
                current_data$.all <- included

                stages[[step$label]] <- list(
                    included   = copy(included),
                    excluded    = copy(excluded),
                    n_included = length(idx_keep),
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
                         included = dt[idx_keep],
                         n_excluded  = length(idx_excl),
                         n_included = length(idx_keep))
                })
                names(arm_results) <- arm_labels

                ## Update current data
                for (aname in arm_labels)
                    current_data[[aname]] <- arm_results[[aname]]$included

                stages[[step$label]] <- list(
                    included   = lapply(arm_results, function(r) copy(r$included)),
                    excluded    = lapply(arm_results, function(r) copy(r$excluded)),
                    n_included = vapply(arm_results, `[[`, integer(1L), "n_included"),
                    n_excluded  = vapply(arm_results, `[[`, integer(1L), "n_excluded")
                )
            }

        } else if (step$type == "stratify") {
            if (!in_arms) {
                ## Level 1: split the trunk by the variable.
                split_result <- split_by_var(current_data$.all, step$variable,
                                             step$labels)
                arm_labels   <- split_result$labels
                current_data <- split_result$data
                in_arms      <- TRUE
                split_level  <- 1L
                stages[["_arm"]] <- list(
                    included   = lapply(current_data, copy),
                    excluded    = NULL,
                    n_included = vapply(current_data, .row_count, integer(1L)),
                    n_excluded  = NA_integer_
                )
            } else {
                ## Level 2 (factorial): split each arm by the second variable. Sub-arm
                ## cells are keyed "<parent>: <child>" (labels repeat across parents) and
                ## share one rectangular level set from the entering cohort.
                var      <- step$variable
                entering <- rbindlist(current_data[arm_labels])
                ecol     <- entering[[var]]
                sub_keys <- if (is.factor(ecol)) levels(ecol) else sort(unique(ecol))
                new_data   <- list()
                new_labels <- character(0L)
                new_parent <- character(0L)
                for (pl in arm_labels) {
                    sp <- split_by_var(current_data[[pl]], var, step$labels, keys = sub_keys)
                    for (j in seq_along(sp$labels)) {
                        key <- paste0(pl, ": ", sp$labels[j])
                        new_data[[key]] <- sp$data[[j]]
                        new_labels      <- c(new_labels, key)
                        new_parent      <- c(new_parent, pl)
                    }
                }
                names(new_parent) <- new_labels
                current_data <- new_data
                arm_labels   <- new_labels
                cell_parent   <- new_parent
                split_level  <- 2L
                stages[["_arm2"]] <- list(
                    included   = lapply(current_data, copy),
                    excluded    = NULL,
                    n_included = vapply(current_data, .row_count, integer(1L)),
                    n_excluded  = NA_integer_
                )
            }

        } else if (step$type == "combine") {
            if (split_level == 2L) {
                ## Factorial peel: recombine each parent's sub-arm cells, restoring the
                ## first-level arms (one split level remains active).
                parent_labels <- unique(cell_parent)
                current_data  <- setNames(lapply(parent_labels, function(pl)
                    rbindlist(current_data[names(cell_parent)[cell_parent == pl]])),
                    parent_labels)
                arm_labels  <- parent_labels
                cell_parent  <- NULL
                split_level <- 1L
                stages[[step$label]] <- list(
                    included   = lapply(current_data, copy),
                    excluded    = NULL,
                    n_included = vapply(current_data, .row_count, integer(1L)),
                    n_excluded  = NA_integer_
                )
            } else {
                ## Full recombine of first-level arms back to a single stream.
                if (in_arms) {
                    current_data <- list(.all = rbindlist(current_data[arm_labels]))
                    in_arms      <- FALSE
                    arm_labels   <- NULL
                    split_level  <- 0L
                }
                stages[[step$label]] <- list(
                    included   = copy(current_data$.all),
                    excluded    = NULL,
                    n_included = .row_count(current_data$.all),
                    n_excluded  = NA_integer_
                )
            }

        } else if (step$type == "endpoint") {
            if (!in_arms) {
                stages[[step$label]] <- list(
                    included   = copy(current_data$.all),
                    excluded    = NULL,
                    n_included = .row_count(current_data$.all),
                    n_excluded  = NA_integer_
                )
            } else {
                stages[[step$label]] <- list(
                    included   = lapply(current_data, copy),
                    excluded    = NULL,
                    n_included = vapply(current_data, .row_count, integer(1L)),
                    n_excluded  = NA_integer_
                )
            }
        }
    }

    list(final = current_data, stages = stages)
}
