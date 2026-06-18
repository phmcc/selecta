### * Initialization

#' Initialize an Enrollment Flow
#'
#' Entry point for building an EQUATOR-style enrollment diagram from a single
#' starting population. Accepts either a \code{data.frame} (data mode,
#' where counts are computed automatically from exclusion expressions) or a
#' starting count \code{n} (manual mode, where counts are supplied explicitly
#' at each step).
#'
#' @param data A \code{data.frame} or \code{data.table} in which each row
#'   represents one participant. When supplied, exclusion expressions passed
#'   to \code{exclude()} are evaluated against this data to compute
#'   counts automatically. If \code{NULL} (default), the flow operates in
#'   manual mode.
#' @param id Character string naming the participant ID column in \code{data}.
#'   Defaults to the first column. Ignored in manual mode.
#' @param n Integer. Starting population count for manual mode. Must be a
#'   non-negative scalar. Ignored when \code{data} is supplied.
#' @param label Character string for the top-level box in the diagram.
#'   Default is \code{"Study Population"}.
#'
#' @return An object of class \code{"selecta"} containing the data (if
#'   supplied), mode, starting count, label, and an empty step list.
#'   Subsequent pipeline functions (\code{exclude()}, \code{stratify()},
#'   \code{endpoint()}, \emph{etc.}) append steps to this object.
#'
#' @details
#' \code{enroll()} begins every single-source pipeline and fixes the
#' operating mode for all subsequent steps. Supplying \code{data} (with
#' \code{id}) selects \emph{data mode}, in which later \code{exclude()} and
#' \code{stratify()} steps filter and partition the dataset and counts are
#' derived from the data. Alternatively, supplying \code{n} instead selects
#' \emph{manual mode}, in which counts are taken from the numbers given at
#' each step. The two modes are mutually exclusive, and the resulting object
#' is intended to be extended with the pipe operator. For diagrams with
#' several entry sources that converge (PRISMA, MOOSE), use \code{sources()}
#' instead of \code{enroll()}.
#'
#' @seealso \code{\link{sources}} for multi-source entry,
#'   \code{\link{exclude}} for adding exclusion criteria,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' # Manual mode
#' enroll(n = 500, label = "Assessed for eligibility")
#'
#' # Data mode
#' enroll(selectaex2, id = "patient_id", label = "Study Population")
#'
#' # Minimal CONSORT pipeline
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Treatment", "Control"), n = c(218, 217)) |>
#'   endpoint("Analyzed")
#'
#' @family flow construction functions
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


#' Initialize a Multi-Source Flow
#'
#' Entry point for flows that begin with multiple parallel identification
#' streams, such as systematic review diagrams. Each named argument defines
#' a source \emph{group} (column). Individual databases or registers within
#' each group are listed as sub-items inside a single box, mirroring the
#' format of exclusion reasons.
#'
#' @param ... Named integer vectors specifying sources. Each argument
#'   name identifies a group and its named elements are individual sources
#'   (\emph{e.g.,} \code{databases = c("PubMed" = 1234, "Embase" = 567)}).
#'   Scalar named arguments are treated as individual sources in a single
#'   default group.
#' @param headers Named character vector mapping group names to column
#'   header labels. For example,
#'   \code{headers = c(databases = "Databases and registers",
#'                     other = "Other methods")}. If omitted, the
#'   argument names are title-cased and used as headers.
#'
#' @return An object of class \code{"selecta"} with a \code{sources} step
#'   pre-loaded. The total starting count is the sum of all source counts
#'   across all groups.
#'
#' @details
#'
#' \code{sources()} initializes a multi-source flow of the kind used in the
#' identification stage of systematic-review diagrams (PRISMA, MOOSE), where
#' records arrive from several origins and are pooled. Counts are supplied
#' as named numeric values; passing named vectors instead of scalars groups
#' the sources into labeled columns, and at most three groups are
#' supported, matching the standard PRISMA layout. A \code{sources()} flow
#' is operated in manual mode and is normally followed by \code{combine()}
#' to merge the streams into a single downstream node. For a conventional
#' single-entry study, use \code{enroll()} instead.
#'
#' @seealso \code{\link{enroll}} for single-source entry,
#'   \code{\link{combine}} to merge parallel streams into a single flow
#'
#' @examples
#' # Simple multi-source (one column, no header)
#' sources(PubMed = 1234, Embase = 567, CENTRAL = 89)
#'
#' # Grouped sources (PRISMA two-column layout)
#' sources(
#'   databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
#'   other     = c("Citation search" = 55, "Websites" = 34)
#' )
#'
#' # Three columns with custom headers
#' sources(
#'   previous  = c("Previous review" = 12, "Previous reports" = 15),
#'   databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
#'   other     = c("Citation search" = 55, "Websites" = 34),
#'   headers   = c(previous  = "Previous studies",
#'                 databases = "Databases and registers",
#'                 other     = "Other methods")
#' ) |>
#'   combine("Records after deduplication") |>
#'   exclude("Records removed", n = 352, show_count = FALSE,
#'           reasons = c("Duplicates" = 340,
#'                       "Marked ineligible" = 12))
#'
#' @family flow construction functions
#' @export
sources <- function(..., headers = NULL) {

    args <- list(...)

    if (length(args) == 0L)
        stop("Supply at least one named source", call. = FALSE)
    if (length(args) > 3L)
        stop("At most 3 source groups are supported", call. = FALSE)

    ## Detect grouped vs flat argument pattern
    is_grouped <- any(vapply(args, function(a) {
        is.numeric(a) && (length(a) > 1L || !is.null(names(a)))
    }, logical(1L)))

    if (is_grouped) {
        ## Grouped mode: each argument is a group
        groups <- lapply(seq_along(args), function(i) {
            a <- args[[i]]
            group_label <- names(args)[i]
            if (is.null(group_label) || group_label == "")
                stop("All arguments to sources() must be named when using grouped sources",
                     call. = FALSE)
            if (is.null(names(a)))
                stop(sprintf("Group '%s' must be a named numeric vector", group_label),
                     call. = FALSE)
            if (!is.numeric(a) || any(a < 0))
                stop(sprintf("Group '%s' must have non-negative numeric counts",
                             group_label), call. = FALSE)
            hdr <- if (!is.null(headers) && group_label %in% names(headers)) {
                       headers[[group_label]]
                   } else if (group_label != "_default") {
                       ## Default header: title-case the group name
                       gsub("(^|\\s)(\\w)", "\\1\\U\\2", group_label, perl = TRUE)
                   } else {
                       NULL
                   }
            list(group = group_label, header = hdr,
                 labels = names(a), counts = as.integer(a))
        })
    } else {
        ## Flat mode: all sources in one group
        if (is.null(names(args)) || any(names(args) == ""))
            stop("All arguments to sources() must be named", call. = FALSE)
        flat_vals <- unlist(args)
        if (!is.numeric(flat_vals) || any(flat_vals < 0))
            stop("All source counts must be non-negative", call. = FALSE)
        groups <- list(list(
            group  = "_default",
            header = NULL,
            labels = names(args),
            counts = as.integer(flat_vals)
        ))
    }

    total_n <- sum(vapply(groups, function(g) sum(g$counts), integer(1L)))

    obj <- structure(
        list(
            data    = NULL,
            id      = NULL,
            mode    = "manual",
            steps   = list(),
            label   = NULL,
            n_start = total_n
        ),
        class = "selecta"
    )

    step <- list(
        type   = "sources",
        groups = groups
    )

    obj$steps <- list(step)
    obj
}


### * Exclusion

#' Exclude Participants by Criteria
#'
#' Appends an exclusion step to the enrollment flow. Participants matching the
#' criteria are removed and shown in a side box. Optionally, itemized
#' sub-reasons can be displayed below the total.
#'
#' @param .flow A \code{selecta} object (piped from \code{enroll()} or a
#'   previous step).
#' @param label Character. Human-readable description for the side box
#'   (\emph{e.g.,} \code{"Excluded"} or \code{"Lost to follow-up"}).
#'   After \code{stratify()}, may be a character vector with one label
#'   per arm (\emph{e.g.,} \code{c("Treatment discontinued", "Initiated
#'   treatment")}).
#' @param criterion An unquoted logical expression evaluated against the
#'   data. Should evaluate to \code{TRUE} for rows to be removed.
#'   Compound conditions are supported using the vectorized operators
#'   \code{&} (and), \code{|} (or), and \code{!} (not). Do not use the
#'   scalar short-circuit operators \code{&&} or \code{||}, which evaluate
#'   only the first element of each vector. Data mode only.
#' @param n Integer. Number of participants removed at this step.  After
#'   a \code{stratify()} step, supply a vector with one value per arm.
#'   Manual mode only.
#' @param reasons Exclusion sub-reasons. Accepts these forms:
#'   \itemize{
#'     \item A \emph{character string} (data mode): the name of a column
#'       whose values are tabulated automatically into a flat breakdown.
#'     \item A \emph{length-2 character vector} (data mode): the names of a
#'       reason column and a sub-reason column, cross-tabulated automatically
#'       into a two-level breakdown---parents ordered by total, sub-reasons by
#'       count.
#'     \item A \emph{named numeric vector} (manual mode): counts per reason,
#'       \emph{e.g.,} \code{c("Disease progression" = 12, "Declined" = 8)}. An
#'       entry may itself be a named numeric vector, giving a two-level
#'       breakdown (a reason and its sub-reasons).
#'     \item A \emph{list} of any of the above (data or manual mode after
#'       \code{stratify()}): one entry per arm.
#'   }
#' @param show_zero Logical. If \code{FALSE} (default), sub-reasons with a
#'   count of zero are hidden. Set to \code{TRUE} to display all pre-specified
#'   reason categories, including those with zero participants.
#' @param show_count Logical. If \code{FALSE} (default), the intermediate
#'   count box is suppressed---the count still updates internally but no box
#'   is rendered. Set to \code{TRUE} to force a count box. Overridden by
#'   \code{included_label}: providing any \code{included_label} always
#'   creates a count box regardless of \code{show_count}. Also automatically
#'   suppressed when the next step is \code{stratify()}, \code{endpoint()},
#'   or \code{allocate()}.
#' @param included_label Character string (or vector). Optional text for the
#'   box showing the count remaining after exclusion. When provided, a
#'   count box is always rendered regardless of \code{show_count}. After
#'   \code{stratify()}, may be a character vector with one label per arm.
#' @param collapse_singletons Logical. When \code{TRUE}, a parent reason that
#'   resolves to a single sub-reason is collapsed to a plain leaf carrying the
#'   parent's label and count (dropping the lone, redundant sub-line). Applies
#'   to two-level reasons from either a manual nested specification or a
#'   two-column data-mode cross-tabulation. Default \code{FALSE} keeps every
#'   parent expanded, for full transparency.
#'
#' @return The updated \code{selecta} object with an exclusion step appended.
#'
#' @details
#' \code{exclude()} records participants removed at a step and is the most
#' common pipeline verb. In data mode, \code{criterion} is an unquoted logical
#' expression evaluated against the dataset (rows for which it is
#' \code{TRUE} are removed) and \code{reasons} may name one column (a flat
#' breakdown) or two columns (a reason and a sub-reason, cross-tabulated into a
#' two-level breakdown); in manual mode, \code{n} gives
#' the number removed and \code{reasons} may be a named numeric vector.
#' After a \code{stratify()} or \code{allocate()} split the
#' exclusion applies per arm, in which case \code{n}, \code{reasons}, and
#' \code{included_label} accept per-arm vectors or lists. By default the
#' running count box is suppressed between consecutive exclusions for a
#' compact diagram; supplying \code{included_label} (or
#' \code{show_count = TRUE}) forces a count box to be drawn.
#'
#' When \code{getOption("selecta.check_arithmetic")} is \code{TRUE}, the
#' manual counts of the whole flow are audited together before export: an
#' over-exclusion, a split or combine whose parts do not match the running
#' total, and sub-reasons that do not sum to their exclusion total each
#' raise an advisory warning without altering the figures. The audit runs
#' whenever the flow is computed; this includes calls to \code{flowchart()},
#' \code{flowsave()}, and \code{summary()}, so a single call to any of
#' these functions reports every discrepancy at once.
#'
#' Eligibility that is more naturally framed as inclusion fits this same
#' model: express it as the exclusion of those who fail the criteria, and
#' use \code{included_label} to label the retained count (\emph{e.g.,}
#' \code{included_label = "Eligible cohort"}).
#'
#' After a \code{stratify()} step, both \code{label} and
#' \code{included_label} accept character vectors (one element per arm)
#' for per-arm labeling---useful in observational designs where
#' attrition mechanisms differ across strata.
#'
#' @seealso \code{\link{assess}} for assessment/procedure steps (STARD),
#'   \code{\link{enroll}} for initializing a flow
#'
#' @examples
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65)
#'
#' # With sub-reasons (manual)
#' enroll(n = 500) |>
#'   exclude("Excluded", n = 65,
#'     reasons = c("Did not meet criteria" = 22,
#'                 "Ineligible comorbidities" = 18,
#'                 "Declined to participate" = 15,
#'                 "Lost to follow-up" = 10))
#'
#' # Show intermediate count box (opt-in)
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65, show_count = TRUE) |>
#'   exclude("Declined", n = 20) |>
#'   endpoint("Final")
#'
#' # Or use included_label (always shows count box)
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65,
#'           included_label = "Eligible") |>
#'   endpoint("Final")
#'
#' # Per-arm labels (observational)
#' enroll(n = 1000) |>
#'   stratify(labels = c("Exposed", "Unexposed"), n = c(500, 500),
#'            label = "Classified by exposure") |>
#'   exclude(c("Treatment discontinued", "Initiated treatment"),
#'           n = c(45, 52))
#'
#' # Per-arm reasons (list of named vectors)
#' enroll(n = 900) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
#'   exclude("Discontinued", n = c(30, 25),
#'           reasons = list(
#'               c("Adverse event" = 18, "Withdrew consent" = 12),
#'               c("Adverse event" = 10, "Lost to follow-up" = 15)
#'           )) |>
#'   endpoint("Analyzed")
#'
#' # Compound expression (data mode)
#' data(selectaex2)
#' enroll(selectaex2, id = "patient_id") |>
#'   exclude("Ineligible or duplicate",
#'           criterion = eligible == FALSE | is_duplicate == TRUE)
#'
#' @family flow construction functions
#' @export
exclude <- function(.flow, label, criterion, n = NULL, reasons = NULL,
                    show_zero = FALSE, show_count = FALSE,
                    included_label = NULL, collapse_singletons = FALSE) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    expr_call <- substitute(criterion)
    has_expr <- !missing(criterion)

    if (.flow$mode == "data" && !has_expr)
        stop("Supply 'criterion' in data mode", call. = FALSE)
    if (.flow$mode == "manual" && is.null(n))
        stop("Supply 'n' in manual mode", call. = FALSE)

    ## Classify reasons argument
    reasons_var <- NULL
    reasons_manual <- NULL

    if (!is.null(reasons)) {
        if (is.character(reasons) && is.null(names(reasons))) {
            if (.flow$mode != "data")
                stop("Column-name 'reasons' only works in data mode", call. = FALSE)
            if (length(reasons) > 2L)
                stop("'reasons' may name at most two columns: a reason and, optionally, ",
                     "a sub-reason.", call. = FALSE)
            reasons_var <- reasons   # length 1 (flat) or 2 (reason + sub-reason)
        } else {
            if (!is.list(reasons) && is.null(names(reasons)))
                stop("'reasons' must be a named vector, a column name, or a list",
                     call. = FALSE)
            reasons_manual <- reasons
        }
    }

    step <- list(
        type           = "exclude",
        label          = label,
        expr_call      = if (has_expr) expr_call else NULL,
        n              = n,
        reasons        = reasons_manual,
        reasons_var    = reasons_var,
        show_zero      = show_zero,
        show_count     = show_count,
        included_label = included_label,
        collapse_singletons = isTRUE(collapse_singletons)
    )

    .flow$steps <- c(.flow$steps, list(step))
    .flow
}


### * Flow

#' Split into Parallel Study Arms or Strata
#'
#' Divides the enrollment flow into parallel arms. This is the primary
#' function for splitting a population by any characteristic: treatment
#' assignment, exposure status, diagnostic test result, etc. Subsequent
#' \code{exclude()} calls apply within each arm independently. While
#' \code{stratify()} is the primary function, \code{allocate()} is
#' provided as a convenience alias with default label \code{"Randomized"},
#' suitable for interventional trials (CONSORT).
#'
#' @param .flow A \code{selecta} object.
#' @param variable Character string naming the column that defines the arms.
#'   Data mode only.
#' @param n Integer vector. Number of participants in each arm, in the same
#' order as \code{labels}. Manual mode only.
#' @param labels A character vector of arm labels. In data mode, this
#'   can be a named vector to relabel factor levels (\emph{e.g.,}
#'   \code{c(A = "Drug A", B = "Placebo")}). In manual mode, these are the
#'   arm names.
#' @param label Character string for the split box. Defaults to
#'   \code{"Stratified"} for \code{stratify()} and \code{"Randomized"} for
#'   \code{allocate()}.
#'
#' @return The updated \code{selecta} object with a stratification step
#'   appended. All subsequent pipeline steps operate independently within
#'   each arm.
#'
#' @details
#' \code{stratify()} splits the flow into parallel arms, after which each
#' \code{exclude()} (and the eventual \code{endpoint()}) applies
#' within every arm. In data mode, \code{variable} names a column whose
#' levels define the arms, optionally relabeled through a named
#' \code{labels} vector; in manual mode, \code{labels} and \code{n} give the
#' arm names and per-arm counts directly.
#'
#' \code{allocate()} is an identical alias differing only in its default
#' \code{label} (\code{"Randomized"}), provided so that interventional
#' trials (CONSORT) read naturally; both record the same step type.
#'
#' Parallel arms may later be merged with \code{combine()} to form a
#' split-and-recombine diagram, and a flow may be split again after
#' combining. A second \code{stratify()} or \code{allocate()} before
#' combining produces a factorial (two-level) split, supported in both
#' data and manual modes.
#'
#' @seealso \code{\link{exclude}} for per-arm exclusions after splitting,
#'   \code{\link{endpoint}} for per-arm endpoints
#'
#' @examples
#' # Observational study (STROBE)
#' enroll(n = 3860) |>
#'   stratify(labels = c("Exposed", "Unexposed"), n = c(1900, 1960),
#'            label = "Classified by exposure")
#'
#' # Randomized trial (CONSORT)
#' enroll(n = 400) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(200, 200))
#'
#' @family flow construction functions
#' @export
stratify <- function(.flow, variable = NULL, labels = NULL, n = NULL,
                     label = "Stratified") {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    ## Count the active (uncombined) split depth. Up to two factorial levels are
    ## supported---e.g., a factorial trial's double-randomization, where a second
    ## stratify()/allocate() splits each arm again. combine() converges the
    ## innermost level back to its parent, so it decrements the depth. A third
    ## level is refused.
    split_depth <- 0L
    for (s in .flow$steps) {
        if (s$type == "stratify") split_depth <- split_depth + 1L
        if (s$type == "combine")  split_depth <- max(0L, split_depth - 1L)
    }
    if (split_depth >= 2L)
        stop("At most two factorial stratify()/allocate() levels are supported; ",
             "use combine() to recombine a level before splitting again.",
             call. = FALSE)

    if (.flow$mode == "data" && is.null(variable))
        stop("Supply 'variable' in data mode", call. = FALSE)
    if (.flow$mode == "manual" && (is.null(labels) || is.null(n)))
        stop("Supply 'labels' and 'n' in manual mode", call. = FALSE)
    if (.flow$mode == "manual") {
        if (split_depth == 0L) {
            ## First-level split: one count per arm.
            if (length(labels) != length(n))
                stop("'labels' and 'n' must have the same length", call. = FALSE)
        } else {
            ## Factorial split: 'labels' are the shared sub-arm names and 'n' gives one
            ## count per sub-arm per parent (parent-major), or a single shared set.
            if (length(n) %% length(labels) != 0L)
                stop("For a factorial split, the length of 'n' must be a multiple of the ",
                     "number of sub-arm labels (one count per sub-arm, optionally ",
                     "repeated for each parent).", call. = FALSE)
        }
    }

    step <- list(
        type     = "stratify",
        variable = variable,
        labels   = labels,
        n        = n,
        label    = label
    )

    .flow$steps <- c(.flow$steps, list(step))
    .flow
}


#' @rdname stratify
#' @export
allocate <- function(.flow, variable = NULL, labels = NULL, n = NULL,
                     label = "Randomized") {
    stratify(.flow, variable = variable, labels = labels, n = n, label = label)
}


#' Merge Parallel Streams
#'
#' Converges all active parallel streams into a single flow. Used to handle
#' either source convergence or split-and-recombine topologies. After
#' \code{stratify()}, recombines strata that were characterized independently
#' back into a unified downstream flow.
#'
#' @param .flow A \code{selecta} object with active parallel streams
#'   (from \code{sources()} or \code{stratify()}).
#' @param label Character string for the merged node.
#' @param sublabel Optional character string rendered below \code{label}
#'   inside the same box. Useful for describing the recombined cohort.
#' @param n Integer. Explicit post-merge count (manual mode). If omitted,
#'   computed as the sum of all active stream counts.
#' @param reasons Optional named integer vector of sub-items displayed
#'   below the count (\emph{e.g.,} outcome categories).
#'
#' @return The updated \code{selecta} object with a combine step
#'   appended. All subsequent steps operate on the single merged stream.
#'
#' @details
#' \code{combine()} converges the active parallel streams into one node and
#' is the counterpart to both entry splits. After \code{sources()}, it
#' pools the identification streams of a systematic review; after
#' \code{stratify()} (or \code{allocate()}), it recombines strata
#' that were handled independently, producing a split-and-recombine diagram.
#' 
#' By default, the merged count is the sum of the incoming streams after
#' any per-arm exclusions applied since the split---an explicit \code{n}
#' overrides this in manual mode. In such situations, an additional option
#' is provided (\code{getOption("selecta.check_arithmetic")}, default
#' \code{TRUE}), which will check arithmetic and raise an advisory warning
#' if there is a discrepancy between counts.
#' 
#' The optional \code{sublabel} parameter prints on a second line inside the
#' merged box, which is convenient for naming the recombined cohort.
#'
#' @seealso \code{\link{sources}} for multi-source entry,
#'   \code{\link{stratify}} for split-and-recombine flows
#'
#' @examples
#' # PRISMA: merge identification sources
#' sources(PubMed = 1234, Embase = 567) |>
#'   combine("Records after deduplication") |>
#'   exclude("Records removed", n = 352, show_count = FALSE,
#'           reasons = c("Duplicates" = 340, "Automation" = 12))
#'
#' # Split-and-recombine: stratify, then combine
#' enroll(n = 158) |>
#'   stratify(labels = c("Not screened", "Screened"), n = c(82, 76),
#'            label = "Screening status") |>
#'   exclude("Condition not confirmed", n = c(44, 66)) |>
#'   combine("Confirmed cohort",
#'           sublabel = "Participants with confirmed diagnosis") |>
#'   exclude("Incomplete records", n = 7) |>
#'   endpoint("Final cohort")
#'
#' @family flow construction functions
#' @export
combine <- function(.flow, label, sublabel = NULL, n = NULL,
                    reasons = NULL) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    has_parallel <- FALSE
    for (s in .flow$steps) {
        if (s$type %chin% c("sources", "stratify")) { has_parallel <- TRUE; break }
    }
    if (!has_parallel)
        stop("combine() requires a preceding sources() or stratify() step",
             call. = FALSE)

    step <- list(
        type     = "combine",
        label    = label,
        sublabel = sublabel,
        n        = n,
        reasons  = reasons
    )

    .flow$steps <- c(.flow$steps, list(step))
    .flow
}


### * Termination

#' Mark the Final Analysis Endpoint
#'
#' Adds the terminal node(s) to the enrollment flow. If arms have been
#' defined via \code{stratify()}, one endpoint box appears per arm.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string for the final box. With \code{groups} it
#'   labels the shared distributor box above the group boxes. Default
#'   \code{"Final Analysis"}.
#' @param breakdown Optional named numeric vector (or, for a per-arm endpoint,
#'   a list of them) itemizing the box total into parts printed \emph{within}
#'   the box, beneath the total. This is the STARD final-diagnosis form, where
#'   each terminal box reports its target-condition composition, \emph{e.g.}
#'   \code{breakdown = c("Target +" = 160, "Target -" = 40)}. Mutually
#'   exclusive with \code{groups}.
#' @param groups Optional character vector of group labels (manual mode). When
#'   supplied, the endpoint splits into one \emph{separate} terminal box per
#'   group, fanning from a shared distributor. Use this for study-design
#'   diagrams that end by displaying the groups to be analyzed (\dQuote{Group
#'   A}, \dQuote{Group B}, ...). A split endpoint requires a single incoming
#'   stream; it cannot follow an unrecombined \code{stratify()} or
#'   \code{allocate()}. Mutually exclusive with \code{breakdown}.
#' @param variable Optional character naming a grouping column (data mode).
#'   Splits the terminal endpoint by that column, one box per level, with
#'   counts tabulated automatically. The data-mode counterpart of
#'   \code{groups}/\code{n}; same single-stream requirement.
#' @param n Optional numeric vector of per-group counts (manual mode), parallel
#'   to \code{groups}.
#'
#' @return The updated \code{selecta} object with an endpoint step appended.
#'
#' @details
#' \code{endpoint()} closes the flow with its terminal node(s) and is usually
#' the last step in a pipeline. When the flow has been split with
#' \code{stratify()} or \code{allocate()} and not recombined, one
#' endpoint box is drawn per arm, and \code{label} and \code{breakdown} may be
#' supplied per arm.
#'
#' Two distinct presentations of detail are available, which are mutually
#' exclusive. \code{breakdown} itemizes a single box's total as text lines
#' inside that box (the STARD final-diagnosis form, reporting each box's
#' target-condition composition). Conversely, \code{groups} divides the
#' endpointinto separate side-by-side boxes, one per group, fanning from a
#' shared distributor; this design favors study diagrams that end by
#' displaying the groups to be analyzed. The completed object is then passed
#' to \code{flowchart()}, \code{flowsave()}, or \code{recdims()}.
#'
#' @seealso \code{\link{assess}} for the diagnostic test-receipt steps that
#'   precede a STARD endpoint, \code{\link{flowchart}} for rendering
#'
#' @examples
#' enroll(n = 300) |>
#'   exclude("Excluded", n = 40) |>
#'   endpoint("Included in analysis")
#'
#' # STARD-style per-arm endpoint with a within-box breakdown
#' enroll(n = 500) |>
#'   stratify(labels = c("Positive", "Negative"), n = c(200, 300),
#'            label = "Index test result") |>
#'   endpoint("Final diagnosis",
#'            breakdown = list(c("Target +" = 160, "Target -" = 40),
#'                             c("Target +" = 25, "Target -" = 275)))
#'
#' # Split endpoint into separate terminal group boxes (manual)
#' enroll(n = 300, label = "Eligible cohort") |>
#'   endpoint("Allocated to study group",
#'            groups = c("Group A", "Group B", "Group C"),
#'            n = c(100, 100, 100))
#'
#' # Split endpoint by a grouping column (data mode)
#' df <- data.frame(id = 1:300, grp = sample(c("A", "B", "C"), 300, TRUE))
#' enroll(df, id = "id", label = "Eligible cohort") |>
#'   endpoint("Allocated to study group", variable = "grp")
#'
#' @family flow construction functions
#' @export
endpoint <- function(.flow, label = "Final Analysis", breakdown = NULL,
                     groups = NULL, n = NULL, variable = NULL) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    is_split <- !is.null(groups) || !is.null(variable)

    if (is_split) {
        if (!is.null(breakdown))
            stop("Supply either a split endpoint (groups/variable) or 'breakdown' ",
                 "(an itemization within one box), not both.", call. = FALSE)

        ## A split endpoint needs a single incoming stream. Detect an active
        ## (uncombined) split now, so the error is raised at construction rather
        ## than deferred to rendering.
        has_active_split <- FALSE
        for (s in .flow$steps) {
            if (s$type == "stratify") has_active_split <- TRUE
            if (s$type == "combine")  has_active_split <- FALSE
        }
        if (has_active_split)
            stop("A split endpoint (groups/variable) requires a single incoming ",
                 "stream; it cannot follow an uncombined stratify()/allocate(). ",
                 "Recombine with combine() first, or lay out per-arm terminal ",
                 "groups manually.", call. = FALSE)

        ## Mode-appropriate inputs, mirroring stratify()/allocate(): a grouping
        ## column in data mode, explicit labels and counts in manual mode.
        if (.flow$mode == "data") {
            if (is.null(variable))
                stop("In data mode, split a terminal endpoint with 'variable' (a ",
                     "grouping column); 'groups'/'n' are for manual mode.",
                     call. = FALSE)
        } else {
            if (is.null(groups))
                stop("In manual mode, supply 'groups' (the group labels) to split a ",
                     "terminal endpoint.", call. = FALSE)
            if (!is.character(groups) || !length(groups))
                stop("'groups' must be a non-empty character vector of group labels.",
                     call. = FALSE)
            if (is.null(n) || length(n) != length(groups))
                stop("Supply 'n' with one count per group (length(groups) == ",
                     "length(n)).", call. = FALSE)
        }
    }

    step <- list(
        type     = "endpoint",
        label    = label,
        reasons  = breakdown,
        groups   = groups,
        n        = n,
        variable = variable
    )

    .flow$steps <- c(.flow$steps, list(step))
    .flow
}

#' Record an Assessment or Procedure Step
#'
#' Models a step where participants undergo (or fail to undergo) a test
#' or procedure. This is the primary building block for STARD-style
#' diagnostic accuracy diagrams. The side box shows who did \emph{not}
#' receive the procedure (with optional reasons), and the main flow
#' continues with those who \emph{were} assessed.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string naming the test or procedure
#'   (\emph{e.g.,} \code{"Index test"}, \code{"Reference standard"}).
#' @param criterion An unquoted logical expression that evaluates to
#'   \code{TRUE} for rows that did \strong{not} receive the test. Data
#'   mode only.
#' @param not_received Integer (manual mode). Number of participants who
#'   did not receive this test.
#' @param reasons Named integer vector of reasons for non-receipt
#'   (\emph{e.g.,} \code{c("Refused" = 12, "Contraindicated" = 10)}).
#' @param show_zero Logical. If \code{TRUE}, display zero-count reasons.
#'   Default \code{FALSE}.
#'
#' @return The updated \code{selecta} object with an assessment step
#'   appended.
#'
#' @details
#' \code{assess()} models a test or procedure that only part of the cohort
#' undergoes, the recurring motif of STARD diagnostic-accuracy diagrams. It
#' is implemented as an \code{exclude()} step with inverted label
#' semantics: the side box reads \dQuote{Did not receive \emph{label}} and
#' the continuing box reads \dQuote{Received \emph{label}}, so the main flow
#' carries those who \emph{were} assessed. In data mode, \code{criterion} is
#' an unquoted logical expression that is \code{TRUE} for participants who
#' did \strong{not} receive the test; in manual mode, \code{not_received}
#' gives that count and \code{reasons} an optional named breakdown. Chained
#' \code{assess()} steps commonly precede a \code{stratify()} split on
#' the index-test result, with each terminal box reporting its
#' target-condition breakdown.
#'
#' @seealso \code{\link{exclude}} for general exclusion steps,
#'   \code{\link{endpoint}} for the terminal diagnosis boxes (STARD)
#'
#' @examples
#' # STARD diagnostic accuracy flow
#' enroll(n = 360, label = "Eligible patients") |>
#'   assess("Index test", not_received = 22,
#'          reasons = c("Refused" = 12, "Contraindicated" = 10)) |>
#'   assess("Reference standard", not_received = 18) |>
#'   stratify(labels = c("Index test positive", "Index test negative"),
#'            n = c(150, 170), label = "Index test result") |>
#'   endpoint("Final diagnosis",
#'            breakdown = list(c("Target +" = 130, "Target -" = 20),
#'                             c("Target +" = 15, "Target -" = 155)))
#'
#' @family flow construction functions
#' @export
assess <- function(.flow, label, criterion, not_received = NULL,
                   reasons = NULL, show_zero = FALSE) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    has_expr <- !missing(criterion)

    if (.flow$mode == "data" && !has_expr)
        stop("Supply 'criterion' in data mode", call. = FALSE)
    if (.flow$mode == "manual" && is.null(not_received))
        stop("Supply 'not_received' in manual mode", call. = FALSE)

    ## Construct exclusion step with inverted label semantics
    side_label <- paste("Did not receive", tolower(label))
    included   <- paste("Received", tolower(label))

    expr_call <- if (has_expr) substitute(criterion) else NULL

    ## Classify reasons argument
    reasons_var <- NULL
    reasons_manual <- NULL
    if (!is.null(reasons)) {
        if (is.character(reasons) && length(reasons) == 1L) {
            if (.flow$mode != "data")
                stop("Column-name 'reasons' only works in data mode", call. = FALSE)
            reasons_var <- reasons
        } else {
            reasons_manual <- reasons
        }
    }

    step <- list(
        type           = "exclude",
        label          = side_label,
        expr_call      = expr_call,
        n              = not_received,
        reasons        = reasons_manual,
        reasons_var    = reasons_var,
        show_zero      = show_zero,
        show_count     = TRUE,
        included_label = included
    )

    .flow$steps <- c(.flow$steps, list(step))
    .flow
}


### * Phase

#' Label a Phase of the Enrollment Flow
#'
#' Adds a vertical phase label to the left margin of the diagram
#' (\emph{e.g.,} \code{"Enrollment"}, \code{"Allocation"},
#' \code{"Follow-up"}, \code{"Analysis"}). Phase labels span all
#' subsequent steps until the next \code{phase()} call or the end of
#' the flow.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string. The phase label, rendered as rotated
#'   text on the left margin.
#'
#' @return The updated \code{selecta} object with a phase marker
#'   appended.
#'
#' @details
#' \code{phase()} inserts a stage boundary rather than a flow node. Each
#' call opens a phase whose label is drawn in the left margin, spanning
#' every subsequent step until the next \code{phase()} or the end of the
#' flow. The purpose of these phase markers is to reflect the stages of
#' analysis in the diagram; as such, they are purely presentational, and
#' they do not alter counts or topology. In the \code{grid} engine,
#' phase labels are rendered vertically and are wrapped to fit their band
#' by default; conversely, the \code{dot} engine renders phase labels
#' horizontally due to engine limitations.
#'
#' @seealso \code{\link{flowchart}} for rendering with phase labels
#'
#' @examples
#' # Phase labels divide a flow into labeled stages. The printed summary
#' # marks each phase with a "--- Label ---" banner.
#' enroll(n = 1200, label = "Records identified") |>
#'   phase("Enrollment") |>
#'   exclude("Duplicates", n = 84) |>
#'   phase("Allocation") |>
#'   stratify(labels = c("Drug A", "Placebo"), n = c(520, 533)) |>
#'   phase("Follow-up") |>
#'   exclude("Lost to follow-up", n = c(23, 31)) |>
#'   phase("Analysis") |>
#'   endpoint("Final Analysis")
#'
#' @family flow construction functions
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
