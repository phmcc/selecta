#' Initialize an Enrollment Flow
#'
#' Entry point for building an EQUATOR-style enrollment diagram from a single
#' starting population. Accepts either a \code{data.frame} (data-driven mode,
#' where counts are computed automatically from exclusion expressions) or a
#' starting count \code{n} (manual mode, where counts are supplied explicitly
#' at each step).
#'
#' For flows that begin with multiple parallel identification streams
#' (\emph{e.g.,} PRISMA 2020 systematic reviews), use \code{\link{sources}}
#' instead.
#'
#' @param data A \code{data.frame} or \code{data.table} in which each row
#'   represents one participant. When supplied, exclusion expressions passed
#'   to \code{\link{exclude}} are evaluated against this data to compute
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
#'   Subsequent pipeline functions (\code{\link{exclude}},
#'   \code{\link{stratify}}, \code{\link{endpoint}}, \emph{etc.}) append
#'   steps to this object.
#'
#' @seealso \code{\link{sources}} for multi-source entry,
#'   \code{\link{exclude}} for adding exclusion criteria,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' # Manual mode
#' enroll(n = 500, label = "Assessed for eligibility")
#'
#' # Data-driven mode
#' enroll(rctselect2, id = "patient_id", label = "Study Population")
#'
#' # Minimal CONSORT pipeline
#' enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Treatment", "Control"), n = c(218, 217)) |>
#'   endpoint("Analysed")
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


#' Initialize a Multi-Source Flow
#'
#' Entry point for flows that begin with multiple parallel identification
#' streams, such as PRISMA 2020 systematic review diagrams. Each named
#' argument defines a source \emph{group} (column). Individual databases
#' or registers within each group are listed as sub-items inside a single
#' box, mirroring the format of exclusion reasons.
#'
#' Up to three columns are supported, matching the PRISMA 2020 structure:
#' previous studies (left), databases and registers (centre), and other
#' methods (right). Use \code{\link{combine}} downstream to merge the
#' parallel streams into a single flow.
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


#' Exclude Participants by a Criterion
#'
#' Appends an exclusion step to the enrollment flow. Participants matching the
#' criterion are removed and shown in a side box. Optionally, itemized
#' sub-reasons can be displayed below the total.
#'
#' After a \code{stratify()} step, both \code{label} and
#' \code{included_label} accept character vectors (one element per arm)
#' for per-arm labelling -- useful in observational designs where
#' attrition mechanisms differ across strata.
#'
#' @param .flow A \code{selecta} object (piped from \code{enroll()} or a
#'   previous step).
#' @param label Character. Human-readable description for the side box
#'   (\emph{e.g.,} \code{"Excluded"} or \code{"Lost to follow-up"}).
#'   After \code{stratify()}, may be a character vector with one label
#'   per arm (\emph{e.g.,} \code{c("Treatment discontinued", "Initiated treatment")}).
#' @param expr An unquoted expression evaluated against the data. Should
#'   evaluate to \code{TRUE} for rows to be \strong{removed}. (Data-driven
#'   mode only.)
#' @param n Integer (manual mode). Number of participants removed at this step.
#'   After a \code{stratify()} step, supply a vector with one value per arm.
#' @param reasons Exclusion sub-reasons. Accepts three forms:
#'   \itemize{
#'     \item A \strong{named integer vector} (manual mode): counts per reason,
#'       \emph{e.g.,} \code{c("Disease progression" = 12, "Declined" = 8)}.
#'     \item A \strong{character string} (data-driven mode): column name whose
#'       values are tabulated automatically.
#'     \item A \strong{list} of named vectors (manual mode after \code{stratify()}):
#'       one vector per arm.
#'   }
#' @param show_zero Logical. If \code{FALSE} (default), sub-reasons with a
#'   count of zero are hidden. Set to \code{TRUE} to display all pre-specified
#'   reason categories, including those with zero participants.
#' @param show_count Logical. If \code{FALSE} (default), the intermediate
#'   count box is suppressed -- the count still updates internally but no box
#'   is rendered. Set to \code{TRUE} to force a count box. Overridden by
#'   \code{included_label}: providing any \code{included_label} always
#'   creates a count box regardless of \code{show_count}. Also automatically
#'   suppressed when the next step is \code{stratify()}, \code{endpoint()},
#'   or \code{allocate()}.
#' @param included_label Character string (or vector). Optional text for the
#'   box showing the count remaining after exclusion. When provided, a
#'   count box is always rendered regardless of \code{show_count}. After
#'   \code{stratify()}, may be a character vector with one label per arm.
#'
#' @return The updated \code{selecta} object with an exclusion step appended.
#'
#' @seealso \code{\link{assess}} for assessment/procedure steps (STARD),
#'   \code{\link{enroll}} for initialising a flow
#'
#' @examples
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
#' @export
exclude <- function(.flow, label, expr, n = NULL, reasons = NULL,
                    show_zero = FALSE, show_count = FALSE,
                    included_label = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  expr_call <- substitute(expr)
  has_expr <- !missing(expr)

  if (.flow$mode == "data" && !has_expr)
    stop("Supply 'expr' in data-driven mode", call. = FALSE)
  if (.flow$mode == "manual" && is.null(n))
    stop("Supply 'n' in manual mode", call. = FALSE)

  ## Classify reasons argument
  reasons_var <- NULL
  reasons_manual <- NULL

  if (!is.null(reasons)) {
    if (is.character(reasons) && length(reasons) == 1L &&
        is.null(names(reasons))) {
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
    type           = "exclude",
    label          = label,
    expr_call      = if (has_expr) expr_call else NULL,
    n              = n,
    reasons        = reasons_manual,
    reasons_var    = reasons_var,
    show_zero      = show_zero,
    show_count     = show_count,
    included_label = included_label
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
#' Internally, \code{assess()} creates an exclusion step with inverted
#' label semantics: the side box reads \code{"Did not receive [label]"}
#' and the remaining-count box reads \code{"Received [label]"}.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string naming the test or procedure
#'   (\emph{e.g.,} \code{"Index test"}, \code{"Reference standard"}).
#' @param expr An unquoted expression that evaluates to \code{TRUE} for
#'   rows that did \strong{not} receive the test. Data-driven mode only.
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
#' @seealso \code{\link{exclude}} for general exclusion steps,
#'   \code{\link{classify}} for cross-classification (STARD)
#'
#' @examples
#' # STARD diagnostic accuracy flow
#' enroll(n = 360, label = "Eligible patients") |>
#'   assess("Index test", not_received = 22,
#'          reasons = c("Refused" = 12, "Contraindicated" = 10)) |>
#'   assess("Reference standard", not_received = 18) |>
#'   classify(rows = c("Target +", "Target -"),
#'            cols = c("Index +", "Index -"),
#'            n = matrix(c(175, 25, 15, 105), nrow = 2,
#'                       dimnames = list(c("Target +", "Target -"),
#'                                       c("Index +", "Index -"))))
#'
#' @export
assess <- function(.flow, label, expr, not_received = NULL,
                   reasons = NULL, show_zero = FALSE) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  has_expr <- !missing(expr)

  if (.flow$mode == "data" && !has_expr)
    stop("Supply 'expr' in data-driven mode", call. = FALSE)
  if (.flow$mode == "manual" && is.null(not_received))
    stop("Supply 'not_received' in manual mode", call. = FALSE)

  ## Construct exclusion step with inverted label semantics
  side_label <- paste("Did not receive", tolower(label))
  remaining  <- paste("Received", tolower(label))

  expr_call <- if (has_expr) substitute(expr) else NULL

  ## Classify reasons argument
  reasons_var <- NULL
  reasons_manual <- NULL
  if (!is.null(reasons)) {
    if (is.character(reasons) && length(reasons) == 1L) {
      if (.flow$mode != "data")
        stop("Column-name 'reasons' only works in data-driven mode", call. = FALSE)
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
    included_label = remaining
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


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
#' @seealso \code{\link{flowchart}} for rendering with phase labels
#'
#' @examples
#' enroll(n = 1200, label = "Records identified") |>
#'   phase("Enrollment") |>
#'   exclude("Duplicates", n = 84) |>
#'   phase("Allocation") |>
#'   stratify(labels = c("Drug A", "Placebo"), n = c(520, 533)) |>
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


#' Split into Parallel Study Arms or Strata
#'
#' Divides the enrollment flow into parallel arms. This is the primary
#' function for splitting a population by any characteristic: treatment
#' assignment, exposure status, diagnostic test result, etc. Subsequent
#' \code{exclude()} calls apply within each arm independently.
#'
#' \code{allocate()} is a convenience alias with default label
#' \code{"Randomized"}, suitable for interventional trials (CONSORT).
#'
#' @param .flow A \code{selecta} object.
#' @param variable Character string naming the column that defines the arms
#'   (data-driven mode). Ignored in manual mode.
#' @param labels A character vector of arm labels. In data-driven mode, this
#'   can be a named vector to relabel factor levels (\emph{e.g.,}
#'   \code{c(A = "Drug A", B = "Placebo")}). In manual mode, these are the
#'   arm names.
#' @param n Integer vector (manual mode). Number of participants in each arm,
#'   in the same order as \code{labels}.
#' @param label Character string for the split box. Defaults to
#'   \code{"Stratified"} for \code{stratify()} and \code{"Randomized"} for
#'   \code{allocate()}.
#'
#' @return The updated \code{selecta} object with a stratification step
#'   appended. All subsequent pipeline steps operate independently within
#'   each arm.
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
#' @export
stratify <- function(.flow, variable = NULL, labels = NULL, n = NULL,
                     label = "Stratified") {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  has_split <- FALSE
  for (s in .flow$steps) {
    if (s$type == "stratify") { has_split <- TRUE; break }
  }
  if (has_split)
    stop("Only one stratify()/allocate() split is currently supported",
         call. = FALSE)

  if (.flow$mode == "data" && is.null(variable))
    stop("Supply 'variable' in data-driven mode", call. = FALSE)
  if (.flow$mode == "manual" && (is.null(labels) || is.null(n)))
    stop("Supply 'labels' and 'n' in manual mode", call. = FALSE)
  if (.flow$mode == "manual" && length(labels) != length(n))
    stop("'labels' and 'n' must have the same length", call. = FALSE)

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
#' Converges all active parallel streams (from \code{\link{sources}}) into
#' a single flow. The merged node displays the total count across all
#' streams. Use \code{\link{exclude}} with \code{show_count = FALSE}
#' immediately after to show items removed during the merge (\emph{e.g.,}
#' duplicate removal).
#'
#' @param .flow A \code{selecta} object with active parallel streams.
#' @param label Character string for the merged node.
#' @param n Integer. Explicit post-merge count (manual mode). If omitted,
#'   computed as the sum of all source counts.
#'
#' @return The updated \code{selecta} object with a combine step
#'   appended. All subsequent steps operate on the single merged stream.
#'
#' @seealso \code{\link{sources}} for creating parallel streams
#'
#' @examples
#' sources(PubMed = 1234, Embase = 567) |>
#'   combine("Records after deduplication") |>
#'   exclude("Records removed", n = 352, show_count = FALSE,
#'           reasons = c("Duplicates" = 340, "Automation" = 12))
#'
#' @export
combine <- function(.flow, label, n = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  has_sources <- FALSE
  for (s in .flow$steps) {
    if (s$type == "sources") { has_sources <- TRUE; break }
  }
  if (!has_sources)
    stop("combine() requires a preceding sources() step", call. = FALSE)

  step <- list(
    type    = "combine",
    label   = label,
    n       = n
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


#' Cross-Classification Grid
#'
#' Adds a terminal cross-classification step, producing an m \eqn{\times}{x}
#' n result grid. This is the primary building block for STARD-style
#' diagnostic accuracy diagrams where participants are classified by index
#' test result and reference standard outcome.
#'
#' @param .flow A \code{selecta} object.
#' @param rows Character vector of row labels (\emph{e.g.,} index test
#'   results).
#' @param cols Character vector of column labels (\emph{e.g.,} reference
#'   standard outcomes).
#' @param n A matrix of counts with \code{length(rows)} rows and
#'   \code{length(cols)} columns. Row and column names are used as labels
#'   if \code{rows}/\code{cols} are not supplied.
#' @param label Optional character string header for the grid.
#'
#' @return The updated \code{selecta} object with a classification step
#'   appended.
#'
#' @seealso \code{\link{assess}} for preceding assessment steps,
#'   \code{\link{endpoint}} for simple terminal nodes
#'
#' @examples
#' enroll(n = 320, label = "Received both tests") |>
#'   classify(
#'     rows = c("Test positive", "Test negative"),
#'     cols = c("Target +", "Target \u2212"),
#'     n = matrix(c(160, 10, 20, 130), nrow = 2)
#'   )
#'
#' @export
classify <- function(.flow, rows = NULL, cols = NULL, n = NULL,
                     label = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  if (is.null(n))
    stop("Supply 'n' as a matrix of counts", call. = FALSE)

  if (!is.matrix(n)) n <- as.matrix(n)

  if (is.null(rows)) rows <- rownames(n)
  if (is.null(cols)) cols <- colnames(n)
  if (is.null(rows))
    stop("Supply 'rows' or a matrix with row names", call. = FALSE)
  if (is.null(cols))
    stop("Supply 'cols' or a matrix with column names", call. = FALSE)
  if (nrow(n) != length(rows) || ncol(n) != length(cols))
    stop("Dimensions of 'n' must match length of 'rows' and 'cols'",
         call. = FALSE)

  step <- list(
    type  = "classify",
    rows  = rows,
    cols  = cols,
    n     = n,
    label = label
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


#' Mark the Final Analysis Endpoint
#'
#' Adds the terminal node(s) to the enrollment flow. If arms have been
#' defined via \code{\link{stratify}}, one endpoint box appears per arm.
#'
#' @param .flow A \code{selecta} object.
#' @param label Character string for the final box. Default
#'   \code{"Final Analysis"}.
#' @param reasons Optional named numeric vector (or list of vectors for
#'   per-arm endpoints) giving sub-item counts displayed below the total.
#'   Useful for STARD-style final diagnosis boxes, \emph{e.g.,}
#'   \code{reasons = c("Target condition +" = 160, "Target condition -" = 20)}.
#'
#' @return The updated \code{selecta} object with an endpoint step
#'   appended.
#'
#' @seealso \code{\link{classify}} for cross-classification terminal grids,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' enroll(n = 300) |>
#'   exclude("Excluded", n = 40) |>
#'   endpoint("Included in analysis")
#'
#' # STARD-style per-arm endpoint with sub-items
#' enroll(n = 500) |>
#'   stratify(labels = c("Positive", "Negative"), n = c(200, 300),
#'            label = "Index test result") |>
#'   endpoint("Final diagnosis",
#'            reasons = list(c("Target +" = 160, "Target -" = 40),
#'                           c("Target +" = 25, "Target -" = 275)))
#'
#' @export
endpoint <- function(.flow, label = "Final Analysis", reasons = NULL) {

  if (!inherits(.flow, "selecta"))
    stop("'.flow' must be a selecta object", call. = FALSE)

  step <- list(
    type    = "endpoint",
    label   = label,
    reasons = reasons
  )

  .flow$steps <- c(.flow$steps, list(step))
  .flow
}


## ============================================================================
## Dataset Documentation
## ============================================================================

#' Simulated Observational Cohort (No Arms)
#'
#' A synthetic dataset of 800 patients in an observational study with
#' no treatment arms. Includes eligibility flags, exclusion reasons,
#' and follow-up loss indicators suitable for demonstrating STROBE-style
#' enrollment diagrams in data-driven mode.
#'
#' @format A \code{data.table} with 800 rows and the following columns:
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
#' data(rctselect0)
#' str(rctselect0)
"rctselect0"


#' Simulated Two-Arm Randomized Trial
#'
#' A synthetic dataset of 2,400 patients in a two-arm randomized
#' controlled trial. Includes screening, eligibility, treatment
#' assignment, and discontinuation variables suitable for demonstrating
#' CONSORT-style enrollment diagrams in data-driven mode.
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
#' data(rctselect2)
#' table(rctselect2$treatment)
"rctselect2"


#' Simulated Three-Arm Randomized Trial
#'
#' A synthetic dataset of 3,600 patients in a three-arm randomized
#' controlled trial. Structure matches \code{\link{rctselect2}} with an
#' additional treatment arm.
#'
#' @format A \code{data.table} with 3,600 rows. See \code{\link{rctselect2}}
#'   for column descriptions.
#'
#' @examples
#' data(rctselect3)
#' table(rctselect3$treatment)
"rctselect3"


#' Simulated Six-Arm Dose-Finding Trial
#'
#' A synthetic dataset of 7,200 patients in a six-arm dose-finding
#' trial. Structure matches \code{\link{rctselect2}} with six treatment
#' arms. Intended for demonstrating wide multi-arm layouts using
#' \code{\link{stratify}}.
#'
#' @format A \code{data.table} with 7,200 rows. See \code{\link{rctselect2}}
#'   for column descriptions.
#'
#' @examples
#' data(rctselect6)
#' table(rctselect6$treatment)
"rctselect6"
