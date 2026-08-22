#' @keywords internal
#'
#' @section Package options:
#' \code{selecta} reads the following session options, each settable with
#' \code{options()} and each with a safe default:
#' \describe{
#'   \item{\code{selecta.number_format}}{Default count formatting when
#'     \code{number_format} is not passed explicitly. A preset (\code{"us"},
#'     \code{"eu"}, \code{"space"}, \code{"none"}) or a custom
#'     \code{c(big.mark, decimal.mark)} pair. Defaults to \code{"us"}.}
#'   \item{\code{selecta.vpad}}{Default vertical padding between rows, in
#'     inches, used by the grid engine and by \code{recdims()}.
#'     Defaults to \code{0.25}.}
#'   \item{\code{selecta.check_arithmetic}}{Whether manual-mode count
#'     consistency checks emit advisory warnings (arm counts not summing to
#'     the split total, an exclusion exceeding the available count,
#'     sub-reasons not summing to their total, or a manual \code{combine()}
#'     disagreeing with its streams). The counts are never altered. Defaults
#'     to \code{TRUE}.}
#'   \item{\code{selecta.debug_layout}}{Whether the computation and rendering
#'     functions print a structured layout trace via \code{message()} (node
#'     and edge tables, computed positions, recommended dimensions, per-phase
#'     band heights, and the generated DOT source). Useful for bug reports.
#'     Defaults to \code{FALSE}.}
#' }
#'
#' @examples
#' \donttest{
#' opts <- options()  # save to restore afterwards
#' options(selecta.number_format = "eu")     # 1.234 instead of 1,234
#' options(selecta.vpad = 0.35)              # looser default spacing
#' options(selecta.check_arithmetic = FALSE) # silence manual-count warnings
#' options(selecta.debug_layout = TRUE)      # print a layout trace
#' options(opts)                             # restore previous options
#' }
"_PACKAGE"

#' @import data.table
#' @import grid
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom stats setNames
NULL


## Null-coalescing operator: returns `y` when `x` is NULL, else `x`.
## Defined internally to avoid the base R `%||%` (added in R 4.4.0), so the
## minimum supported version remains that of the native pipe (R 4.1.0).
#' Null-coalescing operator
#'
#' Returns the second argument when the first is \code{NULL}, else the first.
#' @param x,y Values; \code{y} is returned when \code{x} is \code{NULL}.
#' @return \code{x} when non-\code{NULL}, otherwise \code{y}.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert a Length Between Measurement Units
#'
#' Converts a numeric length between the units accepted by \code{recdims()}
#' and \code{flowsave()}. Inches are the package's internal representation, so
#' conversion is routed through inches in both directions.
#'
#' @param value Numeric length, or a vector of lengths.
#' @param from,to Character strings naming the source and target units, each
#'   one of \code{"in"}, \code{"cm"}, or \code{"mm"}.
#' @return A numeric vector parallel to \code{value}, expressed in \code{to}.
#' @keywords internal
convert_units <- function(value, from = "in", to = "in") {
    if (identical(from, to)) return(value)
    ## Units per inch, so division reaches inches and multiplication leaves them
    per_inch <- c("in" = 1, "cm" = 2.54, "mm" = 25.4)
    value / per_inch[[from]] * per_inch[[to]]
}

#' Emit a Debug Section When Layout Debugging Is Enabled
#'
#' Prints a titled section followed by one or more objects via
#' \code{message()}, but only when \code{options(selecta.debug_layout =
#' TRUE)} is set. Used by the computation and rendering functions to expose
#' intermediate state for diagnosis; a no-op otherwise.
#'
#' @param title Character section title.
#' @param ... Named or unnamed objects to print; data frames and tables are
#'   captured via \code{print()}, scalars are shown inline.
#' @return Invisibly \code{NULL}; called for its side effect.
#' @keywords internal
debug_emit <- function(title, ...) {
    if (!isTRUE(getOption("selecta.debug_layout", FALSE))) return(invisible(NULL))
    cap <- function(s) {
        if (length(s) != 1L || is.na(s) || !nzchar(s)) return(s)
        paste0(toupper(substring(s, 1L, 1L)), substring(s, 2L))
    }
    message(sprintf("=== selecta debug: %s ===", title))
    args <- list(...)
    nms  <- names(args) %||% rep("", length(args))
    for (i in seq_along(args)) {
        obj <- args[[i]]
        lbl <- if (nzchar(nms[i])) nms[i] else NULL
        if (is.data.frame(obj) || is.matrix(obj)) {
            if (!is.null(lbl)) message(sprintf("--- %s ---", cap(lbl)))
            message(paste(utils::capture.output(print(obj)), collapse = "\n"))
        } else if (length(obj) == 1L && is.atomic(obj)) {
            message(sprintf("%s%s", if (!is.null(lbl)) paste0(lbl, " = ") else "", obj))
        } else {
            if (!is.null(lbl)) message(sprintf("--- %s ---", cap(lbl)))
            message(paste(utils::capture.output(print(obj)), collapse = "\n"))
        }
    }
    invisible(NULL)
}

#' Warn About an Inconsistency in a Flow
#'
#' Emits a \code{warning()} describing a counting or attribution
#' inconsistency in a flow---for example, manual arm counts that do not sum
#' to the number entering a split, an exclusion larger than the available
#' count, or a data-mode reason column that does not account for every
#' removed row. Counts are never altered or rejected, since an author may have a
#' legitimate reason for figures that do not reconcile; the warning is purely
#' advisory and may be silenced with
#' \code{options(selecta.check_arithmetic = FALSE)}.
#'
#' @param fmt A \code{sprintf} format string for the message.
#' @param ... Values substituted into \code{fmt}.
#' @return Invisibly \code{NULL}; called for its side effect.
#' @keywords internal
warn_arithmetic <- function(fmt, ...) {
    if (!isTRUE(getOption("selecta.check_arithmetic", TRUE)))
        return(invisible(NULL))
    warning(sprintf(fmt, ...), call. = FALSE)
    invisible(NULL)
}

## Suppress R CMD check notes for data.table NSE column references.
utils::globalVariables(c(
           ".",
           "..cols",
           "..node_cols",
           "arm_id",
           "arm_level",
           "arm_parent",
           "bh_inches",
           "box_h",
           "bw",
           "bw_inches",
           "edge_type",
           "fill_col",
           "from",
           "from_bot",
           "from_row",
           "fr",
           "from_arm",
           "grp",
           "hdr_h",
           "i.arm_id",
           "i.bh_inches",
           "i.box_h",
           "i.hdr_h",
           "i.row",
           "i.y",
           "n",
           "n_reason",
           "n_sides",
           "needed",
           "node_id",
           "r",
           "reasons",
           "role",
           "row",
           "s",
           "side_h",
           "src_h",
           "stack_h",
           "stream_group",
           "sublabel",
           "text",
           "to",
           "to_top",
           "to_row",
           "total",
           "x",
           "x_in",
           "y"
       ))
