### * Main functions

#' Number Formatting Utilities
#'
#' Internal utilities for locale-aware integer formatting of participant
#' counts in selecta diagrams. Counts are always integers, so the formatter
#' only needs a thousands separator (no decimal mark for the value itself,
#' though some preset locales still set one for completeness).
#'
#' @section Global Option:
#' The default number format can be set once per session:
#' \preformatted{
#'   options(selecta.number_format = "eu")
#' }
#' This avoids passing \code{number_format} to every function call.
#'
#' @name number_format
#' @keywords internal
NULL


#' Resolve number format marks
#'
#' Converts a \code{number_format} specification into a list of \code{big.mark}
#' and \code{decimal.mark} values used by all downstream formatting functions.
#' Supports named presets, custom two-element vectors, and the global
#' \code{selecta.number_format} option.
#'
#' @param number_format Character string specifying a named preset, a
#'   two-element character vector \code{c(big.mark, decimal.mark)}, or
#'   \code{NULL} to use the global option (falling back to \code{"us"}).
#'
#'   Named presets:
#'   \describe{
#'     \item{\code{"us"}}{Comma thousands, period decimal: 1,234.56}
#'     \item{\code{"eu"}}{Period thousands, comma decimal: 1.234,56}
#'     \item{\code{"space"}}{Thin-space thousands, period decimal: 1 234.56
#'       (SI/ISO 31-0 standard)}
#'     \item{\code{"none"}}{No thousands separator, period decimal: 1234.56}
#'   }
#'
#'   Custom vector: \code{c(",", ".")} or \code{c(".", ",")} \emph{etc.}
#'   The first element is \code{big.mark}, the second is \code{decimal.mark}.
#'
#' @return A list with components \code{big.mark} and \code{decimal.mark}.
#' @keywords internal
resolve_number_marks <- function(number_format = NULL) {

  ## Fall back to global option, then to "us"
  if (is.null(number_format))
    number_format <- getOption("selecta.number_format", "us")

  ## Custom vector: c(big.mark, decimal.mark)
  if (is.character(number_format) && length(number_format) == 2L)
    return(list(big.mark = number_format[1L],
                decimal.mark = number_format[2L]))

  ## Named presets (use thin space U+202F for SI style)
  if (is.character(number_format) && length(number_format) == 1L) {
    return(switch(number_format,
      "us"    = list(big.mark = ",",      decimal.mark = "."),
      "eu"    = list(big.mark = ".",      decimal.mark = ","),
      "space" = list(big.mark = "\u202F", decimal.mark = "."),
      "none"  = list(big.mark = "",       decimal.mark = "."),
      stop("Unknown number_format preset: '", number_format,
           "'. Use 'us', 'eu', 'space', 'none', or a ",
           "two-element character vector c(big.mark, decimal.mark).",
           call. = FALSE)
    ))
  }

  stop("'number_format' must be a character string preset or a ",
       "two-element character vector.", call. = FALSE)
}


#' Validate number_format parameter
#'
#' Checks that a \code{number_format} value is valid before use. Called early
#' in top-level functions to fail fast with a clear error message.
#'
#' @param number_format Value to validate.
#' @return Invisibly returns \code{TRUE} if valid.
#' @keywords internal
validate_number_format <- function(number_format) {
  if (is.null(number_format)) return(invisible(TRUE))

  if (!is.character(number_format))
    stop("'number_format' must be a character string or character vector.",
         call. = FALSE)

  if (length(number_format) == 1L) {
    valid_presets <- c("us", "eu", "space", "none")
    if (!number_format %in% valid_presets)
      stop("Unknown number_format preset: '", number_format,
           "'. Valid presets are: ",
           paste(paste0("'", valid_presets, "'"), collapse = ", "),
           ". Or use a two-element vector c(big.mark, decimal.mark).",
           call. = FALSE)
  } else if (length(number_format) == 2L) {
    if (number_format[1L] == number_format[2L] && nchar(number_format[1L]) > 0L)
      stop("big.mark and decimal.mark cannot be the same non-empty character.",
           call. = FALSE)
  } else {
    stop("Custom 'number_format' must be a two-element character vector ",
         "c(big.mark, decimal.mark).", call. = FALSE)
  }

  invisible(TRUE)
}


#' Format integer counts with a locale-aware thousands separator
#'
#' Formats integer participant counts for display in diagram boxes and
#' text summaries. Values below 1000 are returned without a separator.
#' The function is vectorized: a vector of counts yields a parallel
#' character vector, so an entire set of exclusion sub-reasons can be
#' formatted in a single call.
#'
#' @param n Integer count value, or a vector of counts. \code{NA}
#'   elements are returned as empty strings.
#' @param marks List with \code{big.mark} and \code{decimal.mark} as
#'   returned by \code{\link{resolve_number_marks}}. May be \code{NULL},
#'   in which case the current global setting is resolved automatically.
#'   \code{decimal.mark} is forwarded to \code{\link{format}} so that
#'   locales whose thousands separator is a period (\emph{e.g.,} the
#'   \code{"eu"} preset) do not trip \code{format}'s "big.mark and
#'   decimal.mark are both '.'" warning.
#' @return A character vector of formatted counts, parallel to \code{n}.
#' @keywords internal
fmt_n <- function(n, marks = NULL) {
  if (is.null(marks)) marks <- resolve_number_marks()
  vapply(n, function(x) {
    if (is.na(x)) return("")
    if (abs(x) >= 1000)
      trimws(format(as.integer(x), big.mark = marks$big.mark,
                    decimal.mark = marks$decimal.mark,
                    scientific = FALSE))
    else
      as.character(as.integer(x))
  }, character(1L), USE.NAMES = FALSE)
}
