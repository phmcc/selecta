#' Render an Enrollment Flowchart
#'
#' Computes counts from the pipeline, lays out nodes, and draws an
#' EQUATOR-style enrollment diagram. This is the primary rendering
#' function for interactive use; for saving to file with auto-sized
#' dimensions, see \code{\link{autoflow}}.
#'
#' @param .flow A \code{selecta} object created by \code{\link{enroll}} or
#'   \code{\link{sources}} and populated with pipeline steps.
#' @param engine Character. Rendering engine: \code{"grid"} (default) for
#'   base grid graphics, or \code{"dot"} to return a Graphviz DOT string
#'   (for use with \pkg{DiagrammeR}).
#' @param count_first Logical. If \code{TRUE}, side-box labels are rendered
#'   as \code{"214  Discontinued"} (bold count before label) rather than the
#'   default \code{"Discontinued (n = 214)"}. Applies to all box types.
#'   Default \code{FALSE}.
#' @param ... Additional arguments passed to the rendering function
#'   (\emph{e.g.,} \code{cex}, \code{cex_side}, \code{cex_phase},
#'   \code{box_fill}, \code{phase_fill}, \code{vpad}, \code{margin}).
#'
#' @return For \code{engine = "grid"}: invisibly returns the computed graph
#'   structure (a list of \code{nodes}, \code{edges}, and \code{phases}
#'   data.tables). For \code{engine = "dot"}: returns a DOT-language string.
#'
#' @seealso \code{\link{autoflow}} for saving to file,
#'   \code{\link{suggest_size}} for dimension recommendations,
#'   \code{\link{plot.selecta}} for S3 plot method
#'
#' @examples
#' # Minimal CONSORT diagram
#' enroll(n = 500) |>
#'   exclude("Age < 18", n = 45) |>
#'   exclude("Missing consent", n = 12) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(220, 223)) |>
#'   exclude("Lost to follow-up", n = c(10, 15)) |>
#'   endpoint("Final Analysis") |>
#'   flowchart()
#'
#' # With phases and sub-reasons
#' enroll(n = 1200) |>
#'   phase("Enrollment") |>
#'   exclude("Excluded", n = 150,
#'     reasons = c("Progressive disease" = 55,
#'                 "Declined" = 48,
#'                 "Unresectable" = 47)) |>
#'   phase("Allocation") |>
#'   allocate(labels = c("Neoadjuvant", "Upfront surgery"),
#'       n = c(520, 530)) |>
#'   phase("Analysis") |>
#'   endpoint("Final Analysis") |>
#'   flowchart()
#'
#' @export
flowchart <- function(.flow, engine = c("grid", "dot"),
                      count_first = FALSE, ...) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    engine <- match.arg(engine)
    graph  <- compute(.flow)
    graph  <- layout_nodes(graph)

    if (engine == "grid") {
        draw_grid(graph, count_first = count_first, ...)
    } else {
        return(to_dot(graph))
    }

    invisible(graph)
}


#' @rdname flowchart
#' @usage \method{plot}{selecta}(x, engine = c("grid", "dot"), ...)
#' @param x A \code{selecta} object.
#' @export
plot.selecta <- function(x, engine = c("grid", "dot"), ...) {
    flowchart(x, engine = engine, ...)
}


#' Print an Enrollment Flow Summary
#'
#' Displays a concise text summary of the pipeline steps and their
#' parameters. Intended for interactive inspection of a \code{selecta}
#' object before rendering.
#'
#' @param x A \code{selecta} object.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65,
#'     reasons = c("No consent" = 30, "Under 18" = 35)) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
#'   endpoint("Analysed")
#' flow
#'
#' @export
print.selecta <- function(x, ...) {

    cat(sprintf("selecta flow (%s mode)\n", x$mode))
    cat(sprintf("  Starting N: %s\n", fmt_n(x$n_start)))
    cat(sprintf("  Steps: %d\n", length(x$steps)))

    for (i in seq_along(x$steps)) {
        s <- x$steps[[i]]

        if (s$type == "phase") {
            cat(sprintf("  --- %s ---\n", s$label))

        } else if (s$type == "sources") {
            for (grp in s$groups) {
                hdr <- if (!is.null(grp$header)) grp$header
                       else if (grp$group != "_default") grp$group
                       else NULL
                if (!is.null(hdr)) {
                    cat(sprintf("  [%d] sources (%s):\n", i, hdr))
                } else {
                    cat(sprintf("  [%d] sources:\n", i))
                }
                for (j in seq_along(grp$labels)) {
                    cat(sprintf("         %s (n = %s)\n",
                                grp$labels[j], fmt_n(grp$counts[j])))
                }
            }

        } else if (s$type == "combine") {
            n_text <- if (!is.null(s$n)) sprintf(" (n = %s)", fmt_n(s$n)) else ""
            cat(sprintf("  [%d] combine: \"%s\"%s\n", i, s$label, n_text))

        } else if (s$type == "exclude") {
            n_text <- if (!is.null(s$n)) sprintf(" (n = %s)", fmt_n(sum(s$n))) else ""
            lbl_text <- if (length(s$label) > 1L) {
                paste(sprintf("\"%s\"", s$label), collapse = " / ")
            } else {
                sprintf("\"%s\"", s$label)
            }
            cat(sprintf("  [%d] exclude: %s%s\n", i, lbl_text, n_text))
            ## Print sub-reasons
            if (!is.null(s$reasons) && !is.list(s$reasons)) {
                reason_text <- sprintf("         \u2022 %s = %s",
                                       names(s$reasons), fmt_n(s$reasons))
                cat(reason_text, sep = "\n")
                cat("\n")
            }

        } else if (s$type == "stratify") {
            labs <- if (!is.null(s$labels)) paste(s$labels, collapse = ", ") else s$variable
            cat(sprintf("  [%d] %s: %s\n", i, s$label, labs))

        } else if (s$type == "classify") {
            cat(sprintf("  [%d] classify: %d x %d grid\n",
                        i, length(s$rows), length(s$cols)))
            cat(sprintf("         rows: %s\n", paste(s$rows, collapse = ", ")))
            cat(sprintf("         cols: %s\n", paste(s$cols, collapse = ", ")))

        } else if (s$type == "endpoint") {
            cat(sprintf("  [%d] endpoint: \"%s\"\n", i, s$label))
            if (!is.null(s$reasons)) {
                if (is.list(s$reasons)) {
                    for (ai in seq_along(s$reasons)) {
                        cat(sprintf("       Arm %d:\n", ai))
                        reason_text <- sprintf("         \u2022 %s = %s",
                                               names(s$reasons[[ai]]),
                                               fmt_n(s$reasons[[ai]]))
                        cat(reason_text, sep = "\n")
                        cat("\n")
                    }
                } else {
                    reason_text <- sprintf("         \u2022 %s = %s",
                                           names(s$reasons), fmt_n(s$reasons))
                    cat(reason_text, sep = "\n")
                    cat("\n")
                }
            }
        }
    }

    invisible(x)
}


#' Summarize an Enrollment Flow
#'
#' Computes all counts from the pipeline and returns a \code{data.table}
#' summarising each node in the diagram.
#'
#' @param object A \code{selecta} object.
#' @param ... Ignored.
#'
#' @return A \code{data.table} with columns \code{phase}, \code{role},
#'   \code{arm}, \code{text}, and \code{n}. Each row corresponds to one
#'   node in the computed diagram.
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
#'   endpoint("Analysed")
#' summary(flow)
#'
#' @export
summary.selecta <- function(object, ...) {
    graph <- compute(object)
    out   <- graph$nodes[, .(phase, role, arm_id, text, n)]
    setnames(out, "arm_id", "arm")
    out[]
}


#' Suggest Figure Dimensions
#'
#' Computes recommended width and height in inches based on diagram
#' content. A throwaway PDF device is opened to obtain accurate text
#' measurements, then closed immediately.
#'
#' @param x A \code{selecta} object.
#' @param vpad Numeric. Vertical spacing between elements in inches.
#'   Default 0.25.
#' @param pad Numeric. Internal padding within boxes in inches.
#'   Default 0.08.
#' @param line_height Numeric. Vertical line spacing in inches.
#'   Default 0.20.
#' @param count_first Logical. If \code{TRUE}, measure using the
#'   count-first label layout. Default \code{FALSE}.
#' @param cex Numeric. Font size multiplier for main text. Default 0.85.
#' @param cex_side Numeric. Font size multiplier for side box text.
#'   Defaults to the value of \code{cex}.
#' @param cex_phase Numeric. Font size multiplier for phase labels.
#'   Default 0.9.
#' @param phase_width Numeric. Width of phase label boxes in inches.
#'   Default 0.22.
#' @param margin Numeric. Fixed margin on all four sides in inches.
#'   Default 0.25.
#' @param .return_graph Logical. If \code{TRUE}, attach the pre-computed
#'   graph as an attribute for reuse by \code{\link{autoflow}}.
#'   Default \code{FALSE}. Internal use only.
#'
#' @return A named numeric vector with elements \code{width} and
#'   \code{height} (in inches), rounded up to the nearest tenth.
#'
#' @seealso \code{\link{autoflow}} for saving to file,
#'   \code{\link{flowchart}} for interactive rendering
#'
#' @export
suggest_size <- function(x, vpad = 0.25, pad = 0.08, line_height = 0.20,
                         count_first = FALSE, cex = 0.85, cex_side = NULL,
                         cex_phase = 0.9, phase_width = 0.22, margin = 0.25,
                         .return_graph = FALSE) {

    if (!inherits(x, "selecta"))
        stop("'x' must be a selecta object", call. = FALSE)

    if (is.null(cex_side)) cex_side <- cex

    graph <- compute(x)

    ## Run draw_grid on throwaway device for exact dimensions
    graph_full <- layout_nodes(graph)
    tf_h <- tempfile(fileext = ".pdf")
    grDevices::pdf(tf_h, width = 10, height = 10)
    draw_args <- list(graph = graph_full, newpage = TRUE,
                      vpad = vpad, pad = pad, line_height = line_height,
                      count_first = count_first, cex = cex, cex_side = cex_side,
                      cex_phase = cex_phase, phase_width = phase_width,
                      margin = margin)
    g <- do.call(draw_grid, draw_args)
    grDevices::dev.off()
    unlink(tf_h)

    h <- g$diagram_height_in
    if (is.null(h) || is.na(h)) h <- 8.0

    w <- g$diagram_width_in
    if (is.null(w) || is.na(w)) w <- 6.0

    result <- c(width = ceiling(w * 10) / 10, height = ceiling(h * 10) / 10)
    if (isTRUE(.return_graph)) attr(result, "graph") <- graph_full
    result
}


#' Save Diagram to File
#'
#' Renders the enrollment diagram and saves it to a file. Supported
#' formats are PDF, PNG, SVG, and TIFF (inferred from the file
#' extension). Dimensions are computed automatically from diagram content
#' via \code{\link{suggest_size}} unless overridden.
#'
#' @param x A \code{selecta} object.
#' @param file Character string. Output file path. The format is inferred
#'   from the file extension.
#' @param width Numeric or \code{NULL}. Width in inches. If \code{NULL}
#'   (default), computed automatically.
#' @param height Numeric or \code{NULL}. Height in inches. If \code{NULL}
#'   (default), computed automatically.
#' @param res Integer. Resolution in DPI for raster formats (PNG, TIFF).
#'   Default 300.
#' @param ... Additional arguments passed to \code{\link{draw_grid}}
#'   (\emph{e.g.,} \code{cex}, \code{cex_side}, \code{count_first},
#'   \code{box_fill}, \code{vpad}, \code{margin}).
#'
#' @return Invisibly returns the output file path.
#'
#' @seealso \code{\link{flowchart}} for interactive rendering,
#'   \code{\link{suggest_size}} for dimension recommendations
#'
#' @examples
#' \dontrun{
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 50) |>
#'   endpoint("Analysis")
#'
#' autoflow(flow, "consort.pdf")
#' autoflow(flow, "consort.png", width = 8, height = 10)
#' }
#'
#' @export
autoflow <- function(x, file, width = NULL, height = NULL,
                        res = 300, ...) {

  if (!inherits(x, "selecta"))
    stop("'x' must be a selecta object", call. = FALSE)

  dots <- list(...)
  cached_graph <- NULL

  if (is.null(width) || is.null(height)) {
    ## Forward layout parameters to suggest_size for consistent canvas sizing.
    ## Request pre-computed graph to reuse the compute() + layout_nodes() result.
    sz_args <- list(x = x, .return_graph = TRUE)
    for (p in c("vpad", "pad", "line_height", "count_first", "cex", "cex_side",
                 "cex_phase", "phase_width", "margin"))
      if (!is.null(dots[[p]])) sz_args[[p]] <- dots[[p]]
    sz <- do.call(suggest_size, sz_args)
    if (is.null(width))  width  <- sz["width"]
    if (is.null(height)) height <- sz["height"]
    cached_graph <- attr(sz, "graph")
  }

  ext <- tolower(tools::file_ext(file))

  switch(ext,
    pdf  = pdf(file, width = width, height = height),
    png  = png(file, width = width, height = height, units = "in",
               res = res, type = "cairo"),
    svg  = svg(file, width = width, height = height),
    tiff =, tif = tiff(file, width = width, height = height, units = "in",
                        res = res, type = "cairo"),
    stop(sprintf("Unsupported format: '%s'", ext), call. = FALSE)
  )
  on.exit(dev.off())

  ## Reuse cached graph (compute + layout are device-agnostic);
  ## draw_grid re-measures text on the real device
  graph <- if (!is.null(cached_graph)) cached_graph else {
    layout_nodes(compute(x))
  }
  draw_grid(graph, newpage = TRUE, ...)

  invisible(file)
}
