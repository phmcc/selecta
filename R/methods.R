#' Render an Enrollment Flowchart
#'
#' Computes counts, lays out nodes, and renders the CONSORT-style diagram.
#' This is the primary rendering function for \code{selecta} objects.
#'
#' @param .flow A \code{selecta} object.
#' @param engine Character. Rendering engine: \code{"grid"} (default) for
#'   base grid graphics, or \code{"dot"} to return a Graphviz DOT string
#'   (for use with \pkg{DiagrammeR}).
#' @param count_first Logical. If \code{TRUE}, side-box labels are rendered
#'   as \code{"214  Discontinued"} (count bolded before label) rather than
#'   the default \code{"Discontinued (n = 214)"}. Default \code{FALSE}.
#' @param ... Additional arguments passed to the rendering function (e.g.,
#'   \code{cex}, \code{cex_side}, \code{box_fill}, \code{phase_fill}).
#'
#' @return For \code{"grid"}: invisibly returns the computed graph structure.
#'   For \code{"dot"}: returns a DOT string.
#'
#' @examples
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
#' @param x A \code{selecta} object.
#' @param ... Ignored.
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

        } else if (s$type == "exclude") {
            n_text <- if (!is.null(s$n)) sprintf(" (n = %s)", fmt_n(sum(s$n))) else ""
            cat(sprintf("  [%d] exclude: \"%s\"%s\n", i, s$label, n_text))
            ## Print sub-reasons (vectorized formatting)
            if (!is.null(s$reasons) && !is.list(s$reasons)) {
                reason_text <- sprintf("         \u2022 %s = %s",
                                       names(s$reasons), fmt_n(s$reasons))
                cat(reason_text, sep = "\n")
                cat("\n")
            }

        } else if (s$type == "allocate") {
            labs <- if (!is.null(s$labels)) paste(s$labels, collapse = ", ") else s$variable
            cat(sprintf("  [%d] arm: %s\n", i, labs))

        } else if (s$type == "endpoint") {
            cat(sprintf("  [%d] endpoint: \"%s\"\n", i, s$label))
        }
    }

    invisible(x)
}


#' Summarize an Enrollment Flow
#'
#' Computes all counts and returns a \code{data.table} summarizing each node.
#'
#' @param object A \code{selecta} object.
#' @param ... Ignored.
#'
#' @return A \code{data.table} with columns: \code{phase}, \code{role},
#'   \code{arm}, \code{text}, \code{n}.
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
#' Computes recommended width and height in inches based on diagram content.
#' Height is computed additively from row heights, gaps, and side box sizes.
#' Width is based on the number of arms.
#'
#' @param x A \code{selecta} object.
#' @param vpad Numeric. Vertical padding between elements in inches. Default 0.15.
#' @param pad Numeric. Padding inside boxes in inches. Default 0.08.
#' @param cex Numeric. Font size. Default 0.85.
#' @return A named numeric vector with \code{width} and \code{height}.
#'
#' @export
suggest_size <- function(x, vpad = 0.25, pad = 0.08, line_height = 0.20,
                         count_first = FALSE, cex = 0.85, cex_side = NULL,
                         margin = 0.25) {

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
                      margin = margin)
    g <- do.call(draw_grid, draw_args)
    grDevices::dev.off()
    unlink(tf_h)

    h <- g$diagram_height_in
    if (is.null(h) || is.na(h)) h <- 8.0

    w <- g$diagram_width_in
    if (is.null(w) || is.na(w)) w <- 6.0

    c(width = ceiling(w * 10) / 10, height = ceiling(h * 10) / 10)
}


#' Save Diagram to File
#'
#' Renders the enrollment diagram and saves to a file (PNG, PDF, SVG, TIFF).
#' Dimensions are auto-computed from diagram content unless overridden.
#'
#' @param x A \code{selecta} object.
#' @param file Character. Output file path. Format inferred from extension.
#' @param width Numeric or \code{NULL}. Width in inches.
#' @param height Numeric or \code{NULL}. Height in inches.
#' @param res Integer. Resolution in DPI (raster formats). Default 300.
#' @param ... Additional arguments passed to the drawing function
#'   (e.g., \code{cex}, \code{cex_side}, \code{count_first}, \code{vpad},
#'   \code{pad}).
#'
#' @examples
#' \dontrun{
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 50) |>
#'   endpoint("Analysis")
#'
#' autodiagram(flow, "consort.pdf")
#' autodiagram(flow, "consort.png", width = 8, height = 10)
#' }
#'
#' @export
autodiagram <- function(x, file, width = NULL, height = NULL,
                        res = 300, ...) {

  if (!inherits(x, "selecta"))
    stop("'x' must be a selecta object", call. = FALSE)

  dots <- list(...)

  if (is.null(width) || is.null(height)) {
    ## Forward layout params to suggest_size so canvas matches rendering
    sz_args <- list(x = x)
    for (p in c("vpad", "pad", "line_height", "count_first", "cex", "cex_side", "margin"))
      if (!is.null(dots[[p]])) sz_args[[p]] <- dots[[p]]
    sz <- do.call(suggest_size, sz_args)
    if (is.null(width))  width  <- sz["width"]
    if (is.null(height)) height <- sz["height"]
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

  graph <- compute(x)
  graph <- layout_nodes(graph)
  draw_grid(graph, newpage = TRUE, ...)

  invisible(file)
}
