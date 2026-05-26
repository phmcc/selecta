#' Render an Enrollment Flowchart
#'
#' Computes counts from the pipeline, lays out nodes, and draws an
#' EQUATOR-style enrollment diagram. This is the primary rendering
#' function for interactive use; for saving to file with auto-sized
#' dimensions, see \code{\link{flowsave}}.
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
#'   \code{box_fill}, \code{phase_fill}, \code{vpad}, \code{margin},
#'   \code{font_family}, \code{number_format}). For
#'   \code{engine = "dot"}, the supported arguments are
#'   \code{formatting}, \code{number_format}, \code{count_first},
#'   \code{ortho}, \code{font_family}, \code{padding_pt},
#'   \code{padding_adjust}, \code{box_fill}, \code{side_fill},
#'   \code{border_col}, \code{arrow_col}, \code{source_fill},
#'   \code{source_header_fill}, \code{source_header_text}. The grid
#'   engine's \code{phase_fill} and \code{phase_text_col} have no DOT
#'   equivalent because the DOT engine does not render phase labels.
#'   The DOT engine's \code{formatting} argument selects label markup:
#'   \code{"plain"} (default) emits plain DOT labels for robust,
#'   pixel-accurate centering across all fonts; \code{"rich"} emits
#'   HTML-like labels with inline bold and italic, matching the grid
#'   engine's typographic conventions at the cost of small residual
#'   centering drift on non-Helvetica fonts. The DOT engine's
#'   \code{font_family} defaults to \code{"Helvetica"} and
#'   \code{padding_pt} to 14 (uniform horizontal padding in points
#'   around each label).
#'
#' @return For \code{engine = "grid"}: invisibly returns the computed graph
#'   structure (a list of \code{nodes}, \code{edges}, and \code{phases}
#'   data.tables). For \code{engine = "dot"}: returns a DOT-language string.
#'
#' @seealso \code{\link{flowsave}} for saving to file,
#'   \code{\link{recdims}} for dimension recommendations,
#'   \code{\link{plot.selecta}} for S3 plot method
#'
#' @examples
#' # Build a flow once, then render it. Most of the package's pipeline
#' # functions are modular and intended to be composed like this rather
#' # than run in isolation; see the vignettes for fuller treatments.
#' flow <- enroll(n = 1200) |>
#'   phase("Enrollment") |>
#'   exclude("Excluded", n = 150,
#'     reasons = c("Did not meet criteria" = 55,
#'                 "Declined to participate" = 48,
#'                 "Other reasons" = 47)) |>
#'   phase("Allocation") |>
#'   allocate(labels = c("Treatment", "Control"),
#'            n = c(520, 530)) |>
#'   phase("Analysis") |>
#'   endpoint("Final Analysis")
#'
#' # The "dot" engine returns a Graphviz DOT string and draws nothing,
#' # so it runs anywhere without opening a graphics device.
#' dot <- flowchart(flow, engine = "dot")
#' substr(dot, 1, 50)
#'
#' \donttest{
#' # The "grid" engine draws to the active graphics device.
#' flowchart(flow)
#'
#' # plot() is a thin wrapper around flowchart().
#' plot(flow)
#'
#' # Locale-aware counts: a European thousands separator.
#' enroll(n = 12500) |>
#'   exclude("Excluded", n = 1450) |>
#'   endpoint("Analyzed") |>
#'   flowchart(number_format = "eu")
#' }
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
        ## DOT engine: forward only its supported options. We use do.call
        ## with a list of present arguments so to_dot()'s defaults remain
        ## authoritative when the user does not explicitly override them.
        dots <- list(...)
        td_args <- list(graph         = graph,
                        number_format = dots$number_format,
                        count_first   = count_first,
                        ortho         = isTRUE(dots$ortho))
        for (p in c("formatting", "font_family",
                    "padding_pt", "padding_adjust",
                    "box_fill", "side_fill", "border_col", "arrow_col",
                    "source_fill", "source_header_fill",
                    "source_header_text"))
          if (!is.null(dots[[p]])) td_args[[p]] <- dots[[p]]
        return(do.call(to_dot, td_args))
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
#'   endpoint("Analyzed")
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
            if (!is.null(s$sublabel))
                cat(sprintf("         \"%s\"\n", s$sublabel))

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
            cat(sprintf("  [%d] stratify: %s\n", i, labs))
            if (!is.null(s$label))
                cat(sprintf("         label: \"%s\"\n", s$label))

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
#' summarizing each node in the diagram.
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
#'   endpoint("Analyzed")
#' summary(flow)
#'
#' @export
summary.selecta <- function(object, ...) {
    graph <- compute(object)
    out   <- graph$nodes[, .(phase, role, arm_id, text, n)]
    setnames(out, "arm_id", "arm")
    out[]
}


#' Recommended Figure Dimensions
#'
#' Computes recommended width and height in inches based on diagram
#' content. A throwaway PDF device is opened to obtain accurate text
#' measurements, then closed immediately.
#'
#' @param x A \code{selecta} object.
#' @param vpad Numeric. Vertical spacing between elements in inches.
#'   Default 0.25; override globally with
#'   \code{options(selecta.vpad = 0.35)}.
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
#' @param font_family Character. Font family for text measurement.
#'   Default \code{"Helvetica"}. Must match the value used at draw time
#'   for accurate dimensions.
#' @param number_format Character string or two-element character vector.
#'   Locale-aware count formatter passed through to \code{\link{draw_grid}}
#'   for accurate text measurement. See \code{\link{flowchart}} for
#'   accepted values.
#' @param ... Additional arguments. Styling-only parameters that do not
#'   affect text measurement (such as \code{box_fill}, \code{phase_fill},
#'   \code{border_col}) are silently ignored, allowing the same call
#'   signature to be shared with \code{\link{flowchart}} and
#'   \code{\link{flowsave}}.
#' @param .return_graph Logical. If \code{TRUE}, attaches the pre-computed
#'   graph as an attribute for reuse by \code{\link{flowsave}}.
#'   Default \code{FALSE}. Internal use only.
#'
#' @return A named numeric vector with elements \code{width} and
#'   \code{height} (in inches), rounded up to the nearest tenth.
#'
#' @seealso \code{\link{flowsave}} for saving to file,
#'   \code{\link{flowchart}} for interactive rendering
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(220, 215)) |>
#'   endpoint("Analyzed")
#'
#' recdims(flow)
#'
#' @export
recdims <- function(x, vpad = getOption("selecta.vpad", 0.25),
                    pad = 0.08, line_height = 0.20,
                    count_first = FALSE, cex = 0.85, cex_side = NULL,
                    cex_phase = 0.9, phase_width = 0.22, margin = 0.25,
                    font_family = "Helvetica",
                    number_format = NULL,
                    ...,
                    .return_graph = FALSE) {

    if (!inherits(x, "selecta"))
        stop("'x' must be a selecta object", call. = FALSE)

    if (is.null(cex_side)) cex_side <- cex

    graph <- compute(x)

    ## Run draw_grid on throwaway device for exact dimensions. Styling
    ## arguments captured in `...` are intentionally discarded: fill colors,
    ## border colors, and arrow colors do not affect text width or layout
    ## geometry, so passing them through would risk argument-mismatch errors
    ## without changing the result.
    graph_full <- layout_nodes(graph)
    tf_h <- tempfile(fileext = ".pdf")
    grDevices::pdf(tf_h, width = 10, height = 10)
    draw_args <- list(graph = graph_full, newpage = TRUE,
                      vpad = vpad, pad = pad, line_height = line_height,
                      count_first = count_first, cex = cex, cex_side = cex_side,
                      cex_phase = cex_phase, phase_width = phase_width,
                      margin = margin, font_family = font_family,
                      number_format = number_format)
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
#' extension). The grid engine renders via R graphics devices; the DOT
#' engine pipes Graphviz output through the system \code{dot} binary.
#' Dimensions are computed automatically from diagram content via
#' \code{\link{recdims}} unless overridden.
#'
#' @param x A \code{selecta} object.
#' @param file Character string. Output file path. The format is inferred
#'   from the file extension. Supported extensions: \code{.pdf},
#'   \code{.png}, \code{.svg}, \code{.tif}/\code{.tiff} (all engines);
#'   \code{.dot} (DOT engine only, writes the raw DOT source).
#' @param engine Character string. One of \code{"grid"} (the default,
#'   uses R's grid graphics) or \code{"dot"} (uses the system Graphviz
#'   binary). The DOT engine requires \code{dot} to be installed and on
#'   the system \code{PATH}.
#' @param width Numeric or \code{NULL}. Width in inches. If \code{NULL}
#'   (default), computed automatically. For the DOT engine, omit to let
#'   Graphviz determine dimensions from layout.
#' @param height Numeric or \code{NULL}. Height in inches. If \code{NULL}
#'   (default), computed automatically. For the DOT engine, omit to let
#'   Graphviz determine dimensions from layout.
#' @param dpi Integer. Resolution in dots per inch for raster formats
#'   (PNG, TIFF). Default 300. Honored by both engines. Mirrors the
#'   \code{dpi} argument of \code{ggplot2::ggsave()}.
#' @param sans_serif Logical. DOT engine only. If \code{TRUE} (default),
#'   the rendered SVG/PDF text is displayed in a sans-serif fallback
#'   chain (\code{Helvetica, Arial, "Liberation Sans", "DejaVu Sans",
#'   sans-serif}) regardless of the layout font. Layout boxes are still
#'   sized using the metrics of the font set via \code{font_family}, so
#'   the result preserves all margins. Set to \code{FALSE} to retain
#'   the layout font as the displayed font.
#' @param ... Additional arguments passed to the rendering engine. For
#'   \code{engine = "grid"}: passed to \code{\link{draw_grid}} (\emph{e.g.,}
#'   \code{cex}, \code{box_fill}, \code{vpad}). For \code{engine = "dot"}:
#'   \code{formatting}, \code{count_first}, \code{number_format},
#'   \code{ortho}, \code{font_family}, \code{padding_pt},
#'   \code{padding_adjust}, and the color parameters \code{box_fill},
#'   \code{side_fill}, \code{border_col}, \code{arrow_col},
#'   \code{source_fill}, \code{source_header_fill},
#'   \code{source_header_text} are honored (see \code{\link{flowchart}}).
#'
#' @return Invisibly returns the output file path.
#'
#' @seealso \code{\link{flowchart}} for interactive rendering,
#'   \code{\link{recdims}} for dimension recommendations
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 50) |>
#'   endpoint("Analysis")
#'
#' \donttest{
#' # Grid engine (default). Files are written under tempdir() here so
#' # the example respects CRAN's no-write policy; in practice supply
#' # any path you like.
#' flowsave(flow, file.path(tempdir(), "consort.pdf"))
#' flowsave(flow, file.path(tempdir(), "consort.png"),
#'          width = 8, height = 10)
#' }
#'
#' \dontrun{
#' # DOT engine: requires the Graphviz 'dot' binary on the system PATH.
#' flowsave(flow, file.path(tempdir(), "consort.svg"), engine = "dot")
#' flowsave(flow, file.path(tempdir(), "consort.dot"), engine = "dot")
#'
#' # DOT engine with Times typography for serif environments.
#' flowsave(flow, file.path(tempdir(), "consort.svg"), engine = "dot",
#'          font_family = "Times-Roman",
#'          sans_serif  = FALSE)
#' }
#'
#' @export
flowsave <- function(x, file, engine = c("grid", "dot"),
                     width = NULL, height = NULL,
                     dpi = 300, sans_serif = TRUE, ...) {

  if (!inherits(x, "selecta"))
    stop("'x' must be a selecta object", call. = FALSE)

  engine <- match.arg(engine)
  ext    <- tolower(tools::file_ext(file))

  ## ---- DOT engine branch ----
  if (engine == "dot") {
    return(.flowsave_dot(x, file, ext, dpi = dpi,
                         sans_serif = sans_serif, ...))
  }

  ## ---- Grid engine branch (original behavior) ----
  if (ext == "dot")
    stop("'.dot' output requires engine = 'dot'", call. = FALSE)

  dots <- list(...)
  cached_graph <- NULL

  if (is.null(width) || is.null(height)) {
    ## Forward layout parameters to recdims for consistent canvas sizing.
    ## Request pre-computed graph to reuse the compute() + layout_nodes() result.
    sz_args <- list(x = x, .return_graph = TRUE)
    for (p in c("vpad", "pad", "line_height", "count_first", "cex", "cex_side",
                 "cex_phase", "phase_width", "margin", "font_family",
                 "number_format"))
      if (!is.null(dots[[p]])) sz_args[[p]] <- dots[[p]]
    sz <- do.call(recdims, sz_args)
    if (is.null(width))  width  <- sz["width"]
    if (is.null(height)) height <- sz["height"]
    cached_graph <- attr(sz, "graph")
  }

  switch(ext,
    pdf  = pdf(file, width = width, height = height),
    png  = png(file, width = width, height = height, units = "in",
               res = dpi, type = "cairo"),
    svg  = svg(file, width = width, height = height),
    tiff =, tif = tiff(file, width = width, height = height, units = "in",
                        res = dpi, type = "cairo"),
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


#' @keywords internal
.flowsave_dot <- function(x, file, ext, dpi = 300, sans_serif = TRUE, ...) {

  ## Generate DOT source via the standard flowchart() dispatch so that
  ## count_first, number_format, ortho, font_family, padding_pt, and
  ## the color parameters are forwarded uniformly.
  dot_str <- flowchart(x, engine = "dot", ...)

  ## Raw DOT source: write directly and skip the binary
  if (ext == "dot") {
    writeLines(dot_str, file)
    return(invisible(file))
  }

  if (!ext %in% c("pdf", "png", "svg", "tif", "tiff"))
    stop(sprintf("Unsupported format for DOT engine: '%s'. ",
                 "Use 'pdf', 'png', 'svg', 'tif', 'tiff', or 'dot'."),
         call. = FALSE)

  if (!nzchar(Sys.which("dot")))
    stop("engine = 'dot' requires the Graphviz 'dot' binary on the ",
         "system PATH. Install Graphviz or use engine = 'grid'.",
         call. = FALSE)

  ## Map our extensions to Graphviz output formats
  gv_fmt <- switch(ext, pdf = "pdf", png = "png", svg = "svg",
                   tif = "tif", tiff = "tif")

  dot_in <- tempfile(fileext = ".dot")
  writeLines(dot_str, dot_in)
  on.exit(unlink(dot_in), add = TRUE)

  args <- c(paste0("-T", gv_fmt))
  if (gv_fmt == "png") args <- c(args, paste0("-Gdpi=", dpi))
  args <- c(args, shQuote(dot_in), "-o", shQuote(file))
  status <- system2("dot", args, stdout = NULL, stderr = NULL)
  if (!identical(status, 0L) && !identical(status, 0))
    warning("'dot' returned non-zero status when rendering '", file, "'",
            call. = FALSE)

  ## Sans-serif font substitution for SVG output. Graphviz's emitted
  ## font-family attribute names a single face (Helvetica with default
  ## settings, or Times-Roman if requested via font_family); we expand
  ## that to a cross-platform fallback chain so the displayed face
  ## resolves to the platform's native sans-serif (Helvetica on macOS,
  ## Arial on Windows, Liberation Sans / DejaVu Sans on Linux). PDF and
  ## raster outputs bake the font choice into the file at render time
  ## and cannot be post-processed here; for those formats the displayed
  ## font matches the layout font.
  if (isTRUE(sans_serif) && gv_fmt == "svg" && file.exists(file)) {
    sans_chain <- "Helvetica, Arial, 'Liberation Sans', 'DejaVu Sans', sans-serif"
    svg_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    ## Match either Helvetica- or Times-anchored attribute values, in
    ## either single- or double-quoted form, with optional CSS family
    ## fallbacks Graphviz appends (Helvetica,sans-Serif and similar).
    svg_text <- gsub('font-family="(Helvetica|Times)[^"]*"',
                     sprintf('font-family="%s"', sans_chain),
                     svg_text, perl = TRUE)
    svg_text <- gsub("font-family='(Helvetica|Times)[^']*'",
                     sprintf("font-family=\"%s\"", sans_chain),
                     svg_text, perl = TRUE)
    writeLines(svg_text, file)
  }

  invisible(file)
}
