### * Main functions

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
#' @details
#' \code{flowchart()} is the primary rendering entry point and accepts a
#' completed pipeline object. The \code{"grid"} engine draws the diagram to
#' the active graphics device using the \pkg{grid} system and is intended
#' for publication-quality figures with phase strips, precise dimensions,
#' and locale-aware counts; the \code{"dot"} engine instead returns a
#' Graphviz DOT-language string for prototyping or rendering through external
#' Graphviz tooling, and draws nothing itself. Styling, font, and
#' number-format options are forwarded to the chosen engine through
#' \code{...}; options unsupported by an engine (for example the phase
#' strips, which the DOT engine does not draw) are ignored. \code{flowchart()}
#' is normally the last call in a pipeline; for direct file output use
#' \code{\link{flowsave}}, and to size a canvas use \code{\link{recdims}}.
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
#' @family flowchart output functions
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
        ## DOT engine: forward only supported options via do.call with a
        ## list of present arguments, so to_dot()'s defaults stay authoritative
        ## unless explicitly overridden.
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
#' @details
#' The \code{print} method gives a compact, text-only view of a
#' \code{selecta} object for interactive inspection before rendering. It
#' lists the operating mode, the starting count, and each pipeline step with
#' its key parameters (exclusion reasons, arm labels, endpoint sub-items),
#' and marks phase boundaries with a \dQuote{--- label ---} banner. It does
#' not draw the diagram or open a graphics device; for that use
#' \code{\link{flowchart}} or \code{\link{flowsave}}.
#'
#' @seealso \code{\link{summary.selecta}} for a tabular per-node summary,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65,
#'     reasons = c("No consent" = 30, "Under 18" = 35)) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
#'   endpoint("Analyzed")
#' flow
#'
#' @family flowchart output functions
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
#' @details
#' The \code{summary} method runs the same count computation that underlies
#' rendering and returns the result as a tidy \code{data.table}, one row per
#' node, rather than drawing anything. This is convenient for programmatic
#' checks (confirming arm totals, extracting the final analyzed count) and
#' for embedding flow figures in tables or reports. The returned object is a
#' plain \code{data.table} and may be filtered or joined like any other. For
#' a human-readable console view use \code{\link{print.selecta}}; to render
#' the diagram use \code{\link{flowchart}}.
#'
#' @seealso \code{\link{print.selecta}} for a console summary,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
#'   endpoint("Analyzed")
#' summary(flow)
#'
#' @family flowchart output functions
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
#' @param phase_multiline Logical. If \code{TRUE} (the default), long phase
#'   labels wrap across stacked lines to fit their band; must match the
#'   draw-time value for accurate dimensions. Default \code{TRUE}.
#' @param phase_max_lines Integer. Maximum wrapped lines per phase label
#'   when wrapping is active. Default 3.
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
#' @param .measure_dev Optional zero-argument function that opens a graphics
#'   device for text measurement, matching the device that will render the
#'   diagram. When \code{NULL} (the default) a pdf device is used. Advanced
#'   use only; see Details.
#' @param .return_graph Logical. If \code{TRUE}, attaches the pre-computed
#'   graph as an attribute for reuse by \code{\link{flowsave}}.
#'   Default \code{FALSE}. Internal use only.
#'
#' @return A named numeric vector with elements \code{width} and
#'   \code{height} (in inches), rounded up to the nearest tenth.
#'
#' @details
#' \code{recdims()} computes the canvas size a flow needs at a given
#' typography and layout, so the figure is neither clipped nor surrounded by
#' excess whitespace. It lays the diagram out and measures it on a throwaway
#' graphics device, returning width and height in inches without drawing
#' anything visible. Because text metrics are font- and device-dependent,
#' any sizing parameter passed here (\code{cex}, \code{font_family},
#' \code{phase_multiline}, \code{number_format}, and so on) should match the
#' values used at render time; styling-only parameters are ignored so the
#' same call can be shared across \code{recdims()}, \code{\link{flowchart}},
#' and \code{\link{flowsave}}. The advanced \code{.measure_dev} argument
#' supplies a custom device opener when measurement must match a non-default
#' device. \code{\link{flowsave}} calls \code{recdims()} internally when
#' \code{width} or \code{height} is left unspecified, so explicit use is
#' only needed when the dimensions themselves are wanted.
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
#' @family flowchart output functions
#' @export
recdims <- function(x, vpad = getOption("selecta.vpad", 0.25),
                    pad = 0.08, line_height = 0.20,
                    count_first = FALSE, cex = 0.85, cex_side = NULL,
                    cex_phase = 0.9, phase_width = 0.22, margin = 0.25,
                    phase_multiline = TRUE, phase_max_lines = 3L,
                    font_family = "Helvetica",
                    number_format = NULL,
                    ...,
                    .measure_dev = NULL, .return_graph = FALSE) {

    if (!inherits(x, "selecta"))
        stop("'x' must be a selecta object", call. = FALSE)

    if (is.null(cex_side)) cex_side <- cex

    graph <- compute(x)

    ## Run draw_grid on a throwaway device for exact dimensions. Styling
    ## arguments in `...` are discarded: colors do not affect text width or
    ## layout, so forwarding them would risk argument-mismatch errors. Phase
    ## wrapping IS forwarded, as it changes strip width and label height.
    ##
    ## Text metrics are device-dependent, so the measuring device should match
    ## the device that will ultimately render the diagram. By default a pdf
    ## device is used (its metrics match the cairo raster and pdf devices that
    ## flowsave() employs). A caller rendering on another device -- for
    ## example the ragg device used in the package vignettes -- may pass
    ## \code{.measure_dev}, a zero-argument function that opens a matching
    ## device, so that non-default fonts are measured consistently.
    graph_full <- layout_nodes(graph)
    if (is.null(.measure_dev)) {
        tf_h <- tempfile(fileext = ".pdf")
        grDevices::pdf(tf_h, width = 10, height = 10)
    } else {
        tf_h <- .measure_dev()
    }
    draw_args <- list(graph = graph_full, newpage = TRUE,
                      vpad = vpad, pad = pad, line_height = line_height,
                      count_first = count_first, cex = cex, cex_side = cex_side,
                      cex_phase = cex_phase, phase_width = phase_width,
                      phase_multiline = phase_multiline,
                      phase_max_lines = phase_max_lines,
                      margin = margin, font_family = font_family,
                      number_format = number_format,
                      measure_only = TRUE)
    g <- do.call(draw_grid, draw_args)
    grDevices::dev.off()
    if (!is.null(tf_h) && file.exists(tf_h)) unlink(tf_h)

    h <- g$diagram_height_in
    if (is.null(h) || is.na(h)) h <- 8.0

    w <- g$diagram_width_in
    if (is.null(w) || is.na(w)) w <- 6.0

    result <- c(width = ceiling(w * 10) / 10, height = ceiling(h * 10) / 10)

    ## Optional debug: recommended canvas dimensions and their raw inputs.
    debug_emit("recdims() dimensions",
               raw_width_in = w, raw_height_in = h,
               phase_strip_w_in = g$phase_strip_w %||% NA_real_,
               width_in = unname(result["width"]),
               height_in = unname(result["height"]))

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
#' For the grid engine, the vector formats (PDF, SVG) use R's standard
#' devices and are recommended where a vector figure is acceptable, as
#' their font model renders the italic \code{n} and \code{N} of the count
#' lines reliably. The raster formats (PNG, TIFF) are produced with the
#' \pkg{ragg} device when it is installed---the same device the package
#' vignettes use---and fall back to the base \code{png()}/\code{tiff()}
#' devices otherwise; installing \pkg{ragg} is advised for raster output,
#' since some cairo-based device configurations drop the plotmath italics.
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
#' @details
#' \code{flowsave()} renders a flow directly to a file, inferring the format
#' from the extension and choosing dimensions automatically unless
#' \code{width} and \code{height} are given. With \code{engine = "grid"} it
#' draws through R's graphics devices (vector \code{.pdf}/\code{.svg} or
#' raster \code{.png}/\code{.tiff}); raster output prefers the \pkg{ragg}
#' device when installed, since some cairo configurations drop the plotmath
#' italics in the count labels. With \code{engine = "dot"} it emits Graphviz
#' DOT: a \code{.dot} extension writes the source text directly and needs no
#' external software, whereas image output shells out to the system
#' \code{dot} binary and therefore requires Graphviz on the \code{PATH}.
#' When sizing automatically, \code{flowsave()} calls \code{\link{recdims}}
#' once and reuses the computed layout, so a separate \code{recdims()} call
#' is unnecessary. The \code{dpi} argument mirrors \code{ggplot2::ggsave()}
#' for raster resolution.
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
#' # the example respects CRAN's no-write policy; in practice any
#' # desired path may be supplied.
#' flowsave(flow, file.path(tempdir(), "consort.pdf"))
#' flowsave(flow, file.path(tempdir(), "consort.png"),
#'          width = 8, height = 10)
#' }
#'
#' \donttest{
#' # DOT engine writing a .dot source file requires no external software.
#' flowsave(flow, file.path(tempdir(), "consort.dot"), engine = "dot")
#'
#' # Rasterized DOT output (.svg, .png, .pdf) requires the Graphviz 'dot'
#' # binary on the system PATH, so guard on its availability.
#' if (nzchar(Sys.which("dot"))) {
#'   flowsave(flow, file.path(tempdir(), "consort.svg"), engine = "dot")
#'
#'   # DOT engine with Times typography for serif environments.
#'   flowsave(flow, file.path(tempdir(), "consort_times.svg"), engine = "dot",
#'            font_family = "Times-Roman",
#'            sans_serif  = FALSE)
#' }
#' }
#'
#' @family flowchart output functions
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
    ## Forward layout parameters to recdims for consistent canvas sizing,
    ## requesting the pre-computed graph to reuse compute() + layout_nodes().
    sz_args <- list(x = x, .return_graph = TRUE)
    for (p in c("vpad", "pad", "line_height", "count_first", "cex", "cex_side",
                 "cex_phase", "phase_width", "phase_multiline", "phase_max_lines",
                 "margin", "font_family", "number_format"))
      if (!is.null(dots[[p]])) sz_args[[p]] <- dots[[p]]
    sz <- do.call(recdims, sz_args)
    if (is.null(width))  width  <- sz["width"]
    if (is.null(height)) height <- sz["height"]
    cached_graph <- attr(sz, "graph")
  }

  ## Device selection. The pdf and svg devices use R's standard PostScript /
  ## PDF font model, under which plotmath faces -- including the italic "n"
  ## and "N" in the count lines -- render reliably. For the raster formats,
  ## the cairo png/tiff devices can fail to apply the plotmath italic face to
  ## a named font family, dropping the italics; the ragg device (already a
  ## suggested dependency, and the device the vignettes render on) does not
  ## have this problem. Raster output therefore prefers ragg when it is
  ## installed and falls back to the base device otherwise. No cairo type is
  ## forced, so the base fallback honours the session's standard bitmap type.
  has_ragg <- requireNamespace("ragg", quietly = TRUE)
  switch(ext,
    pdf  = pdf(file, width = width, height = height),
    svg  = svg(file, width = width, height = height),
    png  = if (has_ragg) {
             ragg::agg_png(file, width = width, height = height,
                           units = "in", res = dpi)
           } else {
             png(file, width = width, height = height, units = "in", res = dpi)
           },
    tiff =, tif = if (has_ragg) {
             ragg::agg_tiff(file, width = width, height = height,
                            units = "in", res = dpi)
           } else {
             tiff(file, width = width, height = height, units = "in", res = dpi)
           },
    stop(sprintf("Unsupported format: '%s'", ext), call. = FALSE)
  )
  on.exit(dev.off())

  ## Reuse cached graph (compute + layout are device-agnostic); draw_grid
  ## re-measures text on the real device.
  graph <- if (!is.null(cached_graph)) cached_graph else {
    layout_nodes(compute(x))
  }
  draw_grid(graph, newpage = TRUE, ...)

  invisible(file)
}


#' @keywords internal
.flowsave_dot <- function(x, file, ext, dpi = 300, sans_serif = TRUE, ...) {

  ## Generate DOT source via the standard flowchart() dispatch so all
  ## formatting and color parameters are forwarded uniformly.
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

  ## Map file extensions to Graphviz output formats
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

  ## Sans-serif font substitution for SVG output. Graphviz emits a single
  ## face (Helvetica, or Times-Roman if requested); this expands it to a
  ## cross-platform fallback chain resolving to the native sans-serif
  ## (Helvetica on macOS, Arial on Windows, Liberation/DejaVu Sans on
  ## Linux). PDF and raster outputs bake the font at render time and are
  ## not post-processed, so their displayed font matches the layout font.
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
