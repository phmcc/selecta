### * Main functions

#' Render an Enrollment Flowchart
#'
#' Computes counts from the pipeline, lays out nodes, and draws an
#' EQUATOR-style enrollment diagram. This is the primary rendering
#' function for interactive use; for saving to file with auto-sized
#' dimensions, see \code{flowsave()}.
#'
#' @param .flow A \code{selecta} object created by \code{enroll()} or
#'   \code{sources()} and populated with pipeline steps.
#' @param engine Character. Rendering engine: \code{"grid"} (default) for
#'   base R graphics, or \code{"dot"} to return a Graphviz DOT string
#'   (for use with \pkg{DiagrammeR} or a locally installed executable).
#' @param count_first Logical. If \code{TRUE}, side-box labels are rendered
#'   as \code{"214  Discontinued"} (bold count before label) rather than the
#'   default \code{"Discontinued (n = 214)"}. Applies to all box types.
#'   Default \code{FALSE}, supplied by the drawing routine rather than
#'   restated here, so this argument shows as \code{NULL} in the usage.
#' @param ... Additional styling and formatting arguments forwarded to the
#'   selected engine; arguments an engine does not recognize are ignored.
#'
#'   For \code{engine = "grid"}:
#'   \describe{
#'     \item{cex, cex_side, cex_phase}{Font-size multipliers for the main,
#'       side-box, and phase text}
#'     \item{box_fill, phase_fill}{Fill colors for boxes and phase strips}
#'     \item{vpad, margin}{Vertical spacing between elements and the outer
#'       margin, in inches}
#'     \item{font_family}{Font family for text}
#'     \item{number_format}{Locale-aware count formatter}
#'   }
#'   For \code{engine = "dot"}:
#'   \describe{
#'     \item{formatting}{Label markup: \code{"plain"} (default) for robust,
#'       pixel-accurate centering across all fonts, or \code{"rich"} for
#'       HTML-like inline bold and italic that match the \code{grid} engine's
#'       typography at the cost of small centering drift on non-Helvetica
#'       fonts}
#'     \item{bullets}{Prefix side-box sub-reasons with a bullet; defaults on
#'       under \code{"plain"} (where indentation alone is a weak cue) and off
#'       under \code{"rich"}}
#'     \item{font_family, padding_pt, padding_adjust}{Font family (default
#'       \code{"Helvetica"}) and the uniform horizontal label padding in
#'       points (default 14) with its fine adjustment}
#'     \item{ortho}{Use orthogonal (right-angled) edge routing}
#'     \item{box_fill, side_fill, border_col, arrow_col}{Box, side-box,
#'       border, and arrow colors}
#'     \item{source_fill, source_header_fill, source_header_text}{Source-box
#'       fill, header fill, and header text color}
#'     \item{phase_labels, phase_fill, phase_text_col}{Toggle and color the
#'       phase-band labels (on by default when the flow defines phases; the
#'       \code{dot} engine draws them as horizontal left-margin bands rather
#'       than the \code{grid} engine's vertical strips)}
#'     \item{rank_sep, node_sep}{Spacing of ranks and nodes, in inches}
#'     \item{number_format}{Locale-aware count formatter, shared with the
#'       \code{grid} engine}
#'   }
#'
#' @return For \code{engine = "grid"}: invisibly returns the computed graph
#'   structure (a list of \code{nodes}, \code{edges}, and \code{phases}
#'   data.tables). For \code{engine = "dot"}: returns a DOT-language string.
#'
#' @details
#' \code{flowchart()} is the primary rendering entry point and accepts a
#' completed pipeline object. The \code{grid} engine draws the diagram to
#' the active graphics device using the \pkg{grid} system and is intended
#' for publication-quality figures with phase strips, precise dimensions,
#' and locale-aware counts; the \code{dot} engine instead returns a
#' Graphviz DOT-language string for prototyping or rendering through external
#' Graphviz tooling, and draws nothing itself. Styling, font, and
#' number-format options are forwarded to the chosen engine through
#' \code{...}; options unsupported by an engine (for example the phase
#' strips, which the \code{dot} engine does not draw) are ignored. \code{flowchart()}
#' is normally the last call in a pipeline; for direct file output use
#' \code{flowsave()}, and to size a canvas use \code{recdims}.
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
#' # The "grid" engine draws to the active graphics device. These calls are
#' # guarded with interactive() so they render in an interactive session but
#' # are skipped during non-interactive documentation builds, where the
#' # diagram cannot be sized to the page and would render incorrectly.
#' if (interactive()) {
#'   flowchart(flow)            # draws to the active device
#'   plot(flow)                 # plot() is a thin wrapper around flowchart()
#'
#'   # Locale-aware counts: a European thousands separator.
#'   enroll(n = 12500) |>
#'     exclude("Excluded", n = 1450) |>
#'     endpoint("Analyzed") |>
#'     flowchart(number_format = "eu")
#' }
#'
#' @family flowchart output functions
#' @export
flowchart <- function(.flow, engine = c("grid", "dot"),
                      count_first = NULL, ...) {

    if (!inherits(.flow, "selecta"))
        stop("'.flow' must be a selecta object", call. = FALSE)

    engine <- match.arg(engine)
    dots   <- list(...)
    ## Fail fast on an invalid number_format before any computation or drawing.
    validate_number_format(dots$number_format)
    graph  <- compute(.flow)
    graph  <- layout_nodes(graph)

    if (engine == "grid") {
        ## Forwarded only when supplied, so export_grid()'s default stays
        ## authoritative rather than being restated here. A plain call rather
        ## than do.call keeps the graph out of deparsed error messages.
        if (is.null(count_first)) export_grid(graph, ...)
        else                      export_grid(graph, count_first = count_first, ...)
    } else {
        ## DOT engine: forward only supported options via do.call with a
        ## list of present arguments, so export_dot()'s defaults stay authoritative
        ## unless explicitly overridden.
        td_args <- list(graph = graph)
        if (!is.null(dots$number_format))
            td_args$number_format <- dots$number_format
        if (!is.null(count_first)) td_args$count_first <- count_first
        for (p in c("ortho", "formatting", "bullets", "font_family",
                    "padding_pt", "padding_adjust",
                    "box_fill", "side_fill", "border_col", "arrow_col",
                    "source_fill", "source_header_fill",
                    "source_header_text",
                    "phase_labels", "phase_fill", "phase_text_col",
                    "rank_sep", "node_sep"))
            if (!is.null(dots[[p]])) td_args[[p]] <- dots[[p]]
        return(do.call(export_dot, td_args))
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
#' and marks phase boundaries with a \dQuote{--- Label ---} banner. It does
#' not draw the diagram or open a graphics device; for that use
#' \code{flowchart()} or \code{flowsave()}.
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
#' rendering and returns the result as a clean \code{data.table}, one row per
#' node, rather than drawing anything. This is convenient for programmatic
#' checks (confirming arm totals, extracting the final analyzed count) and
#' for embedding flow figures in tables or reports. The returned object is a
#' plain \code{data.table} and may be filtered or joined like any other. For
#' a human-readable console view use \code{print.selecta()}; to render
#' the diagram use \code{flowchart()}.
#'
#' @seealso \code{\link{print.selecta}} for a console summary,
#'   \code{\link{flowchart}} for rendering
#'
#' @examples
#' \dontshow{.old <- options(selecta.debug_layout = FALSE)}
#' flow <- enroll(n = 500) |>
#'   exclude("Ineligible", n = 65) |>
#'   allocate(labels = c("Drug A", "Placebo"), n = c(218, 217)) |>
#'   endpoint("Analyzed")
#' summary(flow)
#' \dontshow{options(.old)}
#'
#' @family flowchart output functions
#' @export
summary.selecta <- function(object, ...) {
    graph <- compute(object)
    out   <- graph$nodes[, .(phase, role, arm_id, text, n)]
    setnames(out, "arm_id", "arm")
    out[]
}


## The subset of export_grid() parameters that affect text measurement and
## must therefore hold identical values when a diagram is measured and when it
## is drawn. Named once here and consumed by recdims(), which forwards them,
## and by flowsave(), which collects them from its dots. The default values
## themselves are not restated: they live only in export_grid()'s formals, so
## measurement and rendering cannot drift apart. Styling-only parameters are
## deliberately absent, as they do not change any dimension.
.measure_params <- c("vpad", "pad", "line_height", "count_first",
                     "cex", "cex_side", "cex_phase", "phase_width",
                     "margin", "phase_multiline", "phase_max_lines",
                     "font_family", "number_format")


#' Recommended Figure Dimensions
#'
#' Computes recommended width and height in inches based on diagram
#' content. A throwaway graphics device is opened to obtain accurate
#' text measurements, then closed immediately.
#'
#' @inheritParams export_grid
#' @param x A \code{selecta} object.
#' @param units Character string giving the units the dimensions are
#'   returned in: \code{"in"} (inches, the default), \code{"cm"}, or
#'   \code{"mm"}.
#' @param ... Additional arguments. Styling-only parameters that do not
#'   affect text measurement (such as \code{box_fill}, \code{phase_fill},
#'   \code{border_col}) are silently ignored, allowing the same call
#'   signature to be shared with \code{flowchart()} and
#'   \code{flowsave()}.
#' @param .measure_dev Optional zero-argument function that opens a graphics
#'   device for text measurement, matching the device that will render the
#'   diagram. When \code{NULL} (the default) a pdf device is used. Advanced
#'   use only; see Details.
#' @param .return_graph Logical. If \code{TRUE}, attaches the pre-computed
#'   graph as an attribute for reuse by \code{flowsave()}.
#'   Default \code{FALSE}. Internal use only.
#'
#' @return A named numeric vector with elements \code{width} and
#'   \code{height}, expressed in \code{units} and rounded up to the nearest
#'   tenth. The units are recorded on the result as a \code{"units"}
#'   attribute, so a value carried between functions remains
#'   self-describing.
#'
#' @details
#' \code{recdims()} computes the canvas size a flow needs at a given
#' typography and layout, so the figure is neither clipped nor surrounded by
#' excess whitespace. It lays the diagram out and measures it on a throwaway
#' graphics device, returning width and height without drawing anything
#' visible. Measurement is performed in inches and the result converted to
#' \code{units}, with the rounding applied after conversion so the returned
#' tenth is a tenth of the reported unit. Because text metrics are font- and
#' device-dependent, any sizing parameter passed here (\code{cex},
#' \code{font_family}, \code{phase_multiline}, \code{number_format}, and so
#' on) should match the values used at render time; styling-only parameters
#' are ignored so the same call can be shared across \code{recdims()},
#' \code{flowchart()}, and \code{flowsave()}. A parameter left unspecified
#' is not defaulted here but forwarded unset, so it is measured at exactly
#' the value the drawing routine will apply. The advanced
#' \code{.measure_dev} argument supplies a custom device opener when
#' measurement must match a non-default device.
#' \code{flowsave()} calls \code{recdims()} internally when
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
#' # Journals commonly specify figure widths in millimeters.
#' recdims(flow, units = "mm")
#'
#' @family flowchart output functions
#' @export
recdims <- function(x,
                    vpad = NULL, pad = NULL, line_height = NULL,
                    count_first = NULL, cex = NULL, cex_side = NULL,
                    cex_phase = NULL, phase_width = NULL, margin = NULL,
                    phase_multiline = NULL, phase_max_lines = NULL,
                    font_family = NULL,
                    number_format = NULL,
                    units = c("in", "cm", "mm"),
                    ...,
                    .measure_dev = NULL, .return_graph = FALSE) {

    if (!inherits(x, "selecta"))
        stop("'x' must be a selecta object", call. = FALSE)

    units <- match.arg(units)

    ## Fail fast on an invalid number_format before measuring or drawing.
    validate_number_format(number_format)

    graph <- compute(x)

    ## Run export_grid on a throwaway device for exact dimensions
    graph_full <- layout_nodes(graph)
    if (is.null(.measure_dev)) {
        tf_h <- tempfile(fileext = ".pdf")
        grDevices::pdf(tf_h, width = 10, height = 10)
    } else {
        tf_h <- .measure_dev()
    }

    ## Only values the caller supplied are forwarded, leaving every unsupplied
    ## parameter at the drawing routine's own default. Measurement therefore
    ## cannot disagree with rendering, since neither default is restated here.
    draw_args <- list(graph = graph_full, newpage = TRUE, measure_only = TRUE)
    for (p in .measure_params) {
        v <- get(p, envir = environment())
        if (!is.null(v)) draw_args[[p]] <- v
    }
    g <- do.call(export_grid, draw_args)
    grDevices::dev.off()
    if (!is.null(tf_h) && file.exists(tf_h)) unlink(tf_h)

    h <- g$diagram_height_in
    if (is.null(h) || is.na(h)) h <- 8.0

    w <- g$diagram_width_in
    if (is.null(w) || is.na(w)) w <- 6.0

    ## Convert before rounding so the rounded-up tenth is a tenth of the
    ## reported unit rather than of an inch.
    w_out <- convert_units(w, from = "in", to = units)
    h_out <- convert_units(h, from = "in", to = units)

    result <- c(width  = ceiling(w_out * 10) / 10,
                height = ceiling(h_out * 10) / 10)

    ## Recorded so a consumer that assumed inches cannot mis-size the output
    ## by the conversion factor.
    attr(result, "units") <- units

    ## Optional debug: recommended canvas dimensions and their raw inputs.
    debug_emit("recdims() dimensions",
               raw_width_in = w, raw_height_in = h,
               phase_strip_w_in = g$phase_strip_w %||% NA_real_,
               units = units,
               width = unname(result["width"]),
               height = unname(result["height"]))

    if (isTRUE(.return_graph)) attr(result, "graph") <- graph_full
    result
}


#' Save Diagram to File
#'
#' Renders the enrollment diagram and saves it to a file. Supported
#' formats are PDF, PNG, SVG, and TIFF (inferred from the file
#' extension). The \code{grid} engine renders via R graphics devices; the
#' \code{dot} engine pipes Graphviz output through the system \code{dot}
#' binary. Dimensions are computed automatically from diagram content via
#' \code{recdims()} unless overridden.
#'
#' @param x A \code{selecta} object.
#' @param file Character string. Output file path. The format is inferred
#'   from the file extension. Supported extensions: \code{.pdf},
#'   \code{.png}, \code{.svg}, \code{.tif}/\code{.tiff} (all engines);
#'   \code{.dot} (\code{dot} engine only, writes the raw DOT source).
#' @param engine Character string. One of \code{"grid"} (the default,
#'   uses R's grid graphics) or \code{"dot"} (uses the system Graphviz
#'   binary). The \code{dot} engine requires \code{dot} to be installed and
#'   on the system \code{PATH}.
#' @param width Numeric or \code{NULL}. Width in \code{units}. If \code{NULL}
#'   (default), computed automatically. For the \code{dot} engine, omit to
#'   let Graphviz determine dimensions from layout.
#' @param height Numeric or \code{NULL}. Height in \code{units}. If
#'   \code{NULL} (default), computed automatically. For the \code{dot}
#'   engine, omit to let Graphviz determine dimensions from layout.
#' @param units Character string giving the units of \code{width} and
#'   \code{height}, and of the dimensions computed when either is left
#'   unspecified: \code{"in"} (inches, the default), \code{"cm"}, or
#'   \code{"mm"}. Graphics devices are driven in inches regardless, so the
#'   conversion is internal. Ignored by the \code{dot} engine, which takes
#'   no dimensions.
#' @param dpi Integer. Resolution in dots per inch for raster formats
#'   (PNG, TIFF). Default 300. Honored by both engines. Mirrors the
#'   \code{dpi} argument of \code{ggplot2::ggsave()}.
#' @param sans_serif Logical. \code{dot} engine only. If \code{TRUE} (default),
#'   the rendered SVG/PDF text is displayed in a sans-serif fallback
#'   chain (\code{Helvetica, Arial, "Liberation Sans", "DejaVu Sans",
#'   sans-serif}) regardless of the layout font. Layout boxes are still
#'   sized using the metrics of the font set via \code{font_family}, so
#'   the result preserves all margins. Set to \code{FALSE} to retain
#'   the layout font as the displayed font.
#' @param quiet Logical. Suppress the message reporting the file written and
#'   the dimensions used. The \code{dot} engine reports neither, so the
#'   setting has no effect there. Default \code{FALSE}.
#' @param ... Additional styling and formatting arguments forwarded to the
#'   selected engine; see \code{flowchart()} for the full descriptions.
#'   \describe{
#'     \item{\code{engine = "grid"}}{\code{cex}, \code{cex_side},
#'       \code{cex_phase}, \code{box_fill}, \code{phase_fill}, \code{vpad},
#'       \code{margin}, \code{font_family}, \code{number_format}}
#'     \item{\code{engine = "dot"}}{\code{formatting}, \code{bullets},
#'       \code{count_first}, \code{number_format}, \code{ortho},
#'       \code{font_family}, \code{padding_pt}, \code{padding_adjust},
#'       \code{box_fill}, \code{side_fill}, \code{border_col},
#'       \code{arrow_col}, \code{source_fill}, \code{source_header_fill},
#'       \code{source_header_text}, \code{phase_labels}, \code{phase_fill},
#'       \code{phase_text_col}, \code{rank_sep}, \code{node_sep}}
#'   }
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' \code{flowsave()} renders a flow directly to a file, inferring the format
#' from the extension and choosing dimensions automatically unless
#' \code{width} and \code{height} are given. With \code{engine = "grid"} it
#' draws through R's graphics devices, producing either vector formats
#' (\code{.pdf}, \code{.svg}) or raster formats (\code{.png}, \code{.tiff}).
#'
#' For raster formats, \code{flowsave()} prefers the \pkg{ragg} device when
#' installed, with fallback to the base \code{png()}/\code{tiff()} devices
#' otherwise. Using these devices is generally advised for raster output
#' over other devices such as cairo since some cairo configurations drop
#' the plotmath italics in the count labels. The \code{dpi} argument mirrors
#' \code{ggplot2::ggsave()} for raster resolution.
#'
#' With \code{engine = "dot"}, \code{flowsave()} renders a graphic based on
#' a Graphviz DOT string: a \code{.dot} extension writes the source text
#' directly and needs no external software, whereas image output shells out
#' to the system \code{dot} binary and therefore requires Graphviz on the
#' \code{PATH}.
#'
#' When sizing automatically, \code{flowsave()} calls \code{recdims()}
#' once and reuses the computed layout, so a separate \code{recdims()} call
#' is unnecessary. With the \code{grid} engine, the file written and the
#' dimensions used are reported through a \code{message()} unless
#' \code{quiet = TRUE}, whether those dimensions were computed or supplied,
#' so that a figure written at an unexpected size is apparent at the point it
#' is written. The \code{dot} engine instead lets Graphviz size the output
#' from the layout, so it reports nothing.
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
#'
#' # Dimensions may be given, or computed, in metric units.
#' flowsave(flow, file.path(tempdir(), "consort_metric.pdf"),
#'          width = 180, height = 240, units = "mm")
#'
#' # Suppress the message reporting the file written.
#' flowsave(flow, file.path(tempdir(), "consort_quiet.pdf"), quiet = TRUE)
#' }
#'
#' \donttest{
#' # DOT engine writing a .dot source file requires no external software.
#' flowsave(flow, file.path(tempdir(), "consort.dot"), engine = "dot")
#'
#' # Rasterized DOT output (.svg, .png, .pdf) requires the Graphviz 'dot'
#' # binary on the system PATH.
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
                     units = c("in", "cm", "mm"),
                     dpi = 300, sans_serif = TRUE, quiet = FALSE, ...) {

    if (!inherits(x, "selecta"))
        stop("'x' must be a selecta object", call. = FALSE)

    engine <- match.arg(engine)
    units  <- match.arg(units)
    dots   <- list(...)
    ## Fail fast on an invalid number_format before any computation or drawing.
    validate_number_format(dots$number_format)
    ext    <- tolower(tools::file_ext(file))

    ## ---- DOT engine branch ----
    if (engine == "dot") {
        return(.flowsave_dot(x, file, ext, dpi = dpi,
                             sans_serif = sans_serif, ...))
    }

    ## ---- Grid engine branch (original behavior) ----
    if (ext == "dot")
        stop("'.dot' output requires engine = 'dot'", call. = FALSE)

    ## Checked before measuring or reporting, so an unsupported extension
    ## fails fast rather than after a message announcing the file written.
    if (!ext %in% c("pdf", "svg", "png", "tif", "tiff"))
        stop(sprintf("Unsupported format: '%s'", ext), call. = FALSE)

    cached_graph <- NULL

    if (is.null(width) || is.null(height)) {
        ## Forward layout parameters to recdims for consistent canvas sizing,
        ## requesting the pre-computed graph to reuse compute() + layout_nodes().
        sz_args <- list(x = x, units = units, .return_graph = TRUE)
        for (p in .measure_params)
            if (!is.null(dots[[p]])) sz_args[[p]] <- dots[[p]]
        sz <- do.call(recdims, sz_args)
        if (is.null(width))  width  <- unname(sz["width"])
        if (is.null(height)) height <- unname(sz["height"])
        cached_graph <- attr(sz, "graph")
    }

    ## Reported whether the dimensions were computed or supplied, so a figure
    ## written at an unexpected size is apparent at the point it is written.
    if (!quiet)
        message(sprintf(
            "Flowchart saved to %s (width = %.1f %s, height = %.1f %s)",
            file, width, units, height, units))

    ## Graphics devices are driven in inches whatever units the dimensions
    ## were supplied or computed in.
    width_in  <- convert_units(width,  from = units, to = "in")
    height_in <- convert_units(height, from = units, to = "in")

    ## Device selection
    has_ragg <- requireNamespace("ragg", quietly = TRUE)
    switch(ext,
           pdf  = pdf(file, width = width_in, height = height_in),
           svg  = svg(file, width = width_in, height = height_in),
           png  = if (has_ragg) {
                      ragg::agg_png(file, width = width_in, height = height_in,
                                    units = "in", res = dpi)
                  } else {
                      png(file, width = width_in, height = height_in, units = "in", res = dpi)
                  },
           tiff =, tif = if (has_ragg) {
                             ragg::agg_tiff(file, width = width_in, height = height_in,
                                            units = "in", res = dpi)
                         } else {
                             tiff(file, width = width_in, height = height_in, units = "in", res = dpi)
                         },
           ## Unreachable given the check above; retained as a guard.
           stop(sprintf("Unsupported format: '%s'", ext), call. = FALSE)
           )
    on.exit(dev.off())

    ## Reuse cached graph (compute + layout are device-agnostic); export_grid
    ## re-measures text on the real device.
    graph <- if (!is.null(cached_graph)) cached_graph else {
        layout_nodes(compute(x))
    }
    export_grid(graph, newpage = TRUE, ...)

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

    ## Sans-serif font substitution for SVG output
    if (isTRUE(sans_serif) && gv_fmt == "svg" && file.exists(file)) {
        sans_chain <- "Helvetica, Arial, 'Liberation Sans', 'DejaVu Sans', sans-serif"
        svg_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
        ## Match either Helvetica- or Times-anchored attribute values, in
        ## either single- or double-quoted form, with optional CSS family
        ## fallbacks Graphviz appends (Helvetica, sans-serif and similar).
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
