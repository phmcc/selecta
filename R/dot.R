### * Main functions

#' Convert Graph to Graphviz DOT String
#'
#' Generates a Graphviz DOT-language representation of a computed graph.
#' Node fill colors follow the package's standard palette: navy blue with
#' white text for source headers, light blue for source boxes, and white
#' for everything else.
#'
#' The engine has two label-formatting modes selected by the
#' \code{formatting} argument:
#'
#' \describe{
#'   \item{\code{"plain"} (default)}{Labels are emitted as plain DOT
#'     text without inline markup. Graphviz handles plain text reliably
#'     across all backends, producing exactly-centered labels at every
#'     font and zoom level. Source headers receive a bold typeface via
#'     a whole-node \code{fontname} (\emph{e.g.,} \code{"Helvetica-Bold"})
#'     rather than inline \code{<B>} markup; this preserves the visual
#'     emphasis without invoking Graphviz's HTML-label code path.}
#'   \item{\code{"rich"}}{Labels use HTML-like markup with inline bold
#'     for the descriptive text and italic for the lowercase \emph{n} in
#'     "n = X", matching the typographic conventions used by the grid
#'     engine and by published EQUATOR diagrams. This mode invokes
#'     Graphviz's HTML-label code path, whose text-width estimator
#'     drifts slightly from the actually-rendered glyph widths.
#'     Width measurement uses embedded Adobe Font Metric (AFM) tables
#'     for the rendered Helvetica and Times families, with trailing-
#'     whitespace compensation to recenter the visible glyphs. The
#'     result is sub-pixel-accurate centering for Helvetica and exact
#'     centering for Times; other fonts (Courier, system sans-serifs)
#'     may show small residual drift since their Graphviz HTML-label
#'     metrics differ from what browsers actually render.}
#' }
#'
#' Most users should accept the default \code{"plain"} formatting,
#' which is the more robust choice for prototyping and web embedding.
#' The \code{"rich"} mode is available for diagrams where the inline
#' italic-\emph{n} and bold-label typography is essential.
#'
#' @param graph A computed and laid-out graph.
#' @param number_format Locale-aware count formatter (see
#'   \code{\link{flowchart}}). Defaults to the
#'   \code{selecta.number_format} option.
#' @param count_first Logical. If \code{TRUE}, the count appears before
#'   the label text in each box (\emph{e.g.,} \verb{200 Excluded}
#'   instead of \verb{Excluded, n = 200}), matching the count-first
#'   layout available in the grid engine. Default \code{FALSE}.
#' @param ortho Logical. If \code{TRUE}, requests right-angle (orthogonal)
#'   edges via Graphviz's \code{splines=ortho} attribute. Default
#'   \code{FALSE}.
#' @param formatting Character string, either \code{"plain"} (default)
#'   or \code{"rich"}. See Details.
#' @param font_family Character string. Graphviz \code{fontname} value
#'   for the body text. Default \code{"Helvetica"}.
#' @param padding_pt Numeric. Horizontal padding applied uniformly on
#'   each side of every node's text, in points. Default 14.
#' @param padding_adjust Numeric. Additive offset to \code{padding_pt}
#'   for fine-tuning, in points. Default 0.
#' @param box_fill Character. Fill color for main boxes. Default
#'   \code{"#FFFFFF"}.
#' @param side_fill Character. Fill color for side (exclusion) boxes.
#'   Default \code{"#F0F0F0"}.
#' @param border_col Character. Border color for all boxes. Default
#'   \code{"black"}.
#' @param arrow_col Character. Color for arrows and connector lines.
#'   Default \code{"black"}.
#' @param source_fill Character. Fill color for source boxes in
#'   multi-source diagrams (PRISMA, MOOSE). Default \code{"#FFFFFF"},
#'   matching the grid engine.
#' @param source_header_fill Character. Fill color for source-column
#'   header boxes. Default \code{"#D0D0D0"}, matching the grid engine.
#' @param source_header_text Character. Text color for source-column
#'   header labels. Default \code{"black"}, matching the grid engine.
#' @return A character string in DOT format.
#' @keywords internal
to_dot <- function(graph, number_format = NULL, count_first = FALSE,
                   ortho = FALSE,
                   formatting = c("plain", "rich"),
                   font_family = "Helvetica",
                   padding_pt = 14, padding_adjust = 0,
                   box_fill           = "#FFFFFF",
                   side_fill          = "#F0F0F0",
                   border_col         = "black",
                   arrow_col          = "black",
                   source_fill        = "#FFFFFF",
                   source_header_fill = "#D0D0D0",
                   source_header_text = "black") {

  formatting <- match.arg(formatting)
  padding_pt <- padding_pt + padding_adjust

  marks <- resolve_number_marks(number_format)
  fn    <- function(n) fmt_n(n, marks)

  nodes <- graph$nodes
  edges <- graph$edges

  is_times   <- grepl("^Times",   font_family, ignore.case = TRUE)
  is_courier <- grepl("^Courier", font_family, ignore.case = TRUE)

  ## Padding to inches for the margin attribute; vertical is held smaller
  ## since the line-height already provides comfortable vertical spacing.
  margin_x_in <- padding_pt / 72
  margin_y_in <- 6 / 72

  font_size_pt <- 14

  ## ---- DOT emission preamble ----
  lines <- character()
  lines <- c(lines, "digraph selecta {")
  lines <- c(lines, "  rankdir=TB;")
  if (isTRUE(ortho)) {
    lines <- c(lines, "  splines=ortho;")
    lines <- c(lines, "  concentrate=false;")
    lines <- c(lines, "  nodesep=0.6;")
    lines <- c(lines, "  ranksep=0.5;")
  }
  lines <- c(lines, sprintf(
    paste0('  node [shape=box, style=filled, fontname="%s", ',
           'fontsize=%d, margin="%.3f,%.3f", color="%s"];'),
    font_family, font_size_pt, margin_x_in, margin_y_in, border_col))

  ## ---- Per-formatting-mode label and node emission ----
  if (formatting == "rich") {

    ## Rich mode: HTML labels with inline bold/italic; width from embedded
    ## AFM tables, trailing whitespace correcting Graphviz's centering.
    rich_node <- build_rich_emitter(
      fn = fn, count_first = count_first,
      is_times = is_times, is_courier = is_courier,
      font_family = font_family,
      padding_pt = padding_pt,
      font_size_pt = font_size_pt,
      box_fill = box_fill, side_fill = side_fill,
      source_fill = source_fill,
      source_header_fill = source_header_fill,
      source_header_text = source_header_text
    )
    for (i in seq_len(nrow(nodes))) {
      lines <- c(lines, rich_node(nodes[i]))
    }

  } else {

    ## Plain mode: plain DOT labels, which Graphviz measures and centers
    ## accurately; source headers gain bold via a whole-node Bold fontname.
    plain_node <- build_plain_emitter(
      fn = fn, count_first = count_first,
      font_family = font_family,
      box_fill = box_fill, side_fill = side_fill,
      source_fill = source_fill,
      source_header_fill = source_header_fill,
      source_header_text = source_header_text
    )
    for (i in seq_len(nrow(nodes))) {
      lines <- c(lines, plain_node(nodes[i]))
    }
  }

  ## ---- Edges (formatting-independent) ----
  for (i in seq_len(nrow(edges))) {
    e <- edges[i]
    extras <- switch(e$edge_type,
      exclude  = sprintf(' style=dashed, color="%s", fontcolor="%s"',
                         arrow_col, arrow_col),
      converge = sprintf(' style=bold, color="%s", fontcolor="%s"',
                         arrow_col, arrow_col),
      sprintf(' color="%s", fontcolor="%s"', arrow_col, arrow_col)
    )
    lines <- c(lines, sprintf("  n%d -> n%d [%s];", e$from, e$to, extras))
  }

  ## ---- Source-header positioning (multi-source flows) ----
  ## Source headers are pure labels with no edges of their own, so Graphviz
  ## has nothing tying each header above its column and packs all the
  ## parentless top nodes into one interleaved row. Three constraints fix
  ## this for any number of source groups: an invisible edge from each header
  ## to its own source box (matched by stream_group) ranks the header
  ## directly above that box, a high weight on that edge keeps the box
  ## centered under its (often wider) header rather than being pulled toward
  ## the converge target below, and a rank=same grouping holds all the
  ## headers together on a single top row.
  hdr_idx <- which(nodes$role == "source_header")
  if (length(hdr_idx) > 0L) {
    src_idx <- which(nodes$role == "source")
    for (h in hdr_idx) {
      grp <- nodes$stream_group[h]
      if (is.na(grp)) next
      match_src <- src_idx[nodes$stream_group[src_idx] == grp]
      if (length(match_src) >= 1L)
        lines <- c(lines, sprintf("  n%d -> n%d [style=invis, weight=100];",
                                  nodes$node_id[h], nodes$node_id[match_src[1L]]))
    }
    if (length(hdr_idx) >= 2L) {
      hdr_ids <- paste(sprintf("n%d", nodes$node_id[hdr_idx]), collapse = "; ")
      lines <- c(lines, sprintf("  { rank=same; %s; }", hdr_ids))
    }
  }

  lines <- c(lines, "}")
  dot_src <- paste(lines, collapse = "\n")

  ## With options(selecta.debug_layout = TRUE): emit the generated DOT source
  ## and its salient settings, mirroring the grid engine's layout debug so
  ## that both rendering paths can be inspected from the same option.
  debug_emit("to_dot() source",
             settings = sprintf(
                 "formatting=%s  ortho=%s  count_first=%s  font=%s  nodes=%d  edges=%d",
                 formatting, isTRUE(ortho), isTRUE(count_first), font_family,
                 nrow(nodes), nrow(edges)),
             dot = dot_src)

  dot_src
}


### * Plain-label emitter

#' Build a Plain-Label DOT Node Emitter
#'
#' Produces a closure emitting one plain DOT node-statement per call.
#' Source headers receive a bold variant of the body font via the per-node
#' \code{fontname}, which Graphviz measures accurately.
#'
#' @param fn Count-formatting function.
#' @param count_first Logical; place the count before the label text.
#' @param font_family Character body font family.
#' @param box_fill,side_fill,source_fill Fill colors for main, side, and
#'   source boxes.
#' @param source_header_fill,source_header_text Fill and text colors for
#'   source-header boxes.
#' @return A function of a single node row returning a DOT node-statement.
#' @keywords internal
build_plain_emitter <- function(fn, count_first, font_family,
                                box_fill, side_fill, source_fill,
                                source_header_fill, source_header_text) {

  ## Plain DOT escapes: backslash first (to avoid double-processing), then
  ## double-quote; newline is emitted later as the literal "\n".
  esc <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub('"',  '\\"',  s, fixed = TRUE)
    s
  }

  ## Derive a bold font name for source headers from the chosen family
  ## (e.g. Helvetica -> Helvetica-Bold, Times-Roman -> Times-Bold).
  bold_font <- if (grepl("^Times", font_family, ignore.case = TRUE)) {
    "Times-Bold"
  } else if (grepl("^Courier", font_family, ignore.case = TRUE)) {
    "Courier-Bold"
  } else {
    paste0(font_family, "-Bold")
  }

  build_label <- function(text, n, role) {
    has_text <- nchar(text) > 0L
    n_str    <- fn(n)
    if (role == "source_header") {
      esc(text)
    } else if (!has_text) {
      sprintf("n = %s", esc(n_str))
    } else if (isTRUE(count_first)) {
      sprintf("%s %s", esc(n_str), esc(text))
    } else {
      ## Plain DOT line break: written as "\\n" in source so Graphviz sees \n.
      sprintf("%s\\nn = %s", esc(text), esc(n_str))
    }
  }

  function(nd) {
    lbl <- build_label(nd$text, nd$n, nd$role)
    fill <- switch(nd$role,
      side          = side_fill,
      source        = source_fill,
      source_header = source_header_fill,
      cell          = box_fill,
      alloc         = box_fill,
      box_fill
    )
    if (nd$role == "source_header") {
      sprintf(
        '  n%d [label="%s", fillcolor="%s", fontcolor="%s", fontname="%s"];',
        nd$node_id, lbl, fill, source_header_text, bold_font)
    } else {
      sprintf('  n%d [label="%s", fillcolor="%s"];',
              nd$node_id, lbl, fill)
    }
  }
}


### * Rich (HTML-label) emitter

#' Build a Rich HTML-Label DOT Node Emitter
#'
#' Emits HTML-like labels with inline bold/italic markup and a calibrated
#' trailing-whitespace span compensating for Graphviz's bold-text width
#' underestimate on the SVG backend. Width measurement uses embedded AFM
#' tables for the supported font families.
#'
#' @param fn Count-formatting function.
#' @param count_first Logical; place the count before the label text.
#' @param is_times,is_courier Logical flags for the active font family.
#' @param font_family Character body font family.
#' @param padding_pt,font_size_pt Numeric horizontal padding and font size
#'   in points.
#' @param box_fill,side_fill,source_fill Fill colors for main, side, and
#'   source boxes.
#' @param source_header_fill,source_header_text Fill and text colors for
#'   source-header boxes.
#' @return A function of a single node row returning a DOT node-statement.
#' @keywords internal
build_rich_emitter <- function(fn, count_first, is_times, is_courier,
                               font_family, padding_pt, font_size_pt,
                               box_fill, side_fill, source_fill,
                               source_header_fill, source_header_text) {

  ## XML entity escaping for HTML-like labels.
  esc <- function(s) {
    s <- gsub("&", "&amp;",  s, fixed = TRUE)
    s <- gsub("<", "&lt;",   s, fixed = TRUE)
    s <- gsub(">", "&gt;",   s, fixed = TRUE)
    s <- gsub('"', "&quot;", s, fixed = TRUE)
    s <- gsub("'", "&#39;",  s, fixed = TRUE)
    s
  }

  ## ---- Embedded Adobe Font Metric (AFM) tables ----
  ## Character advance widths in 1/1000 em units, ASCII range 32-126.
  helvetica_widths <- c(278,278,355,556,556,889,667,222,333,333,389,584,
    278,333,278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,
    584,584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,469,
    556,222,556,556,500,556,556,278,556,556,222,222,500,222,833,556,556,
    556,556,333,500,278,556,500,722,500,500,500,334,260,334,584)

  helvetica_bold_widths <- c(278,333,474,556,556,889,722,278,333,333,389,
    584,278,333,278,278,556,556,556,556,556,556,556,556,556,556,333,333,
    584,584,584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,
    833,722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,278,556,611,556,611,556,333,611,611,278,278,556,278,889,611,
    611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,584)

  times_roman_widths <- c(250,333,408,500,500,833,778,333,333,333,500,
    564,250,333,250,278,500,500,500,500,500,500,500,500,500,500,278,278,
    564,564,564,444,921,722,667,667,722,611,556,722,722,333,389,722,611,
    889,722,722,556,722,667,556,611,722,722,944,722,722,611,333,278,333,
    469,500,333,444,500,444,500,444,333,500,500,278,278,500,278,778,500,
    500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,541)

  times_bold_widths <- c(250,333,555,500,500,1000,833,333,333,333,500,
    570,250,333,250,278,500,500,500,500,500,500,500,500,500,500,333,333,
    570,570,570,500,930,722,667,722,722,667,611,778,778,389,500,778,667,
    944,722,778,611,778,722,556,667,722,722,1000,722,722,667,333,278,333,
    581,500,333,500,556,444,556,444,333,500,556,278,333,556,278,833,556,
    500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,520)

  times_italic_widths <- c(250,333,420,500,500,833,778,333,333,333,500,
    675,250,333,250,278,500,500,500,500,500,500,500,500,500,500,333,333,
    675,675,675,500,920,611,611,667,722,611,611,722,722,333,444,667,556,
    833,667,722,611,722,611,500,556,722,611,833,611,556,556,389,278,389,
    422,500,333,500,500,444,500,444,278,500,500,278,278,444,278,722,500,
    500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,541)

  courier_widths <- rep(600L, 95L)

  metrics <- if (is_times) {
    list(plain  = times_roman_widths,
         bold   = times_bold_widths,
         italic = times_italic_widths)
  } else if (is_courier) {
    list(plain  = courier_widths,
         bold   = courier_widths,
         italic = courier_widths)
  } else {
    list(plain  = helvetica_widths,
         bold   = helvetica_bold_widths,
         italic = helvetica_widths)
  }

  measure_pt <- function(s, face = "plain") {
    if (!nzchar(s)) return(0)
    table <- metrics[[face]]
    cps   <- utf8ToInt(s)
    widths <- ifelse(cps >= 32 & cps <= 126,
                     table[pmax(1L, cps - 31L)],
                     mean(table))
    sum(widths) * font_size_pt / 1000
  }

  ## Per-line maximum width (used to set the `width=` attribute below).
  width_for_node <- function(text, n, role) {
    n_str <- fn(n)
    if (role == "source_header") {
      measure_pt(text, "bold")
    } else if (!nzchar(text)) {
      measure_pt("n", "italic") + measure_pt(sprintf(" = %s", n_str))
    } else if (isTRUE(count_first)) {
      measure_pt(n_str, "bold") + measure_pt(sprintf(" %s", text))
    } else {
      max(measure_pt(text, "bold"),
          measure_pt("n", "italic") +
            measure_pt(sprintf(" = %s", n_str)))
    }
  }

  ## Trailing-whitespace centering correction: Graphviz under-measures
    ## Helvetica-Bold by ~0.22 pt/char, so non-breaking spaces at a small
    ## point size inflate its estimate and recentre the glyphs. Times needs
    ## no correction; Courier is not reliably calibrated, so none is applied.
    bold_gap_per_char <- if (is_times || is_courier) 0 else 0.22
    ws_pt_size        <- 8L
    ws_unit_pt        <- 0.278 * ws_pt_size
    trailing_ws <- function(text) {
        if (!nzchar(text) || bold_gap_per_char == 0) return("")
        gap_pt <- nchar(text) * bold_gap_per_char
        if (gap_pt <= 0.5) return("")
        n_spaces <- max(1L, as.integer(round(gap_pt / ws_unit_pt)))
        sprintf('<FONT POINT-SIZE="%d">%s</FONT>',
                ws_pt_size, strrep("&nbsp;", n_spaces))
    }
    bold_gap_pt <- function(text) {
        if (!nzchar(text)) return(0)
        nchar(text) * bold_gap_per_char
    }

    ## Width-attribute eligibility: only when AFM metrics match the renderer
    ## output (Helvetica and Times via the embedded Adobe AFM tables).
    metrics_reliable <- is_times || identical(font_family, "Helvetica")

    build_label <- function(text, n, role) {
        has_text <- nchar(text) > 0L
        n_str    <- fn(n)
        body <- if (role == "source_header") {
                    sprintf("<B>%s</B>%s", esc(text), trailing_ws(text))
                } else if (!has_text) {
                    sprintf("<I>n</I> = %s", esc(n_str))
                } else if (isTRUE(count_first)) {
                    gap_pt  <- bold_gap_pt(n_str) + 2
                    ws_html <- if (gap_pt > 0.5) {
                                   n_spaces <- max(1L, as.integer(round(gap_pt / ws_unit_pt)))
                                   sprintf('<FONT POINT-SIZE="%d">%s</FONT>',
                                           ws_pt_size, strrep("&nbsp;", n_spaces))
                               } else ""
                    sprintf("<B>%s</B> %s%s", esc(n_str), esc(text), ws_html)
                } else {
                    sprintf(
                        '<B>%s</B>%s<BR/><FONT POINT-SIZE="4"> </FONT><BR/><I>n</I> = %s',
                        esc(text), trailing_ws(text), esc(n_str))
                }
        sprintf("<%s>", body)
    }

    function(nd) {
        lbl      <- build_label(nd$text, nd$n, nd$role)
        width_pt <- width_for_node(nd$text, nd$n, nd$role)
        width_in <- (width_pt + 2 * padding_pt) / 72
        fill <- switch(nd$role,
                       side          = side_fill,
                       source        = source_fill,
                       source_header = source_header_fill,
                       cell          = box_fill,
                       alloc         = box_fill,
                       box_fill
                       )
        width_attr <- if (metrics_reliable) sprintf(", width=%.3f", width_in) else ""
        if (nd$role == "source_header") {
            sprintf('  n%d [label=%s, fillcolor="%s", fontcolor="%s"%s];',
                    nd$node_id, lbl, fill, source_header_text, width_attr)
        } else {
            sprintf('  n%d [label=%s, fillcolor="%s"%s];',
                    nd$node_id, lbl, fill, width_attr)
        }
    }
}
