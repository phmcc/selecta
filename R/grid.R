### * Main functions

#' Draw Enrollment Diagram via Grid Graphics
#'
#' Computes all layout in inches using physical text measurements, then
#' renders the diagram within a fixed-margin viewport. Intended to be called
#' by \code{flowchart()} or \code{flowsave()} rather than directly.
#'
#' @param graph A laid-out graph (output of \code{layout_nodes()}).
#' @param cex Numeric. Font size multiplier for main box text. Default 0.85.
#' @param cex_side Numeric. Font size multiplier for side box text. Defaults
#'   to the value of \code{cex}.
#' @param cex_phase Numeric. Font size multiplier for phase labels.
#'   Default 0.9.
#' @param box_fill Character. Fill color for main boxes. Default \code{"white"}.
#' @param side_fill Character. Fill color for side (exclusion) boxes.
#'   Default \code{"white"}.
#' @param border_col Character. Border color for all boxes.
#'   Default \code{"black"}.
#' @param arrow_col Character. Color for arrows and connector lines.
#'   Default \code{"black"}.
#' @param phase_fill Character. Background color for phase label boxes.
#'   Default \code{"black"}.
#' @param phase_text_col Character. Text color for phase labels.
#'   Default \code{"white"}.
#' @param lwd Numeric. Line width for borders and arrows. Default 1.
#' @param count_first Logical. If \code{TRUE}, side-box labels are rendered
#'   as \code{"214  Discontinued"} (bold count before label) rather than the
#'   default \code{"Discontinued (n = 214)"}. Default \code{FALSE}.
#' @param newpage Logical. If \code{TRUE}, calls \code{grid.newpage()} before
#'   drawing. Default \code{TRUE}.
#' @param vpad Numeric. Vertical spacing between elements in inches. Controls
#'   the uniform gap between any box edge and the next adjacent element.
#'   Default 0.25; override globally with
#'   \code{options(selecta.vpad = 0.35)}.
#' @param pad Numeric. Internal padding within boxes in inches. Default 0.08.
#' @param line_height Numeric. Vertical line spacing in inches, controlling
#'   box heights for both main and side boxes. Scales proportionally with
#'   \code{cex}. Default 0.20.
#' @param margin Numeric. Fixed margin on all four sides of the canvas in
#'   inches. Default 0.25.
#' @param phase_width Numeric. Width of phase label boxes in inches.
#'   Default 0.22. When \code{phase_multiline = TRUE} the strip is widened
#'   automatically to fit the wrapped lines, so this acts as a per-line
#'   minimum rather than a hard cap.
#' @param phase_multiline Logical. If \code{TRUE} (the default), a phase
#'   label longer than the vertical extent of the boxes it spans is
#'   word-wrapped across multiple stacked lines (drawn rotated in the strip),
#'   trading strip width for height so the diagram is not stretched
#'   vertically to fit a long rotated label. Set to \code{FALSE} to force
#'   every label onto a single line, in which case a label taller than its
#'   band stretches the diagram instead. A label that cannot be wrapped (a
#'   single word taller than its band) falls back to stretching either way.
#'   Labels containing an explicit newline (\code{"\\n"}) are always split on
#'   it regardless of this setting. Default \code{TRUE}.
#' @param phase_max_lines Integer. Maximum number of wrapped lines per phase
#'   label when wrapping is active; any overflow is collapsed into the final
#'   line. Default 3.
#' @param font_family Character. Font family used for all text in the
#'   diagram. Default \code{"Helvetica"}. Set to \code{""} to use the
#'   device default.
#' @param number_format Character string or two-element character vector.
#'   Locale-aware formatting for participant counts: \code{"us"} (default,
#'   \code{1,234}), \code{"eu"} (\code{1.234}), \code{"space"}
#'   (\code{1\\u202F234}), \code{"none"} (\code{1234}), or a custom
#'   \code{c(big.mark, decimal.mark)} pair. Falls back to
#'   \code{getOption("selecta.number_format", "us")} when \code{NULL}.
#' @param measure_only Logical. When \code{TRUE}, the function computes the
#'   layout and canvas dimensions but returns before issuing any drawing
#'   primitives, so no graphics output is produced. Used internally by
#'   \code{recdims()} to size the canvas without the cost of rendering.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the graph, augmented with computed layout
#'   dimensions (\code{diagram_width_in}, \code{diagram_height_in}).
#' @keywords internal
export_grid <- function(graph,
                        cex            = 0.85,
                        cex_side       = NULL,
                        cex_phase      = 0.9,
                        box_fill       = "white",
                        side_fill      = "white",
                        border_col     = "black",
                        arrow_col      = "black",
                        phase_fill     = "black",
                        phase_text_col = "white",
                        lwd            = 1,
                        count_first    = FALSE,
                        newpage        = TRUE,
                        vpad           = getOption("selecta.vpad", 0.25),
                        pad            = 0.08,
                        line_height    = 0.20,
                        margin         = 0.25,
                        phase_width    = 0.22,
                        phase_multiline = TRUE,
                        phase_max_lines = 3L,
                        font_family    = "Helvetica",
                        number_format  = NULL,
                        measure_only   = FALSE) {

    nodes  <- graph$nodes
    edges  <- graph$edges
    phases <- graph$phases

    if (nrow(nodes) == 0L) {
        warning("Empty diagram -- no nodes to draw", call. = FALSE)
        return(invisible(graph))
    }

    if (is.null(cex_side)) cex_side <- cex

    ## Resolve number formatting once
    marks       <- resolve_number_marks(number_format)
    .pkg_fmt_n  <- fmt_n
    fmt_n       <- function(n) .pkg_fmt_n(n, marks)

    ## ---- Viewport ----
    has_phases <- nrow(phases) > 0L

    if (newpage) grid.newpage()

    ## Device dimensions
    dev_w_in <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
    dev_h_in <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)

    ## Line-height and padding scalars (inches)
    lh      <- line_height * (cex / 0.85)
    side_lh <- line_height * (cex_side / 0.85)
    pad_h   <- pad
    pad_v   <- pad
    vpad_in <- vpad

    ## Phase label strip sizing
    phase_lines <- NULL
    label_h_in  <- NULL
    if (has_phases) {
        ff0 <- if (nzchar(font_family)) font_family else NULL
        gp_ph_meas0 <- gpar(cex = cex_phase, fontface = "bold",
                            fontfamily = ff0)
        tw0 <- function(label, gp)
            convertWidth(grobWidth(textGrob(label, gp = gp)),
                         "inches", valueOnly = TRUE)
        ## Wrapping (and its line cap) applies only when phase_multiline is
        ## on; explicit "\n" splits regardless and is never capped away.
        ml_wrap <- isTRUE(phase_multiline)
        ml_max  <- if (ml_wrap) max(1L, as.integer(phase_max_lines)) else NA_integer_

        ## Per-phase wrap target
        ph_targets <- rep(NA_real_, nrow(phases))
        if (ml_wrap) {
            nreason_v <- lengths(nodes$reasons)
            node_h_est <- function(j) {
                r <- nodes$role[j]; k <- nreason_v[j]
                if (r == "side")                       (1 + k) * side_lh + 2 * pad_v
                else if (r == "source")                (1 + k) * lh + 2 * pad_v
                else if (r == "source_header")         1 * lh + 2 * pad_v
                else if (r == "endpoint" && k > 0L)    (1 + k) * lh + 2 * pad_v
                else                                   (if (count_first) 1L else 2L) * lh +
                    k * lh + 2 * pad_v
            }
            heights <- vapply(seq_len(nrow(nodes)), node_h_est, numeric(1L))
            ## Map each phase strip's phase-index span to row indices through
            ## the same compaction layout_nodes() uses, exactly as
            ## phase_band_deficits() does, so gapped phase indices are handled.
            ph_nums <- sort(unique(nodes$phase))
            ph2row  <- setNames(seq_along(ph_nums), as.character(ph_nums))
            for (i in seq_len(nrow(phases))) {
                rws <- ph2row[as.character(seq.int(phases$phase_start[i],
                                                   phases$phase_end[i]))]
                rws <- as.integer(rws[!is.na(rws)])
                ## Per-row natural height = tallest box in that row; the band
                ## spans the rows plus one vpad between consecutive rows.
                rh <- vapply(rws, function(rr)
                    max(heights[nodes$row == rr], 0), numeric(1L))
                ext <- sum(rh) + max(length(rh) - 1L, 0L) * vpad_in
                ## Floor: at least a couple of phase line-heights, so a short
                ## single-row phase still has a usable (wrappable) target.
                ph_targets[i] <- max(ext, 2 * line_height * (cex_phase / 0.85))
            }
        }
        lab <- lapply(seq_len(nrow(phases)), function(i)
            measure_phase_label(phases$label[i], gp_ph_meas0, pad, tw0,
                                wrap = ml_wrap, max_lines = ml_max,
                                max_width_in = if (ml_wrap) ph_targets[i] else NULL))
        phase_lines <- lapply(lab, `[[`, "lines")
        label_h_in  <- vapply(lab, `[[`, numeric(1L), "height_in")
        n_line_max  <- max(vapply(lab, `[[`, integer(1L), "n_lines"))

        ## Strip thickness
        per_line_in   <- line_height * (cex_phase / 0.85)
        ph_box_w_in   <- max(phase_width, n_line_max * per_line_in)
        phase_gap_in  <- margin  # gap between phase boxes and content = margin
        phase_strip_w <- ph_box_w_in + phase_gap_in
    } else {
        ph_box_w_in   <- 0
        phase_gap_in  <- 0
        phase_strip_w <- 0
    }

    ## Content area dimensions
    content_w <- dev_w_in - 2 * margin - phase_strip_w
    content_h <- dev_h_in - 2 * margin

    ## Content viewport: right of phase strip, centered vertically
    content_x_center <- margin + phase_strip_w + content_w / 2
    content_y_center <- dev_h_in / 2

    pushViewport(viewport(
        x      = unit(content_x_center, "inches"),
        y      = unit(content_y_center, "inches"),
        width  = unit(content_w, "inches"),
        height = unit(content_h, "inches"),
        clip   = "off"
    ))

    ## gpar objects -- fontfamily applied uniformly so the diagram renders
    ## with the same typeface across cairo, ragg, quartz, and pdf devices.
    ff           <- if (nzchar(font_family)) font_family else NULL
    gp_main      <- gpar(cex = cex,                              fontfamily = ff)
    gp_main_bold <- gpar(cex = cex,             fontface = "bold",   fontfamily = ff)
    gp_side      <- gpar(cex = cex_side,                         fontfamily = ff)
    gp_side_bold <- gpar(cex = cex_side,        fontface = "bold",   fontfamily = ff)
    gp_reas      <- gpar(cex = cex_side * 0.92, fontface = "italic", fontfamily = ff)

    ## ---- Measurement helpers (all return inches) ----
    .tw_cache <- new.env(hash = TRUE, parent = emptyenv())
    tw_in <- function(label, gp) {
        key <- paste0(label, "\x01", paste(gp, collapse = "\x02"))
        cached <- .tw_cache[[key]]
        if (!is.null(cached)) return(cached)
        val <- convertWidth(grobWidth(textGrob(label, gp = gp)),
                            "inches", valueOnly = TRUE)
        .tw_cache[[key]] <- val
        val
    }

    ## ==== Pass 1a: Box sizes (inches) ====
    if (!"stream_group" %in% names(nodes)) nodes[, stream_group := NA_character_]
    if (!"sublabel" %in% names(nodes)) nodes[, sublabel := NA_character_]

    ## Establish two-level nested reasons
    .reason_is_parent <- function(v) length(v) > 1L || !is.null(names(v))
    expand_reason_rows <- function(r) {
        if (is.null(r) || length(r) == 0L) return(list())
        rows <- list()
        for (j in seq_along(r)) {
            v <- r[[j]]
            if (.reason_is_parent(v)) {
                rows[[length(rows) + 1L]] <-
                    list(label = names(r)[j], count = sum(v), sub = FALSE)
                for (k in seq_along(v))
                    rows[[length(rows) + 1L]] <-
                        list(label = names(v)[k], count = as.numeric(v[k]), sub = TRUE)
            } else {
                rows[[length(rows) + 1L]] <-
                    list(label = names(r)[j], count = as.numeric(v), sub = FALSE)
            }
        }
        rows
    }

    ## n_reason is the total number of displayed reason lines (parents plus
    ## sub-reasons), used for box height
    nodes[, n_reason := vapply(reasons,
                               function(r) length(expand_reason_rows(r)),
                               integer(1L))]

    n_nd <- nrow(nodes)
    bw_in <- numeric(n_nd)
    bh_in <- numeric(n_nd)

    ## Common text measurements
    indent_in       <- 0.10
    subindent_in    <- 0.22
    gap_side_in     <- tw_in("  ", gp_side_bold)
    gap_main_in     <- tw_in("  ", gp_main_bold)

    for (i in seq_len(n_nd)) {
        nd_role <- nodes$role[i]
        nd_text <- nodes$text[i]
        nd_n    <- nodes$n[i]
        nreas   <- nodes$n_reason[i]
        nd_reas <- nodes$reasons[[i]]

        if (nd_role == "side") {
            rows <- expand_reason_rows(nd_reas)
            if (count_first) {
                all_nums <- c(fmt_n(nd_n),
                              vapply(rows, function(r) fmt_n(r$count), character(1L)))
                num_col_w <- max(vapply(all_nums,
                                        function(s) tw_in(s, gp_side_bold), numeric(1L)))
                hdr_w <- num_col_w + gap_side_in + tw_in(nd_text, gp_side)
                max_reas_w <- 0
                for (row in rows) {
                    ind <- if (isTRUE(row$sub)) subindent_in else indent_in
                    rw  <- ind + num_col_w + gap_side_in + tw_in(row$label, gp_side)
                    max_reas_w <- max(max_reas_w, rw)
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            } else {
                lbl_w <- tw_in(paste0(nd_text, " "), gp_side_bold)
                cnt_w <- tw_in(paste0("(n = ", fmt_n(nd_n), ")"), gp_side)
                hdr_w <- lbl_w + cnt_w
                max_reas_w <- 0
                for (row in rows) {
                    ind  <- if (isTRUE(row$sub)) subindent_in else indent_in
                    rstr <- paste0(row$label, " (n = ", fmt_n(row$count), ")")
                    max_reas_w <- max(max_reas_w, tw_in(rstr, gp_side) + ind)
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            }
            bh_in[i] <- (1 + nreas) * side_lh + 2 * pad_v

        } else if (nd_role == "source") {
            ## Source group node: header line + indented sub-items (like side)
            nreas <- nodes$n_reason[i]
            nd_reas <- nodes$reasons[[i]]
            if (count_first) {
                ## Column layout: right-aligned count, left-aligned text
                all_nums <- fmt_n(nd_n)
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_w <- max(vapply(all_nums, function(s) tw_in(s, gp_main_bold), numeric(1L)))
                hdr_w <- num_col_w + gap_main_in + tw_in(nd_text, gp_main)
                max_reas_w <- 0
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    for (j in seq_along(nd_reas)) {
                        rw <- indent_in + num_col_w + gap_main_in + tw_in(names(nd_reas)[j], gp_main)
                        max_reas_w <- max(max_reas_w, rw)
                    }
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            } else {
                ## Standard: "Records identified (n = X)" header + indented sources
                lbl_w <- tw_in(paste0(nd_text, " "), gp_main_bold)
                cnt_w <- tw_in(paste0("(n = ", fmt_n(nd_n), ")"), gp_main)
                hdr_w <- lbl_w + cnt_w
                max_reas_w <- 0
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    for (j in seq_along(nd_reas)) {
                        rstr <- paste0(names(nd_reas)[j], " (n = ", fmt_n(nd_reas[j]), ")")
                        rw <- tw_in(rstr, gp_main) + indent_in
                        max_reas_w <- max(max_reas_w, rw)
                    }
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            }
            bh_in[i] <- (1 + nreas) * lh + 2 * pad_v

        } else if (nd_role == "source_header") {
            ## Column header: bold text, no count
            txt_w <- tw_in(nd_text, gp_main_bold)
            bw_in[i] <- txt_w + 2 * pad_h
            bh_in[i] <- 1 * lh + 2 * pad_v

        } else if (nd_role == "endpoint" && nreas > 0L) {
            ## Endpoint with sub-items (STARD final diagnosis)
            if (count_first) {
                cnt_str <- fmt_n(nd_n)
                cnt_w <- tw_in(cnt_str, gp_main_bold)
                hdr_w <- cnt_w + gap_main_in + tw_in(nd_text, gp_main)
                ## Sub-reasons: indented, italic count + italic label
                all_reas_nums <- vapply(nd_reas, fmt_n, character(1L))
                reas_num_w <- max(vapply(all_reas_nums,
                                         function(s) tw_in(s, gp_reas), numeric(1L)))
                max_reas_w <- 0
                for (ri in seq_len(nreas)) {
                    rw <- indent_in + reas_num_w + gap_side_in +
                        tw_in(names(nd_reas)[ri], gp_reas)
                    max_reas_w <- max(max_reas_w, rw)
                }
            } else {
                hdr_w <- tw_in(nd_text, gp_main_bold)
                cnt_w <- tw_in(paste0("n = ", fmt_n(nd_n)), gp_main)
                hdr_w <- max(hdr_w, cnt_w)
                ## Sub-reasons: centered italic, measured without indent
                max_reas_w <- 0
                for (ri in seq_len(nreas)) {
                    rn <- names(nd_reas)[ri]
                    rv <- fmt_n(nd_reas[ri])
                    rw <- tw_in(paste0(rn, " (n = ", rv, ")"), gp_reas)
                    max_reas_w <- max(max_reas_w, rw)
                }
            }
            bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            ## count_first: 1 header line + reason lines; default: 2 lines + reasons
            n_hdr_lines <- if (count_first) 1L else 2L
            bh_in[i] <- (n_hdr_lines + nreas) * lh + 2 * pad_v

        } else {
            has_lab <- nchar(nd_text) > 0L
            nd_sublabel <- nodes$sublabel[i]
            has_sub <- !is.na(nd_sublabel) && nchar(nd_sublabel) > 0L
            if (count_first && has_lab) {
                cnt_str <- fmt_n(nd_n)
                cnt_w <- tw_in(cnt_str, gp_main_bold)
                if (has_sub) {
                    ## Two lines: bold title centered (top),
                    ##            count + sublabel left-aligned (bottom)
                    title_w    <- tw_in(nd_text, gp_main_bold)
                    sub_line_w <- cnt_w + gap_main_in + tw_in(nd_sublabel, gp_main)
                    max_w   <- max(title_w, sub_line_w)
                    n_lines <- 2L
                } else {
                    ## Single line: bold count + gap + label
                    txt_w <- tw_in(nd_text, gp_main)
                    max_w   <- cnt_w + gap_main_in + txt_w
                    n_lines <- 1L
                }
                bw_in[i] <- max_w + 2 * pad_h
                bh_in[i] <- n_lines * lh + 2 * pad_v
            } else {
                txt_w <- if (has_lab) tw_in(nd_text, gp_main_bold) else 0
                cnt_w <- tw_in(paste0("N = ", fmt_n(nd_n)), gp_main)
                sub_w <- if (has_sub) tw_in(nd_sublabel, gp_main) else 0
                bw_in[i] <- max(txt_w, cnt_w, sub_w) + 2 * pad_h
                n_lines <- if (has_lab) 2L else 1L
                if (has_sub) n_lines <- n_lines + 1L
                bh_in[i] <- n_lines * lh + 2 * pad_v
            }
        }
    }

    nodes[, bw_inches := bw_in]
    nodes[, bh_inches := bh_in]

    ## ==== Pass 1b: Horizontal layout (inches) ====
    hpad_in <- vpad  # horizontal gap between adjacent columns

    arm_ids <- sort(unique(nodes[!is.na(arm_id), arm_id]))
    n_arms  <- length(arm_ids)
    has_sources <- nrow(nodes[role == "source"]) > 0L

    ## Factorial (two-level) layout
    is_factorial <- "arm_level" %chin% names(nodes) &&
        any(!is.na(nodes$arm_level) & nodes$arm_level == 2L)

    ## Widths of the widest main/side/source boxes per column
    pre_main_w <- max(c(nodes[is.na(arm_id) & role %chin% c("main", "alloc", "endpoint"), bw_inches], 0))
    pre_side_w <- max(c(nodes[is.na(arm_id) & role == "side", bw_inches], 0))

    ## Source column widths (by group)
    ## Each column's width = max(header width, source box width)
    src_section_w <- 0
    src_group_info <- NULL
    if (has_sources) {
        src_nodes <- nodes[role == "source"]
        hdr_nodes <- nodes[role == "source_header"]
        groups <- unique(src_nodes$stream_group)
        src_group_info <- lapply(groups, function(g) {
            src_w <- max(c(src_nodes[stream_group == g, bw_inches], 0))
            hdr_w <- max(c(hdr_nodes[stream_group == g, bw_inches], 0))
            list(group = g, max_w = max(src_w, hdr_w))
        })
        n_groups <- length(groups)
        group_widths <- vapply(src_group_info, function(g) g$max_w, numeric(1L))
        src_section_w <- sum(group_widths) + hpad_in * max(n_groups - 1L, 0L)
    }

    if (n_arms == 0L) {
        ## ---- 0-arm: main column + side column, sources above ----
        main_section_w <- pre_main_w + ifelse(pre_side_w > 0, hpad_in + pre_side_w, 0)
        total_content_w <- max(main_section_w, src_section_w)

        if (has_sources && !is.null(src_group_info)) {
            ## Anchor layout on source midpoint as center axis
            src_mid <- src_section_w / 2

            ## Space needed left and right of the convergence center
            left_of_center  <- max(src_mid, pre_main_w / 2)
            right_of_center <- max(src_mid,
                                   pre_main_w / 2 + ifelse(pre_side_w > 0,
                                                           hpad_in + pre_side_w, 0))
            total_content_w <- left_of_center + right_of_center

            ## main_x at center
            main_x_in <- left_of_center

            ## Position source columns centered on main_x
            src_start <- main_x_in - src_mid
            cursor <- src_start
            for (gi in seq_along(src_group_info)) {
                g <- src_group_info[[gi]]
                g_center <- cursor + g$max_w / 2
                gi_idx <- nodes$stream_group == g$group &
                    nodes$role %chin% c("source", "source_header")
                gi_idx[is.na(gi_idx)] <- FALSE
                set(nodes, i = which(gi_idx), j = "x_in", value = g_center)
                set(nodes, i = which(gi_idx), j = "bw_inches", value = g$max_w)
                cursor <- cursor + g$max_w + hpad_in
            }
        } else {
            main_x_in <- pre_main_w / 2
            if (main_section_w < total_content_w) {
                offset <- (total_content_w - main_section_w) / 2
                main_x_in <- offset + pre_main_w / 2
            }
        }

        side_left_in <- main_x_in + pre_main_w / 2 + hpad_in

        nodes[role %chin% c("main", "alloc", "endpoint") & is.na(arm_id), x_in := main_x_in]
        side_idx <- which(nodes$role == "side" & is.na(nodes$arm_id))
        for (si in side_idx) {
            set(nodes, i = si, j = "x_in",
                value = side_left_in + nodes$bw_inches[si] / 2)
        }

    } else {
        ## ---- Multi-arm layouts ----
        ## Measure per-arm column widths (single grouped aggregation)
        arm_nodes <- nodes[!is.na(arm_id)]
        arm_main_subset <- arm_nodes[role != "side"]
        arm_side_subset <- arm_nodes[role == "side"]
        arm_main_w <- numeric(n_arms)
        arm_side_w <- numeric(n_arms)
        if (nrow(arm_main_subset) > 0L) {
            arm_main_dt <- arm_main_subset[, .(w = max(bw_inches)), by = arm_id]
            arm_main_w[match(arm_main_dt$arm_id, arm_ids)] <- arm_main_dt$w
        }
        if (nrow(arm_side_subset) > 0L) {
            arm_side_dt <- arm_side_subset[, .(w = max(bw_inches)), by = arm_id]
            arm_side_w[match(arm_side_dt$arm_id, arm_ids)] <- arm_side_dt$w
        }

        pre_section_w <- pre_main_w + ifelse(pre_side_w > 0, hpad_in + pre_side_w, 0)

        if (is_factorial) {
            ## ---- Factorial (two-level) layout ----

            ## Arm tree: arm_parent / arm_level, deduplicated to one row per arm.
            arm_tree <- nodes[!is.na(arm_id),
                              .(arm_id, arm_parent, arm_level)]
            arm_tree <- arm_tree[!duplicated(arm_id)][order(arm_id)]
            is_parent <- function(a) any(arm_tree$arm_parent == a, na.rm = TRUE)
            leaf_mask <- !vapply(arm_tree$arm_id, is_parent, logical(1L))
            leaf_ids  <- arm_tree$arm_id[leaf_mask]
            par_ids   <- arm_tree$arm_id[!leaf_mask]

            ## Leaf column order
            lvl1 <- sort(arm_tree$arm_id[arm_tree$arm_level == 1L])
            leaf_seq <- integer(0L)
            for (p in lvl1) {
                kids <- sort(arm_tree$arm_id[which(arm_tree$arm_parent == p)])
                leaf_seq <- c(leaf_seq, if (length(kids)) kids else p)
            }
            leaf_seq <- c(leaf_seq, setdiff(leaf_ids, leaf_seq))
            n_leaf   <- length(leaf_seq)

            leaf_main_w <- arm_main_w[match(leaf_seq, arm_ids)]
            leaf_side_w <- arm_side_w[match(leaf_seq, arm_ids)]
            max_main_w  <- max(leaf_main_w)
            max_side_w  <- max(c(leaf_side_w, 0))
            col_w       <- max_main_w +
                ifelse(max_side_w > 0, hpad_in + max_side_w, 0)

            ## Center parent arm over children
            for (p in par_ids) {
                kids <- intersect(
                    arm_tree$arm_id[which(arm_tree$arm_parent == p)], leaf_seq)
                k_p  <- length(kids)
                if (k_p < 1L) next
                pbw <- nodes[arm_id == p & role != "side",
                             if (.N) max(bw_inches) else 0]
                need <- (pbw - (k_p - 1L) * hpad_in) / k_p
                if (length(need) && is.finite(need) && need > col_w) col_w <- need
            }

            arm_section_w <- n_leaf * col_w + hpad_in * (n_leaf - 1L)

            ## Per-leaf side-box direction
            leaf_dir <- vapply(leaf_seq, function(a) {
                pa <- arm_tree$arm_parent[arm_tree$arm_id == a]
                if (length(pa) != 1L || is.na(pa)) return("right")
                sibs <- sort(arm_tree$arm_id[which(arm_tree$arm_parent == pa)])
                if (length(sibs) == 2L && a == sibs[1L]) "left" else "right"
            }, character(1L))

            ## Level-1 parent splay
            two_lvl1 <- length(lvl1) == 2L
            par_dir  <- vapply(as.character(par_ids), function(p)
                if (two_lvl1 && as.integer(p) == lvl1[1L]) "left" else "right",
                character(1L))

            ## Leaf centers relative to arm_section_start = 0
            leaf_ctr0 <- numeric(n_leaf)
            for (k in seq_len(n_leaf)) {
                left <- identical(leaf_dir[[k]], "left") && max_side_w > 0
                cl   <- (k - 1L) * (col_w + hpad_in)
                leaf_ctr0[k] <- if (left) cl + col_w - max_main_w / 2
                                else      cl + max_main_w / 2
            }
            names(leaf_ctr0) <- as.character(leaf_seq)

            ## Recombining (pre-factorial stratification) arms reuse the level-1
            ## arm ids but collapse back to the trunk above the allocation split
            conv_i        <- which(edges$edge_type == "converge")
            trunk_nid     <- nodes$node_id[is.na(nodes$arm_id)]
            conv_i        <- conv_i[edges$to[conv_i] %in% trunk_nid]
            recomb_from   <- edges$from[conv_i]
            recomb_to     <- edges$to[conv_i]
            recomb_arm    <- nodes$node_id[!is.na(nodes$arm_id) &
                                           nodes$role == "arm" &
                                           nodes$node_id %in% recomb_from]
            recomb_arm_id <- unique(nodes$arm_id[match(recomb_arm, nodes$node_id)])

            ## Cost outboard parent side boxes (mirrors the dedicated two-arm
            ## branch's left_tail / right_tail)
            left_pad  <- 0
            right_pad <- 0
            for (p in par_ids) {
                if (p %in% recomb_arm_id) next
                kids <- intersect(
                    arm_tree$arm_id[which(arm_tree$arm_parent == p)], leaf_seq)
                if (!length(kids)) next
                psw <- arm_side_w[match(p, arm_ids)]
                if (is.na(psw) || psw <= 0) next
                pctr0 <- mean(leaf_ctr0[as.character(kids)])
                pw    <- arm_main_w[match(p, arm_ids)]
                if (identical(par_dir[[as.character(p)]], "left")) {
                    edge <- pctr0 - pw / 2 - hpad_in - psw
                    if (edge < 0) left_pad <- max(left_pad, -edge)
                } else {
                    edge <- pctr0 + pw / 2 + hpad_in + psw
                    if (edge > arm_section_w)
                        right_pad <- max(right_pad, edge - arm_section_w)
                }
            }

            eff_w             <- left_pad + arm_section_w + right_pad
            total_content_w   <- max(pre_section_w, eff_w, src_section_w)
            arm_section_start <- (total_content_w - eff_w) / 2 + left_pad

            ## Place each leaf column; record centers for parent / trunk centering
            cursor   <- arm_section_start
            leaf_ctr <- numeric(n_leaf)
            for (k in seq_len(n_leaf)) {
                a    <- leaf_seq[k]
                left <- identical(leaf_dir[[k]], "left") && max_side_w > 0
                ctr  <- if (left) cursor + col_w - max_main_w / 2
                        else      cursor + max_main_w / 2
                leaf_ctr[k] <- ctr
                nodes[arm_id == a & role != "side", x_in := ctr]
                if (!is.na(leaf_side_w[k]) && leaf_side_w[k] > 0) {
                    sidx <- which(nodes$arm_id == a & nodes$role == "side")
                    if (left) {
                        ## Flush-right against the arm's left edge (outboard left)
                        sr <- ctr - max_main_w / 2 - hpad_in
                        for (si in sidx)
                            set(nodes, i = si, j = "x_in",
                                value = sr - nodes$bw_inches[si] / 2)
                    } else {
                        ## Flush-left against the arm's right edge (outboard right)
                        sl <- ctr + max_main_w / 2 + hpad_in
                        for (si in sidx)
                            set(nodes, i = si, j = "x_in",
                                value = sl + nodes$bw_inches[si] / 2)
                    }
                }
                cursor <- cursor + col_w + hpad_in
            }
            names(leaf_ctr) <- as.character(leaf_seq)

            ## Center each parent arm over its children (mean of child centers)
            for (p in par_ids) {
                kids <- intersect(
                    arm_tree$arm_id[which(arm_tree$arm_parent == p)], leaf_seq)
                if (!length(kids)) next
                pctr <- mean(leaf_ctr[as.character(kids)])
                nodes[arm_id == p & role != "side", x_in := pctr]
                psw <- arm_side_w[match(p, arm_ids)]
                if (!is.na(psw) && psw > 0) {
                    pw    <- arm_main_w[match(p, arm_ids)]
                    psidx <- which(nodes$arm_id == p & nodes$role == "side")
                    if (identical(par_dir[[as.character(p)]], "left")) {
                        psr <- pctr - pw / 2 - hpad_in
                        for (si in psidx)
                            set(nodes, i = si, j = "x_in",
                                value = psr - nodes$bw_inches[si] / 2)
                    } else {
                        psl <- pctr + pw / 2 + hpad_in
                        for (si in psidx)
                            set(nodes, i = si, j = "x_in",
                                value = psl + nodes$bw_inches[si] / 2)
                    }
                }
            }

            ## Trunk (pre-split / post-combine) centered over all leaves
            arm_mid_x  <- (leaf_ctr[1L] + leaf_ctr[n_leaf]) / 2
            pre_main_x <- arm_mid_x
            nodes[is.na(arm_id) & role %chin% c("main", "alloc", "endpoint"),
                  x_in := pre_main_x]
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            pre_side_idx  <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx)
                set(nodes, i = si, j = "x_in",
                    value = pre_side_left + nodes$bw_inches[si] / 2)

            ## Reseat each recombining stratification group as a thin cluster on
            ## the trunk center, overriding the wide factorial-parent seats above
            if (length(recomb_arm)) {
                conv_tgt <- recomb_to[match(recomb_arm, recomb_from)]
                for (tg in unique(conv_tgt)) {
                    grp  <- recomb_arm[conv_tgt == tg]
                    grp  <- grp[order(nodes$arm_id[match(grp, nodes$node_id)])]
                    ng   <- length(grp)
                    gw   <- max(nodes$bw_inches[match(grp, nodes$node_id)])
                    step <- gw + hpad_in
                    for (j in seq_len(ng)) {
                        nid <- grp[j]
                        ctr <- arm_mid_x + (j - (ng + 1L) / 2) * step
                        set(nodes, which(nodes$node_id == nid), "x_in", ctr)
                        sb <- edges$to[edges$edge_type == "exclude" &
                                       edges$from == nid]
                        sb <- sb[sb %in% nodes$node_id[nodes$role == "side"]]
                        if (!length(sb)) next
                        left <- (ng == 2L && j == 1L)
                        for (si in sb) {
                            ix <- which(nodes$node_id == si)
                            bw <- nodes$bw_inches[ix]
                            x  <- if (left) ctr - gw / 2 - hpad_in - bw / 2
                                  else      ctr + gw / 2 + hpad_in + bw / 2
                            set(nodes, ix, j = "x_in", value = x)
                        }
                    }
                }
            }

        } else if (n_arms == 2L) {
            left_tail  <- if (arm_side_w[1] > 0) arm_side_w[1] + hpad_in else 0
            right_tail <- if (arm_side_w[2] > 0) hpad_in + arm_side_w[2] else 0

            ## Use the widest arm main box for BOTH arms so that
            ## split/combine arrows fan out symmetrically.
            max_main_w <- max(arm_main_w)
            inner_w    <- 2 * max_main_w + hpad_in

            ## Arm centers are at equal distances from arm_mid = 0
            half_span      <- (max_main_w + hpad_in) / 2
            left_main_rel  <- -half_span
                right_main_rel <-  half_span

            ## Pre-section centered on arm_mid = 0
            pre_right_ext <- pre_main_w / 2 +
                ifelse(pre_side_w > 0, hpad_in + pre_side_w, 0)

            ## Find absolute left/right extents from arm_mid
            right_extent <- max(inner_w / 2 + right_tail, pre_right_ext,
                                src_section_w / 2)
            left_extent  <- max(inner_w / 2 + left_tail, pre_main_w / 2,
                                src_section_w / 2)

            total_content_w <- left_extent + right_extent

            ## arm_mid in content coordinates = left_extent
            arm_mid_x <- left_extent

            ## Position arm main columns
            left_main_x  <- arm_mid_x + left_main_rel
            right_main_x <- arm_mid_x + right_main_rel
            nodes[arm_id == arm_ids[1] & role != "side", x_in := left_main_x]
            nodes[arm_id == arm_ids[2] & role != "side", x_in := right_main_x]

            ## Left arm side boxes: RIGHT-edge aligned (flush toward arm)
            if (arm_side_w[1] > 0) {
                left_side_right_edge <- arm_mid_x - inner_w / 2 - hpad_in
                si_left <- which(nodes$arm_id == arm_ids[1] & nodes$role == "side")
                for (si in si_left) {
                    set(nodes, i = si, j = "x_in",
                        value = left_side_right_edge - nodes$bw_inches[si] / 2)
                }
            }

            ## Right arm side boxes: LEFT-edge aligned (flush toward arm)
            if (arm_side_w[2] > 0) {
                right_side_left_edge <- arm_mid_x + inner_w / 2 + hpad_in
                si_right <- which(nodes$arm_id == arm_ids[2] & nodes$role == "side")
                for (si in si_right) {
                    set(nodes, i = si, j = "x_in",
                        value = right_side_left_edge + nodes$bw_inches[si] / 2)
                }
            }

            ## Pre-split and post-combine sections: centered on arm_mid
            pre_main_x <- arm_mid_x
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            nodes[is.na(arm_id) & role %chin% c("main", "alloc", "endpoint"), x_in := pre_main_x]
            pre_side_idx <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx) {
                set(nodes, i = si, j = "x_in",
                    value = pre_side_left + nodes$bw_inches[si] / 2)
            }

        } else {
            ## Use uniform column widths so split/combine arrows are centered.
            ## Each column gets the width of the widest arm column.
            max_main_w <- max(arm_main_w)
            max_side_w <- max(c(arm_side_w, 0))
            col_w <- max_main_w + ifelse(max_side_w > 0, hpad_in + max_side_w, 0)
            arm_section_w <- n_arms * col_w + hpad_in * (n_arms - 1L)

            total_content_w <- max(pre_section_w, arm_section_w, src_section_w)

            arm_section_start <- (total_content_w - arm_section_w) / 2
            cursor <- arm_section_start
            arm_main_centers <- numeric(n_arms)
            for (k in seq_along(arm_ids)) {
                a <- arm_ids[k]
                arm_center <- cursor + max_main_w / 2
                arm_main_centers[k] <- arm_center
                nodes[arm_id == a & role != "side", x_in := arm_center]
                if (arm_side_w[k] > 0) {
                    arm_side_left <- cursor + max_main_w + hpad_in
                    arm_side_idx <- which(nodes$arm_id == a & nodes$role == "side")
                    for (si in arm_side_idx) {
                        set(nodes, i = si, j = "x_in",
                            value = arm_side_left + nodes$bw_inches[si] / 2)
                    }
                }
                cursor <- cursor + col_w + hpad_in
            }

            arm_mid_x <- (arm_main_centers[1] + arm_main_centers[n_arms]) / 2
            pre_main_x <- arm_mid_x
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            nodes[is.na(arm_id) & role %chin% c("main", "alloc", "endpoint"), x_in := pre_main_x]
            pre_side_idx <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx) {
                set(nodes, i = si, j = "x_in",
                    value = pre_side_left + nodes$bw_inches[si] / 2)
            }
        }

        ## Position source groups if present
        if (has_sources && !is.null(src_group_info)) {
            src_start <- (total_content_w - src_section_w) / 2
            cursor <- src_start
            for (gi in seq_along(src_group_info)) {
                g <- src_group_info[[gi]]
                g_center <- cursor + g$max_w / 2
                nodes[role == "source" & stream_group == g$group,
                      x_in := g_center]
                nodes[role == "source_header" & stream_group == g$group,
                      x_in := g_center]
                cursor <- cursor + g$max_w + hpad_in
            }
        }
    }

    ## Store computed content width for recdims
    graph$content_width_in <- total_content_w

    ## ==== Pass 2: Vertical layout (inches) ====
    n_rows <- max(nodes$row)
    setkey(nodes, node_id)

    ## ---- Row heights ----
    main_roles <- c("side", "source", "source_header")
    row_h_in <- numeric(n_rows)

    ## Main/arm/alloc/endpoint boxes
    mn_subset <- nodes[!role %chin% main_roles]
    if (nrow(mn_subset) > 0L) {
        mn_h <- mn_subset[, .(h = max(bh_inches)), by = row]
        row_h_in[mn_h$row] <- mn_h$h
    }

    ## Source rows: consolidated box + header
    src_nodes <- nodes[role == "source"]
    if (nrow(src_nodes) > 0L) {
        sn_h <- src_nodes[, .(src_h = max(bh_inches)), by = row]
        sn_h[, hdr_h := 0]
        hdr_nodes <- nodes[role == "source_header"]
        if (nrow(hdr_nodes) > 0L) {
            hn_h <- hdr_nodes[, .(hdr_h = max(bh_inches)), by = row]
            sn_h[hn_h, hdr_h := i.hdr_h + vpad_in * 0.3, on = "row"]
        }
        sn_h[, total := src_h + hdr_h]
        row_h_in[sn_h$row] <- pmax(row_h_in[sn_h$row], sn_h$total)
    }

    ## ---- Pair gaps ----
    ## Rows with any non-side/non-header content get vpad; others get 0
    content_roles <- c("side", "source_header")
    rows_with_content <- unique(nodes[!role %chin% content_roles, row])
    pair_gap_in <- numeric(n_rows)
    pair_gap_in[rows_with_content] <- vpad_in
    ## ---- Exclude-edge gap ----
    excl_edges <- edges[edge_type == "exclude"]
    if (nrow(excl_edges) > 0L) {
        ## Join to get parent row and side box height
        ee <- copy(excl_edges)
        ee[nodes, on = .(from = node_id), from_row := i.row]
        ee[nodes, on = .(to = node_id), side_h := i.bh_inches]
        ee[, needed := side_h + 2 * vpad_in]

        ## Per-parent-row max needed
        ee_max <- ee[, .(needed = max(needed)), by = from_row]
        pair_gap_in[ee_max$from_row] <- pmax(pair_gap_in[ee_max$from_row],
                                             ee_max$needed)

        ## Stacked side boxes: parents with >1 exclude edge
        stack_dt <- ee[, .(n_sides = .N,
                           stack_h = sum(side_h) + (.N - 1L) * vpad_in,
                           from_row = from_row[1L]),
                       by = from][n_sides > 1L]
        if (nrow(stack_dt) > 0L) {
            stack_dt[, needed := stack_h + 2 * vpad_in]
            stack_max <- stack_dt[, .(needed = max(needed)), by = from_row]
            pair_gap_in[stack_max$from_row] <- pmax(
                pair_gap_in[stack_max$from_row], stack_max$needed)
        }
    }

    ## Double gap after every row that fans out a split, and after source rows
    alloc_rows  <- unique(nodes[role == "alloc", row])
    source_rows <- unique(nodes[role == "source", row])
    split_from_rows <- integer(0L)
    if (nrow(edges[edge_type == "split"]) > 0L) {
        se <- edges[edge_type == "split"]
        se[nodes, on = .(from = node_id), fr := i.row]
        split_from_rows <- unique(se$fr)
    }
    double_rows <- unique(c(alloc_rows, source_rows, split_from_rows))
    if (length(double_rows) > 0L) {
        pair_gap_in[double_rows] <- pmax(pair_gap_in[double_rows], 2 * vpad_in)
    }

    ## Extra gap for converge source rows
    conv_from_rows <- integer(0L)
    if (nrow(edges[edge_type == "converge"]) > 0L) {
        ce <- edges[edge_type == "converge"]
        ce[nodes, on = .(from = node_id), fr := i.row]
        conv_from_rows <- unique(ce$fr)
    }
    if (length(conv_from_rows) > 0L) {
        pair_gap_in[conv_from_rows] <- pair_gap_in[conv_from_rows] + vpad_in
    }

    ## Content height in inches
    total_h_in <- sum(row_h_in) + sum(pair_gap_in[seq_len(n_rows - 1L)])

    ## ---- Phase-label vertical fit (band model) ----
    ## Total deficit definition
    phase_deficit_in <- numeric(0)
    ## Inter-phase strip separation (inches); single source of truth shared
    ## by the deficit calc and the band-placement pass (historical 0.01 npc).
    ph_gap_in <- 0.01 * content_h
    if (has_phases && nrow(phases) > 0L) {
        phase_deficit_in <- phase_band_deficits(
            nodes, edges, phases, row_h_in, pair_gap_in,
            n_rows, vpad_in, ph_gap_in, label_h_in)
    }
    extra_band_in <- if (length(phase_deficit_in)) sum(phase_deficit_in) else 0
    ## Total content height = natural stack + band growth
    total_h_in <- sum(row_h_in) + sum(pair_gap_in[seq_len(n_rows - 1L)]) +
        extra_band_in

    ## Store for recdims(): total canvas = content + margins + phase strip
    graph$diagram_height_in <- total_h_in + 2 * margin
    graph$diagram_width_in  <- total_content_w + 2 * margin + phase_strip_w
    graph$phase_strip_w     <- phase_strip_w

    ## Dimension-only callers
    if (isTRUE(measure_only))
        return(invisible(graph))

    ## ---- Convert to npc of the content viewport ----

    to_npc_h <- function(inches) inches / content_h
    to_npc_w <- function(inches) inches / content_w

    nodes[, bw := to_npc_w(bw_inches)]
    nodes[, box_h := to_npc_h(bh_inches)]

    ## Convert inch-based X to content NPC
    nodes[, x := to_npc_w(x_in)]

    row_h <- to_npc_h(row_h_in)
    gap_npc <- to_npc_h(pair_gap_in)

    lh_npc      <- to_npc_h(lh)
    side_lh_npc <- to_npc_h(side_lh)
    pad_v_npc   <- to_npc_h(pad_v)
    pad_h_npc   <- to_npc_w(pad_h)
    indent_npc  <- to_npc_w(0.10)
    subindent_npc <- to_npc_w(0.22)

    ## Pre-compute NPC gap constants used repeatedly in the rendering loop
    gap_main_npc <- convertWidth(grobWidth(textGrob("  ", gp = gp_main_bold)),
                                 "npc", valueOnly = TRUE)
    gap_side_npc <- convertWidth(grobWidth(textGrob("  ", gp = gp_side_bold)),
                                 "npc", valueOnly = TRUE)

    ## NPC text width via memoized inch cache
    tw_npc <- function(label, gp) to_npc_w(tw_in(label, gp))

    ## Row y positions, natural top-down layout (no phase deficits yet)
    row_y <- numeric(n_rows)
    row_y[1L] <- 1.0 - row_h[1L] / 2   # top-anchored for now
    if (n_rows > 1L) {
        ## Drop between consecutive row centers:
        ## half-height of row r  +  gap below row r  +  half-height of row r+1
        deltas <- row_h[seq_len(n_rows - 1L)] / 2 +
            gap_npc[seq_len(n_rows - 1L)] +
            row_h[2:n_rows] / 2
        row_y[2:n_rows] <- row_y[1L] - cumsum(deltas)
    }

    row_y_map <- setNames(row_y, as.character(seq_len(n_rows)))

    ## Position non-side, non-source nodes at row centers
    nodes[!role %chin% c("side", "source", "source_header"), y := row_y_map[as.character(row)]]

    ## Position source nodes: one box per group, header above
    src_nodes_idx <- which(nodes$role == "source")
    hdr_nodes_idx <- which(nodes$role == "source_header")
    if (length(src_nodes_idx) > 0L) {
        src_row <- nodes$row[src_nodes_idx[1L]]
        row_center_y <- row_y_map[as.character(src_row)]

        ## Check if headers exist
        has_hdrs <- length(hdr_nodes_idx) > 0L

        if (has_hdrs) {
            ## Headers above, sources below, both within the row
            hdr_h_npc <- nodes$box_h[hdr_nodes_idx[1L]]
            gap_npc_s <- to_npc_h(vpad_in * 0.3)
            row_h_npc <- row_h[src_row]
            hdr_y <- row_center_y + row_h_npc / 2 - hdr_h_npc / 2
            nodes[role == "source_header", y := hdr_y]

            ## Source boxes centered in remaining space below header
            src_top_y <- hdr_y - hdr_h_npc / 2 - gap_npc_s
            for (si in src_nodes_idx) {
                set(nodes, i = si, j = "y",
                    value = src_top_y - nodes$box_h[si] / 2)
            }
        } else {
            ## No headers: center source boxes in row
            for (si in src_nodes_idx) {
                set(nodes, i = si, j = "y", value = row_center_y)
            }
        }
    }

    ## ==== Pass 3: Side-box Y ====
    ## When multiple side boxes share a parent, stack downward
    vpad_npc <- to_npc_h(vpad_in)

    ## Group exclude edges by from-node using split()
    excl_edges_dt <- edges[edge_type == "exclude"]
    if (nrow(excl_edges_dt) > 0L) {
        ## node_id is 1..n_nd sequential, matching row indices after setkey
        excl_groups <- split(excl_edges_dt, by = "from")

        for (grp in excl_groups) {
            from_id <- grp$from[1L]

            ## Start stacking from vpad below parent bottom
            y_cursor <- nodes$y[from_id] - nodes$box_h[from_id] / 2 - vpad_npc

            for (j in seq_len(nrow(grp))) {
                sid <- grp$to[j]
                side_h <- nodes$box_h[sid]
                mid_y  <- y_cursor - side_h / 2
                set(nodes, i = sid, j = "y", value = mid_y)
                y_cursor <- y_cursor - side_h - vpad_npc
            }
        }
    }

    ## ==== Pass 4: Phase bands ====
    ## Grow each band by its deficit, translate later phases down, recenter,
    ## and place content within each band (even gaps for multi-element
    ## phases, centered for single-element); returns per-phase strip edges.
    phase_band_top <- phase_band_bot <- NULL
    if (has_phases && nrow(phases) > 0L) {
        pb <- apply_phase_bands(nodes, edges, phases, phase_deficit_in,
                                to_npc_h, to_npc_w, vpad_in, ph_gap_in)
        phase_band_top <- pb$band_top    # per-phase strip TOP edge (npc)
        phase_band_bot <- pb$band_bot    # per-phase strip BOTTOM edge (npc)
    }

    ## Opt-in numeric geometry dump for debugging the vertical fit
    if (isTRUE(getOption("selecta.debug_layout", FALSE))) {
        dbg_band <- NULL
        if (!is.null(phase_band_top)) {
            dbg_band <- data.frame(
                phase     = seq_len(nrow(phases)),
                label     = phases$label,
                label_h_in = if (!is.null(label_h_in)) round(label_h_in, 4) else NA_real_,
                deficit_in = if (length(phase_deficit_in))
                                 round(phase_deficit_in, 4) else 0,
                band_top_npc = round(phase_band_top, 4),
                band_bot_npc = round(phase_band_bot, 4),
                band_h_npc   = round(phase_band_top - phase_band_bot, 4),
                band_h_in    = round((phase_band_top - phase_band_bot) * content_h, 4)
            )
        }
        dbg_nodes <- nodes[order(row, node_id),
                           .(node_id, row, role,
                             phase = if ("phase" %in% names(nodes)) phase else NA_integer_,
                             y      = round(y, 4),
                             top    = round(y + box_h / 2, 4),
                             bot    = round(y - box_h / 2, 4),
                             h_in   = round(box_h * content_h, 4))]
        dims_line <- sprintf("content_h=%.4f in  total_h=%.4f in  extra_band=%.4f in",
                             content_h, total_h_in, extra_band_in)
        debug_emit("export_grid() layout", dimensions = dims_line,
                   `phase bands` = dbg_band, nodes = dbg_nodes)
        ## Also stash on the returned graph for programmatic access.
        graph$debug_layout <- list(content_h_in = content_h,
                                   total_h_in   = total_h_in,
                                   extra_band_in = extra_band_in,
                                   bands = dbg_band, nodes = dbg_nodes)
    }

    ## :::: Draw graph ::::

    arr <- arrow(length = unit(0.10, "inches"), type = "closed")

    ## ---- Phase labels (drawn at negative x in content viewport) ----

    if (has_phases) {
        gp_ph_text <- gpar(cex = cex_phase, col = phase_text_col,
                           fontface = "bold", fontfamily = ff)
        gp_ph_box  <- gpar(fill = phase_fill, col = phase_fill, lwd = 0)

        ph_box_w_npc <- to_npc_w(ph_box_w_in)
        ph_x_npc     <- to_npc_w(-(phase_gap_in + ph_box_w_in / 2))

        n_ph <- nrow(phases)

        ## Per-phase band edges from the band pass are authoritative: each
        ## is already tall enough for its label and separated by ph_gap, so
        ## strips are drawn directly with no midpoint estimation.
        bt_vec <- phase_band_top
        bb_vec <- phase_band_bot
        per_line_w <- to_npc_w(line_height * (cex_phase / 0.85))

        for (idx in seq_len(n_ph)) {
            top <- bt_vec[idx]
            bot <- bb_vec[idx]
            ht  <- top - bot
            ym  <- (top + bot) / 2
            grid.rect(x = unit(ph_x_npc, "npc"), y = unit(ym, "npc"),
                      width = unit(ph_box_w_npc, "npc"),
                      height = unit(ht, "npc"),
                      gp = gp_ph_box)
            ## Draw the (possibly multi-line) label as rotated rows of text
            ## offset along x, centered on ph_x_npc.
            lines_i <- if (!is.null(phase_lines)) phase_lines[[idx]] else phases$label[idx]
            n_li    <- length(lines_i)
            if (n_li <= 1L) {
                grid.text(lines_i[[1L]],
                          x = unit(ph_x_npc, "npc"), y = unit(ym, "npc"),
                          rot = 90, gp = gp_ph_text, just = "center")
            } else {
                ## Lines read left-to-right: the first line is the leftmost
                ## (outer) column, each subsequent line one line-width to the
                ## right (toward the content edge).
                x0 <- ph_x_npc - (n_li - 1) / 2 * per_line_w
                for (li in seq_len(n_li)) {
                    grid.text(lines_i[[li]],
                              x = unit(x0 + (li - 1) * per_line_w, "npc"),
                              y = unit(ym, "npc"),
                              rot = 90, gp = gp_ph_text, just = "center")
                }
            }
        }
    }

    ## ---- Edges ----

    gp_edge <- gpar(col = arrow_col, lwd = lwd, fill = arrow_col)

    ## Shared horizontal bar Y for converge edges
    converge_bar_y <- NULL
    conv_idx <- which(edges$edge_type == "converge")
    if (length(conv_idx) > 0L) {
        conv_edges <- edges[conv_idx]
        ## Join from/to node positions and rows
        conv_edges[nodes, on = .(from = node_id),
                   `:=`(from_bot  = i.y - i.box_h / 2,
                        from_arm  = i.arm_id,
                        from_row  = i.row)]
        conv_edges[nodes, on = .(to = node_id),
                   `:=`(to_top = i.y + i.box_h / 2,
                        to_row = i.row)]

        bar_dt <- conv_edges[, {
            lowest <- min(from_bot)
            row_lo <- min(from_row)
            row_hi <- to_row[1L]

            ## Only consider side boxes in the same arm columns and
            ## within the row range of this split-combine span
            from_arm_ids <- unique(from_arm)
            from_arm_ids <- from_arm_ids[!is.na(from_arm_ids)]
            if (length(from_arm_ids) > 0L) {
                arm_sides <- nodes[role == "side" &
                                   arm_id %in% from_arm_ids &
                                   row >= row_lo & row <= row_hi]
                if (nrow(arm_sides) > 0L) {
                    side_bots <- arm_sides$y - arm_sides$box_h / 2
                    lowest <- min(lowest, side_bots)
                }
            }

            .(bar_y = (lowest + to_top[1L]) / 2)
        }, by = to]

        converge_bar_y <- setNames(bar_dt$bar_y, as.character(bar_dt$to))
    }

    ## Join from/to node positions onto edges
    ed <- copy(edges)
    ed[nodes, on = .(from = node_id), `:=`(fx = x, fy = y, fbh = box_h)]
    ed[nodes, on = .(to = node_id), `:=`(tx = x, ty = y, tbh = box_h, tbw = bw)]

    ## ---- Flow edges ----
    ed_simple <- ed[edge_type == "flow"]
    if (nrow(ed_simple) > 0L) {
        grid.segments(
            x0 = unit(ed_simple$fx, "npc"),
            y0 = unit(ed_simple$fy - ed_simple$fbh / 2, "npc"),
            x1 = unit(ed_simple$tx, "npc"),
            y1 = unit(ed_simple$ty + ed_simple$tbh / 2, "npc"),
            gp = gp_edge, arrow = arr)
    }

    ## ---- Exclude edges ----
    ed_excl <- ed[edge_type == "exclude"]
    if (nrow(ed_excl) > 0L) {
        excl_to_x <- fifelse(ed_excl$tx > ed_excl$fx,
                             ed_excl$tx - ed_excl$tbw / 2,
                             ed_excl$tx + ed_excl$tbw / 2)
        grid.segments(
            x0 = unit(ed_excl$fx, "npc"), y0 = unit(ed_excl$ty, "npc"),
            x1 = unit(excl_to_x, "npc"),  y1 = unit(ed_excl$ty, "npc"),
            gp = gp_edge, arrow = arr)
    }

    ## ---- Split edges ----
    ed_split <- ed[edge_type == "split"]
    for (i in seq_len(nrow(ed_split))) {
        e <- ed_split[i]
        drop_y <- (e$fy - e$fbh / 2 + e$ty + e$tbh / 2) / 2
        grid.lines(x = unit(c(e$fx, e$fx), "npc"),
                   y = unit(c(e$fy - e$fbh / 2, drop_y), "npc"), gp = gp_edge)
        grid.lines(x = unit(c(e$fx, e$tx), "npc"),
                   y = unit(c(drop_y, drop_y), "npc"), gp = gp_edge)
        grid.lines(x = unit(c(e$tx, e$tx), "npc"),
                   y = unit(c(drop_y, e$ty + e$tbh / 2), "npc"),
                   gp = gp_edge, arrow = arr)
    }

    ed_conv <- ed[edge_type == "converge"]
    for (i in seq_len(nrow(ed_conv))) {
        e <- ed_conv[i]
        bar_y <- converge_bar_y[[as.character(e$to)]]
        grid.lines(x = unit(c(e$fx, e$fx), "npc"),
                   y = unit(c(e$fy - e$fbh / 2, bar_y), "npc"), gp = gp_edge)
        grid.lines(x = unit(c(e$fx, e$tx), "npc"),
                   y = unit(c(bar_y, bar_y), "npc"), gp = gp_edge)
        grid.lines(x = unit(c(e$tx, e$tx), "npc"),
                   y = unit(c(bar_y, e$ty + e$tbh / 2), "npc"),
                   gp = gp_edge, arrow = arr)
    }

    ## ---- Boxes ----

    nodes[, fill_col := fifelse(role == "side", side_fill,
                                fifelse(role == "source_header", "#d0d0d0", box_fill))]

    for (fc in unique(nodes$fill_col)) {
        batch <- nodes[fill_col == fc]
        grid.rect(x = unit(batch$x, "npc"), y = unit(batch$y, "npc"),
                  width = unit(batch$bw, "npc"), height = unit(batch$box_h, "npc"),
                  gp = gpar(fill = fc, col = border_col, lwd = lwd))
    }

    ## ---- Text ----

    for (i in seq_len(n_nd)) {
        nd_x    <- nodes$x[i]
        nd_y    <- nodes$y[i]
        nd_bw   <- nodes$bw[i]
        nd_bh   <- nodes$box_h[i]
        nd_role <- nodes$role[i]
        nd_text <- nodes$text[i]
        nd_n    <- nodes$n[i]
        nd_reas <- nodes$reasons[[i]]
        nd_id   <- nodes$node_id[i]
        nd_sublabel <- nodes$sublabel[i]

        if (nd_role == "side") {
            left_x  <- nd_x - nd_bw / 2 + pad_h_npc
            rows    <- expand_reason_rows(nd_reas)
            n_reas  <- length(rows)
            block_h <- (1 + n_reas) * side_lh_npc
            top_y   <- nd_y + block_h / 2 - side_lh_npc / 2

            if (count_first) {
                all_nums <- c(fmt_n(nd_n),
                              vapply(rows, function(r) fmt_n(r$count), character(1L)))
                num_col_npc <- max(vapply(all_nums, function(s) {
                    tw_npc(s, gp_side_bold)
                }, numeric(1L)))
                text_x <- left_x + num_col_npc + gap_side_npc

                grid.text(fmt_n(nd_n), x = unit(left_x + num_col_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_side_bold, just = c("right", "center"))
                grid.text(nd_text, x = unit(text_x, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_side, just = c("left", "center"))
            } else {
                grid.text(nd_text, x = unit(left_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_side_bold, just = c("left", "center"))
                lbl_npc <- tw_npc(paste0(nd_text, " "), gp_side_bold)
                grid.text(bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_n)) * ")"),
                          x = unit(left_x + lbl_npc, "npc"), y = unit(top_y, "npc"),
                          gp = gp_side, just = c("left", "center"))
            }

            for (m in seq_along(rows)) {
                row <- rows[[m]]
                ry  <- top_y - m * side_lh_npc
                ind <- if (isTRUE(row$sub)) subindent_npc else indent_npc
                if (count_first) {
                    grid.text(fmt_n(row$count),
                              x = unit(left_x + ind + num_col_npc, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_side_bold, just = c("right", "center"))
                    grid.text(row$label,
                              x = unit(left_x + ind + num_col_npc + gap_side_npc, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_side, just = c("left", "center"))
                } else {
                    grid.text(row$label,
                              x = unit(left_x + ind, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_side, just = c("left", "center"))
                    rn_npc <- tw_npc(paste0(row$label, " "), gp_side)
                    grid.text(
                        bquote("(" * italic("n") ~ "=" ~ .(fmt_n(row$count)) * ")"),
                        x = unit(left_x + ind + rn_npc, "npc"),
                        y = unit(ry, "npc"),
                        gp = gp_side, just = c("left", "center"))
                }
            }

        } else if (nd_role == "alloc") {
            if (count_first) {
                left_x <- nd_x - nd_bw / 2 + pad_h_npc
                cnt_str <- fmt_n(nd_n)
                cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main_bold, just = c("right", "center"))
                grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main, just = c("left", "center"))
            } else {
                sep <- lh_npc * 0.55
                grid.text(nd_text, x = unit(nd_x, "npc"),
                          y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "center")
                grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                          x = unit(nd_x, "npc"),
                          y = unit(nd_y - sep, "npc"), gp = gp_main, just = "center")
            }

        } else if (nd_role == "source") {
            ## Consolidated source box: header line + indented sub-items
            nd_reas <- nodes$reasons[[i]]
            n_reas  <- nodes$n_reason[i]
            left_x  <- nd_x - nd_bw / 2 + pad_h_npc
            top_y   <- nd_y + nd_bh / 2 - pad_v_npc - lh_npc / 2

            if (count_first) {
                ## Column layout: right-aligned count, left-aligned text
                all_nums <- fmt_n(nd_n)
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_npc_s <- max(vapply(all_nums, function(s)
                    tw_npc(s, gp_main_bold), numeric(1L)))

                ## Header line
                grid.text(fmt_n(nd_n),
                          x = unit(left_x + num_col_npc_s, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = c("right", "center"))
                grid.text(nd_text,
                          x = unit(left_x + num_col_npc_s + gap_main_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main, just = c("left", "center"))

                ## Indented sub-items
                if (n_reas > 0L) {
                    for (j in seq_len(n_reas)) {
                        ry <- top_y - j * lh_npc
                        grid.text(fmt_n(nd_reas[j]),
                                  x = unit(left_x + indent_npc + num_col_npc_s, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main_bold, just = c("right", "center"))
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc + num_col_npc_s + gap_main_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main, just = c("left", "center"))
                    }
                }
            } else {
                ## Standard: "Records identified (n = X)" + indented sources
                grid.text(nd_text,
                          x = unit(left_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = c("left", "center"))
                lbl_npc <- tw_npc(paste0(nd_text, " "), gp_main_bold)
                grid.text(
                    bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_n)) * ")"),
                    x = unit(left_x + lbl_npc, "npc"), y = unit(top_y, "npc"),
                    gp = gp_main, just = c("left", "center"))

                ## Indented sub-items
                if (n_reas > 0L) {
                    for (j in seq_len(n_reas)) {
                        ry <- top_y - j * lh_npc
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main, just = c("left", "center"))
                        rn_npc <- tw_npc(paste0(names(nd_reas)[j], " "), gp_main)
                        grid.text(
                            bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_reas[j])) * ")"),
                            x = unit(left_x + indent_npc + rn_npc, "npc"),
                            y = unit(ry, "npc"),
                            gp = gp_main, just = c("left", "center"))
                    }
                }
            }

        } else if (nd_role == "source_header") {
            ## Column header: bold centered text, distinct background
            grid.text(nd_text, x = unit(nd_x, "npc"), y = unit(nd_y, "npc"),
                      gp = gp_main_bold, just = "center")

        } else if (nd_role == "endpoint" && !is.null(nd_reas) && length(nd_reas) > 0L) {
            ## Endpoint with sub-items (STARD final diagnosis)
            n_reas <- length(nd_reas)
            n_hdr_lines <- if (count_first) 1L else 2L
            total_lines <- n_hdr_lines + n_reas
            block_h <- total_lines * lh_npc
            top_y <- nd_y + block_h / 2 - lh_npc / 2
            left_x <- nd_x - nd_bw / 2 + pad_h_npc

            if (count_first) {
                ## Single header line: bold count + label, left-aligned
                cnt_str <- fmt_n(nd_n)
                cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = c("right", "center"))
                grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main, just = c("left", "center"))
            } else {
                grid.text(nd_text, x = unit(nd_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = "center")
                grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                          x = unit(nd_x, "npc"), y = unit(top_y - lh_npc, "npc"),
                          gp = gp_main, just = "center")
            }

            ## Sub-reasons: smaller italic font
            if (count_first) {
                ## Indented, right-aligned italic count + italic label
                all_reas_nums <- vapply(nd_reas, fmt_n, character(1L))
                reas_num_npc <- max(vapply(all_reas_nums,
                                           function(s) tw_npc(s, gp_reas), numeric(1L)))
                for (j in seq_len(n_reas)) {
                    ry <- top_y - (n_hdr_lines - 1 + j) * lh_npc
                    rc <- fmt_n(nd_reas[j])
                    grid.text(rc,
                              x = unit(left_x + indent_npc + reas_num_npc, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_reas, just = c("right", "center"))
                    grid.text(names(nd_reas)[j],
                              x = unit(left_x + indent_npc + reas_num_npc +
                                       gap_side_npc, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_reas, just = c("left", "center"))
                }
            } else {
                ## Centered italic
                for (j in seq_len(n_reas)) {
                    ry <- top_y - (n_hdr_lines - 1 + j) * lh_npc
                    rn <- names(nd_reas)[j]
                    rv <- fmt_n(nd_reas[j])
                    rstr <- paste0(rn, " (n = ", rv, ")")
                    grid.text(rstr,
                              x = unit(nd_x, "npc"), y = unit(ry, "npc"),
                              gp = gp_reas, just = "center")
                }
            }

        } else {
            has_lab <- nchar(nd_text) > 0L
            has_sub <- !is.na(nd_sublabel) && nchar(nd_sublabel) > 0L
            n_let <- if (nd_id == 1L && nd_role != "source") "N" else "n"

            if (count_first && has_lab) {
                if (has_sub) {
                    ## Two lines: bold title centered (top),
                    ##            count + sublabel left-aligned (bottom)
                    sep <- lh_npc * 0.55
                    grid.text(nd_text, x = unit(nd_x, "npc"),
                              y = unit(nd_y + sep, "npc"),
                              gp = gp_main_bold, just = "center")
                    left_x <- nd_x - nd_bw / 2 + pad_h_npc
                    cnt_str <- fmt_n(nd_n)
                    cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                    grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                              y = unit(nd_y - sep, "npc"),
                              gp = gp_main_bold, just = c("right", "center"))
                    grid.text(nd_sublabel,
                              x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                              y = unit(nd_y - sep, "npc"),
                              gp = gp_main, just = c("left", "center"))
                } else {
                    ## Single line: bold count left, non-bold label right
                    left_x <- nd_x - nd_bw / 2 + pad_h_npc
                    cnt_str <- fmt_n(nd_n)
                    cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                    grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                              y = unit(nd_y, "npc"),
                              gp = gp_main_bold, just = c("right", "center"))
                    grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                              y = unit(nd_y, "npc"),
                              gp = gp_main, just = c("left", "center"))
                }
            } else {
                if (has_lab && has_sub) {
                    ## Three lines: label (top), sublabel (middle), count (bottom)
                    grid.text(nd_text, x = unit(nd_x, "npc"),
                              y = unit(nd_y + lh_npc, "npc"),
                              gp = gp_main_bold, just = "center")
                    grid.text(nd_sublabel, x = unit(nd_x, "npc"),
                              y = unit(nd_y, "npc"),
                              gp = gp_main, just = "center")
                    grid.text(bquote(italic(.(n_let)) ~ "=" ~ .(fmt_n(nd_n))),
                              x = unit(nd_x, "npc"),
                              y = unit(nd_y - lh_npc, "npc"),
                              gp = gp_main, just = "center")
                } else if (has_lab) {
                    sep <- lh_npc * 0.55
                    grid.text(nd_text, x = unit(nd_x, "npc"),
                              y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "center")
                    grid.text(bquote(italic(.(n_let)) ~ "=" ~ .(fmt_n(nd_n))),
                              x = unit(nd_x, "npc"),
                              y = unit(nd_y - sep, "npc"), gp = gp_main, just = "center")
                } else {
                    grid.text(bquote(italic(.(n_let)) ~ "=" ~ .(fmt_n(nd_n))),
                              x = unit(nd_x, "npc"), y = unit(nd_y, "npc"),
                              gp = gp_main, just = "center")
                }
            }
        }
    }

    popViewport()

    ## Remove transient rendering columns
    for (col in c("fill_col", "n_reason"))
        if (col %in% names(nodes)) set(nodes, j = col, value = NULL)

    invisible(graph)
}

### * Phase-label vertical-fit helpers

#' Measure a (Possibly Wrapped) Phase Label
#'
#' Returns the rotated-height demand of a phase label and the lines it
#' splits to.  Phase labels are drawn rotated 90 degrees, so the relevant
#' demand on the strip is the unrotated width of the longest line, plus
#' vertical padding.  Explicit \code{"\n"} newlines are ALWAYS honored
#' and are never collapsed.  Greedy word-wrapping is applied to each
#' hard-split segment only when \code{wrap = TRUE} (with a
#' \code{max_width_in} cap); the \code{max_lines} cap then limits only the
#' \emph{wrap}-generated lines within a segment, never merging across
#' explicit newlines.  Leading/trailing whitespace around each line is
#' trimmed so a stray space (e.g. \code{"A \n test"}) does not inflate the
#' measured width or the rendered line.
#'
#' @param label Character scalar phase label.
#' @param gp A \code{gpar} for measurement (font face/size/family).
#' @param pad_v Numeric. Vertical padding added to both ends (inches).
#' @param tw A measurement function \code{function(label, gp)} returning
#'   the unrotated text width in inches.
#' @param wrap Logical. If \code{TRUE}, word-wrap over-long segments.
#'   Default \code{FALSE} (explicit newlines still split).
#' @param max_lines Integer or \code{NA}. Cap on wrap-generated lines per
#'   hard segment; overflow is collapsed into that segment's final line.
#'   \code{NA} (default) means no cap.
#' @param max_width_in Numeric or \code{NULL}. Wrap cap (inches).
#' @return A list with \code{lines} (character vector), \code{n_lines}
#'   (integer), and \code{height_in} (numeric, the rotated strip height).
#' @keywords internal
measure_phase_label <- function(label, gp, pad_v, tw,
                                wrap = FALSE, max_lines = NA_integer_,
                                max_width_in = NULL) {
    ## Explicit newlines always split, independent of `wrap`.
    hard <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    if (length(hard) == 0L) hard <- ""
    hard <- trimws(hard)

    wrap_seg <- function(s) {
        if (!isTRUE(wrap) || is.null(max_width_in)) return(s)
        words <- strsplit(s, "\\s+")[[1L]]
        words <- words[nzchar(words)]
        if (length(words) == 0L) return(s)
        lines <- character(0L); cur <- ""
        for (w in words) {
            cand <- if (nzchar(cur)) paste(cur, w) else w
            if (tw(cand, gp) > max_width_in && nzchar(cur)) {
                lines <- c(lines, cur); cur <- w
            } else {
                cur <- cand
            }
        }
        if (nzchar(cur)) lines <- c(lines, cur)
        ## Cap only the wrap lines of THIS segment; collapse the overflow
        ## into the segment's last line (never across hard newlines).
        if (!is.na(max_lines) && max_lines >= 1L && length(lines) > max_lines) {
            keep <- if (max_lines > 1L) lines[seq_len(max_lines - 1L)] else character(0L)
            tail_lines <- lines[max(1L, max_lines):length(lines)]
            lines <- c(keep, paste(tail_lines, collapse = " "))
        }
        lines
    }

    lines <- unlist(lapply(hard, wrap_seg), use.names = FALSE)
    if (length(lines) == 0L) lines <- ""

    longest <- max(vapply(lines, function(s) tw(s, gp), numeric(1L)))
    list(lines     = lines,
         n_lines   = length(lines),
         height_in = longest + 2 * pad_v)
}


#' Place Rows in Inches (Top-Down)
#'
#' Single monotone top-down placement of every node in distance-from-top
#' inches, used by the phase-fit pass to measure phase extents from
#' actual node positions.  Anchoring (non-side) boxes sit at their row
#' centers; side boxes hang \code{vpad_in} below their exclude-edge
#' parent and stack downward, exactly as in the main rendering pass --
#' so a phase's measured extent includes side boxes that hang off a
#' neighboring phase's row.
#'
#' @param nodes Node \code{data.table} with \code{node_id}, \code{role},
#'   \code{row}, \code{bh_inches}.
#' @param edges Edge \code{data.table} with \code{edge_type}, \code{from},
#'   \code{to}.
#' @param row_h_in Numeric vector of row heights (inches), length n_rows.
#' @param pair_gap_in Numeric vector of gaps below each row (inches).
#' @param n_rows Integer number of rows.
#' @param vpad_in Numeric vertical pad (inches).
#' @param lead_in Numeric leading pad above the first row (inches).
#' @return A list with \code{top}, \code{bot} (numeric vectors aligned to
#'   \code{nodes} row order), \code{d_row}, and \code{bottom_in}.
#' @keywords internal
place_rows_in <- function(nodes, edges, row_h_in, pair_gap_in,
                          n_rows, vpad_in, lead_in = 0) {
    d_row <- numeric(n_rows)
    d_row[1L] <- lead_in + row_h_in[1L] / 2
    if (n_rows > 1L) for (r in 2:n_rows)
                         d_row[r] <- d_row[r - 1L] + row_h_in[r - 1L] / 2 +
                             pair_gap_in[r - 1L] + row_h_in[r] / 2

    nn  <- nrow(nodes)
    top <- bot <- rep(NA_real_, nn)
    is_side <- nodes$role == "side"

    ## Anchoring boxes at row centers
    mi <- which(!is_side)
    if (length(mi) > 0L) {
        cr <- d_row[nodes$row[mi]]
        top[mi] <- cr - nodes$bh_inches[mi] / 2
        bot[mi] <- cr + nodes$bh_inches[mi] / 2
    }

    ## Source rows: header sits atop the source box (a contiguous stack),
    ## mirroring the main rendering pass
    src_idx <- which(nodes$role == "source")
    hdr_idx <- which(nodes$role == "source_header")
    if (length(src_idx) > 0L && length(hdr_idx) > 0L) {
        gap_s <- vpad_in * 0.3
        for (r in unique(nodes$row[src_idx])) {
            rc      <- d_row[r]
            row_top <- rc - row_h_in[r] / 2
            hh      <- max(nodes$bh_inches[hdr_idx[nodes$row[hdr_idx] == r]])
            ## Header flush to the row top.
            hr <- hdr_idx[nodes$row[hdr_idx] == r]
            top[hr] <- row_top
            bot[hr] <- row_top + nodes$bh_inches[hr]
            ## Source boxes immediately below the header band.
            sr <- src_idx[nodes$row[src_idx] == r]
            top[sr] <- row_top + hh + gap_s
            bot[sr] <- top[sr] + nodes$bh_inches[sr]
        }
    }

    ## Side boxes hang off their exclude-edge parent, stacked downward
    excl <- edges[edge_type == "exclude"]
    if (nrow(excl) > 0L) {
        for (grp in split(excl, by = "from")) {
            pix <- match(grp$from[1L], nodes$node_id)
            if (is.na(pix)) next
            cur <- d_row[nodes$row[pix]] + nodes$bh_inches[pix] / 2 + vpad_in
            for (j in seq_len(nrow(grp))) {
                sid <- match(grp$to[j], nodes$node_id)
                if (is.na(sid)) next
                top[sid] <- cur
                bot[sid] <- cur + nodes$bh_inches[sid]
                cur <- bot[sid] + vpad_in
            }
        }
    }

    list(top = top, bot = bot, d_row = d_row,
         bottom_in = max(bot, na.rm = TRUE))
}


#' Per-Phase Band Deficits
#'
#' Lays the rows out naturally (in inches) and returns, for each phase,
#' the vertical deficit \code{D_i = max(0, label_height_i - natural_band_i)}.
#' The natural band is the phase's slice of the diagram: the two terminal
#' phases extend \code{vpad_in/4} past the outermost node, and interior
#' boundaries fall at the half-way line between neighboring phase content
#' but stop \code{ph_gap_in/2} short on each side so adjacent strips are
#' separated by \code{ph_gap_in}.  Phase extents are measured from final
#' node positions, so a side box hanging off a neighboring phase's row is
#' attributed to its own phase.  These deficits are consumed by
#' \code{apply_phase_bands()}; their sum is the extra canvas height needed.
#'
#' @param nodes,edges,phases Graph components.
#' @param row_h_in,pair_gap_in Natural row heights and gaps (inches).
#' @param n_rows Integer row count.
#' @param vpad_in Numeric vertical pad (inches); terminal overhang is
#'   \code{vpad_in/4}.
#' @param ph_gap_in Numeric separation between adjacent strips (inches).
#' @param label_h_in Numeric vector (one per phase) of required band
#'   heights (rotated label height incl. padding).
#' @return Numeric vector of length \code{nrow(phases)} of deficits (in).
#' @keywords internal
phase_band_deficits <- function(nodes, edges, phases, row_h_in, pair_gap_in,
                                n_rows, vpad_in, ph_gap_in, label_h_in) {
    n_ph <- nrow(phases)
    if (n_ph == 0L) return(numeric(0))

    ph_nums <- sort(unique(nodes$phase))
    ph2row  <- setNames(seq_along(ph_nums), as.character(ph_nums))
    phase_rows <- function(i) {
        r <- ph2row[as.character(seq.int(phases$phase_start[i],
                                         phases$phase_end[i]))]
        as.integer(r[!is.na(r)])
    }

    pl <- place_rows_in(nodes, edges, row_h_in, pair_gap_in,
                        n_rows, vpad_in, lead_in = 0)
    et <- eb <- rep(NA_real_, n_ph)
    for (i in seq_len(n_ph)) {
        idx <- which(nodes$row %in% phase_rows(i))
        if (length(idx) == 0L) next
        et[i] <- min(pl$top[idx], na.rm = TRUE)   # d from top, smaller = higher
        eb[i] <- max(pl$bot[idx], na.rm = TRUE)
    }

    ## Natural band boundaries in d-from-top space (descending d)
    overhang <- vpad_in / 4
    bt <- bb <- numeric(n_ph)
    bt[1L]   <- et[1L] - overhang
    bb[n_ph] <- eb[n_ph] + overhang
    if (n_ph > 1L) for (k in seq_len(n_ph - 1L)) {
                       mid     <- (eb[k] + et[k + 1L]) / 2
                       bb[k]       <- mid - ph_gap_in / 2
                       bt[k + 1L]  <- mid + ph_gap_in / 2
                   }
    nat_h <- bb - bt

    pmax(0, label_h_in - nat_h)
}


#' Apply Phase Bands (Grow, Translate, Place Content)
#'
#' Given nodes already positioned in content NPC and a per-phase deficit
#' vector, grows each phase band by its own deficit, rigidly translates
#' every later phase downward by the cumulative deficit above it, and
#' vertically recenters the whole (taller) diagram.  Band geometry mirrors
#' \code{phase_band_deficits()}: the two terminal phases overhang the
#' outermost node by \code{vpad/4}, and adjacent strips are separated by
#' \code{ph_gap}.  Within a band the content is placed by:
#' \itemize{
#'   \item \strong{no deficit} -- natural node positions are preserved
#'         (so the terminal overhang stays exactly \code{vpad/4}); the
#'         block is simply translated into its grown/recentered band.
#'   \item \strong{deficit} -- the band's elements (distinct rows, a
#'         two-arm row counting as one) are spread to \emph{equal gaps}:
#'         with \eqn{m} elements there are \eqn{m+1} equal slots (above,
#'         between each pair, and below), so e.g. a two-element phase
#'         seats its boxes at the 1/3 and 2/3 marks.
#' }
#' Because each band grows only by its own deficit and neighbors are
#' merely translated, growing one phase never alters another's band
#' height (no bystander stretch).  Node \code{y} values are updated in
#' place; per-phase band top/bottom edges (NPC) are returned for the
#' strip-drawing pass.
#'
#' @param nodes Node \code{data.table} with \code{y}, \code{box_h},
#'   \code{row}, \code{phase}, \code{role}, \code{node_id} (modified in
#'   place).
#' @param edges Edge \code{data.table} (\code{edge_type}, \code{from},
#'   \code{to}); currently unused for placement but kept for signature
#'   stability with \code{phase_band_deficits()}.
#' @param phases Phase table with \code{phase_start}, \code{phase_end}.
#' @param deficit_in Numeric per-phase deficit (inches) from
#'   \code{phase_band_deficits()}.
#' @param to_npc_h,to_npc_w Inch->NPC converters (height, width).
#' @param vpad_in Numeric vertical pad (inches); terminal overhang is
#'   \code{vpad_in/4}.
#' @param ph_gap_in Numeric separation between adjacent strips (inches).
#' @return A list with \code{band_top} and \code{band_bot}: numeric
#'   vectors (length \code{nrow(phases)}) of each phase strip's top and
#'   bottom edge in NPC.
#' @keywords internal
apply_phase_bands <- function(nodes, edges, phases, deficit_in,
                              to_npc_h, to_npc_w, vpad_in, ph_gap_in) {
    n_ph <- nrow(phases)
    ph_nums <- sort(unique(nodes$phase))
    ph2row  <- setNames(seq_along(ph_nums), as.character(ph_nums))
    phase_rows <- function(i) {
        r <- ph2row[as.character(seq.int(phases$phase_start[i],
                                         phases$phase_end[i]))]
        as.integer(r[!is.na(r)])
    }
    D        <- to_npc_h(if (length(deficit_in)) deficit_in else rep(0, n_ph))
    overhang <- to_npc_h(vpad_in / 4)
    ## Vertical tolerance for grouping boxes into one element
    clust_tol <- to_npc_h(vpad_in / 2)
    ph_gap   <- to_npc_h(ph_gap_in)

    ## Node index sets per phase, and natural extent (npc, larger y =
    ## higher) of each phase from current node positions
    idx_of <- lapply(seq_len(n_ph), function(i)
        which(nodes$row %in% phase_rows(i)))
    ext_top <- vapply(seq_len(n_ph), function(i) {
        ii <- idx_of[[i]]; if (!length(ii)) return(NA_real_)
        max(nodes$y[ii] + nodes$box_h[ii] / 2)
    }, numeric(1L))
    ext_bot <- vapply(seq_len(n_ph), function(i) {
        ii <- idx_of[[i]]; if (!length(ii)) return(NA_real_)
        min(nodes$y[ii] - nodes$box_h[ii] / 2)
    }, numeric(1L))

    ## Natural per-phase band edges (descending y)
    nb_t <- nb_b <- numeric(n_ph)
    nb_t[1L]   <- ext_top[1L] + overhang
    nb_b[n_ph] <- ext_bot[n_ph] - overhang
    if (n_ph > 1L) for (k in seq_len(n_ph - 1L)) {
                       mid        <- (ext_bot[k] + ext_top[k + 1L]) / 2
                       nb_b[k]      <- mid + ph_gap / 2
                       nb_t[k + 1L] <- mid - ph_gap / 2
                   }

    ## Grow: band k taller by D[k]
    cum_above <- c(0, cumsum(D))    # length n_ph+1; cum_above[k]=above phase k
    bt <- nb_t - cum_above[seq_len(n_ph)]
    bb <- nb_b - cum_above[seq_len(n_ph) + 1L]

    ## Recenter the grown diagram in [0,1].
    total_grown <- bt[1L] - bb[n_ph]
    recenter    <- (1 - total_grown) / 2 - bb[n_ph]
    bt <- bt + recenter
    bb <- bb + recenter

    ## Place each phase's content within its grown band.
    for (i in seq_len(n_ph)) {
        ii <- idx_of[[i]]; if (!length(ii)) next
        band_t <- bt[i]; band_b <- bb[i]

        ## Elements = vertical bands, found by clustering boxes whose
        ## extents overlap or lie within clust_tol
        ord  <- ii[order(-nodes$y[ii])]    # top to bottom
        grp_top <- grp_bot <- numeric(0)
        grp_mem <- list()
        for (nd_i in ord) {
            nt_i <- nodes$y[nd_i] + nodes$box_h[nd_i] / 2
            nb_i <- nodes$y[nd_i] - nodes$box_h[nd_i] / 2
            hit <- 0L
            if (length(grp_top)) for (g in seq_along(grp_top)) {
                                     if (!(nb_i > grp_top[g] + clust_tol ||
                                           nt_i < grp_bot[g] - clust_tol)) { hit <- g; break }
                                 }
            if (hit == 0L) {
                grp_top <- c(grp_top, nt_i); grp_bot <- c(grp_bot, nb_i)
                grp_mem <- c(grp_mem, list(nd_i))
            } else {
                grp_top[hit] <- max(grp_top[hit], nt_i)
                grp_bot[hit] <- min(grp_bot[hit], nb_i)
                grp_mem[[hit]] <- c(grp_mem[[hit]], nd_i)
            }
        }
        m    <- length(grp_mem)
        el_h <- grp_top - grp_bot

        if (D[i] > 1e-9) {
            ## Distribute slack as (m + 1) equal gaps (above, between, and
            ## below the elements)
            slot <- (band_t - band_b - sum(el_h)) / (m + 1L)
            cur  <- band_t - slot                # top of first element
            for (e in seq_len(m)) {
                jj <- grp_mem[[e]]
                sh <- cur - grp_top[e]           # shift element so its top = cur
                set(nodes, i = jj, j = "y", value = nodes$y[jj] + sh)
                cur <- cur - el_h[e] - slot
            }
        } else {
            ## No deficit: preserve natural layout, translating by the same
            ## amount this band moved (recenter minus the deficit above)
            set(nodes, i = ii, j = "y",
                value = nodes$y[ii] + recenter - cum_above[i])
        }
    }

    list(band_top = bt, band_bot = bb)
}
