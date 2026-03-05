#' Draw Enrollment Diagram via Grid Graphics
#'
#' Computes all layout in inches using physical text measurements, then
#' renders the diagram within a fixed-margin viewport. Intended to be called
#' by \code{\link{flowchart}} or \code{\link{autoflow}} rather than
#' directly.
#'
#' @param graph A laid-out graph (output of \code{layout_nodes()}).
#' @param cex Numeric. Font size multiplier for main box text. Default 0.85.
#' @param cex_side Numeric. Font size multiplier for side box text. Defaults
#'   to the value of \code{cex}.
#' @param cex_phase Numeric. Font size multiplier for phase labels.
#'   Default 0.9.
#' @param box_fill Character. Fill colour for main boxes. Default \code{"white"}.
#' @param side_fill Character. Fill colour for side (exclusion) boxes.
#'   Default \code{"white"}.
#' @param border_col Character. Border colour for all boxes.
#'   Default \code{"black"}.
#' @param arrow_col Character. Colour for arrows and connector lines.
#'   Default \code{"black"}.
#' @param phase_fill Character. Background colour for phase label boxes.
#'   Default \code{"black"}.
#' @param phase_text_col Character. Text colour for phase labels.
#'   Default \code{"white"}.
#' @param lwd Numeric. Line width for borders and arrows. Default 1.
#' @param count_first Logical. If \code{TRUE}, side-box labels are rendered
#'   as \code{"214  Discontinued"} (bold count before label) rather than the
#'   default \code{"Discontinued (n = 214)"}. Default \code{FALSE}.
#' @param newpage Logical. If \code{TRUE}, calls \code{grid.newpage()} before
#'   drawing. Default \code{TRUE}.
#' @param vpad Numeric. Vertical spacing between elements in inches. Controls
#'   the uniform gap between any box edge and the next adjacent element.
#'   Default 0.25.
#' @param pad Numeric. Internal padding within boxes in inches. Default 0.08.
#' @param line_height Numeric. Vertical line spacing in inches, controlling
#'   box heights for both main and side boxes. Scales proportionally with
#'   \code{cex}. Default 0.20.
#' @param margin Numeric. Fixed margin on all four sides of the canvas in
#'   inches. Default 0.25.
#' @param phase_width Numeric. Width of phase label boxes in inches.
#'   Default 0.22.
#'
#' @return Invisibly returns the graph, augmented with computed layout
#'   dimensions (\code{diagram_width_in}, \code{diagram_height_in}).
#' @keywords internal
draw_grid <- function(graph,
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
                      vpad           = 0.25,
                      pad            = 0.08,
                      line_height    = 0.20,
                      margin         = 0.25,
                      phase_width    = 0.22) {

    nodes  <- graph$nodes
    edges  <- graph$edges
    phases <- graph$phases

    if (nrow(nodes) == 0L) {
        warning("Empty diagram -- no nodes to draw", call. = FALSE)
        return(invisible(graph))
    }

    if (is.null(cex_side)) cex_side <- cex

    ## ==================================================================
    ## VIEWPORT
    ## ==================================================================

    has_phases <- nrow(phases) > 0L

    if (newpage) grid.newpage()

    ## Device dimensions
    dev_w_in <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
    dev_h_in <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)

    ## Phase label strip: measure text to size boxes, plus margin-sized gap
    if (has_phases) {
        ph_box_w_in  <- phase_width
        phase_gap_in <- margin  # gap between phase boxes and content = margin
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

    ## gpar objects
    gp_main      <- gpar(cex = cex)
    gp_main_bold <- gpar(cex = cex, fontface = "bold")
    gp_side      <- gpar(cex = cex_side)
    gp_side_bold <- gpar(cex = cex_side, fontface = "bold")
    gp_reas      <- gpar(cex = cex_side * 0.92, fontface = "italic")

    ## ==================================================================
    ## MEASUREMENT HELPERS (all return inches)
    ## ==================================================================

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

    ## ==================================================================
    ## LINE HEIGHT: user-controllable, in inches; scales with font size
    ## ==================================================================
    lh      <- line_height * (cex / 0.85)
    side_lh <- line_height * (cex_side / 0.85)

    ## Padding in inches
    pad_h   <- pad
    pad_v   <- pad
    vpad_in <- vpad

    ## ==================================================================
    ## PASS 1: Box sizes in inches
    ## ==================================================================

    ## Ensure grid_row and grid_col columns exist
    if (!"grid_row" %in% names(nodes)) nodes[, grid_row := NA_integer_]
    if (!"grid_col" %in% names(nodes)) nodes[, grid_col := NA_integer_]
    if (!"stream_group" %in% names(nodes)) nodes[, stream_group := NA_character_]

    nodes[, n_reason := lengths(reasons)]

    n_nd <- nrow(nodes)
    bw_in <- numeric(n_nd)
    bh_in <- numeric(n_nd)

    ## Common text measurements
    indent_in       <- 0.10
    gap_side_in     <- tw_in("  ", gp_side_bold)
    gap_main_in     <- tw_in("  ", gp_main_bold)

    for (i in seq_len(n_nd)) {
        nd_role <- nodes$role[i]
        nd_text <- nodes$text[i]
        nd_n    <- nodes$n[i]
        nreas   <- nodes$n_reason[i]
        nd_reas <- nodes$reasons[[i]]

        if (nd_role == "side") {
            if (count_first) {
                all_nums <- fmt_n(nd_n)
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_w <- max(vapply(all_nums, function(s) tw_in(s, gp_side_bold), numeric(1L)))
                hdr_w <- num_col_w + gap_side_in + tw_in(nd_text, gp_side)
                max_reas_w <- 0
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    for (j in seq_along(nd_reas)) {
                        rw <- indent_in + num_col_w + gap_side_in + tw_in(names(nd_reas)[j], gp_side)
                        max_reas_w <- max(max_reas_w, rw)
                    }
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            } else {
                lbl_w <- tw_in(paste0(nd_text, " "), gp_side_bold)
                cnt_w <- tw_in(paste0("(n = ", fmt_n(nd_n), ")"), gp_side)
                hdr_w <- lbl_w + cnt_w
                max_reas_w <- 0
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    for (j in seq_along(nd_reas)) {
                        rstr <- paste0(names(nd_reas)[j], " (n = ", fmt_n(nd_reas[j]), ")")
                        rw <- tw_in(rstr, gp_side) + indent_in
                        max_reas_w <- max(max_reas_w, rw)
                    }
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            }
            bh_in[i] <- (1 + nreas) * side_lh + 2 * pad_v

        } else if (nd_role == "cell") {
            ## Classification grid cell: text + count
            txt_w <- if (nchar(nd_text) > 0L) tw_in(nd_text, gp_main_bold) else 0
            cnt_w <- tw_in(paste0("n = ", fmt_n(nd_n)), gp_main)
            bw_in[i] <- max(txt_w, cnt_w) + 2 * pad_h
            bh_in[i] <- 2 * lh + 2 * pad_v

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
            if (count_first && has_lab) {
                ## Single line: bold count + gap + label
                cnt_str <- fmt_n(nd_n)
                cnt_w <- tw_in(cnt_str, gp_main_bold)
                txt_w <- tw_in(nd_text, gp_main)
                bw_in[i] <- cnt_w + gap_main_in + txt_w + 2 * pad_h
                bh_in[i] <- 1 * lh + 2 * pad_v
            } else {
                txt_w <- if (has_lab) tw_in(nd_text, gp_main_bold) else 0
                cnt_w <- tw_in(paste0("N = ", fmt_n(nd_n)), gp_main)
                bw_in[i] <- max(txt_w, cnt_w) + 2 * pad_h
                n_lines <- if (has_lab) 2L else 1L
                bh_in[i] <- n_lines * lh + 2 * pad_v
            }
        }
    }

    nodes[, bw_inches := bw_in]
    nodes[, bh_inches := bh_in]

    ## ==================================================================
    ## PASS 1b: Horizontal layout in inches
    ## ==================================================================

    hpad_in <- vpad  # horizontal gap between adjacent columns

    arm_ids <- sort(unique(nodes[!is.na(arm_id), arm_id]))
    n_arms  <- length(arm_ids)
    has_sources <- nrow(nodes[role == "source"]) > 0L

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

        src_group_centers <- NULL

        if (has_sources && !is.null(src_group_info)) {
            n_groups <- length(src_group_info)
            src_group_centers <- numeric(n_groups)

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
                src_group_centers[gi] <- g_center
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

        if (n_arms == 2L) {
            left_tail  <- if (arm_side_w[1] > 0) arm_side_w[1] + hpad_in else 0
            right_tail <- if (arm_side_w[2] > 0) hpad_in + arm_side_w[2] else 0
            inner_w    <- arm_main_w[1] + hpad_in + arm_main_w[2]

            ## Position everything relative to arm_mid = 0
            ## Arm section spans: [-inner_w/2 - left_tail, inner_w/2 + right_tail]
            left_main_rel  <- -inner_w / 2 + arm_main_w[1] / 2
            right_main_rel <-  inner_w / 2 - arm_main_w[2] / 2

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

            ## Pre-split section: centered on arm_mid
            pre_main_x <- arm_mid_x
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            nodes[is.na(arm_id) & role %chin% c("main", "alloc", "endpoint"), x_in := pre_main_x]
            pre_side_idx <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx) {
                set(nodes, i = si, j = "x_in",
                    value = pre_side_left + nodes$bw_inches[si] / 2)
            }

        } else {
            arm_col_w <- arm_main_w + ifelse(arm_side_w > 0, hpad_in + arm_side_w, 0)
            arm_section_w <- sum(arm_col_w) + hpad_in * (n_arms - 1L)

            total_content_w <- max(pre_section_w, arm_section_w, src_section_w)

            arm_section_start <- (total_content_w - arm_section_w) / 2
            cursor <- arm_section_start
            arm_main_centers <- numeric(n_arms)
            for (k in seq_along(arm_ids)) {
                a <- arm_ids[k]
                arm_center <- cursor + arm_main_w[k] / 2
                arm_main_centers[k] <- arm_center
                nodes[arm_id == a & role != "side", x_in := arm_center]
                if (arm_side_w[k] > 0) {
                    arm_side_left <- cursor + arm_main_w[k] + hpad_in
                    arm_side_idx <- which(nodes$arm_id == a & nodes$role == "side")
                    for (si in arm_side_idx) {
                        set(nodes, i = si, j = "x_in",
                            value = arm_side_left + nodes$bw_inches[si] / 2)
                    }
                }
                cursor <- cursor + arm_col_w[k] + hpad_in
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
            n_groups <- length(src_group_info)
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

    ## ---- Cell nodes: position in grid ----
    cell_nodes_idx <- which(nodes$role == "cell")
    if (length(cell_nodes_idx) > 0L) {
        cell_max_w <- max(nodes$bw_inches[cell_nodes_idx])
        grid_cols <- sort(unique(nodes$grid_col[cell_nodes_idx]))
        n_gc <- length(grid_cols)
        cell_gap <- hpad_in  ## full hpad between grid cells

        if (any(!is.na(nodes$arm_id[cell_nodes_idx]))) {
            ## Per-arm grids: cells under each arm
            per_arm_cell_span <- n_gc * cell_max_w + (n_gc - 1) * cell_gap

            for (a in unique(nodes$arm_id[cell_nodes_idx])) {
                arm_x <- nodes[arm_id == a & role %chin% c("arm", "main", "endpoint"), x_in]
                if (length(arm_x) == 0L) arm_x <- total_content_w / 2
                arm_base <- arm_x[1L]
                cell_start <- arm_base - per_arm_cell_span / 2
                for (ci in seq_along(grid_cols)) {
                    cx <- cell_start + (ci - 1) * (cell_max_w + cell_gap) + cell_max_w / 2
                    nodes[role == "cell" & arm_id == a & grid_col == grid_cols[ci],
                          x_in := cx]
                }
            }

            ## Ensure total width covers the grid extent
            all_cell_x <- nodes$x_in[cell_nodes_idx]
            cell_left  <- min(all_cell_x) - cell_max_w / 2
            cell_right <- max(all_cell_x) + cell_max_w / 2
            if (cell_left < 0 || cell_right > total_content_w) {
                new_w <- cell_right - min(cell_left, 0)
                shift <- if (cell_left < 0) -cell_left else 0
                if (shift > 0) {
                    nodes[!is.na(x_in), x_in := x_in + shift]
                }
                total_content_w <- max(total_content_w, new_w)
            }
        } else {
            ## Single-stream grid
            cell_span <- n_gc * cell_max_w + (n_gc - 1) * cell_gap
            cell_start <- (total_content_w - cell_span) / 2
            for (ci in seq_along(grid_cols)) {
                cx <- cell_start + (ci - 1) * (cell_max_w + cell_gap) + cell_max_w / 2
                nodes[role == "cell" & grid_col == grid_cols[ci],
                      x_in := cx]
            }
            ## Widen total if grid is wider
            total_content_w <- max(total_content_w, cell_span)
        }
    }

    ## Store computed content width for suggest_size
    graph$content_width_in <- total_content_w

    ## ==================================================================
    ## PASS 2: Vertical layout in inches
    ## ==================================================================

    n_rows <- max(nodes$row)
    setkey(nodes, node_id)

    ## -- Row heights --
    main_roles <- c("side", "cell", "source", "source_header")
    row_h_in <- numeric(n_rows)

    ## Main/arm/alloc/endpoint boxes
    mn_subset <- nodes[!role %chin% main_roles]
    if (nrow(mn_subset) > 0L) {
        mn_h <- mn_subset[, .(h = max(bh_inches)), by = row]
        row_h_in[mn_h$row] <- mn_h$h
    }

    ## Cell rows
    cn_nodes <- nodes[role == "cell"]
    if (nrow(cn_nodes) > 0L) {
        cn_h <- cn_nodes[, .(h = max(bh_inches)), by = row]
        row_h_in[cn_h$row] <- pmax(row_h_in[cn_h$row], cn_h$h)
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

    ## -- Pair gaps --
    ## Rows with any non-side/non-header content get vpad; others get 0
    content_roles <- c("side", "cell", "source_header")
    rows_with_content <- unique(nodes[!role %chin% content_roles, row])
    pair_gap_in <- numeric(n_rows)
    pair_gap_in[rows_with_content] <- vpad_in
    ## -- Exclude-edge gap --
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

    ## Double gap after allocation rows and source rows (convergence arrows)
    alloc_rows  <- unique(nodes[role == "alloc", row])
    source_rows <- unique(nodes[role == "source", row])
    double_rows <- unique(c(alloc_rows, source_rows))
    if (length(double_rows) > 0L) {
        pair_gap_in[double_rows] <- pmax(pair_gap_in[double_rows], 2 * vpad_in)
    }

    ## Content height in inches
    total_h_in <- sum(row_h_in) + sum(pair_gap_in[seq_len(n_rows - 1L)])

    ## Store for suggest_size(): total canvas = content + margins + phase strip
    graph$diagram_height_in <- total_h_in + 2 * margin
    graph$diagram_width_in  <- total_content_w + 2 * margin + phase_strip_w
    graph$phase_strip_w     <- phase_strip_w

    ## ==================================================================
    ## Convert to npc of the content viewport
    ## ==================================================================

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

    ## Pre-compute NPC gap constants used repeatedly in the rendering loop
    gap_main_npc <- convertWidth(grobWidth(textGrob("  ", gp = gp_main_bold)),
                                 "npc", valueOnly = TRUE)
    gap_side_npc <- convertWidth(grobWidth(textGrob("  ", gp = gp_side_bold)),
                                 "npc", valueOnly = TRUE)

    ## NPC text width via memoized inch cache
    tw_npc <- function(label, gp) to_npc_w(tw_in(label, gp))

    ## Row y positions (top-down, centered in content area)
    actual_h_npc <- sum(row_h) + sum(gap_npc[seq_len(n_rows - 1L)])
    y_offset <- (1.0 - actual_h_npc) / 2

    row_y <- numeric(n_rows)
    row_y[1L] <- 1.0 - y_offset - row_h[1L] / 2
    if (n_rows > 1L) {
        ## Drop between consecutive row centers:
        ##   half-height of row r  +  gap below row r  +  half-height of row r+1
        deltas <- row_h[seq_len(n_rows - 1L)] / 2 +
                  gap_npc[seq_len(n_rows - 1L)] +
                  row_h[2:n_rows] / 2
        row_y[2:n_rows] <- row_y[1L] - cumsum(deltas)
    }

    row_y_map <- setNames(row_y, as.character(seq_len(n_rows)))

    ## Position non-side, non-cell, non-source nodes at row centers
    nodes[!role %chin% c("side", "cell", "source", "source_header"), y := row_y_map[as.character(row)]]

    ## Position cell nodes at row centers
    nodes[role == "cell", y := row_y_map[as.character(row)]]

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

    ## ==================================================================
    ## PASS 3: Side-box Y -- anchor vpad below parent
    ## When multiple side boxes share a parent, stack them downward.
    ## ==================================================================

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

    ## ==================================================================
    ## DRAW
    ## ==================================================================

    arr <- arrow(length = unit(0.10, "inches"), type = "closed")

    ## ---- Phase labels (drawn at negative x in content viewport) ----

    if (has_phases) {
        gp_ph_text <- gpar(cex = cex_phase, col = phase_text_col, fontface = "bold")
        gp_ph_box  <- gpar(fill = phase_fill, col = phase_fill, lwd = 0)

        ph_box_w_npc <- to_npc_w(ph_box_w_in)
        ph_x_npc     <- to_npc_w(-(phase_gap_in + ph_box_w_in / 2))
        ph_gap_npc   <- 0.01

        n_ph <- nrow(phases)
        all_phase_nums <- sort(unique(nodes$phase))
        phase_to_row <- setNames(seq_along(all_phase_nums),
                                 as.character(all_phase_nums))

        ph_ext <- lapply(seq_len(n_ph), function(idx) {
            ps <- phases$phase_start[idx]
            pe <- phases$phase_end[idx]
            ph_rows <- phase_to_row[as.character(seq.int(ps, pe))]
            ph_nds <- nodes[row %in% ph_rows]
            if (nrow(ph_nds) == 0L) return(list(top = NA_real_, bot = NA_real_))
            list(top = max(ph_nds$y + ph_nds$box_h / 2),
                 bot = min(ph_nds$y - ph_nds$box_h / 2))
        })

        ## Top/bottom boundaries for phase strips
        ## When source headers exist, flush the top (no cushion)
        has_hdrs_for_flush <- nrow(nodes[role == "source_header"]) > 0L
        top_cushion <- if (has_hdrs_for_flush) 0 else 0.008
        all_top <- max(nodes$y + nodes$box_h / 2, na.rm = TRUE) + top_cushion
        all_bot <- min(nodes$y - nodes$box_h / 2, na.rm = TRUE) - 0.008

        for (idx in seq_len(n_ph)) {
            ext <- ph_ext[[idx]]
            if (is.na(ext$top)) next
            y_hi <- if (idx == 1L) all_top else {
                (ph_ext[[idx - 1L]]$bot + ext$top) / 2 - ph_gap_npc / 2
            }
            y_lo <- if (idx == n_ph) all_bot else {
                (ext$bot + ph_ext[[idx + 1L]]$top) / 2 + ph_gap_npc / 2
            }
            ym <- (y_hi + y_lo) / 2
            ht <- max(y_hi - y_lo, 0.015)
            grid.rect(x = unit(ph_x_npc, "npc"), y = unit(ym, "npc"),
                      width = unit(ph_box_w_npc, "npc"),
                      height = unit(ht, "npc"),
                      gp = gp_ph_box)
            grid.text(phases$label[idx],
                      x = unit(ph_x_npc, "npc"), y = unit(ym, "npc"),
                      rot = 90, gp = gp_ph_text, just = "centre")
        }
    }

    ## ---- Edges ----

    gp_edge <- gpar(col = arrow_col, lwd = lwd, fill = arrow_col)

    ## Shared horizontal bar Y for converge edges
    converge_bar_y <- NULL
    conv_idx <- which(edges$edge_type == "converge")
    if (length(conv_idx) > 0L) {
        conv_edges <- edges[conv_idx]
        ## Join from/to node positions
        conv_edges[nodes, on = .(from = node_id),
                   from_bot := i.y - i.box_h / 2]
        conv_edges[nodes, on = .(to = node_id),
                   to_top := i.y + i.box_h / 2]
        ## Bar at midpoint between lowest source bottom and merge top
        bar_dt <- conv_edges[, .(bar_y = (min(from_bot) + to_top[1L]) / 2),
                             by = to]
        converge_bar_y <- setNames(bar_dt$bar_y, as.character(bar_dt$to))
    }

    ## Join from/to node positions onto edges
    ed <- copy(edges)
    ed[nodes, on = .(from = node_id), `:=`(fx = x, fy = y, fbh = box_h)]
    ed[nodes, on = .(to = node_id), `:=`(tx = x, ty = y, tbh = box_h, tbw = bw)]

    ## -- Flow and classify edges --
    ed_simple <- ed[edge_type %chin% c("flow", "classify")]
    if (nrow(ed_simple) > 0L) {
        grid.segments(
            x0 = unit(ed_simple$fx, "npc"),
            y0 = unit(ed_simple$fy - ed_simple$fbh / 2, "npc"),
            x1 = unit(ed_simple$tx, "npc"),
            y1 = unit(ed_simple$ty + ed_simple$tbh / 2, "npc"),
            gp = gp_edge, arrow = arr)
    }

    ## -- Exclude edges --
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

    ## -- Split edges --
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

        if (nd_role == "side") {
            left_x <- nd_x - nd_bw / 2 + pad_h_npc
            n_reas <- if (!is.null(nd_reas)) length(nd_reas) else 0L
            block_h <- (1 + n_reas) * side_lh_npc
            top_y   <- nd_y + block_h / 2 - side_lh_npc / 2

            if (count_first) {
                all_nums <- fmt_n(nd_n)
                if (n_reas > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_npc <- max(vapply(all_nums, function(s) {
                    tw_npc(s, gp_side_bold)
                }, numeric(1L)))
                text_x <- left_x + num_col_npc + gap_side_npc

                cs <- fmt_n(nd_n)
                grid.text(cs, x = unit(left_x + num_col_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_side_bold, just = c("right", "centre"))
                grid.text(nd_text, x = unit(text_x, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_side, just = c("left", "centre"))
            } else {
                grid.text(nd_text, x = unit(left_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_side_bold, just = c("left", "centre"))
                lbl_npc <- tw_npc(paste0(nd_text, " "), gp_side_bold)
                grid.text(bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_n)) * ")"),
                          x = unit(left_x + lbl_npc, "npc"), y = unit(top_y, "npc"),
                          gp = gp_side, just = c("left", "centre"))
            }

            if (n_reas > 0L) {
                for (j in seq_len(n_reas)) {
                    ry <- top_y - j * side_lh_npc
                    if (count_first) {
                        rc <- fmt_n(nd_reas[j])
                        grid.text(rc,
                                  x = unit(left_x + indent_npc + num_col_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side_bold, just = c("right", "centre"))
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc + num_col_npc + gap_side_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side, just = c("left", "centre"))
                    } else {
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side, just = c("left", "centre"))
                        rn_npc <- tw_npc(paste0(names(nd_reas)[j], " "), gp_side)
                        grid.text(
                            bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_reas[j])) * ")"),
                            x = unit(left_x + indent_npc + rn_npc, "npc"),
                            y = unit(ry, "npc"),
                            gp = gp_side, just = c("left", "centre"))
                    }
                }
            }

        } else if (nd_role == "alloc") {
            if (count_first) {
                left_x <- nd_x - nd_bw / 2 + pad_h_npc
                cnt_str <- fmt_n(nd_n)
                cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main_bold, just = c("right", "centre"))
                grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main, just = c("left", "centre"))
            } else {
                sep <- lh_npc * 0.55
                grid.text(nd_text, x = unit(nd_x, "npc"),
                          y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "centre")
                grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                          x = unit(nd_x, "npc"),
                          y = unit(nd_y - sep, "npc"), gp = gp_main, just = "centre")
            }

        } else if (nd_role == "cell") {
            ## Classification grid cell
            sep <- lh_npc * 0.55
            grid.text(nd_text, x = unit(nd_x, "npc"),
                      y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "centre")
            grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                      x = unit(nd_x, "npc"),
                      y = unit(nd_y - sep, "npc"), gp = gp_main, just = "centre")

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
                          gp = gp_main_bold, just = c("right", "centre"))
                grid.text(nd_text,
                          x = unit(left_x + num_col_npc_s + gap_main_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main, just = c("left", "centre"))

                ## Indented sub-items
                if (n_reas > 0L) {
                    for (j in seq_len(n_reas)) {
                        ry <- top_y - j * lh_npc
                        grid.text(fmt_n(nd_reas[j]),
                                  x = unit(left_x + indent_npc + num_col_npc_s, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main_bold, just = c("right", "centre"))
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc + num_col_npc_s + gap_main_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main, just = c("left", "centre"))
                    }
                }
            } else {
                ## Standard: "Records identified (n = X)" + indented sources
                grid.text(nd_text,
                          x = unit(left_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = c("left", "centre"))
                lbl_npc <- tw_npc(paste0(nd_text, " "), gp_main_bold)
                grid.text(
                    bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_n)) * ")"),
                    x = unit(left_x + lbl_npc, "npc"), y = unit(top_y, "npc"),
                    gp = gp_main, just = c("left", "centre"))

                ## Indented sub-items
                if (n_reas > 0L) {
                    for (j in seq_len(n_reas)) {
                        ry <- top_y - j * lh_npc
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_main, just = c("left", "centre"))
                        rn_npc <- tw_npc(paste0(names(nd_reas)[j], " "), gp_main)
                        grid.text(
                            bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_reas[j])) * ")"),
                            x = unit(left_x + indent_npc + rn_npc, "npc"),
                            y = unit(ry, "npc"),
                            gp = gp_main, just = c("left", "centre"))
                    }
                }
            }

        } else if (nd_role == "source_header") {
            ## Column header: bold centered text, distinct background
            grid.text(nd_text, x = unit(nd_x, "npc"), y = unit(nd_y, "npc"),
                      gp = gp_main_bold, just = "centre")

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
                          gp = gp_main_bold, just = c("right", "centre"))
                grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                          y = unit(top_y, "npc"),
                          gp = gp_main, just = c("left", "centre"))
            } else {
                grid.text(nd_text, x = unit(nd_x, "npc"), y = unit(top_y, "npc"),
                          gp = gp_main_bold, just = "centre")
                grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                          x = unit(nd_x, "npc"), y = unit(top_y - lh_npc, "npc"),
                          gp = gp_main, just = "centre")
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
                              gp = gp_reas, just = c("right", "centre"))
                    grid.text(names(nd_reas)[j],
                              x = unit(left_x + indent_npc + reas_num_npc +
                                       gap_side_npc, "npc"),
                              y = unit(ry, "npc"),
                              gp = gp_reas, just = c("left", "centre"))
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
                              gp = gp_reas, just = "centre")
                }
            }

        } else {
            has_lab <- nchar(nd_text) > 0L
            n_let <- if (nd_id == 1L && nd_role != "source") "N" else "n"

            if (count_first && has_lab) {
                ## Single line: bold count left, non-bold label right
                left_x <- nd_x - nd_bw / 2 + pad_h_npc
                cnt_str <- fmt_n(nd_n)
                cnt_npc <- tw_npc(cnt_str, gp_main_bold)
                grid.text(cnt_str, x = unit(left_x + cnt_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main_bold, just = c("right", "centre"))
                grid.text(nd_text, x = unit(left_x + cnt_npc + gap_main_npc, "npc"),
                          y = unit(nd_y, "npc"),
                          gp = gp_main, just = c("left", "centre"))
            } else {
                sep <- lh_npc * 0.55
                if (has_lab) {
                    grid.text(nd_text, x = unit(nd_x, "npc"),
                              y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "centre")
                    grid.text(bquote(italic(.(n_let)) ~ "=" ~ .(fmt_n(nd_n))),
                              x = unit(nd_x, "npc"),
                              y = unit(nd_y - sep, "npc"), gp = gp_main, just = "centre")
                } else {
                    grid.text(bquote(italic(.(n_let)) ~ "=" ~ .(fmt_n(nd_n))),
                              x = unit(nd_x, "npc"), y = unit(nd_y, "npc"),
                              gp = gp_main, just = "centre")
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
