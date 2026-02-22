#' Draw Enrollment Diagram via Grid Graphics
#'
#' Computes all layout in inches using physical text measurements, then
#' renders. Use \code{autodiagram()} to auto-size the output device.
#'
#' @param graph A laid-out graph (output of \code{layout_nodes()}).
#' @param cex Numeric. Font size for main box text. Default 0.85.
#' @param cex_side Numeric. Font size for side box text. Default same as cex.
#' @param cex_phase Numeric. Font size for phase labels.
#' @param box_fill,side_fill Character. Fill colors.
#' @param border_col,arrow_col Character. Border and arrow colors.
#' @param phase_fill,phase_text_col Character. Phase box colors.
#' @param lwd Numeric. Line width.
#' @param count_first Logical. Count before label in side boxes.
#' @param newpage Logical. Call \code{grid.newpage()} before drawing?
#' @param vpad Numeric. Vertical padding between elements in inches.
#'   This is the single tunable factor that controls the uniform vertical
#'   spacing between any box edge and the next adjacent element. Default 0.25.
#' @param pad Numeric. Padding inside boxes in inches. Default 0.08.
#' @param line_height Numeric. Line spacing in inches. Controls box heights
#'   for both main and side boxes. Default 0.20.
#' @param margin Numeric. Fixed margin on all four sides in inches.
#'   Default 0.25.
#' @param phase_width Numeric. Width of phase label boxes in inches.
#'   Default 0.22.
#'
#' @return Invisibly returns the graph.
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

    if (is.null(cex_side)) cex_side <- cex

    ## ==================================================================
    ## VIEWPORT — fixed inch-based margins
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

    ## ==================================================================
    ## MEASUREMENT HELPERS (all return inches)
    ## ==================================================================

    tw_in <- function(label, gp) {
        convertWidth(grobWidth(textGrob(label, gp = gp)), "inches", valueOnly = TRUE)
    }

    ## ==================================================================
    ## LINE HEIGHT: user-controllable, in inches
    ## ==================================================================
    lh      <- line_height
    side_lh <- line_height

    ## Padding in inches
    pad_h   <- pad
    pad_v   <- pad
    vpad_in <- vpad

    ## ==================================================================
    ## PASS 1: Box sizes in inches
    ## ==================================================================

    nodes[, n_reason := vapply(reasons, function(r) {
        if (is.null(r)) 0L else length(r)
    }, integer(1L))]

    n_nd <- nrow(nodes)
    bw_in <- numeric(n_nd)
    bh_in <- numeric(n_nd)

    for (i in seq_len(n_nd)) {
        nd_role <- nodes$role[i]
        nd_text <- nodes$text[i]
        nd_n    <- nodes$n[i]
        nreas   <- nodes$n_reason[i]
        nd_reas <- nodes$reasons[[i]]

        if (nd_role == "side") {
            if (count_first) {
                ## Column layout: [bold number] [gap] [text]
                ## Find widest number (header + all reasons) for column width
                all_nums <- fmt_n(nd_n)
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_w <- max(vapply(all_nums, function(s) tw_in(s, gp_side_bold), numeric(1L)))
                num_gap <- tw_in("  ", gp_side_bold)
                ## Header: number + gap + text
                hdr_w <- num_col_w + num_gap + tw_in(nd_text, gp_side)
                ## Reasons: indent + number + gap + text
                max_reas_w <- 0
                indent_in <- 0.10
                if (!is.null(nd_reas) && length(nd_reas) > 0L) {
                    for (j in seq_along(nd_reas)) {
                        rw <- indent_in + num_col_w + num_gap + tw_in(names(nd_reas)[j], gp_side)
                        max_reas_w <- max(max_reas_w, rw)
                    }
                }
                bw_in[i] <- max(hdr_w, max_reas_w) + 2 * pad_h
            } else {
                lbl_w <- tw_in(paste0(nd_text, " "), gp_side_bold)
                cnt_w <- tw_in(paste0("(n = ", fmt_n(nd_n), ")"), gp_side)
                hdr_w <- lbl_w + cnt_w
                max_reas_w <- 0
                indent_in <- 0.10
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

        } else {
            txt_w <- if (nchar(nd_text) > 0L) tw_in(nd_text, gp_main_bold) else 0
            cnt_w <- tw_in(paste0("N = ", fmt_n(nd_n)), gp_main)
            bw_in[i] <- max(txt_w, cnt_w) + 2 * pad_h
            n_lines <- if (nchar(nd_text) > 0L) 2L else 1L
            bh_in[i] <- n_lines * lh + 2 * pad_v
        }
    }

    nodes[, bw_inches := bw_in]
    nodes[, bh_inches := bh_in]

    ## ==================================================================
    ## PASS 1b: Horizontal layout in inches
    ## ==================================================================
    ## Compute X positions for all nodes in inches, then derive
    ## the total content width needed. This prevents stretching.

    hpad_in <- vpad  # horizontal gap between adjacent columns

    arm_ids <- sort(unique(nodes[!is.na(arm_id), arm_id]))
    n_arms  <- length(arm_ids)

    ## Widths of the widest main/side boxes per column
    pre_main_w <- max(nodes[is.na(arm_id) & role != "side", bw_inches], 0)
    pre_side_w <- max(c(nodes[is.na(arm_id) & role == "side", bw_inches], 0))

    if (n_arms == 0L) {
        ## ---- 0-arm: main column + side column ----
        total_content_w <- pre_main_w + hpad_in + pre_side_w
        main_x_in <- pre_main_w / 2
        ## Side boxes: left-aligned (left edge at pre_main_w + hpad_in)
        side_left_in <- pre_main_w + hpad_in

        nodes[role != "side", x_in := main_x_in]
        ## Per-node x so left edges align
        side_idx <- which(nodes$role == "side")
        for (si in side_idx) {
            nodes[si, x_in := side_left_in + bw_inches / 2]
        }

    } else {
        ## ---- Multi-arm layouts ----

        ## Measure per-arm column widths
        arm_main_w <- vapply(arm_ids, function(a) {
            max(c(nodes[arm_id == a & role != "side", bw_inches], 0))
        }, numeric(1L))
        arm_side_w <- vapply(arm_ids, function(a) {
            max(c(nodes[arm_id == a & role == "side", bw_inches], 0))
        }, numeric(1L))

        ## Pre-randomization section width
        pre_section_w <- pre_main_w + ifelse(pre_side_w > 0, hpad_in + pre_side_w, 0)

        if (n_arms == 2L) {
            ## ---- 2-arm symmetric: side boxes on OUTSIDE (inverted Y) ----
            ## Layout: [side_L] [hpad] [main_L] [hpad] [main_R] [hpad] [side_R]
            left_tail  <- if (arm_side_w[1] > 0) arm_side_w[1] + hpad_in else 0
            right_tail <- if (arm_side_w[2] > 0) hpad_in + arm_side_w[2] else 0
            arm_section_w <- left_tail + arm_main_w[1] + hpad_in +
                             arm_main_w[2] + right_tail

            total_content_w <- max(pre_section_w, arm_section_w)

            ## Position arm section centered in content area
            arm_start <- (total_content_w - arm_section_w) / 2

            ## Left arm
            left_main_x <- arm_start + left_tail + arm_main_w[1] / 2
            nodes[arm_id == arm_ids[1] & role != "side", x_in := left_main_x]
            if (arm_side_w[1] > 0) {
                left_side_x <- arm_start + arm_side_w[1] / 2
                nodes[arm_id == arm_ids[1] & role == "side", x_in := left_side_x]
            }

            ## Right arm
            right_main_x <- arm_start + left_tail + arm_main_w[1] + hpad_in + arm_main_w[2] / 2
            nodes[arm_id == arm_ids[2] & role != "side", x_in := right_main_x]
            if (arm_side_w[2] > 0) {
                right_side_x <- arm_start + left_tail + arm_main_w[1] + hpad_in +
                                arm_main_w[2] + hpad_in + arm_side_w[2] / 2
                nodes[arm_id == arm_ids[2] & role == "side", x_in := right_side_x]
            }

            ## Center pre-randomization on midpoint of arm main boxes
            arm_mid_x <- (left_main_x + right_main_x) / 2
            pre_main_x <- arm_mid_x
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            nodes[is.na(arm_id) & role != "side", x_in := pre_main_x]
            pre_side_idx <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx) {
                nodes[si, x_in := pre_side_left + bw_inches / 2]
            }

        } else {
            ## ---- 3+ arms: side boxes to the right of each arm ----

            arm_col_w <- arm_main_w + ifelse(arm_side_w > 0, hpad_in + arm_side_w, 0)
            arm_section_w <- sum(arm_col_w) + hpad_in * (n_arms - 1L)

            total_content_w <- max(pre_section_w, arm_section_w)

            ## Position arm columns left-to-right, centered in content area
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
                        nodes[si, x_in := arm_side_left + bw_inches / 2]
                    }
                }
                cursor <- cursor + arm_col_w[k] + hpad_in
            }

            ## Center pre-randomization on midpoint of outermost arm main boxes
            arm_mid_x <- (arm_main_centers[1] + arm_main_centers[n_arms]) / 2
            pre_main_x <- arm_mid_x
            pre_side_left <- pre_main_x + pre_main_w / 2 + hpad_in
            nodes[is.na(arm_id) & role != "side", x_in := pre_main_x]
            pre_side_idx <- which(is.na(nodes$arm_id) & nodes$role == "side")
            for (si in pre_side_idx) {
                nodes[si, x_in := pre_side_left + bw_inches / 2]
            }
        }
    }

    ## Store computed content width for suggest_size
    graph$content_width_in <- total_content_w

    ## ==================================================================
    ## PASS 2: Vertical layout in inches
    ## ==================================================================

    n_rows <- max(nodes$row)
    setkey(nodes, node_id)

    row_h_in <- numeric(n_rows)
    for (r in seq_len(n_rows)) {
        mn <- nodes[row == r & role != "side"]
        row_h_in[r] <- if (nrow(mn) > 0L) max(mn$bh_inches) else 0
    }

    pair_gap_in <- numeric(n_rows)
    for (r in seq_len(n_rows)) {
        has_main <- nrow(nodes[row == r & role != "side"]) > 0L
        pair_gap_in[r] <- if (has_main) vpad_in else 0
    }
    for (i in seq_len(nrow(edges))) {
        e <- edges[i]
        if (e$edge_type != "exclude") next
        from_nd <- nodes[.(e$from)]
        side_nd <- nodes[.(e$to)]
        from_row <- from_nd$row
        flow_e <- edges[from == e$from & edge_type == "flow"]
        next_row <- if (nrow(flow_e) > 0L) nodes[.(flow_e$to[1L])]$row else from_row + 1L
        if (next_row <= n_rows && next_row > from_row) {
            needed <- side_nd$bh_inches + 2 * vpad_in
            pair_gap_in[from_row] <- max(pair_gap_in[from_row], needed)
        }
    }

    ## Double gap after allocation rows
    for (r in seq_len(n_rows)) {
        if (nrow(nodes[row == r & role == "alloc"]) > 0L) {
            pair_gap_in[r] <- max(pair_gap_in[r], 2 * vpad_in)
        }
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

    ## Row y positions (top-down, centered in content area)
    actual_h_npc <- sum(row_h) + sum(gap_npc[seq_len(n_rows - 1L)])
    y_offset <- (1.0 - actual_h_npc) / 2

    row_y <- numeric(n_rows)
    y_cursor <- 1.0 - y_offset - row_h[1L] / 2
    row_y[1L] <- y_cursor
    for (r in seq_len(n_rows - 1L)) {
        y_cursor <- y_cursor - row_h[r] / 2 - gap_npc[r] - row_h[r + 1L] / 2
        row_y[r + 1L] <- y_cursor
    }

    row_y_map <- setNames(row_y, as.character(seq_len(n_rows)))
    nodes[role != "side", y := row_y_map[as.character(row)]]

    ## ==================================================================
    ## PASS 3: Side-box Y — anchor vpad below parent
    ## ==================================================================

    vpad_npc <- to_npc_h(vpad_in)

    for (i in seq_len(nrow(edges))) {
        e <- edges[i]
        if (e$edge_type != "exclude") next
        from_nd <- nodes[.(e$from)]
        side_id <- e$to
        side_nd <- nodes[.(side_id)]

        side_top_y <- from_nd$y - from_nd$box_h / 2 - vpad_npc
        mid_y <- side_top_y - side_nd$box_h / 2
        nodes[.(side_id), y := mid_y]
    }

    ## ==================================================================
    ## DRAW
    ## ==================================================================

    arr <- arrow(length = unit(0.10, "inches"), type = "closed")

    ## ---- Phase labels (drawn at negative x in content viewport) ----

    if (has_phases) {
        gp_ph_text <- gpar(cex = cex_phase, col = phase_text_col, fontface = "bold")
        gp_ph_box  <- gpar(fill = phase_fill, col = phase_fill, lwd = 0)

        ## Phase box position: centered in the phase box area
        ## (not the gap), which is at the left edge of the phase strip
        ph_box_w_npc <- to_npc_w(ph_box_w_in)
        ph_x_npc     <- to_npc_w(-(phase_gap_in + ph_box_w_in / 2))
        ph_gap_npc   <- 0.005

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

        all_top <- max(nodes$y + nodes$box_h / 2, na.rm = TRUE) + 0.008
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

    for (i in seq_len(nrow(edges))) {
        e  <- edges[i]
        fn <- nodes[.(e$from)]
        tn <- nodes[.(e$to)]

        if (e$edge_type == "flow") {
            grid.lines(x = unit(c(fn$x, tn$x), "npc"),
                       y = unit(c(fn$y - fn$box_h / 2, tn$y + tn$box_h / 2), "npc"),
                       gp = gp_edge, arrow = arr)
        } else if (e$edge_type == "exclude") {
            src_x <- fn$x; side_y <- tn$y
            to_x <- if (tn$x > src_x) tn$x - tn$bw / 2 else tn$x + tn$bw / 2
            grid.lines(x = unit(c(src_x, to_x), "npc"),
                       y = unit(c(side_y, side_y), "npc"),
                       gp = gp_edge, arrow = arr)
        } else if (e$edge_type == "split") {
            drop_y <- (fn$y - fn$box_h / 2 + tn$y + tn$box_h / 2) / 2
            grid.lines(x = unit(c(fn$x, fn$x), "npc"),
                       y = unit(c(fn$y - fn$box_h / 2, drop_y), "npc"), gp = gp_edge)
            grid.lines(x = unit(c(fn$x, tn$x), "npc"),
                       y = unit(c(drop_y, drop_y), "npc"), gp = gp_edge)
            grid.lines(x = unit(c(tn$x, tn$x), "npc"),
                       y = unit(c(drop_y, tn$y + tn$box_h / 2), "npc"),
                       gp = gp_edge, arrow = arr)
        }
    }

    ## ---- Boxes and text ----

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

        bf <- if (nd_role == "side") side_fill else box_fill
        grid.rect(x = unit(nd_x, "npc"), y = unit(nd_y, "npc"),
                  width = unit(nd_bw, "npc"), height = unit(nd_bh, "npc"),
                  gp = gpar(fill = bf, col = border_col, lwd = lwd))

        if (nd_role == "side") {
            left_x <- nd_x - nd_bw / 2 + pad_h_npc
            n_reas <- if (!is.null(nd_reas)) length(nd_reas) else 0L

            block_h <- (1 + n_reas) * side_lh_npc
            top_y   <- nd_y + block_h / 2 - side_lh_npc / 2

            if (count_first) {
                ## Compute number column width for this box (widest bold number)
                all_nums <- fmt_n(nd_n)
                if (n_reas > 0L) {
                    all_nums <- c(all_nums, vapply(nd_reas, fmt_n, character(1L)))
                }
                num_col_npc <- max(vapply(all_nums, function(s) {
                    convertWidth(grobWidth(textGrob(s, gp = gp_side_bold)),
                                 "npc", valueOnly = TRUE)
                }, numeric(1L)))
                num_gap_npc <- convertWidth(
                    grobWidth(textGrob("  ", gp = gp_side_bold)),
                    "npc", valueOnly = TRUE)
                text_x <- left_x + num_col_npc + num_gap_npc

                ## Header: right-aligned bold number, then text
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
                lbl_npc <- convertWidth(grobWidth(textGrob(paste0(nd_text, " "), gp = gp_side_bold)),
                                        "npc", valueOnly = TRUE)
                grid.text(bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_n)) * ")"),
                          x = unit(left_x + lbl_npc, "npc"), y = unit(top_y, "npc"),
                          gp = gp_side, just = c("left", "centre"))
            }

            if (n_reas > 0L) {
                for (j in seq_len(n_reas)) {
                    ry <- top_y - j * side_lh_npc
                    if (count_first) {
                        rc <- fmt_n(nd_reas[j])
                        ## Right-aligned bold number in column
                        grid.text(rc,
                                  x = unit(left_x + indent_npc + num_col_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side_bold, just = c("right", "centre"))
                        ## Left-aligned text after column
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc + num_col_npc + num_gap_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side, just = c("left", "centre"))
                    } else {
                        grid.text(names(nd_reas)[j],
                                  x = unit(left_x + indent_npc, "npc"),
                                  y = unit(ry, "npc"),
                                  gp = gp_side, just = c("left", "centre"))
                        rn_npc <- convertWidth(
                            grobWidth(textGrob(paste0(names(nd_reas)[j], " "), gp = gp_side)),
                            "npc", valueOnly = TRUE)
                        grid.text(
                            bquote("(" * italic("n") ~ "=" ~ .(fmt_n(nd_reas[j])) * ")"),
                            x = unit(left_x + indent_npc + rn_npc, "npc"),
                            y = unit(ry, "npc"),
                            gp = gp_side, just = c("left", "centre"))
                    }
                }
            }

        } else if (nd_role == "alloc") {
            sep <- lh_npc * 0.55
            grid.text(nd_text, x = unit(nd_x, "npc"),
                      y = unit(nd_y + sep, "npc"), gp = gp_main_bold, just = "centre")
            grid.text(bquote(italic("n") ~ "=" ~ .(fmt_n(nd_n))),
                      x = unit(nd_x, "npc"),
                      y = unit(nd_y - sep, "npc"), gp = gp_main, just = "centre")

        } else {
            has_lab <- nchar(nd_text) > 0L
            n_let <- if (nd_id == 1L) "N" else "n"
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

    popViewport()
    invisible(graph)
}
