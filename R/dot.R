### * Main functions

#' Convert Graph to Graphviz DOT String
#'
#' Generates a Graphviz DOT-language representation of a computed graph.
#' Node fill colors match the grid engine: a darker gray with bold black
#' text for source-column headers, white for source boxes, light gray for
#' side (exclusion) boxes, and white for everything else. Exclusion
#' sub-reasons, endpoint breakdowns, and the per-source counts of a
#' multi-source flow are rendered inside their boxes, so the DOT output
#' carries the same detail as the grid output.
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
#'   \code{flowchart()}). Defaults to the \code{selecta.number_format} option.
#' @param count_first Logical. If \code{TRUE}, the count appears before
#'   the label text in each box (\emph{e.g.,} \verb{200 Excluded}
#'   instead of \verb{Excluded, n = 200}), matching the count-first
#'   layout available in the grid engine. Default \code{FALSE}.
#' @param ortho Logical. If \code{TRUE} (default), edges are routed at
#'   right angles via Graphviz's \code{splines=ortho} attribute. This
#'   underpins the canonical CONSORT look, in which an exclusion side box
#'   hangs off a tick on the vertical spine rather than from a diagonal
#'   edge. Set to \code{FALSE} only to fall back to spline routing.
#' @param formatting Character string, either \code{"plain"} (default)
#'   or \code{"rich"}. See Details.
#' @param bullets Logical or \code{NULL}. Controls whether exclusion
#'   sub-reasons (and other left-aligned breakdowns inside side and source
#'   boxes) are prefixed with a bullet. \code{NULL} (default) selects by
#'   mode: \code{TRUE} for \code{formatting = "plain"}, where indentation
#'   alone barely separates a sub-reason from its parent label, and
#'   \code{FALSE} for \code{formatting = "rich"}, whose bold parent label
#'   already conveys the hierarchy. An explicit \code{TRUE} or \code{FALSE}
#'   overrides the per-mode default. Centered breakdowns beneath main and
#'   endpoint boxes are never bulleted.
#' @param font_family Character string. Graphviz \code{fontname} value
#'   for the body text. Default \code{"Helvetica"}.
#' @param padding_pt Numeric. Horizontal padding applied uniformly on
#'   each side of every node's text, in points. Default 14.
#' @param padding_adjust Numeric. Additive offset to \code{padding_pt}
#'   for fine-tuning, in points. Default 0.
#' @param box_fill Character. Fill color for main boxes. Default
#'   \code{"#FFFFFF"}.
#' @param side_fill Character. Fill color for side (exclusion) boxes.
#'   Default \code{"#FFFFFF"} (white), following the EQUATOR convention of
#'   plain white boxes throughout; set a gray such as \code{"#F0F0F0"} to
#'   shade exclusion boxes.
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
#' @param phase_labels Logical or \code{NULL}. Whether to render phase
#'   labels as left-margin band labels. \code{NULL} (default) auto-selects:
#'   on whenever the flow defines any phases via \code{phase()}, off
#'   otherwise. Unlike the grid engine's rotated vertical strips, the DOT
#'   labels are horizontal (Graphviz cannot rotate node text), placed in a
#'   left-hand column and rank-aligned to the first row of each band.
#' @param phase_fill Character. Fill color for phase label boxes. Default
#'   \code{"#000000"} (black), following the grid standard's black band
#'   labels.
#' @param phase_text_col Character. Text color for phase labels. Default
#'   \code{"#FFFFFF"} (white).
#' @param side_gap_in Numeric. Horizontal gap, in inches, between the
#'   vertical spine and the left edge of a side box hanging off a tick.
#'   Default 0.4. Realized as a narrow invisible spacer on the joint's
#'   rank; Graphviz's node separation also contributes, so the effective
#'   gap is slightly larger. Lower values pull side boxes toward the spine.
#' @param rank_sep Numeric. Graphviz \code{ranksep} in inches, the vertical
#'   separation between successive rows (and the half-rows introduced by
#'   tick joints). Default 0.4. Lower values produce a more compact diagram.
#' @param node_sep Numeric. Graphviz \code{nodesep} in inches, the minimum
#'   horizontal separation between nodes sharing a rank (arms, source
#'   columns, a side box and its joint). Default 0.5. This also sets the
#'   length of a side box's connector arrow (the box hangs one \code{nodesep}
#'   from its stem) and, for a box seated in the channel between two arms,
#'   the equal gap on each side -- so the box stays centered between the arms.
#' @return A character string in DOT format.
#' @keywords internal
export_dot <- function(graph, number_format = NULL, count_first = FALSE,
                       ortho = TRUE,
                       formatting = c("plain", "rich"),
                       bullets = NULL,
                       font_family = "Helvetica",
                       padding_pt = 14, padding_adjust = 0,
                       box_fill           = "#FFFFFF",
                       side_fill          = "#FFFFFF",
                       border_col         = "black",
                       arrow_col          = "black",
                       source_fill        = "#FFFFFF",
                       source_header_fill = "#D0D0D0",
                       source_header_text = "black",
                       phase_labels       = NULL,
                       phase_fill         = "#000000",
                       phase_text_col     = "#FFFFFF",
                       side_gap_in        = 0.4,
                       rank_sep           = 0.4,
                       node_sep           = 0.5) {

    formatting <- match.arg(formatting)
    padding_pt <- padding_pt + padding_adjust

    ## Bullet points on side/source sub-reasons
    bullets <- if (is.null(bullets)) (formatting == "plain") else isTRUE(bullets)

    ## Phase labels
    has_phases   <- !is.null(graph$phases) && nrow(graph$phases) > 0L
    phase_labels <- if (is.null(phase_labels)) has_phases else
                                                              (isTRUE(phase_labels) && has_phases)

    marks <- resolve_number_marks(number_format)
    fn    <- function(n) fmt_n(n, marks)

    ## Escape a string for a plain (quoted) DOT label at export_dot scope
    esc_dot <- function(s) {
        s <- gsub("\\", "\\\\", s, fixed = TRUE)
        s <- gsub('"',  '\\"',  s, fixed = TRUE)
        gsub("\n", "\\n", s, fixed = TRUE)
    }

    nodes <- data.table::copy(graph$nodes)
    edges <- data.table::copy(graph$edges)

    ## Padding to inches for the margin attribute; vertical is held smaller
    ## since the line-height already provides comfortable vertical spacing.
    margin_x_in <- padding_pt / 72
    margin_y_in <- 6 / 72
    font_size_pt <- 14

    ## ---- Column groups for orthogonal spine alignment ----
    ## Comment terminology:
    ##  - "spine": vertical connector line that the tick joints sit on
    ##             (i.e., the drawn path down the center of the diagram)
    ##  - "trunk": column/group of central single-stream boxes (main/alloc/endpoint)
    ##             whose vertical alignment keeps that spine straight
    ##
    ## Graphviz aligns same-group nodes vertically, so the trunk, arms, and source
    ## columns stay straight under ortho mode, and the tick joints sit on the spine.
    ## The trunk shares one group; arm boxes group by arm_id; source boxes and
    ## headers by stream_group. Side boxes are ungrouped---they hang off a joint,
    ## not a column.
    node_group <- function(role, arm_id, stream_group) {
        if (role %in% c("source", "source_header"))
            if (!is.na(stream_group)) return(paste0("src_", stream_group)) else return(NA_character_)
        if (role == "side") return(NA_character_)
        if (!is.na(arm_id)) return(paste0("arm_", arm_id))
        "trunk"
    }
    nodes[, grp := vapply(seq_len(.N), function(i)
        node_group(role[i], arm_id[i], stream_group[i]), character(1L))]

    ## Arms (ordered left-to-right by arm_id)
    arm_ids_sorted <- sort(unique(nodes$arm_id[!is.na(nodes$arm_id)]))
    n_arms_total   <- length(arm_ids_sorted)

    ## ---- Right-offset for side ticks ----
    ## A side box hangs just outside its column's widest box plus padding (based
    ## on grid outputs): the spacer is half that box (boxes centered, joint at center)
    ## plus side_gap_in. Box widths are estimated from a Helvetica advance-width
    ## table over the longest line; plain DOT auto-sizes the real boxes.
    hv_w <- c(278,278,355,556,556,889,667,222,333,333,389,584,278,333,278,278,
              556,556,556,556,556,556,556,556,556,556,278,278,584,584,584,556,
              1015,667,667,722,722,667,611,778,722,278,500,667,556,833,722,778,
              667,778,722,667,611,722,667,944,667,667,611,278,278,278,469,556,
              333,556,556,500,556,556,278,556,556,222,222,500,222,833,556,556,
              556,556,333,500,278,556,500,722,500,500,500,334,260,334,584)
    line_w_in <- function(s) {
        cps <- utf8ToInt(s)
        w <- ifelse(cps >= 32 & cps <= 126, hv_w[pmax(cps - 31, 1)], 556)
        sum(w) * font_size_pt / 1000 / 72
    }
    ## Widest rendered line of a (possibly multi-line) label, ignoring the
    ## bullet/indent markup---the body lines dominate width anyway.
    box_w_in <- function(text) {
        if (is.na(text) || !nzchar(text)) return(0)
        parts <- strsplit(text, "\\n", fixed = FALSE)[[1L]]
        max(vapply(parts, line_w_in, numeric(1L))) + 2 * margin_x_in
    }
    ## Max box width per column group (trunk / arm_N / src_N), over the boxes
    ## actually in that column (main/alloc/arm/source, not side boxes).
    col_max_w <- list()
    for (g in unique(nodes$grp[!is.na(nodes$grp)])) {
        in_col <- which(nodes$grp == g &
                        nodes$role %in% c("main", "alloc", "arm", "source"))
        col_max_w[[g]] <- if (length(in_col))
                              max(vapply(nodes$text[in_col], box_w_in, numeric(1L))) else 1.5
    }
    ## Force a common minimum (widest arm label, plain), which never clips and
    ## matches the grid. Plain only---rich measures wider bold text.
    arm_idx  <- which(nodes$role == "arm")
    arm_w_in <- if (length(arm_idx))
                    max(vapply(nodes$text[arm_idx], box_w_in, numeric(1L))) else NA_real_
    ## Spacer from the column center to a side box's near edge
    spacer_for_parent <- function(parent_id) {
        g <- nodes$grp[match(parent_id, nodes$node_id)]
        half <- if (!is.na(g) && !is.null(col_max_w[[g]])) col_max_w[[g]] / 2 else 0.75
        max(0.10, half + side_gap_in)
    }

    is_times   <- grepl("^Times",   font_family, ignore.case = TRUE)
    is_courier <- grepl("^Courier", font_family, ignore.case = TRUE)

    ## ---- DOT emission preamble ----
    lines <- character()
    lines <- c(lines, "digraph selecta {")
    lines <- c(lines, "  rankdir=TB;")
    if (isTRUE(ortho)) {
        lines <- c(lines, "  splines=ortho;")
        lines <- c(lines, "  concentrate=false;")
        lines <- c(lines, sprintf("  nodesep=%.3f;", node_sep))
        lines <- c(lines, sprintf("  ranksep=%.3f;", rank_sep))
    }
    lines <- c(lines, sprintf(
                          paste0('  node [shape=box, style=filled, fontname="%s", ',
                                 'fontsize=%d, margin="%.3f,%.3f", color="%s"];'),
                          font_family, font_size_pt, margin_x_in, margin_y_in, border_col))

    ## ---- Phase-label declaration ----
    ## Declaring the left-margin label nodes and their top-to-bottom invisible
    ## chain before any content node makes dot place the label column at the
    ## far left: in rankdir=TB, earlier-declared nodes settle leftward when
    ## other constraints are equal, and the heavy vertical chain keeps them in
    ## a single column. Per-row rank-locking (which references content
    ## nodes) is emitted later, once those nodes exist.
    ph_active <- isTRUE(phase_labels) && !is.null(graph$phases) &&
        nrow(graph$phases) > 0L
    Ls <- character(0)
    if (ph_active) {
        ph <- graph$phases
        for (i in seq_len(nrow(ph))) {
            Lid <- sprintf("PL%d", i)
            Ls  <- c(Ls, Lid)
            lines <- c(lines, sprintf(
                                  paste0('  %s [label="%s", shape=box, style="filled", ',
                                         'fillcolor="%s", fontcolor="%s", color="%s", ',
                                         'group="phase_labels"];'),
                                  Lid, esc_dot(ph$label[i]), phase_fill, phase_text_col, phase_fill))
        }
        if (length(Ls) >= 2L)
            for (k in seq_len(length(Ls) - 1L))
                lines <- c(lines, sprintf("  %s -> %s [style=invis, weight=100];",
                                          Ls[k], Ls[k + 1L]))
    }

    ## ---- Per-formatting-mode label and node emission ----
    ## Insert a Graphviz group= attribute into an already-emitted node line so
    ## the spine and each column stay vertically aligned under ortho. Side and
    ## ungrouped nodes are returned unchanged.
    inject_group <- function(line, grp) {
        if (is.na(grp)) return(line)
        sub("\\];\\s*$", sprintf(', group="%s"];', grp), line)
    }

    ## Pin an arm box to the common arm width (see arm_w_in). A Graphviz width=
    ## is a minimum, so this never clips a label; it only pads narrower arms out
    ## to the widest, which is what lets ortho center each split on the spine.
    ## Non-arm nodes are returned unchanged. Used by the plain emitter only.
    inject_width <- function(line, role) {
        if (!isTRUE(role == "arm") || !is.finite(arm_w_in) || arm_w_in <= 0)
            return(line)
        sub("\\];\\s*$", sprintf(', width=%.3f];', arm_w_in), line)
    }

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
            source_header_text = source_header_text,
            bullets = bullets
        )
        for (i in seq_len(nrow(nodes))) {
            lines <- c(lines, inject_group(rich_node(nodes[i]), nodes$grp[i]))
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
            source_header_text = source_header_text,
            bullets = bullets
        )
        for (i in seq_len(nrow(nodes))) {
            lines <- c(lines, inject_group(
                                  inject_width(plain_node(nodes[i]), nodes$role[i]), nodes$grp[i]))
        }
    }

    ## ---- Edges via orthogonal joints ----
    ## Create a grid-like effect through invisible `point` joints: a side box hangs
    ## off a spine tick, split/converge fans distribute through a horizontal bar.
    ## With ortho off, the same joints give clean right angles via splines.
    ## Joint ids P1, P2, ... never collide with the numeric-suffixed nN box ids.
    joint_seq <- 0L
    next_joint <- function() { joint_seq <<- joint_seq + 1L; sprintf("P%d", joint_seq) }
    spacer_seq <- 0L
    next_spacer <- function() { spacer_seq <<- spacer_seq + 1L; sprintf("W%d", spacer_seq) }

    grp_of   <- function(node_id) nodes$grp[match(node_id, nodes$node_id)]
    ## Edge color attribute (no leading space). Sites that follow another
    ## attribute prepend a separator explicitly (", " or " ").
    col_attr <- sprintf('color="%s"', arrow_col)

    ## Center-joint registries
    split_center <- character(0)
    merge_center <- character(0)

    ## Same-rank subgraph
    mk_rank <- function(members) {
        sprintf("  subgraph { rank=same; rankdir=LR; %s; }",
                paste(members, collapse = "; "))
    }

    ## Classify edges so siblings can be paired
    e_type <- edges$edge_type
    excl_i <- which(e_type == "exclude")
    conv_i <- which(e_type == "converge")
    splt_i <- which(e_type == "split")
    ## Flow edges that are the spine continuation paired with a side tick
    paired_flow <- integer(0)

    ## --- Side ticks (exclude + sibling flow) ---
    ## Parent's exclusions form a vertical joint chain on the spine
    side_joint <- character(0)

    ## Side boxes hung outboard-left (two-arm symmetry)
    left_hung_boxes <- integer(0)

    ## ---- Coordinated multi-arm side-box fan (N >= 2 arms) ----
    ## Emit all arms' boxes for the row into one flat rank with ordering edges,
    ## interleaving boxes and joints (Pj1 .. b1 .. Pj2 .. b2 .. ... .. PjN .. bN).
    ## Each box sits just right of its arm, and the shared rank reserves the
    ## inter-arm channels (arms splay to fit).
    
    ## Applies when every arm parents exactly one exclusion and continues to its
    ## own box; other rows fall to the per-parent loop below.
    fan_handled_parents <- integer(0)   # arm parents emitted by the fan
    fan_paired_flow     <- integer(0)   # their sibling flow edges (already drawn)
    if (n_arms_total >= 2L && length(excl_i) > 0L) {
        ## Candidate arm parents: any box in the arm column (arm box or a downstream
        ## `main` continuation) owning exactly one exclusion and continuing straight
        ## down via a `flow` sibling.
        arm_excl_parent <- function(aid) {
            pid <- nodes$node_id[!is.na(nodes$arm_id) & nodes$arm_id == aid &
                                 nodes$role %in% c("arm", "main")]
            pid[vapply(pid, function(p)
                sum(edges$from[excl_i] == p) == 1L &&
                any(e_type == "flow" & edges$from == p), logical(1L))]
        }
        cand         <- lapply(arm_ids_sorted, arm_excl_parent)
        rows_per_arm <- lapply(cand, function(ps) nodes$phase[match(ps, nodes$node_id)])
        ## A fan row is a phase row on which every arm has exactly one such parent.
        common_rows  <- if (length(rows_per_arm)) Reduce(intersect, rows_per_arm) else integer(0)
        for (frow in common_rows) {
            ps <- vapply(seq_along(arm_ids_sorted), function(k) {
                p <- cand[[k]][rows_per_arm[[k]] == frow]
                if (length(p) == 1L) p else NA_integer_
            }, integer(1L))
            if (anyNA(ps)) next  # need exactly one box per arm
            nA    <- length(ps)
            boxes <- vapply(ps, function(p) edges$to[excl_i[edges$from[excl_i] == p]][1L], integer(1L))
            sibs  <- lapply(ps, function(p) which(e_type == "flow" & edges$from == p))
            joints<- vapply(ps, function(p) next_joint(), character(1L))
            ## Joint per arm, grouped with that arm's spine; register for phase-band
            ## anchoring (the leftmost joint sits on arm 1's spine).
            for (k in seq_len(nA)) {
                g <- grp_of(ps[k]); gatt <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;", joints[k], gatt))
                side_joint[as.character(boxes[k])] <- joints[k]
            }
            ## Spine passes straight through each joint to that arm's included box.
            for (k in seq_len(nA)) {
                lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s, weight=100];",
                                          ps[k], joints[k], col_attr))
                if (length(sibs[[k]]))
                    lines <- c(lines, sprintf("  %s -> n%d [%s, weight=100];",
                                              joints[k], edges$to[sibs[[k]][1L]], col_attr))
            }
            ## Tee + channel placement
            ## Two arms: boxes go outboard
            ## Three or more: every box hangs right
            outboard <- (nA == 2L)
            rank_toks <- character(0)
            if (outboard) {
                lines <- c(lines,
                           sprintf("  n%d -> %s [dir=back, %s];", boxes[1L], joints[1L], col_attr),
                           sprintf("  %s -> n%d [%s];", joints[2L], boxes[2L], col_attr),
                           sprintf("  %s -> %s [style=invis];", joints[1L], joints[2L]))
                rank_toks <- c(sprintf("n%d", boxes[1L]), joints[1L],
                               joints[2L], sprintf("n%d", boxes[2L]))
                left_hung_boxes <- c(left_hung_boxes, boxes[1L])
            } else {
                for (k in seq_len(nA)) {
                    lines <- c(lines, sprintf("  %s -> n%d [%s];", joints[k], boxes[k], col_attr))
                    if (k < nA)
                        lines <- c(lines, sprintf("  n%d -> %s [style=invis];", boxes[k], joints[k + 1L]))
                }
                for (k in seq_len(nA)) rank_toks <- c(rank_toks, joints[k], sprintf("n%d", boxes[k]))
            }
            lines <- c(lines, mk_rank(rank_toks))
            fan_handled_parents <- c(fan_handled_parents, ps)
            for (k in seq_len(nA)) if (length(sibs[[k]]))
                                       fan_paired_flow <- c(fan_paired_flow, sibs[[k]][1L])
        }
    }
    paired_flow <- c(paired_flow, fan_paired_flow)

    ## ---- Split-and-recombine with per-stratum side boxes ----
    ## Emit the convergence as one structure mirroring the grid band order:
    ## arm box -> tail joint (tees the side box) -> shared recombine bar one
    ## rank lower -> merge box, so the merge reads as its own band below the side
    ## boxes.
    recombine_children <- integer(0)
    recombine_parents  <- integer(0)
    if (length(conv_i) > 0L && length(excl_i) > 0L) {
        excl_from <- edges$from[excl_i]
        for (m in unique(edges$to[conv_i])) {
            ps <- edges$from[conv_i[edges$to[conv_i] == m]]
            ## Only when at least one converging arm also hangs a side box.
            if (!any(ps %in% excl_from)) next
            recombine_children <- c(recombine_children, m)
            recombine_parents  <- c(recombine_parents, ps)

            ## Left-to-right by arm_id (falling back to node id) so the tail joints
            ## and the bar line up under the arm columns.
            ps <- ps[order(nodes$arm_id[match(ps, nodes$node_id)], ps)]
            nA <- length(ps)

            ## Per-arm tail joint (grouped with the arm column) and the spine drop
            ## into it; weight keeps the arm column straight through the joint.
            Ts <- vapply(seq_len(nA), function(k) next_joint(), character(1L))
            for (k in seq_len(nA)) {
                g <- grp_of(ps[k]); gatt <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;", Ts[k], gatt))
                lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s, weight=100];",
                                          ps[k], Ts[k], col_attr))
            }

            ## Side-box tees on the shared tail rank
            outboard  <- (nA == 2L)
            box_of    <- lapply(ps, function(p) edges$to[excl_i[edges$from[excl_i] == p]])
            for (k in seq_len(nA)) for (b in box_of[[k]]) side_joint[as.character(b)] <- Ts[k]
            rank_toks <- character(0)
            for (k in seq_len(nA)) {
                sb <- box_of[[k]]
                if (length(sb) == 0L) { rank_toks <- c(rank_toks, Ts[k]); next }
                if (outboard && k == 1L) {
                    for (b in sb)
                        lines <- c(lines, sprintf("  n%d -> %s [dir=back, %s];", b, Ts[k], col_attr))
                    ## West anchor
                    Wj <- next_joint()
                    lines <- c(lines,
                               sprintf("  %s [shape=point, width=0, style=invis];", Wj),
                               sprintf("  %s -> n%d [style=invis, weight=100];", Wj, ps[k]))
                    for (b in sb)
                        lines <- c(lines, sprintf("  %s -> n%d [style=invis, weight=100];", Wj, b))
                    rank_toks <- c(Wj, sprintf("n%d", sb), Ts[k], rank_toks)
                    left_hung_boxes <- c(left_hung_boxes, sb)
                } else {
                    for (b in sb)
                        lines <- c(lines, sprintf("  %s -> n%d [%s];", Ts[k], b, col_attr))
                    rank_toks <- c(rank_toks, Ts[k], sprintf("n%d", sb))
                }
            }
            ## Lock the cross-arm order so mincross cannot reshuffle the tail rank
            if (outboard) {
                lines <- c(lines, sprintf("  %s -> %s [style=invis];", Ts[1L], Ts[2L]))
            } else if (nA >= 2L) {
                for (k in seq_len(nA - 1L)) {
                    bk <- box_of[[k]]
                    last_tok <- if (length(bk)) sprintf("n%d", bk[length(bk)]) else Ts[k]
                    lines <- c(lines, sprintf("  %s -> %s [style=invis];", last_tok, Ts[k + 1L]))
                }
            }
            lines <- c(lines, mk_rank(rank_toks))

            ## Recombine bar one rank below
            Js <- vapply(seq_len(nA), function(k) next_joint(), character(1L))
            for (k in seq_len(nA)) {
                g <- grp_of(ps[k]); gatt <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;", Js[k], gatt))
                lines <- c(lines, sprintf("  %s -> %s [arrowhead=none, %s, weight=100];",
                                          Ts[k], Js[k], col_attr))
            }
            if (nA %% 2L == 1L) {
                mid <- Js[(nA + 1L) %/% 2L]; bar <- Js
            } else {
                ## Center joint inherits the merge box's column group
                C <- next_joint()
                cgrp <- grp_of(m); if (is.na(cgrp)) cgrp <- "trunk"
                lines <- c(lines, sprintf(
                                      "  %s [shape=point, width=0, style=invis] [group=\"%s\"];", C, cgrp))
                half <- nA %/% 2L
                bar  <- c(Js[seq_len(half)], C, Js[(half + 1L):nA]); mid <- C
            }
            merge_center[paste(sort(ps), collapse = ",")] <- mid
            if (length(bar) >= 2L)
                lines <- c(lines, sprintf("  %s [arrowhead=none, %s];",
                                          paste(bar, collapse = " -> "), col_attr))
            lines <- c(lines, sprintf("  %s -> n%d [%s];", mid, m, col_attr))
            lines <- c(lines, mk_rank(bar))
        }
    }

    ## ---- Factorial two-child side-box splay (grid outboard rule) ----
    ## Grid hangs a two-child split's side boxes outward
    ## Three-or-more-child parents and all non-factorial rows fall through to
    ## per-parent loop (all boxes right).
    fac_splay_parents <- integer(0L)
    if (length(splt_i) > 0L && length(excl_i) > 0L) {
        split_from <- edges$from[splt_i]
        split_to   <- edges$to[splt_i]
        lvl2_parents <- sort(unique(split_from[split_from %in% split_to]))
        splay_tokens <- character(0L)    # one shared rank across all pairs
        prev_last    <- NA_character_    # previous pair's right box (ordering chain)
        for (parent_node in lvl2_parents) {
            kids <- edges$to[splt_i[split_from == parent_node]]
            if (length(kids) != 2L) next   # two-child splays only
            ok <- vapply(kids, function(k)
                sum(edges$from[excl_i] == k) == 1L &&
                any(e_type == "flow" & edges$from == k), logical(1L))
            if (!all(ok)) next    # each leaf: one box + flow
            if (any(kids %in% c(fan_handled_parents, recombine_parents))) next
            ## Left-to-right child order is parent-major (arm_id, then node id).
            kids  <- kids[order(nodes$arm_id[match(kids, nodes$node_id)], kids)]
            boxes <- vapply(kids, function(k)
                edges$to[excl_i[edges$from[excl_i] == k]][1L], integer(1L))
            sibs  <- lapply(kids, function(k) which(e_type == "flow" & edges$from == k))
            Qs    <- vapply(seq_len(2L), function(k) next_joint(), character(1L))
            for (k in seq_len(2L)) {
                g <- grp_of(kids[k]); gat <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;",
                                          Qs[k], gat))
                side_joint[as.character(boxes[k])] <- Qs[k]
                ## Spine passes straight through the joint to the included box.
                lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s, weight=100];",
                                          kids[k], Qs[k], col_attr))
                if (length(sibs[[k]]))
                    lines <- c(lines, sprintf("  %s -> n%d [%s, weight=100];",
                                              Qs[k], edges$to[sibs[[k]][1L]], col_attr))
            }
            ## Box 1 outboard-left (dir=back tee draws the arrowhead into the box while
            ## ranking it left of the joint); box 2 outboard-right; order the joints.
            lines <- c(lines,
                       sprintf("  n%d -> %s [dir=back, %s];", boxes[1L], Qs[1L], col_attr),
                       sprintf("  %s -> n%d [%s];", Qs[2L], boxes[2L], col_attr),
                       sprintf("  %s -> %s [style=invis];", Qs[1L], Qs[2L]))
            ## Sequence this pair after the previous one in the shared rank.
            if (!is.na(prev_last))
                lines <- c(lines, sprintf("  %s -> n%d [style=invis];",
                                          prev_last, boxes[1L]))
            prev_last    <- sprintf("n%d", boxes[2L])
            splay_tokens <- c(splay_tokens, sprintf("n%d", boxes[1L]), Qs[1L], Qs[2L],
                              sprintf("n%d", boxes[2L]))
            left_hung_boxes   <- c(left_hung_boxes, boxes[1L])
            fac_splay_parents <- c(fac_splay_parents, kids)
            for (k in seq_len(2L)) if (length(sibs[[k]]))
                                       paired_flow <- c(paired_flow, sibs[[k]][1L])
        }
        if (length(splay_tokens) > 0L)
            lines <- c(lines, mk_rank(splay_tokens))
    }

    ## Per-parent side loop handles every exclusion parent not drawn by the fan,
    ## the split-and-recombine path, or the factorial two-child splay above.
    excl_parents <- setdiff(unique(edges$from[excl_i]),
                            c(fan_handled_parents, recombine_parents,
                              fac_splay_parents))
    for (parent in excl_parents) {
        these <- excl_i[edges$from[excl_i] == parent]
        sides <- edges$to[these]
        g     <- grp_of(parent)
        g_attr <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
        Ps <- vapply(seq_along(sides), function(k) next_joint(), character(1L))
        for (k in seq_along(sides))
            side_joint[as.character(sides[k])] <- Ps[k]
        ## All joints sit on the spine column (parent's group).
        for (P in Ps)
            lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;",
                                      P, g_attr))
        ## Spine: parent -> P1 -> P2 -> ... (straight, no arrowheads between).
        lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s];",
                                  parent, Ps[1L], col_attr))
        if (length(Ps) >= 2L)
            for (k in seq_len(length(Ps) - 1L))
                lines <- c(lines, sprintf("  %s -> %s [arrowhead=none, %s];",
                                          Ps[k], Ps[k + 1L], col_attr))
        ## Each joint tees one side box with a single direct edge
        for (k in seq_along(sides)) {
            sidenode <- sprintf("n%d", sides[k])
            lines <- c(lines,
                       sprintf("  %s -> %s [%s];", Ps[k], sidenode, col_attr),
                       mk_rank(c(Ps[k], sidenode)))
        }
        ## Last joint continues the spine to the included (remaining) box.
        sib <- which(e_type == "flow" & edges$from == parent)
        if (length(sib) >= 1L) {
            included <- edges$to[sib[1L]]
            lines <- c(lines, sprintf("  %s -> n%d [%s];",
                                      Ps[length(Ps)], included, col_attr))
            paired_flow <- c(paired_flow, sib[1L])
        }
    }

    ## --- Split fans (one alloc -> many arms) ---
    ## Group split edges by parent; route each through a chain of joints (one per
    ## child) joined into a horizontal distributor bar. A source convergence pins
    ## the trunk to the middle source column
    has_src_conv <- any(nodes$role %in% c("source", "source_header"))

    ## Factorial split settings---one sub-distributor bar per parent arm
    factorial_bars       <- list()
    factorial_bar_pkey   <- numeric(0L)
    factorial_bar_weight <- 1
    if (length(splt_i) > 0L) {
        for (parent in unique(edges$from[splt_i])) {
            kids <- edges$to[splt_i[edges$from[splt_i] == parent]]
            Js <- vapply(seq_along(kids), function(k) next_joint(), character(1L))
            ## Each joint inherits its child's column group so it sits above it.
            for (k in seq_along(kids)) {
                g <- grp_of(kids[k])
                g_attr <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;",
                                          Js[k], g_attr))
            }
            n_k <- length(Js)
            ## The center joint and any symmetry spacer inherit the split parent's
            ## column group
            pgrp     <- grp_of(parent)
            pgrp_att <- if (!is.na(pgrp)) pgrp else "trunk"
            parent_on_trunk <- is.na(pgrp) || identical(pgrp, "trunk")
            if (n_k %% 2L == 1L) {
                ## Odd: spine enters the middle arm's joint; bar is the joint chain.
                mid <- Js[(n_k + 1L) %/% 2L]
                bar <- Js
            } else {
                ## Even: insert a parent-grouped center joint between the two middle arm
                ## joints so the spine enters dead center
                C    <- next_joint()
                lines <- c(lines, sprintf(
                                      "  %s [shape=point, width=0, style=invis] [group=\"%s\"];", C, pgrp_att))
                half <- n_k %/% 2L
                bar  <- c(Js[seq_len(half)], C, Js[(half + 1L):n_k])
                mid  <- C
                if (has_src_conv && parent_on_trunk) {
                    ## Pinned trunk: a source convergence anchors the spine to the middle
                    ## source column
                    arm_ids <- nodes$arm_id[match(kids, nodes$node_id)]
                    col_idx <- which(nodes$arm_id %in% arm_ids &
                                     nodes$role %in% c("arm", "endpoint"))
                    col_w   <- if (length(col_idx))
                                   max(vapply(nodes$text[col_idx], box_w_in, numeric(1L)))
                               else 0
                    Q <- next_joint()
                    lines <- c(lines, sprintf(
                                          paste0("  %s [shape=box, style=invis, fixedsize=true, ",
                                                 "width=%.3f, height=0.02, label=\"\", group=\"trunk\"];"),
                                          Q, max(0.02, col_w - node_sep)))
                    ## Group alignment is edge-based: tie the spacer to the trunk
                    lines <- c(lines, sprintf("  %s -> %s [style=invis];", C, Q))
                    lines <- c(lines, mk_rank(c(sprintf("n%d", kids[half]),
                                                Q, sprintf("n%d", kids[half + 1L]))))
                }
            }
            split_center[paste(sort(kids), collapse = ",")] <- mid
            ## Horizontal distributor bar joining the joints (and any center joint)
            bar_w_att <- if (!parent_on_trunk && factorial_bar_weight != 1)
                             sprintf(", weight=%g", factorial_bar_weight) else ""
            if (length(bar) >= 2L)
                lines <- c(lines, sprintf("  %s [arrowhead=none, %s%s];",
                                          paste(bar, collapse = " -> "), col_attr,
                                          bar_w_att))
            ## Spine enters the center of the bar
            lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s, weight=20];",
                                      parent, mid, col_attr))
            ## Each arm joint drops to its child with an arrow.
            for (k in seq_along(kids))
                lines <- c(lines, sprintf("  %s -> n%d [%s];", Js[k], kids[k], col_attr))
            if (parent_on_trunk) {
                ## First-level split: lock this bar onto its own rank, as before.
                lines <- c(lines, mk_rank(bar))
            } else {
                ## Factorial sub-distributor: defer ranking. Collect the bar so all
                ## factorial bars share one rank (and one ordering chain) after the loop.
                factorial_bars[[length(factorial_bars) + 1L]] <- bar
                factorial_bar_pkey <- c(factorial_bar_pkey,
                                        nodes$arm_id[match(parent, nodes$node_id)])
            }
        }
    }

    ## --- Factorial-split ordering ---
    ## Each factorial sub-distributor centers its own parent's children, but with a
    ## separate rank per bar the crossing minimizer can swap sibling subtrees.
    if (length(factorial_bars) >= 1L) {
        ord         <- order(factorial_bar_pkey)
        factorial_bars <- factorial_bars[ord]
        if (length(factorial_bars) >= 2L) {
            for (b in seq_len(length(factorial_bars) - 1L)) {
                last_j  <- factorial_bars[[b]][length(factorial_bars[[b]])]
                first_j <- factorial_bars[[b + 1L]][1L]
                lines <- c(lines, sprintf("  %s -> %s [style=invis, weight=20];",
                                          last_j, first_j))
            }
        }
        lines <- c(lines, mk_rank(unlist(factorial_bars)))
    }

    ## --- Converge fans (many streams -> one merge) ---
    ## Mirror of the split fan: parents drop into per-parent joints joined by a
    ## visible bar
    if (length(conv_i) > 0L) {
        ## Skip any merge whose convergence was drawn by the split-and-recombine
        ## path above (those carry per-stratum side boxes and are routed there).
        for (child in setdiff(unique(edges$to[conv_i]), recombine_children)) {
            parents <- edges$from[conv_i[edges$to[conv_i] == child]]
            Js <- vapply(seq_along(parents), function(k) next_joint(), character(1L))
            for (k in seq_along(parents)) {
                g <- grp_of(parents[k])
                g_attr <- if (!is.na(g)) sprintf(" [group=\"%s\"]", g) else ""
                lines <- c(lines, sprintf("  %s [shape=point, width=0, style=invis]%s;",
                                          Js[k], g_attr))
            }
            ## Each parent drops into its joint (no arrowhead; the bar is the join).
            for (k in seq_along(parents))
                lines <- c(lines, sprintf("  n%d -> %s [arrowhead=none, %s];",
                                          parents[k], Js[k], col_attr))
            n_p <- length(Js)
            if (n_p %% 2L == 1L) {
                mid <- Js[(n_p + 1L) %/% 2L]
                bar <- Js
            } else {
                ## Center joint inherits the merge box's column group ("trunk" at the
                ## trunk level, "arm_<id>" for a factorial subtree merging with no
                ## per-arm side boxes) so the single arrow stays under that subtree.
                C   <- next_joint()
                cgrp <- grp_of(child); if (is.na(cgrp)) cgrp <- "trunk"
                lines <- c(lines, sprintf(
                                      "  %s [shape=point, width=0, style=invis] [group=\"%s\"];", C, cgrp))
                half <- n_p %/% 2L
                bar  <- c(Js[seq_len(half)], C, Js[(half + 1L):n_p])
                mid  <- C
            }
            merge_center[paste(sort(parents), collapse = ",")] <- mid
            ## Visible bar (plain, not bold) so the convergence bracket renders.
            if (length(bar) >= 2L)
                lines <- c(lines, sprintf("  %s [arrowhead=none, %s];",
                                          paste(bar, collapse = " -> "), col_attr))
            ## Center of the bar carries the single arrow down to the merge box.
            lines <- c(lines, sprintf("  %s -> n%d [%s];",
                                      mid, child, col_attr))
            lines <- c(lines, mk_rank(bar))
        }
    }

    ## --- Spine straightening across split / converge blocks ---
    ## Couple each split center to the converge center recombining the same arms,
    ## so the trunk segments above and below stay collinear across the arm and
    ## side-box ranks between them
    parent_arm_ids <- unique(nodes$arm_parent[!is.na(nodes$arm_level) &
                                              nodes$arm_level == 2L])
    for (k in intersect(names(split_center), names(merge_center))) {
        key_arms <- nodes$arm_id[match(
                              as.integer(strsplit(k, ",", fixed = TRUE)[[1L]]), nodes$node_id)]
        if (any(key_arms %in% parent_arm_ids)) next
        lines <- c(lines, sprintf("  %s -> %s [style=invis, weight=100];",
                                  split_center[[k]], merge_center[[k]]))
    }

    ## --- Remaining plain flow edges (not consumed by a side tick) ---
    routed <- c(excl_i, conv_i, splt_i, paired_flow)
    for (ei in setdiff(seq_len(nrow(edges)), routed)) {
        if (e_type[ei] != "flow") next
        lines <- c(lines, sprintf("  n%d -> n%d [%s];",
                                  edges$from[ei], edges$to[ei], col_attr))
    }

    ## ---- Source-header positioning (multi-source flows) ----
    ## Headers are pure labels with no edges, so Graphviz packs all parentless top
    ## nodes into one interleaved row
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

    ## ---- Phase-label rank-locking (emitted last) ----
    ## Each label locks onto the first row of its band, listed first in an LR rank
    ## so it stays leftmost.
    if (ph_active) {
        ## All diagram rows that carry anything a label can lock onto: content
        ## rows plus side-box rows (a side box's joint can be locked onto).
        lockable_rows <- sort(unique(nodes$phase[nodes$role != "source_header"]))
        ## First lockable row at or after a band's start, capped at its end; if
        ## none lies inside, take the nearest lockable row to the start.
        anchor_row_for_band <- function(start, end) {
            inside <- lockable_rows[lockable_rows >= start & lockable_rows <= end]
            if (length(inside)) return(inside[1L])
            if (!length(lockable_rows)) return(NA_integer_)
            lockable_rows[which.min(abs(lockable_rows - start))]
        }
        ranked_nodes <- integer(0)    # node ids a label is ranked with
        band_arow    <- rep(NA_integer_, nrow(ph))
        for (i in seq_len(nrow(ph))) {
            Lid  <- sprintf("PL%d", i)
            arow <- anchor_row_for_band(ph$phase_start[i], ph$phase_end[i])
            if (is.na(arow)) next
            band_arow[i] <- arow
            row_nodes <- nodes$node_id[nodes$phase == arow]
            content   <- row_nodes[!(nodes$role[match(row_nodes, nodes$node_id)]
                %in% c("side", "source_header"))]
            if (length(content)) {
                ## PRISMA/MOOSE-style---align the label to the header row instead
                hdrs <- row_nodes[nodes$role[match(row_nodes, nodes$node_id)]
                                  == "source_header"]
                srcs <- content[nodes$role[match(content, nodes$node_id)] == "source"]
                if (length(hdrs) && length(srcs)) {
                    toks <- sprintf("n%d", hdrs)
                    ranked_nodes <- c(ranked_nodes, hdrs)
                } else {
                    ## Row has spine/arm/endpoint content: rank with those nodes only,
                    ## so the label pins left without joints nudging the centered spine.
                    toks <- sprintf("n%d", content)
                    ranked_nodes <- c(ranked_nodes, content)
                }
            } else {
                ## Side-box-only row
                left_here <- row_nodes[row_nodes %in% left_hung_boxes]
                if (length(left_here)) {
                    ## Widest outboard-left box on the row reaches furthest left.
                    target <- left_here[which.max(vapply(
                        nodes$text[match(left_here, nodes$node_id)], box_w_in, numeric(1L)))]
                    toks <- sprintf("n%d", target)
                } else {
                    js <- vapply(row_nodes, function(nid)
                        unname(side_joint[as.character(nid)]), character(1L))
                    js <- js[!is.na(js) & nzchar(js)]
                    toks <- if (length(js)) js[1L] else character(0)
                }
            }
            if (length(toks)) lines <- c(lines, mk_rank(c(Lid, toks)))
        }

        ## ---- Reserve the label column against wide leftmost-column boxes ----
        ## Add an invisible strut (widest label's width) chained into the phase_labels
        ## group at that box's row, ordered to its left, clearing the column without
        ## moving labels.
        lbl_w <- max(c(vapply(ph$label, box_w_in, numeric(1L)), 0.5))
        strut_seq <- 0L
        add_strut <- function(target, rowmates, band_idx) {
            strut_seq <<- strut_seq + 1L
            Sid  <- sprintf("PS%d", strut_seq)
            lab  <- sprintf("PL%d", band_idx)
            wrow <- nodes$phase[match(target, nodes$node_id)]
            ## Chain from the band's label toward the strut's row (the box's row sits
            ## at or below the label's anchor row in every generated case).
            chain <- if (!is.na(band_arow[band_idx]) && wrow < band_arow[band_idx])
                         sprintf("  %s -> %s [style=invis, weight=100];", Sid, lab)
                     else
                         sprintf("  %s -> %s [style=invis, weight=100];", lab, Sid)
            c(sprintf('  %s [shape=box, style=invis, width=%.3f, height=0.10, label="", group="phase_labels"];',
                      Sid, lbl_w),
              chain,
              sprintf("  %s -> n%d [style=invis];", Sid, target),
              mk_rank(c(Sid, sprintf("n%d", rowmates))))
        }
        band_of <- function(row) {
            bi <- which(ph$phase_start <= row & ph$phase_end >= row)
            if (length(bi)) bi[1L] else NA_integer_
        }
        widest_in <- function(ids)
            ids[which.max(vapply(nodes$text[match(ids, nodes$node_id)],
                                 box_w_in, numeric(1L)))]
        ## Leftmost arm column: arm/endpoint boxes of the smallest arm_id
        arms_recombine <- length(conv_i) > 0L &&
            any(nodes$role[match(edges$from[conv_i], nodes$node_id)]
                %in% c("arm", "endpoint"))
        ## With a factorial (two-level) split the first-level arms are centered over
        ## their children rather than occupying the outer columns, so the leftmost
        ## real column is the first leaf arm, not the (smaller-id) parent.
        has_factorial_arms <- any(nodes$role == "arm" &
                                  !is.na(nodes$arm_level) & nodes$arm_level == 2L)
        arm_mask <- nodes$role %in% c("arm", "endpoint") & !is.na(nodes$arm_id)
        if (has_factorial_arms)
            arm_mask <- arm_mask & (is.na(nodes$arm_level) | nodes$arm_level != 1L)
        if (any(arm_mask) && !arms_recombine) {
            la      <- min(nodes$arm_id[arm_mask])
            col_ids <- nodes$node_id[arm_mask & nodes$arm_id == la]
            ## Skip when any box in this column already anchors a label
            if (!any(col_ids %in% ranked_nodes)) {
                target   <- widest_in(col_ids)
                wrow     <- nodes$phase[match(target, nodes$node_id)]
                rowmates <- nodes$node_id[nodes$phase == wrow &
                                          nodes$role %in% c("arm", "endpoint")]
                bidx <- band_of(wrow)
                if (!is.na(bidx)) lines <- c(lines, add_strut(target, rowmates, bidx))
            }
        }
        ## Leftmost source column = the first parent of the converge fan (source
        ## boxes lay out in converge-edge order, so the first converged parent is
        ## leftmost).
        src_mask        <- nodes$role == "source"
        has_src_headers <- any(nodes$role == "source_header")
        if (any(src_mask) && !has_src_headers) {
            target <- NA_integer_
            if (length(conv_i)) {
                merge_child <- edges$to[conv_i][1L]
                parents <- edges$from[conv_i[edges$to[conv_i] == merge_child]]
                if (length(parents)) target <- parents[1L]
            }
            identity_order <- !is.na(target) && target == min(nodes$node_id[src_mask])
            if (identity_order && !(target %in% ranked_nodes)) {
                wrow     <- nodes$phase[match(target, nodes$node_id)]
                rowmates <- nodes$node_id[nodes$phase == wrow & nodes$role == "source"]
                bidx <- band_of(wrow)
                if (!is.na(bidx)) lines <- c(lines, add_strut(target, rowmates, bidx))
            }
        }
    }

    lines <- c(lines, "}")
    dot_src <- paste(lines, collapse = "\n")

    ## Optionally emit the generated DOT source and its salient settings
    debug_emit("export_dot() source",
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
                                source_header_fill, source_header_text,
                                bullets = FALSE) {

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

    build_label <- function(text, n, role, reasons = NULL) {
        has_text    <- nchar(text) > 0L
        n_str       <- fn(n)
        has_reasons <- !is.null(reasons) && length(reasons) > 0L

        ## A reason entry is a leaf (scalar count) or a parent (named numeric
        ## vector of sub-counts, whose displayed count is their sum).
        is_parent    <- function(v) length(v) > 1L || !is.null(names(v))
        reason_count <- function(v) if (is_parent(v)) sum(v) else v
        one_line     <- function(label, cnt)
            if (isTRUE(count_first)) sprintf("%s %s", esc(fn(cnt)), esc(label))
            else                     sprintf("%s (n = %s)", esc(label), esc(fn(cnt)))
        ## Flat lines (no indent), for centered main/endpoint breakdowns.
        reason_lines <- function()
            vapply(seq_along(reasons), function(j)
                one_line(names(reasons)[j], reason_count(reasons[[j]])),
                character(1L))
        ## Indented, possibly two-level lines for side/source boxes: parents get
        ## the bullet/indent, sub-reasons a deeper indent and an en-dash.
        reason_block <- function() {
            p_ind <- if (isTRUE(bullets)) "  \u2022 " else "   "
            c_ind <- if (isTRUE(bullets)) "    \u2013 " else "      "
            out <- character()
            for (j in seq_along(reasons)) {
                v <- reasons[[j]]
                out <- c(out, paste0(p_ind, one_line(names(reasons)[j], reason_count(v))))
                if (is_parent(v))
                    out <- c(out, vapply(seq_along(v), function(k)
                        paste0(c_ind, one_line(names(v)[k], v[k])), character(1L)))
            }
            out
        }

        if (role == "source_header")
            return(esc(text))

        ## Side (exclusion) and source boxes: bold-style inline header with the
        ## count, then indented reason lines, all left-justified (Graphviz "\l").
        if (role == "side" || role == "source") {
            header <- if (isTRUE(count_first))
                          sprintf("%s %s", esc(n_str), esc(text))
                      else
                          sprintf("%s (n = %s)", esc(text), esc(n_str))
            if (!has_reasons)
                return(paste0(header, "\\l"))
            return(paste0(paste(c(header, reason_block()), collapse = "\\l"), "\\l"))
        }

        ## Main / endpoint / alloc boxes: centered. A breakdown is appended as
        ## centered lines.
        base <- if (!has_text)
                    sprintf("n = %s", esc(n_str))
                else if (isTRUE(count_first))
                    sprintf("%s %s", esc(n_str), esc(text))
                else
                    sprintf("%s\\nn = %s", esc(text), esc(n_str))
        if (!has_reasons)
            return(base)
        paste(c(base, reason_lines()), collapse = "\\n")
    }

    function(nd) {
        lbl <- build_label(nd$text, nd$n, nd$role, nd$reasons[[1L]])
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
                               source_header_fill, source_header_text,
                               bullets = FALSE) {

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

    ## Width of the widest sub-reason line, "label (n = count)", optionally
    ## with a left indent (side boxes) measured in the plain face.
    reason_width <- function(reasons, indented) {
        if (is.null(reasons) || length(reasons) == 0L) return(0)
        is_parent <- function(v) length(v) > 1L || !is.null(names(v))
        p_ind <- if (indented) measure_pt("   ", "plain") else 0
        if (indented && isTRUE(bullets)) p_ind <- p_ind + measure_pt("\u2022", "plain")
        c_ind <- if (indented) measure_pt("      ", "plain") else 0
        if (indented && isTRUE(bullets)) c_ind <- c_ind + measure_pt("\u2013", "plain")
        w <- 0
        for (j in seq_along(reasons)) {
            v   <- reasons[[j]]
            cnt <- if (is_parent(v)) sum(v) else v
            w <- max(w, p_ind + measure_pt(
                                    sprintf("%s (n = %s)", names(reasons)[j], fn(cnt)), "plain"))
            if (is_parent(v))
                for (k in seq_along(v))
                    w <- max(w, c_ind + measure_pt(
                                            sprintf("%s (n = %s)", names(v)[k], fn(v[k])), "plain"))
        }
        w
    }

    ## Per-line maximum width (used to set the `width=` attribute below).
    width_for_node <- function(text, n, role, reasons = NULL) {
        n_str <- fn(n)
        base <- if (role == "source_header") {
                    measure_pt(text, "bold")
                } else if (!nzchar(text)) {
                    measure_pt("n", "italic") + measure_pt(sprintf(" = %s", n_str))
                } else if (role == "side" || role == "source") {
                    ## Inline "label (n = X)": bold label plus normal " (n = X)".
                    measure_pt(text, "bold") + measure_pt(sprintf(" (n = %s)", n_str))
                } else if (isTRUE(count_first)) {
                    measure_pt(n_str, "bold") + measure_pt(sprintf(" %s", text))
                } else {
                    max(measure_pt(text, "bold"),
                        measure_pt("n", "italic") +
                        measure_pt(sprintf(" = %s", n_str)))
                }
        max(base, reason_width(reasons, role == "side" || role == "source"))
    }

    ## Trailing-whitespace centering correction (Graphviz under-measures
    ## Helvetica-Bold by ~0.22 pt/char)
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

    ## Sub-reason lines as HTML: "label (n = count)". Side boxes indent and
    ## left-align them; elsewhere (e.g. endpoint breakdowns) they are centered.
    reason_html <- function(reasons, left) {
        ind <- if (!left) ""
               else if (isTRUE(bullets)) "&nbsp;&nbsp;&#8226;&nbsp;"
               else "&nbsp;&nbsp;&nbsp;"
        vapply(seq_along(reasons), function(j)
            sprintf("%s%s (<I>n</I> = %s)", ind, esc(names(reasons)[j]),
                    esc(fn(reasons[j]))), character(1L))
    }

    build_label <- function(text, n, role, reasons = NULL) {
        has_text    <- nchar(text) > 0L
        n_str       <- fn(n)
        has_reasons <- !is.null(reasons) && length(reasons) > 0L

        if (role == "source_header")
            return(sprintf("<<B>%s</B>%s>", esc(text), trailing_ws(text)))

        ## Side (exclusion) and source boxes: inline bold header with the
        ## count, then indented reason lines; every line carries a left-aligned
        ## break so the box reads flush-left.
        if (role == "side" || role == "source") {
            header <- if (isTRUE(count_first))
                          sprintf("<B>%s</B> %s", esc(n_str), esc(text))
                      else
                          sprintf("<B>%s</B> (<I>n</I> = %s)", esc(text), esc(n_str))
            if (!has_reasons)
                return(sprintf("<%s>", header))
            parts <- c(header, reason_html(reasons, TRUE))
            return(sprintf("<%s>",
                           paste0(parts, '<BR align="left"/>', collapse = "")))
        }

        ## Main / endpoint boxes: centered. A breakdown is appended as
        ## centered lines below the count.
        base <- if (!has_text) {
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
        if (!has_reasons)
            return(sprintf("<%s>", base))
        sprintf("<%s<BR/>%s>", base,
                paste(reason_html(reasons, FALSE), collapse = "<BR/>"))
    }

    function(nd) {
        rsn      <- nd$reasons[[1L]]
        lbl      <- build_label(nd$text, nd$n, nd$role, rsn)
        width_pt <- width_for_node(nd$text, nd$n, nd$role, rsn)
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
