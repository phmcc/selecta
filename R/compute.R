### * Main functions

#' Compute Enrollment Counts
#'
#' Walks the step list and resolves all counts, producing a graph of
#' nodes, edges, and phases. Maintains a generalized stream model where
#' parallel tracks (from \code{sources()} or \code{stratify()}) are
#' stored as a list of active streams.
#'
#' @param x A \code{selecta} object.
#' @return A list with components \code{nodes}, \code{edges}, and
#'   \code{phases}, each a \code{data.table}.
#' @keywords internal
compute <- function(x) {

    nodes  <- vector("list", length(x$steps) * 8L)
    edges  <- vector("list", length(x$steps) * 8L)
    phases <- vector("list", length(x$steps))
    nid    <- 0L
    eid    <- 0L
    pid    <- 0L

    add_node <- function(text, n, role, reasons = NULL,
                         arm_id = NA_integer_, phase = NA_integer_,
                         stream_group = NA_character_,
                         sublabel = NA_character_,
                         arm_parent = NA_integer_, arm_level = NA_integer_) {
        nid <<- nid + 1L
        ## Defensive: drop NULL scalars (which would create length-0 list
        ## elements and trigger rbindlist warnings during finalization).
        if (is.null(sublabel))     sublabel     <- NA_character_
        if (is.null(stream_group)) stream_group <- NA_character_
        if (is.null(arm_id))       arm_id       <- NA_integer_
        if (is.null(arm_parent))   arm_parent   <- NA_integer_
        if (is.null(arm_level))    arm_level    <- NA_integer_
        nodes[[nid]] <<- list(
            node_id = nid, text = text, n = n,
            role = role, reasons = list(reasons),
            arm_id = arm_id, phase = phase,
            stream_group = stream_group,
            sublabel = sublabel,
            arm_parent = arm_parent, arm_level = arm_level
        )
        nid
    }

    add_edge <- function(from, to, edge_type = "flow") {
        eid <<- eid + 1L
        edges[[eid]] <<- list(from = from, to = to, edge_type = edge_type)
    }

    add_phase <- function(label, phase_start, phase_end) {
        pid <<- pid + 1L
        phases[[pid]] <<- list(
            label = label, phase_start = phase_start, phase_end = phase_end
        )
    }

    diagram_phase <- 0L

    current_phase_label <- NULL
    current_phase_start <- NA_integer_

    close_phase <- function(end_phase) {
        if (!is.null(current_phase_label) && !is.na(current_phase_start))
            add_phase(current_phase_label, current_phase_start, end_phase)
    }

    ## Record phase start on first node following a phase() step
    try_start_phase <- function() {
        if (!is.null(current_phase_label) && is.na(current_phase_start))
            current_phase_start <<- diagram_phase
    }

    ## ---- Stream state ----
    ## Each stream: list(id, label, group, last_node, data, n, arm_id, parent, level)
    ## where arm_id is a STABLE, globally unique arm identifier (unlike the
    ## positional stream index), so factorial second-level arms get their own
    ## columns/groups without colliding with their first-level parents.
    streams   <- list()
    n_streams <- 0L
    in_arms   <- FALSE   # TRUE after stratify() specifically

    ## Globally unique arm-id allocator (shared across factorial split levels).
    arm_seq     <- 0L
    next_arm_id <- function() { arm_seq <<- arm_seq + 1L; arm_seq }

    ## Single-stream tracking
    last_main   <- NULL     # node_id for single-stream mode

    if (x$mode == "data") {
        current_data <- list(.all = copy(x$data))
    } else {
        current_n <- x$n_start
    }

    ## Next-step type lookup (skipping phase steps)
    steps   <- x$steps
    n_steps <- length(steps)
    step_types <- vapply(steps, `[[`, character(1L), "type")
    .next_type_vec <- rep(NA_character_, n_steps)
    last_seen <- NA_character_
    for (.j in rev(seq_len(n_steps))) {
        .next_type_vec[.j] <- last_seen
        if (step_types[.j] != "phase") last_seen <- step_types[.j]
    }
    next_type <- function(idx) .next_type_vec[idx]

    ## Detect multi-source entry
    has_sources <- n_steps > 0L && step_types[1L] == "sources"

    if (!has_sources) {
        ## Standard single-entry flow
        diagram_phase <- diagram_phase + 1L
        start_id <- add_node(
            text = x$label, n = x$n_start, role = "main", phase = diagram_phase
        )
        last_main <- start_id
    }
    ## Multi-source flows: starting nodes created by the sources handler

    ## ---- Step dispatch ----
    ## diagram_phase of the entry node(s). Set here for enroll() (single
    ## entry); for sources() it is set when that step runs. Lets the first
    ## phase encompass the entry box(es).
    entry_phase    <- if (!has_sources) diagram_phase else NA_integer_
    prev_phase_seen <- FALSE

    for (si in seq_len(n_steps)) {
        step <- steps[[si]]

        ## ---- Phase ----
        if (step$type == "phase") {
            close_phase(diagram_phase)
            current_phase_label <- step$label
            ## A phase normally starts at the next diagram phase. The exception is
            ## the first phase declared immediately after the entry step (enroll()
            ## or sources()): it encompasses the entry node(s), detected by the
            ## entry phase still being current with no prior phase opened.
            current_phase_start <- if (!prev_phase_seen &&
                                       !is.na(entry_phase) &&
                                       diagram_phase == entry_phase)
                                       entry_phase
                                   else
                                       diagram_phase + 1L
            prev_phase_seen <- TRUE
            next
        }

        ## ---- Sources ----
        if (step$type == "sources") {
            diagram_phase <- diagram_phase + 1L
            ## Record the entry diagram phase so a phase() declared immediately
            ## after sources() encompasses the source row (mirrors the enroll()
            ## case, where entry_phase is set before the loop).
            if (!prev_phase_seen && is.na(entry_phase)) entry_phase <- diagram_phase
            try_start_phase()

            ## One header node and one source node per group
            stream_id <- 0L

            for (gi in seq_along(step$groups)) {
                grp <- step$groups[[gi]]
                stream_id <- stream_id + 1L
                group_n <- sum(grp$counts)

                ## Header node (if header is defined)
                if (!is.null(grp$header)) {
                    hdr_id <- add_node(
                        text = grp$header, n = 0L, role = "source_header",
                        phase = diagram_phase,
                        stream_group = grp$group
                    )
                }

                ## Source group node: individual sources as sub-items
                src_reasons <- setNames(grp$counts, grp$labels)
                src_id <- add_node(
                    text = "Records identified", n = group_n, role = "source",
                    reasons = src_reasons,
                    phase = diagram_phase,
                    stream_group = grp$group
                )

                streams[[stream_id]] <- list(
                    id        = stream_id,
                    label     = grp$group,
                    group     = grp$group,
                    last_node = src_id,
                    n         = group_n
                )
            }
            n_streams <- stream_id
            next
        }

        ## ---- Combine ----
        if (step$type == "combine") {
            diagram_phase <- diagram_phase + 1L
            try_start_phase()

            ## ---- Factorial combine: peel the innermost split level ----
            ## When the active streams are second-level sub-arms, combine() converges
            ## each parent's sub-arms into one merge box per parent, restoring the
            ## first-level arms (one active split level remains). This mirrors the
            ## builder's depth model and is the rare counterpart to a double split.
            is_factorial <- in_arms && length(streams) > 0L &&
                any(vapply(streams, function(s) isTRUE(s$level == 2L), logical(1L)))
            if (is_factorial) {
                ## Parents in first-encounter (left-to-right) order.
                parents <- unique(vapply(streams, function(s) s$parent, integer(1L)))
                merged_streams <- vector("list", length(parents))
                if (x$mode == "data") merged_data <- list()
                for (k in seq_along(parents)) {
                    pa      <- parents[k]
                    members <- Filter(function(s) isTRUE(s$parent == pa), streams)
                    if (x$mode == "data") {
                        ## Recombine the parent's sub-arm partitions; the count is exact.
                        mkeys <- vapply(members, function(s) as.character(s$arm_id),
                                        character(1L))
                        pdat  <- rbindlist(current_data[mkeys])
                        mn    <- .row_count(pdat)
                        merged_data[[as.character(pa)]] <- pdat
                    } else {
                        psum <- sum(vapply(members, function(s) s$n, numeric(1L)))
                        ## A per-parent n may be supplied as a vector (indexed by parent
                        ## order); otherwise the shared scalar, or the sub-arm sum.
                        mn <- if (!is.null(step$n)) {
                                  if (length(step$n) == length(parents)) step$n[k] else step$n[1L]
                              } else psum
                        if (!is.null(step$n) && !is.na(mn) && mn != psum)
                            warn_arithmetic(
                                "Combine '%s' is given as %s but the incoming sub-arms sum to %s.",
                                step$label, mn, psum)
                    }
                    merge_id <- add_node(
                        text = step$label, n = mn, role = "main",
                        reasons = step$reasons, sublabel = step$sublabel,
                        arm_id = pa, arm_level = 1L, phase = diagram_phase
                    )
                    for (m in members)
                        add_edge(m$last_node, merge_id, edge_type = "converge")
                    merged_streams[[k]] <- list(
                        id = pa, arm_id = pa, parent = NA_integer_, level = 1L,
                        label = step$label, group = NA_character_,
                        last_node = merge_id, n = mn
                    )
                }
                streams   <- merged_streams
                n_streams <- length(streams)
                if (x$mode == "data") current_data <- merged_data
                ## in_arms stays TRUE: one split level is still active.
                next
            }

            total_n <- sum(vapply(streams, function(s) s$n, numeric(1L)))
            merge_n <- if (!is.null(step$n)) step$n else total_n
            ## A manually supplied combine total should match the streams feeding it.
            if (x$mode == "manual" && !is.null(step$n) &&
                length(step$n) == 1L && !is.na(step$n) && step$n != total_n) {
                warn_arithmetic(
                    "Combine '%s' is given as %s but the incoming streams sum to %s.",
                    step$label, step$n, total_n)
            }

            ## Build node text: label + optional sublabel
            node_text <- step$label
            node_sublabel <- step$sublabel

            ## Merged node
            merge_id <- add_node(
                text = node_text, n = merge_n, role = "main",
                reasons = step$reasons,
                phase = diagram_phase,
                sublabel = node_sublabel
            )

            if (in_arms) {
                ## Post-stratify combine: one converge edge per arm stream
                for (i in seq_len(n_streams)) {
                    add_edge(streams[[i]]$last_node, merge_id, edge_type = "converge")
                }

                ## Recombine data in data mode
                if (x$mode == "data") {
                    arm_keys_cur <- vapply(streams, function(s) as.character(s$arm_id),
                                           character(1L))
                    current_data <- list(.all = rbindlist(current_data[arm_keys_cur]))
                }

                in_arms <- FALSE
            } else {
                ## Post-sources combine: one converge edge per source group
                group_last <- vapply(
                    split(streams, vapply(streams, `[[`, character(1L), "group")),
                    function(ss) ss[[length(ss)]]$last_node,
                    integer(1L)
                )
                for (g_last_node in group_last) {
                    add_edge(g_last_node, merge_id, edge_type = "converge")
                }
            }

            ## Collapse to single stream
            streams <- list()
            n_streams <- 0L
            last_main <- merge_id

            ## Reset the arm-id allocator
            arm_seq <- 0L

            if (x$mode == "manual") {
                current_n <- merge_n
            }

            next
        }

        ## ---- Exclude ----
        if (step$type == "exclude") {
            diagram_phase <- diagram_phase + 1L
            try_start_phase()

            upcoming <- next_type(si)
            has_incl_label <- !is.null(step$included_label)
            skip_count_node <- (!isTRUE(step$show_count) && !has_incl_label) ||
                (!is.na(upcoming) &&
                 upcoming %chin% c("stratify", "allocate", "endpoint", "combine"))

            if (!in_arms) {
                res <- resolve_exclusion(
                    x$mode, step,
                    data      = if (x$mode == "data") current_data$.all else NULL,
                    current_n = if (x$mode == "manual") current_n else NULL
                )

                ## Side box label: scalar (pre-stratify, always length 1)
                side_lbl <- step$label[1L]

                side_id <- add_node(
                    text = side_lbl, n = res$n_excluded, role = "side",
                    reasons = res$reasons, phase = diagram_phase
                )
                add_edge(last_main, side_id, edge_type = "exclude")

                if (!skip_count_node) {
                    rlbl <- if (!is.null(step$included_label)) step$included_label[1L] else ""
                    main_id <- add_node(
                        text = rlbl, n = res$n_included, role = "main",
                        phase = diagram_phase
                    )
                    add_edge(last_main, main_id, edge_type = "flow")
                    last_main <- main_id
                }

                if (x$mode == "data") {
                    current_data$.all <- res$included_data
                } else {
                    current_n <- res$n_included
                }

            } else {
                ## Per-arm exclusions. The per-arm data store is keyed by arm_id.
                arm_keys  <- vapply(streams, function(s) as.character(s$arm_id),
                                    character(1L))
                manual_ns <- step$n

                ## Resolve per-arm side labels and included labels
                ## If scalar, recycle; if vector, index by arm
                side_labels <- step$label
                incl_labels <- step$included_label

                results <- lapply(seq_len(n_streams), function(i) {
                    step_i <- step
                    if (!is.null(step$reasons) && is.list(step$reasons))
                        step_i$reasons <- step$reasons[[i]]
                    ni <- if (x$mode == "manual") {
                              if (length(manual_ns) == 1L) manual_ns else manual_ns[i]
                          } else NULL
                    resolve_exclusion(
                        x$mode, step_i,
                        data              = if (x$mode == "data") current_data[[arm_keys[i]]] else NULL,
                        current_n         = if (x$mode == "manual") streams[[i]]$n else NULL,
                        manual_n_override = ni
                    )
                })

                ## Harmonize reason ordering across arms so categories appear in the
                ## same position in every side box, ordered by total count descending
                has_any_reasons <- any(vapply(results,
                                              function(r) !is.null(r$reasons), logical(1L)))
                if (has_any_reasons) {
                    ## Collect all reason names and sum counts across arms
                    all_names <- unique(unlist(lapply(results,
                                                      function(r) names(r$reasons))))
                    ## Effective count of a reason entry
                    ## Allows two-level reasons to be ordered by their parent totals
                    eff_count <- function(v)
                        if (length(v) > 1L || !is.null(names(v))) sum(v) else as.numeric(v)
                    totals <- vapply(all_names, function(nm) {
                        sum(vapply(results, function(r) {
                            if (nm %in% names(r$reasons)) eff_count(r$reasons[[nm]]) else 0
                        }, numeric(1L)))
                    }, numeric(1L))
                    global_order <- all_names[order(totals, decreasing = TRUE)]

                    ## Reorder each arm's reasons to match
                    for (ri in seq_along(results)) {
                        r <- results[[ri]]$reasons
                        if (!is.null(r)) {
                            ordered <- r[intersect(global_order, names(r))]
                            results[[ri]]$reasons <- ordered
                        }
                    }
                }

                for (i in seq_len(n_streams)) {
                    res <- results[[i]]

                    ## Per-arm side box label
                    s_lbl <- if (length(side_labels) >= i) side_labels[i] else side_labels[1L]

                    side_id <- add_node(
                        text = s_lbl, n = res$n_excluded, role = "side",
                        reasons = res$reasons, arm_id = streams[[i]]$arm_id,
                        arm_parent = streams[[i]]$parent, arm_level = streams[[i]]$level,
                        phase = diagram_phase
                    )
                    add_edge(streams[[i]]$last_node, side_id, edge_type = "exclude")

                    if (!skip_count_node) {
                        ## Per-arm included label
                        rlbl <- if (!is.null(incl_labels)) {
                                    if (length(incl_labels) >= i) incl_labels[i] else incl_labels[1L]
                                } else ""
                        main_id <- add_node(
                            text = rlbl, n = res$n_included, role = "main",
                            arm_id = streams[[i]]$arm_id,
                            arm_parent = streams[[i]]$parent, arm_level = streams[[i]]$level,
                            phase = diagram_phase
                        )
                        add_edge(streams[[i]]$last_node, main_id, edge_type = "flow")
                        streams[[i]]$last_node <- main_id
                    }

                    if (x$mode == "data") {
                        current_data[[arm_keys[i]]] <- res$included_data
                    }
                    ## Track the post-exclusion count for this stream in both modes
                    streams[[i]]$n <- res$n_included
                }
            }
        }

        ## ---- Stratify ----
        if (step$type == "stratify") {
            is_factorial <- isTRUE(in_arms)   # a prior uncombined split is active -> level 2

            if (!is_factorial) {
                ## LEVEL 1: split the single trunk (allocation box + arms)
                diagram_phase <- diagram_phase + 1L
                in_arms <- TRUE

                alloc_label <- step$label %||% "Stratified"

                if (x$mode == "data") {
                    var <- step$variable
                    if (!var %chin% names(current_data$.all))
                        stop(sprintf("Column '%s' not found in data", var), call. = FALSE)
                    all_data <- current_data$.all
                    alloc_n  <- .row_count(all_data)
                    split_result <- split_by_var(all_data, var, step$labels)
                    arm_labels   <- split_result$labels
                    n_arms       <- length(arm_labels)
                    current_data <- split_result$data
                } else {
                    arm_labels <- step$labels
                    n_arms     <- length(arm_labels)
                    alloc_n    <- sum(step$n)
                    ## Arm counts should account for everyone entering the split.
                    if (length(current_n) == 1L && !is.na(current_n) &&
                        alloc_n != current_n) {
                        warn_arithmetic(
                            "Split '%s' arm counts sum to %s but %s entered (a difference of %s).",
                            alloc_label, alloc_n, current_n, abs(current_n - alloc_n))
                    }
                }

                ## Allocation box on its own row
                alloc_id <- add_node(
                    text = alloc_label, n = alloc_n, role = "alloc",
                    phase = diagram_phase
                )
                add_edge(last_main, alloc_id, edge_type = "flow")

                ## Arm nodes on their own row
                diagram_phase <- diagram_phase + 1L

                arm_ns <- if (x$mode == "data") {
                              vapply(current_data, .row_count, integer(1L))
                          } else {
                              step$n
                          }

                ## Create per-arm streams. arm_id is a stable, globally unique id,
                ## assigned up front so the per-arm data store can be re-keyed by it.
                arm_ids <- vapply(seq_len(n_arms), function(i) next_arm_id(), integer(1L))
                streams <- lapply(seq_len(n_arms), function(i) {
                    aid <- arm_ids[i]
                    arm_node <- add_node(
                        text = arm_labels[i], n = arm_ns[i], role = "arm",
                        arm_id = aid, arm_level = 1L, phase = diagram_phase
                    )
                    add_edge(alloc_id, arm_node, edge_type = "split")
                    list(
                        id        = aid,
                        arm_id    = aid,
                        parent    = NA_integer_,
                        level     = 1L,
                        label     = arm_labels[i],
                        group     = NA_character_,
                        last_node = arm_node,
                        n         = arm_ns[i]
                    )
                })
                n_streams <- n_arms
                ## Re-key the per-arm data store by arm_id. Labels are unique within one
                ## split, but a factorial sub-arm label repeats across parents, so arm_id
                ## is the collision-free key the downstream per-arm lookups use.
                if (x$mode == "data")
                    current_data <- setNames(current_data[arm_labels], as.character(arm_ids))

            } else {
                ## LEVEL 2 (factorial): split EACH active arm into sub-arms
                n_parents      <- n_streams
                parent_streams <- streams
                diagram_phase  <- diagram_phase + 1L  # sub-arm row

                if (x$mode == "data") {
                    ## Sub-arm key set derived once from the second variable across the
                    ## whole entering cohort, so the grid is rectangular (a parent missing
                    ## a cell gets an empty sub-arm); each parent splits against that set.
                    var         <- step$variable
                    parent_keys <- vapply(parent_streams,
                                          function(s) as.character(s$arm_id), character(1L))
                    entering    <- rbindlist(current_data[parent_keys])
                    if (!var %chin% names(entering))
                        stop(sprintf("Column '%s' not found in data", var), call. = FALSE)
                    ecol     <- entering[[var]]
                    sub_keys <- if (is.factor(ecol)) levels(ecol) else sort(unique(ecol))
                } else {
                    ## Manual: shared sub-arm labels; counts parent-major as the full
                    ## n_parents * n_sub vector or one shared set recycled to each parent.
                    sub_labels <- step$labels
                    n_sub      <- length(sub_labels)
                    sub_n_all  <- step$n
                    if (length(sub_n_all) == n_sub && n_parents > 1L) {
                        sub_n_all <- rep(sub_n_all, n_parents)
                    } else if (length(sub_n_all) != n_parents * n_sub) {
                        stop(sprintf(
                            paste0("Factorial split '%s': 'n' must supply %d values ",
                                   "(%d parents x %d sub-arms), or %d to share across parents; ",
                                   "got %d."),
                            step$label %||% "Stratified", n_parents * n_sub, n_parents, n_sub,
                            n_sub, length(sub_n_all)), call. = FALSE)
                    }
                }

                new_streams <- list()
                new_data    <- list()
                for (pi in seq_len(n_parents)) {
                    ps <- parent_streams[[pi]]
                    if (x$mode == "data") {
                        sp       <- split_by_var(current_data[[as.character(ps$arm_id)]],
                                                 var, step$labels, keys = sub_keys)
                        sub_lbls <- sp$labels
                        sub_ns   <- vapply(sp$data, .row_count, integer(1L))
                        sub_data <- sp$data
                    } else {
                        base     <- (pi - 1L) * n_sub
                        sub_lbls <- sub_labels
                        sub_ns   <- sub_n_all[base + seq_len(n_sub)]
                        ## Optional arithmetic check: a parent's sub-arms should sum to it.
                        if (!is.na(ps$n) && length(ps$n) == 1L && sum(sub_ns) != ps$n)
                            warn_arithmetic(
                                "Factorial split of '%s': sub-arm counts sum to %s but %s entered (a difference of %s).",
                                ps$label, sum(sub_ns), ps$n, abs(ps$n - sum(sub_ns)))
                    }
                    for (sj in seq_along(sub_lbls)) {
                        aid <- next_arm_id()
                        sub_node <- add_node(
                            text = sub_lbls[sj], n = sub_ns[sj], role = "arm",
                            arm_id = aid, arm_parent = ps$arm_id, arm_level = 2L,
                            phase = diagram_phase
                        )
                        add_edge(ps$last_node, sub_node, edge_type = "split")
                        new_streams <- c(new_streams, list(list(
                                                          id        = aid,
                                                          arm_id    = aid,
                                                          parent    = ps$arm_id,
                                                          level     = 2L,
                                                          label     = sub_lbls[sj],
                                                          group     = NA_character_,
                                                          last_node = sub_node,
                                                          n         = sub_ns[sj]
                                                      )))
                        if (x$mode == "data") new_data[[as.character(aid)]] <- sub_data[[sj]]
                    }
                }
                streams   <- new_streams
                n_streams <- length(streams)
                if (x$mode == "data") current_data <- new_data
            }
        }

        ## ---- Endpoint ----
        if (step$type == "endpoint") {
            diagram_phase <- diagram_phase + 1L

            if (!is.null(step$groups) || !is.null(step$variable)) {
                ## Split endpoint: a shared distributor fans into terminal group boxes.
                ## A single incoming stream only---a per-arm group split would be a
                ## factorial split, which is a deliberate scope boundary.
                if (in_arms)
                    stop("A split endpoint requires a single incoming stream; it cannot ",
                         "follow an uncombined stratify()/allocate().", call. = FALSE)
                if (x$mode == "data") {
                    ## Partition the cohort reaching the endpoint by the grouping column;
                    ## the split is exhaustive, so the groups sum to the data count.
                    sp           <- split_by_var(current_data$.all, step$variable)
                    group_labels <- sp$labels
                    group_ns     <- vapply(sp$data, .row_count, integer(1L))
                } else {
                    group_labels <- step$groups
                    group_ns     <- step$n
                }
                n_groups <- length(group_labels)
                total_n  <- sum(group_ns)
                ## Advisory (manual mode only): the groups should account for everyone
                ## reaching the endpoint. In data mode the partition is exhaustive by
                ## construction, so no mismatch is possible.
                if (x$mode == "manual" && length(current_n) == 1L && !is.na(current_n) &&
                    total_n != current_n)
                    warn_arithmetic(
                        "Split endpoint '%s' group counts sum to %s but %s reached it (a difference of %s).",
                        step$label, total_n, current_n, abs(current_n - total_n))
                ## Shared distributor box (carries the endpoint label), centered on the
                ## trunk; the group boxes sit on their own row and fan from it via split
                ## edges---structurally an allocation whose arms happen to be terminal.
                par_id <- add_node(text = step$label, n = total_n, role = "alloc",
                                   phase = diagram_phase)
                add_edge(last_main, par_id, edge_type = "flow")
                diagram_phase <- diagram_phase + 1L
                for (i in seq_len(n_groups)) {
                    g_node <- add_node(text = group_labels[i], n = group_ns[i],
                                       role = "endpoint", arm_id = i,
                                       phase = diagram_phase)
                    add_edge(par_id, g_node, edge_type = "split")
                }
            } else if (!in_arms) {
                ni <- if (x$mode == "data") .row_count(current_data$.all) else current_n
                ep_reasons <- if (is.list(step$reasons)) step$reasons[[1L]] else step$reasons
                eid_node <- add_node(
                    text = step$label, n = ni, role = "endpoint",
                    reasons = ep_reasons, phase = diagram_phase
                )
                add_edge(last_main, eid_node, edge_type = "flow")
            } else {
                ep_ns <- if (x$mode == "data") {
                             arm_keys <- vapply(streams, function(s) as.character(s$arm_id),
                                                character(1L))
                             vapply(current_data[arm_keys], .row_count, integer(1L))
                         } else {
                             vapply(streams, function(s) s$n, numeric(1L))
                         }
                for (i in seq_len(n_streams)) {
                    ep_reasons_i <- if (is.list(step$reasons) && length(step$reasons) >= i) {
                                        step$reasons[[i]]
                                    } else if (!is.list(step$reasons)) {
                                        step$reasons
                                    } else NULL
                    eid_node <- add_node(
                        text = step$label, n = ep_ns[i], role = "endpoint",
                        reasons = ep_reasons_i, arm_id = streams[[i]]$arm_id,
                        arm_parent = streams[[i]]$parent, arm_level = streams[[i]]$level,
                        phase = diagram_phase
                    )
                    add_edge(streams[[i]]$last_node, eid_node, edge_type = "flow")
                }
            }
        }
    }

    close_phase(diagram_phase)

    nodes_dt  <- rbindlist(nodes[seq_len(nid)])
    edges_dt  <- if (eid > 0L) rbindlist(edges[seq_len(eid)]) else {
                                                                  data.table(from = integer(), to = integer(), edge_type = character())
                                                              }
    phases_dt <- if (pid > 0L) rbindlist(phases[seq_len(pid)]) else {
                                                                   data.table(label = character(), phase_start = integer(),
                                                                              phase_end = integer())
                                                               }

    ## Optional debug: the constructed graph (nodes, edges, phase ranges).
    if (isTRUE(getOption("selecta.debug_layout", FALSE))) {
        node_cols <- intersect(c("node_id", "row", "role", "phase", "arm_id",
                                 "stream_group", "text", "n"), names(nodes_dt))
        debug_emit("compute() graph",
                   n_nodes = nrow(nodes_dt), n_edges = nrow(edges_dt),
                   n_phases = nrow(phases_dt),
                   nodes = nodes_dt[, ..node_cols],
                   edges = edges_dt,
                   phases = phases_dt)
    }

    list(nodes = nodes_dt, edges = edges_dt, phases = phases_dt)
}


### * Exclusion resolution

#' Resolve an Exclusion Step
#'
#' Evaluates a single exclusion step in either data or manual mode and
#' returns the excluded and remaining counts, the remaining data (data
#' mode), and any tabulated sub-reasons.
#'
#' @param mode Character, either \code{"data"} or \code{"manual"}.
#' @param step The exclusion step (list) from the pipeline.
#' @param data A \code{data.table} of current participants (data mode).
#' @param current_n Integer current count (manual mode).
#' @param manual_n_override Optional integer overriding \code{step$n}.
#' @return A list with \code{n_excluded}, \code{n_included},
#'   \code{included_data}, and \code{reasons}.
#' @keywords internal
resolve_exclusion <- function(mode, step, data = NULL, current_n = NULL,
                              manual_n_override = NULL) {
    show_zero <- isTRUE(step$show_zero)

    if (mode == "data") {
        mask <- tryCatch(
            eval(step$expr_call, envir = data, enclos = parent.frame(3L)),
            error = function(e) {
                stop(sprintf("Error evaluating exclusion '%s': %s",
                             step$label, conditionMessage(e)), call. = FALSE)
            }
        )
        if (!is.logical(mask))
            stop(sprintf("Exclusion '%s' must evaluate to logical", step$label),
                 call. = FALSE)
        mask[is.na(mask)] <- FALSE
        ## Only the included rows are needed downstream (included_data). The
        ## excluded count comes from the mask directly, and reason tabulation
        ## needs only the reason column, so the full excluded subset is never
        ## materialized.
        included <- data[!mask]
        n_excl <- sum(mask)
        n_keep <- nrow(included)

        reasons <- NULL
        n_unattributed <- 0L
        if (!is.null(step$reasons_var) && n_excl > 0L) {
            rvar <- step$reasons_var
            if (length(rvar) == 2L) {
                if (all(rvar %chin% names(data))) {
                    reasons <- tabulate_reasons(data[[rvar[1L]]][mask],
                                                data[[rvar[2L]]][mask])
                    n_unattributed <- sum(is.na(data[[rvar[1L]]][mask]))
                }
            } else if (rvar %chin% names(data)) {
                reasons <- tabulate_reasons(data[[rvar]][mask])
                n_unattributed <- sum(is.na(data[[rvar]][mask]))
            }
        }
        ## A reason column with no value for some removed rows silently groups
        ## them under "Other"; warn so an attribution gap is not missed.
        if (n_unattributed > 0L)
            warn_arithmetic(
                "Exclusion '%s': %s of %s removed rows have no value in reason column '%s' and were grouped under \"Other\".",
                step$label, n_unattributed, n_excl, rvar[1L])
        ## The zero-drop applies only to a flat tabulation; a nested (two-column)
        ## result is a list and contains only categories present in the data.
        if (!is.null(reasons) && !show_zero && is.numeric(reasons)) {
            reasons <- reasons[reasons > 0L]
            if (length(reasons) == 0L) reasons <- NULL
        }
        if (isTRUE(step$collapse_singletons))
            reasons <- collapse_singleton_reasons(reasons)

        list(n_excluded = n_excl, n_included = n_keep,
             included_data = included, reasons = reasons)
    } else {
        n_exc <- if (!is.null(manual_n_override)) manual_n_override else step$n

        ## Advisory arithmetic checks (manual mode). Counts are not altered.
        ## All manual-mode checks are evaluated here at compute time so the audit
        ## is complete and consistent
        if (length(current_n) == 1L && !is.na(current_n) &&
            length(n_exc) == 1L && !is.na(n_exc) && n_exc > current_n) {
            warn_arithmetic(
                "Exclusion '%s' removes %s but only %s are available (remaining would be %s).",
                step$label, n_exc, current_n, current_n - n_exc)
        }
        raw_reasons <- step$reasons
        if (!is.null(raw_reasons) && !is.list(raw_reasons) &&
            is.numeric(raw_reasons) && length(n_exc) == 1L && !is.na(n_exc) &&
            sum(raw_reasons) != n_exc) {
            warn_arithmetic(
                "Exclusion '%s' sub-reasons sum to %s but the exclusion total is %s.",
                step$label, sum(raw_reasons), n_exc)
        }
        ## Same check for a two-level reasons list: each entry contributes its own
        ## count (a scalar leaf) or the sum of its sub-reasons (a named vector).
        if (!is.null(raw_reasons) && is.list(raw_reasons) &&
            length(n_exc) == 1L && !is.na(n_exc)) {
            eff <- vapply(raw_reasons, function(v)
                if (length(v) > 1L || !is.null(names(v))) sum(v) else as.numeric(v),
                numeric(1L))
            if (!anyNA(eff) && sum(eff) != n_exc)
                warn_arithmetic(
                    "Exclusion '%s' sub-reasons sum to %s but the exclusion total is %s.",
                    step$label, sum(eff), n_exc)
        }

        reasons <- step$reasons
        if (!is.null(reasons) && !is.list(reasons) && !show_zero) {
            reasons <- reasons[reasons > 0L]
            if (length(reasons) == 0L) reasons <- NULL
        }
        if (isTRUE(step$collapse_singletons))
            reasons <- collapse_singleton_reasons(reasons)
        list(n_excluded = n_exc, n_included = current_n - n_exc,
             included_data = NULL, reasons = reasons)
    }
}


### * Helper functions

#' Tabulate Exclusion Sub-Reasons
#'
#' Counts occurrences of each reason category in a vector, treating
#' \code{NA} as \code{"Other"}, and returns counts sorted descending.
#'
#' @param reason_col A vector of reason values for the excluded participants.
#' @return A named integer vector of counts, ordered by descending count.
#' @keywords internal
tabulate_reasons <- function(reason_col, sub_col = NULL) {
    reason_col[is.na(reason_col)] <- "Other"
    if (is.null(sub_col)) {
        tbl <- data.table(r = reason_col)[, .(n = .N), by = r]
        setorderv(tbl, "n", order = -1L)
        return(setNames(tbl$n, tbl$r))
    }
    ## Two-column cross-tabulation -> nested list(parent = c(sub = n, ...), ...).
    ## Parents are ordered by descending total; sub-reasons within each parent by
    ## descending count.
    sub_col[is.na(sub_col)] <- "Other"
    dt      <- data.table(r = reason_col, s = sub_col)
    par_tot <- dt[, .(tot = .N), by = r]
    setorderv(par_tot, "tot", order = -1L)
    sub_tab <- dt[, .(n = .N), by = .(r, s)]
    out <- lapply(par_tot$r, function(p) {
        sp <- sub_tab[r == p]
        setorderv(sp, "n", order = -1L)
        setNames(sp$n, sp$s)
    })
    names(out) <- par_tot$r
    out
}

#' Collapse Single-Child Parents in a Two-Level Reason List
#'
#' For a nested reasons list, any parent whose breakdown is a single
#' sub-reason is replaced by a plain leaf carrying the parent's label and
#' count---the lone sub-reason is redundant. A flat reasons vector (no
#' parents) passes through unchanged.
#'
#' @param reasons A reasons object: a named numeric vector (flat), or a list
#'   mixing scalar leaves and named sub-reason vectors (nested).
#' @return The reasons object with single-child parents collapsed to leaves.
#' @keywords internal
collapse_singleton_reasons <- function(reasons) {
    if (is.null(reasons) || !is.list(reasons)) return(reasons)
    out <- lapply(reasons, function(v)
        if (!is.null(names(v)) && length(v) == 1L) unname(v[[1L]]) else v)
    names(out) <- names(reasons)
    out
}

#' Split a Dataset into Arm Streams by a Variable
#'
#' Partitions a \code{data.table} by the levels of a splitting variable,
#' optionally relabeling levels, and returns the per-arm data and labels.
#'
#' @param dt A \code{data.table} to partition.
#' @param var Character name of the splitting variable.
#' @param labels Optional character vector of arm labels; may be named to
#'   relabel specific factor levels.
#' @param keys Optional explicit set of factor levels to split against (shared
#'   across parents in a factorial split), keeping partitions rectangular.
#' @return A list with \code{data} (named list of per-arm
#'   \code{data.table}s) and \code{labels} (character arm labels).
#' @keywords internal
split_by_var <- function(dt, var, labels = NULL, keys = NULL) {
    split_col <- dt[[var]]
    lvls <- if (!is.null(keys)) keys
            else if (is.factor(split_col)) levels(split_col)
            else sort(unique(split_col))
    if (!is.null(labels)) {
        if (!is.null(names(labels))) {
            arm_labels <- as.character(labels)
            arm_keys   <- names(labels)
        } else {
            arm_labels <- as.character(labels)
            arm_keys   <- lvls
        }
    } else {
        arm_labels <- as.character(lvls)
        arm_keys   <- lvls
    }
    splits <- split(dt, by = var, drop = TRUE)
    data_list <- setNames(
        lapply(arm_keys, function(k) {
            if (k %chin% names(splits)) splits[[k]] else dt[0L]
        }), arm_labels
    )
    list(labels = arm_labels, data = data_list)
}

#' @keywords internal
.row_count <- nrow
