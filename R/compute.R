#' Compute Enrollment Counts
#'
#' Walks the step list and resolves all counts, producing a graph of
#' nodes, edges, and phases. Maintains a generalised stream model where
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
                       grid_row = NA_integer_, grid_col = NA_integer_) {
    nid <<- nid + 1L
    nodes[[nid]] <<- list(
      node_id = nid, text = text, n = n,
      role = role, reasons = list(reasons),
      arm_id = arm_id, phase = phase,
      stream_group = stream_group,
      grid_row = grid_row, grid_col = grid_col
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
  ## Each stream: list(id, label, group, last_node, data, n)
  streams     <- list()
  n_streams   <- 0L
  in_parallel <- FALSE   # TRUE when multiple streams are active
  in_arms     <- FALSE   # TRUE after stratify() specifically

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

  for (si in seq_len(n_steps)) {
    step <- steps[[si]]

    ## ---- Phase ----
    if (step$type == "phase") {
      close_phase(diagram_phase)
      current_phase_label <- step$label
      current_phase_start <- diagram_phase + 1L
      next
    }

    ## ---- Sources ----
    if (step$type == "sources") {
      diagram_phase <- diagram_phase + 1L
      try_start_phase()

      ## One header node and one source node per group
      n_groups <- length(step$groups)
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
          text = "Records identified from:", n = group_n, role = "source",
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
      in_parallel <- TRUE
      next
    }

    ## ---- Combine ----
    if (step$type == "combine") {
      diagram_phase <- diagram_phase + 1L
      try_start_phase()

      total_n <- sum(vapply(streams, function(s) s$n, numeric(1L)))
      merge_n <- if (!is.null(step$n)) step$n else total_n

      ## Merged node
      merge_id <- add_node(
        text = step$label, n = merge_n, role = "main",
        phase = diagram_phase
      )

      ## Converge edges: one per source group
      group_last <- vapply(
          split(streams, vapply(streams, `[[`, character(1L), "group")),
          function(ss) ss[[length(ss)]]$last_node,
          integer(1L)
      )
      for (g_last_node in group_last) {
        add_edge(g_last_node, merge_id, edge_type = "converge")
      }

      ## Collapse to single stream
      streams <- list()
      n_streams <- 0L
      in_parallel <- FALSE
      last_main <- merge_id

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
         upcoming %chin% c("stratify", "allocate", "endpoint", "classify"))

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
            text = rlbl, n = res$n_remaining, role = "main",
            phase = diagram_phase
          )
          add_edge(last_main, main_id, edge_type = "flow")
          last_main <- main_id
        }

        if (x$mode == "data") {
          current_data$.all <- res$remaining_data
        } else {
          current_n <- res$n_remaining
        }

      } else {
        ## Per-arm exclusions
        arm_labels <- vapply(streams, function(s) s$label, character(1L))
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
            data              = if (x$mode == "data") current_data[[arm_labels[i]]] else NULL,
            current_n         = if (x$mode == "manual") streams[[i]]$n else NULL,
            manual_n_override = ni
          )
        })

          ## Harmonize reason ordering across arms so that the same
          ## categories appear in the same position in every side box.
          ## Order is determined by total count across all arms (descending).
          has_any_reasons <- any(vapply(results,
                                        function(r) !is.null(r$reasons), logical(1L)))
          if (has_any_reasons) {
              ## Collect all reason names and sum counts across arms
              all_names <- unique(unlist(lapply(results,
                                                function(r) names(r$reasons))))
              totals <- vapply(all_names, function(nm) {
                  sum(vapply(results, function(r) {
                      rv <- r$reasons[nm]
                      if (is.na(rv)) 0L else as.integer(rv)
                  }, integer(1L)))
              }, integer(1L))
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
                  reasons = res$reasons, arm_id = i, phase = diagram_phase
              )
              add_edge(streams[[i]]$last_node, side_id, edge_type = "exclude")

              if (!skip_count_node) {
                  ## Per-arm included label
                  rlbl <- if (!is.null(incl_labels)) {
                              if (length(incl_labels) >= i) incl_labels[i] else incl_labels[1L]
                          } else ""
                  main_id <- add_node(
                      text = rlbl, n = res$n_remaining, role = "main",
                      arm_id = i, phase = diagram_phase
                  )
                  add_edge(streams[[i]]$last_node, main_id, edge_type = "flow")
                  streams[[i]]$last_node <- main_id
              }

              if (x$mode == "data") {
                  current_data[[arm_labels[i]]] <- res$remaining_data
              } else {
                  streams[[i]]$n <- res$n_remaining
              }
          }
      }
    }

    ## ---- Stratify ----
    if (step$type == "stratify") {
      diagram_phase <- diagram_phase + 1L
      in_arms <- TRUE
      in_parallel <- TRUE

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

      ## Create per-arm streams
      streams <- lapply(seq_len(n_arms), function(i) {
        arm_node <- add_node(
          text = arm_labels[i], n = arm_ns[i], role = "arm",
          arm_id = i, phase = diagram_phase
        )
        add_edge(alloc_id, arm_node, edge_type = "split")
        list(
          id        = i,
          label     = arm_labels[i],
          group     = NA_character_,
          last_node = arm_node,
          n         = arm_ns[i]
        )
      })
      n_streams <- n_arms
    }

    ## ---- Classify ----
    if (step$type == "classify") {
      diagram_phase <- diagram_phase + 1L

      nr <- length(step$rows)
      nc <- length(step$cols)

      if (in_arms) {
        ## Classify per arm: each arm gets its own grid
        ## For STARD, typically arms are index test result categories
        ## and classify provides the reference standard cross-tab
        for (a in seq_len(n_streams)) {
          for (ri in seq_len(nr)) {
            for (ci in seq_len(nc)) {
              cell_text <- paste0(step$rows[ri], " / ", step$cols[ci])
              cell_n <- step$n[ri, ci]
              if (is.list(step$n)) cell_n <- step$n[[a]][ri, ci]
              cell_id <- add_node(
                text = cell_text, n = cell_n, role = "cell",
                arm_id = a, phase = diagram_phase,
                grid_row = ri, grid_col = ci
              )
              add_edge(streams[[a]]$last_node, cell_id, edge_type = "classify")
            }
          }
        }
      } else {
        ## Single-stream classify
        for (ri in seq_len(nr)) {
          for (ci in seq_len(nc)) {
            cell_text <- paste0(step$rows[ri], " / ", step$cols[ci])
            cell_id <- add_node(
              text = cell_text, n = step$n[ri, ci], role = "cell",
              phase = diagram_phase,
              grid_row = ri, grid_col = ci
            )
            add_edge(last_main, cell_id, edge_type = "classify")
          }
        }
      }
    }

    ## ---- Endpoint ----
    if (step$type == "endpoint") {
      diagram_phase <- diagram_phase + 1L

      if (!in_arms) {
        ni <- if (x$mode == "data") .row_count(current_data$.all) else current_n
        ep_reasons <- if (is.list(step$reasons)) step$reasons[[1L]] else step$reasons
        eid_node <- add_node(
          text = step$label, n = ni, role = "endpoint",
          reasons = ep_reasons, phase = diagram_phase
        )
        add_edge(last_main, eid_node, edge_type = "flow")
      } else {
        ep_ns <- if (x$mode == "data") {
          arm_labels <- vapply(streams, function(s) s$label, character(1L))
          vapply(current_data[arm_labels], .row_count, integer(1L))
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
            reasons = ep_reasons_i, arm_id = i, phase = diagram_phase
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

  list(nodes = nodes_dt, edges = edges_dt, phases = phases_dt)
}


## ---- Exclusion resolution ----

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
    excluded  <- data[mask]
    remaining <- data[!mask]
    n_excl <- nrow(excluded)
    n_keep <- nrow(remaining)

    reasons <- NULL
    if (!is.null(step$reasons_var) && n_excl > 0L) {
      rvar <- step$reasons_var
      if (rvar %chin% names(excluded))
        reasons <- tabulate_reasons(excluded, rvar)
    }
    if (!is.null(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }

    list(n_excluded = n_excl, n_remaining = n_keep,
         remaining_data = remaining, reasons = reasons)
  } else {
    n_exc <- if (!is.null(manual_n_override)) manual_n_override else step$n
    reasons <- step$reasons
    if (!is.null(reasons) && !is.list(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }
    list(n_excluded = n_exc, n_remaining = current_n - n_exc,
         remaining_data = NULL, reasons = reasons)
  }
}


## ---- Helpers ----

#' @keywords internal
tabulate_reasons <- function(dt, rvar) {
  reason_col <- dt[[rvar]]
  reason_col[is.na(reason_col)] <- "Other"
  tbl <- data.table(r = reason_col)[, .(n = .N), by = r]
  setorderv(tbl, "n", order = -1L)
  setNames(tbl$n, tbl$r)
}

#' @keywords internal
split_by_var <- function(dt, var, labels = NULL) {
  split_col <- dt[[var]]
  lvls <- if (is.factor(split_col)) levels(split_col) else sort(unique(split_col))
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

fmt_n <- function(n) formatC(n, format = "d", big.mark = ",")

`%||%` <- function(a, b) if (is.null(a)) b else a
